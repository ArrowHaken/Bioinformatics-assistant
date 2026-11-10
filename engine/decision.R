#!/usr/bin/env Rscript
# =====================================================================
#  decision.R -- 自动判定 + 把依据摆到人工决定面前
#
#  这个文件回答两件事：
#
#  一、能自动判的，不要麻烦人（auto_decide）
#      选好文件夹、点运行之后，机器先自己看一遍：
#        · 这台机器上有没有现成的、能直接用的分析脚本
#        · 内存够不够，够就走原路径，不够就走分块路径（结果等价，已实测）
#        · R 和必需包装齐没有，还缺什么
#      够 → 直接往下走；不够 → 明确写出缺什么、卡在哪，不硬跑。
#
#  二、确实该由人定的，把依据摆出来（sync_gate_evidence）
#      比如「剔除哪些簇」。脚本不去猜，但要把人做决定需要的材料放在眼前：
#        · 每个簇多少细胞、占多少
#        · 每个簇在各样本里各有多少（判断是不是某个样本特有的）
#        · 每个簇靠哪些基因聚在一起（判断这是哪类细胞）
#      人看着这些数字点，而不是凭印象点。
#
#  为什么这样分：可自动的自动，该人定的给出材料。
#  既不是「全自动乱猜」，也不是「什么都问人」。
#
#  ASCII-only source（R 4.1.2 + 中文 Windows locale 不能解析非 ASCII 源码）。
#  中文文案放在 decision_messages.json。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, scipen = 999, warn = -1))

# ---------------------------------------------------------------------
# 文案
# ---------------------------------------------------------------------
.dm_cache <- new.env(parent = emptyenv())
dec_msg <- function(key, dir_engine) {
  if (!exists("m", envir = .dm_cache)) {
    f <- file.path(dir_engine, "workflow", "decision_messages.json")
    m <- if (file.exists(f)) tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE),
                                      error = function(e) NULL) else NULL
    assign("m", m, envir = .dm_cache)
  }
  m <- get("m", envir = .dm_cache)
  if (is.null(m) || is.null(m[[key]])) return(NULL)
  v <- m[[key]]
  if (is.list(v)) enc2native(vapply(v, function(x) as.character(x)[1], character(1)))
  else enc2native(as.character(v))
}

# ---------------------------------------------------------------------
# 一、自动判定
#
#  输出 status/decision.json。字段刻意保持「可核对」：
#    path     最终决定走哪条路
#    checks   每一项的实测值与判定
#    gaps     走不下去时，具体缺什么（供人工补）
# ---------------------------------------------------------------------
auto_decide <- function(run_dir, engine_dir) {
  cfg <- tryCatch(read_cfg(run_dir), error = function(e) NULL)
  sdir <- file.path(run_dir, "status")
  dir.create(sdir, showWarnings = FALSE, recursive = TRUE)

  checks <- list()
  gaps <- character(0)
  addc <- function(id, label, status, detail, value = NULL) {
    checks[[length(checks) + 1L]] <<- list(id = id, label = label, status = status,
                                           detail = detail, value = value)
  }

  # ---- 1. 体检结果（前面 env 那一步已经跑过）----
  ec <- NULL
  ecf <- file.path(sdir, "env_check.json")
  if (file.exists(ecf)) ec <- tryCatch(jsonlite::fromJSON(readLines(ecf, warn = FALSE, encoding = "UTF-8"),
                                                          simplifyVector = FALSE),
                                       error = function(e) NULL)

  # ---- 2. 内存 ----
  mem_gb <- NA_real_
  if (!is.null(ec) && !is.null(ec$items)) {
    for (it in ec$items) {
      if (identical(it$id, "memory_gb") && !is.null(it$value)) mem_gb <- as.numeric(it$value)
    }
  }
  if (!is.na(mem_gb)) {
    if (mem_gb >= 32) {
      addc("memory", "内存", "ok", sprintf("实测 %.1f GB，充裕，走原路径", mem_gb), mem_gb)
    } else if (mem_gb >= 12) {
      addc("memory", "内存", "ok", sprintf("实测 %.1f GB，够用；大样本可能转分块路径（结果等价）", mem_gb), mem_gb)
    } else {
      addc("memory", "内存", "warn",
           sprintf("实测 %.1f GB，偏小。会自动走分块路径（不抽样、不降精度，结果已实测等价）；若仍失败需换机器", mem_gb),
           mem_gb)
    }
  } else {
    addc("memory", "内存", "unknown", "读不到内存，按充足处理；若中途失败再改配置", NULL)
  }

  # ---- 3. 必需包 ----
  missing_req <- character(0)
  if (!is.null(ec) && !is.null(ec$items)) {
    for (it in ec$items) {
      if (identical(it$status, "fail") && identical(it$id, "Seurat")) {
        missing_req <- c(missing_req, "Seurat")
      }
      if (!is.null(it$stage) && identical(it$stage, "必需") && identical(it$status, "fail")) {
        missing_req <- c(missing_req, it$label)
      }
    }
  }
  missing_req <- unique(missing_req)
  if (length(missing_req)) {
    addc("packages", "必需 R 包", "fail",
         paste0("缺：", paste(missing_req, collapse = ", ")), length(missing_req))
    gaps <- c(gaps, paste0("装 R 包：", paste(missing_req, collapse = ", ")))
  } else {
    addc("packages", "必需 R 包", "ok", "必需包齐全", 0)
  }

  # ---- 3b. 去双/批次校正这类「会中断主流程」的可选包 ----
  blocking <- character(0)
  if (!is.null(ec) && !is.null(ec$items)) {
    for (it in ec$items) {
      if (!is.null(it$stage) && it$stage %in% c("去双", "合并") && identical(it$status, "warn")) {
        blocking <- c(blocking, it$label)
      }
    }
  }
  if (length(blocking)) {
    addc("blocking_pkgs", "主流程会用到的包", "fail",
         paste0("缺：", paste(blocking, collapse = ", "), "。缺这些，主流程跑到那一步会停"), length(blocking))
    gaps <- c(gaps, paste0("装这些包（否则主流程会停）：", paste(blocking, collapse = ", ")))
  } else {
    addc("blocking_pkgs", "主流程会用到的包", "ok", "去双与批次校正的包都在", 0)
  }

  # ---- 4. 机器上已有的脚本 ----
  #   有现成的就用现成的。再拷一套过去并行，两套早晚分叉，说不清哪个算数。
  existing <- list()
  if (!is.null(ec) && !is.null(ec$items)) {
    for (it in ec$items) {
      if (identical(it$id, "existing_scripts")) {
        n <- suppressWarnings(as.integer(it$value))
        if (!is.na(n) && n > 0) existing <- as.list(seq_len(n))
      }
    }
  }
  # 更可靠的做法：直接扫一次（体检那一步可能没跑）
  if (!length(existing)) {
    sc <- Sys.which("Rscript")
    rf <- file.path(engine_dir, "scan_scripts.R")
    if (nzchar(sc) && file.exists(rf)) {
      out <- tryCatch(system2(unname(sc), c("--vanilla", rf, "--out", file.path(sdir, "scripts_scan.json")),
                              stdout = FALSE, stderr = FALSE, wait = TRUE), error = function(e) 1L)
      if (identical(as.integer(out), 0L)) {
        sf <- file.path(sdir, "scripts_scan.json")
        if (file.exists(sf)) {
          sj <- tryCatch(jsonlite::fromJSON(readLines(sf, warn = FALSE, encoding = "UTF-8"),
                                            simplifyVector = FALSE), error = function(e) NULL)
          if (!is.null(sj) && !is.null(sj$hits)) existing <- sj$hits
        }
      }
    }
  }
  if (length(existing)) {
    paths <- vapply(existing, function(h) {
      if (is.list(h)) as.character(h$path %||% "?") else "?"
    }, character(1))
    addc("existing_scripts", "机器上已有的脚本", "warn",
         paste0("发现 ", length(existing), " 处现成的脚本库，先确认用哪套：",
                paste0("　", paths, collapse = "")),
         length(existing))
  } else {
    addc("existing_scripts", "机器上已有的脚本", "ok", "没扫到现成的流程脚本库", 0)
  }

  # ---- 5. 数据目录 ----
  indir <- tryCatch(cfg_val(cfg, "io", "indir", NULL), error = function(e) NULL)
  if (!is.null(indir) && length(indir) && nzchar(as.character(indir))) {
    indir <- as.character(indir)
    n_samples <- 0L
    if (dir.exists(indir)) {
      n_samples <- length(list.dirs(indir, recursive = FALSE, full.names = TRUE))
    }
    if (dir.exists(indir) && n_samples > 0) {
      addc("indir", "数据目录", "ok", sprintf("%s（%d 个样本子目录）", indir, n_samples), n_samples)
    } else {
      addc("indir", "数据目录", "fail", paste0("目录不存在或里面没有样本子目录：", indir), 0)
      gaps <- c(gaps, paste0("确认数据目录：", indir))
    }
  } else {
    addc("indir", "数据目录", "unknown", "还没填（需要在闸门里填）", NULL)
  }

  # ---- 6. 查表：该用库里哪一套 ----
  #
  #   这是「存取」里的「取」。判定完环境之后立刻查表，得到三件东西：
  #     环境 E?、需求 D?、取用哪一套。
  #   然后分两种情形处理：
  #     A 代码处理   本机有甲方脚本 → 用本机的；没有 → 从库取
  #     B 分析绘图   不管在哪台机器，都从库取
  #
  #   为什么这一步必须写进判定：
  #     以前只判「能不能跑」。能跑之后到底跑的是哪一套，没人知道 ——
  #     而同一件事在不同环境要用不同脚本，不查表就会各按各的理解选。
  pick_info <- NULL
  pk <- file.path(engine_dir, "pick.R")
  if (file.exists(pk)) {
    pick_info <- tryCatch({
      source(pk, local = TRUE)
      pick_set(engine_dir, category = "A", demand = "D1_pipeline",
               local_scripts_found = length(existing) > 0)
    }, error = function(e) NULL)
  }

  if (!is.null(pick_info) && is.null(pick_info$error)) {
    env_id <- pick_info$env$id %||% "unknown"
    set_id <- pick_info$id %||% "none"
    set_title <- pick_info$title %||% ""
    set_status <- pick_info$status %||% ""
    usable <- isTRUE(pick_info$usable)
    src <- pick_info$source %||% ""

    # 取用结论：写明「用哪套、从哪来、现在能不能用」
    if (identical(src, "local")) {
      addc("library_pick", "取用脚本（A 代码处理）", "ok",
           paste0("本机已有甲方脚本，直接用本机的（不另取）"), set_id)
    } else if (isTRUE(usable)) {
      addc("library_pick", "取用脚本（A 代码处理）", "ok",
           sprintf("环境 %s → 从库取「%s」（%s）", env_id, set_title, src), set_id)
    } else if (identical(set_id, "none") || !nzchar(set_id)) {
      addc("library_pick", "取用脚本（A 代码处理）", "fail",
           "库里没有匹配这套「环境 × 需求」的脚本，需要新建（走兜底规程）", "none")
      gaps <- c(gaps, "库里没有可直接取用的脚本：按 fallback 规程先做探针，再决定改还是新建")
    } else {
      # 库里登记了但取不出来（比如还没导入）—— 这是「假可用」，必须说清
      addc("library_pick", "取用脚本（A 代码处理）", "fail",
           sprintf("环境 %s → 库里对应条目是「%s」，状态 %s，现在取不出来",
                   env_id, set_title, set_status),
           set_id)
      gaps <- c(gaps, sprintf("把库里「%s」的文件导进来（当前状态 %s）", set_title, set_status))
    }
  } else {
    addc("library_pick", "取用脚本（A 代码处理）", "unknown",
         "读不到库表 library/index.json，无法查表", NULL)
  }

  # ---- 7. 分析绘图三类（B）：任何机器上都从库取 ----
  if (!is.null(pick_info) && is.null(pick_info$error)) {
    b <- tryCatch({
      source(pk, local = TRUE)
      pick_set(engine_dir, category = "B")
    }, error = function(e) NULL)
    if (!is.null(b) && !is.null(b$entries) && length(b$entries)) {
      bad <- Filter(function(e) !isTRUE(e$ok), b$entries)
      if (length(bad)) {
        addc("library_B", "取用脚本（B 分析绘图）", "fail",
             paste0("有 ", length(bad), " 项取不出来：",
                    paste0(vapply(bad, function(e) e$title %||% e$id, character(1)),
                           collapse = "、")), length(bad))
      } else {
        addc("library_B", "取用脚本（B 分析绘图）", "ok",
             sprintf("%d 项齐备：参考依据 / 出图 / PPT / 报告提取", length(b$entries)),
             length(b$entries))
      }
    }
  }

  # ---- 汇总：走哪条路 ----
  st <- vapply(checks, function(x) x$status, character(1))
  has_fail <- any(st == "fail")
  has_warn <- any(st == "warn")

  # 只有「先把人叫来」的事才拦：缺包、数据目录不对。
  # 内存偏小、发现有现成脚本，属提醒，不拦。
  blocker <- character(0)
  for (x in checks) {
    if (identical(x$status, "fail") &&
        x$id %in% c("packages", "blocking_pkgs", "indir")) {
      blocker <- c(blocker, x$label)
    }
  }

  path <- if (length(blocker)) "blocked" else if (length(existing)) "need_choice" else "go"
  verdict_cn <- switch(path,
    blocked = paste0("先解决这几件，再跑：", paste(blocker, collapse = "、")),
    need_choice = "数据与内存都够，但机器上已有脚本 —— 先定用哪套，别并行",
    go = "环境与数据都够，可以按流程往下跑"
  )

  out <- list(
    ok = TRUE,
    decided_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    path = path,
    verdict_cn = verdict_cn,
    blockers = as.list(blocker),
    gaps = as.list(gaps),
    checks = checks,
    note = paste0("自动判定只做「能不能跑」和「走哪条等价实现」。",
                  "涉及取舍的决定（剔除哪些簇/样本）不做判断，只把依据摆出来。")
  )
  f <- file.path(sdir, "decision.json")
  con <- file(f, open = "wb")
  writeBin(charToRaw(enc2utf8(jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE,
                                              null = "null", digits = NA))), con)
  close(con)
  out
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---------------------------------------------------------------------
# 二、把依据摆到闸门里
#
#   人在界面上看到的，是 gates.json。脚本算出的是
#   status/cluster1_evidence.json。这两份要接上 ——
#   否则就是「算了一堆依据，但没人看得到」。
#
#   只填 evidence / sample_options，不改 status 与 answer：
#   答复是人给的，程序不代填。
# ---------------------------------------------------------------------
sync_gate_evidence <- function(run_dir, engine_dir) {
  gf <- file.path(run_dir, "gates.json")
  ef <- file.path(run_dir, "status", "cluster1_evidence.json")
  df <- file.path(run_dir, "status", "decision.json")
  if (!file.exists(gf)) return(FALSE)

  gates <- tryCatch(jsonlite::fromJSON(readLines(gf, warn = FALSE, encoding = "UTF-8"),
                                       simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(gates) || is.null(gates$gates)) return(FALSE)

  ev <- if (file.exists(ef)) tryCatch(jsonlite::fromJSON(readLines(ef, warn = FALSE, encoding = "UTF-8"),
                                                         simplifyVector = FALSE),
                                      error = function(e) NULL) else NULL
  dec <- if (file.exists(df)) tryCatch(jsonlite::fromJSON(readLines(df, warn = FALSE, encoding = "UTF-8"),
                                                          simplifyVector = FALSE),
                                       error = function(e) NULL) else NULL
  changed <- FALSE

  for (i in seq_along(gates$gates)) {
    g <- gates$gates[[i]]
    gid <- g$id %||% ""

    # ---- 筛群：把每簇的细胞数 / 占比 / 样本分布 / top marker 摆出来 ----
    if (identical(gid, "cluster_filter") && !is.null(ev) && !is.null(ev$clusters)) {
      total <- ev$total_cells %||% 0
      cols <- c("簇", "细胞数", "占比%", "样本分布", "Top markers")
      rows <- list()
      for (cl in ev$clusters) {
        dist <- ""
        if (!is.null(cl$by_sample) && length(cl$by_sample)) {
          parts <- vapply(names(cl$by_sample), function(nm)
            paste0(nm, ":", cl$by_sample[[nm]]), character(1))
          dist <- paste(parts, collapse = " ")
        }
        tops <- if (!is.null(cl$top_markers) && length(cl$top_markers))
          paste(unlist(cl$top_markers), collapse = ", ") else "（未算出）"
        rows[[length(rows) + 1L]] <- list(
          as.character(cl$cluster), as.character(cl$n_cells),
          as.character(cl$pct), dist, tops
        )
      }
      gates$gates[[i]]$evidence <- list(
        type = "table",
        caption = paste0("本次一次聚类实测：", ev$n_clusters, " 个簇、", total,
                         " 个细胞、", ev$n_samples, " 个样本。",
                         "「样本分布」看这个簇是不是某个样本特有的；",
                         "「Top markers」看这个簇大概是什么细胞。"),
        columns = as.list(cols), rows = rows
      )
      if (!is.null(ev$markers_note) && nzchar(as.character(ev$markers_note))) {
        gates$gates[[i]]$note <- paste0(
          gates$gates[[i]]$note %||% "",
          "　【本次实测补充】", as.character(ev$markers_note))
      }
      gates$gates[[i]]$has_evidence <- TRUE
      changed <- TRUE
    }

    # ---- 剔样本：把每样本细胞数与占比摆出来 ----
    if (identical(gid, "post_cluster_exclusion") && !is.null(ev) && !is.null(ev$samples)) {
      rows <- list()
      sopt <- list()
      for (s in ev$samples) {
        rows[[length(rows) + 1L]] <- list(as.character(s$sample),
                                          as.character(s$n_cells),
                                          paste0(s$pct, "%"))
        sopt[[length(sopt) + 1L]] <- as.character(s$sample)
      }
      gates$gates[[i]]$evidence <- list(
        type = "table",
        caption = paste0("本次各样本细胞数与占比（一次聚类后实测，共 ",
                         ev$total_cells, " 个细胞）。",
                         "剔样本会影响每组数量是否均衡，请对照实验设计判断。"),
        columns = list("样本", "细胞数", "占比"), rows = rows
      )
      # 样本名来自本次实际结果，不是预先写死的
      gates$gates[[i]]$sample_options <- sopt
      changed <- TRUE
    }

    # ---- 低置信：把各簇细胞数摆出来，方便定门槛 ----
    if (identical(gid, "lowconfidence") && !is.null(ev) && !is.null(ev$clusters)) {
      rows <- lapply(ev$clusters, function(cl)
        list(as.character(cl$cluster), as.character(cl$n_cells)))
      gates$gates[[i]]$evidence <- list(
        type = "table",
        caption = paste0("本次各簇细胞数（共 ", ev$n_clusters,
                         " 个簇）。低于门槛的簇会被标为低置信，不硬安名字。"),
        columns = list("簇", "细胞数"), rows = rows
      )
      changed <- TRUE
    }

    # ---- 数据目录 / 注释依据：把自动判定的结果摆出来 ----
    if (identical(gid, "data_indir") && !is.null(dec) && !is.null(dec$checks)) {
      items <- list()
      for (x in dec$checks) {
        items[[length(items) + 1L]] <- list(k = as.character(x$label),
                                            v = as.character(x$detail))
      }
      gates$gates[[i]]$evidence <- list(
        type = "kv",
        caption = paste0("自动判定（", dec$decided_at %||% "", "）：",
                         dec$verdict_cn %||% ""),
        items = items
      )
      changed <- TRUE
    }
  }

  if (changed) {
    gates$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    tmp <- paste0(gf, ".tmp")
    con <- file(tmp, open = "wb")
    writeBin(charToRaw(enc2utf8(jsonlite::toJSON(gates, auto_unbox = TRUE, pretty = TRUE,
                                                null = "null", digits = NA))), con)
    close(con)
    file.rename(tmp, gf)
  }
  changed
}
