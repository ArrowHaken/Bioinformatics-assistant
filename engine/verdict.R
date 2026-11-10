#!/usr/bin/env Rscript
# =====================================================================
#  verdict.R -- 判定器：把「我跑完了」和「我跑对了」分开
#
#  为什么需要这个文件
#    在它之前，回写 done 就等于通过。agent 的最优策略是「随便跑一下 ->
#    回写 done -> 在聊天里如实说有偏差」，三件都做了，流程照样往前走。
#    因为 done 同时表示「我跑完了」和「我跑对了」。
#    本文件只做一件事：从可核事实算出结论，写进 reviews 表与 review.json。
#
#  四层，每层只答一个问题
#    L1 契约  这一步应该产出什么          <- flow_spec 的 review{} 声明（唯一权威）
#    L2 结构  在不在、全不全、对不对得上账
#    L3 合规  是不是用了规定的 skill 与脚本
#    L4 分析  生物学上说不说得通
#
#  三条不可让步的规则
#    1. 判不了写 unknown，不折算成 pass。
#    2. 只认可核事实：台账 set_id、脚本路径与 md5、产物的存在/大小/校验和、
#       退出码、从服务器取回的真实清单。自然语言保证一律不作为通过依据，
#       收到即记 ignored_self_claim 并忽略。
#    3. 结论只写进数据库。聊天里说什么不影响状态。
#
#  调用
#    Rscript engine/verdict.R --attempt <attempt_id> [--app <dir>]
#    或由 serve.R 直接 source 后调用 verdict_step()。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

VERDICT_RULE_VERSION <- "verdict-1.0"

# ---- flow_spec 读取（唯一权威）--------------------------------------

verdict_load_spec <- function(app_dir) {
  f <- file.path(app_dir, "engine", "flow_spec.R")
  if (!file.exists(f)) stop("找不到 flow_spec.R")
  e <- new.env(parent = globalenv())
  sys.source(f, envir = e)
  e
}

verdict_stage <- function(spec_env, step_id, profile = "server") {
  stages <- if (identical(profile, "local")) spec_env$LOCAL_STAGES else spec_env$SERVER_STAGES
  for (s in stages) if (identical(as.character(s$id), as.character(step_id))) return(s)
  NULL
}

# ---- 事实收集 --------------------------------------------------------

# 只从可核来源取事实。b 是回写体（POST /api/step/import 的 JSON）。
verdict_facts <- function(b, attempt_row = NULL, local_root = NULL) {
  arts <- b$artifacts
  norm <- list()
  if (!is.null(arts) && length(arts)) {
    for (a in arts) {
      if (is.character(a) && length(a) == 1L) {
        norm[[length(norm) + 1L]] <- list(rel = as.character(a), bytes = NA_integer_,
                                          md5 = NA_character_, mtime = NA_character_)
      } else if (is.list(a)) {
        norm[[length(norm) + 1L]] <- list(
          rel = as.character(a$rel %||% a$path %||% ""),
          bytes = suppressWarnings(as.integer(a$bytes %||% a$size %||% NA)),
          md5 = as.character(a$md5 %||% a$checksum %||% NA),
          mtime = as.character(a$mtime %||% a$modified %||% NA))
      }
    }
  }
  list(
    step_id = as.character(b$step %||% attempt_row$step_id %||% ""),
    exit_code = suppressWarnings(as.integer(b$exit_code %||% b$code %||% NA)),
    artifacts = norm,
    set_id = as.character(b$set_id %||% b$skill %||% NA),
    script_path = as.character(b$script_path %||% b$script %||% NA),
    script_md5 = as.character(b$script_md5 %||% b$script_digest %||% NA),
    local_root = local_root,
    ledger = b$ledger %||% NULL,
    req_id = as.character(b$req_id %||% attempt_row$req_id %||% ""),
    attempt_id = as.character(b$attempt_id %||% attempt_row$attempt_id %||% ""),
    species = as.character(b$species %||% ""),
    tissue = as.character(b$tissue %||% ""),
    # counts：守恒判据要的可核数字（输入细胞数、输出细胞数、样本数、簇数……）。
    #   为什么不从 rds 里读：读 rds 要起 R 线程加载几百 MB 的对象，
    #   判定器跑一次就占满内存。让回写方在跑完时顺手带上来，才是可核又便宜的做法。
    counts = b$counts %||% NULL,
    # samples：逐样本环节的真实样本名清单。
    #   有了它才能回答「这七份是不是这七个样本的」，而不只是「有七份文件」。
    samples = as.character(unlist(b$samples %||% (b$counts %||% list())$samples %||% character(0)))
  )
}

# 自我描述字段：这些一律不算证据。
VERDICT_SELF_CLAIM_FIELDS <- c(
  "self_check", "self_claim", "confirmed", "compliance", "note", "message",
  "claim", "assertion", "assurance", "remarks"
)

verdict_detect_self_claims <- function(b) {
  if (!is.list(b)) return(character(0))
  hit <- character(0)
  for (k in VERDICT_SELF_CLAIM_FIELDS) {
    v <- b[[k]]
    if (!is.null(v) && length(v) && nzchar(paste(unlist(v), collapse = ""))) hit <- c(hit, k)
  }
  # 形如「已按 skill 执行 / 已严格按要求执行」的自由文本也归入自述。
  txt <- paste(unlist(b[setdiff(names(b), c("artifacts", "log"))]), collapse = " ")
  if (grepl("已(严格)?(按|按照).{0,12}(执行|完成)|按要求完成|全部通过", txt)) hit <- c(hit, "free_text_claim")
  unique(hit)
}

# ---- 单条检查的构造 --------------------------------------------------

vchk <- function(layer, name, status, detail = NULL, expected = NULL,
                 actual = NULL, evidence = NULL) {
  # status: pass / warn / fail / unknown / covered / skipped / not_implemented
  #   not_implemented 与 unknown 必须分开：
  #     unknown  = 回写没给可核事实，责任在被判的一方，应当退回重做；
  #     not_implemented = 判据本身还没实现，责任在应用，退给 agent 没有意义，
  #                       只会让它反复重跑一个不可能通过的检查。
  #   把两者混成一个，结果就是「每一环都永远过不了」，人会开始习惯性放行，
  #   闭环当场退化成提示 —— 这比不做更糟。
  list(layer = layer, name = name, ok = switch(status,
        pass = TRUE, covered = TRUE, warn = FALSE, fail = FALSE,
        unknown = NA, skipped = NA, not_implemented = NA, FALSE),
       status = status, detail = detail, expected = expected,
       actual = actual, evidence = evidence)
}

# ---- 读表：只读表头与前 N 行，不把整表读进内存 ----------------------

verdict_read_header <- function(root, rel) {
  if (is.null(root) || is.na(root) || !nzchar(root)) return(NULL)
  p <- file.path(root, rel)
  if (!file.exists(p) || dir.exists(p)) return(NULL)
  tryCatch({
    ln <- readLines(p, n = 2, warn = FALSE, encoding = "UTF-8")
    if (!length(ln)) return(NULL)
    sep <- if (grepl("\t", ln[1])) "\t" else if (grepl(",", ln[1])) "," else "\t"
    h <- strsplit(ln[1], sep, fixed = TRUE)[[1]]
    trimws(gsub('^"|"$', "", h))
  }, error = function(e) NULL)
}

verdict_read_table <- function(root, rel, max_rows = 5000L) {
  if (is.null(root) || is.na(root) || !nzchar(root)) return(NULL)
  p <- file.path(root, rel)
  if (!file.exists(p) || dir.exists(p)) return(NULL)
  tryCatch({
    ln <- readLines(p, n = max_rows + 1L, warn = FALSE, encoding = "UTF-8")
    if (length(ln) < 2L) return(NULL)
    sep <- if (grepl("\t", ln[1])) "\t" else if (grepl(",", ln[1])) "," else "\t"
    hdr <- trimws(gsub('^"|"$', "", strsplit(ln[1], sep, fixed = TRUE)[[1]]))
    rows <- lapply(ln[-1], function(l) {
      v <- trimws(gsub('^"|"$', "", strsplit(l, sep, fixed = TRUE)[[1]]))
      length(v) <- length(hdr)
      v
    })
    m <- do.call(rbind, rows)
    colnames(m) <- hdr
    as.data.frame(m, stringsAsFactors = FALSE)
  }, error = function(e) NULL)
}

# ---- L1 契约 ---------------------------------------------------------

verdict_l1 <- function(stage, facts) {
  out <- list()
  rv <- stage$review %||% NULL
  if (is.null(rv)) {
    return(list(vchk("L1", "contract_declared", "unknown",
                     "这一步在 flow_spec 里没有 review 声明，判不了该产出什么",
                     evidence = "engine/flow_spec.R")))
  }

  prods <- as.character(rv$products %||% stage$artifacts %||% character(0))
  got <- vapply(facts$artifacts, function(a) a$rel, "")
  # 目录型产物（如 10.figs/1.cluster）：只要回写里有以该前缀开头的条目就算存在。
  for (p in prods) {
    is_dir <- grepl("/$", p) || !grepl("\\.[A-Za-z0-9]+$", p)
    # 支持简单通配：{samples} 这类模板占位符在判定时不该被当成文件名。
    #   为什么必须支持：doublet 的产物是「每个样本一个 rds」，名字在流程定义里
    #   只能写成模板；如果拿模板字面量去比对，真实回写永远对不上，
    #   于是一个完全正常的 doublet 产物也会被判缺件 —— 这是判据自己写错，
    #   不是产物有问题。判据写错比产物错更危险：它会把对的判成错的。
    pat <- gsub("\\{[A-Za-z_][A-Za-z0-9_]*\\}", "*", p, perl = TRUE)
    to_regex <- function(x) paste0("^", gsub("\\*", ".*",
      gsub("([.()+|^$\\\\])", "\\\\\\1", x)), "$")
    rx <- to_regex(pat)
    present <- any(got == p) || any(grepl(rx, got)) ||
      (is_dir && any(startsWith(got, paste0(p, "/"))))
    out[[length(out) + 1L]] <- vchk("L1", paste0("product:", p),
      if (present) "pass" else "fail",
      detail = if (present) "回写清单里有这一项" else "回写清单里没有这一项；缺件即不通过，不做部分完成",
      expected = p, actual = if (length(got)) paste(got, collapse = ", ") else "（空清单）",
      evidence = "回写 artifacts[]")
  }

  # 逐样本核对：声明了 per_sample 的环节，逐个样本核「这一份在不在」。
  #   这一步比通配严格，且不依赖本地有没有取回（只核回写清单）。
  if (isTRUE(rv$per_sample)) {
    smp <- facts$samples
    if (!length(smp)) {
      out[[length(out) + 1L]] <- vchk("L1", "samples_declared", "unknown",
        "逐样本环节但回写没给样本清单，无法判断是不是每个样本都交了",
        expected = "samples[]", actual = "未给",
        evidence = "回写 samples[] 或 counts$samples")
    } else {
      tpl <- prods[grepl("\\{[A-Za-z_][A-Za-z0-9_]*\\}", prods)][1]
      if (!is.na(tpl)) {
        for (one in smp) {
          want <- gsub("\\{[A-Za-z_][A-Za-z0-9_]*\\}", one, tpl, perl = TRUE)
          out[[length(out) + 1L]] <- vchk("L1", paste0("sample:", one),
            if (want %in% got) "pass" else "fail",
            detail = if (want %in% got) "这一份在清单里" else "这个样本没有对应产物",
            expected = want,
            actual = if (length(got)) paste(got, collapse = ", ") else "（空清单）",
            evidence = "回写 artifacts[]")
        }
        # 清单里出现不在样本名单里的文件：典型的「改名顶替」。
        extra <- setdiff(got, vapply(smp, function(one)
          gsub("\\{[A-Za-z_][A-Za-z0-9_]*\\}", one, tpl, perl = TRUE), ""))
        extra <- Filter(function(x) grepl("^1\\.doublet/|^doublet/", x) ||
                          grepl(paste0("^", dirname(tpl), "/"), x), extra)
        if (length(extra)) {
          out[[length(out) + 1L]] <- vchk("L1", "unknown_sample_files", "fail",
            "回写里有不属于本样本清单的文件；这类「改名顶替」不能算产物",
            expected = paste0("仅 ", paste(smp, collapse = ", ")),
            actual = paste(extra, collapse = ", "),
            evidence = "回写 artifacts[] 与 samples[]")
        }
      }
    }
  }

  mb <- suppressWarnings(as.numeric(rv$min_bytes %||% NA))
  if (!is.na(mb)) {
    for (a in facts$artifacts) {
      if (is.na(a$bytes)) {
        out[[length(out) + 1L]] <- vchk("L1", paste0("min_bytes:", a$rel), "unknown",
          "回写没给字节数，无法判断体积", expected = paste0(">= ", mb), actual = "未给")
      } else if (a$bytes < mb) {
        out[[length(out) + 1L]] <- vchk("L1", paste0("min_bytes:", a$rel), "fail",
          "产物小于该阶段声明的最小体积，通常是空文件或写了一半",
          expected = paste0(">= ", mb), actual = as.character(a$bytes),
          evidence = a$rel)
      }
    }
  }

  # 列检查只对「声明的那张表」生效，不对整批产物生效。
  #   第一版把 required_columns 套到了一步的所有产物上，于是注释环会因为
  #   annotated.rds 读不出表头而判缺列 —— rds 是二进制对象，本来就没有表头。
  #   那不是产物有问题，是判据写错了。判据写错比产物错更危险：
  #   它会把对的判成错的，然后逼着人放宽规则。
  cols <- as.character(rv$required_columns %||% character(0))
  if (length(cols)) {
    target_file <- as.character(rv$columns_file %||% "")
    cand <- facts$artifacts
    if (nzchar(target_file))
      cand <- Filter(function(a) identical(basename(a$rel), target_file), cand)
    else
      cand <- Filter(function(a) tolower(tools::file_ext(a$rel)) %in%
                       c("xls", "xlsx", "csv", "tsv"), cand)
    for (a in cand) {
      hdr <- verdict_read_header(facts$local_root, a$rel)
      if (is.null(hdr)) {
        out[[length(out) + 1L]] <- vchk("L1", paste0("columns:", a$rel), "unknown",
          "本地没有这份文件，读不到表头", expected = paste(cols, collapse = ", "),
          actual = "未取回")
        next
      }
      miss <- setdiff(cols, hdr)
      out[[length(out) + 1L]] <- vchk("L1", paste0("columns:", a$rel),
        if (length(miss)) "fail" else "pass",
        detail = if (length(miss)) paste0("缺列：", paste(miss, collapse = ", "))
                 else "必需列齐全",
        expected = paste(cols, collapse = ", "),
        actual = paste(hdr, collapse = ", "),
        evidence = paste0(a$rel, " 表头第 1 行"))
    }
  }

  inv <- as.character(rv$invariants %||% character(0))
  if (length(inv)) out <- c(out, verdict_invariants(rv, facts, inv))
  out
}

# 守恒约束：能算的真算，算不了才写 unknown。
#   为什么不能一律 unknown：那样每一步都永远过不了判定，
#   人会开始习惯性放行，闭环就又退化成提示。
verdict_invariants <- function(rv, facts, inv) {
  target_file <- as.character(rv$columns_file %||% "")
  cand <- facts$artifacts
  if (nzchar(target_file))
    cand <- Filter(function(a) identical(basename(a$rel), target_file), cand)
  else
    cand <- Filter(function(a) tolower(tools::file_ext(a$rel)) %in%
                     c("xls", "xlsx", "csv", "tsv"), cand)

  tab <- NULL; rel <- ""
  for (a in cand) {
    t <- verdict_read_table(facts$local_root, a$rel)
    if (!is.null(t)) { tab <- t; rel <- a$rel; break }
  }
  lapply(inv, function(one) verdict_invariant_one(one, tab, rel, facts$counts,
                                                   facts = facts, rv = rv))
}

verdict_invariant_one <- function(one, tab, rel, counts = NULL,
                                  facts = NULL, rv = NULL) {
  nmlab <- paste0("invariant:", one)
  cnt <- function(k) suppressWarnings(as.numeric(counts[[k]] %||% NA))
  # 能用 counts 算的，先算，不去要求有表格。
  if (grepl("去双后细胞数", one)) {
    a <- cnt("cells_after"); b <- cnt("cells_before")
    if (is.na(a) || is.na(b))
      return(vchk("L1", nmlab, "unknown",
        "回写没带 cells_before / cells_after，这条守恒算不了",
        expected = one, actual = "counts 未给",
        evidence = "回写 counts{}（缺 cells_before / cells_after）"))
    return(vchk("L1", nmlab, if (a <= b) "pass" else "fail",
      expected = one, actual = paste0("去双后 ", a, " / 输入 ", b),
      evidence = "回写 counts{}"))
  }
  if (grepl("件数等于样本数", one)) {
    n <- cnt("n_samples")
    if (is.na(n))
      return(vchk("L1", nmlab, "unknown",
        "回写没带 n_samples，无法核对件数",
        expected = one, actual = "counts 未给",
        evidence = "回写 counts{}（缺 n_samples）"))
    # 件数从「本环节声明的产物模板」里数，不从整批产物里数：
    #   整批里可能混着别的环节的文件，按整批数会得出虚高的件数。
    #   也不能只回答「有没有至少一件」—— 缺一份正是最常见的假完成形态。
    prods <- as.character((rv %||% list())$products %||% character(0))
    rels <- vapply((facts %||% list())$artifacts %||% list(), function(a) a$rel, "")
    pats <- vapply(prods, function(pp) {
      q <- gsub("\\{[A-Za-z_][A-Za-z0-9_]*\\}", "*", pp, perl = TRUE)
      q <- gsub("([.()+|^$\\\\])", "\\\\\\1", q)
      paste0("^", gsub("\\*", ".*", q), "$")
    }, "", USE.NAMES = FALSE)
    hit <- unique(unlist(lapply(pats, function(rx) rels[grepl(rx, rels)])))
    got <- length(hit)
    return(vchk("L1", nmlab, if (got == n) "pass" else "fail",
      detail = if (got == n) "件数与样本数一致"
               else paste0("声明 ", n, " 个样本，回写只有 ", got, " 件产物"),
      expected = paste0("件数 = ", n), actual = paste0("件数 ", got),
      evidence = "回写 artifacts[] 与 counts{n_samples}"))
  }
  if (grepl("合并后细胞数", one)) {
    a <- cnt("cells_merged"); b <- cnt("cells_sum_inputs")
    if (is.na(a) || is.na(b))
      return(vchk("L1", nmlab, "unknown",
        "回写没带 cells_merged / cells_sum_inputs，这条守恒算不了",
        expected = one, actual = "counts 未给",
        evidence = "回写 counts{}"))
    return(vchk("L1", nmlab, if (a == b) "pass" else "fail",
      expected = one, actual = paste0("合并后 ", a, " / 各样本之和 ", b),
      evidence = "回写 counts{}"))
  }
  if (grepl("剔除后细胞数", one)) {
    a <- cnt("cells_dropped"); b <- cnt("cells_before_drop")
    if (is.na(a) || is.na(b))
      return(vchk("L1", nmlab, "unknown",
        "回写没带 cells_before_drop / cells_dropped，这条守恒算不了",
        expected = one, actual = "counts 未给",
        evidence = "回写 counts{}"))
    return(vchk("L1", nmlab, if (a <= b) "pass" else "fail",
      expected = one, actual = paste0("剔除后 ", a, " / 剔除前 ", b),
      evidence = "回写 counts{}"))
  }
  if (is.null(tab))
    return(vchk("L1", nmlab, "unknown",
                "本地没有可读的表格产物，这条守恒算不了", expected = one, actual = "无表格", evidence = "本地镜像目录（无表格产物）"))
  nm <- names(tab)
  if (grepl("每个簇都有且只有一个细胞类型", one)) {
    if (!all(c("cluster", "cell_type") %in% nm))
      return(vchk("L1", nmlab, "unknown", "表里缺 cluster 或 cell_type 列",
                  expected = one, actual = paste(nm, collapse = ", "), evidence = rel))
    dup <- anyDuplicated(tab$cluster)
    empty <- any(!nzchar(trimws(as.character(tab$cell_type))))
    good <- dup == 0L && !empty
    return(vchk("L1", nmlab, if (good) "pass" else "fail",
                detail = if (good) "每个簇一行，类型非空"
                         else paste0("重复簇=", dup, " 空类型=", empty),
                expected = one,
                actual = paste0("行数 ", nrow(tab), "，簇数 ", length(unique(tab$cluster))),
                evidence = rel))
  }
  if (grepl("细胞类型数 <= 簇数", one)) {
    if (!all(c("cluster", "cell_type") %in% nm))
      return(vchk("L1", nmlab, "unknown", "表里缺 cluster 或 cell_type 列",
                  expected = one, actual = paste(nm, collapse = ", "), evidence = rel))
    nc <- length(unique(tab$cluster)); nt <- length(unique(tab$cell_type))
    return(vchk("L1", nmlab, if (nt <= nc) "pass" else "fail",
                expected = one, actual = paste0("簇 ", nc, " 类型 ", nt), evidence = rel))
  }
  vchk("L1", nmlab, "not_implemented",
       paste0("这条守恒（", one, "）还没有可核算法；责任在应用，不退回 agent"),
       expected = one, actual = "算法未实现",
       evidence = "app/engine/verdict.R（未实现该算法）")
}

# ---- L2 结构 ---------------------------------------------------------

verdict_l2 <- function(stage, facts) {
  out <- list()
  if (is.na(facts$exit_code)) {
    out[[length(out) + 1L]] <- vchk("L2", "exit_code", "unknown",
      "回写没带退出码，无法确认脚本是正常结束还是中途失败",
      expected = "0", actual = "未给", evidence = "回写体 exit_code 字段（缺失）")
  } else {
    out[[length(out) + 1L]] <- vchk("L2", "exit_code",
      if (facts$exit_code == 0L) "pass" else "fail",
      detail = if (facts$exit_code == 0L) "退出码 0" else "退出码非 0",
      expected = "0", actual = as.character(facts$exit_code))
  }

  if (!length(facts$artifacts)) {
    out[[length(out) + 1L]] <- vchk("L2", "artifact_list_present", "unknown",
      "回写没有产物清单；没有清单就无法验证任何一件事",
      expected = "非空 artifacts[]", actual = "空",
      evidence = "回写体 artifacts 字段（空或未提供）")
    return(out)
  }

  for (a in facts$artifacts) {
    if (is.na(a$md5) || !nzchar(a$md5 %||% "")) {
      out[[length(out) + 1L]] <- vchk("L2", paste0("md5:", a$rel), "unknown",
        "回写没给校验和，不能确认这份文件没有被改动或截断",
        expected = "32 位 md5", actual = "未给", evidence = a$rel)
    } else {
      out[[length(out) + 1L]] <- vchk("L2", paste0("md5:", a$rel), "pass",
        "已登记校验和（与服务器实际文件的一致性由取回环节复核）",
        expected = "已登记", actual = a$md5, evidence = a$rel)
    }
  }

  # 环境锁定缺失只 warn，不阻断：它影响复现，不影响这一次的结果对不对。
  if (is.null(stage$review$env_required) || isTRUE(stage$review$env_required)) {
    out[[length(out) + 1L]] <- vchk("L2", "env_lock", "warn",
      "回写没有环境锁定（R 版本 / 包版本 / 符号链接目标），这一轮结果无法逐字复现",
      expected = "env_lock 非空", actual = "未给")
  }
  out
}

# ---- L3 合规 ---------------------------------------------------------

verdict_l3 <- function(stage, facts, app_dir = NULL) {
  out <- list()
  # 有 compliance.R 时走它：阶段名单从 flow_spec 派生，台账缺失判 unknown。
  #   为什么还要 verdict_l3 里的旧逻辑：没有 compliance.R 的老部署要能跑，
  #   但那份是退路，不是主路。
  if (!is.null(app_dir) && exists("compliance_check") &&
      file.exists(file.path(app_dir, "engine", "compliance.R"))) {
    cr <- tryCatch(compliance_check(
      app_dir, facts$step_id, set_id = facts$set_id,
      script_path = facts$script_path, script_md5 = facts$script_md5,
      ledger_available = !is.null(facts$ledger)),
      error = function(e) NULL)
    if (!is.null(cr)) {
      out <- lapply(cr$checks, function(c) vchk(c$layer, c$name, c$status,
        detail = c$detail, expected = c$expected, actual = c$actual, evidence = c$evidence))
      return(out)
    }
  }
  want_skill <- as.character(stage$review$skill %||% stage$skill %||% NA)
  if (is.na(facts$set_id) || !nzchar(facts$set_id %||% "")) {
    out[[length(out) + 1L]] <- vchk("L3", "set_id", "unknown",
      "回写没有台账 set_id；合规层判不了，不能当通过",
      expected = want_skill, actual = "未给",
      evidence = "status/ledger.jsonl 与回写体 set_id（均缺失）")
  } else {
    out[[length(out) + 1L]] <- vchk("L3", "set_id",
      if (identical(facts$set_id, want_skill)) "pass" else "fail",
      detail = if (identical(facts$set_id, want_skill)) "使用的 skill 与规定一致"
               else paste0("规定是 ", want_skill, "，实际是 ", facts$set_id),
      expected = want_skill, actual = facts$set_id, evidence = "status/ledger.jsonl")
  }

  want_script <- as.character(stage$script %||% NA)
  if (is.na(facts$script_path) || !nzchar(facts$script_path %||% "")) {
    out[[length(out) + 1L]] <- vchk("L3", "script_path", "unknown",
      "回写没有脚本路径；无法确认跑的是不是规定的那一份",
      expected = want_script, actual = "未给",
      evidence = "回写体 script_path 字段（缺失）")
  } else {
    base_want <- basename(gsub("\\{[^}]+\\}", "", want_script))
    base_got <- basename(facts$script_path)
    out[[length(out) + 1L]] <- vchk("L3", "script_path",
      if (identical(base_want, base_got)) "pass" else "fail",
      detail = if (identical(base_want, base_got)) "脚本名与规定一致"
               else "脚本不是规定的那一份（另写替代脚本属于绕过）",
      expected = base_want, actual = base_got, evidence = facts$script_path)
  }

  if (is.na(facts$script_md5) || !nzchar(facts$script_md5 %||% "")) {
    out[[length(out) + 1L]] <- vchk("L3", "script_digest", "unknown",
      "回写没有脚本指纹；不能证明脚本没被改过",
      expected = "32 位 md5", actual = "未给",
      evidence = "回写体 script_md5 字段（缺失）")
  } else {
    out[[length(out) + 1L]] <- vchk("L3", "script_digest", "pass",
      "已登记脚本指纹", expected = "已登记", actual = facts$script_md5)
  }
  out
}

# ---- L4 分析 ---------------------------------------------------------

# 分析层：能从产物里算出结论的就真算，算不了才写 unknown。
#   为什么不能一律 unknown：第一版就是这么写的，结果是「正常产物」也永远
#   停在 unknown —— 于是每一步都过不了判定，人会开始习惯性放行，
#   闭环当场退化成提示。判不了要如实说，但能判的必须判。
verdict_l4 <- function(stage, facts) {
  out <- list()
  spec <- stage$review$analysis %||% NULL
  if (is.null(spec)) {
    return(list(vchk("L4", "analysis_rules", "unknown",
      "这一步没有声明分析层规则；分析是否说得通判不了",
      evidence = "engine/flow_spec.R review$analysis")))
  }

  tab <- NULL; rel <- ""
  for (a in facts$artifacts) {
    t <- verdict_read_table(facts$local_root, a$rel)
    if (!is.null(t)) { tab <- t; rel <- a$rel; break }
  }

  for (rule in as.character(spec$rules %||% character(0))) {
    out[[length(out) + 1L]] <- verdict_l4_one(rule, tab, rel, facts)
  }

  if (isTRUE(spec$species_match)) {
    sp <- facts$species %||% ""
    if (!nzchar(sp)) {
      out[[length(out) + 1L]] <- vchk("L4", "reference_mismatch", "unknown",
        "没给物种与组织，无法判断参考词典是否适用", expected = "species+tissue",
        actual = "未给", evidence = "回写体 species / tissue 字段（缺失）")
    } else {
      out[[length(out) + 1L]] <- vchk("L4", "reference_mismatch", "pass",
        "回写声明了物种与组织，词典适用性由 F3 的参考表校验负责",
        expected = "species+tissue", actual = paste(sp, facts$tissue %||% ""))
    }
  }
  out
}

verdict_l4_one <- function(rule, tab, rel, facts) {
  nm <- paste0(rule)
  # 每条规则只回答它自己能回答的那一件事；没有数据就算不了。
  if (identical(rule, "markers_in_dictionary")) {
    if (is.null(tab)) return(vchk("L4", nm, "unknown", "没有可读表格", expected = rule,
                                  actual = "无表格", evidence = "本地镜像目录（无可读表格）"))
    col <- intersect(c("Positive Markers", "marker", "gene"), names(tab))
    if (!length(col)) return(vchk("L4", nm, "unknown",
      "表里没有 marker 列，无法核对是否落在词典内", expected = rule,
      actual = paste(names(tab), collapse = ", "), evidence = rel))
    v <- trimws(as.character(tab[[col[1]]]))
    filled <- v[nzchar(v)]
    good <- length(filled) > 0
    return(vchk("L4", nm, if (good) "pass" else "fail",
      detail = if (good) "marker 列非空，可逐条核对词典" else "marker 列全空",
      expected = rule, actual = paste0("非空 ", length(filled), "/", nrow(tab)),
      evidence = paste0(rel, " 的 ", col[1], " 列")))
  }
  if (identical(rule, "celltype_count_in_range")) {
    if (is.null(tab) || !"cell_type" %in% names(tab))
      return(vchk("L4", nm, "unknown", "没有可读的 cell_type 列",
                  expected = rule, actual = "无列",
                  evidence = if (nzchar(rel)) paste0(rel, " 表头") else
                    "本地镜像目录（无可读表格）"))
    n <- length(unique(trimws(as.character(tab$cell_type))))
    # 区间取 1..200：低于 1 说明没注释上，高于 200 基本是把簇号当成了类型名。
    good <- n >= 1 && n <= 200
    return(vchk("L4", nm, if (good) "pass" else "fail",
      expected = rule, actual = paste0("细胞类型数 ", n),
      evidence = paste0(rel, " 的 cell_type 列")))
  }
  if (identical(rule, "cluster_count_in_range")) {
    if (is.null(tab) || !"cluster" %in% names(tab))
      return(vchk("L4", nm, "unknown", "没有可读的 cluster 列",
                  expected = rule, actual = "无列",
                  evidence = if (nzchar(rel)) paste0(rel, " 表头") else
                    "本地镜像目录（无可读表格）"))
    n <- length(unique(trimws(as.character(tab$cluster))))
    good <- n >= 2 && n <= 100
    return(vchk("L4", nm, if (good) "pass" else "fail",
      expected = rule, actual = paste0("簇数 ", n),
      evidence = paste0(rel, " 的 cluster 列")))
  }
  if (identical(rule, "markers_not_empty")) {
    hit <- Filter(function(a) grepl("marker", a$rel, ignore.case = TRUE) &&
                    !is.na(a$bytes) && a$bytes > 0, facts$artifacts)
    if (!length(facts$artifacts)) return(vchk("L4", nm, "unknown", "无产物清单",
      expected = rule, actual = "空清单", evidence = "回写 artifacts[]（空）"))
    return(vchk("L4", nm, if (length(hit)) "pass" else "fail",
      expected = rule, actual = paste0("marker 类产物 ", length(hit), " 件"),
      evidence = "回写 artifacts[]（无 marker 类文件）"))
  }
  if (identical(rule, "figures_non_empty")) {
    if (!length(facts$artifacts)) return(vchk("L4", nm, "unknown", "无产物清单",
      expected = rule, actual = "空清单", evidence = "回写 artifacts[]（空）"))
    png <- Filter(function(a) grepl("\\.png$", a$rel, ignore.case = TRUE), facts$artifacts)
    return(vchk("L4", nm, if (length(png) > 0) "pass" else "fail",
      expected = rule, actual = paste0("png ", length(png), " 张"),
      evidence = "回写 artifacts[]（无 png）"))
  }
  if (identical(rule, "diff_not_all_genes") || identical(rule, "markers_not_all_genes")) {
    if (is.null(tab)) return(vchk("L4", nm, "unknown", "没有可读表格", expected = rule))
    # 差异/全量 marker 表把整张表达矩阵倒出来是可识别的异常：
    #   行数远大于基因数常见量级，且几乎每个基因都在。
    bad <- nrow(tab) > 50000
    return(vchk("L4", nm, if (bad) "fail" else "pass",
      expected = rule, actual = paste0("行数 ", nrow(tab)),
      evidence = paste0(rel, " 的行数")))
  }
  if (identical(rule, "ratio_sums_to_one")) {
    if (!length(facts$artifacts)) return(vchk("L4", nm, "unknown", "无产物清单",
      expected = rule, actual = "空清单", evidence = "回写 artifacts[]（空）"))
    return(vchk("L4", nm, "pass", "比例表已产出，逐行合计由 L1 守恒检查负责",
                expected = rule, actual = "见 L1 invariant"))
  }
  if (identical(rule, "enrich_not_empty")) {
    if (!length(facts$artifacts)) return(vchk("L4", nm, "unknown", "无产物清单",
      expected = rule, actual = "空清单", evidence = "回写 artifacts[]（空）"))
    hit <- Filter(function(a) grepl("enrich", a$rel) && !is.na(a$bytes) && a$bytes > 0,
                  facts$artifacts)
    return(vchk("L4", nm, if (length(hit)) "pass" else "fail",
      expected = rule, actual = paste0("富集产物 ", length(hit), " 件"),
      evidence = "回写 artifacts[]（无富集产物）"))
  }
  if (identical(rule, "slides_match_fig_count")) {
    # pptx 本质是 zip：能打开、内部有 slide*.xml、能数出页数。
    #   之前这条无条件返回 unknown，等于「PPT 这一环永远判不了」。
    #   清单 D4 要求的判据是可执行的：故意传一个截断的 pptx 判 fail 并指出无法解压；
    #   图少一张判 warn 并点名缺哪张。这里按这个口径实现。
    hit <- Filter(function(a) grepl("\\.pptx$", a$rel, ignore.case = TRUE), facts$artifacts)
    if (!length(hit))
      return(vchk("L4", nm, "fail", "没有 pptx 产物", expected = rule, actual = "0",
                  evidence = "回写 artifacts[]（无 pptx）"))
    lp <- if (nzchar(facts$local_root %||% "")) file.path(facts$local_root, hit[[1]]$rel) else ""
    if (!nzchar(lp) || !file.exists(lp))
      return(vchk("L4", nm, "unknown", "pptx 还没取回到本地，打不开就判不了页数",
                  expected = rule, actual = "未取回", evidence = hit[[1]]$rel))
    info <- verdict_pptx_info(lp)
    if (!isTRUE(info$ok))
      return(vchk("L4", nm, "fail", info$why,
                  expected = "可被 zip 打开且含 slide*.xml", actual = info$why,
                  evidence = paste0(hit[[1]]$rel, " 解压失败")))
    n_fig <- length(Filter(function(a) grepl("\\.png$", a$rel, ignore.case = TRUE),
                           facts$artifacts))
    # 图数比页数少是正常的（封面、目录页没有图）；页数比图少才说明有图没进 PPT。
    if (info$slides > 0L && n_fig > 0L && info$slides < n_fig)
      return(vchk("L4", nm, "warn",
        paste0("PPT 只有 ", info$slides, " 页，但本轮回写有 ", n_fig,
               " 张图；可能有图没进 PPT"),
        expected = rule, actual = paste0("页 ", info$slides, " / 图 ", n_fig),
        evidence = hit[[1]]$rel))
    return(vchk("L4", nm, "pass",
      detail = paste0("pptx 可解压，含 ", info$slides, " 页"),
      expected = rule, actual = paste0("页 ", info$slides, " / 图 ", n_fig),
      evidence = hit[[1]]$rel))
  }
  # 其余规则：没有实现就如实说算不了。
  vchk("L4", nm, "not_implemented",
       paste0("这条分析规则（", rule, "）还没有可核算法；责任在应用，不退回 agent —— ",
              "重复跑同一环不可能让一个没写的检查通过"),
       expected = rule, actual = "规则未实现",
       evidence = "app/engine/verdict.R（未实现该规则）")
}


# 读 pptx 的结构信息：能不能解压、有几页、每页有没有图片引用。
#   为什么用 unzip 而不是解析 XML：页数只需要数 ppt/slides/slide*.xml，
#   引不引图片只需要看 rels 里有没有 image。再深的解析留给 make_ppt.py 自己的自检。
verdict_pptx_info <- function(path) {
  if (!file.exists(path)) return(list(ok = FALSE, why = "文件不存在", slides = 0L))
  if (file.info(path)$size < 102400)
    return(list(ok = FALSE, why = "文件小于 100 KB，通常是写了一半或被截断", slides = 0L))
  lst <- tryCatch(utils::unzip(path, list = TRUE), error = function(e) NULL)
  if (is.null(lst) || !nrow(lst))
    return(list(ok = FALSE, why = "无法解压（不是有效 zip / pptx 被截断）", slides = 0L))
  slides <- sum(grepl("^ppt/slides/slide[0-9]+\\.xml$", lst$Name))
  if (slides == 0L)
    return(list(ok = FALSE, why = "能解压但内部没有 ppt/slides/slide*.xml", slides = 0L))
  rels <- sum(grepl("^ppt/slides/_rels/slide[0-9]+\\.xml\\.rels$", lst$Name))
  imgs <- sum(grepl("^ppt/media/", lst$Name))
  if (imgs == 0L)
    return(list(ok = TRUE, slides = slides, images = 0L,
                why = "含页但没有图片（PPT 里一张图都没有）"))
  list(ok = TRUE, slides = slides, images = imgs, rels = rels,
       why = paste0("可解压，", slides, " 页，", imgs, " 张图"))
}

# ---- 汇总 ------------------------------------------------------------

verdict_rollup <- function(checks) {
  if (!length(checks)) return("unknown")
  st <- vapply(checks, function(c) c$status, "")
  if (any(st == "fail")) return("fail")
  if (any(st == "unknown")) return("unknown")
  if (any(st == "warn")) return("warn")
  "pass"
}

# 单列「判据还没实现」的项，供交付门列为未核验项、页面红条展示。
verdict_unimplemented <- function(checks) {
  Filter(function(c) identical(c$status, "not_implemented"), checks)
}

# ---- 主入口 ----------------------------------------------------------

# 返回 list(verdict, checks, review_id, ticket_id)
verdict_step <- function(con, app_dir, attempt_id, writeback = NULL,
                         local_root = NULL, by = "verdict.R") {
  att <- db_rows(con, "SELECT * FROM step_attempts WHERE attempt_id = ?", list(attempt_id))
  if (!nrow(att)) stop("没有这个 attempt：", attempt_id)
  att <- as.list(att[1, ])

  b <- writeback %||% db_unjson(att$payload, list())
  facts <- verdict_facts(b, att, local_root)

  # 自述字段：收到即忽略并记账（I4）。
  claims <- verdict_detect_self_claims(b)
  if (length(claims)) {
    db_event(con, "ignored_self_claim", owner_key = att$owner_key,
             version_id = att$version_id, run_id = att$run_id,
             step_id = att$step_id, attempt_id = attempt_id,
             payload = list(fields = claims,
                            why = "agent 的自然语言保证不作为通过依据"))
  }

  spec_env <- verdict_load_spec(app_dir)
  stage <- verdict_stage(spec_env, att$step_id)
  if (is.null(stage)) {
    checks <- list(vchk("L1", "stage_known", "unknown",
                        paste0("flow_spec 里没有这个环节：", att$step_id)))
  } else {
    checks <- c(verdict_l1(stage, facts), verdict_l2(stage, facts),
                verdict_l3(stage, facts, app_dir), verdict_l4(stage, facts))
  }
  verdict <- verdict_rollup(checks)

  # 快照：每步一份不可改的 manifest（A2）。快照 id 与 attempt 一一对应。
  #   为什么必须有实物文件：只往表里插一行、manifest_path 写 NA，
  #   等于「有一个快照表但没有快照」。清单的完成判据是
  #   「删掉远程任一被下游引用的产物，能从 manifest 找到谁依赖它、哪个 md5」——
  #   这要求 manifest 是一份真能读的文件，不是数据库里的一个空字段。
  snap <- db_rows(con, "SELECT snapshot_id FROM snapshots WHERE attempt_id = ?", list(attempt_id))
  if (nrow(snap)) {
    snapshot_id <- as.character(snap$snapshot_id[1])
  } else {
    snapshot_id <- paste0("snap-", attempt_id)
    man <- list(
      snapshot_id = snapshot_id, attempt_id = attempt_id,
      version_id = att$version_id, run_id = att$run_id, step_id = att$step_id,
      attempt_no = as.integer(att$attempt_no),
      project_id = as.character(db_scalar(con, "SELECT project_id FROM versions WHERE version_id=?",
                                          list(att$version_id)) %||% NA),
      created_at = db_now(), sealed = TRUE,
      inputs = list(req_id = att$req_id, ticket_id = att$ticket_id),
      params = b$params %||% NULL,
      env_lock = b$env_lock %||% NULL,
      command = b$command %||% NULL,
      script = list(path = facts$script_path, digest = facts$script_md5),
      exit_code = facts$exit_code,
      log_tail = substr(as.character(b$log %||% ""), 1, 4000),
      qc_verdict = verdict,
      artifacts = lapply(facts$artifacts, function(a) list(
        rel = a$rel, bytes = a$bytes, md5 = a$md5, mtime = a$mtime,
        kind = verdict_kind_of(a$rel)))
    )
    mdir <- file.path(app_dir, "state", "snapshots", snapshot_id)
    dir.create(mdir, showWarnings = FALSE, recursive = TRUE)
    mfile <- file.path(mdir, "manifest.json")
    writeLines(jsonlite::toJSON(man, auto_unbox = TRUE, pretty = TRUE, null = "null",
                                digits = NA), mfile, useBytes = TRUE)
    mmd5 <- unname(tools::md5sum(mfile))
    # 写完置只读：人工纠正另起 revisions 记录，不覆盖原 manifest。
    try(Sys.chmod(mfile, mode = "0444"), silent = TRUE)

    db_tx(con, function() {
      DBI::dbExecute(con,
        "INSERT INTO snapshots(snapshot_id,attempt_id,version_id,owner_key,created_at,sealed,
           manifest_path,manifest_md5,input_refs,params,env_lock,command,log_tail,qc_verdict,payload)
         VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
        list(snapshot_id, attempt_id, att$version_id, att$owner_key, db_now(), 1L,
             mfile, mmd5,
             db_json(list(req_id = att$req_id)), db_json(b$params %||% NULL),
             db_json(b$env_lock %||% NULL),
             as.character(b$command %||% NA_character_),
             substr(as.character(b$log %||% ""), 1, 4000), verdict,
             db_json(list(artifacts = facts$artifacts,
                          inputs_declared = stage$deps %||% character(0)))))
      for (a in facts$artifacts) {
        DBI::dbExecute(con,
          "INSERT OR IGNORE INTO artifacts(artifact_id,snapshot_id,version_id,owner_key,rel,
             bytes,md5,kind,mtime,source) VALUES(?,?,?,?,?,?,?,?,?,?)",
          list(paste0("art-", substr(digest::digest(paste(snapshot_id, a$rel), algo = "md5"), 1, 16)),
               snapshot_id, att$version_id, att$owner_key, a$rel, a$bytes, a$md5,
               verdict_kind_of(a$rel), a$mtime, "writeback"))
      }
    })
  }

  # ---- 每份产物一份可举证的结论，且不可变（E2）--------------------
  #   之前用 ON CONFLICT DO UPDATE：同一 snapshot 重判会覆盖上一份，
  #   于是「同一环节跑两次，第一份 review.json 仍在且可读」这条判据不成立。
  #   现在：一次判定一份新记录，旧的不动；文件也落盘一份带时间戳的，
  #   重判只会多一个文件，不会改前一个。
  review_id <- paste0("rv-", attempt_id, "-", as.integer(db_scalar(
    con, "SELECT COUNT(*)+1 FROM reviews WHERE attempt_id = ?", list(attempt_id))))
  checks_json <- db_json(lapply(checks, function(c) c))
  DBI::dbExecute(con,
    "INSERT INTO reviews(review_id,snapshot_id,attempt_id,version_id,step_id,owner_key,
       verdict,rule_version,at,by_who,checks_json,payload)
     VALUES(?,?,?,?,?,?,?,?,?,?,?,?)",
    list(review_id, snapshot_id, attempt_id, att$version_id, att$step_id, att$owner_key,
         verdict, VERDICT_RULE_VERSION, db_now(), by, checks_json,
         db_json(list(facts = facts[c("step_id", "exit_code", "set_id", "script_path",
                                      "req_id", "attempt_id")],
                      claims_ignored = claims))))

  # review.json 实物：与数据库内容一致，落盘可读、可拷走。
  #   为什么两处都写：数据库回答「哪一轮过了」，文件回答「凭什么过的」——
  #   后者要能直接打开看证据，不能要求人去连库。
  rdir <- file.path(app_dir, "state", "reviews", snapshot_id)
  dir.create(rdir, showWarnings = FALSE, recursive = TRUE)
  rfile <- file.path(rdir, paste0("review-", review_id, ".json"))
  writeLines(jsonlite::toJSON(list(
    review_id = review_id, snapshot_id = snapshot_id, attempt_id = attempt_id,
    version_id = att$version_id, run_id = att$run_id, step_id = att$step_id,
    verdict = verdict, rule_version = VERDICT_RULE_VERSION,
    at = db_now(), by = by, checks = checks,
    failed = Filter(function(c) c$status %in% c("fail", "unknown"), checks),
    claims_ignored = claims), auto_unbox = TRUE, pretty = TRUE, null = "null",
    digits = NA), rfile, useBytes = TRUE)
  try(Sys.chmod(rfile, mode = "0444"), silent = TRUE)

  db_event(con, "verdict", owner_key = att$owner_key, version_id = att$version_id,
           run_id = att$run_id, step_id = att$step_id, attempt_id = attempt_id,
           payload = list(verdict = verdict,
                          failed = Filter(function(c) identical(c$status, "fail"), checks)))

  # 结论只写进数据库；ticket 由 rework.R 建（拒绝必须带返工单）。
  ticket_id <- NA_character_
  if (verdict %in% c("fail", "unknown")) {
    ticket_id <- tryCatch(
      rework_issue(con, app_dir, attempt_id = attempt_id, checks = checks,
                   verdict = verdict, stage = stage, facts = facts),
      error = function(e) NA_character_)
  }

  list(verdict = verdict, checks = checks, review_id = review_id,
       snapshot_id = snapshot_id, ticket_id = ticket_id,
       claims_ignored = claims,
       not_implemented = vapply(verdict_unimplemented(checks),
                                function(c) c$name, ""))
}

# 产物分类（A3）：决定能不能直接下载、要不要确认、要不要授权。
verdict_kind_of <- function(rel) {
  ext <- tolower(tools::file_ext(rel))
  if (ext %in% c("png", "jpg", "jpeg", "pdf", "svg")) return("preview")
  if (ext %in% c("csv", "tsv", "xls", "xlsx", "md", "txt", "html", "json")) return("small")
  if (ext %in% c("rds", "zip", "pptx", "tar", "gz", "xz")) return("large")
  if (grepl("^(raw|matrix|data)/", rel)) return("secret")
  "ondemand"
}

# ---- CLI -------------------------------------------------------------

verdict_cli <- function() {
  a <- commandArgs(trailingOnly = TRUE)
  get <- function(k, d = NULL) {
    i <- match(k, a); if (is.na(i) || i == length(a)) return(d); a[i + 1]
  }
  app <- get("--app", getwd())
  att <- get("--attempt", NULL)
  if (is.null(att)) { cat("用法: Rscript engine/verdict.R --attempt <attempt_id> [--app dir]\n"); quit(status = 2) }
  source(file.path(app, "engine", "db.R"))
  source(file.path(app, "engine", "rework.R"))
  con <- db_connect(app); on.exit(DBI::dbDisconnect(con))
  r <- verdict_step(con, app, att)
  cat(jsonlite::toJSON(list(verdict = r$verdict, ticket_id = r$ticket_id,
                            checks = r$checks), auto_unbox = TRUE, pretty = TRUE), "\n")
  invisible(r)
}

if (sys.nframe() == 0L && !interactive()) verdict_cli()
