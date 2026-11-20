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

# 派工凭证模块：set_id 由应用派工侧给，不由执行端自报。
#   单独一份文件，因为它的口径与判定器其余部分不同：
#   别处判的是「回写体说了什么」，这里判的是「应用派工时定了什么，
#   以及执行端报的脚本证据能不能与那条派工记录对上」。
{
  dc_file <- file.path(dirname(sys.frame(1)$ofile %||% "."), "dispatch_cred.R")
  if (!file.exists(dc_file))
    dc_file <- file.path(getwd(), "engine", "dispatch_cred.R")
  if (!file.exists(dc_file))
    dc_file <- file.path(Sys.getenv("PA_APP_DIR", unset = "."), "engine", "dispatch_cred.R")
  if (file.exists(dc_file)) sys.source(dc_file, envir = globalenv())
  doublet_file <- file.path(dirname(dc_file), "doublet_review.R")
  if (file.exists(doublet_file)) sys.source(doublet_file, envir = globalenv())
}

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
# 取本次会话的筛群产物 md5 基线。
#   为什么必须限定会话：remote_steps 里可能留着多个会话的 filter 记录，
#   挑"最后一条"就会把别的会话的产物当成这次的输入 —— 实测就是这样串的。
#   判据：会话非空、命中唯一、run_dir/输入路径一致；任何一项不成立返回 NULL。
verdict_filter_baseline <- function(app_dir, facts, con = NULL) {
  scalar <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  sid <- facts$session_id
  rd <- facts$run_dir
  if (!scalar(sid) || !scalar(rd) || is.null(con)) return(NULL)
  f <- file.path(app_dir, "state", "remote_steps.json")
  if (!file.exists(f)) return(NULL)
  d <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(d)) return(NULL)
  hits <- Filter(function(x) identical(x$id, "filter") && identical(x$session_id, sid), d$steps %||% list())
  if (length(hits) != 1L) return(NULL)
  rec <- hits[[1]]
  if (!scalar(rec$status) || !rec$status %in% c("done", "verified") ||
      !identical(rec$run_dir, rd) || !scalar(rec$req_id)) return(NULL)
  input_rel <- "7.after_drop/merged_after_drop.rds"
  digest <- function(arts) {
    a <- Filter(function(x) is.list(x) && identical(x$rel, input_rel), arts %||% list())
    if (length(a) != 1L || !scalar(a[[1]]$md5) || !grepl("^[0-9a-f]{32}$", a[[1]]$md5)) return(NULL)
    a[[1]]$md5
  }
  md5 <- digest(rec$artifacts)
  if (is.null(md5)) return(NULL)
  # A reported 'done' alone is not acceptance. Match the verified upstream payload.
  verified <- tryCatch(DBI::dbGetQuery(con,
    paste0("SELECT payload FROM step_attempts WHERE req_id = ? AND step_id = 'filter' ",
           "AND status = 'verified' AND json_extract(payload, '$.session_id') = ? ",
           "ORDER BY verified_at DESC, attempt_no DESC LIMIT 1"),
    params = list(rec$req_id, sid)), error = function(e) NULL)
  if (is.null(verified) || nrow(verified) != 1L) return(NULL)
  payload <- tryCatch(jsonlite::fromJSON(verified$payload[[1]], simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(payload) || !identical(payload$run_dir, rd) || !identical(digest(payload$artifacts), md5)) return(NULL)
  md5
}

# 取应用侧派工参数，并把命中的任务与当前 attempt 做完整身份绑定。
#   为什么必须绑全套：只看 exec_id 是不够的 —— 同一个 id 若被重复登记
#   （并发派工、手工改档、旧档残留），随便挑一条都可能拿到别的会话的参数，
#   于是「页面选的是 A」这句话就不成立了。
#   任何一项对不上、或命中不唯一，一律返回 NULL：宁可 unknown，
#   也不能拿一份来路不明的派工记录去证明产物口径正确。
# 图的守恒基准：从已验收上游读，不硬编码、不重读 RDS。
#   来源三处，各自都要与它自己的产物 md5 对得上：
#     · 细胞总数：已验收 filter 的 cells_before_drop（也应等于 final 的 cells_total）
#     · 未定型细胞数与已定型类型集合：已验收 annotate 的 annotation_evidence.counts
#     · 逐样本基准：已验收 final 的逐样本表结构
#   任何一处取不到就返回 NULL 字段，判据会据此写 unknown，不折成通过。
verdict_figs_upstream <- function(app_dir, sid = "", con = NULL, sg_wb = NULL) {
  out <- list(total_cells = NA_real_, unassigned_cells = NA_real_,
              typed_types = character(0), unassigned_label = "unassigned",
              type_counts = NULL, sample_group = NULL,
              total_source = NA_character_, ann_source = NA_character_,
              samples_source = NA_character_)
  if (is.null(con) || !nzchar(sid)) return(out)
  get_verified <- function(step) {
    row <- tryCatch(DBI::dbGetQuery(con,
      paste0("SELECT payload FROM step_attempts WHERE step_id = ? AND status = 'verified' ",
             "AND json_extract(payload, '$.session_id') = ? ",
             "ORDER BY verified_at DESC LIMIT 1"), list(step, sid)),
      error = function(e) NULL)
    if (is.null(row) || !nrow(row)) return(NULL)
    tryCatch(jsonlite::fromJSON(row$payload[[1]], simplifyVector = FALSE),
             error = function(e) NULL)
  }
  # 总数优先级：final 的终版合计 → annotate 合计 → filter。
  #   为什么 filter 放最后：在删群流程里 cells_before_drop 与终版对象可能不同，
  #   拿它当基准会把"终版本来就少"误判成缺细胞。
  ann <- get_verified("annotate")
  ae <- (ann %||% list())$annotation_evidence %||% NULL
  cnt <- (ae %||% list())$counts %||% NULL
  if (!is.null(cnt) && length(cnt$rows)) {
    # 绑定：这份 counts 必须就是本次注释登记的那张表，md5 逐字相等。
    rel0 <- as.character(cnt$rel %||% ""); md0 <- as.character(cnt$md5 %||% "")
    art0 <- Filter(function(a) identical(as.character(a$rel %||% ""), rel0),
                   (ann %||% list())$artifacts %||% list())
    amd0 <- if (length(art0)) as.character(art0[[1]]$md5 %||% "") else ""
    if (nzchar(md0) && identical(md0, amd0)) {
      ct <- vapply(cnt$rows, function(r) as.character(r$celltype %||% ""), "")
      nc <- suppressWarnings(as.numeric(vapply(cnt$rows, function(r)
        as.character(r$n_cells %||% ""), "")))
      tp <- vapply(cnt$rows, function(r) as.character(r$typed %||% ""), "")
      u <- ct %in% c("unassigned", "NA", "unknown")
      if (any(u)) out$unassigned_cells <- sum(nc[u], na.rm = TRUE)
      out$typed_types <- ct[tp == "yes"]
      # 逐类型计数：比例表要按类型逐个与上游对齐，只比"类型是否出现"不够。
      tc <- suppressWarnings(as.numeric(nc)); names(tc) <- ct
      if (all(!is.na(tc))) out$type_counts <- tc
      out$ann_source <- paste0("verified annotate counts ", md0)
      if (is.na(out$total_cells)) {
        out$total_cells <- sum(nc, na.rm = TRUE)
        out$total_source <- "verified annotate counts 合计"
      }
    }
  }
  fin <- get_verified("final")
  fs <- (fin %||% list())$final_sums %||% NULL
  if (!is.null(fs)) {
    out$samples_source <- paste0("verified final ",
                                 as.character(fs$samples_table_rel %||% ""))
    # 终版合计才是这一批图的对象；filter 的删除前计数可能不等于它。
    ft <- suppressWarnings(as.numeric(fs$cells_total %||% NA))
    if (is.na(ft)) ft <- suppressWarnings(as.numeric(fs$samples_cells_sum %||% NA))
    if (!is.na(ft)) {
      out$total_cells <- ft
      out$total_source <- "verified final final_sums.cells_total"
    }
  }
  # filter 仅在前两处都取不到时兜底
  if (is.na(out$total_cells)) {
    flt <- get_verified("filter")
    tot <- suppressWarnings(as.numeric((flt$counts %||% list())$cells_before_drop %||% NA))
    if (!is.na(tot)) {
      out$total_cells <- tot
      out$total_source <- "verified filter counts.cells_before_drop"
    }
  }
  # sample→group 映射：来自执行端实读的上游快照，md5 必须等于已验收 final
  #   登记的那张逐样本表。对不上就不用 —— 换组也能过的那类漏检就是这么来的。
  sg_rows <- NULL; sg_rel <- ""; sg_md5 <- ""
  tb_fin <- ((fin %||% list())$tables %||% list())[[as.character(fs$samples_table_rel %||% "")]]
  if (is.list(tb_fin) && length(tb_fin$data %||% list())) {
    # 已验收 final 自己的表结构里就带着逐行，直接用，不再要求外部快照。
    sg_rows <- lapply(tb_fin$data, function(r) r)
    sg_rel <- as.character(fs$samples_table_rel %||% "")
    sg_md5 <- as.character(tb_fin$md5 %||% "")
  }
  # 回写体带回的映射优先：执行端在采集时就读了上游那张表。
  #   采信条件与快照完全一致：rel 与 md5 必须等于已验收 final 登记的值，
  #   并且该条目确实出现在 final 的 artifacts 里 —— 只信"能对上的证据"，
  #   不因为它是执行端带回来的就默认可信。
  if (is.null(sg_rows) && is.list(sg_wb) && length(sg_wb$rows %||% list())) {
    sg_rows <- sg_wb$rows
    sg_rel <- as.character(sg_wb$rel %||% "")
    sg_md5 <- as.character(sg_wb$md5 %||% "")
  }
  # 快照按 session 隔离，避免多分析共用一份人工文件；
  #   只有 md5 与已验收 final 登记的一致才采信。
  sid_tag <- gsub("[^A-Za-z0-9._-]", "_", as.character(sid %||% ""))
  cand <- if (nzchar(sid_tag))
    file.path(app_dir, "state", "upstream", sid_tag, "sample_group.json") else ""
  if (is.null(sg_rows) && nzchar(cand) && file.exists(cand)) {
    sg0 <- tryCatch(jsonlite::fromJSON(paste(readLines(cand, warn = FALSE), collapse = "\n"),
                                       simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(sg0) && length(sg0$rows)) {
      sg_rows <- sg0$rows; sg_rel <- as.character(sg0$source_rel %||% "")
      sg_md5 <- as.character(sg0$source_md5 %||% "")
    }
  }
  sg_f <- ""
  if (!is.null(fs) && !is.null(sg_rows) && length(sg_rows)) {
    want_md5 <- as.character(fs$samples_table_md5 %||% "")
    want_rel <- as.character(fs$samples_table_rel %||% "")
    art_hit <- Filter(function(a) identical(as.character(a$rel %||% ""), want_rel) &&
                        identical(tolower(as.character(a$md5 %||% "")), tolower(want_md5)),
                      (fin %||% list())$artifacts %||% list())
    if (nzchar(want_md5) && identical(sg_md5, want_md5) && identical(sg_rel, want_rel) &&
        length(art_hit) > 0) {
      sm <- vapply(sg_rows, function(r) as.character(r$sample %||% ""), "")
      gr <- vapply(sg_rows, function(r) as.character(r$group %||% ""), "")
      if (all(nzchar(sm)) && all(nzchar(gr)) && length(sm) == length(gr))
        out$sample_group <- setNames(gr, sm)
    }
  }
  out
}

verdict_dispatch_params <- function(app_dir, b, facts) {
  exf <- file.path(app_dir, "state", "exec_tasks.json")
  if (!file.exists(exf)) return(NULL)
  # 自己读文件，不依赖 serve.R 里的读函数：这段要能独立被测。
  txt <- tryCatch(paste(readLines(exf, warn = FALSE, encoding = "UTF-8"),
                        collapse = "\n"), error = function(e) "")
  if (!nzchar(txt)) return(NULL)
  ex <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  items <- ex$items %||% list()
  if (!length(items)) return(NULL)

  want_id <- trimws(as.character(b$exec_id %||% ""))
  want_rq <- trimws(as.character(b$req_id %||% ""))
  want_sid <- trimws(as.character(b$session_id %||% facts$session_id %||% ""))
  want_step <- trimws(as.character(facts$step_id %||% b$step %||% ""))
  want_rd <- trimws(as.character(b$run_dir %||% facts$run_dir %||% ""))

  # 命中必须唯一：0 条没有基线；多条说明同 id 异常重复，不猜。
  hit <- Filter(function(it) {
    id <- trimws(as.character(it$id %||% ""))
    rq <- trimws(as.character(it$req_id %||% ""))
    if (nzchar(want_id)) identical(id, want_id)
    else nzchar(want_rq) && identical(rq, want_rq)
  }, items)
  if (length(hit) != 1L) return(NULL)
  it <- hit[[1]]

  got_sid <- trimws(as.character(it$session_id %||% ""))
  got_step <- trimws(as.character(it$step %||% ""))
  got_rq <- trimws(as.character(it$req_id %||% ""))
  p <- it$params
  if (!is.list(p) || !length(p)) return(NULL)
  bd <- p$binding

  # 筛群走新协议：四项必须非空且逐项相等。
  #   为什么不再用「双方非空才比」：那样缺 req、缺 binding、缺 run_dir
  #   都会被静默跳过，等于用缺失冒充一致。新协议从一开始就带全这四项。
  # 新协议：filter 与 final 都要求四项非空且逐项相等。
  #   final 是这次新加的入队分支，从一开始就带全四项，
  #   没有历史包袱，不该走「缺字段可跳过」的旧协议。
  if (want_step %in% c("filter", "final")) {
    if (!all(nzchar(c(want_sid, want_step, want_rq, want_rd)))) return(NULL)
    if (!identical(got_sid, want_sid)) return(NULL)
    if (!identical(got_step, want_step)) return(NULL)
    if (!identical(got_rq, want_rq)) return(NULL)
    if (!is.list(bd) || !length(bd)) return(NULL)
    b_sid <- trimws(as.character(bd$session_id %||% ""))
    b_rq <- trimws(as.character(bd$req_id %||% ""))
    b_step <- trimws(as.character(bd$step %||% ""))
    b_rd <- trimws(as.character(bd$run_dir %||% ""))
    if (!all(nzchar(c(b_sid, b_rq, b_step, b_rd)))) return(NULL)
    if (!identical(b_sid, want_sid)) return(NULL)
    if (!identical(b_rq, want_rq)) return(NULL)
    if (!identical(b_step, want_step)) return(NULL)
    if (!identical(b_rd, want_rd)) return(NULL)
    return(p)
  }

  # 旧协议（doublet 等）：历史上有的任务没有 binding 块。
  #   保留兼容，但仍在有值时逐项核对；缺值只影响 run_dir 这一项。
  if (!nzchar(want_sid) || !nzchar(want_step)) return(NULL)
  if (!identical(got_sid, want_sid)) return(NULL)
  if (!identical(got_step, want_step)) return(NULL)
  if (nzchar(want_rq) && nzchar(got_rq) && !identical(got_rq, want_rq)) return(NULL)
  if (is.list(bd) && length(bd)) {
    b_sid <- trimws(as.character(bd$session_id %||% ""))
    b_rq <- trimws(as.character(bd$req_id %||% ""))
    b_step <- trimws(as.character(bd$step %||% ""))
    b_rd <- trimws(as.character(bd$run_dir %||% ""))
    if (nzchar(b_sid) && !identical(b_sid, want_sid)) return(NULL)
    if (nzchar(b_rq) && nzchar(want_rq) && !identical(b_rq, want_rq)) return(NULL)
    if (nzchar(b_step) && !identical(b_step, want_step)) return(NULL)
    if (nzchar(want_rd) && nzchar(b_rd) && !identical(b_rd, want_rd)) return(NULL)
  } else if (nzchar(want_rd)) {
    return(NULL)
  }
  p
}

# 把「任意一层目录」归一到应用根：认 engine/verdict.R 这个标记文件。
#   优先用进程里的 APP_DIR（serve.R 的来源就是这个），再不行逐级向上找。
#   找不到就原样返回 —— 不猜路径，宁可判 unknown 也不拿别的地方顶。
annotation_app_root <- function(app_dir) {
  cand <- as.character(app_dir %||% "")
  is_root <- function(p) nzchar(p) && file.exists(file.path(p, "engine", "verdict.R"))
  if (exists("APP_DIR", mode = "function")) {
    a <- tryCatch(APP_DIR(), error = function(e) "")
  } else if (exists("APP_DIR")) {
    a <- tryCatch(as.character(APP_DIR), error = function(e) "")
  } else a <- ""
  if (is_root(a)) return(a)
  p <- cand
  for (i in 1:8) {
    if (is_root(p)) return(p)
    nxt <- dirname(p)
    if (identical(nxt, p) || !nzchar(nxt)) break
    p <- nxt
  }
  cand
}

annotation_baseline <- function(app_dir, session_id = "") {
  # 取本次注释的可信参考基线：来源是应用侧会话与参考登记，不是回写体。
  # 入参 app_dir 未必是应用根：判定器被调用时传下来的是那次尝试的目录
  #   （…/sessions/<sid>/attempts/<req>/verify-xxxx），它下面没有 state/sessions.json，
  #   于是 sess_find 永远找不到记录、reference_id 永远为空 ——
  #   实测后果就是本轮这样：注释产物完全合格，却因为「没有可信参考表基线」
  #   被两条 L4 判 unknown，整步退回。这里先把它归一到应用根，再往下走。
  app_dir <- annotation_app_root(app_dir)
  sid <- as.character(session_id %||% "")
  s0 <- if (exists("sess_find"))
    tryCatch(sess_find(app_dir, sid), error = function(e) NULL) else NULL
  rid <- as.character((s0 %||% list())$reference_id %||% "")
  idxf <- file.path(app_dir, "state", "references", "index.json")
  idx <- tryCatch(jsonlite::fromJSON(read_text_any(idxf), simplifyVector = FALSE),
                  error = function(e) NULL)
  items <- (idx %||% list())$items %||% list()
  hit <- Filter(function(it) identical(as.character(it$id %||% ""), rid), items)
  it <- hit[[1]]
  pth <- as.character(it$server_path %||% "")
  dg <- as.character(it$digest %||% "")
  # 上传的参考表用登记里的真实文件算 md5；服务端条目没有 digest 就判 unknown。
  #   不再按 basename 去存档里猜同名文件 —— 存档名是 ref-<时间>-<随机>.xlsx，
  #   与 Excel 原名不同，猜中与否取决于命名巧合，不能作为证据。
  if (!grepl("^[0-9a-f]{32}$", dg)) {
    cand <- ""
    st <- as.character(it$stored %||% "")
    rp <- as.character(it$ref_path %||% "")
    if (nzchar(st) && file.exists(file.path(app_dir, st))) cand <- file.path(app_dir, st)
    else if (nzchar(rp) && file.exists(rp)) cand <- rp
    if (nzchar(cand)) {
      md <- tryCatch(unname(tools::md5sum(cand)), error = function(e) "")
      if (grepl("^[0-9a-f]{32}$", md)) dg <- md else dg <- ""
    } else {
      # 服务端引用且登记无 digest：宁可 unknown，也不拿别的东西顶。
      dg <- ""
    }
  }
  # 上传件的 server_path 恒为空串：它原本就不在服务器上。派工时应用按
  #   flow_vars 的 reference_xlsx 把它下发到 <work_root>/0.reference/reference.<ext>，
  #   判定器必须按同一规则还原，否则这类分析永远卡在「没有可信参考基线」。
  #   还原不出来（没有 run_dir）就仍旧返回 NULL，绝不拿别的表顶上。
  if (!nzchar(pth)) {
    ext <- tolower(tools::file_ext(as.character(it$name %||% "")))
    if (!nzchar(ext)) ext <- "xlsx"
    wr <- ""
    if (exists("sess_find")) {
      s1 <- tryCatch(sess_find(app_dir, sid), error = function(e) NULL)
      wr <- as.character((s1 %||% list())$run_dir %||% "")
    }
    if (nzchar(wr)) pth <- file.path(wr, "0.reference", paste0("reference.", ext))
  }
  list(reference_id = rid, source_path = pth, source_md5 = dg)
}

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
    # 身份必须透传：下游要用它把证据限定在「本次会话」，
    #   不透传就会退化成"全局挑最后一条"，串到别的会话上去。
    session_id = as.character(b$session_id %||% attempt_row$session_id %||% ""),
    exec_id = as.character(b$exec_id %||% ""),
    run_dir = as.character(b$run_dir %||% ""),
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
    # tables：执行端在集群侧读出的表结构（列名 / 行数 / 簇数）。
    #   为什么要它：判定器跑在应用主机上，看不到集群的 /share；
    #   把 local_root 设成集群路径也读不到 —— 只能由有权访问集群的那一端读好带回来。
    tables = b$tables %||% NULL,
    doublet_evidence = b$doublet_evidence %||% NULL,
    # 图可解码的证据：PNG 签名与宽高，由执行端读文件头带回。
    #   字节数只能证明"有内容"，证明不了"能解码"，两件事不能互相顶替。
    png_probe = b$png_probe %||% NULL,
    # 富集 0 条时的真实原因（脚本自己写的状态行），用来区分
    #   「成功但没有显著结果」与「执行失败/未产出」。
    enrich_status = b$enrich_status %||% NULL,
    # 状态表逐行内容：判定要核每个类型 × 两种分析，只有行数不够。
    enrich_status_rows = b$enrich_status_rows %||% NULL,
    # 执行端实读的 diff 输入清单（work_root 相对路径 + md5），
    #   不能只让状态表自己说输入是谁。
    enrich_inputs = b$enrich_inputs %||% NULL,
    # 注释环的固定结构证据（执行端实读产物）。
    #   规则本身不在这里判，交给 engine/annotation_review.R；
    #   这里只负责把它带进 facts，别在传递环节丢字段。
    annotation_evidence = b$annotation_evidence %||% NULL,
    # objects：执行端读出的真实对象结构（簇列表/簇数/Harmony 维度与有限值），
    #   每条都带该文件自身的 md5，判定时与回写清单对齐。
    #   为什么要它：L4 要看对象里到底有什么，而判定器跑在应用主机，
    #   读不到集群的 /share。
    objects = b$objects %||% NULL,
    # 筛群口径证据：派工参数 / 参数文件 / 生效记录表，三方比对用。
    filter_params = b$filter_params %||% NULL,
    pptx_structure = b$pptx_structure %||% NULL,
    # 终版两张表的合计与来源：不在这里透传，下游永远只看得到 NULL，
    #   守恒就只能一直 unknown —— 实测就是这个结果。
    final_sums = b$final_sums %||% NULL,
    # 终版参数文件与输入对象：用来核「页面选择是否真的生效」。
    final_params = b$final_params %||% NULL,
    # 交付 zip 的条目结构：应用主机打不开集群文件，只能由执行端带回来。
    zip_structure = b$zip_structure %||% NULL,
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

# Only explicitly registered audit files may be excluded from sample RDS counting.
verdict_audit_paths <- function(rv, facts) {
  if (!identical(facts$step_id, "doublet")) return(character(0))
  templates <- as.character(rv$audit_products %||% character(0))
  samples <- as.character(facts$declared_samples %||% character(0))
  unique(unlist(lapply(templates, function(p) {
    if (grepl("{samples}", p, fixed = TRUE))
      vapply(samples, function(s) gsub("{samples}", s, p, fixed = TRUE), "")
    else p
  }), use.names = FALSE))
}

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
  # 目录型产物由规范显式声明，不靠扩展名猜。
  #   为什么不猜："10.figs/1.cluster" 的点号后面像扩展名，
  #   按扩展名判就会把目录当文件，回写里明明有 4 张 png 也说缺件。
  #   声明归声明，仍然要求该目录下真的有条目，不是"所有产品都当任意前缀"。
  dirs_decl <- as.character(rv$product_dirs %||% character(0))
  for (p in prods) {
    # 兼容两种：声明了 product_dirs 的阶段只认声明；
    #   没声明的阶段保留旧行为（末尾没有扩展名就按目录处理）。
    #   为什么不能一刀切改成"只认声明"：enrich 的 products 是
    #   "10.figs/4.diff_enrich/enrich"，没有声明过，改严之后下一环必判缺目录 ——
    #   修好 figs 却坏了下一环，那是用一处判据换另一处误判。
    is_dir <- grepl("/$", p) || (p %in% dirs_decl) ||
      (!length(dirs_decl) && !grepl("\\.[A-Za-z0-9]+$", p))
    # 支持简单通配：{samples} 这类模板占位符在判定时不该被当成文件名。
    #   为什么必须支持：doublet 的产物是「每个样本一个 rds」，名字在流程定义里
    #   只能写成模板；如果拿模板字面量去比对，真实回写永远对不上，
    #   于是一个完全正常的 doublet 产物也会被判缺件 —— 这是判据自己写错，
    #   不是产物有问题。判据写错比产物错更危险：它会把对的判成错的。
    pat <- gsub("\\{[A-Za-z_][A-Za-z0-9_]*\\}", "*", p, perl = TRUE)
    to_regex <- function(x) paste0("^", gsub("\\*", ".*",
      gsub("([.()+|^$\\\\])", "\\\\\\1", x)), "$")
    rx <- to_regex(pat)
    # 目录型：只看"该目录下有条目"，边界必须是完整的目录段，
    #   不接受 "1.clusterX" 这种仅仅是字符串前缀的巧合。
    under_dir <- if (is_dir) any(startsWith(got, paste0(sub("/+$", "", p), "/"))) else FALSE
    present <- any(got == p) || any(grepl(rx, got)) || under_dir
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
    # 基准必须是「应用侧声明的样本清单」，不是回写体自报的 samples[]。
    #   为什么（2026-11-13 隔离实测）：原来拿 b$samples 当基准，
    #   于是只回写 5 份、samples 也只写 5 个，逐样本核对全过、accepted=true。
    #   少报两个就只验两个，缺件这个判据等于不存在。
    #   声明清单来自 config/server_profile.json 的 server.samples，
    #   那是派工时就定了的「应该有哪几个样本」。
    declared <- as.character(facts$declared_samples %||% character(0))
    if (length(declared)) {
      missing <- setdiff(declared, smp)
      extra <- setdiff(smp, declared)
      out[[length(out) + 1L]] <- vchk("L1", "samples_count",
        if (!length(missing) && !length(extra)) "pass" else "fail",
        detail = if (!length(missing) && !length(extra))
                   paste0("回写样本与声明清单一致（", length(declared), " 个）")
                 else paste0("缺：", paste(missing, collapse = ", "),
                             "；多：", paste(extra, collapse = ", ")),
        expected = paste(declared, collapse = ", "),
        actual = if (length(smp)) paste(smp, collapse = ", ") else "（空清单）",
        evidence = "config/server_profile.json 的 server.samples 与回写 samples[]")
      # 核对基准换成声明清单：缺的那几个也要逐个数出来
      if (length(missing)) smp <- unique(c(smp, missing))
    }
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
        # 只对样本产物（.rds）判断"改名顶替"。
        #   为什么：这一步除了每个样本一个 rds，还会写一张辅助统计表。
        #   统计表的文件名本来就不在样本清单里，拿它当"冒充产物"是把对的判成错的。
        #   辅助表另有一条按表结构核验的检查，不会因此漏放。
        extra <- Filter(function(x) tolower(tools::file_ext(x)) == "rds", extra)
        extra <- setdiff(extra, verdict_audit_paths(rv, facts))
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

  # 辅助表：不套 rds 的最小体积下限，改按表结构核验。
  #   判据来自阶段规范里显式列明的 aux_tables，不是凭扩展名猜。
  aux <- rv$aux_tables %||% list()
  aux_rels <- vapply(aux, function(x) as.character(x$rel %||% ""), "")
  for (one in aux) {
    rel <- as.character(one$rel %||% "")
    if (!nzchar(rel)) next
    if (!(rel %in% got)) {
      out[[length(out) + 1L]] <- vchk("L1", paste0("aux_table:", rel), "fail",
        "阶段规范里列明的辅助表没有出现在回写清单里",
        expected = rel, actual = if (length(got)) paste(got, collapse = ", ") else "（空清单）",
        evidence = "engine/flow_spec.R 的 aux_tables")
      next
    }
    # 阶段规范声明了 no_struct 的，不做表结构核验。
    #   原来不读这个字段，JSON 参数文件会掉进「逐样本表」分支，
    #   永远 unknown —— 而它该由 final_params_consistent 逐字段核。
    if (isTRUE(one$no_struct)) {
      out[[length(out) + 1L]] <- vchk("L1", paste0("aux_table:", rel), "pass",
        detail = "阶段规范声明此文件不做表结构核验，内容另由专门规则核",
        expected = "no_struct", actual = "已按声明跳过结构核验",
        evidence = "engine/flow_spec.R 的 aux_tables[no_struct]")
      next
    }
    # 结构核验分两种，按阶段声明决定，不靠猜：
    #   ① 声明了 columns / rows_from / min_rows 的，按列名与行数核；
    #   ② 没声明的保持原行为（逐样本表：行数 = 声明样本数）。
    #   为什么必须分开：筛群的两张表不是逐样本表，套样本口径会永远判失败。
    want_cols <- as.character(one$columns %||% character(0))
    rows_from <- as.character(one$rows_from %||% "")
    min_rows <- suppressWarnings(as.integer(one$min_rows %||% NA))
    if (length(want_cols) || nzchar(rows_from) || !is.na(min_rows)) {
      tb <- (facts$tables %||% list())[[rel]]
      hdr <- NULL
      from_report <- FALSE
      if (is.list(tb) && length(tb$columns)) {
        hdr <- as.character(unlist(tb$columns))
        from_report <- TRUE
      }
      if (is.null(hdr)) {
        h <- verdict_read_header(facts$local_root, rel)
        if (!is.null(h)) hdr <- h
      }
      if (is.null(hdr)) {
        out[[length(out) + 1L]] <- vchk("L1", paste0("aux_table:", rel), "unknown",
          "这份辅助表的结构没取到，列名与行数都核不了",
          expected = if (length(want_cols)) paste(want_cols, collapse = ", ") else "列名与行数",
          actual = "未取回", evidence = rel)
        next
      }
      miss <- setdiff(want_cols, hdr)
      if (length(miss)) {
        out[[length(out) + 1L]] <- vchk("L1", paste0("aux_table:", rel), "fail",
          detail = paste0("缺列：", paste(miss, collapse = ", ")),
          expected = paste(want_cols, collapse = ", "),
          actual = paste(hdr, collapse = ", "), evidence = rel)
        next
      }
      # 行数：能拿到上报行数就核；声明了 rows_from 还要与 counts 对齐。
      rows_have <- suppressWarnings(as.integer(tb$rows %||% NA))
      rows_want <- NA_integer_
      if (nzchar(rows_from))
        rows_want <- suppressWarnings(as.integer((facts$counts %||% list())[[rows_from]] %||% NA))
      if (!is.na(min_rows) && (is.na(rows_have) || rows_have < min_rows)) {
        out[[length(out) + 1L]] <- vchk("L1", paste0("aux_table:", rel), "fail",
          detail = if (is.na(rows_have))
                     paste0("没读到行数，无法确认至少 ", min_rows, " 行")
                   else paste0("只有 ", rows_have, " 行，少于应有的 ", min_rows, " 行"),
          expected = paste0(">= ", min_rows), actual = as.character(rows_have),
          evidence = rel)
        next
      }
      if (!is.na(rows_want)) {
        # 行数不一致是明确的错，判 fail；只有「根本没读到行数」才 unknown。
        #   把不一致降级成 unknown，等于让一张张数不对的表继续往下走。
        ok_rows <- !is.na(rows_have) && rows_have == rows_want
        out[[length(out) + 1L]] <- vchk("L1", paste0("aux_table:", rel),
          if (ok_rows) "pass" else if (is.na(rows_have)) "unknown" else "fail",
          detail = if (ok_rows)
                     paste0("列名齐全、行数与 counts.", rows_from, " 一致（", rows_have, " 行）")
                   else if (is.na(rows_have))
                     paste0("没读到行数，无法与 counts.", rows_from, " = ", rows_want, " 比对")
                   else paste0("行数 ", rows_have, " 与 counts.", rows_from, " = ",
                               rows_want, " 不一致"),
          expected = paste0("行数 = counts.", rows_from, "（", rows_want, "）"),
          actual = as.character(rows_have),
          evidence = paste0(rel, if (from_report) "（执行端读表得到）" else "（本地表头）"))
        next
      }
      # 声明了 rows_from 却拿不到对应计数：不能当通过。
      #   原行为会落到下面的「列名齐全」pass，等于把「没核对行数」
      #   说成「核过了没问题」—— 与 vanish 的守恒检查同性质。
      if (nzchar(rows_from) && is.na(rows_want)) {
        out[[length(out) + 1L]] <- vchk("L1", paste0("aux_table:", rel), "unknown",
          detail = paste0("回写没带 counts.", rows_from,
                          "，无法核对这张表的行数是否等于上游"),
          expected = paste0("行数 = counts.", rows_from),
          actual = paste0("未给（表内 ", if (is.na(rows_have)) "?" else as.character(rows_have), " 行）"),
          evidence = paste0(rel, " 与回写 counts{}"))
        next
      }
      out[[length(out) + 1L]] <- vchk("L1", paste0("aux_table:", rel), "pass",
        detail = paste0("列名齐全（", length(hdr), " 列）"),
        expected = paste(want_cols, collapse = ", "),
        actual = paste(hdr, collapse = ", "),
        evidence = paste0(rel, if (from_report) "（执行端读表得到）" else "（本地表头）"))
      next
    }

    # 结构核验：回写体里带的行数必须等于声明样本数，且逐样本都在
    per <- (facts$counts %||% list())$per_sample_before %||% NULL
    n_decl <- length(as.character(facts$declared_samples %||% character(0)))
    n_have <- suppressWarnings(as.integer((facts$counts %||% list())$n_samples %||% NA))
    if (is.null(per) || is.na(n_have)) {
      out[[length(out) + 1L]] <- vchk("L1", paste0("aux_table:", rel), "unknown",
        "回写没带这张表的逐样本读数，结构核验做不了",
        expected = "counts.n_samples 与 counts.per_sample_before",
        actual = "未给", evidence = "回写 counts{}")
    } else {
      keys <- as.character(names(per))
      ok_rows <- !is.na(n_decl) && n_decl > 0 && n_have == n_decl && length(keys) == n_decl
      ok_names <- length(facts$declared_samples %||% character(0)) == 0 ||
                  setequal(keys, as.character(facts$declared_samples))
      out[[length(out) + 1L]] <- vchk("L1", paste0("aux_table:", rel),
        if (ok_rows && ok_names) "pass" else "fail",
        detail = if (ok_rows && ok_names)
                   paste0("表结构与样本对齐（", n_have, " 行）")
                 else paste0("行数/样本名与声明不一致：行数 ", n_have,
                             "，声明 ", n_decl, "，表内样本 ", length(keys)),
        expected = paste0(as.character(facts$declared_samples), collapse = ", "),
        actual = paste(keys, collapse = ", "),
        evidence = "回写 counts.per_sample_before（由执行端读该表得到）")
    }
  }

  # rds 类产物单独一个下限。
  #   为什么不能与表格共用一个数：128 字节对几百 MB 的对象等于没门槛，
  #   一个空壳 rds 也能过；反过来把表格按 rds 的 1MB 门槛卡又必判失败。
  mb_rds <- suppressWarnings(as.numeric(rv$min_bytes_rds %||% NA))
  if (!is.na(mb_rds)) {
    for (a in facts$artifacts) {
      if (tolower(tools::file_ext(a$rel)) != "rds") next
      if (a$rel %in% verdict_audit_paths(rv, facts)) next
      if (is.na(a$bytes)) {
        out[[length(out) + 1L]] <- vchk("L1", paste0("min_bytes_rds:", a$rel),
          "unknown", "回写没给字节数，无法判断 rds 体积",
          expected = paste0(">= ", mb_rds), actual = "未给", evidence = a$rel)
      } else if (a$bytes < mb_rds) {
        out[[length(out) + 1L]] <- vchk("L1", paste0("min_bytes_rds:", a$rel),
          "fail", "rds 小于该阶段声明的最小体积，通常是空壳或写了一半",
          expected = paste0(">= ", mb_rds), actual = as.character(a$bytes),
          evidence = a$rel)
      }
    }
  }

  mb <- suppressWarnings(as.numeric(rv$min_bytes %||% NA))
  if (!is.na(mb)) {
    # 体积下限只对"本步声明的主产物"生效。
    #   ppt 的 manifest 是随行的来源清单，9.6KB 本来就有余；
    #   套 pptx 的 100KB 下限会把一份正常交付判成"写了一半"。
    #   判据用 products 精确名、目录前缀与模板通配，不做任意前后缀放宽。
    prods_mb <- as.character(rv$products %||% character(0))
    in_scope <- function(rel) {
      if (rel %in% verdict_audit_paths(rv, facts)) return(FALSE)
      if (!length(prods_mb)) return(TRUE)
      for (p0 in prods_mb) {
        if (identical(rel, p0)) return(TRUE)
        if (startsWith(rel, paste0(sub("/+$", "", p0), "/"))) return(TRUE)
        if (grepl("\\{[A-Za-z_][A-Za-z0-9_]*\\}", p0)) {
          rx <- paste0("^", gsub("\\*", ".*", gsub("\\{[A-Za-z_][A-Za-z0-9_]*\\}",
                                                  "*", gsub("([.()+|^$\\\\])", "\\\\\\1", p0))), "$")
          if (grepl(rx, rel)) return(TRUE)
        }
      }
      FALSE
    }
    for (a in facts$artifacts) {
      # 已列明的辅助表不套这个下限：它是纯文本统计表，几百字节是正常的。
      if (a$rel %in% aux_rels) next
      # 规范显式列出的跳过名单：清单类文件本来就只有几 KB。
      if (a$rel %in% as.character(rv$min_bytes_skip %||% character(0))) next
      if (!in_scope(a$rel)) next
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
      from_report <- FALSE
      if (is.null(hdr)) {
        # 本地读不到就看执行端上报的结构。
        #   为什么不算放宽：上报来自有权访问集群的那一端，且必须与回写清单里
        #   同一份产物对应；两处都没有才判 unknown。
        tb <- (facts$tables %||% list())[[a$rel]]
        if (is.list(tb) && length(tb$columns)) {
          hdr <- as.character(unlist(tb$columns))
          from_report <- TRUE
        }
      }
      if (is.null(hdr)) {
        out[[length(out) + 1L]] <- vchk("L1", paste0("columns:", a$rel), "unknown",
          "本地没有这份文件，回写也没带这张表的结构，读不到表头",
          expected = paste(cols, collapse = ", "),
          actual = "未取回", evidence = a$rel)
        next
      }
      miss <- setdiff(cols, hdr)
      out[[length(out) + 1L]] <- vchk("L1", paste0("columns:", a$rel),
        if (length(miss)) "fail" else "pass",
        detail = if (length(miss)) paste0("缺列：", paste(miss, collapse = ", "))
                 else "必需列齐全",
        expected = paste(cols, collapse = ", "),
        actual = paste(hdr, collapse = ", "),
        evidence = if (from_report)
                     paste0(a$rel, " 表头（由执行端在集群侧读取并上报）")
                   else paste0(a$rel, " 表头第 1 行"))
    }
  }

  # 小表守恒审计：由规范声明（tables_audit），逐表核结构、键、跨表汇总与上游绑定。
  #   为什么不套 min_bytes：这几张表本来就几百字节，套下限会把对的判成错的。
  if (identical(as.character(rv$tables_audit %||% ""), "figs_tables")) {
    # 声明了必须核的表审计却没加载模块，不能当作"这一步没有这项检查"——
    #   静默跳过等于让坏证据直接过关。
    if (exists("figs_tables_audit", mode = "function")) {
      out <- c(out, figs_tables_audit(facts, facts$upstream, facts$declared_samples))
    } else {
      out[[length(out) + 1L]] <- vchk("L1", "figs_tables_audit", "fail",
        "这一步声明了表守恒审计，但审计模块未加载（engine/figs_audit.R），坏证据无法排除",
        expected = "figs_tables_audit 可用", actual = "模块缺失",
        evidence = "engine/figs_audit.R")
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

# 注释环的逐簇记录守恒：每簇恰一条记录，允许明确未定型但必须有原因。
#   为什么不用「类型非空」：脚本对无法判定的簇写 NA 并在 reason 里说明，
#   把 12 个真实未定型改写成 0 才是失真。契约是「记录齐全 + 未定型有据」。
verdict_annot_cluster_rows <- function(facts, counts, nmlab, one) {
  cnt <- function(k) suppressWarnings(as.numeric((counts %||% list())[[k]] %||% NA))
  ae <- (facts %||% list())$annotation_evidence %||% NULL
  rows <- (ae %||% list())$result$rows %||% NULL
  clus <- (ae %||% list())$object$clusters %||% NULL
  txt <- function(v) {
    if (is.null(v) || length(v) == 0) return("")
    if (is.list(v)) v <- v[[1]]
    v <- suppressWarnings(as.character(v))
    if (!length(v) || is.na(v[[1]])) "" else trimws(v[[1]])
  }
  if (is.list(rows) && length(rows) && is.list(clus) && length(clus)) {
    rid <- vapply(rows, function(x) txt(x$cluster), "")
    cid <- vapply(clus, function(x) txt(x$cluster), "")
    cty <- vapply(rows, function(x) txt(x$celltype), "")
    rsn <- vapply(rows, function(x) txt(x$reason), "")
    if (any(!nzchar(rid)) || any(!nzchar(cid)))
      return(vchk("L1", nmlab, "unknown", "簇号为空，无法逐簇核对",
        expected = one, actual = "空簇号", evidence = "回写 annotation_evidence"))
    if (anyDuplicated(rid))
      return(vchk("L1", nmlab, "fail",
        detail = paste0("结果表有重复簇行：",
                        paste(unique(rid[duplicated(rid)]), collapse = ", ")),
        expected = one,
        actual = paste0(length(rid), " 行 / ", length(unique(rid)), " 簇"),
        evidence = "回写 annotation_evidence.result.rows"))
    miss <- setdiff(cid, rid)
    if (length(miss))
      return(vchk("L1", nmlab, "fail",
        detail = paste0("结果表缺簇：", paste(miss, collapse = ", ")),
        expected = one, actual = paste0("缺 ", length(miss), " 簇"),
        evidence = "回写 annotation_evidence"))
    extra <- setdiff(rid, cid)
    if (length(extra))
      return(vchk("L1", nmlab, "fail",
        detail = paste0("结果表有对象里没有的簇：", paste(extra, collapse = ", ")),
        expected = one, actual = paste0("多 ", length(extra), " 行"),
        evidence = "回写 annotation_evidence"))
    untyped <- rid[!nzchar(cty)]
    bad <- untyped[!nzchar(rsn[match(untyped, rid)])]
    if (length(bad))
      return(vchk("L1", nmlab, "fail",
        detail = paste0("未定型但没有给原因：", paste(bad, collapse = ", ")),
        expected = one, actual = paste0(length(bad), " 簇"),
        evidence = "回写 annotation_evidence.result.rows"))
    return(vchk("L1", nmlab, "pass",
      detail = paste0(length(rid), " 簇每簇恰一条记录；", sum(nzchar(cty)),
                      " 簇有类型，", length(untyped), " 簇明确未定型且原因完整"),
      expected = one,
      actual = paste0("行 ", length(rid), " / 簇 ", length(cid)),
      evidence = "回写 annotation_evidence（执行端实读 annot_result.xls 与对象）"))
  }
  r <- cnt("n_cluster_rows"); nc <- cnt("n_clusters")
  dupn <- cnt("n_dup_cluster_rows"); unx <- cnt("n_unexplained_type_rows")
  if (!is.na(r) && !is.na(nc) && !is.na(dupn) && !is.na(unx)) {
    good <- dupn == 0 && unx == 0 && r == nc
    return(vchk("L1", nmlab, if (good) "pass" else "fail",
      detail = if (good) paste0("每簇恰一行且未定型均有原因（", r, " 行 / ", nc, " 簇）")
               else paste0("重复簇行 ", dupn, "、未解释未定型 ", unx,
                           "、行数 ", r, " vs 簇数 ", nc),
      expected = one, actual = paste0("行数 ", r, "，簇数 ", nc),
      evidence = "回写 counts{}（执行端读 annot_result.xls 得到）"))
  }
  vchk("L1", nmlab, "unknown",
    "既没有逐行注释证据，也没有可判的计数，无法核对每簇记录",
    expected = one, actual = "未给", evidence = "回写 annotation_evidence / counts{}")
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
    hit <- setdiff(hit, verdict_audit_paths(rv, facts))
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
  # 终版两条守恒：只用回写 counts，不要求本地能读表。
  #   为什么不交给 L4 的 ratio_sums_to_one：那条现在只回答「比例表产出了」，
  #   不真算合计。把「合计等于总量」放在守恒层，能算就算，算不了 unknown。
  if (grepl("各样本细胞数合计", one)) {
    a <- cnt("samples_cells_sum"); b <- cnt("cells_total")
    # 两个数分别来自哪份文件、md5 是多少，必须能说清。
    #   只比两个同源合计、却不知道它们属于哪份表，等于没证明守恒。
    # 来源必须绑到本次回写清单里的那份文件：rel 点名、md5 逐字相等。
    #   只判「字段非空」是不够的 —— 随便填一个 hash 也算"有来源"，
    #   那就等于没核。来源不属于本次产物、或 hash 对不上，一律拒绝采信。
    fs <- (facts %||% list())$final_sums
    # 两份来源必须是这两张指定的表，不能是同一份、也不能互换。
    #   只核「在产物清单里」是不够的：拿 top15_markers.xls 同时充当两份来源、
    #   hash 也对得上，就会一路通过 —— 但那根本没证明细胞数守恒。
    EXP_S <- "8.final/cell_count_by_sample.xls"
    EXP_R <- "8.final/cell_ratio_by_cluster.xls"
    src_of <- function(relk, md5k, expect) {
      if (!is.list(fs)) return(list(ok = FALSE, why = "缺 final_sums"))
      rel <- as.character(fs[[relk]] %||% "")
      md5 <- as.character(fs[[md5k]] %||% "")
      if (!nzchar(rel) || !nzchar(md5))
        return(list(ok = FALSE, why = "source_missing"))
      if (!identical(rel, expect))
        return(list(ok = FALSE,
                    why = paste0("来源不是本阶段指定表：", rel, "（应为 ", expect, "）")))
      hit <- Filter(function(x) identical(as.character(x$rel %||% ""), rel),
                    facts$artifacts %||% list())
      if (!length(hit))
        return(list(ok = FALSE, why = paste0("来源不在本次产物清单：", rel)))
      am <- as.character(hit[[1]]$md5 %||% "")
      if (!nzchar(am) || !identical(md5, am))
        return(list(ok = FALSE, why = paste0("md5 与产物不符：", rel)))
      list(ok = TRUE, rel = rel, md5 = md5)
    }
    sc <- src_of("samples_table_rel", "samples_table_md5", EXP_S)
    rc <- src_of("ratio_table_rel", "ratio_table_md5", EXP_R)
    # 区分「没给」与「给错了」：
    #   两份来源都根本没填 → unknown（核不了）；
    #   只要填了但对不上指定表或 hash 不符 → fail（这是错，不是缺）。
    #   把「填错」降级成 unknown，等于让同源替代一路滑过去。
    # 「完全没给 final_sums」也算缺，不是错：
    #   错是指填了但对不上；没填就是核不了。
    is_missing <- function(w) identical(w, "source_missing") || identical(w, "缺 final_sums")
    miss_both <- !isTRUE(sc$ok) && !isTRUE(rc$ok) &&
      is_missing(sc$why) && is_missing(rc$why)
    if ((!is.na(a) || !is.na(b)) && miss_both)
      return(vchk("L1", nmlab, "unknown",
        "两个合计缺少可核来源（rel 与 md5），无法确认来自本次终版统计表",
        expected = one, actual = "缺来源", evidence = "回写 final_sums{}"))
    if ((!is.na(a) || !is.na(b)) && !isTRUE(sc$ok))
      return(vchk("L1", nmlab, "fail",
        detail = paste0("样本合计的来源不可采信：", sc$why),
        expected = one, actual = as.character(sc$why),
        evidence = "回写 final_sums{}"))
    if ((!is.na(a) || !is.na(b)) && !isTRUE(rc$ok))
      return(vchk("L1", nmlab, "fail",
        detail = paste0("簇合计的来源不可采信：", rc$why),
        expected = one, actual = as.character(rc$why),
        evidence = "回写 final_sums{}"))
    if (is.na(a) || is.na(b))
      return(vchk("L1", nmlab, "unknown",
        "回写没带 samples_cells_sum / cells_total，这条守恒算不了",
        expected = one, actual = "counts 未给",
        evidence = "回写 counts{}（由执行端读 8.final 两张统计表得到）"))
    return(vchk("L1", nmlab, if (a == b) "pass" else "fail",
      expected = one, actual = paste0("各样本合计 ", a, " / 总数 ", b),
      evidence = "回写 counts{}"))
  }
  if (grepl("各簇比例合计", one)) {
    v <- cnt("cluster_pct_sum")
    if (is.na(v))
      return(vchk("L1", nmlab, "unknown",
        "回写没带 cluster_pct_sum，这条守恒算不了",
        expected = one, actual = "counts 未给",
        evidence = "回写 counts{}（由执行端读 cell_ratio_by_cluster.xls 得到）"))
    # 百分比口径允许舍入误差：每簇保留两位，25 个簇最多差 0.25。
    tol <- suppressWarnings(as.numeric((facts %||% list())$pct_tolerance %||% 0.5))
    ok <- abs(v - 100) <= tol
    return(vchk("L1", nmlab, if (ok) "pass" else "fail",
      expected = one, actual = paste0("合计 ", v, "%（容差 ", tol, "）"),
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
  if (grepl("簇数", one) && grepl("行数", one)) {
    # 「簇数 = cluster_stats 行数」：只用回写的 counts，不要求本地能读到那张表。
    #   位置必须在下面的表格守卫之前 —— 守卫会因为应用主机看不到 /share 判 unknown，
    #   把一条本来能算的守恒挡掉。
    a <- cnt("n_clusters"); b <- cnt("cluster_stats_rows")
    if (is.na(a) || is.na(b))
      return(vchk("L1", nmlab, "unknown",
        "回写没带 n_clusters / cluster_stats_rows，这条守恒算不了",
        expected = one, actual = "counts 未给",
        evidence = "回写 counts{}（由执行端在集群侧读 cluster_stats 表得到）"))
    return(vchk("L1", nmlab, if (a == b) "pass" else "fail",
      expected = one, actual = paste0("簇数 ", a, " / 表行数 ", b),
      evidence = "回写 counts{}"))
  }
  # annotate 两条守恒：先用回写 counts，避免被下面的「本地无表」守卫挡成 unknown。
  #   应用主机看不到 /share，本地没有 annot_result.xls 是常态；
  #   执行端读同一张表得到的四个数足以判断，且与文件 md5 一起带回。
  if (grepl("每个簇都有且只有一个细胞类型|每簇有且仅有一条注释记录", one)) {
    return(verdict_annot_cluster_rows(facts, counts, nmlab, one))
  }
  if (grepl("细胞类型数 <= 簇数", one)) {
    k <- cnt("n_celltypes"); nck <- cnt("n_clusters")
    if (!is.na(k) && !is.na(nck))
      return(vchk("L1", nmlab, if (k <= nck) "pass" else "fail",
        expected = one, actual = paste0("类型数 ", k, " / 簇数 ", nck),
        evidence = "回写 counts{}（执行端读 annot_result.xls 得到）"))
  }

  # 两条图守恒：按本次产物清单 + 真实 PNG 证据判，不要求本地能读表。
  #   为什么必须放在表格守卫之前：判定器跑在应用主机，看不到集群 /share，
  #   "本地没有可读表格"是常态；让图证据跟着一起 unknown，
  #   等于图片永远过不了 —— 而图是否真的存在、是否真能解码，
  #   清单与 png_probe 已经答得出来。
  if (grepl("每套图至少一张 png", one)) {
    arts <- (facts %||% list())$artifacts %||% list()
    probe <- (facts %||% list())$png_probe %||% NULL
    dirs <- as.character((rv %||% list())$product_dirs %||% character(0))
    if (!length(dirs)) {
      return(vchk("L1", nmlab, "unknown",
        "这一步没有声明图目录，无法逐套核对", expected = one,
        actual = "未声明 product_dirs", evidence = "engine/flow_spec.R"))
    }
    rels <- vapply(arts, function(a) as.character(a$rel %||% ""), "")
    miss <- character(0); bad <- character(0)
    for (d in dirs) {
      under <- rels[startsWith(rels, paste0(sub("/+$", "", d), "/"))]
      png <- under[grepl("\\.png$", under, ignore.case = TRUE)]
      if (!length(png)) { miss <- c(miss, d); next }
      ok <- vapply(png, function(r) {
        p0 <- (probe %||% list())[[r]]
        if (!is.list(p0) || !isTRUE(p0$signature_ok)) return(FALSE)
        # 签名只证明"能解码"，不证明"就是这一张"。
        #   probe 的 md5 必须与清单里同一条目的 md5 逐字相等，
        #   否则拿旧图的 probe 也能让新图过关。
        am <- Filter(function(a) identical(as.character(a$rel %||% ""), r), arts)
        amd <- if (length(am)) as.character(am[[1]]$md5 %||% "") else ""
        pm <- as.character(p0$md5 %||% "")
        nzchar(amd) && identical(pm, amd)
      }, TRUE)
      if (!all(ok)) bad <- c(bad, d)
    }
    if (length(miss))
      return(vchk("L1", nmlab, "fail",
        paste0("这些图目录里没有任何 png：", paste(miss, collapse = ", ")),
        expected = one, actual = paste(miss, collapse = ","), evidence = "回写 artifacts[]"))
    if (length(bad))
      return(vchk("L1", nmlab, "fail",
        paste0("这些目录里有 png 但签名/尺寸证据不合格：", paste(bad, collapse = ", ")),
        expected = one, actual = paste(bad, collapse = ","), evidence = "回写 png_probe{}"))
    return(vchk("L1", nmlab, "pass",
      paste0("每个图目录都有可解码的 png（", length(dirs), " 套）"),
      expected = one, actual = paste0(length(dirs), " 套齐全"),
      evidence = "回写 artifacts[] 与 png_probe{}"))
  }
  if (grepl("UMAP 与 dotplot", one)) {
    arts <- (facts %||% list())$artifacts %||% list()
    probe <- (facts %||% list())$png_probe %||% NULL
    rels <- vapply(arts, function(a) as.character(a$rel %||% ""), "")
    pick <- function(pat) {
      hit <- rels[grepl(pat, rels, ignore.case = TRUE) & grepl("\\.png$", rels, ignore.case = TRUE)]
      hit[vapply(hit, function(r) {
        p0 <- (probe %||% list())[[r]]
        is.list(p0) && isTRUE(p0$signature_ok)
      }, TRUE)]
    }
    um <- pick("umap"); dp <- pick("dotplot")
    if (!length(um) || !length(dp))
      return(vchk("L1", nmlab, "fail",
        paste0("缺少可用的图：", if (!length(um)) "没有可解码的 UMAP " else "",
               if (!length(dp)) "没有可解码的 dotplot" else ""),
        expected = one,
        actual = paste0("umap ", length(um), " 张 / dotplot ", length(dp), " 张"),
        evidence = "回写 artifacts[] 与 png_probe{}"))
    return(vchk("L1", nmlab, "pass",
      paste0("UMAP ", length(um), " 张、dotplot ", length(dp), " 张，签名与尺寸正常"),
      expected = one, actual = "两类都在", evidence = "回写 artifacts[] 与 png_probe{}"))
  }

  # 富集条目守恒：按回写带回的状态表逐行判，不要求本地能读表。
  #   判据区分三类，不能一刀切：
  #     ok      → 必须真有条目（rows_written >= 1）
  #     no_sig  → 允许 0 条，但必须写清原因（合法空结果，不算失败）
  #     其它/error → 明确失败
  #   为什么不能"任一行有数就通过"：那会放过部分类型静默跳过。
  if (grepl("富集条目非空", one)) {
    rows <- (facts %||% list())$enrich_status_rows %||% NULL
    if (is.null(rows) || !length(rows))
      return(vchk("L1", nmlab, "unknown",
        "回写没带富集状态表逐行内容，这条守恒算不了",
        expected = one, actual = "status_rows 未给", evidence = "回写 enrich_status_rows[]"))
    gv <- function(r, k) {
      v <- as.character((r %||% list())[[k]] %||% "")
      if (!length(v) || is.na(v[1])) "" else trimws(v[1])
    }
    st <- vapply(rows, function(r) tolower(gv(r, "status")), "")
    nw <- suppressWarnings(as.numeric(vapply(rows, function(r) gv(r, "rows_written"), "")))
    rs <- vapply(rows, function(r) gv(r, "reason"), "")
    bad_st <- !(st %in% c("ok", "no_sig") | startsWith(st, "skipped"))
    ok_zero <- st == "ok" & (is.na(nw) | nw < 1)
    nosig_noreason <- st == "no_sig" & !nzchar(rs)
    if (any(bad_st))
      return(vchk("L1", nmlab, "fail",
        paste0("状态表里有非法状态：", paste(unique(st[bad_st]), collapse = ", ")),
        expected = one, actual = paste(unique(st[bad_st]), collapse = ","),
        evidence = "回写 enrich_status_rows[]"))
    if (any(ok_zero))
      return(vchk("L1", nmlab, "fail",
        paste0("标记 ok 但没有任何条目（", sum(ok_zero), " 行）"),
        expected = one, actual = as.character(sum(ok_zero)),
        evidence = "回写 enrich_status_rows[].rows_written"))
    if (any(nosig_noreason))
      return(vchk("L1", nmlab, "fail",
        paste0("no_sig 没写原因（", sum(nosig_noreason), " 行），无法区分空结果与失败"),
        expected = one, actual = as.character(sum(nosig_noreason)),
        evidence = "回写 enrich_status_rows[].reason"))
    tot <- sum(nw, na.rm = TRUE)
    if (tot < 1)
      return(vchk("L1", nmlab, "fail", "所有类型合计 0 条富集结果",
        expected = one, actual = "0", evidence = "回写 enrich_status_rows[]"))
    return(vchk("L1", nmlab, "pass",
      paste0(length(rows), " 行状态全部合规，合计写入 ", tot, " 条（",
             sum(st == "no_sig"), " 行 no_sig）"),
      expected = one, actual = paste0("合计 ", tot), evidence = "回写 enrich_status_rows[]"))
  }

  # PPT 三条守恒：按执行端带回的真实结构判，不套表格口径。
  #   为什么不能走表格守卫：判定器在应用主机，看不到集群 /share，
  #   "本地没有可读表格"对 PPT 永远成立，于是三条会一直 unknown ——
  #   而 pptx 能不能打开、有几页、图有没有真的嵌进去，结构字段已经给出答案。
  if (grepl("pptx 可被 zipfile 打开|slide\\*?\\.xml 数量|图片引用", one)) {
    ppt <- (facts %||% list())$pptx_structure %||% NULL
    if (is.null(ppt) || !length(ppt))
      return(vchk("L1", nmlab, "unknown",
        "回写没带 PPT 结构采证，这条守恒算不了",
        expected = one, actual = "pptx_structure 为空",
        evidence = "回写 pptx_structure{}（verify_delivery.py --step ppt）"))
    gv <- function(k) {
      v <- (ppt %||% list())[[k]]
      if (is.null(v) || !length(v)) return(NA)
      v[[1]]
    }
    why <- as.character(gv("why") %||% "")

    if (grepl("zipfile 打开", one)) {
      crc <- gv("crc_ok")
      if (is.na(crc))
        return(vchk("L1", nmlab, "unknown", "结构里没有 CRC 结论",
          expected = one, actual = "crc_ok 缺失", evidence = "回写 pptx_structure.crc_ok"))
      return(vchk("L1", nmlab, if (isTRUE(crc)) "pass" else "fail",
        if (isTRUE(crc)) "压缩包逐成员 CRC 校验通过，可正常打开"
        else paste0("压缩包校验未通过：", substr(why, 1, 80)),
        expected = one, actual = if (isTRUE(crc)) "CRC OK" else "CRC 失败",
        evidence = "回写 pptx_structure.crc_ok"))
    }

    # 页数分支必须先把"图片引用"那条排除掉：
    #   "每张 slide 至少一个图片引用"里也有 slide 字样，
    #   不排除就会先命中页数分支并直接 return，
    #   图片一致性永远判不到 —— 实测把 source_match_ok 设成 FALSE，
    #   三条 L1 仍然全 pass，就是这条短路造成的。
    if (grepl("slide", one) && !grepl("图片", one)) {
      sl <- suppressWarnings(as.numeric(gv("slides")))
      if (is.na(sl))
        return(vchk("L1", nmlab, "unknown", "结构里没有页数",
          expected = one, actual = "slides 缺失", evidence = "回写 pptx_structure.slides"))
      return(vchk("L1", nmlab, if (sl > 0) "pass" else "fail",
        if (sl > 0) paste0("页 XML 数量 ", sl, " 页，均可在包内解出")
        else "包里没有任何 slide",
        expected = one, actual = as.character(sl),
        evidence = "回写 pptx_structure.slides"))
    }

    # 图片引用：按模板角色判，封面/目次/纯文字页本来不含图，不要求造图。
    sm <- gv("source_match_ok")
    if (is.na(sm))
      return(vchk("L1", nmlab, "unknown",
        "结构里没有逐页图与源图的一致性结论",
        expected = one, actual = "source_match_ok 缺失",
        evidence = "回写 pptx_structure.source_match_ok"))
    mi <- (ppt %||% list())$missing_images %||% list()
    hm <- (ppt %||% list())$hash_mismatches %||% list()
    img <- suppressWarnings(as.numeric(gv("images")))
    exp <- suppressWarnings(as.numeric(gv("expected_images")))
    roles <- as.character(unlist((ppt %||% list())$slide_roles %||% list()))
    if (isTRUE(sm))
      return(vchk("L1", nmlab, "pass",
        paste0("按模板角色逐页核对：应含图的页面都已嵌入且与源图 hash 一致（",
               if (is.na(img)) "?" else img, "/", if (is.na(exp)) "?" else exp,
               " 张）；封面与目次等纯文字页不需要图，不做要求"),
        expected = one, actual = paste0("嵌入 ", if (is.na(img)) "?" else img, " 张"),
        evidence = "回写 pptx_structure.source_match_ok 与 slide_roles"))
    return(vchk("L1", nmlab, "fail",
      paste0("图片引用未通过：", if (length(mi)) paste0("缺嵌入 ", length(mi), " 处；") else "",
             if (length(hm)) paste0("与源图 hash 不一致 ", length(hm), " 处；") else "",
             substr(why, 1, 80)),
      expected = one, actual = paste0("images=", if (is.na(img)) "?" else img,
                                      " expected=", if (is.na(exp)) "?" else exp),
      evidence = "回写 pptx_structure.missing_images / hash_mismatches"))
  }

  # 打包两条守恒：按 verify_delivery --step pack 带回的结构判，不套表格口径。
  if (grepl("zip 内含", one)) {
    zs <- (facts %||% list())$zip_structure %||% NULL
    if (is.null(zs) || !length(zs))
      return(vchk("L1", nmlab, "unknown",
        "回写没带打包结构采证，这条守恒算不了",
        expected = one, actual = "zip_structure 为空",
        evidence = "回写 zip_structure{}（verify_delivery.py --step pack）"))
    gz <- function(k) { v <- (zs %||% list())[[k]]; if (is.null(v) || !length(v)) NA else v[[1]] }
    if (grepl("全部图|10\\.figs", one)) {
      fg <- suppressWarnings(as.numeric(gz("figures")))
      if (is.na(fg))
        return(vchk("L1", nmlab, "unknown", "结构里没有图数",
          expected = one, actual = "figures 缺失", evidence = "回写 zip_structure.figures"))
      mf <- (zs %||% list())$missing_files %||% list()
      return(vchk("L1", nmlab, if (fg > 0 && !length(mf)) "pass" else "fail",
        if (fg > 0 && !length(mf)) paste0("包内含 ", fg, " 张图，逐份 hash 与当前源文件一致")
        else paste0("包内图不足或与源文件不符：图 ", fg, " 张，缺文件 ", length(mf), " 个"),
        expected = one, actual = paste0("figures=", fg),
        evidence = "回写 zip_structure.figures / missing_files / hash_mismatches"))
    }
    hp <- gz("has_pptx"); hm <- gz("has_pptx_manifest")
    if (is.na(hp))
      return(vchk("L1", nmlab, "unknown", "结构里没有 pptx 是否在包内的结论",
        expected = one, actual = "has_pptx 缺失", evidence = "回写 zip_structure.has_pptx"))
    return(vchk("L1", nmlab, if (isTRUE(hp)) "pass" else "fail",
      if (isTRUE(hp)) paste0("包内含 analysis_report.pptx",
                             if (isTRUE(hm)) "及其来源清单" else "")
      else "包内没有 analysis_report.pptx",
      expected = one, actual = if (isTRUE(hp)) "在包内" else "缺",
      evidence = "回写 zip_structure.has_pptx / has_pptx_manifest"))
  }

  if (is.null(tab))
    return(vchk("L1", nmlab, "unknown",
                "本地没有可读的表格产物，这条守恒算不了", expected = one, actual = "无表格", evidence = "本地镜像目录（无表格产物）"))
  nm <- names(tab)
  if (grepl("细胞类型数 <= 簇数", one)) {
    k <- cnt("n_celltypes"); nck <- cnt("n_clusters")
    if (!is.na(k) && !is.na(nck))
      return(vchk("L1", nmlab, if (k <= nck) "pass" else "fail",
        expected = one, actual = paste0("类型数 ", k, " / 簇数 ", nck),
        evidence = "回写 counts{}（执行端读 annot_result.xls 得到）"))
    # 列名按脚本真实写出的表头认：02_annotate.R 写的是 celltype，
    #   旧判据只认 cell_type，于是正常产物被判「缺列」→ unknown → 整步退回。
    #   flow_spec 的 required_columns 早就登记成 celltype 了，两处必须一致。
    ct_col <- if ("cell_type" %in% nm) "cell_type" else if ("celltype" %in% nm) "celltype" else ""
    if (!"cluster" %in% nm || !nzchar(ct_col))
      return(vchk("L1", nmlab, "unknown", "表里缺 cluster 或 celltype 列",
                  expected = one, actual = paste(nm, collapse = ", "), evidence = rel))
    nc <- length(unique(tab$cluster)); nt <- length(unique(tab[[ct_col]]))
    return(vchk("L1", nmlab, if (nt <= nc) "pass" else "fail",
                expected = one, actual = paste0("簇 ", nc, " 类型 ", nt), evidence = rel))
  }
  # 未实现的守恒一律 unknown，不能落成 not_implemented。
  #   为什么必须改：rollup 只认 fail/unknown/warn，not_implemented 会滑成 pass ——
  #   等于"判据没实现"被当成"通过"，那是最糟的一种假成功。
  vchk("L1", nmlab, "unknown",
       paste0("这条守恒（", one, "）还没有可核算法；按未核验阻断，不当作通过"),
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

  facts$marker_product <- verdict_marker_product(stage)
  # 本阶段的表格产物名：diff 这类规则要按「本阶段精确产物」取结构，
  #   不能随手拿第一张表 —— 拿错表得出的行数与这张表无关。
  prods_l4 <- basename(as.character(stage$review$products %||%
                       stage$artifacts %||% character(0)))
  facts$stage_xls <- prods_l4[grepl("[.]xls$", prods_l4, ignore.case = TRUE)]

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

# 取一张上报表的结构，并与回写清单里同名产物的 md5 对齐。
#   为什么必须按文件名点名：tables 里既有 cluster_stats 也有 top15，
#   随手取第一张就会把簇表当 marker 表，行数完全不是一回事。
verdict_table_for <- function(facts, base_name) {
  tabs <- facts$tables %||% NULL
  if (is.null(tabs) || !length(tabs)) return(NULL)
  art <- Filter(function(a) identical(basename(as.character(a$rel %||% "")), base_name),
                facts$artifacts %||% list())
  art_md5 <- if (length(art)) as.character(art[[1]]$md5 %||% NA) else NA_character_
  art_bytes <- if (length(art)) art[[1]]$bytes else NA
  for (k in names(tabs)) {
    t <- tabs[[k]]
    if (!is.list(t)) next
    cand <- c(basename(as.character(k)),
              basename(as.character(t$rel %||% "")))
    if (!(base_name %in% cand)) next
    tb_md5 <- as.character(t$md5 %||% NA)
    md5_ok <- !is.na(art_md5) && nzchar(art_md5) &&
      !is.na(tb_md5) && identical(tb_md5, art_md5)
    return(list(rel = k, tab = t, md5 = tb_md5, artifact_md5 = art_md5,
                md5_ok = md5_ok, artifact_bytes = art_bytes))
  }
  NULL
}

# 本阶段该核哪一张 marker 表：按阶段声明的产物挑，不写死文件名。
#   为什么不能写死 top15_markers.xls：fullmark 阶段的表叫
#   top15_markers_full.xls / all_markers_full.xls；写死之后要么核错表，
#   要么把 fullmark 判成"没有结构"，两种都不对。
verdict_marker_product <- function(stage) {
  prods <- basename(as.character(stage$review$products %||%
                                 stage$artifacts %||% character(0)))
  prods <- prods[grepl("[.]xls$", prods, ignore.case = TRUE)]
  if (!length(prods)) return(NA_character_)
  want_top <- prods[grepl("top15", prods, ignore.case = TRUE) &
                      grepl("marker", prods, ignore.case = TRUE)]
  if (length(want_top)) return(want_top[[1]])
  want_any <- prods[grepl("marker", prods, ignore.case = TRUE)]
  if (length(want_any)) return(want_any[[1]])
  NA_character_
}

# 取一条上报的对象结构，并与回写清单里同名产物的 md5 对齐。
#   为什么必须对齐：否则可以拿别的文件的结构来充数。
#   清单里没有这份产物、或 md5 缺失，md5_ok 一律 FALSE，由调用方判 unknown。
verdict_object_for <- function(facts, base_name) {
  objs <- facts$objects %||% NULL
  if (is.null(objs) || !length(objs)) return(NULL)
  art <- Filter(function(a) identical(basename(as.character(a$rel %||% "")), base_name),
                facts$artifacts %||% list())
  art_md5 <- if (length(art)) as.character(art[[1]]$md5 %||% NA) else NA_character_
  art_bytes <- if (length(art)) art[[1]]$bytes else NA
  for (k in names(objs)) {
    o <- objs[[k]]
    if (!is.list(o)) next
    cand <- c(basename(as.character(k)), basename(as.character(o$rel %||% "")))
    # identical() 不向量化：拿整条向量和标量比永远 FALSE，
    #   于是结构明明报了也取不到，判定退化成 unknown。
    if (!(base_name %in% cand)) next
    ob_md5 <- as.character(o$md5 %||% NA)
    md5_ok <- !is.na(art_md5) && nzchar(art_md5) &&
      !is.na(ob_md5) && identical(ob_md5, art_md5)
    return(list(rel = k, obj = o, md5 = ob_md5, artifact_md5 = art_md5,
                md5_ok = md5_ok, artifact_bytes = art_bytes))
  }
  NULL
}

# 从上报结构里取合并后对象的簇数
verdict_cluster_from_objects <- function(facts) {
  bo <- verdict_object_for(facts, "merge_7sample.rds")
  if (is.null(bo) || is.null(bo$obj))
    return(list(n = NULL, md5_ok = FALSE, md5 = NA_character_,
                artifact_md5 = NA_character_, rel = ""))
  n <- suppressWarnings(as.integer(bo$obj$n_clusters %||% NA))
  if (is.na(n) || n < 0)
    return(list(n = NULL, md5_ok = bo$md5_ok, md5 = bo$md5,
                artifact_md5 = bo$artifact_md5, rel = bo$rel))
  list(n = n, md5_ok = bo$md5_ok, md5 = bo$md5,
       artifact_md5 = bo$artifact_md5, rel = bo$rel)
}

verdict_l4_one <- function(rule, tab, rel, facts) {
  nm <- paste0(rule)
  if (identical(rule, "doublet_rate_in_range")) {
    if (!exists("doublet_review", mode = "function"))
      return(vchk("L4", nm, "unknown", detail = "双细胞审计模块未加载",
                  expected = rule, actual = "模块缺失", evidence = "engine/doublet_review.R"))
    result <- doublet_review(facts)
    return(vchk("L4", nm, result$status, detail = result$detail,
                expected = "原算法预期数、实际分类、质控及最终对象一致",
                actual = result$status,
                evidence = "本次 doublet_evidence、原始分类元数据和产物指纹"))
  }
  # 每条规则只回答它自己能回答的那一件事；没有数据就算不了。
  # 注释环的两条规则统一交给 engine/annotation_review.R。
  #   它核的是「每群每类实际基因、对象与表一致、计数守恒」，
  #   不做科学分类阈值裁定。这里只负责调用与包装。
  if (identical(rule, "markers_in_dictionary") ||
      identical(rule, "celltype_count_in_range")) {
    if (!exists("annotation_review"))
      return(vchk("L4", nm, "unknown",
        detail = "注释验收模块未加载（engine/annotation_review.R）",
        expected = rule, actual = "模块缺失", evidence = "engine/annotation_review.R"))
    baseline <- facts$reference_baseline %||% NULL
    if (is.null(baseline) || !length(baseline))
      return(vchk("L4", nm, "unknown",
        detail = "没有可信的参考表基线，无法确认本次注释用的是哪张表",
        expected = rule, actual = "reference_baseline 未给",
        evidence = "应用侧参考登记（不是回写体）"))
    r0 <- tryCatch(annotation_review(facts, reference_baseline = baseline),
                   annotation_check = function(e) list(status = "fail",
                                                       detail = conditionMessage(e)),
                   error = function(e) list(status = "unknown",
                                            detail = conditionMessage(e)))
    st0 <- as.character((r0 %||% list())$status %||% "unknown")
    dt0 <- as.character((r0 %||% list())$detail %||% "")
    return(vchk("L4", nm, st0, detail = dt0, expected = rule,
                actual = st0, evidence = "回写 annotation_evidence 与参考基线"))
  }
  if (identical(rule, "umap_present")) {
    # 这条以前没有实现，永远 unknown；现在按真实文件与签名核。
    png <- Filter(function(a) grepl("umap", a$rel, ignore.case = TRUE),
                  facts$artifacts)
    if (!length(png))
      return(vchk("L4", nm, "fail", "产物里没有 umap 图", expected = rule,
                  actual = "0 张", evidence = "回写 artifacts[]"))
    probe <- facts$png_probe %||% NULL
    if (is.null(probe) || !length(probe))
      return(vchk("L4", nm, "unknown",
        detail = "没有 PNG 签名与尺寸证据，无法确认 umap 图可用",
        expected = rule, actual = "png_probe 未给", evidence = "回写 png_probe{}"))
    # 与 figures_non_empty 同一口径：probe 的 md5 必须等于本次产物的 md5，
    #   否则旧图的读数也能让新图过关。
    pm <- vapply(png, function(a) {
      p0 <- probe[[a$rel]]
      v <- as.character((p0 %||% list())$md5 %||% "")
      if (!length(v) || is.na(v[1])) "" else v[1]
    }, "")
    am <- vapply(png, function(a) as.character(a$md5 %||% ""), "")
    if (any(nchar(pm) != 32L))
      return(vchk("L4", nm, "unknown",
        detail = "umap 的 PNG 证据没带 md5", expected = rule,
        actual = "probe.md5 缺失", evidence = "回写 png_probe[].md5"))
    if (!identical(unname(pm), unname(am)))
      return(vchk("L4", nm, "fail",
        detail = "umap 的 PNG 证据与产物 md5 不一致", expected = rule,
        actual = "md5 不一致", evidence = "回写 png_probe[] 与 artifacts[]"))
    miss <- 0L; bad <- 0L
    for (a in png) {
      p0 <- probe[[a$rel]]
      if (is.null(p0)) { miss <- miss + 1L; next }
      w <- suppressWarnings(as.numeric(p0$width %||% NA))
      h <- suppressWarnings(as.numeric(p0$height %||% NA))
      if (!isTRUE(p0$signature_ok) || is.na(w) || is.na(h) || w <= 0 || h <= 0) bad <- bad + 1L
    }
    if (miss > 0L)
      return(vchk("L4", nm, "unknown",
        detail = paste0("有 ", miss, " 张 umap 图缺签名/尺寸证据"),
        expected = rule, actual = "部分缺证据", evidence = "回写 png_probe{}"))
    if (bad > 0L)
      return(vchk("L4", nm, "fail",
        detail = paste0("有 ", bad, " 张 umap 图不可解码"), expected = rule,
        actual = as.character(bad), evidence = "回写 png_probe{}"))
    return(vchk("L4", nm, "pass",
      detail = paste0("umap 图 ", length(png), " 张，签名与尺寸正常"),
      expected = rule, actual = paste0("png ", length(png), " 张"),
      evidence = "回写 artifacts[] 与 png_probe{}"))
  }
  if (identical(rule, "figures_non_empty")) {
    if (!length(facts$artifacts)) return(vchk("L4", nm, "unknown", "无产物清单",
      expected = rule, actual = "空清单", evidence = "回写 artifacts[]（空）"))
    png <- Filter(function(a) grepl("\\.png$", a$rel, ignore.case = TRUE), facts$artifacts)
    if (!length(png))
      return(vchk("L4", nm, "fail", "产物清单里没有 png", expected = rule,
                  actual = "0 张", evidence = "回写 artifacts[]（无 png）"))
    # 缺字节数不能当"没问题"：拿不到证据就要说拿不到。
    bytes <- suppressWarnings(as.numeric(vapply(png, function(a)
      as.character(a$bytes %||% NA), "")))
    if (any(is.na(bytes)))
      return(vchk("L4", nm, "unknown",
        detail = paste0("有 ", sum(is.na(bytes)), " 张 png 没带字节数"),
        expected = rule, actual = "bytes 缺失", evidence = "回写 artifacts[].bytes"))
    # artifacts 归一化后缺 md5 的条目会是 NA 而不是 NULL，
    #   直接 nchar(NA) 会得到 NA，进而 if (NA != 32) 报错。
    #   这里统一成""，缺 md5 就走 unknown 分支。
    md5s <- vapply(png, function(a) {
      v <- as.character(a$md5 %||% "")
      if (!length(v) || is.na(v[1])) "" else v[1]
    }, "")
    if (any(nchar(md5s) != 32L))
      return(vchk("L4", nm, "unknown",
        detail = paste0("有 ", sum(nchar(md5s) != 32L), " 张 png 没带 md5"),
        expected = rule, actual = "md5 缺失", evidence = "回写 artifacts[].md5"))
    # md5 与字节数只能证明"文件是那份"，证明不了"图能解码"。
    #   需要执行端读 PNG 签名与宽高；没带就判 unknown，不用 >512 冒充证据。
    probe <- facts$png_probe %||% NULL
    if (is.null(probe) || !length(probe))
      return(vchk("L4", nm, "unknown",
        detail = "回写没带 PNG 签名与尺寸证据，无法确认图可解码",
        expected = rule, actual = "png_probe 未给", evidence = "回写 png_probe{}"))
    # probe 的 md5 必须与本次产物一致：否则旧图的 probe 也能让新图过关。
    pm <- vapply(png, function(a) {
      p0 <- probe[[a$rel]]
      v <- as.character((p0 %||% list())$md5 %||% "")
      if (!length(v) || is.na(v[1])) "" else v[1]
    }, "")
    am <- vapply(png, function(a) as.character(a$md5 %||% ""), "")
    if (any(nchar(pm) != 32L))
      return(vchk("L4", nm, "unknown",
        detail = "PNG 证据里没带 md5，无法确认它对应本次这张图",
        expected = rule, actual = "probe.md5 缺失", evidence = "回写 png_probe[].md5"))
    if (!identical(unname(pm), unname(am)))
      return(vchk("L4", nm, "fail",
        detail = "PNG 证据的 md5 与产物清单不一致，像拿了旧图的读数",
        expected = rule, actual = "md5 不一致", evidence = "回写 png_probe[] 与 artifacts[]"))
    bad <- 0L; miss <- 0L
    for (a in png) {
      p0 <- probe[[a$rel]]
      if (is.null(p0)) { miss <- miss + 1L; next }
      w0 <- suppressWarnings(as.numeric(p0$width %||% NA))
      h0 <- suppressWarnings(as.numeric(p0$height %||% NA))
      # 宽高缺失是 NA，直接写 if (NA <= 0) 会让整个判定崩掉。
      if (!isTRUE(p0$signature_ok) || is.na(w0) || is.na(h0) || w0 <= 0 || h0 <= 0)
        bad <- bad + 1L
    }
    if (miss > 0L)
      return(vchk("L4", nm, "unknown",
        detail = paste0("有 ", miss, " 张 png 没有签名/尺寸证据"),
        expected = rule, actual = "部分缺证据", evidence = "回写 png_probe{}"))
    if (bad > 0L)
      return(vchk("L4", nm, "fail",
        detail = paste0("有 ", bad, " 张 png 签名不对或尺寸为 0，图不可用"),
        expected = rule, actual = as.character(bad), evidence = "回写 png_probe{}"))
    return(vchk("L4", nm, "pass",
      detail = paste0("png ", length(png), " 张，签名与尺寸均正常"),
      expected = rule, actual = paste0("png ", length(png), " 张"),
      evidence = "回写 artifacts[] 与 png_probe{}"))
  }
  if (identical(rule, "markers_not_empty")) {
    # marker 表里到底有没有记录，以及每个簇是不是都算出了 marker。
    #   为什么补（2026-11-19）：flow_spec 在 markers 环节声明了这条规则，
    #   判定器里没有实现，落成 not_implemented → 汇总 unknown → 回写被拒。
    #   实测这次：29 个簇，top15 表 420 行、覆盖 28 个簇，缺 1 个簇 ——
    #   那是"有簇没显著 marker"的真实情况，该如实说出来，不该判 fail 也不该吞掉。
    #   能回答的是"表里有记录、覆盖了几个簇"；不能回答"marker 生物学上对不对"。
    mp <- as.character(facts$marker_product %||% NA)
    if (is.na(mp) || !nzchar(mp)) {
      return(vchk("L4", nm, "unknown",
        "本阶段没有声明该核哪张 marker 表，判不了",
        expected = rule, actual = "未声明",
        evidence = "engine/flow_spec.R review$products"))
    }
    tf <- verdict_table_for(facts, mp)
    if (is.null(tf) || is.null(tf$tab)) {
      return(vchk("L4", nm, "unknown",
        paste0("回写没带 ", mp, " 的结构，无法核有没有 marker 记录"),
        expected = rule, actual = "无结构证据",
        evidence = "回写 tables{}（执行端读该表得到）"))
    }
    if (!isTRUE(tf$md5_ok)) {
      return(vchk("L4", nm, "unknown",
        "上报的表结构与回写清单的 md5 对不上，不能采信",
        expected = rule,
        actual = paste0("结构 md5 ", tf$md5, " / 清单 ", tf$artifact_md5),
        evidence = paste0(tf$rel, "（执行端读表得到）")))
    }
    rows <- suppressWarnings(as.integer(tf$tab$rows %||% NA))
    if (is.na(rows)) {
      return(vchk("L4", nm, "unknown", "上报里没有可用的行数",
        expected = rule, actual = "未给出", evidence = tf$rel))
    }
    if (rows <= 0L) {
      return(vchk("L4", nm, "fail",
        detail = paste0("数据行 ", rows, "：只有表头，一个 marker 都没有"),
        expected = rule, actual = paste0("数据行 ", rows),
        evidence = paste0(tf$rel, " 的数据行数（执行端读表得到）")))
    }
    # 覆盖簇数：marker 表里出现的簇，对上 cluster_stats 的簇数。
    #   两个都读得到才比；读不到就只报"有记录"，并把缺的那层说清楚。
    with_mk <- suppressWarnings(as.integer((tf$tab$distinct %||% list())$cluster %||% NA))
    cs <- (facts$tables %||% list())[["5.markers/cluster_stats.xls"]]
    total_cl <- suppressWarnings(as.integer((cs %||% list())$rows %||% NA))
    if (!is.na(with_mk) && !is.na(total_cl) && total_cl > 0L && with_mk < total_cl) {
      return(vchk("L4", nm, "warn",
        detail = paste0("有 ", rows, " 条 marker 记录，覆盖 ", with_mk, " / ", total_cl,
                        " 个簇；有 ", total_cl - with_mk,
                        " 个簇没有达到显著阈值的 marker。",
                        "这是该有的没有的如实记录，不是判据写错；",
                        "只说明记录存在与簇覆盖情况，不评价 marker 的生物学合理性"),
        expected = rule,
        actual = paste0("rows=", rows, " clusters_with_markers=", with_mk,
                        " clusters_total=", total_cl),
        evidence = paste0(tf$rel, " 与 5.markers/cluster_stats.xls（执行端读表得到，md5 已对齐）")))
    }
    return(vchk("L4", nm, "pass",
      detail = paste0("有 ", rows, " 条 marker 记录",
                      if (!is.na(with_mk)) paste0("，覆盖 ", with_mk, " 个簇") else "",
                      "。只说明表里有真实记录，不评价 marker 的生物学合理性"),
      expected = rule, actual = paste0("rows=", rows),
      evidence = paste0(tf$rel, " 的行数（执行端读表得到，md5 已对齐）")))
  }
  if (identical(rule, "markers_not_all_genes")) {
    # 只认 top15_markers.xls 这一张表，按名字点名 + md5 对齐。
    #   为什么不能取第一张表：tables 里还有 cluster_stats，
    #   拿它数行数会得出「几十行，正常」，等于没检查。
    mp <- as.character(facts$marker_product %||% NA)
    if (is.na(mp) || !nzchar(mp)) {
      return(vchk("L4", nm, "unknown",
        "本阶段没有声明该核哪张 marker 表，判不了",
        expected = rule, actual = "未声明",
        evidence = "engine/flow_spec.R review$products"))
    }
    tf <- verdict_table_for(facts, mp)
    if (is.null(tf) || is.null(tf$tab)) {
      return(vchk("L4", nm, "unknown",
        paste0("回写没带 ", mp, " 的结构，无法核行数是否异常"),
        expected = rule, actual = "无结构证据",
        evidence = "回写 tables{}（执行端读该表得到）"))
    }
    if (!isTRUE(tf$md5_ok)) {
      return(vchk("L4", nm, "unknown",
        "上报的 top15 表结构与回写清单的 md5 对不上，不能采信",
        expected = rule,
        actual = paste0("结构 md5 ", tf$md5, " / 清单 ", tf$artifact_md5),
        evidence = paste0(tf$rel, "（执行端读表得到）")))
    }
    rows <- suppressWarnings(as.integer(tf$tab$rows %||% NA))
    if (is.na(rows)) {
      return(vchk("L4", nm, "unknown", "上报里没有可用的行数",
        expected = rule, actual = "未给出", evidence = tf$rel))
    }
    if (rows <= 0L) {
      return(vchk("L4", nm, "fail",
        detail = paste0("数据行 ", rows, "：只有表头，没有任何 marker 记录"),
        expected = rule, actual = paste0("数据行 ", rows),
        evidence = paste0(tf$rel, " 的数据行数（执行端读表得到）")))
    }
    # 这一段只做粗粒度异常识别：行数远大于基因数量级（> 50000）
    #   说明很可能把整张表达矩阵倒了出来。它不是对 marker 科学合理性的判断。
    bad <- rows > 50000
    return(vchk("L4", nm, if (bad) "fail" else "pass",
      detail = if (bad)
                 paste0("行数 ", rows,
                        " 远超常见基因数量级，疑似把整张表达矩阵导出；",
                        "这是粗粒度异常识别，不是全面科学验证")
               else
                 paste0("行数 ", rows, " 在常见量级内。",
                        "这只是粗粒度异常检查（是否疑似整表导出），",
                        "不代表对 marker 合理性的全面科学验证"),
      expected = rule, actual = paste0("行数 ", rows),
      evidence = paste0(tf$rel, " 的行数（执行端读表得到，md5 已对齐）")))
  }
  if (identical(rule, "diff_not_all_genes")) {
    # 只认本阶段声明的那张差异表，并要求其 md5 与回写清单一致。
    #   改前读本机 tab：应用主机看不到 /share，这条在覆盖验收里必然 unknown。
    cands <- as.character(facts$stage_xls %||% character(0))
    tf <- NULL
    for (cn in cands) {
      t0 <- verdict_table_for(facts, cn)
      if (!is.null(t0) && !is.null(t0$tab)) { tf <- t0; break }
    }
    if (is.null(tf)) return(vchk("L4", nm, "unknown",
      "回写没带本阶段差异表的结构，行数核不了",
      expected = rule, actual = "无结构证据", evidence = "回写 tables{}"))
    if (!isTRUE(tf$md5_ok)) return(vchk("L4", nm, "unknown",
      "差异表结构与回写清单的 md5 对不上，不能采信",
      expected = rule, actual = paste0("结构 ", tf$md5, " / 清单 ", tf$artifact_md5),
      evidence = tf$rel))
    rows <- suppressWarnings(as.integer(tf$tab$rows %||% NA))
    if (is.na(rows)) return(vchk("L4", nm, "unknown",
      "上报里没有可用行数", expected = rule, actual = "未给", evidence = tf$rel))
    if (rows <= 0) return(vchk("L4", nm, "fail",
      detail = "差异表零行：没有任何差异记录",
      expected = rule, actual = "行数 0", evidence = tf$rel))

    # 按正确粒度核：这张表是 (cluster, gene) 堆叠，行数天然可以远超基因数。
    #   拿总行数当阈值会误拒合法结果（实测 24 簇合计 72347 行）。
    #   真正该按基因组量级核的是「去重后的基因数」，重复对则说明表被拼重了。
    cols_have <- as.character(unlist(tf$tab$columns %||% character(0)))
    miss <- setdiff(c("cluster", "gene"), cols_have)
    if (length(miss)) return(vchk("L4", nm, "fail",
      detail = paste0("缺必要列：", paste(miss, collapse = ", ")),
      expected = "cluster, gene", actual = paste(cols_have, collapse = ", "),
      evidence = tf$rel))

    dist <- tf$tab$distinct %||% NULL
    n_gene <- suppressWarnings(as.integer(dist$gene %||% NA))
    n_pair <- suppressWarnings(as.integer(dist$cluster_gene %||% NA))
    if (is.na(n_gene)) return(vchk("L4", nm, "unknown",
      "回写没带去重基因数，按正确粒度核不了（总行数不足以判断）",
      expected = rule, actual = "无去重计数", evidence = tf$rel))

    if (!is.na(n_pair) && n_pair != rows) return(vchk("L4", nm, "fail",
      detail = paste0("存在重复的 (簇, 基因) 组合：行数 ", rows,
                      " 去重对 ", n_pair),
      expected = rule, actual = paste0("rows=", rows, " pairs=", n_pair),
      evidence = tf$rel))

    bad <- n_gene > 50000
    return(vchk("L4", nm, if (bad) "fail" else "pass",
      detail = if (bad)
                 paste0("去重基因 ", n_gene, " 超常见基因组量级，疑似整表导出（粗粒度异常识别）")
               else paste0("去重基因 ", n_gene, "、(", rows,
                           " 行按簇堆叠)；这是结构检查，不代表差异显著性判断"),
      expected = rule, actual = paste0("去重基因 ", n_gene, " / 行数 ", rows),
      evidence = paste0(tf$rel, "（执行端读表得到，md5 已对齐）")))
  }
  if (identical(rule, "ratio_sums_to_one")) {
    if (!length(facts$artifacts)) return(vchk("L4", nm, "unknown", "无产物清单",
      expected = rule, actual = "空清单", evidence = "回写 artifacts[]（空）"))
    return(vchk("L4", nm, "pass", "比例表已产出，逐行合计由 L1 守恒检查负责",
                expected = rule, actual = "见 L1 invariant"))
  }
  if (identical(rule, "enrich_not_empty")) {
    ok_md5 <- function(x) {
      x <- as.character(x %||% "")
      length(x) == 1L && !is.na(x) && grepl("^[0-9a-f]{32}$", x)
    }
    ok_int <- function(x) {
      suppressWarnings({
        v <- as.numeric(x %||% NA)
        length(v) == 1L && !is.na(v) && is.finite(v) && v >= 0 && v == round(v)
      })
    }
    if (!length(facts$artifacts))
      return(vchk("L4", nm, "unknown", "无产物清单", expected = rule,
                  actual = "空清单", evidence = "回写 artifacts[]（空）"))
    ec <- suppressWarnings(as.integer(facts$code %||% facts$exit_code %||% NA))
    if (is.na(ec))
      return(vchk("L4", nm, "unknown", "回写没带退出码", expected = rule,
                  actual = "未给", evidence = "回写 code"))
    if (ec != 0L)
      return(vchk("L4", nm, "fail", paste0("富集退出码 ", ec), expected = rule,
                  actual = as.character(ec), evidence = "回写 code"))

    rels <- vapply(facts$artifacts, function(a) as.character(a$rel %||% ""), "")
    md5s <- vapply(facts$artifacts, function(a) as.character(a$md5 %||% ""), "")
    names(md5s) <- rels
    res_rel <- sort(rels[grepl("^10\\.figs/4\\.diff_enrich/enrich/", rels) &
                         grepl("_(GO_BP|KEGG)\\.xls$", rels)])
    if (!length(res_rel))
      return(vchk("L4", nm, "fail", "没有任何 GO_BP / KEGG 结果表",
        expected = rule, actual = "0", evidence = "回写 artifacts[]"))

    inputs <- facts$enrich_inputs
    if (!is.list(inputs) || !length(inputs))
      return(vchk("L4", nm, "unknown", "没有执行端实读的 diff 输入清单",
        expected = rule, actual = "未给", evidence = "回写 enrich_inputs[]"))
    in_rel <- vapply(inputs, function(x) as.character(x$rel %||% ""), "")
    in_md5 <- vapply(inputs, function(x) as.character(x$md5 %||% ""), "")
    # ok_md5 是标量判定（length==1），直接 all(ok_md5(向量)) 会把整条向量
    #   传进去、长度不等于 1 恒为 FALSE，于是多个类型时全部任务都判 unknown。
    in_ok <- vapply(as.list(in_md5), function(x) isTRUE(ok_md5(x)), TRUE)
    if (!all(in_ok) || any(!nzchar(in_rel)))
      return(vchk("L4", nm, "unknown", "输入清单缺路径或 md5 不合法",
        expected = rule, actual = "不完整", evidence = "回写 enrich_inputs[]"))
    if (anyDuplicated(in_rel))
      return(vchk("L4", nm, "fail", "输入清单里有重复路径",
        expected = rule, actual = paste(unique(in_rel[duplicated(in_rel)]), collapse = ","),
        evidence = "回写 enrich_inputs[]"))

    ST <- "10.figs/4.diff_enrich/enrich/enrich_status.xls"
    if (!ST %in% rels)
      return(vchk("L4", nm, "unknown", "没有富集状态表", expected = rule,
                  actual = "缺", evidence = ST))
    st <- (facts$tables %||% list())[[ST]]
    rows <- facts$enrich_status_rows
    if (!ok_md5(md5s[[ST]]))
      return(vchk("L4", nm, "unknown", "状态表产物缺 md5",
        expected = rule, actual = "缺", evidence = ST))
    if (!is.list(st) || is.null(st$rows))
      return(vchk("L4", nm, "unknown", "状态表结构未采到",
        expected = rule, actual = "缺", evidence = ST))
    # 两边都要有合法 md5 才谈相等：空串相等不是证据。
    if (!ok_md5(st$md5))
      return(vchk("L4", nm, "unknown", "状态表结构证据缺 md5",
        expected = rule, actual = "缺", evidence = ST))
    if (!identical(as.character(st$md5), unname(md5s[[ST]])))
      return(vchk("L4", nm, "fail", "状态表 md5 与产物不一致",
        expected = rule, actual = "不一致", evidence = ST))
    if (is.null(rows) || !length(rows))
      return(vchk("L4", nm, "unknown", "状态表没有逐行内容",
        expected = rule, actual = "缺", evidence = ST))
    if (!ok_int(st$rows))
      return(vchk("L4", nm, "fail", "状态表声明行数不是非负整数",
        expected = rule, actual = as.character(st$rows %||% "缺"), evidence = ST))
    if (as.numeric(st$rows) != length(rows))
      return(vchk("L4", nm, "fail",
        detail = paste0("状态表声明 ", st$rows, " 行，实际 ", length(rows), " 行"),
        expected = rule, actual = as.character(length(rows)), evidence = ST))

    allowed <- c("ok", "no_sig", "lt10_sig", "lt5_mapped", "no_mapping", "no_term_in_range")
    bad <- character(0); seen <- character(0)
    used_res <- character(0); used_in <- character(0)
    for (r in rows) {
      ct <- as.character(r$cell_type %||% ""); an <- as.character(r$analysis %||% "")
      ss <- as.character(r$status %||% "")
      ig <- r$input_genes; mg <- r$mapped_genes; tm <- r$terms
      sg <- r$sig_total; rw <- r$rows_written
      rs <- as.character(r$result_rel %||% "")
      rs_full <- if (nzchar(rs)) file.path("10.figs", rs) else ""
      dr <- as.character(r$diff_rel %||% "")
      dr_full <- if (nzchar(dr)) file.path("10.figs", dr) else ""
      seen <- c(seen, paste0(ct, "|", an))

      if (!ss %in% allowed) { bad <- c(bad, paste0(ct, "/", an, ":状态非法")); next }
      if (!ok_int(ig) || !ok_int(mg) || !ok_int(tm) || !ok_int(sg) || !ok_int(rw)) {
        bad <- c(bad, paste0(ct, "/", an, ":数值字段非法")); next }
      if (as.numeric(mg) > as.numeric(ig))
        bad <- c(bad, paste0(ct, "/", an, ":映射基因数大于输入"))
      if (as.numeric(sg) > as.numeric(tm))
        bad <- c(bad, paste0(ct, "/", an, ":sig_total 大于 terms"))
      if (as.numeric(rw) != min(as.numeric(sg), 30))
        bad <- c(bad, paste0(ct, "/", an, ":rows_written 应为 min(sig_total,30)"))
      if (ss == "ok" && as.numeric(rw) <= 0)
        bad <- c(bad, paste0(ct, "/", an, ":ok 但没写出结果"))
      if (ss != "ok" && as.numeric(rw) != 0)
        bad <- c(bad, paste0(ct, "/", an, ":非 ok 却写了结果"))
      if (ss != "ok" && !nzchar(trimws(as.character(r$reason %||% ""))))
        bad <- c(bad, paste0(ct, "/", an, ":非 ok 没给原因"))

      want_rel <- paste0("10.figs/4.diff_enrich/enrich/", ct, "_", an, ".xls")
      if (!identical(rs_full, want_rel))
        bad <- c(bad, paste0(ct, "/", an, ":结果路径与类型分析不符"))
      if (!nzchar(rs_full) || !rs_full %in% rels) {
        bad <- c(bad, paste0(ct, "/", an, ":结果表不在清单")); next }
      used_res <- c(used_res, rs_full)
      if (!ok_md5(md5s[[rs_full]]) || !ok_md5(r$result_md5) ||
          !identical(as.character(r$result_md5), unname(md5s[[rs_full]])))
        bad <- c(bad, paste0(ct, "/", an, ":结果 md5 缺失或与产物不符"))
      tr <- (facts$tables %||% list())[[rs_full]]
      if (!is.list(tr)) {
        bad <- c(bad, paste0(ct, "/", an, ":结果表没有结构证据")); next }
      if (!ok_md5(tr$md5) || !identical(as.character(tr$md5), unname(md5s[[rs_full]])))
        bad <- c(bad, paste0(ct, "/", an, ":结果表结构 md5 与产物不符"))
      if (!ok_int(tr$rows) || as.numeric(tr$rows) != as.numeric(rw))
        bad <- c(bad, paste0(ct, "/", an, ":结果表行数与 rows_written 不符"))

      if (!nzchar(dr_full) || !dr_full %in% in_rel) {
        bad <- c(bad, paste0(ct, "/", an, ":输入不在实读清单")); next }
      used_in <- c(used_in, dr_full)
      if (!ok_md5(r$diff_md5) ||
          !identical(as.character(r$diff_md5), in_md5[match(dr_full, in_rel)]))
        bad <- c(bad, paste0(ct, "/", an, ":输入 md5 与实读清单不符"))
    }
    if (length(bad))
      return(vchk("L4", nm, "fail", detail = paste(head(bad, 4), collapse = "; "),
        expected = rule, actual = as.character(length(bad)), evidence = ST))

    if (!setequal(unique(used_res), res_rel) || length(unique(used_res)) != length(res_rel))
      return(vchk("L4", nm, "fail",
        detail = paste0("结果表覆盖不一致：清单 ", length(res_rel), "，用到 ",
                        length(unique(used_res))),
        expected = rule, actual = as.character(length(unique(used_res))), evidence = ST))

    cts <- unique(sub("\\|.*$", "", seen))
    need <- as.vector(t(outer(cts, c("GO_BP", "KEGG"), paste, sep = "|")))
    if (!setequal(seen, need) || length(seen) != length(need))
      return(vchk("L4", nm, "fail",
        detail = paste0("类型×分析组合不齐：", length(seen), " 行，应为 ", length(need)),
        expected = rule, actual = as.character(length(seen)), evidence = ST))

    tab <- table(used_in)
    if (length(tab) != length(in_rel) || any(as.integer(tab) != 2L))
      return(vchk("L4", nm, "fail", "有输入没有对应两种分析，或出现清单外输入",
        expected = rule, actual = paste(names(tab), as.integer(tab), collapse = ","),
        evidence = "回写 enrich_inputs[] 与状态表"))

    return(vchk("L4", nm, "pass",
      detail = paste0("类型 ", length(cts), " 个，输入 ", length(in_rel),
                      " 份 × 2 种分析，逐项对齐"),
      expected = rule, actual = paste0("状态行 ", length(rows)), evidence = ST))
  }
  if (identical(rule, "slides_match_fig_count")) {
    # 判据只认执行端采证：engine/sc-ppt-kit/scripts/verify_delivery.py --step ppt
    #   带回的 pptx_structure —— 它实读了 CRC、每一份 XML、以及每张图与源图的一致性。
    #   为什么不再自己解压数页数：页数只是形状，证明不了包没坏，也证明不了
    #   PPT 里的图就是本轮的图；本地解压拿不到 CRC/XML/图源这三项结论，不是等价证据。
    #   「页数 >= 图数」更不能顶替：少放几张图一样满足它，等于用弱判据换一个假的通过。
    #   采证结构缺字段或为空时一律 unknown：拿不到证据就说拿不到，不降级成 pass。
    hit <- Filter(function(a) grepl("\\.pptx$", a$rel, ignore.case = TRUE), facts$artifacts)
    if (!length(hit))
      return(vchk("L4", nm, "fail", "没有 pptx 产物", expected = rule, actual = "0",
                  evidence = "回写 artifacts[]（无 pptx）"))
    ppt <- facts$pptx_structure
    if (!is.list(ppt) || !length(ppt))
      return(vchk("L4", nm, "unknown",
        detail = "回写没带 pptx 采证结构，CRC / XML / 图源一致性都核不了",
        expected = rule, actual = "pptx_structure 为空",
        evidence = "回写 pptx_structure{}（verify_delivery.py --step ppt）"))
    need <- c("rel", "md5", "slides", "images", "crc_ok", "xml_ok",
              "source_match_ok", "ok", "why")
    miss <- setdiff(need, names(ppt))
    if (length(miss))
      return(vchk("L4", nm, "unknown",
        detail = paste0("pptx 采证结构缺字段：", paste(miss, collapse = ", ")),
        expected = rule, actual = "字段缺失",
        evidence = "回写 pptx_structure{}（verify_delivery.py --step ppt）"))
    # 采证必须绑定到本次那一份 pptx：rel 对上、md5 与清单一致。
    #   否则另一份 pptx 的采证结论也能给这一份背书。
    srel <- as.character(ppt$rel %||% "")
    smd5 <- as.character(ppt$md5 %||% "")
    arel <- as.character(hit[[1]]$rel %||% "")
    amd5 <- as.character(hit[[1]]$md5 %||% "")
    if (!identical(srel, arel))
      return(vchk("L4", nm, "fail",
        detail = "采证结构指向的 pptx 不是本次产物清单里的那一份",
        expected = arel, actual = srel,
        evidence = "回写 pptx_structure.rel 与 artifacts[]"))
    if (!length(smd5) || is.na(smd5[1]) || !nzchar(smd5[1]))
      return(vchk("L4", nm, "unknown",
        detail = "采证结构没带 md5，无法确认采证读的就是这份 pptx",
        expected = rule, actual = "pptx_structure.md5 缺失",
        evidence = "回写 pptx_structure.md5"))
    if (!length(amd5) || is.na(amd5[1]) || !nzchar(amd5[1]))
      return(vchk("L4", nm, "unknown",
        detail = "产物清单没带这份 pptx 的 md5，无法与采证结构对齐",
        expected = rule, actual = "artifacts[].md5 缺失",
        evidence = "回写 artifacts[]（pptx 条目）"))
    if (!identical(smd5[1], amd5[1]))
      return(vchk("L4", nm, "fail",
        detail = "采证结构的 md5 与本次 pptx 产物的 md5 不一致，采证不能采信",
        expected = amd5[1], actual = smd5[1],
        evidence = "回写 pptx_structure.md5 与 artifacts[].md5"))
    # 布尔字段必须是明确的 TRUE/FALSE：NA 或字符串都不算证据。
    flag_def <- list(ok = "ok", crc_ok = "crc_ok", xml_ok = "xml_ok",
                     source_match_ok = "source_match_ok")
    for (k in names(flag_def)) {
      v <- ppt[[k]]
      if (!is.logical(v) || length(v) != 1L || is.na(v))
        return(vchk("L4", nm, "unknown",
          detail = paste0("采证字段 ", k, " 不是明确的 TRUE/FALSE，判不了"),
          expected = rule, actual = paste0(k, " 未明确"),
          evidence = "回写 pptx_structure{}"))
    }
    why <- as.character(ppt$why %||% "")
    if (!length(why) || is.na(why[1]) || !nzchar(why[1]))
      why <- "采证脚本未给出原因"
    bad <- names(flag_def)[!vapply(flag_def, function(k) isTRUE(ppt[[k]]), TRUE)]
    if (length(bad))
      return(vchk("L4", nm, "fail",
        detail = paste0("pptx 采证未通过（", paste(bad, collapse = ", "), "）：", why[1]),
        expected = "ok / crc_ok / xml_ok / source_match_ok 全部为 TRUE",
        actual = paste0(paste(bad, collapse = ", "), " = FALSE"),
        evidence = paste0(arel, "（verify_delivery.py --step ppt）")))
    slides <- suppressWarnings(as.integer(ppt$slides))
    images <- suppressWarnings(as.integer(ppt$images))
    if (is.na(slides) || slides <= 0L)
      return(vchk("L4", nm, "fail",
        detail = "采证自称通过，但页数为 0，证据自相矛盾",
        expected = "slides > 0", actual = as.character(ppt$slides),
        evidence = arel))
    if (is.na(images) || images < 0L)
      return(vchk("L4", nm, "unknown",
        detail = "采证没给出可读的嵌入图数，图源是否完整核不了",
        expected = "images >= 0", actual = as.character(ppt$images),
        evidence = arel))
    # 采证脚本的 source_match_ok 要求嵌入图数 > 0；这里 0 图却说全绿，自相矛盾。
    if (images == 0L)
      return(vchk("L4", nm, "fail",
        detail = "采证自称通过，但嵌入图数为 0，证据自相矛盾",
        expected = "images > 0", actual = "0", evidence = arel))
    return(vchk("L4", nm, "pass",
      detail = paste0("pptx 采证通过：", slides, " 页，嵌入图 ", images,
                      " 张，CRC 与 XML 正常，图源与本次一致"),
      expected = "ok / crc_ok / xml_ok / source_match_ok 全部为 TRUE",
      actual = paste0("页 ", slides, " / 图 ", images),
      evidence = paste0(arel, "（verify_delivery.py --step ppt，md5 已对齐）")))
  }
  # 簇数：只核技术结构，不评价"多少个簇才合适"。
  #   为什么补（2026-11-18）：flow_spec 在 merge 环节已经声明了这条规则，
  #   判定器里却没有实现，于是它每轮落成 not_implemented、汇总成 unknown，
  #   回写被拒。代码自己写着"责任在应用，重跑同一环不可能让一个没写的检查通过"，
  #   所以这里按 sibling 规则 batch_corrected 的口径补齐：
  #   能核的是"聚类已落地且编号自洽"，核不了的是"簇数在生物学上是否合适"，
  #   后者不在 pass 的结论里冒充。
  if (identical(rule, "cluster_count_in_range")) {
    bo <- verdict_object_for(facts, "merge_7sample.rds")
    if (is.null(bo) || is.null(bo$obj))
      return(vchk("L4", nm, "unknown",
        "回写没带合并后对象的真实结构，无法核簇数",
        expected = rule, actual = "无结构证据",
        evidence = "回写 objects{}（执行端读 RDS 得到）"))
    if (!isTRUE(bo$md5_ok))
      return(vchk("L4", nm, "unknown",
        "上报的对象结构与回写清单的 md5 对不上，不能采信",
        expected = rule, actual = paste0("结构 md5 ", bo$md5, " / 清单 ", bo$artifact_md5),
        evidence = paste0(bo$rel, "（执行端读对象得到）")))
    o <- bo$obj
    n <- suppressWarnings(as.integer(o$n_clusters %||% NA))
    ids <- as.character(unlist(o$clusters %||% character(0)))
    cells <- suppressWarnings(as.integer(o$cells %||% NA))
    if (is.na(n) || n <= 0L)
      return(vchk("L4", nm, "fail",
        "对象的簇数读不出或为 0，聚类这一步没有落地",
        expected = "n_clusters > 0", actual = as.character(n),
        evidence = paste0(bo$rel, "（执行端读对象得到）")))
    if (!length(ids))
      return(vchk("L4", nm, "unknown",
        "回写没给簇编号清单，无法确认簇数是数出来的还是自报的",
        expected = "clusters[]", actual = "未给", evidence = bo$rel))
    if (length(unique(ids)) != length(ids))
      return(vchk("L4", nm, "fail",
        "簇编号清单里有重复，簇数不可信",
        expected = "编号唯一", actual = paste0("清单 ", length(ids), " 项"),
        evidence = bo$rel))
    if (length(ids) != n)
      return(vchk("L4", nm, "fail",
        "簇数与簇编号清单长度不一致，两者必有一处不实",
        expected = as.character(length(ids)), actual = as.character(n),
        evidence = bo$rel))
    if (is.na(cells) || cells <= 0L)
      return(vchk("L4", nm, "unknown",
        "对象细胞数读不出，无法确认簇是细胞分出来的",
        expected = rule, actual = "未给出", evidence = bo$rel))
    if (n > cells)
      return(vchk("L4", nm, "fail",
        "簇数大于细胞数，聚类结果不可能成立",
        expected = "簇数 <= 细胞数",
        actual = paste0(n, " > ", cells), evidence = bo$rel))
    return(vchk("L4", nm, "pass",
      detail = paste0("技术结构核验通过：对象 ", cells, " 个细胞、", n,
                      " 个簇，簇数与编号清单一致。只说明聚类已落地且编号自洽，",
                      "不评价簇数在生物学上是否合适"),
      expected = rule,
      actual = paste0("n_clusters=", n, " cells=", cells),
      evidence = paste0(bo$rel, "（执行端读对象得到，md5 已对齐）")))
  }

  # 其余规则：没有实现就如实说算不了。
  if (identical(rule, "batch_corrected")) {
    # 只做技术结构核验，不是生物学结论。
    #   这条能回答：对象里有没有 harmony、维度是否 > 0、是否全为有限值、
    #   细胞数是否读得出 —— 即"批次校正这一步确实落地且结果形状正常"。
    #   它不能回答"生物学批次效应已被科学消除"；那需要专门设计，
    #   这里不宣称，pass 的 detail 里也写明这一点。
    bo <- verdict_object_for(facts, "merge_7sample.rds")
    if (is.null(bo) || is.null(bo$obj))
      return(vchk("L4", nm, "unknown",
        "回写没带合并后对象的真实结构，无法核批次校正是否落地",
        expected = rule, actual = "无结构证据",
        evidence = "回写 objects{}（执行端读 RDS 得到）"))
    if (!isTRUE(bo$md5_ok))
      return(vchk("L4", nm, "unknown",
        "上报的对象结构与回写清单的 md5 对不上，不能采信",
        expected = rule,
        actual = paste0("结构 md5 ", bo$md5, " / 清单 ", bo$artifact_md5),
        evidence = paste0(bo$rel, "（执行端读对象得到）")))
    o <- bo$obj
    reds <- as.character(unlist(o$reductions %||% character(0)))
    hd <- suppressWarnings(as.integer(o$harmony_dims %||% NA))
    hf <- o$harmony_finite
    cells <- suppressWarnings(as.integer(o$cells %||% NA))
    if (!("harmony" %in% reds))
      return(vchk("L4", nm, "fail",
        "对象里没有 harmony 降维，批次校正这一步实际没落地",
        expected = "reductions 含 harmony",
        actual = paste(reds, collapse = ", "),
        evidence = paste0(bo$rel, "（执行端读对象得到）")))
    if (is.na(hd) || hd <= 0)
      return(vchk("L4", nm, "fail", "harmony 降维维度异常",
        expected = "dims > 0", actual = as.character(hd),
        evidence = paste0(bo$rel, " 的 harmony embedding")))
    if (!isTRUE(hf))
      return(vchk("L4", nm, "fail",
        "harmony 嵌入里存在非有限值，校正结果不可用",
        expected = "全部有限", actual = "含 NA/Inf",
        evidence = paste0(bo$rel, " 的 harmony embedding")))
    if (is.na(cells) || cells <= 0)
      return(vchk("L4", nm, "unknown", "对象细胞数读不出",
        expected = rule, actual = "未给出", evidence = bo$rel))
    return(vchk("L4", nm, "pass",
      detail = paste0("技术结构核验通过：harmony 维度 ", hd,
                      "、值全为有限、对象 ", cells, " 个细胞。",
                      "只说明校正已落地且结果形状正常，",
                      "不构成生物学批次效应已消除的结论"),
      expected = rule,
      actual = paste0("reductions=", paste(reds, collapse = ","),
                      " harmony_dims=", hd, " finite=", as.character(hf)),
      evidence = paste0(bo$rel, "（执行端读对象得到，md5 已对齐）")))
  }
  if (identical(rule, "drop_flags_match_dispatch")) {
    # 逐簇标记表与派工清单对账。
    #   为什么不能只看这张表「非空」：keep_all 时它同样有 29 行，
    #   非空完全不代表没剔；必须把布尔列与派工清单逐簇比。
    rel_dc <- "7.after_drop/dropped_clusters.xls"
    tbd <- (facts$tables %||% list())[[rel_dc]]
    if (!is.list(tbd) || is.null(tbd$rows)) {
      return(vchk("L4", nm, "unknown",
        "回写里没有逐簇剔除标记表的读数，无法核对是否真的没剔",
        expected = rule, actual = "未给", evidence = rel_dc))
    }
    n_rows <- suppressWarnings(as.integer(tbd$rows))
    tru <- as.character(unlist(tbd$dropped_true %||% list()))
    n_false <- suppressWarnings(as.integer(tbd$dropped_false %||% NA))
    if (is.na(n_rows) || n_rows <= 0L) {
      return(vchk("L4", nm, "unknown",
        "逐簇标记表读到了 0 行，无法确认上游簇集合",
        expected = "行数 = 上游簇数", actual = as.character(n_rows), evidence = rel_dc))
    }
    # 布尔列必须覆盖全部行：TRUE 数 + FALSE 数 = 行数，否则说明有行没标。
    n_true <- length(tru)
    if (is.na(n_false) || n_true + n_false != n_rows) {
      return(vchk("L4", nm, "fail",
        detail = paste0("逐簇标记不完整：TRUE ", n_true, " + FALSE ",
                        if (is.na(n_false)) "未给" else as.character(n_false),
                        " != 行数 ", n_rows),
        expected = "每行都有 dropped 取值", actual = as.character(n_rows),
        evidence = rel_dc))
    }
    # 行数要与上游簇数一致（上游簇数由执行端读表带回）。
    n_up <- suppressWarnings(as.integer((facts$counts %||% list())$clusters_before %||% NA))
    if (!is.na(n_up) && n_up != n_rows) {
      return(vchk("L4", nm, "fail",
        detail = paste0("行数 ", n_rows, " 与上游簇数 ", n_up, " 不一致"),
        expected = paste0("行数 = ", n_up), actual = as.character(n_rows),
        evidence = rel_dc))
    }
    # 派工清单：用应用侧派工记录，不用回写体自报。
    dp <- facts$dispatch_params
    want <- NULL
    if (is.list(dp)) {
      dcl <- dp$drop_clusters
      if (is.null(dcl) && is.list(dp$params)) dcl <- dp$params$drop_clusters
      if (!is.null(dcl)) want <- as.character(unlist(dcl))
    }
    if (is.null(want)) {
      return(vchk("L4", nm, "unknown",
        "拿不到派工时的剔除清单（应用侧派工记录缺失），无法对账",
        expected = rule, actual = "未给", evidence = "派工记录"))
    }
    norm_set <- function(x) sort(unique(trimws(as.character(x[nzchar(trimws(as.character(x)))]))))
    got_set <- norm_set(tru)
    want_set <- norm_set(want)
    same <- identical(got_set, want_set)
    return(vchk("L4", nm, if (same) "pass" else "fail",
      detail = if (same)
                 paste0("逐簇标记与派工一致（标记剔除 ", length(got_set),
                        " 个簇，共 ", n_rows, " 行）")
               else paste0("逐簇标记与派工不一致：派工 ", length(want_set),
                           " 个簇 [", paste(want_set, collapse = ","),
                           "]，表里标 TRUE ", length(got_set), " 个 [",
                           paste(got_set, collapse = ","), "]"),
      expected = paste0("TRUE 集合 = 派工清单 [", paste(want_set, collapse = ","), "]"),
      actual = paste0("[", paste(got_set, collapse = ","), "]（", n_rows, " 行）"),
      evidence = paste0(rel_dc, " 与派工记录")))
  }
  if (identical(rule, "drop_params_consistent")) {
    # 三方口径一致性：应用派工的参数、集群上真实参数文件、脚本写出的生效记录表。
    #   为什么要三方：只查「参数有记录」这种弱证据，页面上选 A、实际跑 B
    #   也会 pass —— 那正是这条最该抓住的错。
    #   任何一方缺就 unknown：没有证据不能替产物背书。
    fp <- facts$filter_params
    if (!is.list(fp)) return(vchk("L4", nm, "unknown",
      "回写里没有筛群口径证据（派工参数 / 参数文件 / 生效记录表），一致性核不了",
      expected = rule, actual = "未给", evidence = "回写 filter_params{}"))
    # 基线必须用应用侧派工记录，而不是回写体自报的 dispatched。
    #   回写体是执行端写的：它可以把「派工参数」也一起改成实际跑的值，
    #   于是「三方一致」变成自己跟自己一致，什么也证明不了。
    #   应用侧的 exec 任务记录是派工那一刻写下的，执行端改不到它。
    disp <- facts$dispatch_params
    if (!is.list(disp) || !length(disp)) return(vchk("L4", nm, "unknown",
      "应用侧没有这一步的派工参数记录，无法确认页面选的是什么；一致性核不了",
      expected = rule, actual = "无派工基线", evidence = "state/exec_tasks.json"))
    fil <- fp$file; usd <- fp$used
    miss <- character(0)
    if (FALSE) miss <- c(miss, "派工参数")
    if (!is.list(fil)) miss <- c(miss, "参数文件")
    if (!is.list(usd)) miss <- c(miss, "生效记录表")
    if (length(miss)) return(vchk("L4", nm, "unknown",
      paste0("缺少：", paste(miss, collapse = "、"), "，三方一致性核不了"),
      expected = rule, actual = paste(miss, collapse = ","),
      evidence = "回写 filter_params{}"))

    # 校验和：两个文件都必须有 32 位 md5，否则无法确认读的就是这份文件。
    if (is.null(fil$md5) || is.null(usd$md5))
      return(vchk("L4", nm, "unknown",
        "参数文件或生效记录表没有 md5，无法确认读取的就是回写清单里的那份",
        expected = rule, actual = "md5 缺失", evidence = "回写 filter_params{}"))

    norm <- function(x) {
      v <- trimws(as.character(unlist(x)))
      v <- v[nzchar(v)]
      sort(unique(v))
    }
    # 缺字段走脚本默认值，但要把「这是默认值」写清楚，不能当成人选过。
    dis_dims <- if (nzchar(as.character(disp$dims %||% ""))) as.character(disp$dims) else "1:20"
    dis_res  <- if (nzchar(as.character(disp$resolution %||% ""))) as.character(disp$resolution) else "0.6"
    dis_drop <- norm(disp$drop_clusters)
    used_drop <- norm(strsplit(as.character(usd$drop_clusters %||% ""), "[,，[:space:]]+")[[1]])
    # 生效表用「（全部保留）」表示空清单。
    used_drop <- used_drop[!grepl("全部保留", used_drop)]

    bad <- character(0)
    if (!identical(norm(fil$drop_clusters), dis_drop))
      bad <- c(bad, paste0("参数文件的剔除清单与派工参数不一致（文件 ",
                           paste(norm(fil$drop_clusters), collapse = ",") %||% "空",
                           " / 派工 ", if (length(dis_drop)) paste(dis_drop, collapse = ",") else "空", "）"))
    if (!identical(used_drop, dis_drop))
      bad <- c(bad, paste0("生效记录表的剔除清单与派工参数不一致（表 ",
                           if (length(used_drop)) paste(used_drop, collapse = ",") else "空",
                           " / 派工 ", if (length(dis_drop)) paste(dis_drop, collapse = ",") else "空", "）"))
    if (!identical(as.character(fil$dims %||% ""), as.character(disp$dims %||% "")) &&
        nzchar(as.character(disp$dims %||% "")))
      bad <- c(bad, paste0("参数文件 dims 与派工不一致（", fil$dims, " / ", disp$dims, "）"))
    if (!identical(as.character(fil$resolution %||% ""), as.character(disp$resolution %||% "")) &&
        nzchar(as.character(disp$resolution %||% "")))
      bad <- c(bad, paste0("参数文件 resolution 与派工不一致（", fil$resolution, " / ", disp$resolution, "）"))
    if (!identical(as.character(usd$dims %||% ""), dis_dims))
      bad <- c(bad, paste0("实际生效 dims 与派工不一致（", usd$dims, " / ", dis_dims, "）"))
    if (!identical(as.character(usd$resolution %||% ""), dis_res))
      bad <- c(bad, paste0("实际生效 resolution 与派工不一致（", usd$resolution, " / ", dis_res, "）"))

    if (length(bad)) return(vchk("L4", nm, "fail",
      detail = paste0(paste(bad, collapse = "；"),
                      "。这说明页面上的选择与实际执行的不是同一套口径"),
      expected = rule,
      actual = paste0("派工 dims=", dis_dims, " res=", dis_res,
                      " 剔除=", if (length(dis_drop)) paste(dis_drop, collapse = ",") else "空"),
      evidence = "回写 filter_params{}（执行端读参数文件与生效记录表得到）"))

    return(vchk("L4", nm, "pass",
      detail = paste0("三方口径一致：dims=", dis_dims, "（", usd$dims_source %||% "-",
                      "）、resolution=", dis_res, "（", usd$resolution_source %||% "-",
                      "）、剔除清单=", if (length(dis_drop)) paste(dis_drop, collapse = ",") else "空",
                      "。这只证明记录与执行一致，不判断筛群选择在生物学上是否正确"),
      expected = rule, actual = paste0("dims=", dis_dims, " res=", dis_res),
      evidence = "回写 filter_params{}（两文件 md5 均已登记）"))
  }
  if (identical(rule, "drop_params_recorded")) {
    # 较弱的一条：只核「参数文件确实存在且有校验和」。
    #   它不能替代三方一致性，单独使用时不足以说明页面选的就是实际跑的。
    fp <- facts$filter_params
    if (!is.list(fp) || !is.list(fp$file)) return(vchk("L4", nm, "unknown",
      "回写里没有参数文件证据，无法核这一轮口径是否被记录",
      expected = rule, actual = "未给", evidence = "回写 filter_params{}"))
    ok <- !is.null(fp$file$md5)
    return(vchk("L4", nm, if (ok) "pass" else "unknown",
      detail = if (ok) "参数文件已记录且带校验和（仅表示有记录，不表示与派工一致）"
               else "参数文件没有校验和，无法确认是这一轮写的",
      expected = rule, actual = if (ok) "已记录" else "缺 md5",
      evidence = "回写 filter_params.file"))
  }
  if (identical(rule, "zip_manifest_consistent")) {
    # 判据只认执行端采证：verify_delivery.py --step pack 带回的 zip_structure。
    #   它做了 CRC 全量校验、逐文件与当前源文件的 md5 比对、缺件/多件清单，
    #   那才是「包与本次源文件一致」；这里只负责读结论并把不合格项点名。
    #   为什么不再自己数条目：条目数到得了 120 条，缺一张图也照样到得了；
    #   用条目数冒充一致性，等于给一个没核过的包盖通过章。
    #   结构缺字段或为空 → unknown，不许滑成 pass。
    zs <- facts$zip_structure
    if (!is.list(zs) || !length(zs))
      return(vchk("L4", nm, "unknown",
        detail = "回写没带 zip 采证结构，CRC 与源文件一致性都核不了",
        expected = rule, actual = "zip_structure 为空",
        evidence = "回写 zip_structure{}（verify_delivery.py --step pack）"))
    need <- c("rel", "md5", "entries", "figures", "has_pptx", "crc_ok",
              "source_match_ok", "has_pptx_manifest", "ok", "why")
    miss <- setdiff(need, names(zs))
    if (length(miss))
      return(vchk("L4", nm, "unknown",
        detail = paste0("zip 采证结构缺字段：", paste(miss, collapse = ", ")),
        expected = rule, actual = "字段缺失",
        evidence = "回写 zip_structure{}（verify_delivery.py --step pack）"))
    zrel <- as.character(zs$rel %||% "")
    zmd5 <- as.character(zs$md5 %||% "")
    hit <- Filter(function(a) identical(as.character(a$rel %||% ""), zrel),
                  facts$artifacts %||% list())
    if (!length(hit)) return(vchk("L4", nm, "fail",
      detail = "采证结构指向的 zip 不是本次产物清单里的那一份",
      expected = rule, actual = zrel,
      evidence = "回写 artifacts[]"))
    am <- as.character(hit[[1]]$md5 %||% "")
    if (!length(am) || is.na(am[1]) || !nzchar(am[1]))
      return(vchk("L4", nm, "unknown",
        detail = "产物清单没带这份 zip 的 md5，无法与采证结构对齐",
        expected = rule, actual = "artifacts[].md5 缺失",
        evidence = "回写 artifacts[]（zip 条目）"))
    if (!length(zmd5) || is.na(zmd5[1]) || !nzchar(zmd5[1]))
      return(vchk("L4", nm, "unknown",
        detail = "采证结构没带 md5，无法确认采证读的就是这份 zip",
        expected = rule, actual = "zip_structure.md5 缺失",
        evidence = "回写 zip_structure.md5"))
    if (!identical(zmd5[1], am[1]))
      return(vchk("L4", nm, "fail",
        detail = "采证结构的 md5 与本次 zip 产物的 md5 不一致，采证不能采信",
        expected = am[1], actual = zmd5[1],
        evidence = "回写 zip_structure.md5 与 artifacts[].md5"))
    ent <- suppressWarnings(as.integer(zs$entries))
    figs <- suppressWarnings(as.integer(zs$figures))
    if (length(ent) != 1L || is.na(ent) || length(figs) != 1L || is.na(figs))
      return(vchk("L4", nm, "unknown",
        detail = "采证结构里条目数或图数不可读，判不了",
        expected = rule, actual = "entries / figures 未给",
        evidence = zrel))
    flag_def <- list(ok = "ok", crc_ok = "crc_ok",
                     source_match_ok = "source_match_ok",
                     has_pptx_manifest = "has_pptx_manifest")
    for (k in names(flag_def)) {
      v <- zs[[k]]
      if (!is.logical(v) || length(v) != 1L || is.na(v))
        return(vchk("L4", nm, "unknown",
          detail = paste0("采证字段 ", k, " 不是明确的 TRUE/FALSE，判不了"),
          expected = rule, actual = paste0(k, " 未明确"),
          evidence = "回写 zip_structure{}"))
    }
    why <- as.character(zs$why %||% "")
    if (!length(why) || is.na(why[1]) || !nzchar(why[1]))
      why <- "采证脚本未给出原因"
    bad <- names(flag_def)[!vapply(flag_def, function(k) isTRUE(zs[[k]]), TRUE)]
    # has_pptx 不是采证必核项，但为 TRUE/FALSE 且为 FALSE 时同样要拦。
    if (isTRUE(is.logical(zs$has_pptx)) && identical(zs$has_pptx, FALSE))
      bad <- unique(c(bad, "has_pptx"))
    if (length(bad))
      return(vchk("L4", nm, "fail",
        detail = paste0("zip 采证未通过（", paste(bad, collapse = ", "), "）：", why[1]),
        expected = "ok / crc_ok / source_match_ok / has_pptx_manifest 全部为 TRUE",
        actual = paste0(paste(bad, collapse = ", "), " = FALSE"),
        evidence = paste0(zrel, "（verify_delivery.py --step pack）")))
    if (ent <= 0L) return(vchk("L4", nm, "fail",
      detail = "采证自称通过，但条目为 0：包是空的", expected = rule,
      actual = "0 条", evidence = zrel))
    if (figs <= 0L) return(vchk("L4", nm, "fail",
      detail = "采证自称通过，但包里没有任何图", expected = rule,
      actual = "0 张图", evidence = zrel))
    return(vchk("L4", nm, "pass",
      detail = paste0("zip 采证通过：", ent, " 条，图 ", figs,
                      " 张，含 pptx 与来源清单，CRC 正常且每个文件与当前源文件一致"),
      expected = "ok / crc_ok / source_match_ok / has_pptx_manifest 全部为 TRUE",
      actual = paste0("entries=", ent, " figs=", figs),
      evidence = paste0(zrel, "（verify_delivery.py --step pack，md5 已对齐）")))
  }
  if (identical(rule, "final_params_consistent")) {
    result <- function(status, detail) vchk("L4", nm, status, detail = detail,
      expected = "本次派工、参数文件、脚本生效记录及已验收筛群输入一致",
      actual = detail, evidence = "final_params / 本次产物清单 / 已验收上游记录")
    scalar <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
    hash <- function(x) scalar(x) && grepl("^[0-9a-f]{32}$", x)
    params <- function(x) {
      if (!is.list(x) || !all(c("keep_all_samples", "drop_samples", "diff_guard") %in% names(x))) return(NULL)
      if (!is.logical(x$keep_all_samples) || length(x$keep_all_samples) != 1L || is.na(x$keep_all_samples)) return(NULL)
      if (!(is.list(x$drop_samples) || is.character(x$drop_samples)) || is.null(x$drop_samples)) return(NULL)
      drops <- unlist(x$drop_samples, use.names = FALSE)
      if (length(drops) && (!is.character(drops) || anyNA(drops) || any(!nzchar(trimws(drops))))) return(NULL)
      if (!scalar(x$diff_guard) || !x$diff_guard %in% c("skip", "stop")) return(NULL)
      drops <- sort(unique(trimws(as.character(drops))))
      if (isTRUE(x$keep_all_samples) && length(drops)) return(NULL)
      if (!isTRUE(x$keep_all_samples) && !length(drops)) return(NULL)
      list(keep_all_samples = x$keep_all_samples, drop_samples = drops, diff_guard = x$diff_guard)
    }
    fp <- facts$final_params
    if (!is.list(fp) || !is.list(fp$file) || !is.list(fp$used) || !is.list(fp$input))
      return(result("unknown", "缺参数文件、脚本生效记录或实际输入证据"))
    d <- params(facts$dispatch_params); f <- params(fp$file); u <- params(fp$used)
    if (is.null(d) || is.null(f) || is.null(u)) return(result("unknown", "三方参数存在缺失、类型错误或自相矛盾，不能核验"))
    bind <- function(rel, md5, expected_rel = NULL) {
      if (!scalar(rel) || !hash(md5)) return("unknown")
      if (grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$)|\\\\)", rel)) return("fail")
      if (!is.null(expected_rel) && !identical(rel, expected_rel)) return("fail")
      arts <- Filter(function(a) identical(a$rel, rel), facts$artifacts %||% list())
      if (!length(arts)) return("unknown")
      if (length(arts) != 1L) return("fail")
      if (!hash(arts[[1]]$md5)) return("unknown")
      if (!identical(md5, arts[[1]]$md5)) return("fail")
      "pass"
    }
    pbind <- bind(fp$file$rel, fp$file$md5, "8.final/final_params.json")
    if (pbind != "pass") return(result(pbind, "参数文件与本次产物清单的路径或校验和未对齐"))
    lbind <- bind(fp$used$log_rel, fp$used$log_md5)
    if (lbind != "pass") return(result(lbind, "实际生效记录的日志未与本次真实日志产物对齐"))
    if (identical(fp$used$log_rel, fp$file$rel) || !grepl("(\\.log$|\\.o[0-9]*$|\\.out$)", fp$used$log_rel))
      return(result("fail", "实际生效记录必须来自执行日志，不能以参数文件替代"))
    expected_input <- "7.after_drop/merged_after_drop.rds"
    if (!scalar(fp$input$rel)) return(result("unknown", "缺实际输入路径"))
    if (!identical(fp$input$rel, expected_input)) return(result("fail", "实际输入不是本次筛群对象"))
    if (!hash(fp$input$md5) || !hash(fp$used$input_md5) || !hash(facts$filter_after_drop_md5))
      return(result("unknown", "缺实际输入、脚本读入或已验收上游的校验和"))
    if (!identical(fp$input$md5, facts$filter_after_drop_md5) || !identical(fp$used$input_md5, fp$input$md5))
      return(result("fail", "脚本读入对象与本次已验收筛群结果不一致"))
    if (!scalar(fp$used$input_rds) || !scalar(facts$run_dir)) return(result("unknown", "缺脚本实际输入路径或本次结果目录"))
    expected_absolute <- paste0(sub("/+$", "", facts$run_dir), "/", expected_input)
    if (!fp$used$input_rds %in% c(expected_input, expected_absolute))
      return(result("fail", "脚本实际输入路径指向其他对象或分析目录"))
    if (!identical(d, f) || !identical(d, u)) return(result("fail", "派工、参数文件与脚本实际生效参数不一致"))
    return(result("pass", "派工、参数文件与脚本生效参数一致；输入匹配本次已验收筛群产物。此检查不判断科学参数应如何选择。"))
  }

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
  # not_implemented 不能滑成 pass。
  #   改前它不参与判断，于是一条"判据还没写"的检查会被汇总成通过：
  #   看起来核过了，实际什么都没核。这比判错更坏 —— 它给出的是假的通过。
  #   现在按 unknown 处理：它不指责产物，只如实说"这一项无法核验"。
  if (any(st == "not_implemented")) return("unknown")
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
  # 参考基线由应用侧算好注入，回写体里写什么都不作数。
  facts$reference_baseline <- tryCatch(
    annotation_baseline(app_dir, b$session_id %||% att$session_id),
    error = function(e) NULL)

  # 逐样本环节的核对基准：应用侧声明的样本清单，不由回写体自报。
  #   取不到就保持空，L1 会退回只看回写体（并记 unknown），不会放宽成通过。
  if (exists("dc_declared_samples")) {
    facts$declared_samples <- tryCatch(dc_declared_samples(app_dir),
                                       error = function(e) character(0))
  }

  # 应用派工的原始参数：这是「页面选了什么」的唯一可信来源。
  #   为什么不能用回写体自报的那份：回写体是执行端写的，
  #   它完全可以同时改「派工参数」和「实际参数文件」让两份看起来一致 ——
  #   那样三方一致就成了自证。基线必须来自应用自己登记的 exec 任务。
  # 应用派工的原始参数：这是「页面选了什么」的唯一可信来源。
  #   为什么不能用回写体自报的那份：回写体是执行端写的，
  #   它可以把「派工参数」也一起改成实际跑的值，让三方看起来一致 ——
  #   那等于自己给自己作证。基线必须来自应用在派工那一刻写下的记录。
  # 筛群产物的 md5 基线：终版的输入 RDS 必须就是这一份。
  #   为什么从 remote_steps 取：那是筛群回写时登记的产物清单，
  #   执行端改不到它。没有它，「输入是这一次的」就只是一句自述。
  # 图的守恒基准：总细胞数 / 未定型数 / 已定型类型集合，全部来自已验收上游。
  #   判定器不重读 RDS，也不把本次数字写死在代码里。
  # session 只能来自回写体：step_attempts 没有 session_id 列，
  #   用 att$session_id 会永远是空串，上游基准就永远取不到 ——
  #   那样每个图步骤都会卡在"缺基准"，看起来像数据问题，其实是接线错了。
  facts$upstream <- tryCatch(
    verdict_figs_upstream(app_dir,
                          as.character(b$session_id %||% att$session_id %||% ""), con,
                          sg_wb = b$upstream_sample_group %||% NULL),
    error = function(e) NULL)
  facts$filter_after_drop_md5 <- tryCatch(
    verdict_filter_baseline(app_dir, facts, con), error = function(e) NULL)

  facts$dispatch_params <- tryCatch(
    verdict_dispatch_params(app_dir, b, facts), error = function(e) NULL)

  # 派工凭证：回写体没有 set_id 时，由应用侧按可核条件补。
  #   只在「唯一在案任务 + 身份一致 + 脚本路径吻合 + 指纹齐全 + 脚本 mtime
  #   不晚于派工时间」五条全过时才补，并记 provenance。
  #   任何一条不过就保持 NA，让合规层继续判 unknown，绝不放行。
  dc_note <- NULL
  if (exists("dc_resolve_set_id")) {
    have <- as.character(facts$set_id %||% NA)
    if (is.na(have) || !nzchar(have)) {
      dc <- tryCatch(
        dc_resolve_set_id(app_dir, as.character(att$step_id %||% facts$step_id),
                          b, script_root = dc_script_root(app_dir)),
        error = function(e) list(ok = FALSE, why = paste0("dispatch_cred_error:",
                                                          conditionMessage(e)),
                                 checks = list()))
      dc_note <- dc
      if (isTRUE(dc$ok)) {
        facts$set_id <- dc$set_id
        facts$set_id_provenance <- dc$provenance
      }
    } else {
      dc_note <- list(ok = TRUE, set_id = have, provenance = "reported",
                      why = "回写体自带 set_id", checks = list())
    }
  }

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
    # 派工凭证留痕：set_id 若是应用侧按可核条件补的，必须写明 provenance，
    #   让人事后能分清「执行端自报」与「应用派工给定」。
    #   补不出来时也留一条 unknown，写清卡在哪一条，不静默。
    if (!is.null(dc_note)) {
      if (isTRUE(dc_note$ok)) {
        checks[[length(checks) + 1L]] <- vchk(
          "L3", "set_id_provenance", "pass",
          detail = paste0("set_id 来源 = ", dc_note$provenance, "；", dc_note$why),
          expected = "application_dispatch", actual = as.character(dc_note$provenance),
          evidence = "engine/dispatch_cred.R")
      } else {
        checks[[length(checks) + 1L]] <- vchk(
          "L3", "set_id_provenance", "unknown",
          detail = paste0("派工凭证未成立：", dc_note$why),
          expected = "application_dispatch", actual = as.character(dc_note$why),
          evidence = paste0("检查项：",
                            paste(vapply(dc_note$checks %||% list(),
                                         function(x) paste0(x$name, "=", x$ok), ""),
                                  collapse = ", ")))
      }
    }
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
       # set_id 与它的来源一起交出去。
       #   为什么要带出来：这个值只在判定里算得出来，回写体里没有。
       #   护照若读回写体，就会与 DB 判定结论打架（一边 pass 一边 unknown）。
       set_id = as.character(facts$set_id %||% NA),
       set_id_provenance = as.character(facts$set_id_provenance %||% NA),
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
