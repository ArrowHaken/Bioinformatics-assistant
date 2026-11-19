#!/usr/bin/env Rscript
# =====================================================================
#  script_archive.R -- register the script that actually ran into the
#  script library (engine/library/archive)
#
#  WHY THIS EXISTS
#    The library used to be a snapshot taken by hand, once: three
#    records, all dated 2026-11-03. When a step finished, the report
#    carried script_path and script_md5, the judge checked them against
#    the expected script, and then nothing recorded them anywhere.
#    Result: the "script database" in the page could only ever show the
#    hand-made records. It never grew with real runs, and L2 (by stage)
#    and L3 (by record) stayed empty for every new analysis.
#
#  THREE RULES, IN ORDER OF IMPORTANCE
#    1. Content or nothing.
#       The script runs on the remote server; this app cannot read that
#       disk. We copy content in ONLY when a local file exists whose md5
#       equals the md5 the runner reported. Without matching content the
#       entry is still created (so the fact "this script ran" is kept),
#       but marked content_missing = TRUE. We never write a file we did
#       not actually see -- a fabricated copy would be worse than none.
#    2. Counters are per attempt id.
#       The same attempt reported twice (retry, duplicate callback) must
#       not inflate used_ok / used_fail.
#    3. Append only, atomic write.
#       Existing records are never dropped or rewritten from scratch.
#
#  ASCII-only source: R 4.1.2 on a Chinese Windows locale cannot parse
#  non-ASCII source. All user-facing wording lives in the web page.
# =====================================================================

if (!exists("%||%", mode = "function"))
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

sa_md5 <- function(p) {
  if (is.null(p) || !length(p) || is.na(p) || !nzchar(p) || !file.exists(p)) return("")
  unname(tools::md5sum(p))
}

sa_archive_root <- function(engine_dir) file.path(engine_dir, "library", "archive")
sa_index_file   <- function(engine_dir) file.path(sa_archive_root(engine_dir), "index.json")

# 运行时记录与随代码分发的底稿分开放（gina 2026-11-19）。
#   为什么必须分开：官方底稿（R1-official）是产品自带的东西，跟着仓库走没问题；
#   而每次分析登记进来的记录（SA-*）带的是某个人那一次处理的服务器路径、
#   归属标识和脚本清单 —— 那是运行数据，不该出现在公开仓库里。
#   以前两者写在同一个 index.json，一推送就把这条记录连同 owner_key 一起公开了。
#   现在：engine/library/archive/index.json 只读，放官方底稿；
#         运行时记录写 <app>/state/script_archive/index.json，state/ 已在忽略规则里。
sa_runtime_dir  <- function(app_dir) file.path(app_dir, "state", "script_archive")
sa_runtime_file <- function(app_dir) file.path(sa_runtime_dir(app_dir), "index.json")

sa_read_index_file <- function(f) {
  if (!file.exists(f)) return(list(records = list()))
  d <- tryCatch(jsonlite::fromJSON(paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                                         collapse = "\n"), simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(d) || !is.list(d)) return(list(records = list()))
  if (is.null(d$records) || !is.list(d$records)) d$records <- list()
  d
}

# 合并读取：官方底稿在前，运行时记录在后。同 id 以运行时为准。
sa_index_read <- function(engine_dir, app_dir = NULL) {
  off <- sa_read_index_file(sa_index_file(engine_dir))
  run <- if (!is.null(app_dir) && nzchar(app_dir)) sa_read_index_file(sa_runtime_file(app_dir))
         else list(records = list())
  recs <- c(off$records %||% list(), run$records %||% list())
  if (length(recs)) {
    ids <- vapply(recs, function(x) as.character(x$id %||% ""), "")
    keep <- !duplicated(ids, fromLast = TRUE)
    recs <- recs[keep]
  }
  list(built_at = off$built_at %||% run$built_at %||% "", records = recs)
}

sa_write_text <- function(p, txt) {
  dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE)
  tmp <- tempfile(".sa-", tmpdir = dirname(p))
  con <- file(tmp, open = "wb")
  writeBin(charToRaw(enc2utf8(txt)), con)
  close(con)
  if (!file.rename(tmp, p)) {
    file.copy(tmp, p, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(TRUE)
}

# 只写运行时记录，且只写到 state 下。
#   官方底稿（engine/library/archive/index.json）是只读的：它随代码分发，
#   一旦被运行时记录回写，下一次推送就会把别人的处理历史带上去。
sa_index_write <- function(app_dir, d) {
  recs <- d$records %||% list()
  # 运行时 = 自动登记的记录。官方底稿的 owner_key 为空且 auto 不为 TRUE。
  keep <- Filter(function(r) {
    id <- as.character(r$id %||% "")
    startsWith(id, "SA-") || isTRUE(r$auto)
  }, recs)
  out <- list(built_at = format(Sys.Date(), "%Y-%m-%d"), records = keep)
  sa_write_text(sa_runtime_file(app_dir),
                jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE,
                                 null = "null", digits = NA))
  invisible(out)
}

sa_safe <- function(x, dflt = "") {
  v <- gsub("[^A-Za-z0-9._-]", "_", as.character(x %||% ""))
  if (!nzchar(v)) dflt else v
}

# Is this a script we should archive? The pack step reports "zip".
sa_is_script <- function(p) {
  if (!nzchar(p)) return(FALSE)
  grepl("\\.(R|r|py|sh|pl)$", p)
}

# Candidate local files for a remote script path.
#   The app ships some of the same scripts under engine/ (sc-pipeline,
#   sc-plot-kit, sc-ppt-kit). The runner reports the remote absolute path,
#   usually .../_engine/<tail>. Keeping the tail is how we find the copy
#   that is byte-identical to what ran.
sa_local_candidates <- function(engine_dir, remote_path) {
  rp <- gsub("\\\\", "/", as.character(remote_path %||% ""))
  out <- character(0)
  m <- regmatches(rp, regexpr("_?engine/.*$", rp))
  if (length(m) && nzchar(m[1])) {
    tailp <- sub("^_?engine/", "", m[1])
    p <- file.path(engine_dir, tailp)
    if (file.exists(p)) out <- c(out, p)
  }
  bn <- basename(rp)
  if (nzchar(bn)) {
    hit <- tryCatch(list.files(engine_dir, pattern = paste0("^", gsub("\\.", "\\\\.", bn), "$"),
                               recursive = TRUE, full.names = TRUE),
                    error = function(e) character(0))
    if (length(hit)) out <- c(out, hit)
  }
  unique(out[nzchar(out)])
}

# Copy content in, or report honestly that we could not.
sa_content_for <- function(engine_dir, remote_path, want_md5) {
  cands <- sa_local_candidates(engine_dir, remote_path)
  if (!length(cands)) return(list(ok = FALSE, why = "no_local_copy"))
  if (nzchar(want_md5)) {
    for (p in cands) if (identical(sa_md5(p), want_md5)) return(list(ok = TRUE, path = p))
    return(list(ok = FALSE, why = "local_md5_mismatch"))
  }
  list(ok = TRUE, path = cands[1])
}

# Which archive records may this identity see?
#   脚本库的用户隔离（gina 2026-11-19）。口径与记录列表一致，不另立一套：
#     · 官方底稿 owner_key 为空 -> 所有人可见（产品自带，不是谁的处理历史）
#     · 自己的记录 -> 可见
#     · 管理员 -> 全部可见
#     · 记录隔离/修改隔离被显式关掉 -> 退回全员可见
#   返回 list(ids=..., isolation_on=..., admin=...) 供调用方回显原因。
sa_visible <- function(engine_dir, owner_key = "", is_admin = FALSE,
                       isolation_on = TRUE, app_dir = NULL) {
  d <- sa_index_read(engine_dir, app_dir)
  recs <- d$records %||% list()
  key <- trimws(as.character(owner_key %||% ""))
  ids <- character(0)
  for (rec in recs) {
    rid <- as.character(rec$id %||% "")
    own <- trimws(as.character(rec$owner_key %||% ""))
    ok <- if (!nzchar(own)) TRUE
          else if (!isTRUE(isolation_on)) TRUE
          else if (isTRUE(is_admin)) TRUE
          else nzchar(key) && identical(own, key)
    if (isTRUE(ok) && nzchar(rid)) ids <- c(ids, rid)
  }
  list(ids = ids, isolation_on = isTRUE(isolation_on), admin = isTRUE(is_admin))
}

# One record per analysis record (session). Stable id, so repeated runs of
# the same analysis extend the same record instead of creating a new one
# for every step.
#   owner_key：脚本库要按使用者隔离（gina 2026-11-19 要求）。
#   以前 archive 是全局一份，谁打开都看到同一批记录 —— 别人的分析用过的
#   脚本、路径、成功失败次数全都暴露。现在每条记录带上归属，读取端按
#   归属过滤；随应用分发的官方脚本库（owner 留空）仍然所有人可见，
#   因为那是产品自带的底稿，不是某个人的处理历史。
sa_record_for <- function(d, sid, session_name, run_dir, owner_key = "") {
  rid <- paste0("SA-", sa_safe(sid, "unknown"))
  for (i in seq_along((d$records %||% list()))) {
    if (identical(as.character(d$records[[i]]$id %||% ""), rid))
      return(list(rec = d$records[[i]], idx = i, id = rid, is_new = FALSE))
  }
  rec <- list(
    id = rid,
    name = if (nzchar(session_name)) session_name else rid,
    when = format(Sys.Date(), "%Y-%m-%d"),
    where = as.character(run_dir %||% ""),
    note = "随每次环节验收自动登记：只收实际跑过并且核对过指纹的脚本。",
    auto = TRUE,
    session_id = as.character(sid %||% ""),
    owner_key = as.character(owner_key %||% ""),
    files = list())
  list(rec = rec, idx = NA_integer_, id = rid, is_new = TRUE)
}

# Register one execution.
#   ok = TRUE  -> the step was verified (counts as used_ok)
#   ok = FALSE -> the step was rejected/failed (counts as used_fail)
sa_register <- function(app_dir, engine_dir, sid, session_name = "", run_dir = "",
                        step, script_path, script_md5 = "", attempt_id = "",
                        ok = TRUE, owner_key = "") {
  sp <- as.character(script_path %||% "")
  if (!sa_is_script(sp)) return(list(ok = FALSE, why = "not_a_script"))
  st <- as.character(step %||% "")
  if (!nzchar(st)) return(list(ok = FALSE, why = "no_step"))

  # 归一化指纹：NA 与空串都算"没给指纹"。
  #   不这么处理的话，DBI 读出来的 NA 会变成字符串 "NA"，
  #   于是被当成一个真实的 md5 —— 既不匹配任何文件，又能拼出 ...NA.R 这种名字。
  want <- as.character(script_md5 %||% "")
  want <- if (!length(want) || is.na(want[1])) "" else tolower(trimws(want[1]))
  if (identical(want, "na")) want <- ""
  d <- sa_index_read(engine_dir, app_dir)
  got <- sa_record_for(d, sid, session_name, run_dir, owner_key)
  rec <- got$rec
  # 老记录没有归属字段：补上调用方给的归属，避免它们永远无主而谁都看不到。
  if (!isTRUE(got$is_new) && !nzchar(as.character(rec$owner_key %||% "")) &&
      nzchar(as.character(owner_key %||% "")))
    rec$owner_key <- as.character(owner_key)

  base_name <- basename(gsub("\\\\", "/", sp))
  # Does this record already carry this script?
  #   Match on content first. When one side has no fingerprint (older
  #   callbacks did not report one), fall back to the file name -- but only
  #   if the other side has no fingerprint either. Two entries that BOTH
  #   carry a fingerprint and disagree are genuinely different versions of
  #   the same file name and must stay apart, otherwise the "不一致就存成
  #   新的一份" rule would be quietly broken by a name match.
  fi <- NA_integer_
  for (i in seq_along((rec$files %||% list()))) {
    f <- rec$files[[i]]
    fmd5 <- tolower(trimws(as.character(f$md5 %||% "")))
    same_name <- identical(as.character(f$name %||% ""), base_name)
    if (nzchar(want) && nzchar(fmd5)) {
      if (identical(fmd5, want)) { fi <- i; break }
      next
    }
    if (!nzchar(want) && !nzchar(fmd5) && same_name) { fi <- i; break }
    if (same_name && (!nzchar(want) || !nzchar(fmd5))) { fi <- i; break }
  }

  # Content: only when we can prove it is the same bytes.
  content <- list(ok = FALSE, why = "already_present")
  if (is.na(fi)) content <- sa_content_for(engine_dir, sp, want)

  if (is.na(fi)) {
    rel <- if (content$ok) file.path("script", base_name) else file.path("script", base_name)
    rel <- gsub("\\\\", "/", rel)
    # Two different contents can share a basename; keep them apart.
    #   区分标记优先用指纹前 8 位；老回写没有指纹时用它自己的来源路径算一个短标记，
    #   否则会拼出 ...NA.R 这种既没信息又容易撞名的后缀。
    tag <- substr(gsub("[^a-z0-9]", "", want), 1, 8)
    if (!nzchar(tag)) tag <- substr(gsub("[^a-z0-9]", "", tolower(sp)), nchar(sp) - 7, nchar(sp))
    if (!nzchar(tag)) tag <- "copy"
    for (j in seq_along((rec$files %||% list()))) {
      if (identical(as.character(rec$files[[j]]$rel %||% ""), rel)) {
        ext0 <- tools::file_ext(base_name)
        stem <- sub(paste0("\\.", ext0, "$"), "", base_name)
        rel <- paste0("script/", stem, ".", tag,
                      if (nzchar(ext0)) paste0(".", ext0) else "")
        break
      }
    }
    entry <- list(
      name = base_name,
      rel = rel,
      bytes = if (content$ok) as.numeric(file.size(content$path)) else NA,
      mtime = if (content$ok) format(file.info(content$path)$mtime, "%Y-%m-%d %H:%M") else "",
      stage = st,
      used_ok = 0, used_fail = 0,
      md5 = want,
      source_path = sp,
      content_missing = !isTRUE(content$ok),
      content_note = if (isTRUE(content$ok)) "" else
        paste0("没有与本机内容逐字节一致的副本（", content$why,
               "），因此只登记了来源路径与指纹，没有伪造内容。"),
      attempts = list())
    if (isTRUE(content$ok)) {
      dst <- file.path(sa_archive_root(engine_dir), rec$id, rel)
      dir.create(dirname(dst), showWarnings = FALSE, recursive = TRUE)
      okc <- tryCatch(file.copy(content$path, dst, overwrite = TRUE), error = function(e) FALSE)
      if (!isTRUE(okc)) {
        entry$content_missing <- TRUE
        entry$content_note <- "复制本机同名脚本时失败，只登记了来源路径与指纹。"
      }
    }
    rec$files[[length(rec$files) + 1L]] <- entry
    fi <- length(rec$files)
  }

  # Counter update, deduped by attempt id.
  aid <- as.character(attempt_id %||% "")
  seen <- as.character(unlist(rec$files[[fi]]$attempts %||% list()))
  new_count <- !nzchar(aid) || !(aid %in% seen)
  if (new_count) {
    if (isTRUE(ok)) rec$files[[fi]]$used_ok <- as.integer(rec$files[[fi]]$used_ok %||% 0L) + 1L
    else            rec$files[[fi]]$used_fail <- as.integer(rec$files[[fi]]$used_fail %||% 0L) + 1L
    if (nzchar(aid)) {
      rec$files[[fi]]$attempts <- c(as.list(seen), list(aid))
      # Keep the tail only: this is a "已计过" marker, not an audit trail.
      n0 <- length(rec$files[[fi]]$attempts)
      if (n0 > 50L) rec$files[[fi]]$attempts <- rec$files[[fi]]$attempts[(n0 - 49L):n0]
    }
  }
  rec$files[[fi]]$stage <- st

  if (isTRUE(got$is_new)) d$records[[length(d$records) + 1L]] <- rec
  else d$records[[got$idx]] <- rec

  sa_index_write(app_dir, d)
  list(ok = TRUE, record = rec$id, file = rec$files[[fi]]$name,
       rel = rec$files[[fi]]$rel, counted = new_count,
       content = if (isTRUE(rec$files[[fi]]$content_missing)) "missing" else "local")
}

# Backfill from what the database already knows.
#   Every verified/rejected attempt carries step_id, script_path and
#   script_digest. Without this, the feature would only start working on
#   the next run and all existing history would stay invisible -- which is
#   exactly the complaint that started this work.
sa_backfill <- function(con, app_dir, engine_dir, limit = 2000L) {
  if (is.null(con)) return(list(ok = FALSE, why = "no_db"))
  # step_attempts 里没有 session_id：它只挂在 run 上，run 上是 work_root。
  #   所以回填必须经 runs.work_root 找回会话，不能凭空拼一个 session_id，
  #   否则同一条分析记录的脚本会被拆到好几条不相干的记录下。
  rows <- tryCatch(DBI::dbGetQuery(con,
    "SELECT s.session_id AS session_id, s.name AS session_name,
            s.owner_key AS owner_key,
            r.work_root AS work_root,
            a.step_id AS step_id, a.script_path AS script_path,
            a.script_digest AS script_digest, a.status AS status,
            a.attempt_id AS attempt_id
       FROM step_attempts a
       JOIN runs r ON r.run_id = a.run_id
       LEFT JOIN sessions s ON s.run_dir = r.work_root
      ORDER BY a.rowid ASC LIMIT ?", list(as.integer(limit))),
    error = function(e) NULL)
  if (is.null(rows) || !nrow(rows)) return(list(ok = TRUE, registered = 0L, skipped = 0L))

  n_ok <- 0L; n_skip <- 0L; n_fail <- 0L
  for (i in seq_len(nrow(rows))) {
    r <- rows[i, ]
    sid <- as.character(r$session_id %||% "")
    sp <- as.character(r$script_path %||% "")
    if (!nzchar(sid) || is.na(sp) || !nzchar(sp)) { n_skip <- n_skip + 1L; next }
    res <- tryCatch(sa_register(
      app_dir, engine_dir,
      sid = sid,
      session_name = as.character(r$session_name %||% ""),
      run_dir = as.character(r$work_root %||% ""),
      step = as.character(r$step_id %||% ""),
      script_path = sp,
      script_md5 = as.character(r$script_digest %||% ""),
      attempt_id = as.character(r$attempt_id %||% ""),
      # 回填也要写归属，否则历史记录永远无主（无主=所有人可见）。
      owner_key = as.character(r$owner_key %||% ""),
      ok = identical(as.character(r$status %||% ""), "verified")),
      error = function(e) list(ok = FALSE, why = conditionMessage(e)))
    if (isTRUE(res$ok)) n_ok <- n_ok + 1L else { n_skip <- n_skip + 1L; n_fail <- n_fail + 1L }
  }
  list(ok = TRUE, registered = n_ok, skipped = n_skip, failed = n_fail,
       records = length(sa_index_read(engine_dir, app_dir)$records))
}
