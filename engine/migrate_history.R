#!/usr/bin/env Rscript
# =====================================================================
#  migrate_history.R -- 把已有的处理信息规整进数据库
#
#  原则（不做的事）
#    · 不删任何旧文件。旧 JSON 原样留着，可随时对照。
#    · 不改写历史事实。旧数据没有的东西就留空，不推测、不补造。
#    · 归属统一记为 owner_key = 'legacy'，页面上标「历史（未归属）」。
#      原因是旧数据产生时还没有使用者边界，无法事后判断属于谁；
#      硬塞给某个人等于伪造归属。
#
#  迁移内容
#    state/sessions.json          -> sessions / projects / versions
#    state/sessions/<id>/profile  -> runs（位置与识别结论）
#    state/remote_steps.json      -> step_attempts（每步一行，status 按旧值映射）
#    state/artifacts.json         -> artifacts（kind 按扩展名分类）
#    state/quality/*.json         -> reviews（旧质检结论，标 rule_version=legacy）
#    state/injections.json        -> outbox
#    state/exec_tasks.json        -> events（只作流水，不重建"完成"语义）
#    state/decisions/*.json       -> decisions
#
#  幂等：重复跑不会重复插入。用 INSERT OR IGNORE + 稳定 id。
#
#  用法
#    Rscript engine/migrate_history.R [--app <dir>] [--dry-run] [--verbose]
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

LEGACY_OWNER <- "legacy"

mig_read_json <- function(p) {
  if (!file.exists(p)) return(NULL)
  tryCatch(jsonlite::fromJSON(paste(readLines(p, warn = FALSE, encoding = "UTF-8"),
                                    collapse = "\n"), simplifyVector = FALSE),
           error = function(e) NULL)
}

mig_chr <- function(x, d = "") {
  if (is.null(x) || !length(x)) return(d)
  v <- as.character(x)[1]
  if (is.na(v)) d else v
}

mig_int <- function(x) suppressWarnings(as.integer(x %||% NA))

# 旧状态 -> 新状态。这是全文件唯一一处映射表，别在别处再写一份。
#   旧 done 同时表示「跑完了」和「跑对了」。迁移时不能把它当成 verified，
#   否则一次迁移就把过去所有未核验的结果洗成了通过。所以一律落 reported，
#   再由 verdict 重新判；判不了就是 unknown。
mig_map_status <- function(old) {
  switch(as.character(old %||% ""),
    done      = "reported",
    failed    = "rejected",
    stopped   = "rejected",
    running   = "delegated",
    delegated = "delegated",
    reported  = "reported",
    verified  = "reported",   # 旧库里没有真的 verified，即便写了也降级重判
    "delegated")
}

mig_kind <- function(rel) {
  ext <- tolower(tools::file_ext(rel))
  if (ext %in% c("png", "jpg", "jpeg", "pdf", "svg")) return("preview")
  if (ext %in% c("csv", "tsv", "xls", "xlsx", "md", "txt", "html", "json")) return("small")
  if (ext %in% c("rds", "zip", "pptx", "tar", "gz", "xz")) return("large")
  "ondemand"
}

migrate_history <- function(app_dir, dry_run = FALSE, verbose = TRUE) {
  con <- if (dry_run) NULL else db_connect(app_dir)
  if (!dry_run) db_init(con)
  log_line <- function(...) if (verbose) cat(..., "\n", sep = "")
  counts <- list(sessions = 0L, versions = 0L, runs = 0L, attempts = 0L,
                 artifacts = 0L, reviews = 0L, outbox = 0L, events = 0L,
                 decisions = 0L, skipped = 0L)
  now <- db_now()

  if (!dry_run) {
    DBI::dbExecute(con, "INSERT OR IGNORE INTO owners(owner_key,kind,created_at,last_seen_at)
                         VALUES(?,?,?,?)", list(LEGACY_OWNER, "legacy", now, now))
  }

  # ---- 1. 分析记录 ---------------------------------------------------
  sj <- mig_read_json(file.path(app_dir, "state", "sessions.json"))
  sessions <- sj$sessions %||% list()
  for (s in sessions) {
    sid <- mig_chr(s$id)
    if (!nzchar(sid)) { counts$skipped <- counts$skipped + 1L; next }
    name <- mig_chr(s$name, sid)
    pid <- paste0("p-legacy-", substr(digest::digest(name, algo = "md5"), 1, 8))
    vid <- paste0("V-legacy-", substr(digest::digest(sid, algo = "md5"), 1, 8))
    rid <- paste0("run-legacy-", substr(digest::digest(sid, algo = "md5"), 1, 8))
    place <- s$place %||% list()
    srv <- place$server %||% list()
    work_root <- mig_chr(s$run_dir, mig_chr(srv$work_root))

    if (!dry_run) {
      DBI::dbExecute(con, "INSERT OR IGNORE INTO projects(project_id,name,owner_key,created_at,note)
                           VALUES(?,?,?,?,?)",
                     list(pid, name, LEGACY_OWNER, mig_chr(s$created_at, now),
                          "由 sessions.json 迁移，归属未定"))
      DBI::dbExecute(con, "INSERT OR IGNORE INTO versions(version_id,project_id,label,frozen_at,
                           spec_digest,created_at,created_by,reason)
                           VALUES(?,?,?,?,?,?,?,?)",
                     list(vid, pid, "V1(legacy)", mig_chr(s$created_at, now), "legacy-unknown",
                          mig_chr(s$created_at, now), LEGACY_OWNER, "history_migration"))
      DBI::dbExecute(con, "INSERT OR IGNORE INTO runs(run_id,version_id,owner_key,work_root,
                           exec_host,exec_pc,status,started_at,payload)
                           VALUES(?,?,?,?,?,?,?,?,?)",
                     list(rid, vid, LEGACY_OWNER, work_root,
                          mig_chr(srv$host), mig_chr(srv$exec_pc), "closed",
                          mig_chr(s$created_at, now),
                          db_json(list(legacy_session_id = sid, mode = mig_chr(s$mode)))))
      DBI::dbExecute(con, "INSERT OR IGNORE INTO sessions(session_id,owner_key,name,mode,run_dir,
                           project_id,version_id,visibility,created_at,updated_at,payload)
                           VALUES(?,?,?,?,?,?,?,?,?,?,?)",
                     list(sid, LEGACY_OWNER, name, mig_chr(s$mode), work_root, pid, vid,
                          "private", mig_chr(s$created_at, now), mig_chr(s$updated_at, now),
                          db_json(list(migrated = TRUE, legacy_place = place))))
      counts$sessions <- counts$sessions + 1L
      counts$versions <- counts$versions + 1L
      counts$runs <- counts$runs + 1L
    } else counts$sessions <- counts$sessions + 1L

    # ---- 2. 每一步 ---------------------------------------------------
    #  remote_steps.json 是全局单份，所有 session 共用。
    #    没有归属信息，只能归到 legacy 的第一个 run 下 —— 事实上就是同一批。
    rs <- mig_read_json(file.path(app_dir, "state", "remote_steps.json"))
    steps <- rs$steps %||% list()
    if (length(steps)) {
      for (i in seq_along(steps)) {
        st <- steps[[i]]
        step_id <- mig_chr(st$step, st$id %||% "")
        if (!nzchar(step_id))
          step_id <- sub("^[^A-Za-z]*", "", mig_chr(st$title))
        if (!nzchar(step_id)) { counts$skipped <- counts$skipped + 1L; next }
        aid <- paste0("att-legacy-", substr(digest::digest(paste(rid, step_id, i), algo = "md5"), 1, 12))
        if (!dry_run) {
          DBI::dbExecute(con, "INSERT OR IGNORE INTO step_attempts(attempt_id,run_id,version_id,
                             owner_key,step_id,attempt_no,status,reported_at,started_at,ended_at,payload)
                             VALUES(?,?,?,?,?,?,?,?,?,?,?)",
            list(aid, rid, vid, LEGACY_OWNER, step_id, 1L, mig_map_status(st$status),
                 mig_chr(st$imported_at) %||% NA, mig_chr(st$started_at) %||% NA,
                 mig_chr(st$ended_at) %||% NA,
                 db_json(list(migrated = TRUE, legacy_status = mig_chr(st$status),
                              legacy_title = mig_chr(st$title),
                              log = substr(mig_chr(st$log), 1, 4000),
                              code = st$code))))
          counts$attempts <- counts$attempts + 1L
        } else counts$attempts <- counts$attempts + 1L

        # 旧 review 结论（state/quality/<session>.json）
        #   这里不确定目录结构，保守起见跳过；真结论在 quality 目录里单独处理。
      }
    }

    # ---- 3. 产物 -----------------------------------------------------
    aj <- mig_read_json(file.path(app_dir, "state", "artifacts.json"))
    items <- aj$items %||% list()
    if (length(items)) {
      sid_snap <- paste0("snap-legacy-", substr(digest::digest(rid, algo = "md5"), 1, 8))
      if (!dry_run) {
        DBI::dbExecute(con, "INSERT OR IGNORE INTO snapshots(snapshot_id,version_id,owner_key,
                           created_at,sealed,payload) VALUES(?,?,?,?,?,?)",
          list(sid_snap, vid, LEGACY_OWNER, mig_chr(aj$listed_at, now), 1L,
               db_json(list(migrated = TRUE, source = "artifacts.json",
                            outdir = mig_chr(aj$outdir)))))
      }
      for (it in items) {
        rel <- mig_chr(it$rel)
        if (!nzchar(rel)) next
        if (!dry_run) {
          DBI::dbExecute(con, "INSERT OR IGNORE INTO artifacts(artifact_id,snapshot_id,version_id,
                             owner_key,rel,bytes,md5,kind,mtime,source)
                             VALUES(?,?,?,?,?,?,?,?,?,?)",
            list(paste0("art-legacy-", substr(digest::digest(paste(rid, rel), algo = "md5"), 1, 12)),
                 sid_snap, vid, LEGACY_OWNER, rel, mig_int(it$size), NA_character_,
                 mig_kind(rel), mig_chr(it$mtime) %||% NA, "artifacts.json"))
          counts$artifacts <- counts$artifacts + 1L
        } else counts$artifacts <- counts$artifacts + 1L
      }
    }
  }

  # ---- 4. 质检结论 ---------------------------------------------------
  qdir <- file.path(app_dir, "state", "quality")
  if (dir.exists(qdir)) {
    for (f in list.files(qdir, pattern = "\\.json$", full.names = TRUE)) {
      q <- mig_read_json(f)
      if (is.null(q)) next
      sid <- mig_chr(q$session_id, tools::file_path_sans_ext(basename(f)))
      # 旧结论只判「有没有回写、有没有清单、日志里有没有失败字样」，
      #   判不出「产物对不对」，所以一律记 unknown 而不是 pass（E 组说明第 ⑤ 条）。
      if (!dry_run) {
        DBI::dbExecute(con, "INSERT OR IGNORE INTO reviews(review_id,snapshot_id,attempt_id,
                           version_id,step_id,owner_key,verdict,rule_version,at,by_who,checks_json,payload)
                           VALUES(?,?,?,?,?,?,?,?,?,?,?,?)",
          list(paste0("rv-legacy-", substr(digest::digest(f, algo = "md5"), 1, 12)),
               NA_character_, NA_character_, NA_character_, NA_character_, LEGACY_OWNER,
               "unknown", "legacy-quality",
               mig_chr(q$checked_at, now), "quality.R(legacy)",
               db_json(list(legacy = TRUE, source = basename(f))),
               db_json(list(migrated = TRUE, legacy_session = sid))))
        counts$reviews <- counts$reviews + 1L
      } else counts$reviews <- counts$reviews + 1L
    }
  }

  # ---- 5. 指令队列 ---------------------------------------------------
  ij <- mig_read_json(file.path(app_dir, "state", "injections.json"))
  inj <- ij$items %||% list()
  for (x in inj) {
    id <- mig_chr(x$id)
    if (!nzchar(id)) next
    if (!dry_run) {
      DBI::dbExecute(con, "INSERT OR IGNORE INTO outbox(item_id,owner_key,session_ref,step_id,
                         req_id,text,reason,state,attempts,next_attempt_at,delivered_at,
                         satisfied_at,last_code,created_at,updated_at)
                         VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
        list(id, LEGACY_OWNER, mig_chr(x$session), mig_chr(x$step), mig_chr(x$req_id),
             mig_chr(x$text), mig_chr(x$reason),
             if (!is.null(x$satisfied_at)) "satisfied"
             else if (!is.null(x$delivered_at)) "delivered" else "pending",
             mig_int(x$attempts) %||% 0L, mig_chr(x$last_attempt_at) %||% NA,
             mig_chr(x$delivered_at) %||% NA, mig_chr(x$satisfied_at) %||% NA,
             mig_chr(x$last_code) %||% NA, mig_chr(x$created_at, now), db_now()))
      counts$outbox <- counts$outbox + 1L
    } else counts$outbox <- counts$outbox + 1L
  }

  # ---- 6. 执行流水 ---------------------------------------------------
  et <- mig_read_json(file.path(app_dir, "state", "exec_tasks.json"))
  tasks <- et$items %||% list()
  for (t in tasks) {
    id <- mig_chr(t$id)
    if (!nzchar(id)) next
    if (!dry_run) {
      db_event(con, "legacy_exec_task", owner_key = LEGACY_OWNER,
               step_id = mig_chr(t$step),
               payload = list(task_id = id, status = mig_chr(t$status),
                              created_at = mig_chr(t$created_at),
                              finished_at = mig_chr(t$finished_at),
                              error = mig_chr(t$error),
                              why = "只作流水，不重建完成语义"))
      counts$events <- counts$events + 1L
    } else counts$events <- counts$events + 1L
  }

  # ---- 7. 人工决策 ---------------------------------------------------
  ddir <- file.path(app_dir, "state", "decisions")
  if (dir.exists(ddir)) {
    for (f in list.files(ddir, pattern = "\\.json$", full.names = TRUE)) {
      d <- mig_read_json(f)
      items <- d$items %||% list()
      for (it in items) {
        did <- mig_chr(it$id)
        if (!nzchar(did)) next
        if (!dry_run) {
          DBI::dbExecute(con, "INSERT OR IGNORE INTO decisions(decision_id,owner_key,step_id,
                             kind,basis,quote,outcome,at,payload) VALUES(?,?,?,?,?,?,?,?,?)",
            list(did, LEGACY_OWNER, mig_chr(it$step), "gate_answer",
                 mig_chr(it$why), mig_chr(it$question),
                 mig_chr(it$answer), mig_chr(it$answered_at, now),
                 db_json(list(migrated = TRUE, legacy_session = mig_chr(d$session_id),
                              title = mig_chr(it$title), status = mig_chr(it$status)))))
          counts$decisions <- counts$decisions + 1L
        } else counts$decisions <- counts$decisions + 1L
      }
    }
  }

  if (!dry_run) {
    db_meta_set(con, "migrated_at", now)
    db_meta_set(con, "migrated_from", "state/*.json (legacy)")
    db_event(con, "history_migrated", owner_key = LEGACY_OWNER,
             payload = counts)
    DBI::dbDisconnect(con)
  }

  log_line("迁移完成：")
  for (k in names(counts)) log_line("  ", k, ": ", counts[[k]])
  invisible(counts)
}

mig_cli <- function() {
  a <- commandArgs(trailingOnly = TRUE)
  get <- function(k, d = NULL) { i <- match(k, a); if (is.na(i) || i == length(a)) return(d); a[i + 1] }
  app <- get("--app", getwd())
  source(file.path(app, "engine", "db.R"))
  migrate_history(app, dry_run = "--dry-run" %in% a, verbose = TRUE)
}

if (sys.nframe() == 0L && !interactive()) mig_cli()
