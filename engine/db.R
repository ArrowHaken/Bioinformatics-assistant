#!/usr/bin/env Rscript
# =====================================================================
#  db.R -- 应用的唯一事实来源（SQLite）
#
#  为什么需要这个文件
#    在它之前，「这一步跑到哪了」这件事散在七八个 JSON 里：
#      state/sessions.json        有哪些分析记录
#      state/remote_steps.json    每一步的状态
#      state/artifacts.json       有哪些产物
#      state/quality/*.json       质检结论
#      state/injections.json      指令送到哪儿了
#      state/exec_tasks.json      执行队列
#    每份文件各写各的，没有共同的键。于是「这张图是 V1 还是 V2 的」
#    这类问题只能靠目录名猜。本文件把六层身份落成表：
#      project -> version -> run -> step_attempt -> snapshot -> artifact
#    并把所有状态变化写成只追加的 events，页面只做投影。
#
#  硬规则（不是建议）
#    1. 取数必须带 version_id。不存在「查最新」这种接口语义。
#       —— 只要允许「最新」，晚到的旧回写就能把新结果盖掉，而且页面看不出来。
#    2. events 只追加。UPDATE / DELETE 由触发器直接 RAISE(ABORT)。
#       —— 台账能被改，闭环就作废；这条是 K1 第 7 项的地基。
#    3. verified 状态只有判定器能写（verdict.R），任何 HTTP 入口都不许写。
#       —— 「完成」的定义权不能留在被判定的一方手里。
#    4. 所有读写带 owner_key。身份拿不到时 fail-closed，不降级成全局视图。
#
#  兼容性
#    旧 JSON 文件不删、不改，只作为迁移来源（migrate_history.R）。
#    迁移后的归属是 owner_key = 'legacy'，页面上标「历史（未归属）」。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---- 路径 ------------------------------------------------------------

db_path <- function(app_dir) file.path(app_dir, "state", "app.db")

db_state_dir <- function(app_dir) file.path(app_dir, "state")

# 每个使用者的私有目录（B2）。共享资产留在 state/ 根下。
user_dir <- function(app_dir, owner_key) {
  ok <- gsub("[^A-Za-z0-9_.:@-]", "_", as.character(owner_key %||% "anonymous"))
  file.path(app_dir, "state", "users", ok)
}

user_subdir <- function(app_dir, owner_key, ...) {
  p <- do.call(file.path, c(list(user_dir(app_dir, owner_key)), list(...)))
  dir.create(p, showWarnings = FALSE, recursive = TRUE)
  p
}

# ---- 连接 ------------------------------------------------------------

db_connect <- function(app_dir, create = TRUE) {
  if (!requireNamespace("RSQLite", quietly = TRUE))
    stop("缺少 RSQLite 包：install.packages('RSQLite')")
  dir.create(db_state_dir(app_dir), showWarnings = FALSE, recursive = TRUE)
  p <- db_path(app_dir)
  if (!create && !file.exists(p)) return(NULL)
  con <- DBI::dbConnect(RSQLite::SQLite(), p)
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 8000")
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  con
}

db_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

db_iso_add <- function(secs, from = Sys.time()) {
  format(from + secs, "%Y-%m-%dT%H:%M:%S")
}

# ---- schema ----------------------------------------------------------

DB_SCHEMA_VERSION <- "1"

db_init <- function(con) {
  stmts <- c(
    "CREATE TABLE IF NOT EXISTS schema_meta (
       key TEXT PRIMARY KEY, value TEXT NOT NULL)",

    # ---- 使用者 ----
    "CREATE TABLE IF NOT EXISTS owners (
       owner_key TEXT PRIMARY KEY,
       kind TEXT NOT NULL,            -- viewer | topic | anonymous | legacy
       external_id TEXT,
       created_at TEXT NOT NULL,
       last_seen_at TEXT)",

    # ---- 六层身份：project / version / run / attempt / snapshot / artifact ----
    "CREATE TABLE IF NOT EXISTS projects (
       project_id TEXT PRIMARY KEY,
       name TEXT NOT NULL,
       owner_key TEXT NOT NULL,
       created_at TEXT NOT NULL,
       note TEXT,
       visibility TEXT NOT NULL DEFAULT 'private',
       UNIQUE(owner_key, name))",

    # 版本一旦 frozen 就不再改。改样本/参数/脚本/参考依据/人工决策 -> 派生新版本。
    "CREATE TABLE IF NOT EXISTS versions (
       version_id TEXT PRIMARY KEY,
       project_id TEXT NOT NULL REFERENCES projects(project_id),
       parent_version_id TEXT REFERENCES versions(version_id),
       label TEXT,
       frozen_at TEXT,
       spec_digest TEXT NOT NULL,
       created_at TEXT NOT NULL,
       created_by TEXT,
       reason TEXT,
       payload TEXT)",

    "CREATE TABLE IF NOT EXISTS runs (
       run_id TEXT PRIMARY KEY,
       version_id TEXT NOT NULL REFERENCES versions(version_id),
       owner_key TEXT NOT NULL,
       work_root TEXT,
       exec_host TEXT,
       exec_pc TEXT,
       status TEXT NOT NULL DEFAULT 'open',
       started_at TEXT,
       ended_at TEXT,
       payload TEXT)",

    # 一次尝试一行，永不覆盖。重跑 = 新 attempt_no。
    "CREATE TABLE IF NOT EXISTS step_attempts (
       attempt_id TEXT PRIMARY KEY,
       run_id TEXT NOT NULL REFERENCES runs(run_id),
       version_id TEXT NOT NULL,
       owner_key TEXT NOT NULL,
       step_id TEXT NOT NULL,
       attempt_no INTEGER NOT NULL,
       req_id TEXT,
       ticket_id TEXT,
       script_path TEXT,
       script_digest TEXT,
       exit_code INTEGER,
       status TEXT NOT NULL,          -- delegated|reported|verified|rejected|escalated|released
       reported_at TEXT,
       verified_at TEXT,
       started_at TEXT,
       ended_at TEXT,
       payload TEXT,
       UNIQUE(run_id, step_id, attempt_no))",

    "CREATE INDEX IF NOT EXISTS idx_attempt_status ON step_attempts(version_id, step_id, status)",
    "CREATE INDEX IF NOT EXISTS idx_attempt_req ON step_attempts(req_id)",

    # 环节快照：每步一份不可改的 manifest（A2）。
    "CREATE TABLE IF NOT EXISTS snapshots (
       snapshot_id TEXT PRIMARY KEY,
       attempt_id TEXT REFERENCES step_attempts(attempt_id),
       version_id TEXT NOT NULL,
       owner_key TEXT NOT NULL,
       created_at TEXT NOT NULL,
       sealed INTEGER NOT NULL DEFAULT 0,
       manifest_path TEXT,
       manifest_md5 TEXT,
       input_refs TEXT,
       params TEXT,
       env_lock TEXT,
       command TEXT,
       log_tail TEXT,
       qc_verdict TEXT,
       payload TEXT)",

    # 产物分类（A3）：identity|preview|small|large|ondemand|secret
    "CREATE TABLE IF NOT EXISTS artifacts (
       artifact_id TEXT PRIMARY KEY,
       snapshot_id TEXT REFERENCES snapshots(snapshot_id),
       version_id TEXT NOT NULL,
       owner_key TEXT NOT NULL,
       rel TEXT NOT NULL,
       bytes INTEGER,
       md5 TEXT,
       kind TEXT NOT NULL DEFAULT 'preview',
       mtime TEXT,
       source TEXT,
       UNIQUE(snapshot_id, rel))",

    "CREATE INDEX IF NOT EXISTS idx_artifacts_ver ON artifacts(version_id, rel)",

    # 事件账本：只追加。
    "CREATE TABLE IF NOT EXISTS events (
       event_id INTEGER PRIMARY KEY AUTOINCREMENT,
       at TEXT NOT NULL,
       type TEXT NOT NULL,
       owner_key TEXT,
       project_id TEXT,
       version_id TEXT,
       run_id TEXT,
       step_id TEXT,
       attempt_id TEXT,
       event_seq INTEGER,
       payload TEXT)",
    "CREATE INDEX IF NOT EXISTS idx_events_attempt ON events(attempt_id, event_seq)",
    "CREATE INDEX IF NOT EXISTS idx_events_type ON events(type, at)",

    # 判定结论（E2）。每份结论不可变，但同一个快照可以有多份结论。
    #   为什么不能加 UNIQUE(snapshot_id)（2026-11-18 实测）：
    #   重判同一环节时用的是同一个 snapshot_id，一加唯一约束，第二次判定
    #   直接 UNIQUE constraint failed，整份回写被判 verdict_failed，
    #   链路卡死在该环，重跑多少次都一样。不可变性靠「只插不改」保证。
    "CREATE TABLE IF NOT EXISTS reviews (
       review_id TEXT PRIMARY KEY,
       snapshot_id TEXT NOT NULL,
       attempt_id TEXT,
       version_id TEXT NOT NULL,
       step_id TEXT,
       owner_key TEXT,
       verdict TEXT NOT NULL,         -- pass|warn|fail|unknown|missing|covered
       rule_version TEXT,
       at TEXT NOT NULL,
       by_who TEXT NOT NULL,
       checks_json TEXT,
       payload TEXT)",

    # 返工单（J1）
    "CREATE TABLE IF NOT EXISTS rework_tickets (
       ticket_id TEXT PRIMARY KEY,
       attempt_id TEXT,
       run_id TEXT,
       version_id TEXT,
       step_id TEXT,
       owner_key TEXT,
       issued_at TEXT NOT NULL,
       due_at TEXT,
       attempt_no INTEGER NOT NULL DEFAULT 1,
       max_auto INTEGER NOT NULL DEFAULT 3,
       state TEXT NOT NULL DEFAULT 'open',   -- open|queued|sent|nudged|escalated|closed
       failed_checks TEXT,
       must_do TEXT,
       command_hint TEXT,
       payload TEXT,
       delivered_at TEXT,
       last_nudge_at TEXT,
       nudges INTEGER NOT NULL DEFAULT 0)",

    "CREATE INDEX IF NOT EXISTS idx_rework_state ON rework_tickets(state, due_at)",

    # 持久调度器（A4）：轮询、取回、返工共用一套租约。
    "CREATE TABLE IF NOT EXISTS poll_jobs (
       job_id TEXT PRIMARY KEY,
       kind TEXT NOT NULL DEFAULT 'poll',    -- poll|fetch|rework
       owner_key TEXT,
       run_id TEXT,
       version_id TEXT,
       step_id TEXT,
       attempt_id TEXT,
       ticket_id TEXT,
       query_request_id TEXT,
       payload TEXT,
       state TEXT NOT NULL DEFAULT 'pending',-- pending|leased|done|error|manual
       next_check_at TEXT NOT NULL,
       attempts INTEGER NOT NULL DEFAULT 0,
       lease_until TEXT,
       leased_by TEXT,
       last_error TEXT,
       created_at TEXT NOT NULL,
       updated_at TEXT NOT NULL)",

    "CREATE INDEX IF NOT EXISTS idx_jobs_due ON poll_jobs(state, next_check_at)",

    # 取回任务（C3）
    "CREATE TABLE IF NOT EXISTS fetch_jobs (
       request_id TEXT PRIMARY KEY,
       owner_key TEXT NOT NULL,
       version_id TEXT,
       rel TEXT NOT NULL,
       expected_md5 TEXT,
       expected_bytes INTEGER,
       received_bytes INTEGER NOT NULL DEFAULT 0,
       state TEXT NOT NULL DEFAULT 'pending', -- pending|fetching|verified|manual|failed
       retries INTEGER NOT NULL DEFAULT 0,
       max_retries INTEGER NOT NULL DEFAULT 3,
       local_path TEXT,
       last_error TEXT,
       created_at TEXT NOT NULL,
       updated_at TEXT NOT NULL)",

    # 分析记录（B1）：每条带 owner，读接口按 owner 过滤。
    "CREATE TABLE IF NOT EXISTS sessions (
       session_id TEXT PRIMARY KEY,
       owner_key TEXT NOT NULL,
       name TEXT,
       mode TEXT,
       run_dir TEXT,
       project_id TEXT,
       version_id TEXT,
       visibility TEXT NOT NULL DEFAULT 'private',
       created_at TEXT NOT NULL,
       updated_at TEXT,
       payload TEXT)",

    "CREATE INDEX IF NOT EXISTS idx_sessions_owner ON sessions(owner_key, updated_at)",

    # Persist observations, not polling ticks. No file paths are accepted by readers.
    "CREATE TABLE IF NOT EXISTS run_log_observations (
       observation_id INTEGER PRIMARY KEY AUTOINCREMENT,
       owner_key TEXT NOT NULL,
       session_id TEXT NOT NULL REFERENCES sessions(session_id),
       step_id TEXT NOT NULL, sub_id TEXT NOT NULL,
       req_id TEXT NOT NULL, status TEXT NOT NULL,
       provenance TEXT NOT NULL CHECK(provenance IN ('callback','history_snapshot','local_snapshot')),
       source TEXT NOT NULL, source_at TEXT NOT NULL,
       received_at TEXT NOT NULL, observed_at TEXT NOT NULL,
       content TEXT NOT NULL, fingerprint TEXT NOT NULL,
       UNIQUE(owner_key, session_id, fingerprint))",
    "CREATE INDEX IF NOT EXISTS idx_run_logs_session ON run_log_observations(owner_key, session_id, observation_id)",

    "CREATE TRIGGER IF NOT EXISTS run_logs_no_update BEFORE UPDATE ON run_log_observations BEGIN SELECT RAISE(ABORT, 'run logs are append only'); END",
    "CREATE TRIGGER IF NOT EXISTS run_logs_no_delete BEFORE DELETE ON run_log_observations BEGIN SELECT RAISE(ABORT, 'run logs are append only'); END",

    # 判断记录（G1）：怎么想的，与 review 分开。
    "CREATE TABLE IF NOT EXISTS decisions (
       decision_id TEXT PRIMARY KEY,
       owner_key TEXT,
       version_id TEXT,
       run_id TEXT,
       step_id TEXT,
       kind TEXT NOT NULL,
       basis TEXT,
       quote TEXT,
       outcome TEXT,
       at TEXT NOT NULL,
       payload TEXT)",

    # 指令投递队列（原 injections.json）
    "CREATE TABLE IF NOT EXISTS outbox (
       item_id TEXT PRIMARY KEY,
       owner_key TEXT,
       session_ref TEXT,
       step_id TEXT,
       req_id TEXT,
       ticket_id TEXT,
       text TEXT NOT NULL,
       reason TEXT,
       state TEXT NOT NULL DEFAULT 'pending',
       attempts INTEGER NOT NULL DEFAULT 0,
       next_attempt_at TEXT,
       delivered_at TEXT,
       satisfied_at TEXT,
       last_code TEXT,
       created_at TEXT NOT NULL,
       updated_at TEXT)",
    "CREATE INDEX IF NOT EXISTS idx_outbox_due ON outbox(state, next_attempt_at)",

    # 台账（K1 第 7 项）：只追加，行数减少或整行重写即 ledger_tamper。
    "CREATE TABLE IF NOT EXISTS ledger (
       entry_id INTEGER PRIMARY KEY AUTOINCREMENT,
       at TEXT NOT NULL,
       run_id TEXT,
       set_id TEXT,
       script_path TEXT,
       script_md5 TEXT,
       phase TEXT,                    -- start|finish
       payload TEXT,
       prev_hash TEXT,
       row_hash TEXT)"
  )

  for (s in stmts) DBI::dbExecute(con, s)

  # ---- 只追加保护：events / ledger ----
  #   为什么用触发器而不是只靠约定：约定拦不住下一个人写一行 UPDATE。
  for (tbl in c("events", "ledger")) {
    for (op in c("UPDATE", "DELETE")) {
      trg <- paste0("trg_", tbl, "_no_", tolower(op))
      DBI::dbExecute(con, sprintf(
        "CREATE TRIGGER IF NOT EXISTS %s BEFORE %s ON %s
         BEGIN SELECT RAISE(ABORT, '%s is append-only'); END", trg, op, tbl, tbl))
    }
  }

  # ---- 六层身份链一致性：attempt 的 version 必须等于 run 的 version ----
  DBI::dbExecute(con, "CREATE TRIGGER IF NOT EXISTS trg_attempt_version_guard
    BEFORE INSERT ON step_attempts
    WHEN (SELECT version_id FROM runs WHERE run_id = NEW.run_id) IS NOT NEW.version_id
    BEGIN SELECT RAISE(ABORT, 'attempt.version_id != run.version_id'); END")

  db_meta_set(con, "schema_version", DB_SCHEMA_VERSION)
  db_meta_set(con, "initialized_at",
              (db_meta_get(con, "initialized_at") %||% db_now()))
  invisible(con)
}

# ---- 元信息 ----------------------------------------------------------

db_meta_get <- function(con, key) {
  r <- DBI::dbGetQuery(con, "SELECT value FROM schema_meta WHERE key = ?", list(key))
  if (!nrow(r)) NULL else as.character(r$value[1])
}

db_meta_set <- function(con, key, value) {
  DBI::dbExecute(con, "INSERT INTO schema_meta(key,value) VALUES(?,?)
                       ON CONFLICT(key) DO UPDATE SET value=excluded.value",
                 list(key, as.character(value)))
  invisible(value)
}

# ---- 小工具 ----------------------------------------------------------

db_scalar <- function(con, sql, params = NULL) {
  r <- DBI::dbGetQuery(con, sql, params)
  if (!nrow(r)) NA else r[[1]][1]
}

db_rows <- function(con, sql, params = NULL) DBI::dbGetQuery(con, sql, params)

# ---- 事务：必须可重入 ------------------------------------------------
# 踩过的坑（2026-11-06）：回写 /api/step/import 一直 409，judge_error 是
#   "cannot start a transaction within a transaction"。
#   原因不是某处写错，而是 db_tx 只能有一层：内层 dbWithTransaction 在外层
#   还没结束时再发一次 BEGIN，SQLite 直接拒绝。回写路径上
#   db_api_report_step -> verdict_step -> rework_issue 每一层都各开一次事务，
#   于是每一份回写都在第二层炸掉，页面永远收不到完成。
#
# 解法不是让调用方自己记住"别嵌套"（下次照样会踩），而是让 db_tx 认层级：
#   外层用真正的 BEGIN/COMMIT；内层改用 SAVEPOINT，失败只回滚自己这一段。
#   SQLite 支持嵌套 SAVEPOINT，回滚粒度也正好是要的语义。
db_tx_depth <- new.env(parent = emptyenv())
db_tx_depth$n <- 0L

db_tx <- function(con, fn) {
  key <- paste0("tx_", as.integer(db_tx_depth$n) + 1L)
  if (db_tx_depth$n == 0L) {
    DBI::dbBegin(con)
    db_tx_depth$n <- 1L
    ok <- FALSE
    on.exit({
      if (!ok) try(DBI::dbRollback(con), silent = TRUE)
      db_tx_depth$n <- 0L
    }, add = TRUE)
    out <- fn()
    DBI::dbCommit(con)
    ok <- TRUE
    db_tx_depth$n <- 0L
    return(out)
  }
  # 内层：SAVEPOINT，只回滚自己
  outer <- db_tx_depth$n
  DBI::dbExecute(con, paste0("SAVEPOINT ", key))
  db_tx_depth$n <- outer + 1L
  ok <- FALSE
  on.exit({
    if (!ok) try(DBI::dbExecute(con, paste0("ROLLBACK TO ", key)), silent = TRUE)
    try(DBI::dbExecute(con, paste0("RELEASE ", key)), silent = TRUE)
    db_tx_depth$n <- outer
  }, add = TRUE)
  out <- fn()
  DBI::dbExecute(con, paste0("RELEASE ", key))
  ok <- TRUE
  db_tx_depth$n <- outer
  out
}

db_json <- function(x) {
  if (is.null(x)) return(NA_character_)
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA))
}

db_unjson <- function(x, default = NULL) {
  if (is.null(x) || length(x) == 0L || is.na(x) || !nzchar(as.character(x))) return(default)
  tryCatch(jsonlite::fromJSON(as.character(x), simplifyVector = FALSE),
           error = function(e) default)
}

# ---- 事件：只追加 ----------------------------------------------------

# 事件序号在同一 attempt 内单调递增。回写带 event_seq，序号小者不覆盖大者。
db_event <- function(con, type, ..., event_seq = NULL) {
  a <- list(...)
  pick <- function(k) {
    v <- a[[k]]
    if (is.null(v) || length(v) == 0L) NA_character_ else as.character(v)[1]
  }
  seqv <- if (is.null(event_seq)) {
    s <- db_scalar(con, "SELECT COALESCE(MAX(event_seq),0)+1 FROM events WHERE attempt_id IS ?",
                   list(pick("attempt_id")))
    if (is.na(s)) 1L else as.integer(s)
  } else as.integer(event_seq)

  DBI::dbExecute(con,
    "INSERT INTO events(at,type,owner_key,project_id,version_id,run_id,step_id,attempt_id,event_seq,payload)
     VALUES(?,?,?,?,?,?,?,?,?,?)",
    list(db_now(), as.character(type), pick("owner_key"), pick("project_id"),
         pick("version_id"), pick("run_id"), pick("step_id"), pick("attempt_id"),
         seqv, db_json(a$payload)))
  invisible(seqv)
}

# ---- 台账（K1 第 7 项）----------------------------------------------
#  为什么不能只建表和触发器：之前 ledger 表 0 行，等于保护一个空表。
#  这里补上唯一的写入入口，并带 prev_hash 形成链：
#  行数减少或整行重写都能被发现（触发器只能拦 UPDATE/DELETE，
#  拦不住"删表重建"或"恢复到旧备份"——那类要靠行数与 hash 链对账）。
db_ledger_append <- function(con, run_id, set_id, script_path = NULL,
                             script_md5 = NULL, phase = c("start", "finish"),
                             payload = NULL) {
  phase <- match.arg(phase)
  prev <- DBI::dbGetQuery(con, "SELECT row_hash FROM ledger ORDER BY entry_id DESC LIMIT 1")
  prev_hash <- if (nrow(prev)) as.character(prev$row_hash[1]) else ""
  at <- db_now()
  row_hash <- substr(digest::digest(paste(prev_hash, at, run_id, set_id,
                                          script_path, script_md5, phase),
                                    algo = "md5"), 1, 32)
  DBI::dbExecute(con,
    "INSERT INTO ledger(at,run_id,set_id,script_path,script_md5,phase,payload,prev_hash,row_hash)
     VALUES(?,?,?,?,?,?,?,?,?)",
    list(at, run_id %||% NA_character_, set_id %||% NA_character_,
         script_path %||% NA_character_, script_md5 %||% NA_character_,
         phase, db_json(payload), prev_hash, row_hash))
  invisible(row_hash)
}

# 台账对账：行数比记忆里的少、或 hash 链断开，都记 ledger_tamper。
db_ledger_check <- function(con) {
  n <- as.integer(db_scalar(con, "SELECT COUNT(*) FROM ledger"))
  expect <- suppressWarnings(as.integer(db_meta_get(con, "ledger_rows") %||% NA))
  rows <- DBI::dbGetQuery(con, "SELECT entry_id,prev_hash,row_hash FROM ledger ORDER BY entry_id")
  chain_ok <- TRUE
  if (nrow(rows) > 1L) {
    for (i in 2:nrow(rows)) {
      if (!identical(as.character(rows$prev_hash[i]), as.character(rows$row_hash[i - 1L]))) {
        chain_ok <- FALSE; break
      }
    }
  }
  shrink <- !is.na(expect) && n < expect
  if (shrink || !chain_ok) {
    db_event(con, "ledger_tamper", payload = list(
      rows = n, expected = expect, chain_ok = chain_ok,
      why = if (shrink) "行数比上次记录少" else "hash 链断开"))
  }
  db_meta_set(con, "ledger_rows", n)
  list(ok = !shrink && chain_ok, rows = n, expected = expect, chain_ok = chain_ok)
}

db_events <- function(con, attempt_id = NULL, version_id = NULL, limit = 200) {
  if (!is.null(attempt_id))
    return(db_rows(con, "SELECT * FROM events WHERE attempt_id = ? ORDER BY event_id DESC LIMIT ?",
                   list(attempt_id, as.integer(limit))))
  if (!is.null(version_id))
    return(db_rows(con, "SELECT * FROM events WHERE version_id = ? ORDER BY event_id DESC LIMIT ?",
                   list(version_id, as.integer(limit))))
  db_rows(con, "SELECT * FROM events ORDER BY event_id DESC LIMIT ?", list(as.integer(limit)))
}

# ---- owner（B1 / B4）------------------------------------------------

# owner_key 的取值顺序：viewer.id > topic_id > 本机匿名令牌。
#   为什么不用 IP：同一台机器上两个人也会撞成一个。
#   为什么匿名令牌要写 cookie：不写的话每次刷新都变成新用户，历史记录会碎一地。
owner_from_identity <- function(ident = NULL, anon_token = NULL) {
  if (is.list(ident) && isTRUE(ident$authenticated)) {
    v <- ident$viewer
    if (is.list(v)) {
      id <- v$id %||% v$user_id %||% v$email %||% NULL
      if (length(id) == 1L && !is.list(id) && !is.na(id) && nzchar(trimws(as.character(id))))
        return(list(key = paste0("viewer:", as.character(id)), kind = "viewer",
                    external_id = as.character(id)))
    }
    tid <- ident$topic_id
    if (!is.null(tid) && nzchar(as.character(tid)))
      return(list(key = paste0("topic:", as.character(tid)), kind = "topic",
                  external_id = as.character(tid)))
  }
  if (!is.null(anon_token) && nzchar(as.character(anon_token)))
    return(list(key = paste0("anonymous:", as.character(anon_token)), kind = "anonymous",
                external_id = as.character(anon_token)))
  # fail-closed：没有令牌就不给任何已有记录的视图。
  list(key = NA_character_, kind = "unidentified", external_id = NA_character_)
}

owner_touch <- function(con, owner) {
  if (is.null(owner) || is.na(owner$key %||% NA)) return(invisible(NULL))
  DBI::dbExecute(con, "INSERT INTO owners(owner_key,kind,external_id,created_at,last_seen_at)
                       VALUES(?,?,?,?,?)
                       ON CONFLICT(owner_key) DO UPDATE SET last_seen_at=excluded.last_seen_at",
                 list(owner$key, owner$kind, owner$external_id %||% NA_character_,
                      db_now(), db_now()))
  invisible(owner$key)
}

# 读接口的统一守卫：owner 为空一律 403，绝不回退到全局。
owner_require <- function(owner) {
  if (is.null(owner) || is.na(owner$key %||% NA) || !nzchar(owner$key %||% ""))
    stop(structure(class = c("owner_unidentified", "error", "condition"),
                   list(message = "身份未识别，不返回任何已有记录", call = NULL)))
  owner$key
}

# ---- 派生 ID ---------------------------------------------------------

db_new_id <- function(prefix) {
  paste0(prefix, "-", format(Sys.time(), "%Y%m%d%H%M%S"),
         "-", paste(sample(c(letters, 0:9), 4, replace = TRUE), collapse = ""))
}

# spec 摘要：版本冻结时算一次，用来回答「这个版本是按哪份流程定义跑的」。
db_spec_digest <- function(app_dir) {
  f <- file.path(app_dir, "engine", "flow_spec.R")
  if (!file.exists(f)) return(NA_character_)
  as.character(tools::md5sum(f))
}

# ---- 便捷：确保某 owner 下有一个 project / version / run -------------

db_ensure_project <- function(con, owner_key, name, project_id = NULL) {
  # project_id 必须含 owner_key。
  #   原来只用 name 做哈希：两个人各建一个都叫「脾脏分析」的项目，
  #   project_id 完全相同，INSERT OR IGNORE 静默丢掉第二个人的那一行，
  #   于是第二个人的版本挂到了第一个人的项目下（versions 表没有 owner 列）。
  #   这种错比报错更难查：页面一切正常，只是数据归错了人。
  #   这也正是 B1「身份要接进数据层」的核心 —— 身份不只是过滤读接口，
  #   主键生成里也必须带着它。
  pid <- project_id %||% paste0("p-", substr(digest::digest(
    paste(owner_key, "\u0001", name), algo = "md5"), 1, 10))
  DBI::dbExecute(con, "INSERT OR IGNORE INTO projects(project_id,name,owner_key,created_at)
                       VALUES(?,?,?,?)", list(pid, name, owner_key, db_now()))
  pid
}

db_ensure_version <- function(con, project_id, owner_key, version_id = NULL,
                              label = NULL, reason = NULL, parent = NULL) {
  if (!is.null(version_id) &&
      nzchar(as.character(db_scalar(con, "SELECT version_id FROM versions WHERE version_id=?",
                                    list(version_id)) %||% "")))
    return(version_id)
  # 版本号必须全局唯一，不能按项目内计数。
  #   原来写的是 COUNT(*)+1 WHERE project_id=? —— 于是每个项目第一版都叫 V1，
  #   而 version_id 是全局主键。第二个项目建版本时直接 UNIQUE 冲突：
  #   表现是「第二个人（或第二个分析记录）根本建不出版本」，
  #   却看不出和版本号有关。
  #   现在用 project 前缀 + 该项目内序号：p<短哈希>-V<n>，可读且全局唯一。
  vid <- version_id %||% {
    pfx <- substr(digest::digest(project_id, algo = "md5"), 1, 8)
    n <- as.integer(db_scalar(con, "SELECT COUNT(*) FROM versions WHERE project_id=?",
                              list(project_id))) + 1L
    paste0("p", pfx, "-V", n)
  }
  # NULL 不能直接进 DBI 参数位：它要求长度 1，NULL 长度 0 会报
  #   "Parameter N does not have length 1"。可空列一律写 NA_character_。
  DBI::dbExecute(con,
    "INSERT INTO versions(version_id,project_id,parent_version_id,label,frozen_at,
                          spec_digest,created_at,created_by,reason)
     VALUES(?,?,?,?,?,?,?,?,?)",
    list(vid, project_id, parent %||% NA_character_, label %||% vid, db_now(),
         as.character(db_meta_get(con, "spec_digest") %||% "unknown"),
         db_now(), owner_key, reason %||% NA_character_))
  vid
}

db_ensure_run <- function(con, version_id, owner_key, run_id = NULL,
                          work_root = NULL, host = NULL, pc = NULL) {
  # 同一版本已有 run 就复用，不要每次回写都新建一个。
  #   为什么这是硬性要求：attempt_no 是「同一 run 内同一步骤的第几次尝试」。
  #   每回写一次就新建 run，attempt_no 永远从 1 开始 ——
  #   于是返工轮次永远算作第 1 次，max_auto=3 的上限永远不生效，
  #   「这是自动返工第 2 次（上限 3 次）」这句话永远说不出口。
  #   这个 bug 的表现很隐蔽：库里确实有两行记录，看起来「历史都在」，
  #   但两行的 attempt_no 都是 1，没有任何一处能据此判断「催了几次」。
  if (is.null(run_id)) {
    ex <- db_rows(con, "SELECT run_id FROM runs WHERE version_id = ? AND owner_key = ?
                        ORDER BY started_at LIMIT 1", list(version_id, owner_key))
    if (nrow(ex)) return(as.character(ex$run_id[1]))
  }
  rid <- run_id %||% paste0("run-", substr(digest::digest(
    paste(version_id, db_now()), algo = "md5"), 1, 10))
  DBI::dbExecute(con,
    "INSERT OR IGNORE INTO runs(run_id,version_id,owner_key,work_root,exec_host,exec_pc,started_at)
     VALUES(?,?,?,?,?,?,?)",
    list(rid, version_id, owner_key, work_root %||% NA_character_,
         host %||% NA_character_, pc %||% NA_character_, db_now()))
  rid
}

db_next_attempt_no <- function(con, run_id, step_id) {
  n <- db_scalar(con, "SELECT COALESCE(MAX(attempt_no),0) FROM step_attempts
                       WHERE run_id=? AND step_id=?", list(run_id, step_id))
  as.integer(n) + 1L
}

db_latest_attempt <- function(con, version_id, step_id) {
  r <- db_rows(con, "SELECT * FROM step_attempts WHERE version_id=? AND step_id=?
                     ORDER BY attempt_no DESC LIMIT 1", list(version_id, step_id))
  if (!nrow(r)) NULL else as.list(r[1, ])
}

db_attempt_by_req <- function(con, req_id, step_id = NULL) {
  if (is.null(req_id) || !nzchar(req_id)) return(NULL)
  # 必须按环节限定（2026-11-18 修）：
  #   整条链共用一个请求号，一次链跑里 doublet / merge / markers … 的 req_id 全一样。
  #   原来只按 req_id 取第一条，第二环回写拿到的就是第一环那条已验收的 attempt ——
  #   它的 version_id 与本次不同，于是被判 stale_write「这份回写属于别的版本」，
  #   每往下一环就必然撞一次，整条链从第二环起永远过不去。
  #   链条自己也是按 (chain_id, step) 记账的，这里按同一口径限定。
  stp <- as.character(step_id %||% "")
  if (nzchar(stp)) {
    r <- db_rows(con, paste0("SELECT * FROM step_attempts WHERE req_id = ? AND step_id = ? ",
                             "ORDER BY attempt_no DESC LIMIT 1"), list(req_id, stp))
  } else {
    r <- db_rows(con, "SELECT * FROM step_attempts WHERE req_id = ? LIMIT 1", list(req_id))
  }
  if (!nrow(r)) NULL else as.list(r[1, ])
}

# ---- 页面投影（A8）：四轴状态 ---------------------------------------

# 页面不再从多个文件各取一个字段拼状态，而是读这里。
#   为什么必须一处算：以前 delegated 与 running 都被当结论用过，
#   因为它们分别来自 injections.json 和 remote_steps.json，
#   两份文件各自都会说一句「正在跑」，页面就把两句话当成了同一件事。
db_project_status <- function(con, version_id, owner_key = NULL) {
  if (is.null(version_id) || !nzchar(as.character(version_id)))
    stop("db_project_status: 必须给 version_id，不接受「最新」语义")
  att <- db_rows(con,
    "SELECT step_id, attempt_no, status, req_id, exit_code, ticket_id,
            reported_at, verified_at
       FROM step_attempts WHERE version_id = ? ORDER BY step_id, attempt_no",
    list(version_id))
  art <- db_rows(con,
    "SELECT rel, kind, bytes, md5 FROM artifacts WHERE version_id = ?", list(version_id))
  rv <- db_rows(con,
    "SELECT snapshot_id, step_id, verdict FROM reviews WHERE version_id = ?", list(version_id))

  steps <- unique(att$step_id)
  out <- lapply(steps, function(s) {
    a <- att[att$step_id == s, , drop = FALSE]
    last <- a[which.max(a$attempt_no), , drop = FALSE]
    sub <- if (identical(last$status, "reported")) "sent"
           else if (identical(last$status, "delegated")) "sent"
           else if (identical(last$status, "verified")) "confirmed"
           else if (identical(last$status, "rejected")) "failed"
           else if (identical(last$status, "escalated")) "unverified"
           else "pending"
    ex <- switch(as.character(last$status),
      delegated = "queued", reported = "running", verified = "done",
      rejected = "failed", escalated = "failed", released = "done", "queued")
    sy <- if (identical(last$status, "verified")) "verified"
          else if (identical(last$status, "reported")) "waiting_identity"
          else if (identical(last$status, "delegated")) "none"
          else "none"
    qv <- rv$verdict[rv$step_id == s]
    qc <- if (!length(qv)) "missing" else as.character(qv[1])
    list(step_id = s, attempt_no = as.integer(last$attempt_no),
         submit = sub, exec = ex, sync = sy, qc = qc,
         status = as.character(last$status), ticket_id = as.character(last$ticket_id %||% NA))
  })

  list(version_id = version_id,
       steps = out,
       artifacts = split(art, art$kind),
       deliverable = db_deliverable(con, version_id))
}

# ---- 交付门（A10 / L1 第 10 项）--------------------------------------

db_deliverable <- function(con, version_id) {
  if (is.null(version_id) || !nzchar(as.character(version_id)))
    return(list(ok = FALSE, code = "version_required", reasons = list("必须指定 version_id")))

  # 全部环节以 flow_spec 为准，不在别处抄一份阶段表（M3）。
  steps <- tryCatch(flow_run_order_engine(), error = function(e) character(0))
  reasons <- list(); unverified <- list()
  for (s in steps) {
    a <- db_rows(con, "SELECT status FROM step_attempts WHERE version_id=? AND step_id=?
                       ORDER BY attempt_no DESC LIMIT 1", list(version_id, s))
    rv <- db_rows(con, "SELECT verdict FROM reviews WHERE version_id=? AND step_id=?
                        ORDER BY at DESC LIMIT 1", list(version_id, s))
    st <- if (nrow(a)) as.character(a$status[1]) else "missing"
    qc <- if (nrow(rv)) as.character(rv$verdict[1]) else "missing"
    ok_qc <- qc %in% c("pass", "covered")
    ok_st <- st %in% c("verified", "released")
    if (!ok_st || !ok_qc) {
      item <- list(step_id = s, status = st, qc = qc)
      if (qc %in% c("unknown", "missing")) unverified[[length(unverified) + 1L]] <- item
      reasons[[length(reasons) + 1L]] <- item
    }
  }
  list(ok = length(reasons) == 0L,
       version_id = version_id,
       checked_steps = length(steps),
       reasons = reasons,
       unverified = unverified,
       rule = "每环 status 必须 verified/released，且 qc 为 pass/covered；unknown 单列为未核验项，不算通过")
}

# ---- 依赖顺序：从 flow_spec 取，不在 serve.R 里再写一份 --------------

flow_run_order_engine <- function(app_dir = getwd()) {
  # 允许在任意 cwd 下调用：先找 app 根。
  cand <- c(file.path(app_dir, "engine", "flow_spec.R"),
            file.path(dirname(app_dir), "engine", "flow_spec.R"),
            file.path(app_dir, "flow_spec.R"))
  f <- cand[file.exists(cand)][1]
  if (is.na(f) || is.null(f)) stop("找不到 flow_spec.R")
  e <- new.env(parent = baseenv())
  sys.source(f, envir = e)
  e$SERVER_RUN_ORDER
}

# Log authority is strictly owned records, including for shared-session viewers.
db_run_log_authorized <- function(con, owner_key, sid) {
  if (is.null(con) || length(owner_key) != 1L || is.na(owner_key) || !nzchar(owner_key) ||
      length(sid) != 1L || is.na(sid) || !nzchar(sid)) return(FALSE)
  as.integer(db_scalar(con, "SELECT COUNT(*) FROM sessions WHERE session_id=? AND owner_key=?",
                       list(sid, owner_key))) == 1L
}

db_run_log_append <- function(con, owner_key, sid, observation, identity = observation) {
  if (!db_run_log_authorized(con, owner_key, sid)) stop("run_log_not_owner")
  field <- function(k) as.character(observation[[k]] %||% "")
  provenance <- field("provenance")
  if (!provenance %in% c("callback", "history_snapshot", "local_snapshot")) stop("invalid_log_provenance")
  # JSON object key order is not event identity. Arrays keep their order.
  canonical <- function(x) {
    if (!is.list(x)) return(x)
    if (!is.null(names(x))) x <- x[order(names(x))]
    lapply(x, canonical)
  }
  fingerprint <- digest::digest(list(provenance, canonical(identity)), algo = "sha256")
  DBI::dbExecute(con, "INSERT OR IGNORE INTO run_log_observations
    (owner_key,session_id,step_id,sub_id,req_id,status,provenance,source,source_at,
     received_at,observed_at,content,fingerprint) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)",
    list(owner_key, sid, field("step_id"), field("sub_id"), field("req_id"), field("status"),
         provenance, field("source"), field("source_at"), field("received_at"), db_now(),
         field("content"), fingerprint))
  invisible(TRUE)
}

db_run_log_read <- function(con, owner_key, sid, limit = NULL) {
  if (!db_run_log_authorized(con, owner_key, sid)) stop("run_log_not_owner")
  db_run_log_read_record(con, owner_key, sid, limit)
}

# Caller must have resolved record capability first. Shared viewers need to read
# the record owner's append-only log without being treated as that owner for writes.
db_run_log_read_record <- function(con, owner_key, sid, limit = NULL) {
  sql <- "SELECT * FROM run_log_observations WHERE owner_key=? AND session_id=? ORDER BY observation_id"
  # The monitor displays a bounded tail; downloads read the complete persisted record.
  if (!is.null(limit)) sql <- paste0("SELECT * FROM (SELECT * FROM run_log_observations WHERE owner_key=? AND session_id=? ORDER BY observation_id DESC LIMIT ",
                                    as.integer(limit), ") ORDER BY observation_id")
  rows <- db_rows(con, sql, list(owner_key, sid))
  if (!nrow(rows)) return(list())
  lapply(seq_len(nrow(rows)), function(i) as.list(rows[i, , drop = FALSE]))
}

db_run_log_text <- function(rows) {
  labels <- c(callback = "执行端回写", history_snapshot = "历史回写快照（非实时 stdout）",
              local_snapshot = "本机日志完整内容快照（多次快照，非去重 stdout）")
  if (!length(rows)) return("没有已持久化的日志观察记录；不能据此判断任务是否启动。\n")
  paste0(paste(vapply(rows, function(x) paste0(
    "阶段：", x$step_id, if (nzchar(x$sub_id)) paste0(" / ", x$sub_id) else "",
    " · 状态：", x$status, " · req_id：", if (nzchar(x$req_id)) x$req_id else "未知",
    "\n来源：", labels[[x$provenance]], " / ", x$source,
    "\n来源时间：", if (nzchar(x$source_at)) x$source_at else "未知",
    " · 接收时间：", if (nzchar(x$received_at)) x$received_at else "未知",
    " · 入库时间：", x$observed_at,
    "\n", if (nzchar(x$content)) x$content else "本次未提供日志正文。"), ""), collapse = "\n\n"), "\n")
}
