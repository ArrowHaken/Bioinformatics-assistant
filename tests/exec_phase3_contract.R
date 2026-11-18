#!/usr/bin/env Rscript
# Stage 3 contract: worker identity, heartbeat propagation, and independent reap.
# Temporary app tree/state/token file only; no production DB, queue or worker.
run_contract <- function() {
  root <- tempfile("exec-phase3-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  stopifnot(file.symlink(normalizePath("engine"), file.path(root, "engine")))
  stopifnot(file.symlink(normalizePath("public"), file.path(root, "public")))
  stopifnot(file.symlink(normalizePath("serve.R"), file.path(root, "serve.R")))
  dir.create(file.path(root, "state"), recursive = TRUE)
  dir.create(file.path(root, "config"), recursive = TRUE)

  old_opts <- options(pa.serve.definitions_only = TRUE, pa.app_dir = root)
  e <- new.env(parent = globalenv())
  sys.source(file.path(root, "serve.R"), envir = e)
  options(old_opts)

  e$send_json <- function(con, code, body) list(code = as.integer(code), body = body)
  e$DB_READY <- TRUE
  e$DB_CON <- e$db_connect(root)
  e$db_init(e$DB_CON)
  on.exit({
    if (!is.null(e$DB_CON) && DBI::dbIsValid(e$DB_CON)) DBI::dbDisconnect(e$DB_CON)
  }, add = TRUE)

  fail <- character()
  check <- function(label, ok) {
    cat(if (isTRUE(ok)) "PASS:" else "FAIL:", label, "\n")
    if (!isTRUE(ok)) fail <<- c(fail, label)
  }
  call <- function(path, body = NULL, token = NULL) {
    e$serve_request(NULL, list(
      target = path,
      method = if (is.null(body)) "GET" else "POST",
      headers = if (is.null(token)) list() else list("x-worker-token" = token),
      body = if (is.null(body)) "" else as.character(jsonlite::toJSON(body, auto_unbox = TRUE))))
  }
  write_json <- function(path, obj) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(as.character(jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null", digits = NA)), path)
  }
  ts <- function(t) format(t, "%Y-%m-%dT%H:%M:%S")
  queue <- function(items) write_json(file.path(root, "state", "exec_tasks.json"), list(items = items))
  hb <- function(worker_id, pool_id, at) write_json(
    file.path(root, "state", "exec_heartbeats", paste0(worker_id, ".json")),
    list(worker_id = worker_id, pool_id = pool_id, at = ts(at), version = "fixture"))
  run_reaper <- function() {
    out <- system2("Rscript", c("engine/exec_reaper.R", root), stdout = TRUE, stderr = TRUE)
    status <- attr(out, "status") %||% 0L
    stopifnot(identical(as.integer(status), 0L))
    out
  }
  items <- function() jsonlite::fromJSON(file.path(root, "state", "exec_tasks.json"),
                                         simplifyVector = FALSE)$items %||% list()
  audit_lines <- function() {
    f <- file.path(root, "state", "exec_reaper_audit.jsonl")
    if (!file.exists(f)) return(list())
    lapply(readLines(f, warn = FALSE), function(x) jsonlite::fromJSON(x, simplifyVector = FALSE))
  }

  tok_a <- e$worker_token_issue(root, "w-a", "pool-a", note = "phase3 fixture")$token
  tok_b <- e$worker_token_issue(root, "w-b", "pool-a")$token
  stopifnot(nzchar(tok_a), nzchar(tok_b))

  r <- call("/api/exec/heartbeat", list(by = "w-a"))
  check("anonymous heartbeat is rejected", r$code == 403L &&
          identical(r$body$code, "worker_token_required"))
  r <- call("/api/exec/heartbeat", list(worker_id = "w-b"), token = tok_a)
  check("worker A cannot forge worker B heartbeat", r$code == 403L &&
          identical(r$body$code, "worker_identity_mismatch"))
  r <- call("/api/exec/heartbeat", list(worker_id = "w-a", pool_id = "pool-a"), token = tok_a)
  check("authenticated heartbeat writes only its own worker file", r$code == 200L &&
          identical(r$body$worker_id, "w-a") &&
          file.exists(file.path(root, "state", "exec_heartbeats", "w-a.json")))
  check("legacy global heartbeat is not written by the authenticated route",
        !file.exists(file.path(root, "state", "exec_heartbeat.json")))

  tok_c <- e$worker_token_issue(root, "w-c", "pool-a")$token
  cid <- e$worker_token_check(root, tok_c)$token_id
  check("revoking a worker token succeeds", isTRUE(e$worker_token_revoke(root, cid)$ok))
  r <- call("/api/exec/heartbeat", list(worker_id = "w-c", pool_id = "pool-a"), token = tok_c)
  check("revoked worker token is rejected",
        r$code == 403L && identical(r$body$code, "worker_token_revoked"))

  tok_e <- e$worker_token_issue(root, "w-e", "pool-a")$token
  tok_obj <- e$worker_tokens_read(root)
  for (i in seq_along(tok_obj$tokens)) {
    if (identical(as.character(tok_obj$tokens[[i]]$worker_id %||% ""), "w-e"))
      tok_obj$tokens[[i]]$expires_at <- ts(Sys.time() - 60)
  }
  e$worker_tokens_write(root, tok_obj)
  r <- call("/api/exec/heartbeat", list(worker_id = "w-e", pool_id = "pool-a"), token = tok_e)
  check("expired worker token is rejected",
        r$code == 403L && identical(r$body$code, "worker_token_expired"))

  queue(list(list(id = "k1", step = "doublet", session_id = "s1", req_id = "r1",
                  pool_id = "pool-a", status = "queued", created_at = ts(Sys.time()))))
  r <- call("/api/exec/claim", list(id = "k1", by = "w-a", worker_id = "w-a", pool_id = "pool-a"), tok_a)
  q1 <- items()[[1]]
  check("claim binds worker_id/pool_id to the task", r$code == 200L &&
          identical(q1$status, "running") && identical(q1$worker_id, "w-a") &&
          identical(q1$claimed_by, "w-a") && identical(q1$pool_id, "pool-a"))
  Sys.sleep(1.1)
  hbf <- file.path(root, "state", "exec_heartbeats", "w-a.json")
  hb_before <- jsonlite::fromJSON(hbf, simplifyVector = FALSE)$at
  r <- call("/api/exec/keepalive", list(id = "k1", worker_id = "w-a", pool_id = "pool-a",
                                        job = "name-only", job_number = "60778"), tok_a)
  q2 <- items()[[1]]
  hb_after <- jsonlite::fromJSON(hbf, simplifyVector = FALSE)$at
  check("keepalive advances task last_seen and worker heartbeat together", r$code == 200L &&
          identical(q2$status, "running") && q2$last_seen > q1$last_seen &&
          hb_after > hb_before && identical(q2$job_number, "60778"))
  r <- call("/api/exec/keepalive", list(id = "k1", worker_id = "w-b", pool_id = "pool-a"), tok_b)
  check("another authenticated worker cannot keepalive a claimed task", r$code == 403L &&
          identical(r$body$code, "exec_task_not_owned"))

  # Independent reaper: online worker means queued never expires, even at 25h.
  now <- Sys.time()
  hb("w-live", "p-live", now)
  queue(list(
    list(id = "q3h", step = "x", session_id = "s", status = "queued",
         pool_id = "p-live", created_at = ts(now - 3 * 3600)),
    list(id = "q25h", step = "x", session_id = "s", status = "queued",
         pool_id = "p-live", created_at = ts(now - 25 * 3600))))
  run_reaper(); run_reaper()
  qq <- items()
  check("fresh heartbeat keeps 3h queued and has no 24h absolute deadline",
        all(vapply(qq, function(x) identical(x$status, "queued"), TRUE)))

  # Stale heartbeat at 31m -> failed with the exact offline reason.
  hb("w-stale", "p-stale", now - 31 * 60)
  queue(list(list(id = "stale31", step = "x", session_id = "s", status = "queued",
                  pool_id = "p-stale", created_at = ts(now - 3 * 3600))))
  run_reaper()
  q1 <- items()[[1]]
  a1 <- Filter(function(x) identical(x$id, "stale31"), audit_lines())
  check("31m stale worker reaps queued as failed with 执行端已离线",
        identical(q1$status, "failed") && grepl("执行端已离线", q1$error, fixed = TRUE) &&
          length(a1) == 1L && identical(a1[[1]]$worker_id, "w-stale") &&
          nzchar(a1[[1]]$heartbeat_at))

  # 29m59s stays queued; >=30m fails.
  hb("w-2999", "p-2999", now - (30 * 60 - 10))
  queue(list(list(id = "q2999", step = "x", session_id = "s", status = "queued",
                  pool_id = "p-2999", created_at = ts(now - 3 * 3600))))
  run_reaper()
  check("offline under 30m queued task remains queued", identical(items()[[1]]$status, "queued"))
  hb("w-3000", "p-3000", now - 30 * 60)
  queue(list(list(id = "q3000", step = "x", session_id = "s", status = "queued",
                  pool_id = "p-3000", created_at = ts(now - 3 * 3600))))
  run_reaper()
  check("30m offline queued task becomes failed", identical(items()[[1]]$status, "failed"))

  # Running has its own 2h last_seen rule; 1h is not enough.
  hb("w-run", "p-run", now)
  queue(list(list(id = "run1h", step = "x", session_id = "s", status = "running",
                  pool_id = "p-run", worker_id = "w-run",
                  last_seen = ts(now - 3600), created_at = ts(now - 6 * 3600))))
  run_reaper()
  check("running with last_seen 1h stays running", identical(items()[[1]]$status, "running"))
  queue(list(list(id = "run2h1", step = "x", session_id = "s", status = "running",
                  pool_id = "p-run", worker_id = "w-run",
                  last_seen = ts(now - 7201), created_at = ts(now - 6 * 3600))))
  run_reaper()
  check("running with last_seen 2h01 becomes failed", identical(items()[[1]]$status, "failed") &&
          grepl("执行端失联", items()[[1]]$error, fixed = TRUE))

  # Never-online pool: only created_at may be used, and the reason must say so.
  queue(list(
    list(id = "never29", step = "x", session_id = "s", status = "queued",
         pool_id = "p-never", created_at = ts(now - 29 * 60)),
    list(id = "never31", step = "x", session_id = "s", status = "queued",
         pool_id = "p-never", created_at = ts(now - 31 * 60))))
  run_reaper()
  qq <- setNames(items(), vapply(items(), function(x) x$id, ""))
  check("never-online uses created_at: 29m queued, 31m failed",
        identical(qq$never29$status, "queued") && identical(qq$never31$status, "failed") &&
          grepl("从未观测到", qq$never31$error, fixed = TRUE))

  # Timestamp priority is diagnostic: last_seen -> claimed_at -> created_at.
  hb("w-prio", "p-prio", now - 31 * 60)
  queue(list(list(id = "prio", step = "x", session_id = "s", status = "queued",
                  pool_id = "p-prio", worker_id = "w-prio",
                  last_seen = ts(now - 1000), claimed_at = ts(now - 500),
                  created_at = ts(now - 100))))
  run_reaper()
  qp <- items()[[1]]
  check("reap diagnostics use last_seen before claimed_at/created_at",
        identical(qp$status, "failed") && identical(qp$reap_ts_source, "last_seen") &&
          identical(qp$reap_ts, ts(now - 1000)))

  # The independent script, not /api/exec/due or /api/exec, produced the audit.
  scans <- Filter(function(x) identical(x$kind, "scan"), audit_lines())
  check("standalone reaper wrote scan audit without any HTTP request", length(scans) >= 6L)

  # A live lock holder makes the independent runner exit without scanning.
  before_lock <- length(Filter(function(x) identical(x$kind, "scan"), audit_lines()))
  lock_dir <- file.path(root, "state", "exec_reaper.lock")
  dir.create(lock_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(as.character(Sys.getpid()), file.path(lock_dir, "pid"))
  out_lock <- run_reaper()
  after_lock <- length(Filter(function(x) identical(x$kind, "scan"), audit_lines()))
  unlink(lock_dir, recursive = TRUE, force = TRUE)
  check("independent reaper file lock prevents concurrent scans",
        any(grepl("another instance is running", out_lock, fixed = TRUE)) &&
          identical(before_lock, after_lock))

  if (length(fail)) stop(paste(length(fail), "phase3 failures"), call. = FALSE)
  cat("PASS: exec phase3 contract\n")
}
run_contract()
