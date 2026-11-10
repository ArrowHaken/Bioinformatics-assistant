# Durable lifecycle only: caller authorizes owner/session and supplies its root.
# RDS snapshots use same-directory rename on a local filesystem. No scheduler,
# shell, PID inference, stale-lock deletion or owner discovery lives here.
# Request IDs are scoped to one run; replay compares exact serialized arguments.
# now is excluded from identity. Replays return the ORIGINAL response (possibly
# stale), never mutate current state; use rl_read for the latest snapshot.
# Evidence records a nonempty file and MD5 at completion, not scientific validity.
# Local-filesystem rename gives atomic visibility, not power-loss fsync durability.
# State machine: queued -> running -> succeeded|failed|cancelled; an authorized
# running worker may hand off to a NEW queued attempt. Terminal runs never reopen.
# Feature checkpoint: read existing boundaries first; one coupled implementation
# and test stream; per-run locks protect shared invariants; single owner holds diff.
# Git/PR and nested delegation skipped by explicit task scope.

rl_error <- function(code, message = code) {
  stop(structure(list(message = message, call = NULL, code = code),
                 class = c("run_lifecycle_error", "error", "condition")))
}

rl_id <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_-]{0,127}$", x))
    rl_error("invalid_id", paste("Invalid", name))
  x
}

rl_time <- function(now) {
  n <- as.numeric(now)
  if (length(n) != 1L || !is.finite(n)) rl_error("invalid_time")
  n
}

rl_paths <- function(root, run_id) {
  rl_id(run_id, "run_id")
  if (!is.character(root) || length(root) != 1L || is.na(root) ||
      !dir.exists(root)) rl_error("invalid_root", "Caller must supply an existing authorized session root")
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  list(root = root, state = file.path(root, paste0("lifecycle-", run_id, ".rds")),
       lock = file.path(root, paste0(".lifecycle-", run_id, ".lock")))
}

rl_snapshot <- function(path) {
  if (isTRUE(nzchar(Sys.readlink(path), keepNA = TRUE))) rl_error("unsafe_path")
  if (!file.exists(path)) return(NULL)
  s <- tryCatch(readRDS(path), error = function(e) rl_error("corrupt_state", conditionMessage(e)))
  if (!is.list(s) || !identical(s$schema, 1L) ||
      !s$status %in% c("queued", "running", "succeeded", "failed", "cancelled"))
    rl_error("corrupt_state")
  s
}

rl_public <- function(s) {
  if (is.null(s)) return(NULL)
  s$requests <- NULL
  s
}

rl_commit <- function(path, state) {
  tmp <- tempfile(pattern = ".lifecycle-write-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(state, tmp, version = 2)
  if (isTRUE(nzchar(Sys.readlink(path), keepNA = TRUE))) rl_error("unsafe_path")
  if (!file.rename(tmp, path)) rl_error("commit_failed")
}

rl_mutate <- function(root, run_id, request_id, operation, arguments, change) {
  p <- rl_paths(root, run_id)
  rl_id(request_id, "request_id")
  if (!dir.create(p$lock, showWarnings = FALSE)) rl_error("locked", "Run lock exists; it will not be cleared automatically")
  # Only the directory created by this invocation is released. No lock stealing.
  token <- tempfile(pattern = paste0(Sys.getpid(), "-"))
  marker <- file.path(p$lock, "holder")
  writeLines(token, marker)
  on.exit({
    owned <- tryCatch(identical(readLines(marker, warn = FALSE), token), error = function(e) FALSE)
    if (owned) unlink(p$lock, recursive = TRUE)
  }, add = TRUE)
  s <- rl_snapshot(p$state)
  signature <- serialize(list(operation = operation, arguments = arguments), NULL, version = 2)
  old <- if (is.null(s)) NULL else s$requests[[request_id]]
  if (!is.null(old)) {
    if (!identical(old$signature, signature)) rl_error("request_conflict")
    return(old$result)
  }
  n <- change(s)
  if (!is.null(s) && n$updated_at < s$updated_at) rl_error("clock_regression")
  n$seq <- if (is.null(s)) 1 else s$seq + 1
  result <- rl_public(n)
  n$requests[[request_id]] <- list(signature = signature, result = result)
  rl_commit(p$state, n)
  result
}

rl_require_worker <- function(s, attempt_id, worker_id) {
  rl_id(attempt_id, "attempt_id"); rl_id(worker_id, "worker_id")
  if (is.null(s)) rl_error("not_found")
  if (!identical(s$attempt_id, attempt_id) || !identical(s$worker_id, worker_id))
    rl_error("not_holder")
  if (!identical(s$status, "running")) rl_error("not_running")
}

rl_create <- function(root, run_id, attempt_id, request_id, payload = list(), now = Sys.time()) {
  rl_id(attempt_id, "attempt_id")
  t <- rl_time(now)
  rl_mutate(root, run_id, request_id, "create", list(attempt_id, payload), function(s) {
    if (!is.null(s)) rl_error("already_exists")
    list(schema = 1L, run_id = run_id, attempt_id = attempt_id, worker_id = NULL,
         status = "queued", payload = payload, created_at = t, updated_at = t,
         heartbeat_at = NULL, started_at = NULL, ended_at = NULL,
         evidence = list(), reason = NULL, attempts = attempt_id, requests = list())
  })
}

rl_claim <- function(root, run_id, attempt_id, worker_id, request_id, now = Sys.time()) {
  rl_id(attempt_id, "attempt_id"); rl_id(worker_id, "worker_id")
  t <- rl_time(now)
  rl_mutate(root, run_id, request_id, "claim", list(attempt_id, worker_id), function(s) {
    if (is.null(s)) rl_error("not_found")
    if (!identical(s$status, "queued")) rl_error("not_queued")
    if (!identical(s$attempt_id, attempt_id)) rl_error("not_holder")
    s$status <- "running"; s$worker_id <- worker_id
    s$started_at <- t; s$updated_at <- t; s$heartbeat_at <- t
    s
  })
}

rl_heartbeat <- function(root, run_id, attempt_id, worker_id, request_id, now = Sys.time()) {
  t <- rl_time(now)
  rl_mutate(root, run_id, request_id, "heartbeat", list(attempt_id, worker_id), function(s) {
    rl_require_worker(s, attempt_id, worker_id)
    s$updated_at <- t; s$heartbeat_at <- t
    s
  })
}

# Handoff requires the current holder's credentials. Dead-worker recovery is a
# caller policy, not an automatic heartbeat timeout that can create two workers.
rl_handoff <- function(root, run_id, attempt_id, worker_id, request_id,
                       next_attempt_id, now = Sys.time()) {
  rl_id(next_attempt_id, "next_attempt_id")
  t <- rl_time(now)
  rl_mutate(root, run_id, request_id, "handoff", list(attempt_id, worker_id, next_attempt_id), function(s) {
    rl_require_worker(s, attempt_id, worker_id)
    if (next_attempt_id %in% s$attempts) rl_error("attempt_reused")
    s$attempt_id <- next_attempt_id; s$attempts <- c(s$attempts, next_attempt_id)
    s$status <- "queued"; s$worker_id <- NULL
    s$updated_at <- t; s$heartbeat_at <- NULL; s$started_at <- NULL
    s
  })
}

rl_evidence <- function(root, evidence) {
  if (!is.character(evidence) || !length(evidence) || anyNA(evidence)) rl_error("evidence_required")
  base <- normalizePath(root, winslash = "/", mustWork = TRUE)
  lapply(evidence, function(path) {
    if (!file.exists(path)) rl_error("evidence_missing")
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    if (!startsWith(path, paste0(base, "/"))) rl_error("evidence_outside_root")
    info <- file.info(path)
    if (is.na(info$isdir) || info$isdir || is.na(info$size) || info$size <= 0)
      rl_error("evidence_empty")
    hash <- unname(tools::md5sum(path))
    if (is.na(hash)) rl_error("evidence_unreadable")
    list(path = path, bytes = unname(info$size), md5 = hash)
  })
}

rl_finish <- function(root, run_id, attempt_id, worker_id, request_id,
                      status, evidence = character(), reason = NULL, now = Sys.time()) {
  if (!is.character(status) || length(status) != 1L || is.na(status) ||
      !status %in% c("succeeded", "failed", "cancelled")) rl_error("invalid_terminal")
  if (!is.null(reason) && (!is.character(reason) || length(reason) != 1L || is.na(reason)))
    rl_error("invalid_reason")
  t <- rl_time(now)
  rl_mutate(root, run_id, request_id, "finish", list(attempt_id, worker_id, status, evidence, reason), function(s) {
    rl_require_worker(s, attempt_id, worker_id)
    proof <- if (identical(status, "succeeded")) rl_evidence(root, evidence) else list()
    s$status <- status; s$updated_at <- t; s$ended_at <- t
    s$evidence <- proof; s$reason <- reason
    s
  })
}

# Durable status is retained separately from observed liveness. A stale or
# unparseable heartbeat is never evidence of success. Reads never mutate state.
rl_read <- function(root, run_id, now = Sys.time(), stale_after = 90) {
  t <- rl_time(now)
  if (!is.numeric(stale_after) || length(stale_after) != 1L ||
      !is.finite(stale_after) || stale_after < 0) rl_error("invalid_threshold")
  s <- rl_public(rl_snapshot(rl_paths(root, run_id)$state))
  if (is.null(s)) return(NULL)
  s$observed_status <- s$status
  s$heartbeat_age <- NA_real_
  if (identical(s$status, "running")) {
    age <- t - s$heartbeat_at
    if (length(age) != 1L || !is.finite(age) || age < 0) s$observed_status <- "unknown"
    else {
      s$heartbeat_age <- age
      if (age > stale_after) s$observed_status <- "stalled"
    }
  }
  s
}
