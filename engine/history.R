#!/usr/bin/env Rscript
# =====================================================================
#  history.R -- run history, snapshots, rollback
#
#  WHY THIS FILE EXISTS
#    The ledger already records what ran. But a ledger alone cannot
#    answer two questions a user actually asks:
#      1. "what did I run last time, with which parameters?"
#         -> needs a snapshot of the config, not just the step names.
#      2. "I want to change a decision and run again from step 3."
#         -> needs a rollback marker that downstream steps can see.
#
#    Without those, the page can only move forward, and the only way
#    back is to delete the working directory. That is what this file
#    fixes.
#
#  THREE THINGS
#    hist_snapshot()   copy pipeline.json / gates.json / ledger tail
#                      into status/snapshots/<ts>_<label>/
#    hist_runs()       group the ledger by run_id so the page can list
#                      "each run: when, which steps, which script set"
#    hist_rollback()   restore a snapshot and mark downstream steps as
#                      stale, so they show up as "needs re-run"
#
#  ASCII-only source: R 4.1.2 on a Chinese Windows locale cannot parse
#  non-ASCII source. All user-facing wording lives in the web page.
# =====================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---------------------------------------------------------------------
# small helpers
# ---------------------------------------------------------------------
hist_read_json <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    simplifyVector = FALSE), error = function(e) NULL)
}

hist_write_json <- function(path, obj) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  con <- file(path, open = "wb")
  writeBin(charToRaw(enc2utf8(jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE,
                                              null = "null", digits = NA))), con)
  close(con)
  invisible(TRUE)
}

hist_status_dir <- function(run_dir) file.path(run_dir, "status")
hist_snap_dir   <- function(run_dir) file.path(hist_status_dir(run_dir), "snapshots")
hist_roll_file  <- function(run_dir) file.path(hist_status_dir(run_dir), "rollback.json")

# ---------------------------------------------------------------------
# ledger
# ---------------------------------------------------------------------
hist_ledger <- function(run_dir) {
  f <- file.path(hist_status_dir(run_dir), "ledger.jsonl")
  if (!file.exists(f)) return(list())
  ln <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"),
                 error = function(e) character(0))
  ln <- ln[nzchar(trimws(ln))]
  out <- list()
  for (l in ln) {
    e <- tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(e)) out[[length(out) + 1L]] <- e
  }
  out
}

# Group ledger entries by run_id.
#   Order matters: the page shows newest first, so we keep encounter
#   order here and reverse at the end rather than sorting by time
#   string (time strings can tie within the same second).
hist_runs <- function(run_dir, limit = 60L) {
  entries <- hist_ledger(run_dir)
  if (!length(entries)) return(list())

  ids <- character(0)
  groups <- list()
  for (e in entries) {
    rid <- as.character(e$run_id %||% "")
    if (!nzchar(rid)) rid <- "(unregistered)"
    if (is.null(groups[[rid]])) {
      groups[[rid]] <- list()
      ids <- c(ids, rid)
    }
    groups[[rid]][[length(groups[[rid]]) + 1L]] <- e
  }

  out <- list()
  for (rid in ids) {
    g <- groups[[rid]]
    ev <- vapply(g, function(x) as.character(x$event %||% ""), character(1))
    tm <- vapply(g, function(x) as.character(x$time %||% ""), character(1))

    steps <- character(0)
    for (x in g) {
      s <- as.character(x$step %||% "")
      if (nzchar(s) && !s %in% steps) steps <- c(steps, s)
    }

    codes <- vapply(g, function(x) {
      v <- x$code
      if (is.null(v) || !length(v)) NA_integer_ else suppressWarnings(as.integer(v))
    }, integer(1))
    ok <- !any(ev == "blocked") && !any(!is.na(codes) & codes != 0L)
    has_blocked <- any(ev == "blocked")

    # which script set / environment this run resolved to
    set_ids <- character(0)
    env_ids <- character(0)
    for (x in g) {
      sid <- as.character(x$set_id %||% "")
      eid <- as.character(x$env %||% "")
      if (nzchar(sid) && !identical(sid, "unknown") && !sid %in% set_ids) set_ids <- c(set_ids, sid)
      if (nzchar(eid) && !identical(eid, "unknown") && !eid %in% env_ids) env_ids <- c(env_ids, eid)
    }

    out[[length(out) + 1L]] <- list(
      run_id = rid,
      started = if (length(tm)) tm[1] else "",
      ended = if (length(tm)) tm[length(tm)] else "",
      steps = as.list(steps),
      n_events = length(g),
      has_blocked = has_blocked,
      ok = ok,
      set_ids = as.list(set_ids),
      env_ids = as.list(env_ids)
    )
  }

  # newest first
  out <- rev(out)
  if (length(out) > limit) out <- out[seq_len(limit)]
  out
}

# Per-step state: ledger + snapshot presence.
#   Adds "stale" --- a step that finished successfully at some point
#   but was later invalidated by a rollback to an earlier step.
hist_step_states <- function(run_dir, order = character(0)) {
  entries <- hist_ledger(run_dir)
  last <- list()
  for (e in entries) {
    s <- as.character(e$step %||% "")
    if (!nzchar(s)) next
    if (as.character(e$event %||% "") %in% c("start", "finish", "blocked", "rollback")) {
      last[[s]] <- e
    }
  }

  rb <- hist_read_json(hist_roll_file(run_dir))
  rb_step <- NULL
  if (!is.null(rb) && !is.null(rb$from_step)) {
    v <- as.character(rb$from_step)
    if (nzchar(v) && length(order) && v %in% order) rb_step <- v
  }
  rb_idx <- if (is.null(rb_step)) NA_integer_ else match(rb_step, order)

  res <- list()
  for (i in seq_along(order)) {
    sid <- order[i]
    e <- last[[sid]]
    state <- if (is.null(e)) {
      "not_run"
    } else if (identical(as.character(e$event), "blocked")) {
      "blocked"
    } else if (identical(as.character(e$event), "finish") &&
               identical(suppressWarnings(as.integer(e$code %||% 1L)), 0L)) {
      "done"
    } else if (identical(as.character(e$event), "start")) {
      "running"
    } else {
      "failed"
    }
    # rollback invalidates this step and everything after it
    invalidated <- !is.na(rb_idx) && i >= rb_idx && identical(state, "done")
    res[[sid]] <- list(
      step = sid,
      state = if (invalidated) "stale" else state,
      was = state,
      time = as.character(e$time %||% ""),
      code = if (is.null(e$code)) NULL else suppressWarnings(as.integer(e$code)),
      invalidated = invalidated
    )
  }
  res
}

# ---------------------------------------------------------------------
# snapshots
# ---------------------------------------------------------------------
hist_snapshot <- function(run_dir, label = "manual", kind = "manual", note = "") {
  d <- hist_snap_dir(run_dir)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  ts <- format(Sys.time(), "%Y%m%d-%H%M%S")
  lab <- gsub("[^A-Za-z0-9_-]", "_", as.character(label))
  if (!nzchar(lab)) lab <- "snap"
  id <- paste0(ts, "_", lab)
  target <- file.path(d, id)
  dir.create(target, showWarnings = FALSE, recursive = TRUE)

  copied <- character(0)
  for (nm in c("pipeline.json", "gates.json")) {
    src <- file.path(run_dir, nm)
    if (file.exists(src)) {
      if (isTRUE(file.copy(src, file.path(target, nm), overwrite = TRUE))) copied <- c(copied, nm)
    }
  }
  led <- file.path(hist_status_dir(run_dir), "ledger.jsonl")
  if (file.exists(led)) {
    # tail only: a full ledger can grow large, and a snapshot is for
    # restoring decisions, not for archiving the whole audit trail
    ln <- tryCatch(readLines(led, warn = FALSE, encoding = "UTF-8"),
                   error = function(e) character(0))
    ln <- utils::tail(ln, 2000L)
    con <- file(file.path(target, "ledger.jsonl"), open = "wb")
    writeBin(charToRaw(enc2utf8(paste0(paste(ln, collapse = "\n"), "\n"))), con)
    close(con)
    copied <- c(copied, "ledger.jsonl")
  }
  ss <- file.path(hist_status_dir(run_dir), "step_state.json")
  if (file.exists(ss)) {
    if (isTRUE(file.copy(ss, file.path(target, "step_state.json"), overwrite = TRUE)))
      copied <- c(copied, "step_state.json")
  }

  meta <- list(id = id, created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
               label = as.character(label), kind = as.character(kind),
               note = as.character(note), copied = as.list(copied),
               run_dir = run_dir)
  hist_write_json(file.path(target, "meta.json"), meta)
  list(ok = TRUE, id = id, path = target, meta = meta)
}

hist_snapshots <- function(run_dir) {
  d <- hist_snap_dir(run_dir)
  if (!dir.exists(d)) return(list())
  ids <- list.dirs(d, recursive = FALSE, full.names = FALSE)
  if (!length(ids)) return(list())
  out <- list()
  for (id in ids) {
    m <- hist_read_json(file.path(d, id, "meta.json"))
    if (is.null(m)) m <- list(id = id, created_at = "", label = id, kind = "unknown")
    m$has_pipeline <- file.exists(file.path(d, id, "pipeline.json"))
    m$has_gates <- file.exists(file.path(d, id, "gates.json"))
    out[[length(out) + 1L]] <- m
  }
  # newest first
  ord <- order(vapply(out, function(x) as.character(x$created_at %||% ""), character(1)),
               decreasing = TRUE)
  out[ord]
}

hist_rollback <- function(run_dir, snapshot_id, order = character(0), from_step = NULL) {
  d <- hist_snap_dir(run_dir)
  sid <- gsub("[^A-Za-z0-9_-]", "", as.character(snapshot_id))
  if (!nzchar(sid)) return(list(ok = FALSE, error = "bad snapshot id"))
  src <- file.path(d, sid)
  # path traversal guard: the resolved path must stay under the snap dir
  if (!dir.exists(src) ||
      !startsWith(normalizePath(src, winslash = "/", mustWork = FALSE),
                  normalizePath(d, winslash = "/", mustWork = FALSE))) {
    return(list(ok = FALSE, error = "snapshot not found"))
  }

  # snapshot the current state first, so a rollback is itself undoable
  hist_snapshot(run_dir, label = paste0("before-rollback-", sid),
                kind = "auto", note = "state captured automatically before rollback")

  restored <- character(0)
  for (nm in c("pipeline.json", "gates.json")) {
    f <- file.path(src, nm)
    if (file.exists(f)) {
      if (isTRUE(file.copy(f, file.path(run_dir, nm), overwrite = TRUE))) restored <- c(restored, nm)
    }
  }

  mk <- list(at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
             from_snapshot = sid,
             from_step = if (is.null(from_step)) NULL else as.character(from_step),
             restored = as.list(restored))
  hist_write_json(hist_roll_file(run_dir), mk)

  # append to the ledger: a rollback is part of the history, not a
  # silent edit of it
  led <- file.path(hist_status_dir(run_dir), "ledger.jsonl")
  dir.create(dirname(led), showWarnings = FALSE, recursive = TRUE)
  entry <- list(run_id = paste0("rollback-", format(Sys.time(), "%Y%m%d%H%M%S")),
                event = "rollback", step = if (is.null(from_step)) "(all)" else as.character(from_step),
                time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                reason = paste0("restored from snapshot ", sid))
  con <- file(led, open = "ab")
  writeBin(charToRaw(paste0(enc2utf8(jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null")), "\n")), con)
  close(con)

  list(ok = TRUE, snapshot = sid, restored = as.list(restored), mark = mk)
}

# Mark "re-run from this step onward".
#   Does NOT delete outputs. Deleting is irreversible and the user may
#   only want to reconsider a decision; re-running overwrites anyway.
hist_mark_rerun <- function(run_dir, from_step, order = character(0)) {
  fs <- as.character(from_step)
  if (!length(order) || !fs %in% order) return(list(ok = FALSE, error = "unknown step"))
  idx <- match(fs, order)
  mk <- list(at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
             from_step = fs,
             will_rerun = as.list(order[idx:length(order)]),
             restored = list())
  hist_write_json(hist_roll_file(run_dir), mk)
  led <- file.path(hist_status_dir(run_dir), "ledger.jsonl")
  dir.create(dirname(led), showWarnings = FALSE, recursive = TRUE)
  entry <- list(run_id = paste0("rerun-", format(Sys.time(), "%Y%m%d%H%M%S")),
                event = "rollback", step = fs,
                time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                reason = paste0("will re-run from ", fs))
  con <- file(led, open = "ab")
  writeBin(charToRaw(paste0(enc2utf8(jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null")), "\n")), con)
  close(con)
  list(ok = TRUE, mark = mk)
}

# ---------------------------------------------------------------------
# recently used working directories
#   The page previously remembered exactly one run directory and gave
#   no way back to the welcome screen. Keeping a small list means the
#   user can switch back and forth instead of being stuck on the last
#   path they picked.
# ---------------------------------------------------------------------
hist_recent_file <- function(app_dir) file.path(app_dir, "state", "recent_dirs.json")

hist_recent_dirs <- function(app_dir) {
  f <- hist_recent_file(app_dir)
  j <- hist_read_json(f)
  if (is.null(j) || is.null(j$dirs)) return(list())
  keep <- Filter(function(x) {
    p <- as.character(x$path %||% "")
    nzchar(p) && dir.exists(p)
  }, j$dirs)
  keep
}

hist_recent_add <- function(app_dir, run_dir, label = NULL) {
  f <- hist_recent_file(app_dir)
  d <- normalizePath(as.character(run_dir), winslash = "/", mustWork = FALSE)
  cur <- hist_recent_dirs(app_dir)
  cur <- Filter(function(x) !identical(as.character(x$path), d), cur)
  cur <- c(list(list(path = d, label = as.character(label %||% basename(d)),
                     at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))), cur)
  if (length(cur) > 20L) cur <- cur[seq_len(20L)]
  hist_write_json(f, list(updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), dirs = cur))
  invisible(TRUE)
}
