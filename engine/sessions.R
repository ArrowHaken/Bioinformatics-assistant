#!/usr/bin/env Rscript
# =====================================================================
#  sessions.R -- 多份分析记录：新建 / 历史 / 当前
#
#  WHY THIS FILE EXISTS
#    Before this, the page kept "which run am I looking at" in the
#    server's state.json and in the browser's memory only. One refresh
#    and everything the user had done -- the identify result, the
#    answers, which step they were on -- was gone from the screen, even
#    though the files were still on disk. The user's wording was
#    "刷新一下所有记录就都没有了".
#
#    So: an analysis is a first-class record, not a transient state.
#    Each record has an id, a name, when it was created and used, where
#    it runs, and its own saved answers and identify result. The page
#    opens the last-used record and stays there across refreshes until
#    the user starts a new one or picks another.
#
#  WHAT IS STORED WHERE
#    state/sessions.json                  the list + which one is current
#    state/sessions/<id>/profile.json     place + identify + saved notes
#    state/sessions/<id>/archives/<ts>/   named archives for rollback
#
#  WHAT IS *NOT* STORED
#    The password. It stays in config/server_profile.json, on this
#    machine, exactly as before. A session records where it runs, not
#    the credential used to get there.
#
#  HONESTY RULE
#    Nothing here invents progress. A session's identify result is only
#    ever what an execution端 actually wrote back; a step is only ever
#    marked done when something reported it done. Empty means empty.
# =====================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---- paths -----------------------------------------------------------

sess_root        <- function(app_dir) {
  # 走统一开关（B2）：分区立起来了就用分区，否则回退 state/。
  #   回退分支必须留着：分区是一次迁移，不是重写，旧数据原件没删。
  if (exists("state_private_root", mode = "function")) return(state_private_root(app_dir))
  file.path(app_dir, "state")
}
sess_file        <- function(app_dir) file.path(sess_root(app_dir), "sessions.json")
sess_dir_all     <- function(app_dir) file.path(sess_root(app_dir), "sessions")
sess_dir         <- function(app_dir, id) file.path(sess_dir_all(app_dir), id)
sess_profile_f   <- function(app_dir, id) file.path(sess_dir(app_dir, id), "profile.json")
sess_archive_dir <- function(app_dir, id) file.path(sess_dir(app_dir, id), "archives")

# ---- tiny json helpers (same shape as elsewhere in the app) ----------

sess_read_json <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
                                    collapse = "\n"), simplifyVector = FALSE),
           error = function(e) NULL)
}

sess_write_json <- function(path, obj) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  con <- file(path, open = "wb")
  writeBin(charToRaw(enc2utf8(jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE,
                                              null = "null", digits = NA))), con)
  close(con)
  invisible(path)
}

sess_chr <- function(x, d = "") {
  if (is.null(x) || !length(x)) return(d)
  v <- as.character(x)[1]
  if (is.na(v)) return(d)
  v
}

# ---- list + current ---------------------------------------------------

sess_read <- function(app_dir) {
  obj <- sess_read_json(sess_file(app_dir))
  if (is.null(obj) || !is.list(obj)) obj <- list()
  if (is.null(obj$sessions) || !is.list(obj$sessions)) obj$sessions <- list()
  if (is.null(obj$current)) obj$current <- NULL
  obj
}

sess_write <- function(app_dir, obj) sess_write_json(sess_file(app_dir), obj)

sess_find <- function(app_dir, id) {
  st <- sess_read(app_dir)
  if (is.null(id) || !nzchar(as.character(id))) return(NULL)
  for (s in st$sessions) if (identical(sess_chr(s$id), as.character(id))) return(s)
  NULL
}

sess_current_id <- function(app_dir) sess_chr(sess_read(app_dir)$current)

sess_current <- function(app_dir) {
  id <- sess_current_id(app_dir)
  if (!nzchar(id)) return(NULL)
  sess_find(app_dir, id)
}

# Name a new record the way a person would read it: count + date.
#   "分析 3 · 2026-10-21"
sess_default_name <- function(app_dir, when = Sys.time()) {
  st <- sess_read(app_dir)
  n <- length(st$sessions) + 1L
  sprintf("分析 %d · %s", n, format(when, "%Y-%m-%d"))
}

sess_new_id <- function(when = Sys.time()) {
  sprintf("s-%s-%s", format(when, "%Y%m%d%H%M%S"),
          paste(sample(c(letters, 0:9), 4L), collapse = ""))
}

# Start a new record. The caller may pass the place block to seed it --
#   a new record still needs to know where it runs, otherwise the page
#   would open blank and the user would have to retype the server.
sess_create <- function(app_dir, name = NULL, place = NULL, note = "", activate = TRUE) {
  now <- Sys.time()
  id <- sess_new_id(now)
  s <- list(
    id = id,
    name = if (nzchar(sess_chr(name))) sess_chr(name) else sess_default_name(app_dir, now),
    created_at = format(now, "%Y-%m-%dT%H:%M:%S"),
    updated_at = format(now, "%Y-%m-%dT%H:%M:%S"),
    note = sess_chr(note),
    mode = sess_chr(place$mode, "server"),
    run_dir = sess_chr(place$server$work_root %||% place$local$path %||% ""),
    place = place,
    identify = NULL,
    steps = list(),
    archives = list()
  )
  st <- sess_read(app_dir)
  st$sessions[[length(st$sessions) + 1L]] <- s
  if (isTRUE(activate)) st$current <- id
  sess_write(app_dir, st)
  dir.create(sess_dir(app_dir, id), showWarnings = FALSE, recursive = TRUE)
  s
}

# Shallow-merge a patch into a record. Nested blocks replace wholesale --
#   these blocks are written by their owner (place / identify), so a
#   half-merge would mix two sources and be harder to trust than a
#   straight replace.
sess_update <- function(app_dir, id, patch = list()) {
  st <- sess_read(app_dir)
  hit <- FALSE
  for (i in seq_along(st$sessions)) {
    if (!identical(sess_chr(st$sessions[[i]]$id), as.character(id))) next
    for (nm in names(patch)) {
      if (is.null(nm) || !nzchar(nm)) next
      if (is.null(patch[[nm]])) next
      st$sessions[[i]][[nm]] <- patch[[nm]]
    }
    st$sessions[[i]]$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    hit <- TRUE
  }
  if (!hit) return(list(ok = FALSE, error = "没有这条分析记录"))
  sess_write(app_dir, st)
  list(ok = TRUE, session = sess_find(app_dir, id))
}

sess_switch <- function(app_dir, id) {
  if (is.null(sess_find(app_dir, id))) return(list(ok = FALSE, error = "没有这条分析记录"))
  st <- sess_read(app_dir)
  st$current <- as.character(id)
  sess_write(app_dir, st)
  list(ok = TRUE, session = sess_find(app_dir, id))
}

# The list the page shows under "历史分析".
#   Newest first, with just enough per row to choose between them.
sess_list <- function(app_dir) {
  st <- sess_read(app_dir)
  out <- list()
  for (s in st$sessions) {
    id <- sess_chr(s$id)
    ident <- s$identify
    out[[length(out) + 1L]] <- list(
      id = id,
      name = sess_chr(s$name, id),
      created_at = sess_chr(s$created_at),
      updated_at = sess_chr(s$updated_at),
      note = sess_chr(s$note),
      mode = sess_chr(s$mode, "local"),
      run_dir = sess_chr(s$run_dir),
      has_identify = !is.null(ident) && !is.null(ident$result),
      verdict = sess_chr(ident$result$verdict),
      verdict_cn = sess_chr(ident$result$verdict_cn),
      identify_at = sess_chr(ident$at),
      n_steps_done = length(s$steps),
      n_archives = length(s$archives),
      current = identical(id, sess_chr(st$current))
    )
  }
  if (length(out)) out <- rev(out)
  list(ok = TRUE, current = sess_chr(st$current), sessions = out)
}

# ---- profile (what the page needs to restore itself) -----------------

sess_profile <- function(app_dir, id) {
  p <- sess_read_json(sess_profile_f(app_dir, id))
  if (is.null(p) || !is.list(p)) p <- list()
  p
}

sess_profile_write <- function(app_dir, id, prof) {
  sess_write_json(sess_profile_f(app_dir, id), prof)
  invisible(TRUE)
}

# Everything the page needs to redraw itself after a refresh, for the
#   current record (or a named one). Deliberately assembled from the
#   record + its profile, so there is one answer to "where was I".
sess_state <- function(app_dir, id = NULL) {
  if (is.null(id) || !nzchar(as.character(id))) id <- sess_current_id(app_dir)
  if (!nzchar(id)) return(list(ok = TRUE, session = NULL, sessions = sess_list(app_dir)$sessions))
  s <- sess_find(app_dir, id)
  if (is.null(s)) return(list(ok = TRUE, session = NULL, sessions = sess_list(app_dir)$sessions))
  prof <- sess_profile(app_dir, id)
  list(ok = TRUE,
       session = s,
       profile = prof,
       sessions = sess_list(app_dir)$sessions,
       current = id)
}

# ---- identify result, kept with the record ---------------------------
#   Why a copy lives here as well as in state/remote_env.json:
#     remote_env.json answers "what was the latest identify", which is
#     fine for a single-track page but wrong once several records exist --
#     opening an old record would show the newest record's numbers.
#     The record's copy answers "what did *this* one find".
sess_set_identify <- function(app_dir, id, result, req_id = NULL, extra = list()) {
  if (is.null(id) || !nzchar(as.character(id))) return(list(ok = FALSE, error = "没有当前分析记录"))
  s <- sess_find(app_dir, id)
  if (is.null(s)) return(list(ok = FALSE, error = "没有这条分析记录"))
  block <- c(list(result = result,
                  req_id = if (is.null(req_id)) NULL else as.character(req_id),
                  at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
             extra)
  saved <- sess_update(app_dir, id, list(identify = block))
  if (!isTRUE(saved$ok)) return(saved)
  actual <- sess_find(app_dir, id)
  if (is.null(actual) || !isTRUE(all.equal(actual$identify, block, check.attributes = FALSE)))
    return(list(ok = FALSE, error = "分析记录写入后读回不一致"))
  list(ok = TRUE, identify = block)
}

# ---- step results, kept with the record ------------------------------
#   Why per-record: in server mode the steps are executed by the assistant on
#     the server, and the write-back is the only evidence that anything ran.
#     If that evidence lived in one global file, switching to another record
#     would show this record's progress under that record's name.
sess_set_step <- function(app_dir, id, step_id, rec) {
  if (is.null(id) || !nzchar(as.character(id))) return(list(ok = FALSE, error = "没有当前分析记录"))
  if (is.null(step_id) || !nzchar(as.character(step_id))) return(list(ok = FALSE, error = "缺少步骤 id"))
  s <- sess_find(app_dir, id)
  if (is.null(s)) return(list(ok = FALSE, error = "没有这条分析记录"))
  steps <- s$steps
  if (is.null(steps) || !is.list(steps)) steps <- list()
  steps[[as.character(step_id)]] <- rec
  saved <- sess_update(app_dir, id, list(steps = steps))
  if (!isTRUE(saved$ok)) return(saved)
  actual <- sess_find(app_dir, id)
  if (is.null(actual) || !isTRUE(all.equal(actual$steps, steps, check.attributes = FALSE)))
    return(list(ok = FALSE, error = "分析记录写入后读回不一致"))
  list(ok = TRUE, steps = steps)
}

sess_get_identify <- function(app_dir, id = NULL) {
  if (is.null(id) || !nzchar(as.character(id))) id <- sess_current_id(app_dir)
  if (!nzchar(id)) return(NULL)
  sess_find(app_dir, id)$identify
}

# ---- pending identify request (per record) ---------------------------
#   Why per-record rather than one global "last commit":
#     each record can have its own identify in flight. One global marker
#     would make switching records look like "the request came back".
sess_set_pending <- function(app_dir, id, req_id) {
  if (is.null(id) || !nzchar(as.character(id))) return(list(ok = FALSE))
  attempt_migrate(app_dir, id)
  if (!is.null(req_id) && !attempt_register(app_dir, id, as.character(req_id)))
    return(list(ok = FALSE, error = "attempt registration failed"))
  prof <- sess_profile(app_dir, id)
  if (!is.null(req_id)) sess_write_json(file.path(sess_dir(app_dir,id), "attempt-head.json"), list(req_id=as.character(req_id)))
  prof$pending_req <- if (is.null(req_id)) NULL else as.character(req_id)
  prof$pending_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  sess_profile_write(app_dir, id, prof)
  list(ok = TRUE, pending_req = prof$pending_req)
}

sess_get_pending <- function(app_dir, id = NULL) {
  if (is.null(id) || !nzchar(as.character(id))) id <- sess_current_id(app_dir)
  if (!nzchar(id)) return(NULL)
  prof <- sess_profile(app_dir, id)
  prof$pending_req
}

sess_clear_pending <- function(app_dir, id) {
  prof <- sess_profile(app_dir, id)
  prof$pending_req <- NULL
  prof$pending_at <- NULL
  sess_profile_write(app_dir, id, prof)
  invisible(TRUE)
}

# ---- steps ledger (per record, mirrored from the execution side) -----

sess_mark_step <- function(app_dir, id, step, status, note = "") {
  if (is.null(id) || !nzchar(as.character(id))) return(list(ok = FALSE))
  s <- sess_find(app_dir, id)
  if (is.null(s)) return(list(ok = FALSE, error = "没有这条分析记录"))
  steps <- s$steps
  if (is.null(steps) || !is.list(steps)) steps <- list()
  steps[[as.character(step)]] <- list(
    step = as.character(step),
    status = as.character(status),
    note = sess_chr(note),
    at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
  sess_update(app_dir, id, list(steps = steps))
  list(ok = TRUE, steps = steps)
}

# ---- archives: named snapshots for "存档与步骤回滚" ------------------

sess_archive <- function(app_dir, id, label = "manual", note = "") {
  if (is.null(id) || !nzchar(as.character(id))) return(list(ok = FALSE, error = "没有当前分析记录"))
  s <- sess_find(app_dir, id)
  if (is.null(s)) return(list(ok = FALSE, error = "没有这条分析记录"))
  ts <- format(Sys.time(), "%Y%m%d-%H%M%S")
  lab <- gsub("[^A-Za-z0-9_-]", "_", sess_chr(label, "snap"))
  if (!nzchar(lab)) lab <- "snap"
  fold <- paste0(ts, "_", lab)
  d <- file.path(sess_archive_dir(app_dir, id), fold)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

  # copy the record itself + its profile; that is the whole of "what was
  # decided", because anything the execution side owns stays on the
  # server and is not ours to copy.
  sess_write_json(file.path(d, "session.json"), s)
  sess_write_json(file.path(d, "profile.json"), sess_profile(app_dir, id))

  arch <- s$archives
  if (is.null(arch) || !is.list(arch)) arch <- list()
  entry <- list(id = fold, label = sess_chr(label, "manual"), note = sess_chr(note),
                created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                n_steps = length(s$steps), dir = d)
  arch[[length(arch) + 1L]] <- entry
  sess_update(app_dir, id, list(archives = arch))
  list(ok = TRUE, archive = entry)
}

sess_archive_list <- function(app_dir, id = NULL) {
  if (is.null(id) || !nzchar(as.character(id))) id <- sess_current_id(app_dir)
  if (!nzchar(id)) return(list())
  s <- sess_find(app_dir, id)
  if (is.null(s)) return(list())
  arch <- s$archives
  if (is.null(arch) || !is.list(arch)) return(list())
  rev(arch)
}

# Restore = put the archived session + profile back as the record's own
#   state. The current state is archived first, so this is undoable in
#   the same way the user expects: nothing is destroyed to go back.
sess_archive_restore <- function(app_dir, id, archive_id) {
  if (is.null(id) || !nzchar(as.character(id))) return(list(ok = FALSE, error = "没有当前分析记录"))
  s <- sess_find(app_dir, id)
  if (is.null(s)) return(list(ok = FALSE, error = "没有这条分析记录"))
  arch <- sess_archive_list(app_dir, id)
  hit <- NULL
  for (a in arch) if (identical(sess_chr(a$id), as.character(archive_id))) hit <- a
  if (is.null(hit)) return(list(ok = FALSE, error = "没有这个存档"))
  snap <- sess_read_json(file.path(sess_chr(hit$dir), "session.json"))
  prof <- sess_read_json(file.path(sess_chr(hit$dir), "profile.json"))
  if (is.null(snap)) return(list(ok = FALSE, error = "这个存档读不出来"))

  # keep what we are about to overwrite
  before <- sess_archive(app_dir, id, label = "before-restore",
                         note = paste0("退回 ", archive_id, " 之前的自动存档"))
  if (!isTRUE(before$ok)) return(before)

  patch <- list(steps = snap$steps, identify = snap$identify,
                mode = snap$mode, run_dir = snap$run_dir, place = snap$place)
  r <- sess_update(app_dir, id, patch)
  if (!is.null(prof)) sess_profile_write(app_dir, id, prof)
  list(ok = TRUE, restored = archive_id, kept = before$archive$id, session = r$session)
}

# Keep attempt facts outside profile snapshots and archive restore.
source(file.path(dirname(sys.frame(1)$ofile %||% 'engine/sessions.R'), 'attempts.R'), local = FALSE)

source(file.path(dirname(sys.frame(1)$ofile %||% "engine/sessions.R"), "instruction_handoff.R"), local = FALSE)
