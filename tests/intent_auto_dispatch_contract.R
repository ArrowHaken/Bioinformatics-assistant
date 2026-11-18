#!/usr/bin/env Rscript
# Stage 4 contract: intent_import stays byte-compatible without auto_dispatch,
# and auto_dispatch runs only after a successful db_tx commit. Temporary DB only.
suppressPackageStartupMessages(library(jsonlite))

extract_route <- function(lines, start_pat, end_pat) {
  a <- grep(start_pat, lines)
  b <- grep(end_pat, lines)
  stopifnot(length(a) == 1L, length(b) == 1L, b > a)
  lines[a:(b - 1L)]
}

legacy_route_env <- function(root, serve_path) {
  `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
  e <- new.env(parent = globalenv())
  for (f in c("engine/sessions.R", "engine/attempts.R", "engine/db.R",
              "engine/intent_bridge.R")) sys.source(f, envir = e)
  fixed <- as.POSIXct("2026-11-16 12:00:00", tz = "Asia/Shanghai")
  e$Sys.time <- function() fixed
  e$sess_new_id <- function(...) "s-fixture-byte"
  e$APP_DIR <- root
  e$DB_READY <- TRUE
  e$DB_CON <- e$db_connect(root)
  e$db_init(e$DB_CON)
  e$HAS_INTENT <- TRUE
  e$HAS_SESSIONS <- TRUE
  e$HAS_AGENT_GATE <- FALSE
  e$p <- "/api/intent/import"
  e$req <- list(method = "POST")
  e$con <- NULL
  e$sess_write(root, list(current = "untouched-selection", sessions = list(), extra = "keep"))
  lines <- readLines(serve_path, warn = FALSE)
  block <- extract_route(lines, '^  if \\(p == "/api/intent/import"',
                         '^  if \\(p == "/api/sessions/new"')
  e$route <- eval(parse(text = c("function() {", block, "}")), envir = e)
  e$send_json <- function(con, status, body) list(status = as.integer(status), body = body)
  e
}

snapshot_legacy <- function(e) {
  paths <- sort(list.files(e$APP_DIR, recursive = TRUE, all.files = TRUE, full.names = TRUE))
  paths <- paths[!grepl("/app[.]db(-wal|-shm)?$", paths)]
  bytes <- lapply(paths, function(p) readBin(p, "raw", n = file.info(p)$size))
  names(bytes) <- substring(paths, nchar(e$APP_DIR) + 2L)
  rows <- DBI::dbGetQuery(e$DB_CON, "SELECT * FROM sessions ORDER BY session_id")
  list(files = bytes, rows = rows,
       current = e$sess_current_id(e$APP_DIR),
       queue = if (file.exists(file.path(e$APP_DIR, "state", "exec_tasks.json")))
         readBin(file.path(e$APP_DIR, "state", "exec_tasks.json"), "raw",
                 n = file.info(file.path(e$APP_DIR, "state", "exec_tasks.json"))$size) else raw())
}

run_compat <- function() {
  raw_body <- list(project_name = "compat fixture", mode = "local",
    paths = list(work_root = "/isolated/compat"), species = "human", tissue = "kidney",
    source = list(topic_id = "topic-compat", message_id = "message-compat"))
  run_one <- function(serve_path) {
    root <- tempfile("intent-compat-"); dir.create(root)
    e <- legacy_route_env(root, serve_path)
    e$owner_ctx <- list(kind = "agent", key = "viewer:compat", external_id = "fixture-token")
    e$body_json <- function() raw_body
    before <- snapshot_legacy(e)
    out <- e$route()
    list(root = root, env = e, out = out, before = before, after = snapshot_legacy(e))
  }
  old <- run_one(".bak-20261116-parallel2/serve.R")
  new <- run_one("serve.R")
  on.exit({
    for (x in list(old, new)) {
      if (DBI::dbIsValid(x$env$DB_CON)) DBI::dbDisconnect(x$env$DB_CON)
      unlink(x$root, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)
  json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE,
                                                    null = "null", digits = NA))
  identical_response <- identical(json(old$out), json(new$out))
  identical_after <- identical(old$after, new$after)
  identical_current <- identical(old$after$current, "untouched-selection") &&
    identical(new$after$current, "untouched-selection")
  queue_empty <- length(old$after$queue) == 0L && length(new$after$queue) == 0L
  cat(if (identical_response && identical_after && identical_current && queue_empty)
      "PASS:" else "FAIL:",
      "intent_import without auto_dispatch is byte-identical and side-effect-identical\n")
  if (!(identical_response && identical_after && identical_current && queue_empty)) {
    print(list(old = old$out, new = new$out,
               response_equal = identical_response, after_equal = identical_after,
               old_after = old$after, new_after = new$after))
    stop("legacy intent_import compatibility failed", call. = FALSE)
  }
}

run_integration <- function() {
  root <- tempfile("intent-auto-"); dir.create(root)
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
  sys.source("engine/attempts.R", envir = e)
  e$send_json <- function(con, code, body) list(code = as.integer(code), body = body)
  e$DB_READY <- TRUE
  e$DB_CON <- e$db_connect(root)
  e$db_init(e$DB_CON)
  on.exit(if (DBI::dbIsValid(e$DB_CON)) DBI::dbDisconnect(e$DB_CON), add = TRUE)

  fail <- character()
  check <- function(label, ok) {
    cat(if (isTRUE(ok)) "PASS:" else "FAIL:", label, "\n")
    if (!isTRUE(ok)) fail <<- c(fail, label)
  }
  queue_items <- function() {
    f <- file.path(root, "state", "exec_tasks.json")
    if (!file.exists(f)) return(list())
    jsonlite::fromJSON(f, simplifyVector = FALSE)$items %||% list()
  }
  snap <- function() {
    files <- sort(list.files(root, recursive = TRUE, all.files = TRUE, full.names = TRUE))
    files <- files[!grepl("/app[.]db(-wal|-shm)?$", files)]
    files <- files[!grepl("/config/agent_tokens[.]json$", files)]
    bytes <- lapply(files, function(p) readBin(p, "raw", n = file.info(p)$size))
    names(bytes) <- substring(files, nchar(root) + 2L)
    list(files = bytes, rows = DBI::dbGetQuery(e$DB_CON,
      "SELECT * FROM sessions ORDER BY session_id"), current = e$sess_current_id(root))
  }
  call <- function(body, credential = token) e$serve_request(NULL, list(
    target = "/api/intent/import", method = "POST",
    headers = list("x-agent-token" = credential),
    body = as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"))))
  intent_body <- function(project, topic) list(project_name = project, mode = "server",
    server = list(host = "fixture-host", user = "fixture-user"),
    paths = list(data_root = "/fixture/data", work_root = paste0("/fixture/", project)),
    species = "human", tissue = "kidney",
    source = list(topic_id = topic, message_id = paste0(topic, "-message")))
  reset_pass <- function() {
    e$HAS_CONTRACT <- FALSE; e$HAS_DECISIONS <- FALSE; e$HAS_REFERENCE <- FALSE
    e$HAS_STAGE_REVIEWER <- FALSE; e$HAS_QUALITY <- FALSE
    e$HAS_LEDGER_OUT <- FALSE; e$HAS_NOTEBOOK <- FALSE
    e$intent_execution_gate <- function(...) list(ok = TRUE)
    e$flow_stage <- function(id, profile = NULL) list(id = id, title = id, skill = "fixture")
    e$flow_profile <- function(...) "server"
    e$flow_execution <- function(st, V) list(ok = TRUE, script = "/fixture/step.R",
                                              argv = list("fixture"))
    e$flow_vars <- function(...) list(work_root = "/fixture/run", run_dir = "/fixture/run")
    e$flow_run_order <- function(...) c("annotate")
    e$STEP_TASK <- list(annotate = list(title = "Annotate fixture"))
    e$remote_step_instruction <- function(...) "INSTRUCTION"
    e$remote_step_put <- function(...) TRUE
    e$remote_step_observe_text <- function(...) "OBSERVE"
    e$inject_enqueue <- function(...) list(ok = TRUE)
    e$db_api_version_of <- function(...) list(version_id = "")
    e$db_api_gate <- function(...) NULL
    e$dec_gate_block <- function(...) NULL
    e$supplement_needs_reference <- function(...) FALSE
  }

  owner <- "viewer:intent-auto-owner"
  token <- e$agent_token_issue(root, owner)$token
  local_dir <- file.path(root, "current-local"); dir.create(local_dir)
  current <- e$sess_create(root, "current", list(mode = "local",
    local = list(path = local_dir), server = list()), activate = TRUE)
  DBI::dbExecute(e$DB_CON,
    "INSERT INTO sessions(session_id,owner_key,name,mode,run_dir,visibility,version_id,created_at,updated_at) VALUES(?,?,?,?,?,?,?,?,?)",
    list(current$id, owner, current$name, current$mode, current$run_dir, "private", "",
         current$created_at, current$updated_at))
  reset_pass()

  before <- snap()
  body_from_to <- intent_body("removed-from-to", "topic-from-to")
  body_from_to$from <- "doublet"; body_from_to$to <- "annotate"
  r <- call(body_from_to)
  check("from/to are rejected before record creation", r$code == 400L &&
          identical(r$body$code, "from_to_removed") && identical(before, snap()))
  body_from_to$from <- NULL; body_from_to$to <- NULL
  body_from_to$auto_dispatch <- list(step = "annotate", req_id = "req-nested-from", from = "x")
  r <- call(body_from_to)
  check("from/to nested inside auto_dispatch are rejected before record creation",
        r$code == 400L && identical(r$body$code, "from_to_removed") && identical(before, snap()))

  body_ok <- intent_body("auto-success", "topic-auto-success")
  body_ok$auto_dispatch <- list(step = "annotate", req_id = "req-auto-success")
  r <- call(body_ok)
  sid_ok <- r$body$session_id %||% ""
  sessions_ok <- e$sess_read(root)$sessions
  queue_after_ok <- queue_items()
  check("auto_dispatch creates the record, queues one task and keeps current",
        r$code == 200L && isTRUE(r$body$record_created) && isTRUE(r$body$queued) &&
          nzchar(r$body$exec_id) && identical(e$sess_current_id(root), current$id) &&
          any(vapply(sessions_ok, function(s) identical(s$id, sid_ok), TRUE)) &&
          nrow(DBI::dbGetQuery(e$DB_CON,
            "SELECT * FROM sessions WHERE session_id=? AND owner_key=?", list(sid_ok, owner))) == 1L &&
          length(queue_after_ok) == 1L &&
          identical(queue_after_ok[[1]]$session_id, sid_ok) &&
          identical(queue_after_ok[[1]]$step, "annotate") &&
          identical(queue_after_ok[[1]]$req_id, "req-auto-success"))
  runs <- e$serve_request(NULL, list(target = "/api/runs", method = "GET",
    headers = list("x-agent-token" = token), body = ""))
  run_ids <- vapply(runs$body$items %||% list(), function(x) as.character(x$session_id %||% ""), "")
  check("the existing runs view can see the new record while current stays put",
        runs$code == 200L && sid_ok %in% run_ids &&
          identical(e$sess_current_id(root), current$id))

  body_fail <- intent_body("auto-compensate", "topic-auto-compensate")
  body_fail$auto_dispatch <- list(step = "annotate", req_id = "req-auto-compensate")
  original_enq <- e$exec_enqueue
  e$exec_enqueue <- function(...) stop("fixture enqueue write failure")
  q_before <- length(queue_items())
  r <- call(body_fail)
  sid_retry <- r$body$session_id %||% ""
  check("enqueue failure keeps the committed record and returns retryable 503",
        r$code == 503L && identical(r$body$code, "dispatch_retryable") &&
          isTRUE(r$body$retryable) && isTRUE(r$body$record_created) &&
          nzchar(sid_retry) && length(queue_items()) == q_before &&
          nrow(DBI::dbGetQuery(e$DB_CON,
            "SELECT * FROM sessions WHERE session_id=?", list(sid_retry))) == 1L &&
          any(vapply(e$sess_read(root)$sessions, function(s) identical(s$id, sid_retry), TRUE)) &&
          !dir.exists(file.path(root, "state", "sessions", sid_retry, "attempts",
                                "req-auto-compensate")) &&
          !identical(e$sess_get_pending(root, sid_retry, slot = "step"), "req-auto-compensate"))
  e$exec_enqueue <- original_enq
  r <- call(body_fail)
  check("retrying the same req_id fills only the queue, not a second record",
        r$code == 200L && isTRUE(r$body$duplicate) && isTRUE(r$body$queued) &&
          identical(r$body$session_id, sid_retry) &&
          length(queue_items()) == q_before + 1L &&
          identical(queue_items()[[length(queue_items())]]$req_id, "req-auto-compensate") &&
          nrow(DBI::dbGetQuery(e$DB_CON,
            "SELECT * FROM sessions WHERE session_id=?", list(sid_retry))) == 1L)

  reset_pass(); e$HAS_REFERENCE <- TRUE
  e$supplement_needs_reference <- function(step) identical(step, "annotate")
  body_gate <- intent_body("auto-gate", "topic-auto-gate")
  body_gate$auto_dispatch <- list(step = "annotate", req_id = "req-auto-gate")
  q_gate <- length(queue_items())
  r <- call(body_gate)
  check("post-commit gate rejection keeps the record and returns 409 non-retryable",
        r$code == 409L && identical(r$body$code, "need_reference") &&
          isTRUE(r$body$record_created) && !isTRUE(r$body$retryable) &&
          length(queue_items()) == q_gate &&
          nrow(DBI::dbGetQuery(e$DB_CON,
            "SELECT * FROM sessions WHERE session_id=?", list(r$body$session_id))) == 1L)

  reset_pass()
  body_bad_step <- intent_body("auto-bad-step", "topic-auto-bad-step")
  body_bad_step$auto_dispatch <- list(step = "not-a-step", req_id = "req-auto-bad-step")
  q_bad <- length(queue_items())
  r <- call(body_bad_step)
  check("invalid auto_dispatch step returns 409 after record commit with no queue item",
        r$code == 409L && identical(r$body$code, "invalid_step") &&
          isTRUE(r$body$record_created) && !isTRUE(r$body$retryable) &&
          length(queue_items()) == q_bad &&
          nrow(DBI::dbGetQuery(e$DB_CON,
            "SELECT * FROM sessions WHERE session_id=?", list(r$body$session_id))) == 1L)

  # Deterministic stand-in for the platform agent's NLU. The app intentionally
  # does not parse natural language; this proves its structured contract can
  # carry the agent's parsed fields through record creation and auto-dispatch.
  reset_pass()
  nl_text <- paste0("请新建项目 nl-stub；数据目录 /nl/data；工作目录 /nl/run；",
                    "物种 human；组织 kidney；主题 topic-nl-stub；",
                    "建好后自动执行 annotate，req_id req-nl-stub")
  stub_agent_parse <- function(text) {
    pick <- function(pat) trimws(sub(pat, "\\1", text))
    project <- pick("^.*项目\\s*([^；]+).*$")
    data_dir <- pick("^.*数据目录\\s*([^；]+).*$")
    work_dir <- pick("^.*工作目录\\s*([^；]+).*$")
    species <- pick("^.*物种\\s*([^；]+).*$")
    tissue <- pick("^.*组织\\s*([^；]+).*$")
    topic <- pick("^.*主题\\s*([^；]+).*$")
    step <- pick("^.*自动执行\\s*([A-Za-z0-9_-]+).*$")
    req_id <- pick("^.*req_id\\s*([A-Za-z0-9._:-]+).*$")
    list(project_name = project, mode = "server",
      server = list(host = "fixture-host", user = "fixture-user"),
      paths = list(data_root = data_dir, work_root = work_dir),
      species = species, tissue = tissue,
      source = list(topic_id = topic, message_id = paste0(topic, "-message")),
      auto_dispatch = list(step = step, req_id = req_id))
  }
  body_nl <- stub_agent_parse(nl_text)
  q_nl <- length(queue_items())
  r <- call(body_nl)
  sid_nl <- r$body$session_id %||% ""
  row_nl <- DBI::dbGetQuery(e$DB_CON,
    "SELECT name, owner_key FROM sessions WHERE session_id=?", list(sid_nl))
  q_nl_after <- queue_items()
  check("agent NL stub reaches record + queued step through the public contract",
        r$code == 200L && isTRUE(r$body$record_created) && isTRUE(r$body$queued) &&
          nzchar(sid_nl) && identical(row_nl$name, "nl-stub") &&
          identical(row_nl$owner_key, owner) && length(q_nl_after) == q_nl + 1L &&
          identical(q_nl_after[[length(q_nl_after)]]$session_id, sid_nl) &&
          identical(q_nl_after[[length(q_nl_after)]]$step, "annotate") &&
          identical(q_nl_after[[length(q_nl_after)]]$req_id, "req-nl-stub") &&
          identical(e$sess_current_id(root), current$id))

  if (length(fail)) stop(paste(length(fail), "intent auto_dispatch failures"), call. = FALSE)
  cat("PASS: intent auto_dispatch contract\n")
}

run_compat()
run_integration()
