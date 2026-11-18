#!/usr/bin/env Rscript
# Stage 4 contract: agent-token dispatch uses the full run_enqueue gate chain and
# never depends on or changes current. Temporary app tree and SQLite only.
run_contract <- function() {
  root <- tempfile("agent-dispatch-")
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
  # sessions.R loads attempts.R with local=FALSE; in this isolated environment
  # re-bind it beside sess_dir so verdict fixtures resolve the same temp tree.
  sys.source("engine/attempts.R", envir = e)
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
  call <- function(body, token = token_a) {
    e$serve_request(NULL, list(
      target = "/api/agent/dispatch", method = "POST",
      headers = if (is.null(token)) list() else list("x-agent-token" = token),
      body = as.character(jsonlite::toJSON(body, auto_unbox = TRUE))))
  }
  db_add <- function(rec, owner, version_id = "") {
    DBI::dbExecute(e$DB_CON,
      paste0("INSERT INTO sessions(session_id,owner_key,name,mode,run_dir,visibility,",
             "version_id,created_at,updated_at) VALUES(?,?,?,?,?,?,?,?,?)"),
      list(rec$id, owner, rec$name, rec$mode, rec$run_dir, "private", version_id,
           rec$created_at, rec$updated_at))
  }
  server_place <- function(run_dir) list(mode = "server", server = list(
    host = "fixture-host", user = "fixture-user", data_root = "/fixture/data",
    work_root = run_dir, ref_mode = "", ref_path = ""))
  make_record <- function(name, owner, run_dir, version_id = "") {
    rec <- e$sess_create(root, name, server_place(run_dir), activate = FALSE)
    db_add(rec, owner, version_id)
    rec
  }
  queue_items <- function() {
    f <- file.path(root, "state", "exec_tasks.json")
    if (!file.exists(f)) return(list())
    jsonlite::fromJSON(f, simplifyVector = FALSE)$items %||% list()
  }
  side_snapshot <- function() {
    q <- if (file.exists(file.path(root, "state", "exec_tasks.json")))
      readBin(file.path(root, "state", "exec_tasks.json"), "raw",
              n = file.info(file.path(root, "state", "exec_tasks.json"))$size)
    else raw()
    attempts <- sort(list.files(file.path(root, "state", "sessions"),
      pattern = "^attempt[.]json$", recursive = TRUE, full.names = TRUE))
    list(queue = q, attempts = attempts)
  }
  reset_pass <- function() {
    e$HAS_CONTRACT <- FALSE
    e$HAS_DECISIONS <- FALSE
    e$HAS_REFERENCE <- FALSE
    e$HAS_STAGE_REVIEWER <- FALSE
    e$HAS_QUALITY <- FALSE
    e$HAS_LEDGER_OUT <- FALSE
    e$HAS_NOTEBOOK <- FALSE
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
    e$contract_check <- function(...) list(issues = list())
    e$db_api_version_of <- function(...) list(version_id = "")
    e$db_api_gate <- function(...) NULL
    e$dec_ensure <- function(...) TRUE
    e$dec_pending <- function(...) list()
    e$dec_ask_text <- function(...) "DECISION"
    e$supplement_needs_reference <- function(...) FALSE
    e$ref_list <- function(...) list()
    e$sr_preflight <- function(...) list(verdict = "pass", checks = list(), blockers = list())
  }
  expect_gate <- function(label, code, body, expected) {
    before <- side_snapshot()
    r <- call(body)
    after <- side_snapshot()
    hit <- identical(r$code, 409L) && identical(r$body$code, expected) &&
      identical(before, after)
    check(label, hit)
  }

  owner_a <- "viewer:dispatch-owner-a"
  owner_b <- "viewer:dispatch-owner-b"
  token_a <- e$agent_token_issue(root, owner_a)$token
  token_b <- e$agent_token_issue(root, owner_b)$token
  run_a <- file.path(root, "run-a")
  dir.create(run_a)
  current <- e$sess_create(root, "current-local", list(
    mode = "local", local = list(path = run_a), server = list()), activate = TRUE)
  db_add(current, owner_a)
  rec_b <- make_record("owned-server", owner_a, "/fixture/run-b")
  rec_other <- make_record("other-server", owner_b, "/fixture/run-other")

  reset_pass()
  current_before <- e$sess_current_id(root)
  r <- call(list(session_id = rec_b$id, step = "annotate", req_id = "req-agent-1"))
  items <- queue_items()
  check("agent dispatch queues its own explicit record without switching current",
        r$code == 200L && isTRUE(r$body$exec_queued) &&
          identical(r$body$session_id, rec_b$id) &&
          identical(e$sess_current_id(root), current$id) &&
          identical(current_before, current$id) && length(items) == 1L &&
          identical(items[[1]]$session_id, rec_b$id) &&
          identical(items[[1]]$step, "annotate") &&
          identical(items[[1]]$req_id, "req-agent-1"))
  first_exec <- r$body$exec_id
  r <- call(list(session_id = rec_b$id, step = "annotate", req_id = "req-agent-1"))
  check("same agent req_id returns the original task and creates no duplicate",
        r$code == 200L && isTRUE(r$body$duplicate_dispatch) &&
          identical(r$body$exec_id, first_exec) && length(queue_items()) == 1L &&
          identical(e$sess_current_id(root), current$id))
  r <- call(list(session_id = rec_other$id, step = "annotate", req_id = "req-agent-other"))
  check("agent cannot dispatch another owner's record", r$code == 403L &&
          length(queue_items()) == 1L && identical(e$sess_current_id(root), current$id))
  r <- call(list(session_id = rec_b$id, step = "annotate", req_id = "req-no-token"), token = NULL)
  check("dispatch without token is 403 agent_token_required",
        r$code == 403L && identical(r$body$code, "agent_token_required"))
  r <- call(list(session_id = rec_b$id, step = "annotate", req_id = "req-force", force = TRUE))
  check("agent force is rejected before queueing",
        r$code == 403L && identical(r$body$code, "force_requires_human") &&
          length(queue_items()) == 1L)
  r <- call(list(session_id = rec_b$id, step = "not-a-step", req_id = "req-invalid-step"))
  check("unknown agent step is rejected before queueing",
        r$code == 409L && identical(r$body$code, "invalid_step") &&
          length(queue_items()) == 1L)

  view <- e$serve_request(NULL, list(target = paste0("/api/exec/view?session=", rec_b$id),
    method = "GET", headers = list("x-agent-token" = token_a), body = ""))
  check("queued progress is visible through the existing record exec view",
        view$code == 200L && length(view$body$items %||% list()) == 1L &&
          identical(view$body$items[[1]]$status, "queued") &&
          identical(view$body$items[[1]]$step, "annotate"))

  qf <- file.path(root, "state", "exec_tasks.json")
  qdoc <- jsonlite::fromJSON(qf, simplifyVector = FALSE)
  qdoc$items[[1]]$status <- "failed"
  writeLines(as.character(jsonlite::toJSON(qdoc, auto_unbox = TRUE, null = "null",
                                           digits = NA)), qf)
  r <- call(list(session_id = rec_b$id, step = "annotate", req_id = "req-agent-1"))
  check("same req_id remains idempotent after the original task reaches failed",
        r$code == 200L && isTRUE(r$body$duplicate_dispatch) &&
          identical(r$body$exec_id, first_exec) && length(queue_items()) == 1L &&
          identical(queue_items()[[1]]$status, "failed") &&
          identical(e$sess_current_id(root), current$id))

  rec_enq <- make_record("queue-write-failure", owner_a, "/fixture/run-enq")
  reset_pass()
  original_enq <- e$exec_enqueue
  e$exec_enqueue <- function(...) stop("fixture queue write failure")
  q_before <- length(queue_items())
  r <- call(list(session_id = rec_enq$id, step = "annotate", req_id = "req-agent-enq-fail"))
  e$exec_enqueue <- original_enq
  check("agent queue write failure is retryable 503, not a false success",
        r$code == 503L && identical(r$body$code, "enqueue_failed") &&
          isTRUE(r$body$retryable) && length(queue_items()) == q_before)
  r <- call(list(session_id = rec_enq$id, step = "annotate", req_id = "req-agent-enq-fail"))
  check("retrying the same agent req_id fills only the queue after 503",
        r$code == 200L && isTRUE(r$body$exec_queued) &&
          length(queue_items()) == q_before + 1L &&
          identical(queue_items()[[length(queue_items())]]$req_id, "req-agent-enq-fail"))

  rec_decision <- make_record("decision-gate", owner_a, "/fixture/run-decision")
  reset_pass(); e$HAS_DECISIONS <- TRUE
  e$dec_pending <- function(...) list(list(step = "annotate", key = "choose", title = "Choose"))
  expect_gate("missing decision returns 409 and writes no queue/attempt", "need_decision",
    list(session_id = rec_decision$id, step = "annotate", req_id = "req-gate-decision"),
    "need_decision")

  rec_ref <- make_record("reference-gate", owner_a, "/fixture/run-ref")
  reset_pass(); e$HAS_REFERENCE <- TRUE
  e$supplement_needs_reference <- function(step) identical(step, "annotate")
  expect_gate("missing reference returns 409 and writes no queue/attempt", "need_reference",
    list(session_id = rec_ref$id, step = "annotate", req_id = "req-gate-ref"),
    "need_reference")

  rec_pre <- make_record("preflight-gate", owner_a, "/fixture/run-pre")
  reset_pass(); e$HAS_STAGE_REVIEWER <- TRUE
  e$sr_preflight <- function(...) list(verdict = "block", permit_id = "permit-x",
    blockers = list(list(fix = "fixture")), checks = list())
  expect_gate("blocked stage preflight returns 409 and writes no queue/attempt",
    "stage_review_blocked",
    list(session_id = rec_pre$id, step = "annotate", req_id = "req-gate-pre"),
    "stage_review_blocked")

  rec_contract <- make_record("contract-gate", owner_a, "/fixture/run-contract")
  reset_pass(); e$HAS_CONTRACT <- TRUE
  e$contract_check <- function(...) list(issues = list(list(
    level = "fail", stages = list("annotate"), what = "fixture mismatch",
    where = "flow_spec", fix = "repair fixture")))
  expect_gate("contract mismatch returns 409 and writes no queue/attempt",
    "contract_mismatch",
    list(session_id = rec_contract$id, step = "annotate", req_id = "req-gate-contract"),
    "contract_mismatch")

  rec_db <- make_record("db-gate", owner_a, "/fixture/run-db", version_id = "v-db")
  reset_pass()
  e$db_api_version_of <- function(...) list(version_id = "v-db")
  e$db_api_gate <- function(...) list(code = 409L, body = list(ok = FALSE,
    code = "db_gate_blocked", step = "annotate", error = "fixture DB gate"))
  expect_gate("DB gate returns 409 and writes no queue/attempt", "db_gate_blocked",
    list(session_id = rec_db$id, step = "annotate", req_id = "req-gate-db"),
    "db_gate_blocked")

  rec_up <- make_record("upstream-gate", owner_a, "/fixture/run-up")
  reset_pass()
  e$flow_run_order <- function(...) c("before", "annotate")
  e$sess_get_pending <- function(...) "rq-upstream-reject"
  ad <- file.path(root, "state", "sessions", rec_up$id, "attempts", "rq-upstream-reject")
  dir.create(ad, recursive = TRUE, showWarnings = FALSE)
  writeLines('{"step":"before","code":"fixture_reject"}',
             file.path(ad, "verdict_reject-1.json"))
  expect_gate("upstream rejected evidence returns 409 and writes no queue/attempt",
    "upstream_rejected",
    list(session_id = rec_up$id, step = "annotate", req_id = "req-gate-upstream"),
    "upstream_rejected")

  rec_intent <- make_record("intent-gate", owner_a, "/fixture/run-intent")
  reset_pass()
  e$intent_execution_gate <- function(...) list(ok = FALSE,
    code = "intent_scope_outside_plan", error = "fixture intent gate")
  expect_gate("intent/profile gate returns 409 and writes no queue/attempt",
    "intent_scope_outside_plan",
    list(session_id = rec_intent$id, step = "annotate", req_id = "req-gate-intent"),
    "intent_scope_outside_plan")

  rec_profile <- make_record("target-flow-profile", owner_a, "/fixture/run-profile")
  reset_pass()
  e$flow_run_order <- function(sid = NULL) {
    if (identical(as.character(sid), rec_profile$id)) c("annotate")
    else c("current-only-step")
  }
  q_before <- length(queue_items())
  r <- call(list(session_id = rec_profile$id, step = "annotate", req_id = "req-target-profile"))
  check("agent step admission uses the target record's flow profile, not current",
        r$code == 200L && isTRUE(r$body$exec_queued) &&
          length(queue_items()) == q_before + 1L &&
          identical(queue_items()[[length(queue_items())]]$session_id, rec_profile$id) &&
          identical(e$sess_current_id(root), current$id))

  if (length(fail)) stop(paste(length(fail), "agent dispatch failures"), call. = FALSE)
  cat("PASS: agent dispatch contract\n")
}
run_contract()
