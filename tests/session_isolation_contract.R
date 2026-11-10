# Run from app/: Rscript tests/session_isolation_contract.R
# Definitions-only server loading; real auth and SQLite live only in a temporary fixture.
run_contract <- function() {
e <- new.env(parent = globalenv())
for (x in parse(Sys.getenv('SERVE_CONTRACT_SOURCE', 'serve.R'))) {
  if (is.call(x) && identical(x[[1]], as.name('<-')) &&
      is.call(x[[3]]) && identical(x[[3]][[1]], as.name('function'))) eval(x, e)
}
for (file in c('engine/sessions.R', 'engine/db.R', 'engine/agent_gate.R', 'engine/db_api.R'))
  source(file, local = e)
root <- tempfile('session-isolation-'); dir.create(root)
on.exit({
  if (!is.null(e$DB_CON) && DBI::dbIsValid(e$DB_CON)) DBI::dbDisconnect(e$DB_CON)
  unlink(root, recursive = TRUE)
}, add = TRUE)
e$APP_DIR <- root; e$STATE_DIR <- file.path(root, 'state'); dir.create(e$STATE_DIR)
e$STATE_FILE <- file.path(e$STATE_DIR, 'state.json')
e$REMOTE_STEP_FILE <- file.path(e$STATE_DIR, 'remote_steps.json')
e$HAS_SESSIONS <- TRUE; e$HAS_PLACE <- TRUE; e$DB_READY <- TRUE; e$DB_CON <- e$db_connect(root)
e$db_init(e$DB_CON)
owner <- 'viewer:session-contract-fixture'
other_owner <- 'viewer:other-contract-fixture'
token <- e$agent_token_issue(root, owner)$token
other_token <- e$agent_token_issue(root, other_owner)$token
stopifnot(nzchar(token), nzchar(other_token))
e$HAS_QUALITY <- FALSE; e$HAS_STAGE_REVIEWER <- FALSE; e$HAS_LEDGER_OUT <- FALSE
e$STATUS_TEXT <- c(`403` = 'Forbidden', `503` = 'Service Unavailable', `200` = 'OK', `400` = 'Bad Request', `409` = 'Conflict', `500` = 'Internal Server Error')
e$STEP_TASK <- list(); e$IS_WIN <- FALSE; e$HAS_FLOW <- FALSE
e$remote_step_instruction <- function(stepId, pl, session_id, req_id, ...) paste(session_id, req_id)
e$flow_subs_of <- function(...) list()
e$read_text_any <- function(p, ...) if (!file.exists(p)) '' else paste(readLines(p, warn = FALSE), collapse = '\n')
e$read_json_file <- function(p) jsonlite::fromJSON(e$read_text_any(p), simplifyVector = FALSE)
e$place_public <- function(...) list(mode = 'server', server = list(work_root = '/remote/shared'))
e$place_read <- e$place_public
e$place_write <- function(...) writeLines('changed', file.path(root, 'place-write'))
e$inject_satisfy <- function(...) NULL
http_send_json <- e$send_json
e$send_json <- function(con, code, body) list(code = code, body = body)
call <- function(path, body = NULL, credential = token) e$serve_request(NULL, list(target = path,
  method = if (is.null(body)) 'GET' else 'POST', headers = if (is.null(credential)) list() else list('x-agent-token' = credential),
  body = if (is.null(body)) '' else as.character(jsonlite::toJSON(body, auto_unbox = TRUE))))
if ('--http' %in% commandArgs(TRUE)) {
  stopifnot(.Platform$OS.type != 'windows', requireNamespace('curl', quietly = TRUE))
  e$send_json <- http_send_json
  e$PORT <- sample(25000:50000, 1); e$AUTOOPEN <- FALSE
  e$say <- function(...) invisible(NULL)
  child <- parallel::mcparallel({
    # SQLite connections must not be shared across forked processes.
    e$DB_CON <- e$db_connect(root)
    e$main()
  }, silent = TRUE)
  on.exit({tools::pskill(child$pid); parallel::mccollect(child, wait = FALSE)}, add = TRUE, after = FALSE)
  base <- paste0('http://127.0.0.1:', e$PORT)
  ready <- FALSE
  for (i in seq_len(30)) {
    ready <- tryCatch({curl::curl_fetch_memory(paste0(base, '/api/ping')); TRUE}, error = function(x) FALSE)
    if (ready) break
    Sys.sleep(0.1)
  }
  stopifnot(ready)
  call <- function(path, body = NULL, credential = token) {
    h <- curl::new_handle(timeout = 5)
    if (!is.null(credential)) curl::handle_setheaders(h, 'x-agent-token' = credential)
    if (!is.null(body)) curl::handle_setopt(h, customrequest = 'POST',
      postfields = as.character(jsonlite::toJSON(body, auto_unbox = TRUE)))
    x <- curl::curl_fetch_memory(paste0(base, path), h)
    list(code = x$status_code, body = jsonlite::fromJSON(rawToChar(x$content), simplifyVector = FALSE))
  }
}
fail <- character(); check <- function(label, value) {
  cat(if (isTRUE(value)) 'PASS:' else 'FAIL:', label, '\n')
  if (!isTRUE(value)) fail <<- c(fail, label)
}
stopifnot(identical(call('/api/me')$body$owner_key, owner))
a_response <- call('/api/sessions/new', list(name = 'A'))
stopifnot(a_response$code == 200, nzchar(a_response$body$session$id))
a <- a_response$body$session$id
check('new session immediately owns its database partition',
  identical(e$db_scalar(e$DB_CON, 'SELECT owner_key FROM sessions WHERE session_id=?', list(a)), owner))
check('unauthenticated request cannot create records',
  call('/api/sessions/new', list(name = 'denied'), credential = NULL)$code == 403)
check('unknown fixture token fails closed',
  call('/api/sessions/new', list(name = 'denied'), credential = 'pa_agent_fixture_invalid')$code == 403)
invisible(e$sess_set_pending(root, a, 'request-A'))
e$remote_step_put('annotate', list(status = 'delegated'), sid = a)
# Legacy local global state must not escape into newly selected records.
old <- file.path(root, 'old-run'); dir.create(file.path(old, 'status'), recursive = TRUE)
e$write_json_file(file.path(old, 'status', 'step_state.json'), list(status = 'done', step = 'annotate'))
e$save_state(list(runDir = old, runDirRemote = FALSE))
b_response <- call('/api/sessions/new', list(name = '261106测试'))
stopifnot(b_response$code == 200, nzchar(b_response$body$session$id))
b <- b_response$body$session$id
b_status <- call(paste0('/api/status?session=', b))
# Current status is evidence-only: an untouched remote record is unknown, not idle.
check('new record status does not read previous local run', b_status$code == 200 &&
  identical(b_status$body$status, 'unknown') && is.null(b_status$body$step) &&
  length(b_status$body$log_entries) == 0L)
check('new record has no old delegated remote steps', length(call(paste0('/api/status?session=', e$cur_sid()))$body$steps) == 0L)
invisible(e$sess_update(root, b, list(run_dir = '')))
invisible(call('/api/sessions/switch', list(id = b)))
check('switch to empty run clears inherited state', is.null(e$read_state()$runDir))
unlink(file.path(root, 'place-write'))
before <- e$sess_find(root, b)
r <- call('/api/step/import', list(step = 'annotate', status = 'delegated', log = 'late A'))
check('unbound step callback rejected before writing current', r$code == 409 && identical(e$sess_find(root, b), before))
r <- call('/api/envcheck/import', list(result = list(marker = 'late A')))
check('unbound env callback rejected before writing current', r$code == 409 && identical(e$sess_find(root, b), before))
for (status in c('running', 'delegated', 'stopped', 'failed', 'done')) {
  r <- call('/api/step/import', list(session_id = a, req_id = 'old-A', step = 'annotate', status = status))
  check(paste('stale request rejected for', status), r$code == 409)
}
# A request alone is sufficient only when it resolves to one existing owner.
r <- call('/api/step/import', list(req_id = 'request-A', step = 'annotate', status = 'failed', log = 'real failure'))
check('unique request callback resolves A not currently selected B', r$code == 200 && identical(r$body$session, a) && is.null(e$sess_find(root, b)$steps$annotate))
before_owner_attack <- e$sess_find(root, a)
r <- call('/api/step/import', list(session_id = a, req_id = 'request-A',
  step = 'annotate', status = 'failed', log = 'foreign owner'), credential = other_token)
check('other owner token cannot write A even with exact request binding',
  r$code == 403 && identical(r$body$code, 'run_log_not_owner') &&
  identical(e$sess_find(root, a), before_owner_attack) && identical(e$cur_sid(), b))
r <- call(paste0('/api/status?session=', b), credential = other_token)
check('other owner token cannot read selected B status', r$code == 403)
check('owner B status remains empty after late A and foreign callbacks',
  length(call(paste0('/api/status?session=', b))$body$steps) == 0L &&
  length(call(paste0('/api/status?session=', b))$body$log_entries) == 0L)
before_env_attack <- e$sess_find(root, a)
r <- call('/api/envcheck/import', list(session_id = a, req_id = 'request-A',
  result = list(marker = 'foreign owner')), credential = other_token)
check('other owner token cannot replace A environment result',
  r$code == 403 && identical(e$sess_find(root, a), before_env_attack))
r <- call('/api/envcheck/import', list(req_id_carried = 'request-A', result = list(marker = 'A', checked_from = 'PC-A')))
check('carried legacy request resolves original env owner', r$code == 200 && identical(r$body$session, a))
check('late env callback never changes current execution config', !file.exists(file.path(root, 'place-write')))
check('envcheck on B does not expose A latest result', is.null(call('/api/envcheck')$body$result))
invisible(call('/api/sessions/switch', list(id = a)))
r <- call(paste0('/api/run/logs?session=', a), credential = other_token)
check('other owner token cannot read selected A logs', r$code == 403 &&
  identical(r$body$code, 'run_log_not_owner'))
check('envcheck on A restores own result', identical(call('/api/envcheck')$body$result$marker, 'A'))
a_status <- call(paste0('/api/status?session=', a))
# Remote stage observations are returned in steps; no synthetic current stage.
check('status on A restores own remote step', a_status$code == 200 &&
  length(a_status$body$steps) == 1L &&
  identical(a_status$body$steps[[1]]$id, 'annotate') &&
  identical(a_status$body$steps[[1]]$status, 'failed') &&
  identical(a_status$body$steps[[1]]$log, 'real failure'))
check('late A callback persisted only in A owner log partition',
  length(a_status$body$log_entries) == 1L &&
  identical(a_status$body$log_entries[[1]]$owner_key, owner) &&
  identical(a_status$body$log_entries[[1]]$session_id, a) &&
  identical(a_status$body$log_entries[[1]]$content, 'real failure'))
e$write_json_file(file.path(e$STATE_DIR, 'last_commit.json'), list(id = 'request-B'))
r <- call(paste0('/api/brief?step=annotate&session=', a))
check('brief preserves explicit session and its own request', identical(r$body$text, paste(a, 'request-A')))
r <- call('/api/config', list())
check('config delegation uses active record request not global latest', grepl('request-A', r$body$instruction, fixed = TRUE))
invisible(e$sess_set_pending(root, b, 'request-A'))
r <- call('/api/step/import', list(req_id = 'request-A', step = 'annotate', status = 'running'))
check('ambiguous legacy request never chooses current', r$code == 409)
# Explicit owner without request remains supported, but cannot claim another request.
r <- call('/api/step/import', list(session_id = b, step = 'selftest', status = 'failed', log = 'explicit owner'))
check('explicit legacy owner without req remains compatible', r$code == 200 && identical(r$body$session, b))
check('non-current callback leaves current selection unchanged', identical(e$cur_sid(), a))
r <- call(paste0('/api/status?session=', b))
check('stale tab status cannot read another selected record', r$code == 409 && identical(r$body$code, 'session_selection_changed'))
r <- call(paste0('/api/config?session=', b), list(session_id = b))
check('stale tab write rejected before current record delegation', r$code == 409)
r <- call(paste0('/api/status?session=', a))
check('matching tab status remains available', r$code == 200)
if (length(fail)) stop(paste(length(fail), 'session isolation contract failures'), call. = FALSE)
cat('PASS: session isolation contract\n')
}
run_contract()
