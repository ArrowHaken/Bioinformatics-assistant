# Run from app/: Rscript tests/record_context_contract.R
# Definitions-only server loading. State, SQLite, mirrors and tokens are temporary.
run_contract <- function() {
  `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
  e <- new.env(parent = globalenv())
  for (x in parse('serve.R')) {
    if (is.call(x) && identical(x[[1]], as.name('<-')) &&
        is.call(x[[3]]) && identical(x[[3]][[1]], as.name('function'))) eval(x, e)
  }

  root <- tempfile('record-context-')
  dir.create(root, recursive = TRUE)
  on.exit({
    if (!is.null(e$DB_CON) && DBI::dbIsValid(e$DB_CON)) DBI::dbDisconnect(e$DB_CON)
    unlink(root, recursive = TRUE)
  }, add = TRUE)
  e$APP_DIR <- root
  e$ENGINE_DIR <- normalizePath('engine', winslash = '/', mustWork = TRUE)
  e$PUBLIC_DIR <- normalizePath('public', winslash = '/', mustWork = TRUE)
  e$GUARD_DIR <- file.path(e$ENGINE_DIR, 'workflow')
  e$SUPPLEMENT_DIR <- file.path(e$ENGINE_DIR, 'library', 'C_supplement')
  e$LIB_ROOT <- file.path(e$ENGINE_DIR, 'library')
  e$STATE_DIR <- file.path(root, 'state')
  e$STATE_FILE <- file.path(e$STATE_DIR, 'state.json')
  e$REMOTE_STEP_FILE <- file.path(e$STATE_DIR, 'remote_steps.json')
  e$EXEC_FILE <- file.path(e$STATE_DIR, 'exec_tasks.json')
  e$EXEC_HB_FILE <- file.path(e$STATE_DIR, 'exec_heartbeat.json')
  dir.create(e$STATE_DIR, recursive = TRUE)

  for (file in c('engine/sessions.R', 'engine/attempts.R', 'engine/db.R',
                 'engine/agent_gate.R', 'engine/db_api.R', 'engine/record_access.R',
                 'engine/run_enqueue.R', 'engine/flow_spec.R', 'engine/cover_rules.R'))
    source(file, local = e)
  old_defs_only <- getOption('pa.chain.definitions_only')
  options(pa.chain.definitions_only = TRUE)
  source('engine/run_chain.R', local = e)
  options(pa.chain.definitions_only = old_defs_only)

  e$HAS_SESSIONS <- TRUE
  e$HAS_PLACE <- FALSE
  e$HAS_FLOW <- TRUE
  e$HAS_QUALITY <- FALSE
  e$HAS_CONTRACT <- FALSE
  e$HAS_DECISIONS <- FALSE
  e$HAS_INTENT <- FALSE
  for (flag in c('HAS_AUDIT', 'HAS_EXEC_GUARD', 'HAS_EXEC_RECOVER',
                 'HAS_FIGS_AUDIT', 'HAS_FRESHNESS', 'HAS_HISTORY',
                 'HAS_LEDGER_OUT', 'HAS_MIRROR', 'HAS_NOTEBOOK',
                 'HAS_PACK_BINDING', 'HAS_PPT_BUILD', 'HAS_REFERENCE',
                 'HAS_RUNS', 'HAS_SCRIPT_OPS', 'HAS_SCRIPT_RESOLVE',
                 'HAS_STAGE_REVIEWER', 'HAS_VERDICT'))
    assign(flag, FALSE, envir = e)
  e$DB_READY <- TRUE
  e$DB_CON <- e$db_connect(root)
  e$db_init(e$DB_CON)
  e$STATUS_TEXT <- c(`200` = 'OK', `400` = 'Bad Request', `403` = 'Forbidden',
                     `404` = 'Not Found', `409` = 'Conflict',
                     `500` = 'Internal Server Error', `503` = 'Service Unavailable')
  e$STEP_TASK <- list()
  e$IS_WIN <- FALSE
  e$ARTIFACT_KINDS <- list('.png' = '图片', '.pptx' = 'PPT', '.zip' = '打包')
  e$read_text_any <- function(p, ...) if (!file.exists(p)) '' else
    paste(readLines(p, warn = FALSE), collapse = '\n')
  e$pjoin <- function(...) paste(pkg <- c(...), collapse = '/')
  e$species_canon <- function(x) as.character(x %||% '')
  e$send_json <- function(con, code, body) list(code = code, body = body)

  fail <- character()
  check <- function(label, value) {
    cat(if (isTRUE(value)) 'PASS:' else 'FAIL:', label, '\n')
    if (!isTRUE(value)) fail <<- c(fail, label)
  }
  call <- function(path, body = NULL, credential = token_a) {
    e$serve_request(NULL, list(
      target = path,
      method = if (is.null(body)) 'GET' else 'POST',
      headers = if (is.null(credential)) list() else list('x-agent-token' = credential),
      body = if (is.null(body)) '' else as.character(jsonlite::toJSON(body, auto_unbox = TRUE))))
  }

  owner_a <- 'viewer:record-context-a'
  owner_s <- 'viewer:record-context-s'
  token_a <- e$agent_token_issue(root, owner_a)$token
  token_s <- e$agent_token_issue(root, owner_s)$token
  stopifnot(nzchar(token_a), nzchar(token_s))

  run_a <- file.path(root, 'run-a'); dir.create(file.path(run_a, 'status'), recursive = TRUE)
  run_b <- '/remote/run-b'
  run_s <- '/remote/run-s'
  run_empty <- file.path(root, 'run-empty'); dir.create(run_empty)
  run_s_local <- file.path(root, 'run-s-local')
  dir.create(file.path(run_s_local, 'status'), recursive = TRUE)
  run_b_local <- file.path(root, 'run-b-local')
  dir.create(run_b_local, recursive = TRUE)
  run_noplace <- file.path(root, 'run-noplace'); dir.create(run_noplace)
  run_a_server <- '/remote/run-a-server'
  place_a <- list(mode = 'local', local = list(path = run_a), server = list())
  place_b <- list(mode = 'server', server = list(
    host = 'fixture-host', work_root = run_b,
    ref_mode = 'path', ref_path = '/refs/B.xlsx'))
  place_s <- list(mode = 'server', server = list(
    host = 'fixture-host', work_root = run_s,
    ref_mode = 'path', ref_path = '/refs/S.xlsx'))
  place_empty <- list(mode = 'local', local = list(path = ''), server = list())
  place_s_local <- list(mode = 'local', local = list(path = run_s_local), server = list())
  place_b_local <- list(mode = 'local', local = list(path = run_b_local), server = list())
  place_a_server <- list(mode = 'server', server = list(
    host = 'fixture-host', work_root = run_a_server,
    ref_mode = 'path', ref_path = '/refs/A-server.xlsx'))
  a <- e$sess_create(root, 'A', place_a, activate = TRUE)
  b <- e$sess_create(root, 'B', place_b, activate = FALSE)
  s <- e$sess_create(root, 'S', place_s, activate = FALSE)
  empty <- e$sess_create(root, 'EMPTY', place_empty, activate = FALSE)
  s_local <- e$sess_create(root, 'S_LOCAL', place_s_local, activate = FALSE)
  b_local <- e$sess_create(root, 'B_LOCAL', place_b_local, activate = FALSE)
  a_server <- e$sess_create(root, 'A_SERVER', place_a_server, activate = FALSE)
  noplace <- e$sess_create(root, 'NOPLACE', list(), activate = FALSE)
  e$sess_update(root, noplace$id, list(mode = 'local', run_dir = run_noplace,
                                      place = list()))
  noplace <- e$sess_find(root, noplace$id)
  db_empty <- e$sess_create(root, 'DB_EMPTY', place_a, activate = FALSE)
  vjson <- e$sess_create(root, 'VJSON', place_a, activate = FALSE)
  e$sess_update(root, vjson$id, list(version_id = 'vJSON'))
  vjson <- e$sess_find(root, vjson$id)
  db_add <- function(rec, owner, visibility = 'private', version_id = '') {
    DBI::dbExecute(e$DB_CON,
      'INSERT INTO sessions(session_id,owner_key,name,mode,run_dir,visibility,version_id,created_at,updated_at) VALUES(?,?,?,?,?,?,?,?,?)',
      list(rec$id, owner, rec$name, rec$mode, rec$run_dir, visibility,
           version_id, rec$created_at, rec$updated_at))
  }
  db_add(a, owner_a, 'private', 'vA')
  db_add(b, owner_a, 'private', 'vB')
  db_add(s, owner_s, 'shared', 'vS')
  db_add(empty, owner_a, 'private', 'vEmpty')
  db_add(s_local, owner_s, 'shared', 'vSL')
  db_add(b_local, owner_a, 'private', 'vBL')
  db_add(a_server, owner_a, 'private', 'vAS')
  db_add(noplace, owner_a, 'private', 'vNP')
  db_add(db_empty, owner_a, 'private', '')
  DBI::dbExecute(e$DB_CON, 'UPDATE sessions SET run_dir = ? WHERE session_id = ?',
                 list('', db_empty$id))
  db_add(vjson, owner_a, 'private', '')

  first_stage <- e$FLOW_ORDER('server')[[1L]]
  stopifnot(nzchar(first_stage))
  e$remote_step_put(first_stage, list(
    title = 'B step', status = 'done', log = 'B_ONLY',
    session_id = b$id), sid = b$id)
  e$remote_step_put(first_stage, list(
    title = 'S step', status = 'done', log = 'S_ONLY',
    session_id = s$id,
    artifacts = list(list(rel = 'S_ONLY.png', bytes = 7))), sid = s$id)
  e$write_json_file(file.path(run_a, 'status', 'step_state.json'),
                    list(step = 'local-A', status = 'running'))
  writeLines('A_ONLY', file.path(run_a, 'status', 'run.log'))
  e$write_json_file(file.path(run_s_local, 'status', 'step_state.json'),
                    list(step = 'local-S', status = 'running', pid = 99999999L))
  writeLines('S_LOCAL_ONLY', file.path(run_s_local, 'status', 'run.log'))

  current_before <- e$sess_current_id(root)
  exec_before <- if (file.exists(e$EXEC_FILE)) readLines(e$EXEC_FILE, warn = FALSE) else character()
  a_pipeline <- file.path(run_a, 'pipeline.json')
  a_before <- if (file.exists(a_pipeline)) readLines(a_pipeline, warn = FALSE) else character()

  r <- call(paste0('/api/status?session=', b$id))
  check('status uses target B session/mode/run_dir and remote steps',
        r$code == 200 && identical(r$body$session_id, b$id) &&
        identical(r$body$mode, 'server') && identical(r$body$run_dir, run_b) &&
        isTRUE(r$body$remote) && length(r$body$steps) == 1L &&
        identical(r$body$steps[[1]]$log, 'B_ONLY') &&
        identical(r$body$steps[[1]]$session_id, b$id) &&
        identical(r$body$steps[[1]]$run_dir, run_b) &&
        !grepl('A_ONLY', paste(vapply(r$body$steps, function(x) x$log %||% '', ''),
                               collapse = '\n'), fixed = TRUE))

  r <- call('/api/status')
  check('status without session still reads true current A',
        r$code == 200 && identical(r$body$session_id, a$id) &&
        identical(r$body$mode, 'local') && isTRUE(grepl('A_ONLY', r$body$log, fixed = TRUE)))

  flow_b1 <- call(paste0('/api/flow?session=', b$id))
  flow_b2 <- call(paste0('/api/flow?session=', b$id))
  flow_text <- if (isTRUE(flow_b1$code == 200)) jsonlite::toJSON(flow_b1$body, auto_unbox = TRUE) else ''
  check('flow binds target B and contains only B remote progress',
        flow_b1$code == 200 && identical(flow_b1$body$session_id, b$id) &&
        identical(flow_b1$body$run_dir, run_b) && grepl('B_ONLY', flow_text, fixed = TRUE) &&
        !grepl('A_ONLY', flow_text, fixed = TRUE) && !grepl('S_ONLY', flow_text, fixed = TRUE))
  check('flow progress is deterministic for the target record',
        identical(flow_b1$code, flow_b2$code) && identical(flow_b1$body, flow_b2$body))

  r <- call(paste0('/api/status?session=', s$id), credential = token_a)
  check('shared detail read comes from S with read-only capability',
        r$code == 200 && identical(r$body$session_id, s$id) &&
        identical(r$body$run_dir, run_s) && isTRUE(r$body$can_read) &&
        !isTRUE(r$body$can_write) && isTRUE(r$body$readonly) &&
        length(r$body$steps) == 1L && identical(r$body$steps[[1]]$log, 'S_ONLY'))
  s_local_state <- file.path(run_s_local, 'status', 'step_state.json')
  s_local_before <- readBin(s_local_state, 'raw', n = file.info(s_local_state)$size)
  r <- call(paste0('/api/status?session=', s_local$id), credential = token_a)
  s_local_after <- readBin(s_local_state, 'raw', n = file.info(s_local_state)$size)
  check('shared local status reads target without correcting/writing step_state.json',
        r$code == 200 && identical(r$body$session_id, s_local$id) &&
        identical(r$body$run_dir, run_s_local) && !isTRUE(r$body$remote) &&
        isTRUE(grepl('S_LOCAL_ONLY', r$body$log, fixed = TRUE)) &&
        identical(s_local_before, s_local_after))
  r <- call(paste0('/api/artifacts?session=', s$id), credential = token_a)
  art_s <- vapply(r$body$items %||% list(), function(x) as.character(x$rel %||% ''), '')
  check('shared artifacts list reads only the explicit S record',
        r$code == 200 && identical(r$body$session_id, s$id) &&
        isTRUE(r$body$can_read) && !isTRUE(r$body$can_write) &&
        identical(art_s, 'S_ONLY.png'))
  r <- call('/api/sessions/state')
  check('session state marks current fallback as record_source=current',
        r$code == 200 && identical(r$body$viewing_session_id, a$id) &&
        identical(r$body$record_source, 'current'))
  r <- call(paste0('/api/sessions/state?id=', b$id))
  check('session state marks an explicit target as record_source=explicit',
        r$code == 200 && identical(r$body$viewing_session_id, b$id) &&
        identical(r$body$record_source, 'explicit') &&
        identical(e$sess_current_id(root), current_before))

  # A record without its own place must not inherit the global profile. This is
  # the distinction the page relies on to avoid showing another record's host.
  dir.create(file.path(root, 'config'), recursive = TRUE, showWarnings = FALSE)
  e$write_json_file(file.path(root, 'config', 'server_profile.json'),
                    list(mode = 'server', local = list(path = run_a),
                         server = list(host = 'GLOBAL_ONLY', user = 'u',
                                       data_root = '/global', work_root = '/global')))
  e$HAS_PLACE <- TRUE
  source('engine/place.R', local = e)
  r <- call(paste0('/api/sessions/state?id=', noplace$id))
  state_place <- r$body$place %||% list()
  r_place <- call(paste0('/api/place?session=', noplace$id))
  place_place <- r_place$body %||% list()
  check('state/place derive only from the target record, never global place',
        r$code == 200 && identical(r$body$viewing_session_id, noplace$id) &&
        !identical(state_place$server$host, 'GLOBAL_ONLY') &&
        r_place$code == 200 && identical(r_place$body$session_id, noplace$id) &&
        !identical(place_place$server$host, 'GLOBAL_ONLY') &&
        identical(place_place$local_path, run_noplace))

  r <- call('/api/config', list(session_id = s$id, `pipeline.json` = list(fixture = 'shared-write')))
  check('shared write remains forbidden without side effects',
        r$code == 403 && identical(r$body$code, 'not_owner') &&
        identical(e$sess_current_id(root), current_before))
  r <- call('/api/sessions/switch', list(id = s$id))
  check('shared record cannot be made current by switch',
        r$code == 403 && identical(e$sess_current_id(root), current_before))
  r <- call('/api/stop', list(session_id = s$id), credential = token_a)
  check('shared stop is forbidden before touching any process/context',
        r$code == 403 && identical(r$body$code, 'not_owner') &&
        identical(e$sess_current_id(root), current_before))

  shared_record_before <- e$sess_find(root, s$id)
  shared_paths <- list(
    list('/api/sessions/update', list(id = s$id, name = 'must-not-write')),
    list('/api/sessions/archive', list(id = s$id)),
    list('/api/sessions/restore', list(id = s$id)),
    list('/api/place', list(session_id = s$id, local = list(path = run_a))),
    list('/api/run', list(session_id = s$id, step = first_stage)),
    list('/api/review', list(session_id = s$id, stage = first_stage)),
    list('/api/ppt/build', list(session_id = s$id)),
    list('/api/pack/build', list(session_id = s$id)),
    list('/api/artifact/request', list(session_id = s$id, rel = '10.figs/S_ONLY.png')),
    list('/api/artifacts/import', list(session_id = s$id, items = list())),
    list('/api/notebook/cells', list(session_id = s$id, cells = list())))
  shared_results <- lapply(shared_paths, function(x)
    call(x[[1L]], x[[2L]], credential = token_a))
  check('all shared metadata/execution writes are rejected without side effects',
        all(vapply(shared_results, function(x)
          identical(x$code, 403L) && identical(x$body$code, 'not_owner'), logical(1))) &&
        identical(e$sess_find(root, s$id), shared_record_before) &&
        identical(e$sess_current_id(root), current_before))

  paths <- c('/api/status', '/api/flow', '/api/run', '/api/config', '/api/stop',
             '/api/review', '/api/ppt/build', '/api/pack/build')
  conflict_results <- lapply(paths, function(path)
    call(paste0(path, '?session=', b$id), list(session_id = a$id)))
  check('query/body conflict is 400 on every record execution route',
        all(vapply(conflict_results, function(x)
          identical(x$code, 400L) && identical(x$body$code, 'session_conflict'), logical(1))) &&
        identical(e$sess_current_id(root), current_before) &&
        identical(if (file.exists(e$EXEC_FILE)) readLines(e$EXEC_FILE, warn = FALSE) else character(), exec_before))
  reverse_conflict <- call(paste0('/api/status?session=', a$id), list(session_id = b$id))
  check('reverse query/body conflict is also rejected before reading',
        reverse_conflict$code == 400 && identical(reverse_conflict$body$code,
                                                  'session_conflict') &&
        identical(e$sess_current_id(root), current_before))
  r <- call(paste0('/api/sessions/switch?session=', b$id), list(id = a$id))
  check('sessions query session and body id use the same canonical conflict rule',
        r$code == 400 && identical(r$body$code, 'session_conflict') &&
        identical(r$body$query_session, b$id) &&
        identical(r$body$body_session_id, a$id) &&
        identical(e$sess_current_id(root), current_before))
  r <- call(paste0('/api/config?session=', b$id), list(session_id = a$id))
  check('conflict is rejected before handler and leaves current/queue/A unchanged',
        r$code == 400 && identical(r$body$code, 'session_conflict') &&
        identical(e$sess_current_id(root), current_before) &&
        identical(if (file.exists(e$EXEC_FILE)) readLines(e$EXEC_FILE, warn = FALSE) else character(), exec_before) &&
        identical(if (file.exists(a_pipeline)) readLines(a_pipeline, warn = FALSE) else character(), a_before))

  r <- call('/api/config', list(
    session_id = b$id,
    `gates.json` = list(gates = list(list(id = 'fixture', status = 'answered', answer = 'B')))))
  cfg_b <- e$remote_step_get('config', b$id)
  check('config writes the explicit target B, never current A',
        r$code == 200 && identical(r$body$session_id, b$id) && !is.null(cfg_b) &&
        identical(cfg_b$session_id, b$id) && identical(cfg_b$status, 'delegated') &&
        identical(if (file.exists(a_pipeline)) readLines(a_pipeline, warn = FALSE) else character(), a_before))

  r <- call('/api/config', list(
    session_id = b_local$id,
    `pipeline.json` = list(fixture = 'B_LOCAL_ONLY')))
  b_local_pipeline <- file.path(run_b_local, 'pipeline.json')
  check('local config writes the explicit target run_dir, not current A',
        r$code == 200 && identical(r$body$session_id, b_local$id) &&
        file.exists(b_local_pipeline) &&
        identical(jsonlite::fromJSON(b_local_pipeline, simplifyVector = FALSE)$fixture,
                  'B_LOCAL_ONLY') &&
        identical(if (file.exists(a_pipeline)) readLines(a_pipeline, warn = FALSE) else character(), a_before))

  mismatch_req <- 'req-record-context-mismatch'
  e$attempt_register(root, b$id, mismatch_req, origin = 'fixture')
  e$sess_set_pending(root, b$id, mismatch_req, slot = 'step')
  step_before <- e$remote_steps_read(b$id)
  r <- call('/api/step/import', list(
    session_id = b$id, req_id = mismatch_req, step = first_stage,
    status = 'failed', run_dir = run_a, log = 'must not import'))
  check('step import rejects a different record run_dir before any writeback',
        r$code == 409 && identical(r$body$code, 'record_run_dir_mismatch') &&
        identical(r$body$run_dir, run_b) &&
        identical(e$remote_steps_read(b$id), step_before))

  dbempty_req <- 'req-record-context-dbempty'
  e$attempt_register(root, db_empty$id, dbempty_req, origin = 'fixture')
  e$sess_set_pending(root, db_empty$id, dbempty_req, slot = 'step')
  dbempty_steps_before <- e$remote_steps_read(db_empty$id)
  r <- call('/api/step/import', list(
    session_id = db_empty$id, req_id = dbempty_req, step = first_stage,
    status = 'failed', run_dir = run_a, log = 'must not fall back to JSON run_dir'))
  check('step import fails closed when SQLite run_dir is empty even if JSON has one',
        r$code == 409 && identical(r$body$code, 'record_run_dir_missing') &&
        identical(e$remote_steps_read(db_empty$id), dbempty_steps_before))

  r <- call('/api/stop', list(session_id = b$id))
  check('server-mode target stop never stops current A',
        r$code == 409 && identical(r$body$code, 'stop_not_supported_server') &&
        identical(e$sess_current_id(root), current_before))
  r <- call('/api/review', list(session_id = b$id, run_dir = run_a, stage = first_stage))
  check('review rejects another record run_dir before writing quality',
        r$code == 409 && identical(r$body$code, 'review_run_dir_mismatch') &&
        identical(e$sess_current_id(root), current_before))

  a_mtime <- file.info(run_a)$mtime
  r <- call(paste0('/api/status?session=', empty$id))
  check('missing run_dir fails closed on status without using A', r$code == 409 &&
        identical(r$body$code, 'record_run_dir_missing') &&
        identical(file.info(run_a)$mtime, a_mtime))
  r <- call('/api/run', list(session_id = empty$id, step = first_stage))
  check('missing run_dir fails closed on run without enqueueing', r$code == 409 &&
        identical(r$body$code, 'record_run_dir_missing') &&
        identical(if (file.exists(e$EXEC_FILE)) readLines(e$EXEC_FILE, warn = FALSE) else character(), exec_before))

  ctx_a <- e$record_context(e$record_capability(e$DB_CON, root, a$id,
                                                list(key = owner_a, kind = 'fixture')))
  ctx_b <- e$record_context(e$record_capability(e$DB_CON, root, b$id,
                                                list(key = owner_a, kind = 'fixture')))
  check('record context keeps local/server mode and run_dir per record',
        isTRUE(ctx_a$ok) && identical(ctx_a$mode, 'local') && identical(ctx_a$run_dir, run_a) &&
        isTRUE(ctx_b$ok) && identical(ctx_b$mode, 'server') && identical(ctx_b$run_dir, run_b) &&
        identical(ctx_b$ref_path, '/refs/B.xlsx'))

  vjson_cap <- e$record_capability(e$DB_CON, root, vjson$id,
                                   list(key = owner_a, kind = 'fixture'))
  check('version_id never falls back to the JSON session record',
        isTRUE(vjson_cap$ok) && identical(vjson_cap$version_id, ''))

  e$record_artifact_index_write(root, a$id, list(items = list(list(rel = 'A_ONLY.png'))))
  e$record_artifact_index_write(root, b$id, list(items = list(list(rel = 'B_ONLY.png'))))
  ia <- e$record_artifact_index_read(root, a$id)
  ib <- e$record_artifact_index_read(root, b$id)
  check('artifact indexes are session-scoped and do not share rows',
        identical(ia$session_id, a$id) && identical(ib$session_id, b$id) &&
        identical(ia$items[[1]]$rel, 'A_ONLY.png') &&
        identical(ib$items[[1]]$rel, 'B_ONLY.png') &&
        !file.exists(file.path(e$STATE_DIR, 'artifacts.json')))

  e$exec_enqueue('queue-A', session_id = a$id, req_id = 'req-A')
  e$exec_enqueue('queue-B', session_id = b$id, req_id = 'req-B')
  e$exec_enqueue('queue-S', session_id = s$id, req_id = 'req-S')
  e$exec_enqueue('queue-db-empty', session_id = db_empty$id, req_id = 'req-DE')
  due <- call('/api/exec/due')
  due_sids <- unique(vapply(due$body$items %||% list(), function(x) as.character(x$session_id %||% ''), ''))
  check('worker due keeps the full queue across records',
        due$code == 200 && all(c(a$id, b$id, s$id) %in% due_sids))
  due_db_empty <- Filter(function(x) identical(as.character(x$session_id %||% ''),
                                                db_empty$id), due$body$items %||% list())
  check('worker due does not backfill run_dir from the JSON record',
        length(due_db_empty) == 1L && identical(as.character(due_db_empty[[1]]$run_dir %||% ''),
                                                 ''))
  all_exec <- call('/api/exec')
  all_sids <- unique(vapply(all_exec$body$items %||% list(), function(x) as.character(x$session_id %||% ''), ''))
  check('worker exec view keeps the full queue across records',
        all_exec$code == 200 && all(c(a$id, b$id, s$id) %in% all_sids))
  view_b <- call(paste0('/api/exec/view?session=', b$id))
  view_b_sids <- unique(vapply(view_b$body$items %||% list(), function(x) as.character(x$session_id %||% ''), ''))
  check('exec/view is a read-capability single-record view',
        view_b$code == 200 && identical(view_b$body$session_id, b$id) &&
        identical(view_b_sids, b$id))
  view_s <- call(paste0('/api/exec/view?session=', s$id), credential = token_a)
  check('exec/view allows shared read without write capability',
        view_s$code == 200 && identical(view_s$body$session_id, s$id) &&
        isTRUE(view_s$body$can_read) && !isTRUE(view_s$body$can_write))

  # Reverse mode: current is A=server, target B is local. The local branch must
  # use B's run_dir and notebook binding, while current and A stay untouched.
  e$sess_switch(root, a_server$id)
  dir.create(file.path(run_b_local, 'status'), recursive = TRUE, showWarnings = FALSE)
  e$write_json_file(file.path(run_b_local, 'status', 'step_state.json'),
                    list(step = 'B_LOCAL_STEP', status = 'idle'))
  writeLines('B_LOCAL_ONLY', file.path(run_b_local, 'status', 'run.log'))
  e$HAS_NOTEBOOK <- TRUE
  source('engine/notebook_build.R', local = e)
  e$nb_bind_local(run_a, root, a$id)
  a_nb_binding <- readBin(file.path(run_a, 'status', 'notebook_session.json'),
                          'raw', n = 100000L)
  launched <- NULL
  e$launch_step <- function(step, run_dir) {
    launched <<- list(step = step, run_dir = run_dir)
    list(ok = TRUE, started = step)
  }
  e$HAS_REFERENCE <- FALSE
  r <- call(paste0('/api/run?session=', b_local$id),
            list(session_id = b_local$id, step = first_stage))
  b_nb <- tryCatch(jsonlite::fromJSON(
    file.path(run_b_local, 'status', 'notebook_session.json'), simplifyVector = FALSE),
    error = function(e) NULL)
  a_nb_after <- readBin(file.path(run_a, 'status', 'notebook_session.json'),
                        'raw', n = 100000L)
  check('local run binds B sid/run_dir for notebook and never A',
        r$code == 200 && identical(r$body$session_id, b_local$id) &&
        !is.null(b_nb) && identical(b_nb$session_id, b_local$id) &&
        identical(normalizePath(b_nb$run_dir), normalizePath(run_b_local)) &&
        identical(a_nb_binding, a_nb_after) &&
        identical(launched$run_dir, run_b_local) &&
        identical(e$sess_current_id(root), a_server$id))
  e$HAS_NOTEBOOK <- FALSE
  r <- call(paste0('/api/status?session=', b_local$id))
  check('reverse A=server/B=local status follows B local branch and content',
        r$code == 200 && identical(r$body$session_id, b_local$id) &&
        !isTRUE(r$body$remote) && identical(r$body$run_dir, run_b_local) &&
        isTRUE(grepl('B_LOCAL_ONLY', r$body$log, fixed = TRUE)) &&
        identical(e$sess_current_id(root), a_server$id))
  e$sess_switch(root, a$id)

  # Reference gate must use B's record place, never a global/current ref.
  source('engine/reference.R', local = e)
  e$HAS_REFERENCE <- TRUE
  e$supplement_needs_reference <- function(step) TRUE
  e$ref_list <- function() list()
  e$flow_execution <- function(stage, V) list(ok = TRUE, script = 'fixture.sh', argv = c('x'))
  e$flow_stage <- function(id, prof = NULL) list(id = id, title = id, skill = 'fixture')
  ref_yes <- e$sess_create(root, 'REF_YES', list(mode = 'server', server = list(
    host = 'fixture-host', work_root = '/remote/ref-yes', ref_mode = 'path',
    ref_path = '/refs/B.xlsx')), activate = FALSE)
  ref_no <- e$sess_create(root, 'REF_NO', list(mode = 'server', server = list(
    host = 'fixture-host', work_root = '/remote/ref-no', ref_mode = '', ref_path = '')), activate = FALSE)
  db_add(ref_yes, owner_a, 'private', 'vRY')
  db_add(ref_no, owner_a, 'private', 'vRN')
  global_place <- file.path(root, 'config', 'server_profile.json')
  dir.create(dirname(global_place), recursive = TRUE, showWarnings = FALSE)
  e$write_json_file(global_place, list(mode = 'server', server = list(
    host = 'global-host', ref_mode = 'none', ref_path = '')))
  queue_before_ref <- e$exec_read()$items %||% list()
  r <- call('/api/run', list(session_id = ref_yes$id, step = first_stage))
  ref_queue <- Filter(function(x) identical(as.character(x$session_id %||% ''), ref_yes$id),
                      e$exec_read()$items %||% list())
  check('target record reference path is selected and carried into queue',
        r$code == 200 && length(ref_queue) == 1L &&
        identical(as.character(ref_queue[[1]]$run_dir), '/remote/ref-yes') &&
        identical(as.character(ref_queue[[1]]$ref_mode), 'path') &&
        identical(as.character(ref_queue[[1]]$ref_path), '/refs/B.xlsx') &&
        nzchar(as.character(ref_queue[[1]]$reference_id)) &&
        identical(as.character(r$body$reference_id), as.character(ref_queue[[1]]$reference_id)) &&
        identical(as.character(r$body$ref_path), '/refs/B.xlsx'))
  due_ref <- call('/api/exec/due')
  due_ref_item <- Filter(function(x) identical(as.character(x$session_id %||% ''), ref_yes$id),
                          due_ref$body$items %||% list())
  check('worker due carries the stored B run_dir/reference binding',
        due_ref$code == 200 && length(due_ref_item) == 1L &&
        identical(as.character(due_ref_item[[1]]$run_dir), '/remote/ref-yes') &&
        identical(as.character(due_ref_item[[1]]$reference_id), as.character(ref_queue[[1]]$reference_id)) &&
        identical(as.character(due_ref_item[[1]]$ref_path), '/refs/B.xlsx'))
  e$write_json_file(global_place, list(mode = 'server', server = list(
    host = 'global-host', ref_mode = 'path', ref_path = '/refs/GLOBAL_OLD.xlsx')))
  queue_no_before <- e$exec_read()$items %||% list()
  r <- call('/api/run', list(session_id = ref_no$id, step = first_stage))
  queue_no_after <- e$exec_read()$items %||% list()
  check('target record without a ref still yields need_reference and no queue item',
        r$code == 409 && identical(r$body$code, 'need_reference') &&
        identical(queue_no_before, queue_no_after))

  # Artifact preview and generated indexes must be scoped to explicit B.
  e$HAS_MIRROR <- TRUE
  e$mirror_dir <- function(sid) file.path(root, 'mirror', as.character(sid))
  e$mirror_path <- function(sid, rel) file.path(e$mirror_dir(sid), rel)
  e$mirror_table_preview <- function(fp) list(ok = TRUE, rows = list(readLines(fp, warn = FALSE)))
  dir.create(file.path(e$mirror_dir(a$id), 'tables'), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(e$mirror_dir(b$id), 'tables'), recursive = TRUE, showWarnings = FALSE)
  writeLines('A_ONLY', file.path(e$mirror_dir(a$id), 'tables', 'same.tsv'))
  writeLines('B_ONLY', file.path(e$mirror_dir(b$id), 'tables', 'same.tsv'))
  r <- call(paste0('/api/artifact/preview?session=', b$id, '&rel=tables/same.tsv'))
  check('artifact preview resolves B mirror even when A has the same name',
        r$code == 200 && isTRUE(grepl('B_ONLY', paste(unlist(r$body$rows), collapse = '\n'), fixed = TRUE)) &&
        !grepl('A_ONLY', paste(unlist(r$body$rows), collapse = '\n'), fixed = TRUE))

  e$HAS_PPT_BUILD <- TRUE
  e$ppt_build_from_mirror <- function(engine_dir, mroot, out_rel, ...) {
    list(ok = TRUE, out_rel = out_rel, bytes = 123L, missing_dirs = list())
  }
  e$zip_bin <- function() Sys.which('zip')
  a_index_before <- e$record_artifact_index_read(root, a$id)
  dir.create(file.path(e$mirror_dir(b$id), '10.figs'), recursive = TRUE, showWarnings = FALSE)
  writeLines('png', file.path(e$mirror_dir(b$id), '10.figs', 'B.png'))
  r <- call('/api/ppt/build', list(session_id = b$id))
  b_index_ppt <- e$record_artifact_index_read(root, b$id)
  check('PPT build reads B mirror and writes only B artifact index',
        r$code == 200 && identical(r$body$session_id, b$id) &&
        any(vapply(b_index_ppt$items %||% list(), function(x)
          identical(as.character(x$rel %||% ''), '11.report/analysis_report.pptx'), logical(1))) &&
        identical(e$record_artifact_index_read(root, a$id), a_index_before))
  r <- call('/api/pack/build', list(session_id = b$id, out_rel = '11.report/analysis_results.zip'))
  b_index_pack <- e$record_artifact_index_read(root, b$id)
  check('pack build uses B mirror and writes only B artifact index',
        r$code == 200 && identical(r$body$session_id, b$id) &&
        any(vapply(b_index_pack$items %||% list(), function(x)
          identical(as.character(x$rel %||% ''), '11.report/analysis_results.zip'), logical(1))) &&
        identical(e$record_artifact_index_read(root, a$id), a_index_before))

  check('all target reads leave current selection unchanged',
        identical(e$sess_current_id(root), current_before))
  if (length(fail)) stop(paste(length(fail), 'record context failures'), call. = FALSE)
  cat('PASS: record context contract\n')
}
run_contract()
