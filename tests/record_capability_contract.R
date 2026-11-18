# Run from app/: Rscript tests/record_capability_contract.R
# Temporary app tree + SQLite only; no production state, queue, mirror, or process.
run_contract <- function() {
  `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
  source('engine/db.R', local = TRUE)
  source('engine/sessions.R', local = TRUE)
  source('engine/record_access.R', local = TRUE)

  root <- tempfile('record-capability-')
  dir.create(file.path(root, 'state'), recursive = TRUE)
  con <- db_connect(root)
  on.exit({
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    unlink(root, recursive = TRUE)
  }, add = TRUE)
  db_init(con)

  owner_a <- 'viewer:cap-a'
  owner_c <- 'viewer:cap-c'
  run_a <- file.path(root, 'run-a'); dir.create(run_a)
  run_b <- file.path(root, 'run-b'); dir.create(run_b)
  run_s <- file.path(root, 'run-shared'); dir.create(run_s)
  place_a <- list(mode = 'local', local = list(path = run_a), server = list())
  place_b <- list(mode = 'local', local = list(path = run_b), server = list())
  place_s <- list(mode = 'server', server = list(
    host = 'fixture-host', work_root = run_s, ref_mode = 'path', ref_path = '/refs/S.xlsx'))
  a <- sess_create(root, 'A', place_a, activate = TRUE)
  b <- sess_create(root, 'B', place_b, activate = FALSE)
  s <- sess_create(root, 'S', place_s, activate = FALSE)
  add <- function(rec, owner, visibility = 'private', version_id = '') {
    DBI::dbExecute(con,
      'INSERT INTO sessions(session_id,owner_key,name,mode,run_dir,visibility,version_id,created_at,updated_at) VALUES(?,?,?,?,?,?,?,?,?)',
      list(rec$id, owner, rec$name, rec$mode, rec$run_dir, visibility,
           version_id, rec$created_at, rec$updated_at))
  }
  add(a, owner_a, 'private', 'vA')
  add(b, owner_a, 'private', 'vB')
  add(s, owner_c, 'shared', 'vS')

  fail <- character()
  check <- function(label, value) {
    cat(if (isTRUE(value)) 'PASS:' else 'FAIL:', label, '\n')
    if (!isTRUE(value)) fail <<- c(fail, label)
  }
  ctx_a <- list(key = owner_a, kind = 'fixture')
  ctx_c <- list(key = owner_c, kind = 'fixture')
  cap_a <- record_capability(con, root, a$id, ctx_a)
  check('owner A can read and write A', isTRUE(cap_a$ok) && isTRUE(cap_a$can_read) &&
          isTRUE(cap_a$can_write) && !isTRUE(cap_a$readonly))
  cap_foreign <- record_capability(con, root, b$id, ctx_c)
  check('foreign private record is not readable', isTRUE(cap_foreign$ok) &&
          !isTRUE(cap_foreign$can_read) && !isTRUE(cap_foreign$can_write))
  cap_shared <- record_capability(con, root, s$id, ctx_a)
  check('shared record is read-only for A', isTRUE(cap_shared$ok) &&
          isTRUE(cap_shared$can_read) && !isTRUE(cap_shared$can_write) &&
          isTRUE(cap_shared$readonly))

  r <- record_access(con, root, b$id, ctx_c, need = 'read')
  check('foreign read is 403 record_forbidden', identical(r$status, 403L) &&
          identical(r$body$code, 'record_forbidden'))
  r <- record_access(con, root, s$id, ctx_a, need = 'write')
  check('shared write is 403 not_owner', identical(r$status, 403L) &&
          identical(r$body$code, 'not_owner'))

  c_a <- record_context(cap_a)
  c_s <- record_context(cap_shared)
  check('context uses target run_dir/mode', isTRUE(c_a$ok) &&
          identical(c_a$run_dir, run_a) && identical(c_a$mode, 'local'))
  check('server context uses target place/ref', isTRUE(c_s$ok) &&
          isTRUE(c_s$run_dir_remote) && identical(c_s$ref_path, '/refs/S.xlsx'))

  empty <- sess_create(root, 'empty', list(mode = 'local', local = list(path = '')), activate = FALSE)
  add(empty, owner_a, 'private', 'vEmpty')
  empty_cap <- record_capability(con, root, empty$id, ctx_a)
  empty_ctx <- record_context(empty_cap)
  check('empty run_dir fails closed', identical(empty_ctx$status, 409L) &&
          identical(empty_ctx$code, 'record_run_dir_missing'))
  bad_mode <- sess_create(root, 'bad-mode', list(mode = 'weird', local = list(path = run_b)), activate = FALSE)
  add(bad_mode, owner_a, 'private', 'vBad')
  bad_ctx <- record_context(record_capability(con, root, bad_mode$id, ctx_a))
  check('invalid mode fails closed', identical(bad_ctx$code, 'record_mode_invalid'))
  no_srv <- list(mode = 'server', server = list())
  srv_rec <- sess_create(root, 'no-server', list(mode = 'server', server = list()), activate = FALSE)
  srv_rec$mode <- 'server'; srv_rec$run_dir <- '/remote/empty'; srv_rec$place <- no_srv
  sess_update(root, srv_rec$id, list(mode = 'server', run_dir = '/remote/empty', place = no_srv))
  add(srv_rec, owner_a, 'private', 'vNoServer')
  srv_ctx <- record_context(record_capability(con, root, srv_rec$id, ctx_a))
  check('server mode without server place fails closed',
        identical(srv_ctx$code, 'record_server_place_missing'))

  before_current <- sess_current_id(root)
  before_sessions <- readLines(file.path(root, 'state', 'sessions.json'), warn = FALSE)
  before_db <- db_rows(con, 'SELECT * FROM sessions ORDER BY session_id')

  canon <- record_session_target(list(session = b$id), list(session_id = a$id), path = '/api/status')
  check('query/body sid conflict is 400', !isTRUE(canon$ok) && identical(canon$status, 400L) &&
          identical(canon$code, 'session_conflict'))
  canon <- record_session_target(list(), list(), path = '/api/status', current_sid = a$id,
                                 allow_current = TRUE, require = FALSE)
  check('read fallback may use current explicitly', isTRUE(canon$ok) && identical(canon$sid, a$id))
  canon <- record_session_target(list(), list(), path = '/api/config', current_sid = a$id,
                                 allow_current = FALSE, require = TRUE)
  check('write route requires explicit sid', !isTRUE(canon$ok) && identical(canon$code, 'session_required'))

  pf <- record_request_preflight(root, con, '/api/status', 'GET', list(session = b$id), list(),
                                 ctx_a, current_sid = a$id)
  check('record read preflight authorized for owner target', isTRUE(pf$ok) &&
          identical(pf$target$sid, b$id) && isTRUE(pf$access$cap$can_read))
  pf <- record_request_preflight(root, con, '/api/run', 'POST', list(session = s$id),
                                 list(session_id = s$id), ctx_a, current_sid = a$id)
  check('record write preflight rejects shared target', !isTRUE(pf$ok) && identical(pf$status, 403L))
  pf <- record_request_preflight(root, con, '/api/config', 'POST', list(session = b$id),
                                 list(session_id = a$id), ctx_a, current_sid = a$id)
  check('preflight conflict happens before handler', !isTRUE(pf$ok) && identical(pf$status, 400L) &&
          identical(pf$body$code, 'session_conflict'))

  pf <- record_request_preflight(root, con, '/api/deliverable', 'GET',
                                 list(version_id = 'vB'), list(), ctx_a, current_sid = a$id)
  check('version route maps version_id to target record', isTRUE(pf$ok) &&
          identical(pf$target$sid, b$id) && identical(pf$policy$scope, 'version'))
  pf <- record_request_preflight(root, con, '/api/version/status', 'GET',
                                 list(version_id = 'missing'), list(), ctx_a, current_sid = a$id)
  check('unknown version fails closed', !isTRUE(pf$ok) && identical(pf$status, 404L))
  pf <- record_request_preflight(root, con, '/api/deliverable', 'GET',
                                 list(version_id = 'vS'), list(), ctx_a, current_sid = a$id)
  check('version preflight uses shared read capability', isTRUE(pf$ok) &&
          identical(pf$target$sid, s$id) && !isTRUE(pf$access$cap$can_write))

  after_db <- db_rows(con, 'SELECT * FROM sessions ORDER BY session_id')
  check('denials leave current unchanged', identical(sess_current_id(root), before_current))
  check('denials leave sessions.json unchanged',
        identical(readLines(file.path(root, 'state', 'sessions.json'), warn = FALSE), before_sessions))
  check('denials leave SQLite sessions unchanged', identical(after_db, before_db))

  if (length(fail)) stop(paste(length(fail), 'record capability failures'), call. = FALSE)
  cat('PASS: record capability contract\n')
}
run_contract()
