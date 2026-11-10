#!/usr/bin/env Rscript
# Only definitions and the complete import route are evaluated; no server/token/state startup.
run <- function() {
  e <- new.env(parent = globalenv())
  source('engine/db.R', local = e)
  source('engine/sessions.R', local = e)
  source(Sys.getenv('INTENT_TEST_BRIDGE', 'engine/intent_bridge.R'), local = e)
  lines <- readLines(Sys.getenv('INTENT_TEST_SERVE', 'serve.R'), warn = FALSE)
  first <- grep('^  if \\(p == "/api/intent/import"', lines)
  last <- grep('^  if \\(p == "/api/sessions/new"', lines)
  e$route <- eval(parse(text = paste(c('function() {', lines[first:(last - 1L)], '}'), collapse = '\n')), e)
  e$HAS_INTENT <- e$HAS_SESSIONS <- e$DB_READY <- TRUE
  e$HAS_AGENT_GATE <- FALSE
  e$p <- '/api/intent/import'; e$req <- list(method = 'POST'); e$con <- NULL
  e$send_json <- function(con, status, body) list(status = status, body = body)
  e$db_api_notebook_authorized <- function(con, owner, sid) {
    x <- DBI::dbGetQuery(con, 'SELECT owner_key FROM sessions WHERE session_id=?', list(sid))
    nrow(x) == 1L && identical(x$owner_key[[1]], owner$key)
  }
  root <- tempfile('intent-import-', tmpdir = '/tmp'); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  e$APP_DIR <- root; e$DB_CON <- e$db_connect(root)
  on.exit(DBI::dbDisconnect(e$DB_CON), add = TRUE)
  e$db_init(e$DB_CON)
  dir.create(file.path(root, 'config'))
  writeLines('config sentinel', file.path(root, 'config/server_profile.json'))
  e$sess_write(root, list(current = 'untouched-selection', sessions = list(), extra = 'keep'))
  raw <- list(project_name = 'test project', mode = 'local', paths = list(work_root = '/isolated/work'),
    source = list(topic_id = 'topic', message_id = 'message'), species = 'human', tissue = 'kidney')
  e$body_json <- function() raw
  owner_a <- list(kind = 'agent', key = 'test-owner-a', external_id = 'test-only-no-token')
  owner_b <- list(kind = 'agent', key = 'test-owner-b', external_id = 'test-only-no-token')
  e$owner_ctx <- owner_a
  snapshot <- function() {
    paths <- sort(list.files(root, recursive = TRUE, all.files = TRUE, full.names = TRUE))
    paths <- paths[!grepl('/app\\.db(-wal|-shm)?$', paths)]
    bytes <- lapply(paths, function(p) readBin(p, 'raw', n = file.info(p)$size))
    names(bytes) <- substring(paths, nchar(root) + 2L)
    list(files = bytes, rows = DBI::dbGetQuery(e$DB_CON, 'SELECT * FROM sessions ORDER BY session_id'),
      dirs = sort(list.dirs(root, recursive = TRUE)))
  }
  expect_failure <- function(label) {
    before <- snapshot(); out <- e$route()
    if (!identical(out$status, 503L) && !identical(out$status, 503)) stop(label, ': expected 503')
    if (!identical(before, snapshot())) stop(label, ': changed state after failure')
    cat('PASS', label, '503 and byte-exact rollback\n')
  }
  DBI::dbExecute(e$DB_CON, "CREATE TRIGGER reject_import BEFORE INSERT ON sessions BEGIN SELECT RAISE(ABORT, 'test reject'); END")
  expect_failure('create DB trigger reject')
  DBI::dbExecute(e$DB_CON, 'DROP TRIGGER reject_import')
  original_writer <- e$intent_json_write
  inject <- function(target, after = FALSE, false = FALSE) {
    e$intent_json_write <- function(path, obj) {
      if (identical(basename(path), target)) {
        if (after) original_writer(path, obj)
        if (false) return(FALSE)
        stop('test injected file failure')
      }
      original_writer(path, obj)
    }
  }
  retry <- function(duplicate, sid = NULL) {
    e$intent_json_write <- original_writer
    out <- e$route(); stopifnot(out$status == 200, identical(out$body$duplicate, duplicate))
    if (!is.null(sid)) stopifnot(identical(out$body$session_id, sid))
    stopifnot(identical(e$sess_current_id(root), 'untouched-selection'))
    out$body$session_id
  }
  for (target in c('sessions.json', 'profile.json', 'intents.json')) {
    for (after in c(FALSE, TRUE)) {
      inject(target, after); expect_failure(paste('create', target, 'after rename', after))
    }
  }
  inject('intents.json', TRUE, TRUE); expect_failure('create false writer return')
  sid <- retry(FALSE)
  stopifnot(identical(retry(TRUE, sid), sid))
  raw$project_name <- 'updated'; raw$tissue <- 'updated tissue'
  DBI::dbExecute(e$DB_CON, "CREATE TRIGGER reject_update BEFORE UPDATE ON sessions BEGIN SELECT RAISE(ABORT, 'test reject'); END")
  expect_failure('duplicate DB trigger reject')
  DBI::dbExecute(e$DB_CON, 'DROP TRIGGER reject_update')
  for (target in c('sessions.json', 'profile.json', 'intents.json')) {
    for (after in c(FALSE, TRUE)) {
      inject(target, after); expect_failure(paste('duplicate', target, 'after rename', after))
    }
  }
  inject('profile.json', TRUE, TRUE); expect_failure('duplicate false writer return')
  e$intent_json_write <- original_writer
  original_tx <- e$db_tx
  e$db_tx <- function(con, fn) original_tx(con, function() { fn(); stop('test precommit failure') })
  expect_failure('duplicate transaction precommit failure')
  e$db_tx <- original_tx
  # Deferred FK violation occurs at DBI::dbCommit, after all JSON renames.
  DBI::dbExecute(e$DB_CON, 'CREATE TABLE import_parent(id INTEGER PRIMARY KEY)')
  DBI::dbExecute(e$DB_CON, 'CREATE TABLE import_child(id INTEGER REFERENCES import_parent(id) DEFERRABLE INITIALLY DEFERRED)')
  DBI::dbExecute(e$DB_CON, 'CREATE TRIGGER reject_commit AFTER UPDATE ON sessions BEGIN INSERT INTO import_child VALUES(999); END')
  expect_failure('duplicate actual SQLite commit rejection')
  stopifnot(nrow(DBI::dbGetQuery(e$DB_CON, 'SELECT * FROM import_child')) == 0L)
  DBI::dbExecute(e$DB_CON, 'DROP TRIGGER reject_commit')
  DBI::dbExecute(e$DB_CON, 'DROP TABLE import_child')
  DBI::dbExecute(e$DB_CON, 'DROP TABLE import_parent')
  retry(TRUE, sid)
  stopifnot(identical(e$sess_profile(root, sid)$tissue, 'updated tissue'))
  before_a <- readBin(e$sess_profile_f(root, sid), 'raw', n = file.info(e$sess_profile_f(root, sid))$size)
  e$owner_ctx <- owner_b
  sid_b <- retry(FALSE)
  stopifnot(sid_b != sid); retry(TRUE, sid_b)
  stopifnot(identical(before_a, readBin(e$sess_profile_f(root, sid), 'raw', n = length(before_a))))
  stopifnot(nrow(DBI::dbGetQuery(e$DB_CON, 'SELECT * FROM sessions')) == 2L,
    length(e$sess_read(root)$sessions) == 2L)
  cat('PASS retries stable, two owners isolated, no duplicate records\n')
  # A corrupt mapping pointing at another owner's existing record must remain forbidden.
  index <- e$intent_store_read(root); key_b <- paste0(owner_b$key, '|', e$intent_key(e$intent_normalize(raw)$intent))
  index$items[[key_b]]$session_id <- sid; e$intent_store_write(root, index)
  before <- snapshot(); out <- e$route()
  stopifnot(out$status == 403L, identical(snapshot(), before))
  index$items[[key_b]]$session_id <- sid_b; e$intent_store_write(root, index)
  for (path in c(e$sess_file(root), e$sess_profile_f(root, sid_b), e$intent_store_file(root))) {
    original <- readBin(path, 'raw', n = file.info(path)$size)
    for (bad in c('{broken', '[]', 'null')) {
      writeLines(bad, path); expect_failure(paste('corrupt state', basename(path), bad))
    }
    e$intent_atomic_bytes(path, original)
  }
  stopifnot(identical(readLines(file.path(root, 'config/server_profile.json')), 'config sentinel'))
  cat('PASS authorization, corruption fail-closed, config and selection unchanged\n')
}
run()
