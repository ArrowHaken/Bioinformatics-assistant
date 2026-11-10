# Run from app/: Rscript tests/db_api_host_auth_boundary.R
# Definitions only, temporary fixture, no live server/state/gateway/token access.
run_contract <- function() {
  e <- new.env(parent = baseenv())
  # Real owner mapping and request parser, without sourcing serve.R startup.
  for (file in c('engine/db.R', 'serve.R')) {
    wanted <- if (file == 'engine/db.R') 'owner_from_identity' else 'read_request'
    for (x in parse(file)) {
      if (is.call(x) && identical(x[[1]], as.name('<-')) &&
          identical(x[[2]], as.name(wanted))) eval(x, e)
    }
  }
  source('engine/db_api.R', local = e)
  e$DB_READY <- TRUE
  e$db_now <- function() 'fixture-time'
  root <- tempfile('host-auth-boundary-')
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  failures <- character()
  check <- function(label, ok) {
    cat(if (isTRUE(ok)) 'PASS:' else 'FAIL:', label, '\n')
    if (!isTRUE(ok)) failures <<- c(failures, label)
  }
  parse_request <- function(host = NULL, extra = character()) {
    wire <- paste0('GET /api/me HTTP/1.1\r\n',
      if (!is.null(host)) paste0('Host: ', host, '\r\n') else '',
      if (length(extra)) paste0(paste(extra, collapse = '\r\n'), '\r\n') else '',
      '\r\n')
    con <- rawConnection(charToRaw(wire), 'rb')
    on.exit(close(con))
    e$read_request(con)
  }
  hosts <- c('0.0.0.0:18980', '127.0.0.1:18980', 'localhost:18980',
             '[::1]:18980', 'artifact.catsco.cc', '127.0.0.1.attacker.test',
             'LOCALHOST:18980', '')
  for (host in hosts) {
    owner <- e$db_api_owner(root, parse_request(host))
    check(paste('unauthenticated Host cannot grant identity:', host), is.na(owner$key))
  }
  check('missing Host cannot grant identity', is.na(e$db_api_owner(root, parse_request())$key))
  owner <- e$db_api_owner(root, parse_request('0.0.0.0:18980'))
  response <- e$db_api_handle(TRUE, root, '/api/me', parse_request('0.0.0.0:18980'),
                             list(), function() list(), NULL, owner)
  check('/api/me returns unidentified with no records for forged Host',
        identical(response$body$identified, FALSE) &&
        identical(response$body$code, 'identity_unavailable') &&
        length(response$body$records) == 0L)
  for (extra in list(
      'Forwarded: for=127.0.0.1;host=localhost;proto=http',
      c('X-Forwarded-For: 127.0.0.1', 'X-Forwarded-Host: localhost'),
      c('X-Real-IP: 127.0.0.1', 'Remote-Addr: 127.0.0.1'))) {
    check(paste('untrusted proxy headers cannot grant identity:', extra[1]),
          is.na(e$db_api_owner(root, parse_request('0.0.0.0:18980', extra))$key))
  }
  req <- parse_request('127.0.0.1:18980')
  req$remote_addr <- '127.0.0.1'
  req$REMOTE_ADDR <- '127.0.0.1'
  check('loopback-looking peer fields do not prove direct trusted transport',
        is.na(e$db_api_owner(root, req)$key))
  check('serve parser exposes only method/target/headers/body, no socket peer',
        identical(names(parse_request('localhost')), c('method', 'target', 'headers', 'body')))

  # Token branch compatibility is tested with in-memory stubs, never mint/check real tokens.
  e$agent_token_from_req <- function(req) req$fixture_token
  e$agent_token_check <- function(app_dir, tok) {
    if (identical(tok, 'fixture-valid'))
      list(ok = TRUE, owner_key = 'viewer:fixture-only', token_id = 'fixture-id')
    else list(ok = FALSE, code = 'fixture_invalid')
  }
  touches <- 0L
  e$agent_token_touch <- function(...) { touches <<- touches + 1L }
  req$fixture_token <- 'fixture-invalid'
  owner <- e$db_api_owner(root, req)
  check('invalid agent token never falls back to local identity',
        is.na(owner$key) && identical(owner$kind, 'agent_token_invalid') && touches == 0L)
  req$fixture_token <- 'fixture-valid'
  owner <- e$db_api_owner(root, req)
  check('valid fixture agent token retains its exact owner',
        identical(owner$key, 'viewer:fixture-only') && identical(owner$kind, 'agent') && touches == 1L)
  check('fixture remains empty: no state/token writes', length(list.files(root, all.files = TRUE, no.. = TRUE)) == 0L)
  if (length(failures)) stop(length(failures), ' boundary contract failures', call. = FALSE)
  cat('Host authentication boundary contract passed\n')
}
run_contract()
