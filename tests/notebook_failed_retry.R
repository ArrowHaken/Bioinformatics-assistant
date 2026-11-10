#!/usr/bin/env Rscript
source('engine/notebook_build.R')
root <- tempfile('real-notebook-retry-', tmpdir = '/tmp'); dir.create(root)
engine <- file.path(root, 'engine'); dir.create(engine)
for (f in c('run_chain.R', 'run_notebook.R', 'notebook_build.R', 'run_lifecycle.R')) stopifnot(file.copy(file.path('engine', f), engine))
rd <- file.path(root, 'run'); dir.create(rd); nb_bind_local(rd, root, 'session')
writeLines('cat("previous success\\n")', file.path(root, 'ok.R'))
writeLines(c("CHAIN_ORDER <- 'phase'", "resolve_step_cli <- function(...) { if(file.exists(file.path(RUN_DIR,'fail-resolve'))) stop('fixture resolution failure'); list(title='phase',cmd='Rscript',args=file.path(dirname(ENGINE_DIR),'ok.R'),cwd=RUN_DIR) }"), file.path(engine, 'step_plan.R'))
run <- function() system2('Rscript', c(file.path(engine, 'run_chain.R'), '--run-dir', rd, '--engine-dir', engine, '--only', 'phase'), stdout = TRUE, stderr = TRUE)
first <- run(); stopifnot(is.null(attr(first, 'status')))
session <- list(id = 'session', mode = 'local', run_dir = rd); stopifnot(nb_sync_local(root, session))
file.create(file.path(rd, 'fail-resolve')); failed <- run()
stopifnot(!is.null(attr(failed, 'status')), attr(failed, 'status') != 0L)
report <- jsonlite::fromJSON(file.path(rd, 'status/run_notebook.json'))
state <- jsonlite::fromJSON(file.path(rd, 'status/step_state.json'))
lines <- readLines('serve.R', warn = FALSE)
a <- grep('^  if \\(p == "/api/notebook/cells"', lines); b <- grep('^  if \\(p == "/api/runs"', lines)[1]
handler <- eval(parse(text = c('function(p,req,q,con) {', lines[a:(b-1)], '}'))[[1]])
APP_DIR <- root; HAS_NOTEBOOK <- TRUE; DB_READY <- FALSE; DB_CON <- NULL
owner_ctx <- list(key = 'fixture'); sess_find <- function(...) session
db_api_notebook_authorized <- function(...) TRUE; cur_sid <- function() 'session'
send_json <- function(con, status, body) list(status = status, body = body)
con <- rawConnection(raw(), 'wb')
response <- handler('/api/notebook/download', list(method = 'GET'), list(session = 'session'), con)
http <- rawToChar(rawConnectionValue(con)); close(con)
stopifnot(state$status == 'failed', !report$execution_succeeded)
if (nzchar(http)) {
 i <- regexpr('\r\n\r\n', http, fixed = TRUE)
 nb <- jsonlite::fromJSON(substring(http, i+4L), simplifyVector = FALSE)
 stopifnot(!nb$metadata$pa$capture$submitted_execution_succeeded,
   !grepl('previous success', http, fixed = TRUE))
} else stopifnot(response$status %in% c(404L, 409L))
stopifnot(!file.exists(file.path(nb_store_dir(root, 'session'), 'reproducible.ipynb')))
cat('PASS actual run_chain success then resolve failure: failed/2, process nonzero; real download refuses stale success\n')
cat('Fixtures:', root, '\n')
