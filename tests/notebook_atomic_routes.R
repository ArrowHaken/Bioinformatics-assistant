#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))
source('engine/notebook_build.R')
root <- tempfile('notebook-contract-', tmpdir = '/tmp'); dir.create(root)
d <- file.path(root, 'ledger')
x <- list(code = 'cat(1)', lang = 'R', exit_code = 0L)
stopifnot(nb_append_cell(d, x, 'evt1'), nb_append_cell(d, x, 'evt1'), length(nb_read_cells(d)) == 1L)
stopifnot(!nb_append_cell(d, list(code = 'cat(2)', lang = 'R'), 'evt1'))
for (bad in list(list(code = 1), list(level = 2.5), list(exit_code = '0'), list(inputs = list(1)), list(event_id = ''), list(lang = 'sql')))
  stopifnot(!nb_append_cell(d, bad))
# Independent real forked R processes serialize the shared ledger and event retry.
res <- parallel::mclapply(1:24, function(i) nb_append_cell(d, x, paste0('event-', i)), mc.cores = 2)
stopifnot(all(unlist(res)), length(nb_read_cells(d)) == 25L,
 identical(vapply(nb_read_cells(d), function(x) as.integer(x$seq), 0L), 1:25))
res <- parallel::mclapply(1:12, function(i) nb_append_cell(d, x, 'shared'), mc.cores = 2)
stopifnot(all(unlist(res)), length(nb_read_cells(d)) == 26L)
f <- nb_cells_file(d); cat('{broken\n', file = f, append = TRUE); before <- readBin(f, 'raw', file.info(f)$size)
stopifnot(!nb_append_cell(d, x, 'new'), identical(before, readBin(f, 'raw', file.info(f)$size)), !nb_build(d)$ok)
stopifnot(length(nb_check_paths(list(list(code = 'readRDS("/share/input.rds")', step = '03')))) == 1L,
 length(nb_check_paths(list(list(code = 'readRDS("/share/input.rds")', step = '02')))) == 0L)
cat('PASS ledger: concurrent atomic writes, duplicate retry, conflict, schema, corrupt-file preservation, raw path detection\n')
# Extract only notebook route bodies; never source server initialization or real state/token code.
lines <- readLines('serve.R', warn = FALSE)
a <- grep('^  if \\(p == "/api/notebook/cells"', lines)
b <- grep('^  if \\(p == "/api/runs"', lines)[1]
route <- parse(text = c('function(p, req, q, con) {', lines[a:(b-1L)], '}'))[[1]]
APP_DIR <- root; HAS_NOTEBOOK <- TRUE; DB_READY <- FALSE; DB_CON <- NULL
sessions <- list(remote = list(id = 'remote', name = 'Remote', mode = 'server'), local = list(id = 'local', name = 'Local', mode = 'local'))
sess_find <- function(app, sid) sessions[[sid]]
owner_ctx <- list(kind = 'agent', key = 'fixture')
allow <- TRUE
db_api_notebook_authorized <- function(...) allow
cur_sid <- function() 'remote'
send_json <- function(con, status, body) list(status = status, body = body)
body <- NULL; body_json <- function() body
handler <- eval(route)
post <- function(path, value) { body <<- value; handler(path, list(method = 'POST'), list(), NULL) }
response <- post('/api/notebook/cells', list(session_id = 'remote', cell = x, event_id = 'a', final = TRUE))
stopifnot(response$status == 200L, file.exists(response$body$path), !response$body$capture_complete,
 !response$body$full_analysis_replay_verified, response$body$submitted_execution_succeeded)
response <- post('/api/notebook/cells', list(session_id = 'remote', cell = x, event_id = 'a', final = TRUE))
stopifnot(response$status == 200L, length(nb_read_cells(nb_store_dir(root, 'remote'))) == 1L)
response <- post('/api/notebook/cells', list(session_id = 'remote', cell = list(code = 'cat(2)', lang = 'R'), event_id = 'a'))
stopifnot(response$status == 400L)
allow <- FALSE
stopifnot(post('/api/notebook/cells', list(session_id = 'remote', cell = x))$status == 403L,
 post('/api/notebook/build', list(session_id = 'remote'))$status == 403L,
 handler('/api/notebook/download', list(method = 'GET'), list(session = 'remote'), NULL)$status == 403L)
allow <- TRUE
response <- post('/api/notebook/cells', list(session_id = 'remote', cell = list(title = 'Missing script', exit_code = 0L), final = TRUE))
stopifnot(response$status == 200L, !response$body$capture_complete, !response$body$submitted_execution_succeeded)
response <- post('/api/notebook/cells', list(session_id = 'remote', cell = list(code = 'stop("failed")', lang = 'R', exit_code = 1L), build = TRUE))
stopifnot(response$status == 200L, !response$body$capture_complete, !response$body$submitted_execution_succeeded)
sessions$empty <- list(id = 'empty', mode = 'server')
stopifnot(post('/api/notebook/cells', list(session_id = 'empty', cell = list(title = 'No code'), final = TRUE))$status == 400L)
cat('PASS HTTP handlers: final/build auto assembly, retry/conflict, owner rejection, missing and failed code never complete\n')
# Bound local capture selects explicit latest phase attempts.
source('engine/run_notebook.R')
rd <- file.path(root, 'run'); dir.create(rd)
sessions$local$run_dir <- rd
stopifnot(nb_bind_local(rd, root, 'local'))
stopifnot(inherits(tryCatch(nb_bind_local(rd, root, 'other'), error = identity), 'error'))
engine <- normalizePath('engine')
for (step in c('phase-a', 'phase-b')) {
 s <- rn_init(rd, engine, step, 'single')
 rn_append(s, list(step = step, title = step, code = paste0('cat("', step, '")'), lang = 'R', exit_code = 0L))
 rn_finish(s, FALSE)
}
stopifnot(nb_sync_local(root, sessions$local))
ledger <- nb_read_cells(nb_store_dir(root, 'local'))
stopifnot(all(c('phase-a', 'phase-b') %in% vapply(ledger, function(x) x$step %||% '', '')))
# Download is actual raw HTTP header+body serialization, evaluated against the real route.
out <- rawConnection(raw(), 'wb')
handler('/api/notebook/download', list(method = 'GET'), list(session = 'local'), out)
http <- rawConnectionValue(out); close(out)
text <- rawToChar(http)
stopifnot(startsWith(text, 'HTTP/1.1 200 OK'), grepl('phase-a', text), grepl('phase-b', text),
 grepl('full_analysis_replay_verified', text))
header <- regexpr('\r\n\r\n', text, fixed = TRUE)
notebook <- fromJSON(substring(text, header + 4L), simplifyVector = FALSE)
stopifnot(!notebook$metadata$pa$full_analysis_replay_verified, !notebook$metadata$pa$capture$capture_complete)
sessions$wrong <- list(id = 'wrong', mode = 'local', run_dir = rd)
stopifnot(handler('/api/notebook/download', list(method = 'GET'), list(session = 'wrong'), NULL)$status == 409L)
# Remote directory is never imported even if its path exists locally.
sessions$remote$run_dir <- rd; stopifnot(!nb_sync_local(root, sessions$remote))
cat('PASS HTTP download: safe explicit local binding, two indexed phases preserved, cross-session and remote-path rejection\n')
cat('Fixture:', root, '\n')

# Real loopback HTTP fixture, bounded to two connections and stopped before exit.
port <- sample(21000:45000, 1)
listener <- serverSocket(port)
worker <- parallel::mcparallel({
 send_json <- function(con, status, body) {
   txt <- as.character(toJSON(body, auto_unbox = TRUE, null = "null"))
   writeBin(charToRaw(paste0("HTTP/1.1 ", status, " Fixture\r\nContent-Type: application/json\r\nContent-Length: ", nchar(txt, type = "bytes"), "\r\nConnection: close\r\n\r\n", txt)), con)
   flush(con)
 }
 for (request_i in 1:2) {
   con <- socketAccept(listener, blocking = TRUE, open = "a+b", timeout = 10)
   first <- readLines(con, n = 1L, warn = FALSE)
   headers <- character()
   repeat { line <- readLines(con, n = 1L, warn = FALSE); if (!length(line) || !nzchar(line)) break; headers <- c(headers, line) }
   len <- grep("^Content-Length:", headers, value = TRUE, ignore.case = TRUE)
   if (length(len)) body <- fromJSON(rawToChar(readBin(con, "raw", as.integer(sub("^[^:]+: *", "", len[1])))), simplifyVector = FALSE)
   if (startsWith(first, "POST")) handler('/api/notebook/cells', list(method = 'POST'), list(), con)
   else handler('/api/notebook/download', list(method = 'GET'), list(session = 'local'), con)
   close(con)
 }
 close(listener)
 TRUE
})
post_file <- file.path(root, 'post.json')
writeLines(as.character(toJSON(list(session_id = 'remote', cell = x, event_id = 'http-event', final = TRUE), auto_unbox = TRUE)), post_file)
response <- system2('curl', c('-sS', '--max-time', '10', '-H', shQuote('Content-Type: application/json'), '--data-binary', paste0('@', post_file), paste0('http://127.0.0.1:', port, '/api/notebook/cells')), stdout = TRUE)
stopifnot(fromJSON(paste(response, collapse = ''))$ok)
download <- file.path(root, 'http-download.ipynb')
status <- system2('curl', c('-sS', '--max-time', '10', '-o', download, paste0('http://127.0.0.1:', port, '/api/notebook/download?session=local')))
stopifnot(status == 0L, fromJSON(download)$nbformat == 4L)
stopifnot(isTRUE(parallel::mccollect(worker)[[1]])); close(listener)
cat('PASS real HTTP loopback: POST final builds and GET download parses as notebook\n')
