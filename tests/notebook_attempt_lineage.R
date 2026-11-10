#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))
source('engine/notebook_build.R'); source('engine/run_notebook.R')
root <- tempfile('notebook-lineage-', tmpdir = '/tmp'); dir.create(root)
engine <- normalizePath('engine')
newrun <- function(id) {
 rd <- file.path(root, id); dir.create(rd); nb_bind_local(rd, root, id)
 list(id = id, mode = 'local', run_dir = rd)
}
record <- function(sess, step, code, success = TRUE) {
 s <- rn_init(sess$run_dir, engine, step, 'single')
 rn_record(s, list(step = step, title = step, command = 'fixture', argv = list(), cwd = root,
   exportable = TRUE, lang = 'R', code = code), 0L)
 rn_finish(s, success); s
}
codes <- function(sess) vapply(Filter(function(x) nzchar(x$code %||% ''),
 nb_read_cells(nb_store_dir(root, sess$id))), function(x) x$code, '')
sess <- newrun('repeat')
a <- record(sess, 'a', 'value <- 1L'); b <- record(sess, 'b', 'value <- value + 1L')
c <- record(sess, 'b', 'value <- value + 1L')
stopifnot(nb_sync_local(root, sess), length(codes(sess)) == 2L)
env <- new.env(); for (code in codes(sess)) eval(parse(text = code), env)
stopifnot(env$value == 2L)
selection <- nb_read_json(file.path(nb_store_dir(root, sess$id), 'status/local_selection.json'))
stopifnot(selection$submitted_execution_succeeded, selection$phases[[2]]$attempt_id == c$attempt_id,
 file.exists(nb_cells_file(a$attempt_dir)), file.exists(nb_cells_file(b$attempt_dir)))
cat('PASS repeated phase: exported code cells=2 replay value=2; prior attempts retained independently\n')
# Retry an upstream phase: old downstream is explicitly stale and never replayed.
record(sess, 'a', 'value <- 10L')
stopifnot(nb_sync_local(root, sess), length(codes(sess)) == 1L)
nb <- fromJSON(file.path(nb_store_dir(root, sess$id), 'reproducible.ipynb'), simplifyVector = FALSE)
stopifnot(!nb$metadata$pa$capture$submitted_execution_succeeded,
 nb$metadata$pa$capture$lineage$phases[[2]]$state == 'stale')
cat('PASS upstream retry: downstream excluded and lineage marked stale, success false\n')
# Failed/no-record retry replaces old success even if the runner never got to prepare.
sess <- newrun('failure'); record(sess, 'a', 'old_success <- TRUE'); nb_sync_local(root, sess)
s <- rn_init(sess$run_dir, engine, 'a', 'single'); rn_finish(s, FALSE)
stopifnot(!nb_sync_local(root, sess), !length(codes(sess)),
 !file.exists(file.path(nb_store_dir(root, sess$id), 'reproducible.ipynb')))
cat('PASS failed retry without code: no cached notebook and no old successful script\n')
# An unfinished invocation also cannot be presented as successful.
sess <- newrun('running'); record(sess, 'a', 'old_success <- TRUE'); nb_sync_local(root, sess)
s <- rn_init(sess$run_dir, engine, 'a', 'single')
stopifnot(!nb_sync_local(root, sess), !length(codes(sess)))
rn_append(s, list(step = 'a', code = 'new_in_progress <- TRUE', lang = 'R', exit_code = 0L))
stopifnot(nb_sync_local(root, sess))
nb <- fromJSON(file.path(nb_store_dir(root, sess$id), 'reproducible.ipynb'), simplifyVector = FALSE)
stopifnot(!nb$metadata$pa$capture$submitted_execution_succeeded); rn_finish(s, FALSE)
cat('PASS running attempt: old script removed, new diagnostic code never claims success\n')
# Touching pre-binding history cannot admit it; rounding new ledgers cannot exclude them.
rd <- file.path(root, 'mtime'); dir.create(rd)
nb_append_cell(rd, list(step = 'foreign', code = 'foreign_old <- TRUE', lang = 'R', exit_code = 0L))
nb_bind_local(rd, root, 'mtime'); Sys.setFileTime(nb_cells_file(rd), Sys.time() + 3600)
sess <- list(id = 'mtime', mode = 'local', run_dir = rd)
stopifnot(!nb_sync_local(root, sess), !length(codes(sess)))
s <- record(sess, 'fresh', 'new_valid <- TRUE')
Sys.setFileTime(nb_cells_file(s$attempt_dir), as.POSIXct(0, origin = '1970-01-01'))
stopifnot(nb_sync_local(root, sess), identical(codes(sess), 'new_valid <- TRUE'))
cat('PASS timestamps: touched pre-binding history excluded; epoch-mtime valid attempt included\n')
# Deterministic independent-process overlap; rejected process cannot mutate active capture.
sess <- newrun('concurrent'); gate <- file.path(root, 'ready'); release <- file.path(root, 'release')
a <- parallel::mcparallel({
 s <- rn_init(sess$run_dir, engine, 'a', 'single'); file.create(gate)
 deadline <- Sys.time() + 10
 while (!file.exists(release)) { if (Sys.time() > deadline) stop('gate timeout'); Sys.sleep(.01) }
 rn_append(s, list(step = 'a', code = 'from_A <- TRUE', lang = 'R', exit_code = 0L)); rn_finish(s, FALSE)
 s$attempt_dir
})
deadline <- Sys.time() + 10
while (!file.exists(gate)) { if (Sys.time() > deadline) stop('gate timeout'); Sys.sleep(.01) }
rejected <- tryCatch(rn_init(sess$run_dir, engine, 'b', 'single'), error = identity)
stopifnot(inherits(rejected, 'error'), grepl('already active', conditionMessage(rejected)))
file.create(release); attempt <- parallel::mccollect(a)[[1]]
raw <- nb_read_cells(attempt)
stopifnot(identical(vapply(Filter(function(x) nzchar(x$code %||% ''), raw), function(x) x$code, ''), 'from_A <- TRUE'))
s <- record(sess, 'b', 'from_B <- TRUE')
stopifnot(!any(vapply(nb_read_cells(s$attempt_dir), function(x) grepl('from_A', x$code %||% ''), TRUE)))
cat('PASS rn_init concurrency: overlapping process rejected; later invocation isolated\n')
cat('Fixtures:', root, '\n')
