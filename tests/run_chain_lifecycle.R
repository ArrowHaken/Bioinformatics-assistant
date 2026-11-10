#!/usr/bin/env Rscript
# Only disposable fixtures; no production sessions, service restart or Git.
suppressPackageStartupMessages(library(jsonlite))
stopifnot(requireNamespace('processx', quietly = TRUE))
source('engine/run_lifecycle.R')
root <- tempfile('runner-lifecycle-', tmpdir = '/tmp'); dir.create(root)
engine <- file.path(root, 'engine'); dir.create(engine)
for (f in c('run_chain.R','run_notebook.R','notebook_build.R','run_lifecycle.R'))
  stopifnot(file.copy(file.path('engine', f), engine))
checks <- 0L
check <- function(x) { stopifnot(isTRUE(x)); checks <<- checks + 1L }
writeLines(c("CHAIN_ORDER <- 'compute'",
  'resolve_step_cli <- function(stepId, runDir, engineDir) {',
  ' a <- readRDS(file.path(runDir, "argv.rds"))',
  ' list(cmd=Sys.which("Rscript"),args=c(file.path(engineDir,"compute.R"),a),cwd=runDir,title="compute")',
  '}'), file.path(engine, 'step_plan.R'))
writeLines(c('a <- commandArgs(trailingOnly=TRUE)',
  'saveRDS(a, "received.rds")',
  'cat("COMPUTE_STARTED\\n"); flush(stdout())',
  'if (a[1] == "fail") quit(save="no", status=7L)',
  'deadline <- Sys.time() + if (a[1] == "compute") 3 else 0.1',
  'value <- 0',
  'while (Sys.time() < deadline) value <- value + sum(sqrt(seq_len(50000)))',
  'writeLines(as.character(value), "result.txt")',
  'cat("COMPUTE_DONE\\n")'), file.path(engine,'compute.R'))
newrun <- function(name, argv) {
  d <- file.path(root, name); dir.create(d); saveRDS(argv, file.path(d,'argv.rds')); d
}
start <- function(d, eng = engine) processx::process$new(Sys.which('Rscript'),
  c(file.path(eng,'run_chain.R'),'--run-dir',d,'--engine-dir',eng),
  stdout='|',stderr='2>&1',cleanup=TRUE,cleanup_tree=TRUE)
wait <- function(p) { p$wait(15000); if (p$is_alive()) {p$kill_tree(); stop('runner timeout')}; p$get_exit_status() }
states <- function(d) list.files(file.path(d,'status'), pattern='^lifecycle-.*[.]rds$', full.names=TRUE)
d <- newrun('compute', 'compute'); p <- start(d)
seqs <- numeric(); beats <- numeric(); live_log <- FALSE; run_id <- NULL
deadline <- Sys.time() + 15
while (p$is_alive()) {
  if (Sys.time() > deadline) {p$kill_tree(); stop('heartbeat timeout')}
  files <- states(d)
  if (length(files)) {
    s <- readRDS(files[1]); run_id <- s$run_id
    if (s$status == 'running') {
      seqs <- c(seqs,s$seq); beats <- c(beats,s$heartbeat_at)
      lf <- file.path(d,'status','run.log')
      live_log <- live_log || (file.exists(lf) && any(grepl('COMPUTE_STARTED',readLines(lf,warn=FALSE))))
    }
  }
  Sys.sleep(.05)
}
check(wait(p) == 0L)
check(length(unique(beats)) >= 4L)
check(length(unique(seqs)) >= 4L)
check(live_log)
s <- rl_read(file.path(d,'status'),run_id)
check(s$status == 'succeeded')
check(length(s$evidence) == 2L)
check(all(vapply(s$evidence,function(e) grepl(paste0('/notebook_attempts/',run_id,'/'),e$path,fixed=TRUE),TRUE)))
check(all(vapply(s$evidence,function(e) identical(unname(tools::md5sum(e$path)),e$md5),TRUE)))
check(grepl('NOT verified',s$reason,fixed=TRUE))
cat('PASS real CPU computation: observed',length(unique(beats)),'distinct live heartbeats; streaming log and attempt-only evidence\n')
# Fresh R process reopens persisted state, not an in-memory cached status.
reader <- file.path(root,'reader.R')
writeLines(c(sprintf('source(%s)', deparse(normalizePath('engine/run_lifecycle.R'))),
  sprintf('s <- rl_read(%s,%s)',deparse(file.path(d,'status')),deparse(run_id)),
  'stopifnot(s$status == "succeeded", length(s$evidence) == 2L)'),reader)
check(processx::run(Sys.which('Rscript'),reader,error_on_status=FALSE)$status == 0L)
# Same directory, new failed attempt: old notebook cannot stand in as evidence.
old_id <- run_id; saveRDS('fail',file.path(d,'argv.rds')); p <- start(d)
check(wait(p) != 0L)
all <- lapply(states(d),readRDS); failed <- Filter(function(x) x$status == 'failed',all)
check(length(all) == 2L && length(failed) == 1L)
check(failed[[1]]$run_id != old_id)
check(!length(failed[[1]]$evidence))
check(!file.exists(file.path(d,'reproducible.ipynb')))
check(rl_read(file.path(d,'status'),old_id)$status == 'succeeded')
check(fromJSON(file.path(d,'status','step_state.json'))$status == 'failed')
cat('PASS nonzero exit failed; retry has distinct run ID; old successful evidence retained but never reused\n')
# Raw argv (current resolver) and explicit POSIX shQuote tokens (legacy callers).
raw <- c('fast','two words', 'He said "hello"', "apostrophe's value",'$literal;not shell','', 'back\\slash')
for (quoted in c(FALSE,TRUE)) {
  rd <- newrun(if (quoted) 'quoted' else 'raw-spaces',if (quoted) shQuote(raw) else raw)
  q <- start(rd); check(wait(q) != 0L)
  check(identical(readRDS(file.path(rd,'received.rds')),raw))
  st <- readRDS(states(rd)[1]); check(st$status == 'failed')
  check(!length(st$evidence))
}
cat('PASS raw and shell-quoted argv preserve spaces, apostrophes, quotes, empty values and literal shell characters\n')
# Finalizer exception must fail even after a zero-exit compute child.
badengine <- file.path(root,'bad-engine'); dir.create(badengine)
file.copy(list.files(engine,full.names=TRUE),badengine)
cat('\nrn_finish <- function(...) stop("injected finalizer failure")\n',file=file.path(badengine,'run_notebook.R'),append=TRUE)
rd <- newrun('finish-failure','fast'); q <- start(rd,badengine)
check(wait(q) != 0L); st <- readRDS(states(rd)[1])
check(st$status == 'failed' && !length(st$evidence))
check(fromJSON(file.path(rd,'status','step_state.json'))$status == 'failed')
cat('PASS rn_finish exception makes lifecycle and legacy status failed\n')
# A heartbeat lock failure while child is alive must terminate only that child.
rd <- newrun('heartbeat-failure','compute'); q <- start(rd)
deadline <- Sys.time() + 10
repeat {
  if (Sys.time() > deadline) {q$kill_tree();stop('child start timeout')}
  if (file.exists(file.path(rd,'received.rds')) && length(states(rd))) break
  Sys.sleep(.01)
}
st <- readRDS(states(rd)[1]); lock <- rl_paths(file.path(rd,'status'),st$run_id)$lock
dir.create(lock); writeLines('foreign-holder',file.path(lock,'holder'))
check(wait(q) != 0L)
check(dir.exists(lock) && identical(readLines(file.path(lock,'holder')),'foreign-holder'))
Sys.sleep(.2)
check(!file.exists(file.path(rd,'result.txt')))
check(fromJSON(file.path(rd,'status','step_state.json'))$status == 'failed')
cat('PASS heartbeat failure kills compute child, preserves foreign lock; blocked terminal lifecycle write stays running, never success\n')
# Existing fixtures omit run_lifecycle.R: refuse rather than execute untracked work.
missing <- file.path(root,'missing-engine'); dir.create(missing)
file.copy(list.files(engine,full.names=TRUE),missing)
unlink(file.path(missing,'run_lifecycle.R'))
rd <- newrun('missing-module','fast'); q <- start(rd,missing)
check(wait(q) == 3L); check(!file.exists(file.path(rd,'received.rds')))
cat('PASS missing lifecycle module fails closed before executing\n')
# Mask only processx lookup in the fixture resolver; commands must never start.
no_process <- file.path(root,'no-processx-engine'); dir.create(no_process)
file.copy(list.files(engine,full.names=TRUE),no_process)
cat('\nbase_require <- requireNamespace\nrequireNamespace <- function(package, ...) if (package == "processx") FALSE else base_require(package, ...)\n',file=file.path(no_process,'step_plan.R'),append=TRUE)
rd <- newrun('missing-processx','fast'); q <- start(rd,no_process)
check(wait(q) != 0L)
check(!file.exists(file.path(rd,'received.rds')))
check(readRDS(states(rd)[1])$status == 'failed')
cat('PASS processx unavailable fails closed with failed lifecycle, no child execution\n')
cat('PASS',checks,'checks; fixtures:',root,'\n')
