#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))
root <- tempfile("remote-chain-subfailure-", tmpdir = "/tmp")
dir.create(root)
STATE_DIR <- root
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
write_json_file <- function(path, value) writeLines(as.character(jsonlite::toJSON(value, auto_unbox=TRUE, null="null", simplifyVector=FALSE)), path)
read_text_any <- function(path) paste(readLines(path, warn=FALSE), collapse="\n")
exec_ts <- function() "test-time"
exec_items <- list(list(id="job-old", chain_id="req-old", step="first", session_id="session-a", status="running"))
exec_read <- function() list(items=exec_items)
HAS_SESSIONS <- FALSE
FLOW_ORDER <- function(profile) c("first", "second")
flow_stage <- function(id, profile) list(cover_by="")
last_dispatch <- NULL
exec_with_lock <- function(fn) fn()
serve_request <- function(con, req) {
  last_dispatch <<- jsonlite::fromJSON(req$body, simplifyVector=FALSE)
  list(code=200L, body=list(ok=TRUE, delegated=TRUE, title=last_dispatch$step))
}
exec_set <- function(id, fields) {
  for (i in seq_along(exec_items)) if (identical(exec_items[[i]]$id, id)) exec_items[[i]] <<- modifyList(exec_items[[i]], fields)
  invisible(TRUE)
}
# Dispatch is captured above so failure tests can assert that nothing was sent.
options(pa.chain.definitions_only=TRUE)
source("engine/run_chain.R")
checks <- 0L
check <- function(x) { stopifnot(isTRUE(x)); checks <<- checks + 1L }
old <- list(session_id="session-a", run_dir="/shared/out", req_id="req-old", todo=as.list(c("first","second")), index=1L, status="running", completed=list(), owner_ctx=list())
rc_write(old)
failed <- list(session_id="session-a", req_id="req-old", step="first", sub="qc", sub_status="failed", status="running", code=1L)
res <- rc_callback(failed, FALSE)
check(identical(res$status, "failed"))
check(identical(rc_read("session-a")$status, "failed"))
check(!exists("last_set", envir=.GlobalEnv, inherits=FALSE))
check(identical(exec_read()$items[[1]]$status, "running"))
cat("PASS substage failure terminates chain without falsely releasing a running executor task\n")
old$req_id <- "req-old2"; old$status <- "running"; old$index <- 1L; rc_write(old)
late <- rc_callback(within(failed, req_id <- "req-old"), FALSE)
check(is.null(late))
check(identical(rc_read("session-a")$req_id, "req-old2"))
cat("PASS stale callback cannot mutate replacement chain\n")

# Old executor still running: a confirmed overwrite must be refused, not started.
exec_items <- list(list(id="job-old2", chain_id="req-old2", step="first", session_id="session-a", status="running"))
blocked <- rc_begin("session-a", "/shared/out", "req-new", list(), overwrite=TRUE)
check(identical(blocked$code, 409L))
check(identical(blocked$body$code, "session_execution_busy"))
check(identical(rc_read("session-a")$req_id, "req-old2"))
cat("PASS overwrite refused while the previous executor still runs\n")

# Executor reported real exit: the new request starts again from the first stage.
exec_items[[1]]$status <- "failed"
last_dispatch <- NULL
started <- rc_begin("session-a", "/shared/out", "req-new", list(), overwrite=TRUE)
check(identical(started$code, 200L))
check(identical(rc_read("session-a")$req_id, "req-new"))
check(identical(rc_read("session-a")$index, 1L))
check(identical(last_dispatch$step, "first"))
check(identical(last_dispatch$req_id, "req-new"))
cat("PASS confirmed rerun restarts from the first stage after executor exits\n")

# A late writeback from the replaced chain must not touch the new one.
late2 <- rc_callback(list(session_id="session-a", req_id="req-old2", step="first", status="done", code=0L), TRUE)
check(is.null(late2))
check(identical(rc_read("session-a")$req_id, "req-new"))
cat("PASS replaced chain writeback cannot advance the new chain\n")
cat("PASS", checks, "checks\n")
