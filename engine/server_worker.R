#!/usr/bin/env Rscript
# One long-lived request worker for the local HTTP server.
#
# The master blocks in accept() and owns the public sockets. This process owns
# one database connection and executes one router call at a time. A completed
# response is returned over a fresh localhost connection with a PA-DELIVERY
# header, which wakes the master immediately without socketSelect().

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

argv <- commandArgs(trailingOnly = TRUE)
arg_value <- function(key) {
  i <- match(key, argv)
  if (is.na(i) || i == length(argv)) return("")
  argv[[i + 1L]]
}

port <- suppressWarnings(as.integer(arg_value("--port")))
app_dir <- arg_value("--app")
# The master passes the shared token through the environment; a command-line
# argument would make it visible to every process on the machine.
token <- Sys.getenv("PA_WORKER_TOKEN", unset = "")
if (!nzchar(token)) token <- arg_value("--token")
if (is.na(port) || !nzchar(app_dir) || !nzchar(token)) quit(save = "no", status = 2L)
app_dir <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)
options(pa.app_dir = app_dir, pa.serve.definitions_only = TRUE)
old_wd <- getwd()
setwd(app_dir)
on.exit(setwd(old_wd), add = TRUE)

e <- new.env(parent = globalenv())
load_error <- tryCatch({
  sys.source(file.path(app_dir, "serve.R"), envir = e, keep.source = FALSE)
  NULL
}, error = function(err) err)
if (!is.null(load_error)) {
  message("worker definitions failed: ", conditionMessage(load_error))
  quit(save = "no", status = 3L)
}
if (isTRUE(e$HAS_DB)) {
  e$DB_CON <- tryCatch(e$db_api_boot(app_dir), error = function(err) NULL)
  e$DB_READY <- !is.null(e$DB_CON)
}

con <- NULL
for (attempt in seq_len(200L)) {
  con <- tryCatch(socketConnection("127.0.0.1", port, open = "r+b",
                                   blocking = TRUE, timeout = 30,
                                   options = "no-delay"),
                  error = function(err) NULL)
  if (!is.null(con)) break
  Sys.sleep(0.05)
}
if (is.null(con)) quit(save = "no", status = 4L)

writeBin(charToRaw(paste0("PA-WORKER ", token, "\r\n")), con)
flush(con)
ack <- tryCatch(readLines(con, n = 1L, warn = FALSE), error = function(err) character(0))
if (!length(ack) || !identical(ack[[1L]], "PA-WORKER-OK")) {
  try(close(con), silent = TRUE)
  quit(save = "no", status = 5L)
}
try(socketTimeout(con, 86400), silent = TRUE)

execute_request <- function(req) {
  out <- rawConnection(raw(0), "wb")
  on.exit(try(close(out), silent = TRUE), add = TRUE)
  err <- tryCatch({
    e$serve_request(out, req)
    NULL
  }, error = function(ex) ex)
  if (!is.null(err) && !length(rawConnectionValue(out))) {
    try(e$send_json(out, 500L, list(
      ok = FALSE,
      error = enc2utf8(conditionMessage(err)))), silent = TRUE)
  }
  flush(out)
  rawConnectionValue(out)
}

deliver <- function(job_id, bytes) {
  dcon <- tryCatch(socketConnection("127.0.0.1", port, open = "r+b",
                                    blocking = TRUE, timeout = 30,
                                    options = "no-delay"),
                   error = function(err) NULL)
  if (is.null(dcon)) return(FALSE)
  ok <- tryCatch({
    writeBin(charToRaw(paste0("PA-DELIVERY ", token, " ", job_id, "\r\n")), dcon)
    flush(dcon)
    serialize(list(kind = "result", token = token, job_id = as.integer(job_id),
                   bytes = bytes), dcon)
    flush(dcon)
    TRUE
  }, error = function(err) FALSE)
  try(close(dcon), silent = TRUE)
  ok
}

repeat {
  msg <- tryCatch(unserialize(con), error = function(err) {
    message("worker unserialize error: ", conditionMessage(err))
    NULL
  })
  if (is.null(msg) || !is.list(msg)) {
    message("worker received EOF")
    break
  }
  if (identical(msg$kind, "shutdown")) break
  if (!identical(msg$kind, "request") || !identical(msg$token, token)) next

  job_id <- suppressWarnings(as.integer(msg$job_id))
  if (length(job_id) != 1L || is.na(job_id)) next
  bytes <- execute_request(msg$req)
  if (!length(bytes)) bytes <- charToRaw(
    "HTTP/1.1 500 Internal Server Error\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Length: 15\r\nConnection: close\r\n\r\nworker internal")
  if (!deliver(job_id, bytes)) break
}

try(close(con), silent = TRUE)
quit(save = "no", status = 0L)
