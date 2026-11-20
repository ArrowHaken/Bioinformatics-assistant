#!/usr/bin/env Rscript
# Tiny entrypoint. Keep this file small: Rscript parses a script incrementally,
# and parsing the old 10k-line serve.R directly cost tens of seconds of CPU on
# every cold start. The implementation lives in engine/server_main.R and is
# loaded by source(), which parses the file once and is measurably fast.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

forced <- getOption("pa.app_dir", Sys.getenv("PA_APP_DIR", unset = ""))
if (is.character(forced) && length(forced) == 1L && nzchar(forced)) {
  APP_DIR <- normalizePath(forced, winslash = "/", mustWork = TRUE)
} else {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
  APP_DIR <- if (length(file_arg)) {
    normalizePath(dirname(file_arg[[1]]), winslash = "/", mustWork = TRUE)
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  }
}

MAIN_FILE <- file.path(APP_DIR, "engine", "server_main.R")
if (!file.exists(MAIN_FILE)) stop("server implementation missing: ", MAIN_FILE)

options(pa.app_dir = APP_DIR)
# The HTTP router is still one very large closure inherited from the old
# entrypoint. R's default JIT compiles a function on its first call; compiling
# that closure cost 18 seconds before the first response (measured with
# /api/ping, which does no database or gateway work). Disable JIT before
# loading the implementation so cold-start latency is paid by neither the
# first browser request nor the health probe. Revisit this only after the
# route dispatcher is split into smaller handlers.
if (requireNamespace("compiler", quietly = TRUE)) invisible(compiler::enableJIT(0L))
definitions_only <- isTRUE(getOption("pa.serve.definitions_only", FALSE))
old_wd <- getwd()
setwd(APP_DIR)
if (definitions_only) on.exit(setwd(old_wd), add = TRUE)

sys.source(MAIN_FILE, envir = environment(), keep.source = FALSE)
