#!/usr/bin/env Rscript
# The entrypoint must stay small so Rscript does not spend tens of seconds
# parsing it on every cold start. Definitions are loaded through sys.source().

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))
APP <- normalizePath(getwd())
if (!dir.exists(file.path(APP, "engine"))) APP <- normalizePath(file.path(APP, ".."))

PASS <- 0L; FAIL <- 0L
ok <- function(name, cond, detail = "") {
  if (isTRUE(cond)) { PASS <<- PASS + 1L; cat(sprintf("  [ok]   %s\n", name)) }
  else { FAIL <<- FAIL + 1L; cat(sprintf("  [FAIL] %s  %s\n", name, detail)) }
}

entry <- file.path(APP, "serve.R")
main <- file.path(APP, "engine", "server_main.R")
ok("small entrypoint exists", file.exists(entry))
ok("server implementation exists", file.exists(main))
ok("entrypoint is below 8 KB", file.info(entry)$size < 8192,
   paste("size", file.info(entry)$size))

old <- options(pa.serve.definitions_only = TRUE, pa.app_dir = APP)
on.exit(options(old), add = TRUE)
e <- new.env(parent = globalenv())
res <- tryCatch({ sys.source(entry, envir = e); NULL }, error = function(err) err)
ok("entrypoint loads definitions", is.null(res),
   if (is.null(res)) "" else conditionMessage(res))
ok("HTTP router is loaded", exists("serve_request", envir = e, mode = "function"))
ok("migration function is loaded", exists("migrate_history", envir = e, mode = "function"))
ok("JIT is disabled for the oversized first-call router",
   identical(compiler::enableJIT(-1L), 0L),
   paste("JIT level", compiler::enableJIT(-1L)))
guard <- readLines(file.path(APP, "engine", "startup_guard.R"), warn = FALSE)
ok("startup probe writes over a binary socket",
   any(grepl('open = "r\\+b"', guard, fixed = FALSE)),
   "health probe must not fall back to a text connection")
ok("engine root is resolved",
   identical(normalizePath(e$ENGINE_DIR, winslash = "/", mustWork = TRUE),
             normalizePath(file.path(APP, "engine"), winslash = "/", mustWork = TRUE)))

cat(sprintf("\nPASS=%d FAIL=%d\n", PASS, FAIL))
quit(save = "no", status = if (FAIL == 0L) 0L else 1L)
