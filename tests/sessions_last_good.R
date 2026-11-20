#!/usr/bin/env Rscript
# sessions.json recovery contract: the primary index is authoritative, while
# .last-good is a read-only fallback when the primary is missing or unreadable.

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

args <- commandArgs(trailingOnly = TRUE)
APP <- local({
  a <- sub("^--app=", "", args[grepl("^--app=", args)])
  if (length(a)) normalizePath(a) else normalizePath(getwd())
})
if (!dir.exists(file.path(APP, "engine"))) APP <- normalizePath(file.path(APP, ".."))
source(file.path(APP, "engine", "sessions.R"))

PASS <- 0L; FAIL <- 0L
ok <- function(name, cond, detail = "") {
  if (isTRUE(cond)) { PASS <<- PASS + 1L; cat(sprintf("  [ok]   %s\n", name)) }
  else { FAIL <<- FAIL + 1L; cat(sprintf("  [FAIL] %s  %s\n", name, detail)) }
}
read_capture <- function(path) {
  warning_text <- NULL
  value <- withCallingHandlers(sess_read(path),
    warning = function(w) {
      warning_text <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    })
  list(value = value, warning = warning_text)
}

TMP <- tempfile("sessions-last-good-")
dir.create(TMP, recursive = TRUE, showWarnings = FALSE)
on.exit(try(unlink(TMP, recursive = TRUE, force = TRUE), silent = TRUE), add = TRUE)

one <- list(current = "one", sessions = list(list(id = "one", name = "first")))
two <- list(current = "two", sessions = list(list(id = "two", name = "second")))
three <- list(current = "three", sessions = list(list(id = "three", name = "third")))
sess_write(TMP, one)
sess_write(TMP, two)
sess_write(TMP, three)

index <- sess_file(TMP)
backup <- paste0(index, ".last-good")
ok("primary and backup both exist", file.exists(index) && file.exists(backup))
valid <- read_capture(TMP)
ok("valid primary is authoritative", identical(valid$value$current, "three") &&
     is.null(valid$warning))

writeLines("not-json", index, useBytes = TRUE)
recovered <- read_capture(TMP)
ok("corrupt primary falls back to last-good",
   identical(recovered$value$current, "two") &&
     grepl("last-good", recovered$warning, fixed = TRUE))

unlink(index)
missing <- read_capture(TMP)
ok("missing primary falls back to last-good",
   identical(missing$value$current, "two") &&
     grepl("last-good", missing$warning, fixed = TRUE))

writeLines("not-json", index, useBytes = TRUE)
writeLines("not-json", backup, useBytes = TRUE)
both_bad <- tryCatch(sess_read(TMP), error = function(e) e)
ok("unreadable primary and backup fail closed",
   inherits(both_bad, "error") && nzchar(conditionMessage(both_bad)))

unlink(index)
unlink(backup)
both_missing <- read_capture(TMP)
ok("no index is an empty history, not an error",
   is.list(both_missing$value) && identical(both_missing$value$sessions, list()) &&
     is.null(both_missing$value$current) &&
     is.null(both_missing$warning))

cat(sprintf("\nPASS=%d FAIL=%d\n", PASS, FAIL))
quit(save = "no", status = if (FAIL == 0L) 0L else 1L)
