main <- function() {
  e <- .GlobalEnv
  source("engine/sessions.R", local = e)
  root <- tempfile("pa-pending-test-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  s <- e$sess_create(root, "pending test")
  sid <- as.character(s$id)
  ok <- e$sess_set_pending(root, sid, "req-baseline")
  stopifnot(isTRUE(ok$ok), identical(e$sess_get_pending(root, sid), "req-baseline"))
  stopifnot(isTRUE(e$attempt_register(root, sid, "req-direct")$ok))
  bad_id <- e$attempt_register(root, sid, "bad request id")
  stopifnot(!isTRUE(bad_id$ok), identical(bad_id$error, "invalid request id"))
  missing_session <- e$attempt_register(root, "s-missing", "req-valid")
  stopifnot(!isTRUE(missing_session$ok), identical(missing_session$error, "session not found"))

  # Reproduce the old silent-write path: persistence reports success but writes nothing.
  e$sess_profile_write <- function(...) invisible(TRUE)
  failed <- e$sess_set_pending(root, sid, "req-should-fail")
  stopifnot(!isTRUE(failed$ok),
            identical(failed$error, "pending profile writeback mismatch"))

  cat("PASS baseline pending readback\n")
  cat("PASS silent profile write failure rejected: ", failed$error, "\n", sep = "")
}
main()
