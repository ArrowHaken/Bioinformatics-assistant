#!/usr/bin/env Rscript
# Run from app root: Rscript tests/notebook_replay_api.R
app <- normalizePath(".", mustWork = TRUE)
source(file.path(app, "engine", "notebook_replay_api.R"))
scratch <- tempfile("nr-api-", tmpdir = file.path(app, "tmp"))
dir.create(scratch, recursive = TRUE)
write_json <- function(x, p) {
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  writeLines(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)), p)
}
read_json <- function(p) jsonlite::fromJSON(p, simplifyVector = FALSE)
checks <- 0L
expect_error <- function(expr) {
  result <- tryCatch({ force(expr); FALSE }, error = function(e) {
    stopifnot(identical(conditionMessage(e), "Declared replay unavailable or invalid")); TRUE
  })
  stopifnot(result); checks <<- checks + 1L
}
run <- function() {
  on.exit(unlink(scratch, recursive = TRUE))
  # Independent application fixture; no writes to the application's real state.
  fixture <- file.path(scratch, "app"); dir.create(file.path(fixture, "engine"), recursive = TRUE)
  for (f in c("notebook_build.R", "notebook_replay.R"))
    stopifnot(file.copy(file.path(app, "engine", f), file.path(fixture, "engine", f)))
  rd <- file.path(scratch, "run"); dir.create(rd)
  session <- list(id = "replay_test", mode = "local", run_dir = rd)
  binding <- list(session_id = session$id, app_dir = fixture, run_dir = rd, binding_id = "bind-1")
  bp <- file.path(rd, "status", "notebook_session.json")
  mp <- file.path(rd, "status", "notebook_manifest.json")
  ap <- file.path(rd, "status", "notebook_attempts", "attempt-1", "status", "attempt.json")
  m <- list(binding_id = binding$binding_id,
    phases = list(list(step = "01", attempt_id = "attempt-1", state = "succeeded")))
  a <- list(attempt_id = "attempt-1", binding_id = binding$binding_id, execution_succeeded = TRUE)
  reset <- function() { write_json(binding, bp); write_json(m, mp); write_json(a, ap) }
  reset()
  # A marker-producing script proves construction and download do not run analysis.
  script <- "from pathlib import Path\nPath('EXECUTED').write_text('bad')\n"
  writeBin(charToRaw(script), file.path(rd, "analysis.py"))
  writeBin(as.raw(c(0, 1, 2, 255)), file.path(rd, "input.bin"))
  writeBin(charToRaw("original\r\n"), file.path(rd, "out.csv"))
  manifest <- list(files = list(list(path = "analysis.py", role = "script"),
    list(path = "input.bin", role = "input", embed_authorized = TRUE)),
    steps = list(list(command = "python3", argv = list("analysis.py"), cwd = ".")),
    expected_outputs = list("out.csv"))
  build <- function() nr_api_build(fixture, session, manifest)
  download <- function() nr_api_download(fixture, session)
  stopifnot(build()$ok)
  d <- download()
  p <- file.path(fixture, "state", "notebooks", session$id, "declared-replay.ipynb")
  stopifnot(identical(d$bytes, readBin(p, "raw", n = file.info(p)$size)),
    !file.exists(file.path(rd, "EXECUTED")),
    !file.exists(file.path(dirname(p), "reproducible.ipynb")),
    jsonlite::fromJSON(rawToChar(d$bytes))$nbformat == 4)
  checks <<- checks + 1L
  wrong <- session; wrong$id <- "other_session"
  expect_error(nr_api_build(fixture, wrong, manifest))
  expect_error(nr_api_download(fixture, wrong))
  wrong <- session; wrong$mode <- "remote"
  expect_error(nr_api_build(fixture, wrong, manifest))
  wrong$mode <- NULL; expect_error(nr_api_build(fixture, wrong, manifest))
  for (field in c("session_id", "app_dir", "run_dir", "binding_id")) {
    b <- binding; b[[field]] <- "different"; write_json(b, bp)
    expect_error(build()); expect_error(download()); reset()
  }
  alias <- file.path(scratch, "alias"); stopifnot(file.symlink(rd, alias))
  wrong <- session; wrong$run_dir <- alias
  expect_error(nr_api_build(fixture, wrong, manifest))
  expect_error(nr_api_download(fixture, wrong))
  # Reject linked status and linked per-session application storage, not just roots.
  status <- file.path(rd, "status"); saved_status <- file.path(scratch, "saved-status")
  stopifnot(file.rename(status, saved_status), file.symlink(saved_status, status))
  expect_error(build()); expect_error(download()); unlink(status)
  stopifnot(file.rename(saved_status, status))
  store <- dirname(p); saved_store <- file.path(scratch, "saved-store")
  stopifnot(file.rename(store, saved_store), file.symlink(saved_store, store))
  expect_error(build()); expect_error(download()); unlink(store)
  stopifnot(file.rename(saved_store, store))
  unlink(bp); expect_error(build()); expect_error(download()); reset()
  bad <- manifest; bad$root <- rd
  expect_error(nr_api_build(fixture, session, bad))
  for (state in c("failed", "stale", "running")) {
    mm <- m; mm$phases[[1]]$state <- state; write_json(mm, mp)
    expect_error(build()); expect_error(download()); reset()
  }
  mm <- m; mm$active_attempt <- "attempt-new"; write_json(mm, mp)
  expect_error(build()); expect_error(download()); reset()
  mm <- m; mm$binding_id <- "other"; write_json(mm, mp)
  expect_error(build()); expect_error(download()); reset()
  for (field in c("attempt_id", "binding_id", "execution_succeeded")) {
    aa <- a; aa[[field]] <- if (field == "execution_succeeded") FALSE else "other"
    write_json(aa, ap); expect_error(build()); expect_error(download()); reset()
  }
  # A new successful attempt is not allowed to reuse an older successful export.
  mm <- m; mm$phases[[1]]$attempt_id <- "attempt-2"
  aa <- a; aa$attempt_id <- "attempt-2"
  write_json(aa, file.path(rd, "status", "notebook_attempts", "attempt-2", "status", "attempt.json"))
  write_json(mm, mp); expect_error(download())
  stopifnot(build()$ok, download()$ok); reset(); expect_error(download()); stopifnot(build()$ok)
  for (f in c("analysis.py", "input.bin", "out.csv")) {
    fp <- file.path(rd, f); bytes <- readBin(fp, "raw", n = file.info(fp)$size)
    changed <- bytes; changed[[1]] <- as.raw(bitwXor(as.integer(changed[[1]]), 1L))
    stamp <- file.info(fp)$mtime
    writeBin(changed, fp); Sys.setFileTime(fp, stamp)
    expect_error(download())
    writeBin(bytes, fp); stopifnot(download()$ok)
    writeBin(c(bytes, as.raw(10)), fp); expect_error(download()); writeBin(bytes, fp)
  }
  # A declared symlink must not be followed during build OR cached download.
  fp <- file.path(rd, "input.bin"); saved <- file.path(scratch, "saved.bin")
  stopifnot(file.rename(fp, saved), file.symlink(saved, fp))
  expect_error(build()); expect_error(download()); unlink(fp); stopifnot(file.rename(saved, fp))
  # Attempt metadata and exported notebook are also checked as non-symlink files.
  saved <- file.path(scratch, "attempt.json"); stopifnot(file.rename(ap, saved), file.symlink(saved, ap))
  expect_error(build()); expect_error(download()); unlink(ap); stopifnot(file.rename(saved, ap))
  original <- readBin(p, "raw", n = file.info(p)$size)
  writeBin(c(original, as.raw(10)), p); expect_error(download()); writeBin(original, p)
  stopifnot(download()$ok)
  # Local synchronization never touches declared-replay.ipynb.
  e <- new.env(parent = baseenv()); sys.source(file.path(fixture, "engine", "notebook_build.R"), e)
  e$nb_append_cell(file.path(rd, "status", "notebook_attempts", "attempt-1"),
    list(step = "01", lang = "python", code = "print('captured')", exit_code = 0L, attempt_id = "attempt-1"))
  e$nb_sync_local(fixture, session)
  stopifnot(identical(original, download()$bytes), !file.exists(file.path(rd, "EXECUTED")))
  cat("notebook_replay_api:", checks, "checks passed\n")
}
run()
