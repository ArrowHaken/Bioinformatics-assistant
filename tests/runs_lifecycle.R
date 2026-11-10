#!/usr/bin/env Rscript
source("engine/runs.R")
rl <- new.env(parent = baseenv())
sys.source("engine/run_lifecycle.R", rl)
app <- normalizePath(getwd())
now <- as.POSIXct("2026-11-09 12:00:00", tz = "UTC")
checks <- 0L
expect <- function(actual, expected, label) {
  if (!identical(actual, expected)) stop(label, ": ", paste(actual, collapse = ","))
  checks <<- checks + 1L
}
write_json <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")), path)
}
main <- function() {
  root <- tempfile("runs-lifecycle-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  make <- function(name, status = "running", age = 5, attempt = "a-1") {
    rd <- file.path(root, name); dir.create(rd)
    sd <- file.path(rd, "status"); dir.create(sd)
    binding <- list(session_id = name, app_dir = app, run_dir = normalizePath(rd), binding_id = "binding-1")
    write_json(binding, file.path(sd, "notebook_session.json"))
    phases <- list(list(step = "01", attempt_id = attempt, state = status))
    manifest <- list(binding_id = "binding-1", phases = phases, active_attempt = attempt)
    write_json(manifest, file.path(sd, "notebook_manifest.json"))
    identity <- list(attempt_id = attempt, binding_id = "binding-1")
    proof <- file.path(sd, "notebook_attempts", attempt, "status", "attempt.json")
    write_json(identity, proof)
    invisible(rl$rl_create(sd, attempt, attempt, "create", now = now - age))
    if (status != "queued") invisible(rl$rl_claim(sd, attempt, attempt, "worker", "claim", now = now - age))
    if (status %in% c("succeeded", "failed", "cancelled"))
      invisible(rl$rl_finish(sd, attempt, attempt, "worker", "finish", status,
        evidence = if (status == "succeeded") proof else character(), now = now - age))
    list(sess = list(id = name, mode = "local", run_dir = rd), sd = sd, manifest = manifest,
      binding = binding, proof = proof)
  }
  read <- function(f) runs_local_lifecycle(app, f$sess, now)
  rewrite_manifest <- function(f, m) write_json(m, file.path(f$sd, "notebook_manifest.json"))
  for (status in c("queued", "running", "succeeded", "failed", "cancelled")) {
    f <- make(status, status)
    got <- read(f)
    expect(got$state, if (status == "succeeded") "execution_completed" else status, status)
    expect(got$full_analysis_replay_verified, FALSE, "replay never verified")
    expect(got$scientific_validity_verified, FALSE, "science never verified")
  }
  for (age in c(90, 91, 1801)) {
    f <- make(paste0("age", age), age = age)
    expect(read(f)$state, if (age <= 90) "running" else "stalled", "durable stalled not lost")
  }
  f <- make("future", age = -5)
  expect(read(f)$state, "unknown", "future heartbeat")
  f <- make("bad-heartbeat")
  path <- file.path(f$sd, "lifecycle-a-1.rds")
  state <- readRDS(path); state$heartbeat_at <- NULL; saveRDS(state, path)
  expect(read(f)$state, "unknown", "missing heartbeat")
  f <- make("mine")
  f$sess$id <- "someone-else"
  expect(read(f)$state, "unknown", "other session denied")
  f$sess$id <- "mine"
  b <- f$binding; b$app_dir <- root; write_json(b, file.path(f$sd, "notebook_session.json"))
  expect(read(f)$state, "unknown", "wrong app denied")
  b <- f$binding; b$run_dir <- root; write_json(b, file.path(f$sd, "notebook_session.json"))
  expect(read(f)$state, "unknown", "wrong normalized run denied")
  write_json(f$binding, file.path(f$sd, "notebook_session.json"))
  write_json(list(attempt_id = "a-1", binding_id = "foreign"), f$proof)
  expect(read(f)$state, "unknown", "foreign attempt denied")
  f <- make("manifest-binding")
  m <- f$manifest; m$binding_id <- "foreign"; rewrite_manifest(f, m)
  expect(read(f)$state, "unknown", "foreign manifest denied")
  f <- make("no-binding")
  unlink(file.path(f$sd, "notebook_session.json"))
  expect(read(f), NULL, "unbound legacy")
  f$sess$mode <- "remote"
  expect(read(f), NULL, "remote untouched")
  f <- make("active-first")
  m <- f$manifest
  m$phases <- list(list(step = "01", attempt_id = "old", state = "succeeded"))
  rewrite_manifest(f, m)
  expect(read(f)$state, "running", "active beats old phases")
  m$active_attempt <- "missing"; rewrite_manifest(f, m)
  expect(read(f)$state, "unknown", "missing active cannot fall back")
  f <- make("retry", "failed")
  m <- f$manifest; m$active_attempt <- NULL
  m$phases <- list(list(step = "01", attempt_id = "a-1", state = "failed"),
    list(step = "02", attempt_id = "old-success", state = "stale"))
  rewrite_manifest(f, m)
  expect(read(f)$state, "failed", "stale downstream cannot replace retry")
  f <- make("phase-last", "succeeded", attempt = "a-2")
  m <- f$manifest; m$active_attempt <- NULL
  m$phases <- list(list(step = "01", attempt_id = "a-1", state = "succeeded"),
    list(step = "02", attempt_id = "a-2", state = "succeeded"))
  rewrite_manifest(f, m)
  expect(read(f)$attempt_id, "a-2", "explicit last phase")
  m$phases[[3]] <- list(step = "03", attempt_id = "a-1", state = "succeeded")
  rewrite_manifest(f, m)
  expect(read(f)$state, "unknown", "ambiguous attempt groups")
  m$phases[[1]]$state <- "stale"; rewrite_manifest(f, m)
  expect(read(f)$state, "unknown", "non suffix stale ambiguous")
  f <- make("wrong-runid")
  path <- file.path(f$sd, "lifecycle-a-1.rds")
  state <- readRDS(path); state$run_id <- "other"; saveRDS(state, path)
  expect(read(f)$state, "unknown", "lifecycle run identity")
  f <- make("wrong-attemptid")
  path <- file.path(f$sd, "lifecycle-a-1.rds")
  state <- readRDS(path); state$attempt_id <- "other"; saveRDS(state, path)
  expect(read(f)$state, "unknown", "lifecycle attempt identity")
  f <- make("mtime-ignored", "failed")
  invisible(rl$rl_create(f$sd, "newer-file", "newer-file", "create", now = now))
  invisible(rl$rl_claim(f$sd, "newer-file", "newer-file", "worker", "claim", now = now))
  Sys.setFileTime(file.path(f$sd, "lifecycle-newer-file.rds"), now + 3600)
  write_json(list(state = "done"), file.path(f$sd, "step_state.json"))
  m <- f$manifest; m$active_attempt <- NULL; rewrite_manifest(f, m)
  expect(read(f)$state, "failed", "mtime and unbound step state never select attempts")
  m$phases <- list(); rewrite_manifest(f, m)
  expect(read(f)$state, "unknown", "no pointer cannot select available lifecycle")
  f <- make("normalized-alias")
  f$sess$run_dir <- paste0(f$sess$run_dir, "/.")
  expect(read(f)$state, "running", "normalized run matches")
  f <- make("corrupt")
  writeLines("broken", file.path(f$sd, "lifecycle-a-1.rds"))
  expect(suppressWarnings(read(f))$state, "unknown", "corrupt lifecycle")
  f <- make("corrupt-json")
  writeLines("broken", file.path(f$sd, "notebook_manifest.json"))
  expect(read(f)$state, "unknown", "corrupt manifest")
  for (kind in c("state", "status", "binding", "attempt", "run")) {
    f <- make(paste0("link-", kind))
    path <- switch(kind, state = file.path(f$sd, "lifecycle-a-1.rds"), status = f$sd,
      binding = file.path(f$sd, "notebook_session.json"), attempt = dirname(dirname(f$proof)), run = f$sess$run_dir)
    destination <- paste0(path, "-foreign")
    stopifnot(file.rename(path, destination), file.symlink(destination, path))
    expect(read(f)$state, "unknown", paste("reject symlink", kind))
  }
  f <- make("list-test", "succeeded")
  # runs_list still receives DB-filtered sessions, and never enumerates run roots.
  remote_steps_read <- function(sid) list(steps = list(list(id = "01", status = "running")))
  flow_prog_read <- function(sid) list(stages = list())
  env <- new.env(parent = globalenv())
  sys.source("engine/runs.R", env)
  env$remote_steps_read <- remote_steps_read; env$flow_prog_read <- flow_prog_read
  before <- ls(globalenv(), all.names = TRUE)
  rows <- env$runs_list(app, "owner", list(f$sess), readonly = f$sess$id, order = "01", now = now)
  expect(rows[[1]]$state, "execution_completed", "list overlay")
  expect(rows[[1]]$readonly, TRUE, "readonly retained")
  expect(rows[[1]]$current_stage, NULL, "legacy current cleared")
  expect(ls(globalenv(), all.names = TRUE), before, "isolated module no global pollution")
  expect(length(env$runs_list(app, "owner", list(), now = now)), 0L, "does not discover others")
  env$rl_read <- rl$rl_read
  expect(env$runs_local_lifecycle(app, f$sess, now)$state, "execution_completed", "already loaded reader")
  unlink(file.path(f$sd, "notebook_session.json"))
  expect(env$runs_list(app, "owner", list(f$sess), order = "01", now = now)[[1]]$state,
    "unknown", "legacy summary unchanged without binding")
  expect(grepl("未验证", runs_state_sentence(list(state = "execution_completed")), fixed = TRUE), TRUE, "explicit execution scope")
}
main()
cat("PASS runs_lifecycle:", checks, "checks\n")
