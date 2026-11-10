#!/usr/bin/env Rscript
# Real runner subprocesses; only disposable sessions/state and synthetic inputs.
app <- normalizePath(".", mustWork = TRUE)
source(file.path(app, "engine", "notebook_auto_export.R"))
source(file.path(app, "engine", "notebook_replay_api.R"))
root <- tempfile("notebook-auto-", tmpdir = "/tmp"); dir.create(root)
write_json <- function(x, p) {
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  writeLines(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")), p)
}
read_json <- function(p) jsonlite::fromJSON(p, simplifyVector = FALSE)
checks <- 0L
check <- function(...) { stopifnot(...); checks <<- checks + 1L }
run <- function() {
  on.exit(unlink(root, recursive = TRUE))
  fixture <- file.path(root, "app"); engine <- file.path(fixture, "engine")
  dir.create(engine, recursive = TRUE)
  for (f in c("run_chain.R", "run_notebook.R", "notebook_build.R", "run_lifecycle.R",
              "notebook_replay.R", "notebook_replay_api.R", "notebook_auto_export.R", "sessions.R", "state_paths.R"))
    stopifnot(file.copy(file.path(app, "engine", f), engine))
  writeLines(c('CHAIN_ORDER <- c("r", "py")',
    'resolve_step_cli <- function(stepId, runDir, engineDir) list(',
    'cmd = if (stepId == "r") Sys.which("Rscript") else Sys.which("python3"),',
    'args = file.path(runDir, if (stepId == "r") "analysis.R" else "analysis.py"),',
    'cwd = runDir, title = stepId)'), file.path(engine, "step_plan.R"))
  rd <- file.path(root, "run"); dir.create(rd)
  session <- list(id = "auto_test", mode = "local", run_dir = rd)
  sp <- file.path(fixture, "state", "sessions.json")
  write_json(list(sessions = list(session)), sp)
  b <- list(session_id = session$id, app_dir = fixture, run_dir = rd, binding_id = "bind-auto")
  bp <- file.path(rd, "status", "notebook_session.json"); write_json(b, bp)
  writeLines('writeLines("synthetic R", "r-output.txt")', file.path(rd, "analysis.R"))
  writeLines(c('from pathlib import Path', 'Path("py-output.txt").write_text("synthetic Python")'), file.path(rd, "analysis.py"))
  manifest <- list(files = list(list(path = "analysis.R", role = "script"),
    list(path = "analysis.py", role = "script")),
    steps = list(list(command = "Rscript", argv = list("analysis.R"), cwd = "."),
      list(command = "python3", argv = list("analysis.py"), cwd = ".")),
    expected_outputs = list("r-output.txt", "py-output.txt"))
  manifest_path <- file.path(rd, "replay-manifest.json")
  auto_path <- file.path(rd, "status", "notebook_auto_export.json")
  mp <- file.path(rd, "status", "notebook_manifest.json")
  invoke <- function(run_path = rd, success = TRUE) {
    out <- suppressWarnings(system2(Sys.which("Rscript"), c(file.path(engine, "run_chain.R"),
      "--run-dir", run_path, "--engine-dir", engine), stdout = TRUE, stderr = TRUE))
    check(if (success) is.null(attr(out, "status")) else !is.null(attr(out, "status")))
    read_json(auto_path)
  }
  result <- invoke()
  check(result$code == "missing_manifest", result$status == "skipped",
    read_json(file.path(rd, "status", "step_state.json"))$status == "done")
  write_json(manifest, manifest_path)
  result <- invoke()
  check(result$status == "succeeded", result$code == "exported", !result$full_analysis_replay_verified)
  d <- nr_api_download(fixture, session)
  notebook <- jsonlite::fromJSON(rawToChar(d$bytes), simplifyVector = FALSE)
  check(notebook$nbformat == 4L, !d$full_analysis_replay_verified,
    readLines(file.path(rd, "r-output.txt")) == "synthetic R",
    readLines(file.path(rd, "py-output.txt")) == "synthetic Python")
  old_attempt <- result$attempt_id
  # New attempt, invalid declaration: calculation stays done, cached export becomes unavailable.
  bad <- manifest; bad$files[[1]]$path <- "absent.R"; write_json(bad, manifest_path)
  result <- invoke()
  check(result$status == "failed", result$code == "export_failed", result$attempt_id != old_attempt,
    read_json(file.path(rd, "status", "step_state.json"))$status == "done")
  denied_download <- function() {
    check(tryCatch({ nr_api_download(fixture, session); FALSE }, error = function(e) TRUE))
  }
  denied_download()
  write_json(manifest, manifest_path); result <- invoke(); check(result$status == "succeeded")
  attempt <- result$attempt_id
  build <- function(app_arg = fixture, engine_arg = engine, run_arg = rd, ok = TRUE, terminal = TRUE)
    nae_export(app_arg, engine_arg, run_arg, attempt, ok, terminal)
  expect_fail <- function(...) { r <- build(...); check(r$status == "failed"); r }
  # No science execution during packaging: synthetic marker script is embedded but never run.
  writeLines('from pathlib import Path\nPath("NEVER").write_text("bad")', file.path(rd, "never.py"))
  explicit <- manifest
  explicit$files[[3]] <- list(path = "never.py", role = "script")
  explicit$steps[[3]] <- list(command = "python3", argv = list("never.py"), cwd = ".")
  write_json(explicit, manifest_path); check(build()$status == "succeeded", !file.exists(file.path(rd, "NEVER")))
  write_json(manifest, manifest_path)
  # All manifest validation stays in nr_api_build, with no implicit input authorization.
  writeLines("synthetic", file.path(rd, "input.txt"))
  bad <- manifest; bad$files[[3]] <- list(path = "input.txt", role = "input")
  write_json(bad, manifest_path); expect_fail(); write_json(manifest, manifest_path)
  bad <- manifest; bad$unexpected <- TRUE; write_json(bad, manifest_path); expect_fail()
  write_json(manifest, manifest_path)
  # A forged binding cannot synthesize a session or cross the application boundary.
  for (field in c("session_id", "app_dir", "run_dir", "binding_id")) {
    wrong <- b; wrong[[field]] <- "foreign"; write_json(wrong, bp); expect_fail(); write_json(b, bp)
  }
  for (field in c("id", "mode", "run_dir")) {
    wrong <- session; wrong[[field]] <- "foreign"
    write_json(list(sessions = list(wrong)), sp); expect_fail()
  }
  write_json(list(sessions = list()), sp); expect_fail()
  write_json(list(sessions = list(session)), sp)
  # The same trusted owner partition as sessions.R wins over the legacy index.
  owner_index <- file.path(fixture, "state", "users", "local:default", "sessions.json")
  write_json(list(sessions = list()), owner_index); expect_fail()
  write_json(list(sessions = list(session)), owner_index)
  write_json(list(sessions = list()), sp); check(build()$status == "succeeded")
  unlink(owner_index); write_json(list(sessions = list(session)), sp)
  expect_fail(engine_arg = root); expect_fail(run_arg = "/"); expect_fail(app_arg = root)
  alias <- file.path(root, "alias"); check(file.symlink(rd, alias)); expect_fail(run_arg = alias)
  # The runner normalizes RUN_DIR before terminal processing; the hook must retain
  # the CLI path so normalization cannot launder a symlink into a trusted root.
  aliased <- invoke(alias)
  check(aliased$status == "failed", aliased$code == "unsafe_path",
    read_json(file.path(rd, "status", "step_state.json"))$status == "done")
  result <- invoke(); check(result$status == "succeeded")
  attempt <- result$attempt_id
  # Link refusal for declaration, binding, state index, module and status destination.
  for (p in c(manifest_path, bp, sp, file.path(engine, "notebook_replay_api.R"), auto_path)) {
    saved <- tempfile("saved-", tmpdir = root)
    check(file.rename(p, saved), file.symlink(saved, p))
    expect_fail(); unlink(p); check(file.rename(saved, p))
  }
  # Missing/active/failed phase/current attempt evidence cannot become export success.
  original <- read_json(mp)
  for (state in c("failed", "running", "stale")) {
    mm <- original; mm$phases[[1]]$state <- state; write_json(mm, mp); expect_fail()
  }
  mm <- original; mm$active_attempt <- "new-attempt"; write_json(mm, mp); expect_fail(); denied_download()
  write_json(original, mp); expect_fail(ok = FALSE); expect_fail(terminal = FALSE)
  check(build()$status == "succeeded", nr_api_download(fixture, session)$ok)
  # Exact-byte download checks remain fail closed.
  writeLines("changed", file.path(rd, "py-output.txt")); denied_download()
  # Status records never persist application/run paths or error text.
  text <- paste(readLines(auto_path), collapse = "")
  check(!grepl(root, text, fixed = TRUE), !grepl("Declared replay", text, fixed = TRUE),
    identical(read_json(auto_path)$binding_id, b$binding_id), identical(read_json(auto_path)$attempt_id, attempt))
  # A real failed computation cannot invoke successful terminal packaging.
  writeLines('stop("synthetic failure")', file.path(rd, "analysis.R"))
  failed <- invoke(success = FALSE)
  check(failed$status == "failed", failed$code == "incomplete_attempt",
    read_json(file.path(rd, "status", "step_state.json"))$status == "failed")
  denied_download()
  cat("notebook_auto_export:", checks, "checks passed (real runner R/Python subprocesses)\n")
}
run()
