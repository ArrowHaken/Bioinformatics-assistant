#!/usr/bin/env Rscript
# Disposable real runner fixture; do not source the lifecycle test's side effects.
source("engine/runs.R")
source("engine/notebook_auto_export.R")
source("engine/notebook_replay_api.R")
app <- normalizePath(".")
checks <- 0L
check <- function(...) { stopifnot(...); checks <<- checks + 1L }
write_json <- function(x, p) {
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  writeLines(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")), p)
}
read_json <- function(p) jsonlite::fromJSON(p, simplifyVector = FALSE)
main <- function() {
  root <- tempfile("runs-auto-", tmpdir = "/tmp"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- file.path(root, "app"); engine <- file.path(fixture, "engine")
  dir.create(engine, recursive = TRUE)
  for (f in c("run_chain.R", "run_notebook.R", "notebook_build.R", "run_lifecycle.R",
              "notebook_replay.R", "notebook_replay_api.R", "notebook_auto_export.R", "sessions.R", "state_paths.R"))
    check(file.copy(file.path(app, "engine", f), engine))
  writeLines(c('CHAIN_ORDER <- "r"',
    'resolve_step_cli <- function(stepId, runDir, engineDir) list(cmd = Sys.which("Rscript"),',
    'args = file.path(runDir, "analysis.R"), cwd = runDir, title = stepId)'), file.path(engine, "step_plan.R"))
  rd <- file.path(root, "run"); dir.create(rd)
  sess <- list(id = "runs_auto", mode = "local", run_dir = rd)
  write_json(list(sessions = list(sess)), file.path(fixture, "state", "sessions.json"))
  sd <- file.path(rd, "status")
  bp <- file.path(sd, "notebook_session.json")
  binding <- list(session_id = sess$id, app_dir = fixture, run_dir = rd, binding_id = "bind-auto")
  write_json(binding, bp)
  writeLines('writeLines("synthetic", "output.txt")', file.path(rd, "analysis.R"))
  write_json(list(files = list(list(path = "analysis.R", role = "script")),
    steps = list(list(command = "Rscript", argv = list("analysis.R"), cwd = ".")),
    expected_outputs = list("output.txt")), file.path(rd, "replay-manifest.json"))
  out <- system2(Sys.which("Rscript"), c(file.path(engine, "run_chain.R"),
    "--run-dir", rd, "--engine-dir", engine), stdout = TRUE, stderr = TRUE)
  check(is.null(attr(out, "status")))
  ap <- file.path(sd, "notebook_auto_export.json")
  mp <- file.path(sd, "notebook_manifest.json")
  original <- read_json(ap); manifest <- read_json(mp)
  reader <- function() runs_local_lifecycle(fixture, sess)
  receipt <- function(x) write_json(x, ap)
  unknown <- function(execution = "execution_completed") {
    x <- reader()
    check(identical(x$state, execution), identical(x$notebook_export_state, "unknown"),
      is.null(x$notebook_export_code), identical(x$full_analysis_replay_verified, FALSE),
      identical(x$scientific_validity_verified, FALSE))
  }
  check(original$status == "succeeded", reader()$notebook_export_state == "ready",
    nr_api_download(fixture, sess)$ok)
  sentence <- runs_state_sentence(reader())
  check(grepl("已生成清单 notebook，下载时重新校验", sentence, fixed = TRUE),
    grepl("仅生成记录", sentence, fixed = TRUE), grepl("均未验证", sentence, fixed = TRUE))
  # The receipt survives changed bytes, while the real download rejects them.
  writeLines("modified", file.path(rd, "output.txt"))
  check(reader()$notebook_export_state == "ready", identical(runs_state_sentence(reader()), sentence),
    tryCatch({ nr_api_download(fixture, sess); FALSE }, error = function(e) TRUE))
  failed_codes <- c("unsafe_path", "missing_evidence", "invalid_evidence", "invalid_attempt",
    "foreign_engine", "foreign_binding", "foreign_session", "incomplete_attempt",
    "invalid_manifest", "export_failed", "status_write_failed")
  for (status in c("succeeded", "skipped", "failed", "unknown")) {
    for (code in c("exported", "missing_manifest", failed_codes, "arbitrary /private/path")) {
      x <- original; x$status <- status; x$code <- code; receipt(x)
      expected <- if (status == "succeeded" && code == "exported") "ready"
        else if (status == "skipped" && code == "missing_manifest") "not_configured"
        else if (status == "failed" && code %in% failed_codes) "failed" else "unknown"
      got <- reader()
      check(identical(got$state, "execution_completed"), identical(got$notebook_export_state, expected),
        identical(got$notebook_export_code, if (expected == "unknown") NULL else code))
    }
  }
  for (field in names(original)) {
    for (bad in list(NULL, TRUE, 1, list("a", "b"))) {
      x <- original; x[field] <- list(bad); receipt(x); unknown()
    }
  }
  for (field in c("schema", "attempt_id", "binding_id")) {
    x <- original; x[[field]] <- "foreign"; receipt(x); unknown()
  }
  x <- original; x$full_analysis_replay_verified <- TRUE; receipt(x); unknown()
  for (field in c("error", "path", "extra")) {
    x <- original; x[[field]] <- "/private/secret"; receipt(x); unknown()
    check(!grepl("/private/secret", jsonlite::toJSON(reader()), fixed = TRUE))
  }
  receipt(original)
  text <- paste(readLines(ap), collapse = "")
  writeLines(sub('"schema":', '"schema":"duplicate","schema":', text, fixed = TRUE), ap); unknown()
  writeLines("broken", ap); unknown()
  unlink(ap); unknown()
  receipt(original)
  saved <- paste0(ap, "-saved")
  check(file.rename(ap, saved), file.symlink(saved, ap)); unknown()
  unlink(ap); check(file.rename(saved, ap))
  for (state in c("failed", "running", "stale", "queued", "unknown")) {
    m <- manifest; m$phases[[1]]$state <- state; write_json(m, mp)
    unknown(if (state == "stale") "unknown" else "execution_completed")
  }
  m <- manifest; m$active_attempt <- original$attempt_id; write_json(m, mp); unknown()
  write_json(manifest, mp)
  # Existing execution status is never replaced by a receipt, even if its timestamps are newer.
  lp <- file.path(sd, paste0("lifecycle-", original$attempt_id, ".rds"))
  life <- readRDS(lp)
  for (status in c("failed", "cancelled", "running", "queued")) {
    l <- life; l$status <- status; l$heartbeat_at <- as.numeric(Sys.time())
    saveRDS(l, lp); unknown(status)
  }
  saveRDS(life, lp)
  for (field in c("session_id", "app_dir", "run_dir", "binding_id")) {
    b <- binding; b[[field]] <- "foreign"; write_json(b, bp); unknown("unknown")
  }
  write_json(binding, bp)
  proof <- file.path(sd, "notebook_attempts", original$attempt_id, "status", "attempt.json")
  identity <- read_json(proof)
  for (field in c("attempt_id", "binding_id")) {
    x <- identity; x[[field]] <- "foreign"; write_json(x, proof); unknown("unknown")
  }
  write_json(identity, proof)
  old <- original; old$attempt_id <- "previous-attempt"; receipt(old)
  Sys.setFileTime(ap, Sys.time() + 3600); unknown()
  receipt(original)
  env <- new.env(parent = globalenv()); sys.source("engine/runs.R", env)
  env$nr_api_download <- function(...) stop("must not inspect notebook bytes")
  env$remote_steps_read <- function(...) list(steps = list())
  env$flow_prog_read <- function(...) list(stages = list())
  check(env$runs_local_lifecycle(fixture, sess)$notebook_export_state == "ready",
    length(env$runs_list(fixture, "other-owner", list())) == 0L)
  for (state in c("not_configured", "failed", "unknown", "injected /private/path")) {
    row <- list(state = "execution_completed", notebook_export_state = state)
    s <- runs_state_sentence(row)
    check(grepl("已完成计算", s, fixed = TRUE), !grepl("/private/path", s, fixed = TRUE),
      grepl("未验证", s, fixed = TRUE))
  }
  cat("PASS runs_auto_export:", checks, "checks (real run_chain export and changed-output download refusal)\n")
}
main()
