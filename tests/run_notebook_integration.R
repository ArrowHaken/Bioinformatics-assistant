#!/usr/bin/env Rscript
# All execution and state writes stay in a disposable fixture, never application state.
suppressPackageStartupMessages(library(jsonlite))
app <- normalizePath(".")
root <- tempfile("run-notebook-fixture-", tmpdir = "/tmp")
dir.create(root)
engine <- file.path(root, "engine"); dir.create(engine)
for (f in c("run_chain.R", "run_notebook.R", "notebook_build.R", "run_lifecycle.R"))
  stopifnot(file.copy(file.path(app, "engine", f), engine))
writeLines(c('CHAIN_ORDER <- c("r", "py")',
 'resolve_step_cli <- function(stepId, runDir, engineDir) {',
 ' list(cmd = if (stepId == "r") Sys.which("Rscript") else Sys.which("python3"),',
 ' args = c(file.path(engineDir, if (stepId == "r") "fixture.R" else "fixture.py"), "hello"),',
 ' cwd = runDir, title = paste("Fixture", stepId))', '}'), file.path(engine, "step_plan.R"))
rtext <- c('a <- commandArgs(trailingOnly = TRUE)', 'writeLines(paste("R_RESULT", a[1]), "r-result.txt")', 'cat("R_OUTPUT", a[1], "\\n")')
pytext <- c('import sys', 'from pathlib import Path', 'Path("py-result.txt").write_text("PY_RESULT " + sys.argv[1])', 'print("PY_OUTPUT", sys.argv[1])')
writeLines(rtext, file.path(engine, "fixture.R")); writeLines(pytext, file.path(engine, "fixture.py"))
run <- function(name) { d <- file.path(root, name); dir.create(d); d }
invoke <- function(d) system2(Sys.which("Rscript"), c(file.path(engine, "run_chain.R"), "--run-dir", d, "--engine-dir", engine), stdout = TRUE, stderr = TRUE)
d <- run("success"); invoke(d)
stopifnot(readLines(file.path(d, "r-result.txt")) == "R_RESULT hello", readLines(file.path(d, "py-result.txt")) == "PY_RESULT hello")
nb <- fromJSON(file.path(d, "reproducible.ipynb"), simplifyVector = FALSE)
cells <- nb$cells; texts <- vapply(cells, function(c) paste(unlist(c$source), collapse = ""), "")
stopifnot(any(startsWith(texts, "# ")), sum(startsWith(texts, "## ")) >= 3L,
 any(grepl("R_RESULT", texts, fixed = TRUE)), any(grepl("PY_RESULT", texts, fixed = TRUE)),
 identical(nb$metadata$pa$local_capture$capture_complete, TRUE),
 identical(nb$metadata$pa$local_capture$full_analysis_replay_verified, FALSE))
raw <- lapply(readLines(file.path(d, "status", "cells.jsonl")), fromJSON, simplifyVector = FALSE)
stopifnot(identical(vapply(raw[-1], `[[`, "", "step"), c("r", "py")))
outputs <- paste(vapply(cells, function(c) paste(unlist(c$outputs), collapse = ""), ""), collapse = "")
stopifnot(grepl("R_OUTPUT hello", outputs, fixed = TRUE), grepl("PY_OUTPUT hello", outputs, fixed = TRUE))
# Verify complete source literals, not only distinctive output strings.
r_literal <- paste(capture.output(dput(paste(rtext, collapse = "\n"))), collapse = "\n")
py_literal <- as.character(toJSON(paste(pytext, collapse = "\n"), auto_unbox = TRUE))
stopifnot(grepl(r_literal, raw[[2]]$code, fixed = TRUE), grepl(py_literal, raw[[3]]$code, fixed = TRUE))
# Actual replay: move the original scripts away, run the embedded cells without them.
stopifnot(file.rename(file.path(engine, "fixture.R"), file.path(engine, "fixture.R.hidden")))
stopifnot(file.rename(file.path(engine, "fixture.py"), file.path(engine, "fixture.py.hidden")))
for (c in raw[-1]) {
 f <- file.path(root, if (c$lang == "R") "replay.R" else "replay.py")
 writeLines(c$code, f)
 status <- system2(if (c$lang == "R") Sys.which("Rscript") else Sys.which("python3"), f)
 stopifnot(status == 0L)
}
file.rename(file.path(engine, "fixture.R.hidden"), file.path(engine, "fixture.R"))
file.rename(file.path(engine, "fixture.py.hidden"), file.path(engine, "fixture.py"))
# New invocation archives old capture, never mixes runs.
invoke(d); stopifnot(length(readLines(file.path(d, "status", "cells.jsonl"))) == 3L)
# Failed execution does not produce an apparently complete notebook (even after prior success).
writeLines('stop("fixture intentional failure")', file.path(engine, "fixture.R"))
failed_run <- invoke(d)
stopifnot(!is.null(attr(failed_run, "status")), attr(failed_run, "status") != 0L,
  !file.exists(file.path(d, "reproducible.ipynb")))
report <- fromJSON(file.path(d, "status", "run_notebook.json"))
stopifnot(!report$execution_succeeded, !report$capture_complete)
# Unsupported command and secrets are explicit, never fabricated code.
source(file.path(engine, "run_notebook.R"))
s <- rn_init(run("unsupported"), engine, "zip", "single")
r <- rn_prepare(s, "zip", list(cmd = "zip", args = c("-r", "x.zip"), cwd = d, title = "archive"))
stopifnot(!r$exportable); rn_record(s, r, 0L); report <- rn_finish(s, TRUE)
stopifnot(!report$capture_complete, is.null(report$notebook))
# A mixed supported/unsupported run still exports an explicitly incomplete notebook.
s <- rn_init(run("mixed-unsupported"), engine, c("py", "zip"), "chain")
r <- rn_prepare(s, "py", list(cmd = Sys.which("python3"), args = c(file.path(engine, "fixture.py"), "hello"), cwd = d, title = "Python"))
rn_record(s, r, 0L, "PY_OUTPUT hello")
r <- rn_prepare(s, "zip", list(cmd = "zip", args = c("-r", "x.zip"), cwd = d, title = "archive"))
rn_record(s, r, 0L)
report <- rn_finish(s, TRUE)
stopifnot(!report$capture_complete, file.exists(report$notebook))
mixed <- fromJSON(report$notebook, simplifyVector = FALSE)
stopifnot(any(vapply(mixed$cells, function(c) grepl("UNABLE TO EXPORT", paste(unlist(c$source), collapse = ""), fixed = TRUE), TRUE)))
# Shell-ambiguous tokens and interpreter modes must not be guessed.
for (a in list(c("-c", "print(1)"), c("fixture.py", "two words"))) {
 r <- rn_prepare(s, "py", list(cmd = Sys.which("python3"), args = a, cwd = d, title = "ambiguous"))
 stopifnot(!r$exportable)
}
Sys.setenv(TEST_API_TOKEN = "fixture-sensitive-value")
s <- rn_init(run("secret"), engine, "r", "single")
writeLines(c('cat("fixture-sensitive-value")', 'cat("safe")'), file.path(engine, "fixture.R"))
r <- rn_prepare(s, "r", list(cmd = Sys.which("Rscript"), args = c(file.path(engine, "fixture.R"), "--token", "fixture-sensitive-value"), cwd = d, title = "secret"))
stopifnot(!r$exportable); rn_record(s, r, 0L, "fixture-sensitive-value"); rn_finish(s, TRUE)
stopifnot(!any(grepl("fixture-sensitive-value", readLines(file.path(s$run_dir, "status", "cells.jsonl")), fixed = TRUE)))
Sys.unsetenv("TEST_API_TOKEN")
cat("PASS: real R/Python execution, embedded replay, order, outputs, rerun isolation, failure, unsupported command, credential withholding\n")
cat("Fixture evidence:", root, "\n")
