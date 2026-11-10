# Run from app: Rscript --vanilla tests/local_launch_contract.R
# --reproduce records the original mismatch before applying the fix.
# Contract only: no real entry or scientific script is executed. Gate decisions
# are explicit fixtures; this does not certify gate readiness or scientific output.
local({
  stopifnot(requireNamespace("jsonlite", quietly = TRUE))
  ast <- parse("serve.R")
  assignment <- function(expr, name) {
    is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name(name))
  }
  get_assignment <- function(name, expressions = ast) {
    hit <- Filter(function(x) assignment(x, name), as.list(expressions))
    stopifnot(length(hit) == 1L)
    hit[[1]]
  }
  e <- new.env(parent = globalenv())
  # Match source ownership, not an invented local step list.
  plan_binding <- get_assignment("PLAN_FILE")
  entry_binding <- get_assignment("ENTRY_FILE")
  e$ENGINE_DIR <- normalizePath("engine")
  eval(plan_binding, e)
  eval(entry_binding, e)
  entry_ast <- parse(e$ENTRY_FILE)
  stopifnot(identical(get_assignment("PLAN_FILE", entry_ast), plan_binding))
  sources_plan <- function(x) {
    is.call(x) && identical(x[[1]], as.name("source")) &&
      identical(x[[2]], as.name("PLAN_FILE")) && isTRUE(x$local)
  }
  stopifnot(any(vapply(as.list(ast), sources_plan, logical(1))),
            any(vapply(as.list(entry_ast), sources_plan, logical(1))))
  sys.source(e$PLAN_FILE, envir = e)
  sys.source("engine/flow_spec.R", envir = e)
  e$HAS_FLOW <- TRUE
  eval(get_assignment("STEP_ORDER"), e)
  stopifnot(identical(e$STEP_ORDER, e$FLOW_ORDER("server")),
            "pipeline1" %in% e$CHAIN_ORDER,
            !("pipeline1" %in% e$STEP_ORDER),
            "doublet" %in% e$STEP_ORDER,
            !("doublet" %in% e$CHAIN_ORDER))
  for (name in c("write_json_file", "status_dir", "step_state_file",
                 "step_log_file", "read_step_state", "step_is_running", "launch_step")) {
    eval(get_assignment(name), e)
  }
  e$`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
  if (.Platform$OS.type == "windows") stop("POSIX stub executable required")
  root <- tempfile("local-launch-contract-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  stub <- file.path(root, "Rscript-stub")
  # Persist actual argv after the asynchronous system2 child starts. The real
  # ENTRY_FILE remains argv[2], but is never sourced by this executable.
  writeLines(c("#!/bin/sh", "for arg do run_dir=$arg; done",
               "printf '%s\\n' \"$@\" > \"$run_dir/argv.tmp\"",
               "mv \"$run_dir/argv.tmp\" \"$run_dir/argv.sentinel\""), stub)
  Sys.chmod(stub, "0755")
  serial <- 0L
  fixture <- function() {
    serial <<- serial + 1L
    rd <- file.path(root, paste0("run-", serial))
    dir.create(rd)
    e$write_json_file(file.path(rd, "pipeline.json"),
                     list(environment = list(rscript_path = stub)))
    rd
  }
  gate_calls <- character(0)
  pending <- character(0)
  e$build_steps <- function(...) {
    lapply(e$CHAIN_ORDER, function(id) list(id = id, needGate = e$step_needs_gate(id)))
  }
  e$pending_gate_names <- function(runDir, ph) {
    gate_calls <<- c(gate_calls, ph)
    pending
  }
  await_argv <- function(rd) {
    sentinel <- file.path(rd, "argv.sentinel")
    for (i in seq_len(200L)) {
      if (file.exists(sentinel)) return(readLines(sentinel, warn = FALSE))
      Sys.sleep(0.025)
    }
    stop("child sentinel missing: ", rd)
  }
  if ("--reproduce" %in% commandArgs(TRUE)) {
    p <- fixture()
    p_result <- e$launch_step("pipeline1", p)
    d <- fixture()
    d_result <- e$launch_step("doublet", d)
    stopifnot(!p_result$ok, !file.exists(e$step_state_file(p)), d_result$ok)
    argv <- await_argv(d)
    stopifnot(identical(argv, c("--vanilla", e$ENTRY_FILE, "run", "--step",
                              "doublet", "--force", "--dir", d)))
    cat("REPRO: pipeline1 rejected before state; doublet started with real child argv sentinel\n")
    return(invisible(NULL))
  }
  accepted <- function(id) {
    rd <- fixture()
    gate_calls <<- character(0)
    result <- e$launch_step(id, rd)
    stopifnot(isTRUE(result$ok), identical(result$started, id))
    expected <- c("--vanilla", e$ENTRY_FILE,
                  if (identical(id, "all")) "run-all" else c("run", "--step", id, "--force"),
                  "--dir", rd)
    stopifnot(identical(await_argv(rd), expected))
    state <- e$read_json_file(e$step_state_file(rd))
    stopifnot(identical(state$status, "running"), identical(state$step, id))
    expected_gates <- if (identical(id, "all")) c("A", "B") else e$step_gate_phase(id)
    stopifnot(identical(gate_calls, if (is.null(expected_gates)) character(0) else expected_gates))
  }
  for (id in c(e$CHAIN_ORDER, "all")) accepted(id)
  cat("PASS: every CHAIN_ORDER id and all; real child argv/state sentinels; fixture gates\n")
  rejected <- function(id) {
    rd <- fixture()
    result <- e$launch_step(id, rd)
    stopifnot(identical(result$ok, FALSE), is.character(result$error),
              length(result$error) == 1L, !dir.exists(e$status_dir(rd)),
              !file.exists(file.path(rd, "argv.sentinel")))
    # A rejection must also leave an existing idle state byte-for-byte intact.
    e$write_json_file(e$step_state_file(rd), list(status = "idle", step = "env"))
    before <- readBin(e$step_state_file(rd), "raw", n = 10000L)
    stopifnot(identical(e$launch_step(id, rd)$ok, FALSE),
              identical(before, readBin(e$step_state_file(rd), "raw", n = 10000L)))
    Sys.sleep(0.05)
    stopifnot(!file.exists(file.path(rd, "argv.sentinel")))
  }
  for (id in setdiff(e$STEP_ORDER, e$CHAIN_ORDER)) rejected(id)
  for (id in list("unknown", "", character(0), NULL, NA_character_,
                  c("pipeline1", "pipeline2"), c("all", "env"), 1L, list("env"))) rejected(id)
  cat("PASS: flow-only/unknown/empty/multivalue steps rejected without state mutation or child\n")
  # Gate enforcement remains separate from membership and still blocks locally.
  pending <- "fixture unanswered gate"
  rd <- fixture()
  result <- e$launch_step("pipeline1", rd)
  stopifnot(!result$ok, isTRUE(result$blocked), !dir.exists(e$status_dir(rd)))
  cat("PASS: pipeline1 still blocked by unsatisfied A gate; no scientific computation performed\n")
})
