# Local-runner notebook capture. The builder remains the sole notebook assembler.
rn_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA))
rn_sensitive <- function(x) grepl("(?i)(password|passwd|secret|token|api[_-]?key|authorization|credential|private[_-]?key)", x, perl = TRUE)
rn_secrets <- function(args) {
  env <- Sys.getenv()
  values <- unname(env[rn_sensitive(names(env)) & nchar(env) >= 4L])
  for (i in seq_along(args)) if (rn_sensitive(args[i])) {
    if (grepl("=", args[i], fixed = TRUE)) values <- c(values, sub("^[^=]*=", "", args[i]))
    else if (i < length(args)) values <- c(values, args[i + 1L])
  }
  unique(values[nchar(values) >= 1L])
}
rn_clean <- function(x, secrets = character()) {
  for (s in secrets) x <- gsub(s, "[REDACTED]", x, fixed = TRUE)
  x <- gsub("(?i)(https?://)[^/@[:space:]]+:[^/@[:space:]]+@", "\\1[REDACTED]@", x, perl = TRUE)
  gsub("(?i)((?:password|passwd|secret|token|api[_-]?key|authorization|credential)[[:space:]\"']*[:=][[:space:]\"']*)[^[:space:]\"',;]+", "\\1[REDACTED]", x, perl = TRUE)
}
rn_init <- function(run_dir, engine_dir, todo, mode) {
  e <- new.env(parent = baseenv())
  sys.source(file.path(engine_dir, "notebook_build.R"), envir = e)
  s <- new.env(parent = emptyenv())
  s$builder <- e; s$run_dir <- run_dir; s$records <- list(); s$todo <- as.character(todo); s$mode <- mode
  s$attempt_id <- e$nb_identity()
  s$attempt_dir <- e$nb_attempt_dir(run_dir, s$attempt_id)
  e$nb_with_lock(run_dir, function() {
    manifest <- e$nb_read_json(e$nb_manifest_file(run_dir))
    if (!is.null(manifest$active_attempt)) stop("Notebook invocation already active; concurrent run_dir use rejected")
    binding <- e$nb_read_json(file.path(run_dir, "status", "notebook_session.json"))
    s$binding_id <- binding$binding_id
    if (is.null(manifest) || !identical(manifest$binding_id, binding$binding_id))
      manifest <- list(schema = "pa.notebook.attempts.v1", binding_id = binding$binding_id, phases = list())
    phases <- manifest$phases
    steps <- vapply(phases, function(x) x$step, "")
    changed <- which(steps %in% s$todo)
    if (length(changed)) for (i in seq_along(phases)) {
      if (i > min(changed)) phases[[i]]$state <- "stale"
    }
    for (step in s$todo) {
      entry <- list(step = step, attempt_id = s$attempt_id, state = "running")
      hit <- match(step, steps)
      if (is.na(hit)) { phases[[length(phases) + 1L]] <- entry; steps <- c(steps, step) }
      else phases[[hit]] <- entry
    }
    manifest$phases <- phases; manifest$active_attempt <- s$attempt_id
    e$nb_write_json(manifest, e$nb_manifest_file(run_dir))
    dir.create(file.path(s$attempt_dir, "status"), recursive = TRUE, showWarnings = FALSE)
    e$nb_write_json(list(attempt_id = s$attempt_id, binding_id = binding$binding_id,
      todo = as.list(s$todo), state = "running"), file.path(s$attempt_dir, "status", "attempt.json"))
    unlink(c(e$nb_cells_file(run_dir), file.path(run_dir, "reproducible.ipynb"),
      file.path(run_dir, "status", "run_notebook.json")))
  })
  s$secrets <- rn_secrets(character()); s$capture_ok <- TRUE
  s$limitations <- paste(
    "This is a local-runner execution snapshot, not a verified full-analysis replay.",
    "Only the directly invoked script is embedded; sourced/imported modules, input data, configuration files and packages are external dependencies.",
    "Replay starts a fresh subprocess using a temporary script: commandArgs --file, Python __file__, sys.path[0], R sys.frame/ofile and script-relative imports may differ.",
    "Original argv and cwd are recorded; absolute paths and external inputs must exist or be adapted.",
    "Environment values are intentionally not exported; credentials must be supplied separately.",
    "Interpreter startup, package versions and external services are not frozen. Captured output is evidence of the original run, not notebook re-execution.",
    "The generator's generic ProjectConfig instructions do not apply to these unmodified script snapshots.")
  rn_append(s, list(level = 2L, step = "00", title = "Execution context and limitations",
    note = paste(s$limitations, "\nRunner environment:", R.version.string,
                 "\nPlatform:", R.version$platform, "\nLocale:", Sys.getlocale(),
                 "\nR library paths:", paste(.libPaths(), collapse = "; "),
                 "\nR session/package context:\n", paste(capture.output(sessionInfo()), collapse = "\n"),
                 "\nSelected steps:", paste(todo, collapse = " -> "), "\nMode:", mode)))
  s
}
rn_append <- function(s, cell) {
  # Sanitize the serialized object too: titles, context and script paths can contain secrets.
  safe <- jsonlite::fromJSON(rn_clean(rn_json(cell), s$secrets), simplifyVector = FALSE)
  s$builder$nb_with_lock(s$run_dir, function() {
    manifest <- s$builder$nb_read_json(s$builder$nb_manifest_file(s$run_dir))
    if (!identical(manifest$active_attempt, s$attempt_id)) stop("Notebook attempt is no longer active")
    safe$attempt_id <- s$attempt_id
    if (!isTRUE(s$builder$nb_append_cell(s$attempt_dir, safe))) stop("Notebook capture append failed")
    # Compatibility snapshot only; export provenance never reads this shared path.
    if (!file.copy(s$builder$nb_cells_file(s$attempt_dir), s$builder$nb_cells_file(s$run_dir), overwrite = TRUE))
      stop("Cannot refresh current notebook snapshot")
  })
}
rn_prepare <- function(s, step, r) {
  a <- as.character(r$args); cmd <- as.character(r$cmd); cwd <- normalizePath(r$cwd, mustWork = FALSE)
  s$secrets <- unique(c(s$secrets, rn_secrets(a)))
  rec <- list(step = step, title = r$title, command = cmd, argv = as.list(a), cwd = cwd,
              exportable = FALSE, reason = "Unsupported command or interpreter flags", lang = NULL, code = NULL)
  bin <- tolower(sub("\\.exe$", "", basename(cmd)))
  lang <- if (bin == "rscript") "R" else if (grepl("^python([0-9]+(\\.[0-9]+)*)?$", bin)) "python" else NULL
  # Only direct script commands are supported. Shell expressions and interpreter switches are not guessed.
  if (is.null(lang) || !length(a) || startsWith(a[1], "-")) return(rec)
  if (any(grepl("[[:space:]'\"`$;&|<>\\\\]", a))) {
    rec$reason <- "Ambiguous shell quoting in system2 argv; cannot safely reconstruct actual argv"; return(rec)
  }
  path <- if (grepl("^(/|[A-Za-z]:)", a[1])) a[1] else file.path(cwd, a[1])
  if (!file.exists(path) || dir.exists(path)) { rec$reason <- "Invoked script is not readable"; return(rec) }
  code <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  rec$lang <- lang
  if (rn_sensitive(code) || any(vapply(s$secrets, function(v) grepl(v, code, fixed = TRUE), TRUE)) ||
      any(rn_sensitive(a)) || !identical(rn_clean(paste(cmd, cwd, a, collapse = "\n"), s$secrets), paste(cmd, cwd, a, collapse = "\n"))) {
    rec$reason <- "Potential credentials in script or command context: code export withheld"; return(rec)
  }
  candidates <- a[-1]
  input_paths <- vapply(candidates, function(p) if (grepl("^(/|[A-Za-z]:)", p)) p else file.path(cwd, p), "")
  input_paths <- unique(input_paths[file.exists(input_paths)])
  rec$inputs <- lapply(input_paths, function(p) list(path = normalizePath(p, mustWork = FALSE),
    kind = if (dir.exists(p)) "directory (contents not inventoried)" else "file",
    bytes = unname(file.info(p)$size),
    modified = as.character(file.info(p)$mtime)))
  rec$script <- normalizePath(path, mustWork = TRUE); rec$script_text <- code
  rec$md5 <- unname(tools::md5sum(path)); rec$exportable <- TRUE; rec$reason <- NULL
  # Embed the complete script, then execute it in a fresh process. Never source the original path.
  if (lang == "R") {
    literal <- function(x) paste(capture.output(dput(x)), collapse = "\n")
    rec$code <- paste0("local({\n  script_text <- ", literal(code),
      "\n  script <- tempfile(fileext = '.R')\n  on.exit(unlink(script), add = TRUE)\n",
      "  writeLines(script_text, script, useBytes = TRUE)\n  old <- getwd()\n  on.exit(setwd(old), add = TRUE)\n  setwd(", literal(cwd), ")\n",
      "  status <- system2(", literal(cmd), ", shQuote(c(script, ", literal(a[-1]), ")))\n",
      "  if (status != 0L) stop('Captured R step failed: ', status)\n})\n")
  } else {
    rec$code <- paste0("import os, subprocess, tempfile\nscript_text = ", rn_json(code),
      "\nwith tempfile.TemporaryDirectory() as _capture_dir:\n    _script = os.path.join(_capture_dir, ", rn_json(basename(path)), ")\n",
      "    with open(_script, 'w', encoding='utf-8') as _f:\n        _f.write(script_text)\n",
      "    subprocess.run([", rn_json(cmd), ", _script] + ",
      as.character(jsonlite::toJSON(as.list(a[-1]), auto_unbox = TRUE)), ", cwd=", rn_json(cwd), ", check=True)\n")
  }
  rec
}
rn_record <- function(s, rec, exit_code, output = "") {
  rec$exit_code <- exit_code
  rec$output <- rn_clean(output, s$secrets)
  note <- paste("Command context (argv as provided to system2):", rn_clean(rn_json(list(command = rec$command,
    argv = rec$argv, cwd = rec$cwd, script = rec$script, md5 = rec$md5, inputs = rec$inputs)), s$secrets),
    "\nExit code:", exit_code,
    if (!isTRUE(rec$exportable)) paste("\nUNABLE TO EXPORT:", rec$reason) else "\nFull directly invoked script embedded below.")
  rn_append(s, list(level = 2L, step = rec$step, title = rec$title, note = note,
    lang = rec$lang, code = rec$code, exit_code = exit_code))
  s$records[[length(s$records) + 1L]] <- rec
}
rn_finish <- function(s, success) {
  complete <- isTRUE(success) && isTRUE(s$capture_ok) && length(s$records) == length(s$todo) &&
    all(vapply(s$records, function(x) isTRUE(x$exportable) && identical(as.integer(x$exit_code), 0L), TRUE))
  report <- list(execution_succeeded = isTRUE(success), capture_complete = complete,
    full_analysis_replay_verified = FALSE, limitations = s$limitations,
    steps = lapply(s$records, function(x) list(step = x$step, exportable = x$exportable, reason = x$reason, exit_code = x$exit_code)))
  path <- file.path(s$attempt_dir, "reproducible.ipynb")
  report$notebook <- NULL
  if (isTRUE(success)) {
    built <- s$builder$nb_build(s$attempt_dir, main_title = if (complete)
      "Analysis execution snapshot — replay not verified" else "INCOMPLETE analysis execution snapshot")
    if (isTRUE(built$ok)) {
      nb <- built$notebook
      nb$metadata$pa$local_capture <- report
      # Builder deliberately leaves outputs empty; attach only sanitized original-run evidence.
      code_i <- which(vapply(nb$cells, function(c) identical(c$cell_type, "code"), TRUE))
      executable <- Filter(function(x) isTRUE(x$exportable), s$records)
      if (length(executable)) for (j in seq_along(executable)) {
        k <- tail(code_i, length(executable))[j]
        nb$cells[[k]]$outputs <- list(list(output_type = "stream", name = "stdout",
          text = as.list(s$builder$nb_split_lines(executable[[j]]$output))))
      }
      s$builder$nb_write(nb, path); report$notebook <- path
    } else { report$build_error <- built$error; report$capture_complete <- FALSE }
  }
  report$attempt_id <- s$attempt_id
  report$binding_id <- s$binding_id
  s$builder$nb_with_lock(s$run_dir, function() {
    manifest <- s$builder$nb_read_json(s$builder$nb_manifest_file(s$run_dir))
    if (!identical(manifest$active_attempt, s$attempt_id)) stop("Notebook attempt is no longer active")
    for (i in seq_along(manifest$phases)) if (identical(manifest$phases[[i]]$attempt_id, s$attempt_id)) {
      phase_records <- Filter(function(x) identical(x$step, manifest$phases[[i]]$step), s$records)
      manifest$phases[[i]]$state <- if (isTRUE(success) && isTRUE(s$capture_ok) &&
        length(phase_records) == 1L && identical(as.integer(phase_records[[1]]$exit_code), 0L)) "succeeded" else "failed"
    }
    if (!is.null(report$notebook)) {
      current <- file.path(s$run_dir, "reproducible.ipynb")
      s$builder$nb_write(nb, current)
      report$notebook <- current
    }
    report$state <- if (isTRUE(success)) "succeeded" else "failed"
    s$builder$nb_write_json(report, file.path(s$attempt_dir, "status", "attempt.json"))
    s$builder$nb_write_json(report, file.path(s$run_dir, "status", "run_notebook.json"))
    manifest$active_attempt <- NULL
    s$builder$nb_write_json(manifest, s$builder$nb_manifest_file(s$run_dir))
  })
  report
}
