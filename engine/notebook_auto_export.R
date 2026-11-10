# Terminal-only opt-in packaging. This does not discover dependencies or execute replay.
nae_export <- local({
  string <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  object <- function(x) is.list(x) && !is.null(names(x)) &&
    all(nzchar(names(x))) && !anyDuplicated(names(x))
  reject <- function(code) stop(structure(list(message = code, call = NULL, code = code),
    class = c("nae_error", "error", "condition")))
  no_links <- function(p) {
    repeat {
      link <- Sys.readlink(p)
      if (!is.na(link) && nzchar(link)) reject("unsafe_path")
      parent <- dirname(p)
      if (identical(parent, p)) break
      p <- parent
    }
    p
  }
  absolute <- function(p) {
    if (!string(p) || !startsWith(p, "/") || grepl("[\\\\[:cntrl:]]", p) ||
        any(strsplit(p, "/", fixed = TRUE)[[1]] %in% c(".", ".."))) reject("unsafe_path")
    no_links(p)
    if (!dir.exists(p) || identical(p, "/")) reject("unsafe_path")
    n <- normalizePath(p, winslash = "/", mustWork = TRUE)
    if (!identical(n, p)) reject("unsafe_path")
    n
  }
  checked <- function(p) {
    no_links(p)
    if (!file.exists(p) || dir.exists(p)) reject("missing_evidence")
    p
  }
  json <- function(p) {
    x <- jsonlite::fromJSON(checked(p), simplifyVector = FALSE)
    if (!object(x)) reject("invalid_evidence")
    x
  }
  function(app_dir, engine_dir, run_dir, attempt_id, ok_all, terminal_ok, run_path = run_dir) {
    # Caller supplies the application derived from the runner location, never from binding JSON.
    record <- list(schema = "pa.notebook.auto-export.v1", status = "failed", code = "export_failed",
      attempt_id = NULL, binding_id = NULL, full_analysis_replay_verified = FALSE)
    safe_status <- NULL
    result <- tryCatch({
      root <- absolute(run_dir)
      status <- absolute(file.path(root, "status"))
      safe_status <- file.path(status, "notebook_auto_export.json")
      no_links(safe_status)
      # run_chain normalizes RUN_DIR early; retain the original CLI spelling to reject aliases.
      if (!string(run_path)) reject("unsafe_path")
      no_links(run_path)
      if (!identical(normalizePath(run_path, winslash = "/", mustWork = TRUE), root)) reject("unsafe_path")
      if (!string(attempt_id) || !grepl("^[A-Za-z0-9_-]+$", attempt_id)) reject("invalid_attempt")
      record$attempt_id <- attempt_id
      app <- absolute(app_dir)
      engine <- absolute(engine_dir)
      if (!identical(engine, file.path(app, "engine"))) reject("foreign_engine")
      b <- json(file.path(status, "notebook_session.json"))
      if (!string(b$session_id) || !grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", b$session_id) ||
          !string(b$binding_id) || !grepl("^[A-Za-z0-9_-]+$", b$binding_id) ||
          !identical(b$app_dir, app) || !identical(b$run_dir, root)) reject("foreign_binding")
      record$binding_id <- b$binding_id
      # Load only trusted application modules, with no inherited global session resolver.
      e <- new.env(parent = baseenv())
      for (f in c("state_paths.R", "sessions.R"))
        sys.source(checked(file.path(engine, f)), envir = e)
      no_links(file.path(app, "state"))
      no_links(e$state_user_root(app))
      no_links(e$state_user_root(app, "legacy"))
      checked(e$sess_file(app))
      session <- e$sess_find(app, b$session_id)
      if (!is.list(session) || !identical(session$id, b$session_id) ||
          !identical(session$mode, "local") || !identical(session$run_dir, root)) reject("foreign_session")
      m <- json(file.path(status, "notebook_manifest.json"))
      if (!isTRUE(ok_all) || !isTRUE(terminal_ok) || !identical(m$binding_id, b$binding_id) ||
          !is.null(m$active_attempt) || !is.list(m$phases) || !length(m$phases) ||
          !is.null(names(m$phases)) || !all(vapply(m$phases, function(p)
            object(p) && identical(p$state, "succeeded"), TRUE)) ||
          !any(vapply(m$phases, function(p) identical(p$attempt_id, attempt_id), TRUE)))
        reject("incomplete_attempt")
      a <- json(file.path(status, "notebook_attempts", attempt_id, "status", "attempt.json"))
      if (!identical(a$attempt_id, attempt_id) || !identical(a$binding_id, b$binding_id) ||
          !isTRUE(a$execution_succeeded)) reject("incomplete_attempt")
      manifest_path <- file.path(root, "replay-manifest.json")
      no_links(manifest_path)
      if (!file.exists(manifest_path)) {
        record$status <- "skipped"; record$code <- "missing_manifest"
      } else {
        manifest <- json(manifest_path)
        if (!setequal(names(manifest), c("files", "steps", "expected_outputs"))) reject("invalid_manifest")
        sys.source(checked(file.path(engine, "notebook_replay_api.R")), envir = e)
        built <- e$nr_api_build(app, session, manifest)
        if (!isTRUE(built$ok)) reject("export_failed")
        record$status <- "succeeded"; record$code <- "exported"
      }
      record
    }, error = function(err) {
      record$code <- if (inherits(err, "nae_error")) err$code else "export_failed"
      record$status <- "failed"
      record
    })
    if (!is.null(safe_status)) tryCatch({
      no_links(safe_status); no_links(dirname(safe_status))
      tmp <- tempfile(".auto-export-", tmpdir = dirname(safe_status))
      on.exit(unlink(tmp), add = TRUE)
      writeLines(as.character(jsonlite::toJSON(result, auto_unbox = TRUE, null = "null")), tmp)
      if (!file.rename(tmp, safe_status)) reject("status_write_failed")
    }, error = function(e) {
      result$status <<- "failed"; result$code <<- "status_write_failed"
    })
    result
  }
})
