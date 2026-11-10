# Service API, not an HTTP handler. Caller MUST authorize the database session owner
# before either function (including before any filesystem probe). session is the DB
# row, not POST data; mode must be explicitly local. Only manifest is user POST data.
# Source this module once. It exports only nr_api_build and nr_api_download plus a
# private closure holder. Trusted notebook helpers load privately, never analysis.
# Build returns metadata; download returns exact raw bytes, filename/content_type.
# Errors intentionally have no filesystem paths; caller may map them to HTTP errors.
# Hash baselines describe explicit real outputs, not complete/scientific validation.
.nr_api <- local({
  fail <- function() stop("Declared replay unavailable or invalid", call. = FALSE)
  string <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  object <- function(x) is.list(x) && !is.null(names(x)) &&
    all(nzchar(names(x))) && !anyDuplicated(names(x))
  no_links <- function(p) {
    repeat {
      link <- Sys.readlink(p)
      if (!is.na(link) && nzchar(link)) fail()
      parent <- dirname(p)
      if (identical(parent, p)) break
      p <- parent
    }
  }
  absolute <- function(p, exists = TRUE) {
    if (!string(p) || !startsWith(p, "/") || grepl("[\\\\[:cntrl:]]", p) ||
        any(strsplit(p, "/", fixed = TRUE)[[1]] %in% c(".", ".."))) fail()
    no_links(p)
    if (exists && !dir.exists(p)) fail()
    normalizePath(p, winslash = "/", mustWork = exists)
  }
  checked <- function(p) {
    no_links(p)
    if (!file.exists(p) || dir.exists(p)) fail()
    p
  }
  hash <- function(p) digest::digest(file = checked(p), algo = "sha256")
  json <- function(p) {
    x <- jsonlite::fromJSON(checked(p), simplifyVector = FALSE)
    if (!object(x)) fail()
    x
  }
  encode <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA))
  guarded <- function(action) tryCatch(action(), error = function(e) fail())
  context <- function(app_dir, session) {
    if (!is.list(session) || !identical(session$mode, "local") || !string(session$id) ||
        !grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", session$id)) fail()
    app <- absolute(app_dir); root <- absolute(session$run_dir)
    if (identical(root, "/")) fail()
    e <- new.env(parent = baseenv())
    for (f in c("notebook_build.R", "notebook_replay.R"))
      sys.source(checked(file.path(app, "engine", f)), envir = e)
    store <- e$nb_store_dir(app, session$id)
    no_links(store)
    list(app = app, root = root, sid = session$id, store = store, e = e)
  }
  snapshot <- function(c) {
    binding_path <- file.path(c$root, "status", "notebook_session.json")
    binding <- json(binding_path)
    if (!identical(binding$session_id, c$sid) || !identical(binding$app_dir, c$app) ||
        !identical(binding$run_dir, c$root) || !string(binding$binding_id)) fail()
    manifest_path <- file.path(c$root, "status", "notebook_manifest.json")
    m <- json(manifest_path)
    if (!identical(m$binding_id, binding$binding_id) || !is.null(m$active_attempt) ||
        !is.list(m$phases) || !length(m$phases) || !is.null(names(m$phases))) fail()
    phases <- lapply(m$phases, function(p) {
      if (!object(p) || !string(p$step) || !identical(p$state, "succeeded") ||
          !string(p$attempt_id) || !grepl("^[A-Za-z0-9_-]+$", p$attempt_id)) fail()
      f <- file.path(c$root, "status", "notebook_attempts", p$attempt_id, "status", "attempt.json")
      a <- json(f)
      if (!identical(a$attempt_id, p$attempt_id) || !identical(a$binding_id, binding$binding_id) ||
          !isTRUE(a$execution_succeeded)) fail()
      list(step = p$step, attempt_id = p$attempt_id, identity_sha256 = hash(f))
    })
    if (anyDuplicated(vapply(phases, function(p) p$step, ""))) fail()
    list(binding_id = binding$binding_id, binding_sha256 = hash(binding_path),
         phase_manifest_sha256 = hash(manifest_path), phases = phases)
  }
  lock <- function(c, action) {
    # Same lock/order as local runner and nb_sync_local: root, then store.
    no_links(file.path(c$root, "status", "cells.jsonl.lock"))
    c$e$nb_with_lock(c$root, function() {
      before <- snapshot(c)
      no_links(file.path(c$store, "status", "cells.jsonl.lock"))
      c$e$nb_with_lock(c$store, function() action(before))
    })
  }
  inventories <- function(m) lapply(c(m$files, m$expected_outputs), function(f)
    f[c("path", "bytes", "sha256")])
  verify_files <- function(c, inventory) {
    if (!is.list(inventory) || !length(inventory)) fail()
    for (f in inventory) {
      if (!object(f) || !string(f$path) || !is.numeric(f$bytes) || length(f$bytes) != 1L ||
          !is.finite(f$bytes) || f$bytes < 0 || !string(f$sha256)) fail()
      p <- c$e$nbr_path(c$root, f$path)
      if (unname(file.info(p)$size) != f$bytes || !identical(hash(p), f$sha256)) fail()
    }
  }
  filename <- "declared-replay.ipynb"
  sidecar_name <- "declared-replay.provenance.json"
  build <- function(app_dir, session, manifest) guarded(function() {
    if (!object(manifest) || !all(names(manifest) %in% c("files", "steps", "expected_outputs"))) fail()
    c <- context(app_dir, session)
    lock(c, function(before) {
      built <- c$e$nbr_build(c$root, manifest)
      inventory <- inventories(built$manifest)
      verify_files(c, inventory)
      if (!identical(before, snapshot(c))) fail()
      dst <- file.path(c$store, filename); side <- file.path(c$store, sidecar_name)
      no_links(dst); no_links(side)
      tmp <- tempfile(".declared-", tmpdir = c$store)
      meta <- tempfile(".declared-meta-", tmpdir = c$store)
      on.exit(unlink(c(tmp, meta)), add = TRUE)
      c$e$nbr_write(built, tmp)
      record <- list(schema = "pa.notebook.declared-export.v1", session_id = c$sid,
        app_dir = c$app, run_dir = c$root, provenance = before,
        manifest_sha256 = digest::digest(encode(manifest), algo = "sha256", serialize = FALSE),
        manifest = manifest, inventory = inventory, export_sha256 = hash(tmp),
        export_bytes = unname(file.info(tmp)$size), full_analysis_replay_verified = FALSE)
      writeLines(encode(record), meta, useBytes = TRUE)
      # A torn two-file replacement fails closed on the export hash at download.
      if (!file.rename(tmp, dst) || !file.rename(meta, side)) fail()
      list(ok = TRUE, filename = filename, bytes = record$export_bytes,
           sha256 = record$export_sha256, full_analysis_replay_verified = FALSE)
    })
  })
  download <- function(app_dir, session) guarded(function() {
    c <- context(app_dir, session)
    lock(c, function(before) {
      record <- json(file.path(c$store, sidecar_name))
      if (!identical(record$schema, "pa.notebook.declared-export.v1") ||
          !identical(record$session_id, c$sid) || !identical(record$app_dir, c$app) ||
          !identical(record$run_dir, c$root) || !identical(record$provenance, before) ||
          !identical(record$manifest_sha256,
            digest::digest(encode(record$manifest), algo = "sha256", serialize = FALSE))) fail()
      if (!object(record$manifest) ||
          !all(names(record$manifest) %in% c("files", "steps", "expected_outputs"))) fail()
      declared <- c(lapply(record$manifest$files, function(f) f$path),
                    record$manifest$expected_outputs)
      if (!identical(declared, lapply(record$inventory, function(f) f$path))) fail()
      verify_files(c, record$inventory)
      p <- checked(file.path(c$store, filename))
      size <- unname(file.info(p)$size)
      if (!is.numeric(record$export_bytes) || length(record$export_bytes) != 1L ||
          !is.finite(size) || !identical(as.numeric(size), as.numeric(record$export_bytes))) fail()
      bytes <- readBin(p, "raw", n = size)
      if (length(bytes) != size || !identical(record$export_sha256,
          digest::digest(bytes, algo = "sha256", serialize = FALSE))) fail()
      verify_files(c, record$inventory)
      if (!identical(before, snapshot(c))) fail()
      list(ok = TRUE, bytes = bytes, filename = filename,
           content_type = "application/x-ipynb+json", full_analysis_replay_verified = FALSE)
    })
  })
  list(build = build, download = download)
})
nr_api_build <- .nr_api$build
nr_api_download <- .nr_api$download
