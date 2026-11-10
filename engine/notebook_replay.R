# Explicit, authorized file inventories only. This module never runs analysis at build time.
nbr_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA))
nbr_string <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x))
    stop(label, " must be a nonempty string", call. = FALSE)
  x
}
nbr_relative <- function(x, dot = FALSE) {
  x <- nbr_string(x, "relative path")
  if (dot && identical(x, ".")) return(x)
  if (grepl("[\\\\:[:cntrl:]]", x) || startsWith(x, "/") || startsWith(x, "~") ||
      any(strsplit(x, "/", fixed = TRUE)[[1]] %in% c("", ".", "..")) || endsWith(x, "/"))
    stop("Unsafe relative path", call. = FALSE)
  x
}
nbr_no_symlinks <- function(path) {
  p <- path
  repeat {
    link <- Sys.readlink(p)
    if (!is.na(link) && nzchar(link)) stop("Symlink rejected", call. = FALSE)
    parent <- dirname(p)
    if (identical(parent, p)) break
    p <- parent
  }
  invisible(TRUE)
}
nbr_path <- function(root, relative, directory = FALSE) {
  relative <- nbr_relative(relative, dot = directory)
  p <- file.path(root, relative)
  nbr_no_symlinks(p)
  if (!file.exists(p) || (directory != dir.exists(p))) stop("Declared path missing or wrong kind", call. = FALSE)
  resolved <- normalizePath(p, winslash = "/", mustWork = TRUE)
  if (!(identical(resolved, root) || startsWith(resolved, paste0(root, "/"))))
    stop("Path escaped authorized root", call. = FALSE)
  resolved
}
nbr_sensitive_path <- function(path) {
  grepl("(?i)(^|/)(state|\\.git|\\.ssh|\\.aws|\\.azure)(/|$)|(^|/)(\\.env([.]|$)|\\.Renviron$|\\.Rprofile$)|credential|password|passwd|secret|token|api[_-]?key|private[_-]?key|id_rsa|id_ed25519|\\.(pem|p12|key)$", path, perl = TRUE)
}
nbr_sensitive_text <- function(text) {
  grepl(paste0("(?i)",
    "-----BEGIN [A-Z ]*PRIVATE KEY-----|",
    "(?:AKIA|ASIA)[A-Z0-9]{16}|(?:gh[pousr]_|github_pat_)[A-Za-z0-9_]{12,}|",
    "https?://[^/[:space:]]+:[^/@[:space:]]+@|",
    "(?:password|passwd|secret|token|api[_-]?key|authorization|credential|private[_-]?key)[[:space:]\\\"']*[:=][[:space:]\\\"']*[^[:space:]\\\"',;}]+|",
    "Bearer[[:space:]]+[A-Za-z0-9._-]{8,}|",
    "eyJ[A-Za-z0-9_-]{8,}\\.[A-Za-z0-9_-]+\\.[A-Za-z0-9_-]+"), text, perl = TRUE)
}
nbr_scan <- function(bytes) {
  # NULs are replaced for inspection only; embedded bytes remain untouched.
  inspected <- bytes; inspected[inspected == as.raw(0)] <- as.raw(32)
  text <- iconv(rawToChar(inspected), from = "latin1", to = "UTF-8")
  # Latin-1 inspection preserves every ASCII credential marker in binary inputs.
  if (nbr_sensitive_text(text)) stop("Potential credential content rejected", call. = FALSE)
  invisible(TRUE)
}
nbr_array <- function(x, label, nonempty = TRUE) {
  if (!(is.list(x) || is.character(x)) || (!is.null(names(x)) && any(nzchar(names(x)))) ||
      (nonempty && !length(x))) stop(label, " must be an array", call. = FALSE)
  as.list(x)
}

# manifest = list(files=list(list(path, role='script'|'config'|'input',
#   embed_authorized=TRUE for every input)), steps=list(list(title, command,
#   argv=list('relative/script.py', ...), cwd='.')), expected_outputs=list('out.csv')).
# Expected outputs must already exist: their SHA-256 hashes are the explicit baseline.
nbr_build <- function(authorized_project_root, manifest, title = "Declared analysis replay",
                      max_file_bytes = 10 * 1024^2, max_total_bytes = 50 * 1024^2) {
  for (pkg in c("jsonlite", "digest")) if (!requireNamespace(pkg, quietly = TRUE))
    stop("Required package unavailable: ", pkg, call. = FALSE)
  for (cap in list(max_file_bytes, max_total_bytes))
    if (!is.numeric(cap) || length(cap) != 1L || !is.finite(cap) || cap < 1)
      stop("Byte limits must be finite positive numbers", call. = FALSE)
  root <- nbr_string(authorized_project_root, "authorized_project_root")
  if (!startsWith(root, "/")) stop("Authorized root must be absolute", call. = FALSE)
  # Check before normalization, which otherwise conceals symlink components.
  if (any(strsplit(root, "/", fixed = TRUE)[[1]] %in% c(".", ".."))) stop("Unsafe authorized root")
  nbr_no_symlinks(root)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  if (!dir.exists(root) || identical(root, "/")) stop("Authorized root must be a project directory")
  title <- nbr_string(title, "title")
  if (nbr_sensitive_text(title)) stop("Potential credentials in title")
  if (!is.list(manifest) || is.null(names(manifest)) || anyDuplicated(names(manifest)) ||
      !all(names(manifest) %in% c("files", "steps", "expected_outputs"))) stop("Invalid manifest object")
  files <- nbr_array(manifest$files, "files")
  total <- 0
  packed <- lapply(files, function(f) {
    if (!is.list(f) || is.null(names(f)) || anyDuplicated(names(f)) ||
        !all(names(f) %in% c("path", "role", "embed_authorized"))) stop("Invalid file declaration")
    path <- nbr_relative(f$path)
    if (nbr_sensitive_path(path)) stop("Sensitive filename rejected", call. = FALSE)
    role <- nbr_string(f$role, "role")
    if (!role %in% c("script", "config", "input")) stop("Invalid file role")
    if (role == "input" && !isTRUE(f$embed_authorized)) stop("Input embedding requires explicit authorization")
    p <- nbr_path(root, path)
    size <- unname(file.info(p)$size)
    total <<- total + size
    if (!is.finite(size) || size > max_file_bytes || total > max_total_bytes)
      stop("Embedded file size limit exceeded", call. = FALSE)
    bytes <- readBin(p, "raw", n = size)
    if (length(bytes) != size) stop("Declared file changed while reading")
    nbr_scan(bytes)
    text <- NULL
    if (role %in% c("script", "config")) {
      text <- tryCatch(rawToChar(bytes), error = function(e) stop("Text file contains NUL bytes"))
      if (!validUTF8(text)) stop("Scripts and configs must be UTF-8 for readable source")
      Encoding(text) <- "UTF-8"
    }
    list(path = path, role = role, bytes = size, sha256 = digest::digest(bytes, algo = "sha256", serialize = FALSE),
         base64 = gsub("[\r\n]", "", jsonlite::base64_enc(bytes)), readable_source = text)
  })
  paths <- vapply(packed, function(x) x$path, "")
  if (anyDuplicated(paths)) stop("Duplicate declared path")
  # No file may be an ancestor of another declared file.
  for (p in paths) if (any(startsWith(paths, paste0(p, "/")))) stop("Conflicting declared layout")
  steps <- nbr_array(manifest$steps, "steps")
  steps <- lapply(seq_along(steps), function(i) {
    s <- steps[[i]]
    if (!is.list(s) || is.null(names(s)) || anyDuplicated(names(s)) ||
        !all(names(s) %in% c("title", "command", "argv", "cwd"))) stop("Invalid step declaration")
    command <- nbr_string(s$command, "command")
    if (!command %in% c("Rscript", "python", "python3")) stop("Only direct Rscript/Python commands supported")
    cwd <- nbr_relative(s$cwd, dot = TRUE); nbr_path(root, cwd, directory = TRUE)
    args <- nbr_array(s$argv, "argv")
    args <- lapply(args, function(a) {
      a <- nbr_string(a, "argv value")
      if (grepl("[[:cntrl:]]", a) || startsWith(a, "/") || startsWith(a, "~") ||
          grepl("[A-Za-z]:|\\\\|(^|/)\\.\\.(/|$)", a) || nbr_sensitive_text(a) ||
          grepl("(?i)password|passwd|secret|token|api[_-]?key|credential|authorization", a, perl = TRUE))
        stop("Unsafe or potentially sensitive argv")
      a
    })
    script <- nbr_relative(args[[1]])
    script <- if (cwd == ".") script else paste(cwd, script, sep = "/")
    hit <- match(script, paths)
    if (is.na(hit) || packed[[hit]]$role != "script") stop("Invoked script must be explicitly declared")
    if ((command == "Rscript" && !grepl("\\.[Rr]$", script)) ||
        (command != "Rscript" && !grepl("\\.py$", script))) stop("Script extension does not match interpreter")
    heading <- if (is.null(s$title)) paste("Step", i) else nbr_string(s$title, "step title")
    if (nbr_sensitive_text(heading)) stop("Potential credentials in step title")
    list(title = heading, command = command, argv = args, cwd = cwd)
  })
  outputs <- nbr_array(manifest$expected_outputs, "expected_outputs")
  output_paths <- vapply(outputs, nbr_relative, "")
  if (anyDuplicated(output_paths) || any(output_paths %in% paths)) stop("Duplicate output or output overlaps input")
  for (p in c(paths, output_paths))
    if (any(startsWith(c(paths, output_paths), paste0(p, "/")))) stop("Conflicting output layout")
  outputs <- lapply(output_paths, function(path) {
    if (nbr_sensitive_path(path)) stop("Sensitive output filename rejected")
    p <- nbr_path(root, path)
    list(path = path, bytes = unname(file.info(p)$size), sha256 = digest::digest(file = p, algo = "sha256"))
  })
  payload <- list(schema = "pa.notebook.explicit-replay.v1", files = packed, steps = steps,
                  expected_outputs = outputs, full_analysis_replay_verified = FALSE)
  object <- function() structure(list(), names = character())
  cell <- function(source, code = FALSE) {
    c <- list(cell_type = if (code) "code" else "markdown", metadata = object(), source = list(source))
    if (code) { c["execution_count"] <- list(NULL); c$outputs <- list() }
    c
  }
  cells <- list(cell(paste0("# ", title, "\n\n## Scope and prerequisites\n\n",
    "Explicit authorized inventory, not automatic dependency discovery. Python 3 kernel; Rscript and declared Python/R packages must already be installed. No rpy2 required. No environment values, real application state, or original project root are exported.\n\n",
    "Set REPLAY_ROOT in the next cell to a NEW or EMPTY directory, or keep None for a new temporary directory. All declared bytes are restored with their relative layout. Commands use real argv/cwd, shell=False, and a minimal environment. Scripts are trusted executable code, NOT sandboxed; review them before running. Packages, OS, external services, undeclared files and interpreter versions are not frozen.\n\n",
    "Expected output SHA-256 hashes are baselines from the explicitly authorized source outputs. An executed notebook checks these; the generator does not execute analysis. full_analysis_replay_verified remains FALSE. A fixture check is not biological/full scRNA validation.\n")),
    cell("# Set only this location. None creates a fresh empty temporary directory.\nREPLAY_ROOT = None\n", TRUE))
  cells[[length(cells) + 1L]] <- cell(paste0("## Explicit manifest and exact-byte restoration\n\n",
    "The payload includes base64 bytes plus SHA-256 and readable UTF-8 source. Restoration verifies both representations. Original final-newline and CRLF bytes are preserved.\n"))
  cells[[length(cells) + 1L]] <- cell(paste0("import json\n_REPLAY = json.loads(", nbr_json(nbr_json(payload)), ")\n", nbr_bootstrap()), TRUE)
  for (f in packed) if (!is.null(f$readable_source)) {
    fence <- "```"
    while (grepl(fence, f$readable_source, fixed = TRUE)) fence <- paste0(fence, "`")
    lang <- if (grepl("\\.py$", f$path)) "python" else if (grepl("\\.[Rr]$", f$path)) "r" else "text"
    cells[[length(cells) + 1L]] <- cell(paste0("## Source: ", f$path, "\n\nRole: ", f$role,
      "; SHA-256: ", f$sha256, "; bytes: ", f$bytes, "\n\n", fence, lang, "\n", f$readable_source, "\n", fence, "\n"))
  }
  for (i in seq_along(steps)) {
    cells[[length(cells) + 1L]] <- cell(paste0("## ", sprintf("%02d", i), ". ", steps[[i]]$title,
      "\n\nCommand / ordered argv / relative cwd:\n\n", nbr_json(steps[[i]]), "\n"))
    cells[[length(cells) + 1L]] <- cell(sprintf("_run_declared_step(%d)\n", i - 1L), TRUE)
  }
  cells[[length(cells) + 1L]] <- cell("## Verify every declared intermediate and final output\n\nMissing files, nonzero steps, symlinks, and byte/hash differences fail visibly. This check does not promote the notebook to verified full analysis.\n")
  cells[[length(cells) + 1L]] <- cell(nbr_verify_code(), TRUE)
  notebook <- list(nbformat = 4L, nbformat_minor = 5L, cells = cells,
    metadata = list(kernelspec = list(display_name = "Python 3", language = "python", name = "python3"),
      language_info = list(name = "python"), pa = list(explicit_replay = list(
        schema = payload$schema, full_analysis_replay_verified = FALSE, fixture_replay_verified = FALSE,
        expected_outputs = outputs, files = lapply(packed, function(f) f[c("path", "role", "bytes", "sha256")])))))
  for (i in seq_along(notebook$cells)) notebook$cells[[i]]$id <- sprintf("replay-%03d", i)
  list(ok = TRUE, notebook = notebook, manifest = payload, full_analysis_replay_verified = FALSE)
}

nbr_write <- function(built, path) {
  if (!is.list(built) || !isTRUE(built$ok)) stop("Expected successful nbr_build result")
  writeLines(nbr_json(built$notebook), path, useBytes = TRUE)
  invisible(path)
}

nbr_bootstrap <- function() paste(c(
  "import base64, hashlib, pathlib, tempfile, subprocess, shutil",
  "def _relative(value, dot=False):",
  "    if not isinstance(value, str) or not value or '\\\\' in value or ':' in value or any(ord(c)<32 for c in value):",
  "        raise ValueError('Unsafe relative replay path')",
  "    if dot and value == '.': return pathlib.Path('.')",
  "    if value.startswith(('/', '~')) or any(p in ('', '.', '..') for p in value.split('/')):",
  "        raise ValueError('Unsafe relative replay path')",
  "    return pathlib.Path(value)",
  "_requested = pathlib.Path(REPLAY_ROOT) if REPLAY_ROOT is not None else pathlib.Path(tempfile.mkdtemp(prefix='analysis-replay-'))",
  "if not _requested.is_absolute(): raise ValueError('REPLAY_ROOT must be absolute')",
  "if '..' in _requested.parts: raise ValueError('REPLAY_ROOT must not contain ..')",
  "if any(p.is_symlink() for p in [_requested, *_requested.parents]): raise ValueError('Symlink replay root rejected')",
  "_requested.mkdir(parents=True, exist_ok=True)",
  "if any(_requested.iterdir()): raise ValueError('REPLAY_ROOT must be empty')",
  "REPLAY_ROOT = _requested.resolve()",
  "def _target(relative, dot=False):",
  "    p = REPLAY_ROOT / _relative(relative, dot)",
  "    if any(x.is_symlink() for x in [p, *p.parents]): raise ValueError('Symlink replay path rejected')",
  "    p.resolve().relative_to(REPLAY_ROOT)",
  "    return p",
  "for _f in _REPLAY['files']:",
  "    _data = base64.b64decode(_f['base64'], validate=True)",
  "    if len(_data) != _f['bytes'] or hashlib.sha256(_data).hexdigest() != _f['sha256']: raise ValueError('Embedded bytes/hash mismatch')",
  "    if _f['readable_source'] is not None and _f['readable_source'].encode('utf-8') != _data: raise ValueError('Readable source differs from original bytes')",
  "    _p = _target(_f['path']); _p.parent.mkdir(parents=True, exist_ok=True)",
  "    with _p.open('xb') as _handle: _handle.write(_data)",
  "for _s in _REPLAY['steps']: _target(_s['cwd'], dot=True).mkdir(parents=True, exist_ok=True)",
  "_replay_results = []",
  "def _run_declared_step(index):",
  "    s = _REPLAY['steps'][index]",
  "    if index != len(_replay_results): raise RuntimeError('Run steps once in declared order')",
  "    if s['command'] not in ('Rscript','python','python3'): raise ValueError('Unsupported direct interpreter')",
  "    executable = shutil.which(s['command'], path='/usr/local/bin:/usr/bin:/bin')",
  "    if executable is None: raise RuntimeError('Required interpreter unavailable: ' + s['command'])",
  "    cwd = _target(s['cwd'], dot=True)",
  "    script = _target(str((_relative(s['cwd'], dot=True) / _relative(s['argv'][0])).as_posix()))",
  "    if not script.is_file(): raise RuntimeError('Declared script missing')",
  "    # No inherited environment values or startup profiles are replay inputs.",
  "    env = {'PATH':'/usr/local/bin:/usr/bin:/bin', 'HOME':str(REPLAY_ROOT), 'TMPDIR':str(REPLAY_ROOT), 'LANG':'C.UTF-8', 'LC_ALL':'C.UTF-8', 'PYTHONNOUSERSITE':'1', 'R_ENVIRON_USER':'/dev/null', 'R_PROFILE_USER':'/dev/null', 'R_ENVIRON':'/dev/null', 'R_PROFILE':'/dev/null'}",
  "    argv = [executable] + (['--vanilla'] if s['command'] == 'Rscript' else []) + s['argv']",
  "    result = subprocess.run(argv, cwd=str(cwd), env=env, shell=False, capture_output=True, text=True)",
  "    print(result.stdout, end=''); print(result.stderr, end='')",
  "    if result.returncode: raise RuntimeError('Declared step failed: ' + s['title'] + ' exit=' + str(result.returncode))",
  "    _replay_results.append({'step':s['title'], 'exit_code':result.returncode})",
  "print('Declared files restored to', REPLAY_ROOT)", ""), collapse = "\n")

nbr_verify_code <- function() paste(c(
  "if len(_replay_results) != len(_REPLAY['steps']): raise RuntimeError('Not all declared steps succeeded')",
  "_output_checks = []",
  "for _out in _REPLAY['expected_outputs']:",
  "    _path = _target(_out['path'])",
  "    if not _path.is_file(): raise RuntimeError('Expected output missing: ' + _out['path'])",
  "    _hash = hashlib.sha256()",
  "    with _path.open('rb') as _handle:",
  "        for _chunk in iter(lambda: _handle.read(1048576), b''): _hash.update(_chunk)",
  "    if _path.stat().st_size != _out['bytes'] or _hash.hexdigest() != _out['sha256']: raise RuntimeError('Output baseline mismatch: ' + _out['path'])",
  "    _output_checks.append({'path':_out['path'], 'sha256':_hash.hexdigest(), 'matches_baseline':True})",
  "REPLAY_VERIFICATION = {'declared_outputs_match':True, 'outputs':_output_checks, 'full_analysis_replay_verified':False, 'fixture_replay_verified':False}",
  "print(json.dumps(REPLAY_VERIFICATION, indent=2))", ""), collapse = "\n")
