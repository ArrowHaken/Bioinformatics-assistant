#!/usr/bin/env Rscript
# =====================================================================
#  script_ops.R -- swap, upload, back up and statically review scripts
#
#  THE PROBLEM THIS SOLVES
#    Until now the scripts were a closed set: whatever shipped in the
#    package is what ran. Two things were impossible:
#      a) "this is my own version of step 03, use mine"
#      b) "I want this line changed, have the assistant change it"
#
#    Both boil down to the same missing piece: a controlled way to put
#    a script in, and a check that says whether the result still fits
#    the rest of the workflow.
#
#  WHY A STATIC CHECK AND NOT JUST "ASK THE AI"
#    An AI review is a judgement call and takes time. Half the ways a
#    replacement script breaks the workflow are mechanical and can be
#    answered instantly:
#      - no entry guard  -> it can be run out of band, no ledger
#      - non-ASCII source -> dies on a Chinese Windows locale
#      - hardcoded thresholds -> silently wrong on the next dataset
#      - no stop() on failure -> carries on with a broken object
#    So: mechanical checks run here, immediately. The AI review is a
#    separate, explicitly requested step for the parts that need
#    reading comprehension (does it still implement the same method?).
#
#  NOTHING IS OVERWRITTEN WITHOUT A BACKUP
#    Every replace copies the previous file to audit/backups/ first.
#    The whitelist is rebuilt afterwards, otherwise reviewer.R would
#    fail every later run with "script changed".
#
#  ASCII-only source. Chinese wording lives in the web page.
# =====================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

sop_read_lines <- function(p, n = -1L) {
  tryCatch(readLines(p, warn = FALSE, encoding = "UTF-8", n = n),
           error = function(e) character(0))
}

sop_read_text <- function(p) {
  if (!file.exists(p)) return("")
  ln <- sop_read_lines(p)
  paste(ln, collapse = "\n")
}

sop_write_text <- function(p, txt) {
  dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE)
  con <- file(p, open = "wb")
  writeBin(charToRaw(enc2utf8(txt)), con)
  close(con)
  invisible(TRUE)
}

sop_md5 <- function(p) if (!file.exists(p)) NA_character_ else unname(tools::md5sum(p))

sop_rel <- function(p, base) {
  p <- normalizePath(p, winslash = "/", mustWork = FALSE)
  b <- normalizePath(base, winslash = "/", mustWork = FALSE)
  sub(paste0("^", gsub("([\\W])", "\\\\\\1", b), "/?"), "", p)
}

# Guard against "just walk to a different directory" style input.
#   rel is always resolved against engine_dir and must stay inside it.
sop_safe_path <- function(engine_dir, rel) {
  rel <- gsub("\\\\", "/", as.character(rel))
  rel <- sub("^/+", "", rel)
  if (!nzchar(rel)) return(NULL)
  if (grepl("\\.\\./|\\.\\.$", rel)) return(NULL)
  full <- file.path(engine_dir, rel)
  fr <- normalizePath(full, winslash = "/", mustWork = FALSE)
  er <- normalizePath(engine_dir, winslash = "/", mustWork = FALSE)
  if (!startsWith(fr, paste0(er, "/")) && !identical(fr, er)) return(NULL)
  fr
}

# ---------------------------------------------------------------------
# inventory: which files may be replaced, grouped by workflow stage
#
#   The stage list itself is NOT defined here. stage_scripts.R already
#   owns "which stage lives in which directory"; defining it twice is
#   how you get a page that lists four stages and a writer that only
#   knows three. We read it from there and only fall back to a local
#   copy if that file is missing.
# ---------------------------------------------------------------------
SOP_STAGES_FALLBACK <- list(
  list(id = "entry",    skill = "workflow",      dir = "workflow",              ext = "\\.R$"),
  list(id = "pipeline", skill = "sc-pipeline",   dir = "sc-pipeline/scripts",   ext = "\\.R$"),
  list(id = "plots",    skill = "sc-plot-kit",   dir = "sc-plot-kit/scripts",   ext = "\\.R$"),
  list(id = "ppt",      skill = "sc-ppt-kit",    dir = "sc-ppt-kit/scripts",    ext = "\\.(py|json)$")
)

sop_stages <- function(engine_dir) {
  f <- file.path(engine_dir, "stage_scripts.R")
  if (file.exists(f)) {
    d <- tryCatch({
      env <- new.env(parent = globalenv())
      source(f, local = env)
      get("STAGE_DEF", envir = env)
    }, error = function(e) NULL)
    if (!is.null(d) && length(d)) {
      return(lapply(d, function(s) {
        list(id = s$id, skill = s$skill, dir = s$dir, ext = s$ext %||% "\\.R$")
      }))
    }
  }
  SOP_STAGES_FALLBACK
}

sop_inventory <- function(engine_dir) {
  out <- list()
  for (st in sop_stages(engine_dir)) {
    full <- file.path(engine_dir, st$dir)
    files <- character(0)
    if (dir.exists(full)) {
      files <- list.files(full, pattern = st$ext, recursive = TRUE, full.names = TRUE)
      files <- files[!grepl("/legacy_reference/|/audit/", files)]
      files <- sort(files)
    }
    items <- lapply(files, function(f) {
      rel <- sop_rel(f, engine_dir)
      list(
        name = basename(f),
        rel = rel,
        md5 = sop_md5(f),
        size = as.integer(file.info(f)$size),
        ext = tolower(tools::file_ext(f)),
        editable = TRUE
      )
    })
    out[[length(out) + 1L]] <- list(id = st$id, skill = st$skill, dir = st$dir,
                                    count = length(items), scripts = items)
  }
  list(ok = TRUE, stages = out, engine = engine_dir)
}

# ---------------------------------------------------------------------
# static review
#
#   Each check returns id + status (pass / warn / fail) + a short
#   machine-readable evidence string. The page turns ids into wording,
#   so this file stays ASCII and the wording has one home.
# ---------------------------------------------------------------------
sop_review <- function(engine_dir, rel, content = NULL) {
  full <- sop_safe_path(engine_dir, rel)
  if (is.null(full)) return(list(ok = FALSE, error = "path rejected"))
  if (!file.exists(full)) return(list(ok = FALSE, error = "file not found"))

  ext <- tolower(tools::file_ext(full))
  is_r <- ext == "r"
  # content = NULL means "check what is on disk".
  # Passing content means "check this draft" --- that is the case that
  # matters: after editing, the useful question is whether the NEW text
  # would fit the workflow, not whether the old one did.
  lines <- if (is.null(content)) sop_read_lines(full) else strsplit(content, "\n", fixed = TRUE)[[1]]
  txt <- paste(lines, collapse = "\n")
  head_txt <- paste(utils::head(lines, 100L), collapse = "\n")
  is_draft <- !is.null(content)
  checks <- list()
  add <- function(id, status, evidence) {
    checks[[length(checks) + 1L]] <<- list(id = id, status = status,
                                           evidence = as.character(evidence))
  }

  # 1. is this file part of the inventory the workflow knows about?
  inv <- sop_inventory(engine_dir)
  known <- character(0)
  for (st in inv$stages) for (s in st$scripts) known <- c(known, s$rel)
  if (rel %in% known) {
    add("in_inventory", "pass", rel)
  } else {
    add("in_inventory", "fail", "not under any stage directory")
  }

  # 2. entry guard (R only).
  #    Whether a guard is REQUIRED depends on the role of the file:
  #      - module files (lib/xxx.R) are only ever sourced by another
  #        script. They cannot start anything on their own, so a guard
  #        on them would be dead code. Reported as "not applicable".
  #      - runnable scripts should carry one. Missing it does not break
  #        the chain (the workflow still launches everything through
  #        the single entry), but it does mean someone could run this
  #        by hand and leave no ledger entry.
  #    Grading the two the same way is how a check ends up flagging
  #    its own framework files and gets ignored.
  is_module <- grepl("(^|/)lib/", rel) || grepl("(^|/)legacy_reference/", rel)
  if (is_r) {
    has_guard <- grepl("guard_load|sc_guard_banner", txt)
    if (has_guard) {
      add("guard", "pass", "guard call present")
    } else if (is_module) {
      add("guard", "skip", "module file, sourced by others")
    } else {
      add("guard", "warn", "runnable but no guard call")
    }
  } else {
    add("guard", "skip", "not an R script")
  }

  # 3. ASCII-only source (R only).
  #    R 4.1.2 on a Chinese Windows locale cannot parse non-ASCII source;
  #    the script then fails before it prints anything useful.
  #
  #    Graded warn, not fail, and here is why:
  #    most files in this package carry Chinese comments, and several of
  #    the scripts in daily use have Chinese inside string literals too.
  #    If every one of them came back "fail", the user would open any
  #    script, see a red cross, and stop reading this check --- which is
  #    worse than not running it. So it is reported as a caution: it
  #    matters only when the target machine is an old R on a Chinese
  #    Windows locale, which is exactly the case the note names.
  if (is_r) {
    bad <- which(!grepl("^[\\x00-\\x7F]*$", lines, perl = TRUE))
    if (!length(bad)) {
      add("ascii", "pass", "pure ASCII")
    } else {
      only_comments <- all(grepl("^[ \\t]*#", lines[bad]))
      add("ascii", "warn",
          paste0(if (only_comments) "non-ASCII in comments, lines " else "non-ASCII outside comments, lines ",
                 paste(utils::head(bad, 6L), collapse = ",")))
    }
  } else {
    add("ascii", "skip", "not an R script")
  }

  # 4. reads the config instead of carrying business defaults.
  #    A threshold written into the code does not throw an error on the
  #    next dataset; it silently produces a different answer.
  if (is_r) {
    reads_cfg <- grepl("--config|read_cfg|cfg_get|cfg_val|cfg_step_get", txt)
    if (reads_cfg) {
      add("reads_config", "pass", "reads pipeline.json")
    } else {
      add("reads_config", "warn", "no config read found")
    }
  } else {
    add("reads_config", "skip", "not an R script")
  }

  # 5. hardcoded numeric thresholds in a config-driven script.
  #    Whitelisted: indices, dims, seed-free constants and anything
  #    already pulled from config. This is a hint, not a verdict.
  if (is_r && grepl("read_cfg|cfg_get|cfg_val", txt)) {
    hits <- grep("(qc_min|qc_max|res1|res2|min\\.[a-z_]*)\\s*<-\\s*[0-9]",
                 lines, ignore.case = TRUE, value = FALSE)
    if (length(hits)) {
      add("hardcoded", "warn", paste0("numeric default near lines ",
                                     paste(utils::head(hits, 6L), collapse = ",")))
    } else {
      add("hardcoded", "pass", "no obvious hardcoded threshold")
    }
  } else {
    add("hardcoded", "skip", "not applicable")
  }

  # 6. fails loudly. "no degradation" is the rule this workflow is built
  #    on; a replacement that warns and continues breaks it.
  if (is_r || ext == "py") {
    if (grepl("stop\\(|quit\\(status\\s*=\\s*[1-9]|sys\\.exit\\([1-9]", txt)) {
      add("stop_on_fail", "pass", "has explicit failure exit")
    } else {
      add("stop_on_fail", "warn", "no stop() / non-zero exit found")
    }
  } else {
    add("stop_on_fail", "skip", "not applicable")
  }

  # 7. writes into the output directory the workflow knows about.
  if (is_r) {
    if (grepl("OUTDIR|outdir|results", txt)) {
      add("writes_outdir", "pass", "references outdir")
    } else {
      add("writes_outdir", "warn", "no outdir reference")
    }
  } else {
    add("writes_outdir", "skip", "not applicable")
  }

  # 8. declares which stage it belongs to, in the header comment.
  if (grepl("--stage|cluster1|continue|STAGE_DEF|invocable", head_txt)) {
    add("declares_stage", "pass", "stage semantics present")
  } else {
    add("declares_stage", "warn", "no stage marker in header")
  }

  # 9. does it actually parse?
  #    This is the one hard failure that is both objective and worth
  #    blocking on. A file that cannot be parsed breaks the step the
  #    moment it runs, and the error it throws at that point is far
  #    harder to read than a verdict here.
  #    R is checked with parse(), Python with py_compile; anything else
  #    (json) with a JSON parse.
  if (is_r) {
    ok_parse <- tryCatch({
      suppressWarnings(parse(text = txt, keep.source = FALSE)); TRUE
    }, error = function(e) conditionMessage(e))
    if (isTRUE(ok_parse)) {
      add("parses", "pass", "R parse ok")
    } else {
      add("parses", "fail", paste0("R parse error: ",
                                   substr(gsub("\\s+", " ", as.character(ok_parse)), 1, 90)))
    }
  } else if (ext == "py") {
    # Check the TEXT we were handed, not whatever is on disk.
    #   Getting this wrong is subtle: editing a draft, hitting "check",
    #   and being told the old file is fine is worse than no check at all.
    #   Same reason the R branch parses `txt` instead of re-reading `full`.
    tmp <- tempfile(fileext = ".py")
    writeLines(lines, tmp, useBytes = TRUE)
    res <- tryCatch({
      py <- Sys.which(c("python3", "python"))
      py <- py[nzchar(py)]
      if (!length(py)) {
        "no-python"
      } else {
        o <- suppressWarnings(system2(unname(py[1]),
              c("-c", shQuote("import ast,sys;ast.parse(open(sys.argv[1],encoding='utf-8').read())"),
                shQuote(tmp)), stdout = TRUE, stderr = TRUE))
        st <- attr(o, "status")
        if (is.null(st) || identical(as.integer(st), 0L)) "ok"
        else paste0("syntax error: ", substr(gsub("\\s+", " ",
                   paste(utils::tail(o, 2L), collapse = " ")), 1, 80))
      }
    }, error = function(e) conditionMessage(e))
    try(unlink(tmp), silent = TRUE)
    if (identical(res, "ok")) add("parses", "pass", "python parse ok")
    else if (identical(res, "no-python")) add("parses", "skip", "no python on this machine")
    else add("parses", "fail", as.character(res))
  } else if (ext == "json") {
    ok_j <- tryCatch({
      jsonlite::fromJSON(txt, simplifyVector = FALSE); TRUE
    }, error = function(e) conditionMessage(e))
    if (isTRUE(ok_j)) add("parses", "pass", "JSON parse ok")
    else add("parses", "fail", paste0("JSON error: ", substr(as.character(ok_j), 1, 70)))
  } else {
    add("parses", "skip", "no parser for this type")
  }

  n_fail <- sum(vapply(checks, function(c) identical(c$status, "fail"), logical(1)))
  n_warn <- sum(vapply(checks, function(c) identical(c$status, "warn"), logical(1)))
  verdict <- if (n_fail > 0L) "fail" else if (n_warn > 0L) "warn" else "pass"

  list(ok = TRUE, rel = rel, ext = ext, md5 = sop_md5(full),
       size = as.integer(file.info(full)$size),
       verdict = verdict, n_fail = n_fail, n_warn = n_warn,
       checks = checks,
       reviewed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
}

# ---------------------------------------------------------------------
# backup + write
# ---------------------------------------------------------------------
sop_backup_dir <- function(engine_dir) file.path(engine_dir, "audit", "backups")

# Backups keep the directory structure under a timestamp folder:
#     audit/backups/<ts>/<original/relative/path.R>
#
#   Why not flatten the path into the filename.
#     An earlier version wrote "20261020-115006__sc-pipeline_scripts_lib_x.R"
#     and then reconstructed the target path by replacing underscores back
#     into slashes. That cannot work: the original path itself contains
#     underscores, so the guess is wrong, `file.exists` fails, and
#     "restore" reports failure while looking like a permissions problem.
#     A restore target must be recorded, never re-derived.
sop_backup <- function(engine_dir, rel) {
  full <- sop_safe_path(engine_dir, rel)
  if (is.null(full) || !file.exists(full)) return(NULL)
  ts <- format(Sys.time(), "%Y%m%d-%H%M%S")
  root <- file.path(sop_backup_dir(engine_dir), ts)
  dest <- file.path(root, rel)
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  if (!isTRUE(file.copy(full, dest, overwrite = TRUE))) return(NULL)
  list(id = paste0(ts, "/", rel),
       ts = ts, rel = rel, path = dest,
       md5 = sop_md5(full),
       size = as.integer(file.info(full)$size),
       at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
}

sop_backups <- function(engine_dir, rel = NULL) {
  root <- sop_backup_dir(engine_dir)
  if (!dir.exists(root)) return(list())
  ts_dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  if (!length(ts_dirs)) return(list())
  out <- list()
  for (tsd in ts_dirs) {
    ts <- basename(tsd)
    files <- list.files(tsd, recursive = TRUE, full.names = TRUE)
    files <- files[!dir.exists(files)]
    for (f in files) {
      r <- sub(paste0("^", gsub("([\\W])", "\\\\\\1", tsd), "/?"),
               "", normalizePath(f, winslash = "/", mustWork = FALSE))
      info <- tryCatch(file.info(f), error = function(e) NULL)
      out[[length(out) + 1L]] <- list(
        id = paste0(ts, "/", r),
        name = basename(f),
        rel = r,
        ts = ts,
        md5 = sop_md5(f),
        size = if (is.null(info)) NA_integer_ else as.integer(info$size),
        at = if (is.null(info)) ts else format(info$mtime, "%Y-%m-%dT%H:%M:%S")
      )
    }
    # Leftovers from the older flat naming scheme (<ts>__<flattened>.R).
    #   Still listed, but flagged as not restorable: flattening the path
    #   into the filename destroyed the information needed to put the
    #   file back, so any guess would be wrong.

    flat <- list.files(root, pattern = "^[0-9]{8}-[0-9]{6}__", full.names = TRUE)
    for (f in flat) {
      if (dir.exists(f)) next
      out[[length(out) + 1L]] <- list(
        id = basename(f), name = basename(f),
        rel = "", ts = sub("__.*$", "", basename(f)),
        md5 = sop_md5(f), size = as.integer(file.info(f)$size),
        at = format(file.info(f)$mtime, "%Y-%m-%dT%H:%M:%S"),
        legacy = TRUE
      )
    }
  }
  if (!is.null(rel)) out <- Filter(function(x) identical(x$rel, rel), out)
  if (!length(out)) return(list())
  ord <- order(vapply(out, function(x) as.character(x$at %||% ""), character(1)), decreasing = TRUE)
  out[ord]
}

sop_restore_backup <- function(engine_dir, backup_id) {
  id <- gsub("\\\\", "/", as.character(backup_id))
  id <- sub("^/+", "", id)
  parts <- strsplit(id, "/", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  if (length(parts) < 2L) {
    return(list(ok = FALSE, error = "bad backup id (need <timestamp>/<path>)"))
  }
  ts <- parts[1]
  rel <- paste(parts[-1], collapse = "/")
  full <- sop_safe_path(engine_dir, rel)
  src <- file.path(sop_backup_dir(engine_dir), ts, rel)
  if (is.null(full) || !file.exists(src)) return(list(ok = FALSE, error = "backup not found"))
  res <- sop_put(engine_dir, rel, sop_read_text(src), mode = "replace")
  c(res, list(restored_from = id, rel = rel))
}

# Write a replacement in place. mode:
#   "replace"  must already exist (a new file would silently join the
#              whitelist and nobody would notice it is unreviewed)
#   "new"      must not exist
sop_put <- function(engine_dir, rel, content, mode = "replace") {
  full <- sop_safe_path(engine_dir, rel)
  if (is.null(full)) return(list(ok = FALSE, error = "path rejected"))
  if (!nzchar(content)) return(list(ok = FALSE, error = "empty content"))
  exists_now <- file.exists(full)
  if (identical(mode, "replace") && !exists_now)
    return(list(ok = FALSE, error = "target does not exist"))
  if (identical(mode, "new") && exists_now)
    return(list(ok = FALSE, error = "target already exists"))

  bkp <- if (exists_now) sop_backup(engine_dir, rel) else NULL
  sop_write_text(full, content)

  # The whitelist must be rebuilt, otherwise reviewer.R reports
  # "script changed" on every subsequent run and the user learns to
  # ignore the check. Rebuilding here keeps it meaningful.
  wl <- tryCatch({
    rv <- file.path(engine_dir, "reviewer.R")
    if (file.exists(rv)) {
      rs <- file.path(R.home("bin"), "Rscript")
      o <- suppressWarnings(system2(rs, c("--vanilla", rv, "--build-whitelist", "--engine", engine_dir),
                                    stdout = TRUE, stderr = TRUE))
      TRUE
    } else FALSE
  }, error = function(e) FALSE)

  list(ok = TRUE, rel = rel, mode = mode,
       # Return the backup ID, not the backup's absolute path.
       #   The caller hands this straight back to sop_restore_backup,
       #   which splits it into <timestamp>/<relative-path>. An absolute
       #   path splits into "srv"/... and silently resolves to a path
       #   that does not exist; restore then fails while looking like a
       #   missing file rather than a wrong identifier.
       backup_id = if (is.null(bkp)) NULL else bkp$id,
       backup_path = if (is.null(bkp)) NULL else bkp$path,
       md5 = sop_md5(full),
       whitelist_rebuilt = wl)
}

# ---------------------------------------------------------------------
# incoming: uploaded scripts waiting to be reviewed or adopted
# ---------------------------------------------------------------------
sop_incoming_dir <- function(engine_dir) file.path(engine_dir, "incoming")

sop_incoming_add <- function(engine_dir, name, content, note = "") {
  d <- sop_incoming_dir(engine_dir)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  nm <- gsub("[^A-Za-z0-9._-]", "_", basename(as.character(name)))
  if (!nzchar(nm)) return(list(ok = FALSE, error = "bad name"))
  ts <- format(Sys.time(), "%Y%m%d-%H%M%S")
  id <- paste0(ts, "__", nm)
  sop_write_text(file.path(d, id), content)
  meta <- list(id = id, name = nm, note = as.character(note),
               at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
               size = nchar(content, type = "bytes"),
               md5 = sop_md5(file.path(d, id)))
  sop_write_text(file.path(d, paste0(id, ".meta.json")),
                 jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE, null = "null"))
  list(ok = TRUE, id = id, meta = meta)
}

sop_incoming_list <- function(engine_dir) {
  d <- sop_incoming_dir(engine_dir)
  if (!dir.exists(d)) return(list())
  f <- list.files(d, pattern = "\\.meta\\.json$", full.names = TRUE)
  if (!length(f)) return(list())
  out <- list()
  for (x in f) {
    j <- tryCatch(jsonlite::fromJSON(paste(readLines(x, warn = FALSE, encoding = "UTF-8"),
                                           collapse = "\n"), simplifyVector = FALSE),
                  error = function(e) NULL)
    if (!is.null(j)) out[[length(out) + 1L]] <- j
  }
  ord <- order(vapply(out, function(z) as.character(z$at %||% ""), character(1)), decreasing = TRUE)
  out[ord]
}

sop_incoming_text <- function(engine_dir, id) {
  f <- file.path(sop_incoming_dir(engine_dir), basename(as.character(id)))
  if (!file.exists(f)) return(NULL)
  sop_read_text(f)
}

sop_incoming_drop <- function(engine_dir, id) {
  d <- sop_incoming_dir(engine_dir)
  b <- basename(as.character(id))
  f <- file.path(d, b)
  ok <- FALSE
  if (file.exists(f)) ok <- file.remove(f)
  m <- file.path(d, paste0(b, ".meta.json"))
  if (file.exists(m)) try(file.remove(m), silent = TRUE)
  list(ok = isTRUE(ok))
}

# ---------------------------------------------------------------------
# AI task queue
#
#   The app cannot call a model itself. What it can do is write down a
#   well-formed request, and the assistant (which works on this machine
#   and can reach both the engine and the run directory) picks it up.
#   Keeping one queue for every kind of request means there is a single
#   place that shows "what is waiting on the assistant".
# ---------------------------------------------------------------------
sop_queue_file <- function(app_dir) file.path(app_dir, "state", "tasks.jsonl")

sop_queue_add <- function(app_dir, kind, payload, run_dir = NULL) {
  f <- sop_queue_file(app_dir)
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  task <- c(list(
    id = paste0(kind, "-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                paste(sample(c(letters, 0:9), 4L), collapse = "")),
    kind = as.character(kind),
    run_dir = if (is.null(run_dir)) NULL else as.character(run_dir),
    status = "pending",
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    result = NULL
  ), payload)
  con <- file(f, open = "a", encoding = "UTF-8")
  writeLines(jsonlite::toJSON(task, auto_unbox = TRUE, null = "null"), con, useBytes = TRUE)
  close(con)
  try(Sys.chmod(f, mode = "0600"), silent = TRUE)
  task
}

sop_queue_list <- function(app_dir, limit = 40L) {
  f <- sop_queue_file(app_dir)
  if (!file.exists(f)) return(list())
  ln <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
  ln <- ln[nzchar(trimws(ln))]
  ln <- utils::tail(ln, limit)
  out <- list()
  for (x in ln) {
    j <- tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(j)) out[[length(out) + 1L]] <- j
  }
  rev(out)
}

# ---------------------------------------------------------------------
# change requests: "change this line for me"
#   Stored as a task with the current file attached, so the assistant
#   can produce a diff against the exact bytes that were in place when
#   the request was made. Reviewing against a stale copy is how you get
#   a patch that does not apply.
# ---------------------------------------------------------------------
sop_change_request <- function(app_dir, engine_dir, rel, idea, run_dir = NULL) {
  full <- sop_safe_path(engine_dir, rel)
  if (is.null(full) || !file.exists(full)) return(list(ok = FALSE, error = "file not found"))
  # Empty idea check lives here too, not only in the web layer.
  #   A request with no instruction is not a request; queueing it just
  #   puts a blank row in front of the assistant and wastes a turn.
  if (!nzchar(trimws(as.character(idea)))) return(list(ok = FALSE, error = "empty idea"))
  txt <- sop_read_text(full)
  if (nchar(txt, type = "bytes") > 200000L)
    return(list(ok = FALSE, error = "file too large for a change request (200KB)"))
  # keep a copy next to the task: the file will change, the request must not
  bkp <- sop_backup(engine_dir, rel)
  task <- sop_queue_add(app_dir, "script_change", list(
    rel = as.character(rel),
    idea = as.character(idea),
    base_md5 = sop_md5(full),
    snapshot = if (is.null(bkp)) NULL else bkp$path,
    engine = engine_dir
  ), run_dir = run_dir)
  list(ok = TRUE, task = task)
}

sop_review_request <- function(app_dir, engine_dir, rel, run_dir = NULL, question = "") {
  full <- sop_safe_path(engine_dir, rel)
  if (is.null(full) || !file.exists(full)) return(list(ok = FALSE, error = "file not found"))
  task <- sop_queue_add(app_dir, "script_review", list(
    rel = as.character(rel),
    question = as.character(question),
    base_md5 = sop_md5(full),
    engine = engine_dir,
    static = sop_review(engine_dir, rel)
  ), run_dir = run_dir)
  list(ok = TRUE, task = task)
}
