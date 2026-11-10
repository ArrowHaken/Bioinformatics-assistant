#!/usr/bin/env Rscript
# =====================================================================
#  guard_load.R -- one-liner loader for analysis scripts
#
#  Each analysis script starts with:
#
#      source(file.path(.this_dir, "..", "..", "workflow", "guard_load.R"))
#      sc_guard_banner("pipeline", .this_dir)
#
#  Why a separate loader: the path arithmetic lives in exactly one place.
#  If a script has to compute "../../workflow" itself, one of them will
#  eventually get the depth wrong and silently skip the check -- which is
#  the worst possible failure mode for a guard.
#
#  This file deliberately does NOT define a fallback. If guard.R cannot
#  be found, it stops. A guard that degrades to "allow" is not a guard.
#
#  ASCII-only source: R 4.1.2 on a Chinese Windows locale cannot parse
#  non-ASCII source files.
# =====================================================================

.guard_load_script_dir <- local({
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(f) && length(f) && nzchar(f)) return(dirname(normalizePath(f)))
  # fallback: callers pass their own dir via sc_guard_banner anyway
  "."
})

.guard_load_dir <- local({
  # resolve relative to THIS file, not the caller
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(f) && length(f) && nzchar(f)) return(dirname(normalizePath(f)))
  ca <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", ca, value = TRUE)
  if (length(m) > 0) {
    p <- sub("^--file=", "", m[1])
    if (nzchar(p) && file.exists(p)) return(dirname(normalizePath(p)))
  }
  "."
})

.guard_file <- file.path(.guard_load_dir, "guard.R")
if (!file.exists(.guard_file)) {
  stop("\n  REFUSED: workflow/guard.R not found next to guard_load.R\n",
       "  looked in: ", .guard_load_dir, "\n",
       "  the package may be incomplete. Refusing to run unguarded.\n", call. = FALSE)
}
source(.guard_file, local = FALSE)

.GUARD_DIR <- .guard_load_dir
