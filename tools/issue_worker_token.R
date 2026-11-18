#!/usr/bin/env Rscript
# Issue a worker token without touching agent_tokens.json.
# Usage: Rscript tools/issue_worker_token.R <app_dir> <worker_id> [pool_id] [note]
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) stop("usage: issue_worker_token.R <app_dir> <worker_id> [pool_id] [note]")
app_dir <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
source(file.path(app_dir, "engine", "worker_auth.R"), local = TRUE)
out <- worker_token_issue(app_dir, args[[2]],
                          if (length(args) >= 3L) args[[3]] else NULL,
                          if (length(args) >= 4L) args[[4]] else "")
cat(out$token, "\n")
