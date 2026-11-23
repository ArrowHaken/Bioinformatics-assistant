#!/usr/bin/env Rscript
# 尝试身份下发契约：attempt_id 必须随 due 与 claim 一起交给执行端。
#
# 为什么必须有（2026-11-23，子任务查证 19 条「回写未被接受」的根因）：
#   attempt_id 由服务端派工时生成，也写进了队列条目，但 /api/exec/due 的字段
#   清单和 /api/exec/claim 的响应里都没有它 —— 执行端根本拿不到这个值。
#   回写时它只能发空，服务端回落到按 req_id 猜是哪一次尝试，
#   猜不中就判 attempt_id_unknown，作业在集群上真跑完了结果却写不回来。
#
# 判据：
#   1) due 返回的条目带 attempt_id，值等于队列里存的那一个
#   2) claim 返回的条目同样带 attempt_id
#   3) claimed 后队列条目保留 attempt_id（不被覆盖成空）
#
# 用法：Rscript tests/exec_attempt_binding_contract.R [app_dir]

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

args <- commandArgs(trailingOnly = TRUE)
app_src <- normalizePath(if (length(args)) args[[1]] else ".", mustWork = TRUE)

fail <- character()
check <- function(label, ok) {
  cat(if (isTRUE(ok)) "PASS:" else "FAIL:", label, "\n")
  if (!isTRUE(ok)) fail <<- c(fail, label)
}

root <- tempfile("exec-attempt-")
dir.create(root)
on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
stopifnot(file.symlink(file.path(app_src, "engine"), file.path(root, "engine")))
if (dir.exists(file.path(app_src, "public")))
  stopifnot(file.symlink(file.path(app_src, "public"), file.path(root, "public")))
stopifnot(file.symlink(file.path(app_src, "serve.R"), file.path(root, "serve.R")))
dir.create(file.path(root, "state"), recursive = TRUE)
dir.create(file.path(root, "config"), recursive = TRUE)

old <- options(pa.serve.definitions_only = TRUE, pa.app_dir = root)
e <- new.env(parent = globalenv())
sys.source(file.path(root, "serve.R"), envir = e)
options(old)

e$send_json <- function(con, code, body) list(code = as.integer(code), body = body)
e$DB_READY <- FALSE
e$EXEC_FILE <- file.path(root, "state", "exec_tasks.json")

tok <- tryCatch(e$worker_token_issue(root, "w-att", "default", note = "attempt fixture")$token,
                error = function(err) "")
check("fixture token is issued", nzchar(tok))

want_att <- "att-contract-0001"
seed <- list(items = list(list(
  id = "ex-att-1", step = "doublet", session_id = "s-att", req_id = "req-att",
  status = "queued", pool_id = "default", attempt_id = want_att,
  created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))))
writeLines(jsonlite::toJSON(seed, auto_unbox = TRUE, null = "null"), e$EXEC_FILE)

call <- function(path, body = NULL) e$serve_request(NULL, list(
  target = path,
  method = if (is.null(body)) "GET" else "POST",
  headers = list("x-worker-token" = tok),
  body = if (is.null(body)) "" else as.character(jsonlite::toJSON(body, auto_unbox = TRUE))))

# ---- 1) due 必须带 attempt_id -------------------------------------
r1 <- call("/api/exec/due")
items1 <- (r1$body %||% list())$items %||% list()
hit1 <- Filter(function(x) identical(as.character(x$id %||% ""), "ex-att-1"), items1)
check("due returns the seeded task", length(hit1) == 1L)
check("due carries attempt_id",
      length(hit1) == 1L &&
        identical(as.character(hit1[[1]]$attempt_id %||% ""), want_att))

# ---- 2) claim 必须带 attempt_id ------------------------------------
r2 <- call("/api/exec/claim", list(id = "ex-att-1", worker_id = "w-att", pool_id = "default"))
item2 <- (r2$body %||% list())$item %||% list()
check("claim succeeds", isTRUE((r2$body %||% list())$ok))
check("claim carries attempt_id",
      identical(as.character(item2$attempt_id %||% ""), want_att))

# ---- 3) 领取后队列条目仍保留 attempt_id ----------------------------
after <- jsonlite::fromJSON(e$EXEC_FILE, simplifyVector = FALSE)$items %||% list()
row <- Filter(function(x) identical(as.character(x$id %||% ""), "ex-att-1"), after)
check("queue entry keeps attempt_id after claim",
      length(row) == 1L && identical(as.character(row[[1]]$attempt_id %||% ""), want_att))

if (length(fail)) {
  cat("\nFAILED:", paste(fail, collapse = "; "), "\n")
  quit(save = "no", status = 1L)
}
cat("\nexec attempt binding contract\n")