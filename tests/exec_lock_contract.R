#!/usr/bin/env Rscript
# 并发领取契约：两个独立进程同时领同一条 queued 任务，只能有一个真正领到。
#
# 为什么必须有这条（2026-11-23 实测）：
#   队列是单个 JSON 文件，读出来改完整份写回。原来"判断能不能领"在锁外、
#   "写占用"在锁内，两个执行端同时读到同一条 queued，两边都回报"已领到"，
#   实际只有一个能占住 —— 另一个拿着同一份任务去跑，产物写同一个目录。
#   这不是理论风险，用两个独立 R 进程复现过。
#
# 用法：
#   Rscript tests/exec_lock_contract.R [app_dir]
#   app_dir 默认当前目录。传一个旧版本目录可以复现失败（fail-first）。
#
# 判据：领到的那条 running 且 claimed_by 唯一；另一个进程必须明确拿到
#   "已被占用"，而不是也返回成功。

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

args <- commandArgs(trailingOnly = TRUE)
app_src <- normalizePath(if (length(args)) args[[1]] else ".", mustWork = TRUE)

root <- tempfile("exec-lock-")
dir.create(root)
on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
stopifnot(file.symlink(file.path(app_src, "engine"), file.path(root, "engine")))
if (dir.exists(file.path(app_src, "public")))
  stopifnot(file.symlink(file.path(app_src, "public"), file.path(root, "public")))
stopifnot(file.symlink(file.path(app_src, "serve.R"), file.path(root, "serve.R")))
dir.create(file.path(root, "state"), recursive = TRUE)

qfile <- file.path(root, "state", "exec_tasks.json")
barrier <- file.path(root, "go")
seed <- list(items = list(list(id = "task-A", step = "doublet",
                               session_id = "s-test", status = "queued")))
writeLines(jsonlite::toJSON(seed, auto_unbox = TRUE, null = "null"), qfile)

child <- file.path(root, "child.R")
writeLines(c(
  'a <- commandArgs(trailingOnly = TRUE)',
  'root <- a[[1]]; who <- a[[2]]; hold <- as.numeric(a[[3]])',
  'options(pa.app_dir = root, pa.serve.definitions_only = TRUE)',
  '`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x',
  'e <- new.env(parent = globalenv())',
  'sys.source(file.path(root, "serve.R"), envir = e)',
  '# 两个子进程同时启动，谁先拿到读改写谁先动。',
  'res <- e$exec_mutate(function(d) {',
  '  # 模拟真实"读完之后才写回去"的窗口：真实的领取也不是瞬间完成的。',
  '  Sys.sleep(hold)',
  '  for (i in seq_along(d$items)) {',
  '    it <- d$items[[i]]',
  '    if (!identical(as.character(it$id %||% ""), "task-A")) next',
  '    if (!identical(as.character(it$status %||% ""), "queued"))',
  '      return(list(data = d, value = list(ok = FALSE, code = "exec_task_taken")))',
  '    d$items[[i]]$status <- "running"',
  '    d$items[[i]]$claimed_by <- who',
  '    return(list(data = d, value = list(ok = TRUE)))',
  '  }',
  '  list(data = d, value = list(ok = FALSE, code = "exec_task_missing"))',
  '})',
  'cat("done", if (isTRUE(res$ok)) "claimed" else paste0("rejected:", res$code %||% ""), "\\n")'
), child)

rscript <- file.path(R.home("bin"), "Rscript")
logs <- file.path(root, c("w1.log", "w2.log"))
# hold 足够长，确保旧实现的两次"读"都发生在任何一次"写"之前。
hold <- "0.8"
for (i in 1:2) {
  system2(rscript, c(child, root, c("worker-1", "worker-2")[i], hold),
          stdout = logs[i], stderr = file.path(root, paste0("w", i, ".err")),
          wait = FALSE)
}
deadline <- Sys.time() + 120
repeat {
  txt <- vapply(logs, function(f) if (file.exists(f))
    paste(readLines(f, warn = FALSE), collapse = "\n") else "", "")
  if (all(grepl("done", txt))) break
  if (Sys.time() > deadline) stop("child processes did not finish")
  Sys.sleep(0.05)
}

out <- jsonlite::fromJSON(qfile, simplifyVector = FALSE)
item <- Filter(function(x) identical(as.character(x$id %||% ""), "task-A"), out$items)[[1]]
claimed <- vapply(logs, function(f) grepl("done claimed", paste(readLines(f, warn = FALSE),
                                                                 collapse = "\n")), TRUE)

fail <- character()
check <- function(label, ok) {
  cat(if (isTRUE(ok)) "PASS:" else "FAIL:", label, "\n")
  if (!isTRUE(ok)) fail <<- c(fail, label)
}

check("exactly one worker reports claimed", sum(claimed) == 1L)
check("queue item ends up running", identical(as.character(item$status %||% ""), "running"))
check("claimed_by is the one that reported success",
      identical(as.character(item$claimed_by %||% ""),
                c("worker-1", "worker-2")[which(claimed)[1]] %||% ""))
check("no leftover lock directory", !dir.exists(paste0(qfile, ".lock")))

if (length(fail)) {
  cat("\nFAILED:", paste(fail, collapse = "; "), "\n")
  quit(save = "no", status = 1L)
}
cat("\nexec lock contract\n")
