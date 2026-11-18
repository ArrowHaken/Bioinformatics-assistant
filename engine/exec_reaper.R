#!/usr/bin/env Rscript
# =====================================================================
#  exec_reaper.R -- 独立队列回收入口（不依赖任何 HTTP 请求）
#
#  为什么必须独立
#    exec_reap() 过去只在 /api/exec/due 与 /api/exec 里顺手调用。worker 离线后
#    不会再请求 due，页面关闭后也不会请求 exec —— 于是「离线 30 分钟回收
#    queued」在真实链路里可能永远不发生。本脚本由 systemd timer / cron /
#    一个每 60 秒循环的 sidecar 拉起，只读队列、心跳并写审计。
#
#  用法
#    Rscript engine/exec_reaper.R [app_dir]
#    PA_APP_DIR=/path/to/app Rscript engine/exec_reaper.R
#
#  安全边界
#    · 文件锁保证同一时刻只有一个 reaper；锁里写 pid，进程不在时自动清掉。
#    · 只加载 serve.R 的定义，不启动 HTTP、不连接生产数据库。
#    · 不调用 execute_shell，也不常驻内存；部署侧决定定时器还是 sidecar。
# =====================================================================

args <- commandArgs(trailingOnly = TRUE)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
app_dir <- Sys.getenv("PA_APP_DIR", unset = "")
if (!nzchar(app_dir) && length(args)) app_dir <- args[[1]]
if (!nzchar(app_dir)) {
  f <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[
    grep("^--file=", commandArgs(trailingOnly = FALSE))])
  app_dir <- if (length(f)) dirname(dirname(normalizePath(f[1]))) else getwd()
}
app_dir <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)

options(pa.serve.definitions_only = TRUE, pa.app_dir = app_dir)
e <- new.env(parent = globalenv())
sys.source(file.path(app_dir, "serve.R"), envir = e)

state_dir <- normalizePath(e$STATE_DIR, winslash = "/", mustWork = FALSE)
dir.create(state_dir, showWarnings = FALSE, recursive = TRUE)
lock_dir <- file.path(state_dir, "exec_reaper.lock")

acquire_lock <- function() {
  if (dir.exists(lock_dir)) {
    holder <- tryCatch(readLines(file.path(lock_dir, "pid"), warn = FALSE)[1],
                       error = function(x) "")
    pid <- suppressWarnings(as.integer(holder))
    alive <- !is.na(pid) && pid > 0L && dir.exists(file.path("/proc", as.character(pid)))
    if (alive) return(FALSE)
    unlink(lock_dir, recursive = TRUE, force = TRUE)
  }
  if (!dir.create(lock_dir, showWarnings = FALSE)) return(FALSE)
  cat(as.character(Sys.getpid()), file = file.path(lock_dir, "pid"))
  TRUE
}

if (!acquire_lock()) {
  cat("exec_reaper: another instance is running\n")
  quit(save = "no", status = 0L)
}
on.exit(unlink(lock_dir, recursive = TRUE, force = TRUE), add = TRUE)

scan_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
result <- tryCatch(e$exec_reap(now = Sys.time()), error = function(err) {
  e$exec_reap_audit_append(list(at = scan_at, kind = "scan_error",
                                error = conditionMessage(err)))
  stop(err)
})
reaped <- attr(result, "reaped") %||% list()
ids <- vapply(reaped, function(x) as.character(x$id %||% ""), "")
e$exec_reap_audit_append(list(at = scan_at, kind = "scan",
                              scanned = length(result$items %||% list()),
                              reaped = length(reaped), ids = as.list(ids)))
cat("exec_reaper: scanned=", length(result$items %||% list()),
    " reaped=", length(reaped), "\n", sep = "")
for (rec in reaped) {
  cat("exec_reaper: reclaimed=", as.character(rec$id %||% ""),
      " status=", as.character(rec$previous_status %||% ""),
      " worker=", as.character(rec$worker_id %||% ""),
      " heartbeat=", as.character(rec$heartbeat_at %||% ""),
      " reason=", as.character(rec$reason_code %||% ""), "\n", sep = "")
}
quit(save = "no", status = 0L)
