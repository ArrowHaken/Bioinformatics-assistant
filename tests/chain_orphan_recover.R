#!/usr/bin/env Rscript
# 孤儿链契约：链停在 running，但队列里已经没有它的任务时，再点必须能重新派出去。
#
# 为什么必须有这条（2026-11-23，gina 报"旧任务一直卡着，新任务进不去"）：
#   rc_begin 原来对 "同一条链 + running" 直接回 duplicate=TRUE，假设 running
#   的链一定有一条任务在队列里推进。实测不成立：任务被人工作废、被回收器
#   标成 failed、或回写失败后链没收到通知，链就停在 running 而队列里一条不剩。
#   此后每次点击都回"已交接"，页面画成成功，实际什么都没派出去。
#
# 用法：
#   Rscript tests/chain_orphan_recover.R [app_dir]
#   app_dir 默认当前目录；传修复前的目录应失败（fail-first）。
#
# 判据：
#   1) 孤儿链（running 但名下无 queued/running 任务）被判为 superseded 并留痕
#   2) 有任务的 running 链仍然回 duplicate，不被误判
#   3) paused 链不受影响

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

args <- commandArgs(trailingOnly = TRUE)
app_src <- normalizePath(if (length(args)) args[[1]] else ".", mustWork = TRUE)
chain_file <- file.path(app_src, "engine", "run_chain.R")
stopifnot(file.exists(chain_file))

fail <- character()
check <- function(label, ok) {
  cat(if (isTRUE(ok)) "PASS:" else "FAIL:", label, "\n")
  if (!isTRUE(ok)) fail <<- c(fail, label)
}

# 只加载 run_chain.R 的函数定义，其余依赖全部桩掉。
#   run_chain.R 的主体本来就包在 if (!getOption("pa.chain.definitions_only"))
#   里，CLI 入口也在其中。开这个开关再 sys.source 就只加载定义、不会执行到
#   --run-dir 检查并 quit()。不用自己切文件：文件里有跨行表达式，按行切会断括号。
make_env <- function(root) {
  old <- options(pa.chain.definitions_only = TRUE)
  on.exit(options(old), add = TRUE)
  e <- new.env(parent = globalenv())
  e$APP_DIR <- root
  e$STATE_DIR <- root
  e$`%||%` <- `%||%`
  e$exec_ts <- function(t = Sys.time()) format(t, "%Y-%m-%dT%H:%M:%S")
  e$read_text_any <- function(p) paste(readLines(p, warn = FALSE), collapse = "\n")
  e$write_json_file <- function(p, obj) {
    writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null", digits = NA), p)
  }
  e$rc_path <- function(sid) file.path(root, paste0("chain_", sid, ".json"))
  e$rc_read <- function(sid) {
    p <- e$rc_path(sid)
    if (!file.exists(p)) return(NULL)
    jsonlite::fromJSON(e$read_text_any(p), simplifyVector = FALSE)
  }
  e$rc_write <- function(c) { e$write_json_file(e$rc_path(c$session_id), c); c }
  e$items <- list()
  e$exec_read <- function() list(items = e$items)
  e$exec_reap <- function(...) NULL
  e$FLOW_ORDER <- function(profile = "server") character(0)
  e$flow_stage <- function(id, profile = "server") NULL
  e$rc_dispatch <- function(c) list(code = 200L, body = list(ok = TRUE, dispatched = TRUE))
  # pa.chain.definitions_only 已经打开，sys.source 只会加载定义，不会跑到 CLI 入口。
  sys.source(chain_file, envir = e)
  e
}

seed_chain <- function(e, sid, status, req_id, index = 1L) {
  e$rc_write(list(session_id = sid, req_id = req_id, status = status,
                  index = index, todo = list("doublet", "merge"), run_dir = "/tmp/x"))
}

root <- tempfile("chain-orphan-")
dir.create(root)
on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

# --- 场景 1：孤儿链必须自愈 ---
e <- make_env(root)
sid <- "s-orphan-1"
seed_chain(e, sid, "running", "chain-orphan-A")
e$items <- list()   # 队列里没有这条链的任务
res <- tryCatch(e$rc_begin(sid, "/tmp/x", req_id = "chain-orphan-A", owner_ctx = list()),
                error = function(err) list(code = -1L, error = conditionMessage(err)))
after <- e$rc_read(sid)
check("orphan chain is marked superseded",
      identical(as.character(after$status %||% ""), "superseded"))
check("orphan recovery leaves a trace",
      nzchar(as.character(after$orphan_recovered_at %||% "")) &&
        nzchar(as.character(after$orphan_recovered_reason %||% "")))
check("orphan chain is not reported as duplicate",
      !isTRUE(res$body$duplicate))

# --- 场景 2：真的有任务在跑时，不能被误判成孤儿 ---
e2 <- make_env(root)
sid2 <- "s-live-2"
seed_chain(e2, sid2, "running", "chain-live-B")
e2$items <- list(list(id = "ex-1", session_id = sid2, chain_id = "chain-live-B",
                      step = "doublet", status = "running"))
res2 <- e2$rc_begin(sid2, "/tmp/x", req_id = "chain-live-B", owner_ctx = list())
after2 <- e2$rc_read(sid2)
check("live chain stays running", identical(as.character(after2$status %||% ""), "running"))
check("live chain still reports duplicate", isTRUE(res2$body$duplicate))

# --- 场景 3：排队中的也算在管，不能自愈 ---
e3 <- make_env(root)
sid3 <- "s-queued-3"
seed_chain(e3, sid3, "running", "chain-queued-C")
e3$items <- list(list(id = "ex-2", session_id = sid3, chain_id = "chain-queued-C",
                      step = "doublet", status = "queued"))
e3$rc_begin(sid3, "/tmp/x", req_id = "chain-queued-C", owner_ctx = list())
after3 <- e3$rc_read(sid3)
check("queued task keeps chain running",
      identical(as.character(after3$status %||% ""), "running"))

# --- 场景 4：paused 优先，不被自愈顺手重启 ---
e4 <- make_env(root)
sid4 <- "s-paused-4"
seed_chain(e4, sid4, "paused", "chain-paused-D")
e4$items <- list()
res4 <- e4$rc_begin(sid4, "/tmp/x", req_id = "chain-paused-D", owner_ctx = list())
after4 <- e4$rc_read(sid4)
check("paused chain is untouched",
      identical(as.character(after4$status %||% ""), "paused") &&
        identical(as.character(res4$body$code %||% ""), "chain_paused"))

if (length(fail)) {
  cat("\nFAILED:", paste(fail, collapse = "; "), "\n")
  quit(save = "no", status = 1L)
}
cat("\nchain orphan recover contract\n")
