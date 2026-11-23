#!/usr/bin/env Rscript
# 链派工标记与 blocked 死锁的回归测试。
#   两个历史缺陷，各自有一条能真失败的断言：
#   1) 链自己派第一步时，run_enqueue 看不到派工标记，被「链进行中」守卫 409，
#      链写成 blocked。判据：rc_dispatch 交给 handle 的 req 上必须带标记，
#      且守卫在 chain_dispatch=TRUE 时不拦。
#   2) blocked 被当成「在跑」拦单步，链一旦 blocked 就永无出口。
#      判据：blocked 状态下守卫放行；再用新请求号点一次全流程能重开。
suppressPackageStartupMessages(library(jsonlite))
root <- tempfile("chain-dispatch-flag-", tmpdir = "/tmp")
dir.create(root, recursive = TRUE)
STATE_DIR <- root
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
write_json_file <- function(path, value)
  writeLines(as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null",
                                           simplifyVector = FALSE)), path)
read_text_any <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
exec_ts <- function() "test-time"
exec_items <- list()
exec_read <- function() list(items = exec_items)
exec_set <- function(id, fields) invisible(TRUE)
HAS_SESSIONS <- FALSE
FLOW_ORDER <- function(profile) c("first", "second")
flow_stage <- function(id, profile) list(cover_by = "")

captured_req <- NULL
exec_with_lock <- function(fn) fn()
serve_request <- function(con, req) {
  captured_req <<- req
  # 模拟修复前的坏行为只需改这里：标记丢失时 handler 会被守卫拦住。
  flag <- isTRUE(req$pa_chain_dispatch)
  if (!flag) list(code = 409L, body = list(ok = FALSE, code = "chain_in_progress"))
  else list(code = 200L, body = list(ok = TRUE, delegated = TRUE))
}

options(pa.chain.definitions_only = TRUE)
source("engine/run_chain.R")

checks <- 0L
check <- function(x, what) {
  if (!isTRUE(x)) stop("FAIL: ", what)
  checks <<- checks + 1L
  cat("ok  ", what, "\n")
}

chain <- list(session_id = "s-a", run_dir = "/shared/out", req_id = "req-1",
              todo = as.list(c("first", "second")), index = 1L,
              status = "starting", completed = list(), owner_ctx = list())
rc_write(chain)

# --- 缺陷 1：派工标记必须真的到达 handler，链才会进 running ---
ans <- rc_dispatch(chain)
check(isTRUE(captured_req$pa_chain_dispatch), "rc_dispatch 把派工标记写在 req 上")
check(identical(ans$code, 200L), "带标记的派工返回 200，不再被自己的守卫 409")
check(identical(rc_read("s-a")$status, "running"), "派工成功后链状态是 running")

# 同样一次派工，把标记抹掉，必须复现成 blocked。这条是反向对照，
#   证明上面的断言不是恒真。
captured_req <- NULL
chain$status <- "starting"
rc_write(chain)
save_flag <- TRUE
serve_request_ok <- serve_request
serve_request <- function(con, req) {
  captured_req <<- req
  list(code = 409L, body = list(ok = FALSE, code = "chain_in_progress"))
}
ans <- rc_dispatch(chain)
check(identical(rc_read("s-a")$status, "blocked"), "派工被 409 时链写成 blocked（对照）")
serve_request <- serve_request_ok

# --- 缺陷 2：blocked 不能再当成「在跑」拦单步 ---
blocked <- list(session_id = "s-a", run_dir = "/shared/out", req_id = "req-1",
                todo = as.list(c("first", "second")), index = 1L,
                status = "blocked", completed = list(), owner_ctx = list())
check(is.null(rc_step_guard(blocked, FALSE)), "blocked 状态下单步守卫放行")
check(is.null(rc_step_guard(blocked, TRUE)), "链派工时守卫一律放行")
running <- within(blocked, status <- "running")
check(identical(rc_step_guard(running, FALSE)$body$code, "chain_in_progress"),
      "running 状态下单步仍然拦，code=chain_in_progress")
check(is.null(rc_step_guard(running, TRUE)), "链自己派下一步时 running 不拦")
paused <- within(blocked, status <- "paused")
check(identical(rc_step_guard(paused, FALSE)$body$code, "chain_paused"),
      "paused 仍然拦，且用 chain_paused 而不是 chain_in_progress")
check(identical(rc_step_guard(paused, TRUE)$body$code, "chain_paused"),
      "链派工也不能绕过暂停")

# --- 缺陷 2 的出口：blocked 后用新请求号点一次全流程，必须能重开 ---
rc_write(blocked)
out <- rc_begin("s-a", "/shared/out", "req-2", list(), overwrite = FALSE)
check(identical(out$code, 200L), "blocked 链用新请求号重开，返回 200 而不是 chain_in_progress")
fresh <- rc_read("s-a")
check(identical(fresh$req_id, "req-2"), "重开后链使用新的请求号")

# 同号再点：blocked + same 走恢复派发，不是重开。
rc_write(blocked)
out <- rc_begin("s-a", "/shared/out", "req-1", list(), overwrite = FALSE)
check(identical(out$code, 200L), "同请求号点 blocked 链走恢复派发")

cat("\nPASS", checks, "项断言\n")
