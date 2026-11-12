#!/usr/bin/env Rscript
# =====================================================================
#  run_chain.R -- 运行器：把「点按钮」变成「真的跑起来」
#
#  为什么要有这个文件：
#    页面上的按钮不能直接开子进程 —— 它得先能回答「现在在跑吗」「跑到哪了」
#    「退出码多少」「能停吗」。这些都需要一个负责登记状态的中间层。
#    这个文件就是这个中间层。
#
#  用法：
#    Rscript run_chain.R --run-dir <目录> --engine-dir <目录> [--only <步骤>]
#                        [--from <步骤>] [--to <步骤>] [--check-gates]
#
#    --only env        只跑一步
#    --from pipeline   从某一步一路跑到结束（一键全流程）
#
#  关键设计：每一步的命令，在轮到它的时候才解析。
#    因为后面几步的输入是前面几步的产物（出图要 processed.rds，
#    PPT 要 plots 目录）。链启动时全定下来，「出图」必被判死。
#
#  状态写到 <run-dir>/status/step_state.json，日志追加到 <run-dir>/status/run.log。
#  界面靠读这两个文件显示进度，所以格式要保持稳定、写入要原子（先写 .tmp 再改名）。
# =====================================================================

# Remote coordinator definitions share this runner, but never launch the CLI when sourced.
rc_path <- function(sid) file.path(STATE_DIR, paste0("remote_chain_", paste(as.integer(charToRaw(enc2utf8(sid))), collapse = "-"), ".json"))
rc_read <- function(sid) {
  p <- rc_path(sid)
  if (!file.exists(p)) return(NULL)
  jsonlite::fromJSON(read_text_any(p), simplifyVector = FALSE)
}
rc_write <- function(c) { write_json_file(rc_path(c$session_id), c); c }
rc_dispatch <- function(c) {
  stage <- as.character(c$todo[[c$index]])
  # Clone only the request closure: callbacks retain their original session, regardless
  # of UI selection. No synthetic public request flag can bypass this binding.
  ctx <- new.env(parent = environment(serve_request))
  ctx$cur_sid <- function() c$session_id
  ctx$read_state <- function() list(runDir = c$run_dir, runDirRemote = TRUE)
  ctx$flow_profile <- function() "server"
  ctx$RC_DISPATCH <- TRUE
  ctx$db_api_owner <- function(...) c$owner_ctx
  ctx$send_json <- function(con, code, body) list(code = code, body = body)
  handler <- serve_request; environment(handler) <- ctx
  ans <- handler(NULL, list(target = "/api/run", method = "POST", headers = list(),
    body = as.character(jsonlite::toJSON(list(step = stage, session_id = c$session_id,
      req_id = c$req_id), auto_unbox = TRUE))))
  c$status <- if (ans$code == 200L) "running" else "blocked"
  c$blocked <- if (ans$code == 200L) NULL else ans$body
  rc_write(c)
  ans$body$chain_id <- c$req_id
  ans
}
rc_begin <- function(sid, run_dir, req_id = "", owner_ctx = list(), overwrite = FALSE) {
  c <- rc_read(sid)
  if (!is.null(c)) {
    same <- !nzchar(req_id) || identical(req_id, c$req_id)
    if (identical(c$status, "blocked") && same) return(rc_dispatch(c))
    if (same && c$status %in% c("running", "starting", "done"))
      return(list(code = 200L, body = list(ok = TRUE, delegated = identical(c$status, "running"),
        duplicate = TRUE, chain_id = c$req_id, step = c$todo[[c$index]], chain_status = c$status)))
    if (same && c$status %in% c("failed", "superseded")) {
      # 上一次这条链没跑成，再点一键全跑就是「重跑」。
      #   原来这里也回 duplicate=TRUE：页面拿到 ok 就画成"已交接"，
      #   实际一条任务都没入队 —— 每一次点击都石沉大海，屏幕上却显示成功。
      #   现在先把旧链标成被取代，再当新请求重开一轮。
      c$status <- "superseded"
      c$superseded_at <- exec_ts()
      rc_write(c)
      c <- NULL
    }
    if (!is.null(c) && !same && c$status %in% c("running", "starting", "blocked")) {
      if (!isTRUE(overwrite))
        return(list(code = 409L, body = list(ok = FALSE, code = "chain_in_progress")))
      active <- Filter(function(it) identical(it$session_id, sid) &&
        identical(it$chain_id, c$req_id) && it$status %in% c("queued", "running"),
        exec_read()$items %||% list())
      if (length(active))
        return(list(code = 409L, body = list(ok = FALSE, code = "session_execution_busy",
          error = "旧任务仍在执行或排队，等待它退出后才能安全重跑。")))
      c$status <- "superseded"
      c$superseded_by <- req_id
      c$superseded_at <- exec_ts()
      rc_write(c)
    }
  }
  todo <- Filter(function(id) {
    st <- flow_stage(id, "server")
    !is.null(st) && !nzchar(as.character(st$cover_by %||% ""))
  }, FLOW_ORDER("server"))
  if (!length(todo)) return(list(code = 409L, body = list(ok = FALSE, code = "empty_remote_chain")))
  busy <- Filter(function(it) identical(it$session_id, sid) && it$status %in% c("queued", "running"), exec_read()$items %||% list())
  if (length(busy)) return(list(code = 409L, body = list(ok = FALSE, code = "session_execution_busy")))
  if (!nzchar(req_id)) req_id <- paste0("chain-", gsub("[^A-Za-z0-9]", "", exec_ts()), "-", paste(sample(c(letters, 0:9), 8, TRUE), collapse = ""))
  if (isTRUE(HAS_SESSIONS)) {
    saved <- sess_set_pending(APP_DIR, sid, req_id, slot = "step")
    if (isFALSE(saved$ok))
      return(list(code = 409L, body = list(ok = FALSE, code = "chain_request_not_saved",
        reason = as.character(saved$error %||% "pending persistence failed"))))
    actual <- tryCatch(sess_get_pending(APP_DIR, sid, slot = "step"), error = function(e) NULL)
    if (!identical(as.character(actual %||% ""), req_id))
      return(list(code = 409L, body = list(ok = FALSE, code = "chain_request_not_saved",
        reason = "pending request readback mismatch")))
  }
  c <- list(session_id = sid, run_dir = run_dir, req_id = req_id, todo = as.list(todo),
    index = 1L, status = "starting", completed = list(), owner_ctx = owner_ctx)
  rc_write(c)
  rc_dispatch(c)
}
rc_validate <- function(b) {
  sid <- as.character(b$session_id %||% ""); rq <- as.character(b$req_id %||% "")
  c <- rc_read(sid)
  bad <- function(code) list(code = 409L, body = list(ok = FALSE, code = code))
  # Prevent a chain request id being imported into another session.
  if (nzchar(rq)) for (p in list.files(STATE_DIR, "^remote_chain_.*[.]json$", full.names = TRUE)) {
    other <- jsonlite::fromJSON(read_text_any(p), simplifyVector = FALSE)
    if (identical(rq, other$req_id) && !identical(sid, other$session_id)) return(bad("chain_session_mismatch"))
  }
  if (is.null(c)) return(NULL)
  if (!identical(rq, c$req_id)) return(bad("chain_request_mismatch"))
  step <- as.character(b$step %||% "")
  if (step %in% unlist(c$completed) && identical(b$status, "done"))
    return(list(code = 200L, body = list(ok = TRUE, duplicate = TRUE, chain_id = c$req_id)))
  if (!identical(c$status, "running") || !identical(step, c$todo[[c$index]])) return(bad("chain_stage_not_active"))
  NULL
}
rc_callback <- function(b, accepted) {
  c <- rc_read(as.character(b$session_id %||% ""))
  if (is.null(c) || !identical(c$status, "running") ||
      !identical(as.character(b$req_id %||% ""), c$req_id) ||
      !identical(as.character(b$step), c$todo[[c$index]])) return(NULL)
  has_sub <- nzchar(as.character(b$sub %||% ""))
  sub_status <- as.character(b$sub_status %||% b$status)
  sub_terminal <- has_sub && sub_status %in% c("failed", "stopped")
  if (!b$status %in% c("done", "failed", "stopped") && !sub_terminal) return(NULL)
  if (has_sub && identical(sub_status, "done")) return(NULL)
  success <- !has_sub && isTRUE(accepted) && identical(b$status, "done") &&
    length(b$code) == 1L && !is.na(b$code) && identical(as.integer(b$code), 0L)
  for (it in exec_read()$items %||% list()) if (identical(it$chain_id, c$req_id) && identical(it$step, b$step))
    exec_set(it$id, list(callback_received = TRUE, callback_accepted = if (has_sub) FALSE else success))
  if (!success) { c$status <- "failed"; rc_write(c); return(list(status = "failed")) }
  c$completed <- c(c$completed, list(b$step))
  if (c$index >= length(c$todo)) { c$status <- "done"; rc_write(c); return(list(status = "done")) }
  c$index <- c$index + 1L; c$status <- "starting"; rc_write(c)
  rc_dispatch(c)
}

if (!isTRUE(getOption("pa.chain.definitions_only", FALSE))) {
suppressWarnings(options(stringsAsFactors = FALSE, scipen = 999, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# 自己的位置（这一步必须放在最前面：下面算 ENGINE_DIR 的默认值要用它）
#   只用 sys.frame(1)$ofile 在 Rscript 下会拿到 NULL，所以补 --file= 退路。
SELF_DIR <- local({
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

args <- commandArgs(trailingOnly = TRUE)
get_opt <- function(k, d = NULL) {
  i <- match(k, args)
  if (is.na(i)) return(d)
  if (i == length(args) || grepl("^--", args[i + 1])) return(d)
  args[i + 1]
}
has_flag <- function(k) k %in% args

RUN_DIR    <- get_opt("--run-dir")
ENGINE_DIR <- get_opt("--engine-dir")
ONLY       <- get_opt("--only", NULL)
FROM       <- get_opt("--from", NULL)
TO         <- get_opt("--to", NULL)
CHECK_GATES <- has_flag("--check-gates")

if (is.null(RUN_DIR) || is.na(RUN_DIR) || !dir.exists(RUN_DIR)) {
  cat("run_chain.R: --run-dir 不可用\n"); quit(save = "no", status = 2)
}
RUN_DIR <- normalizePath(RUN_DIR, winslash = "/", mustWork = FALSE)
if (is.null(ENGINE_DIR) || is.na(ENGINE_DIR)) ENGINE_DIR <- SELF_DIR
if (!dir.exists(ENGINE_DIR)) {
  cat("run_chain.R: --engine-dir 不可用\n"); quit(save = "no", status = 2)
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("run_chain.R: 缺少 jsonlite 包\n"); quit(save = "no", status = 2)
}

# ---- 载入「每一步跑什么」的唯一出处 ----
plan_file <- file.path(ENGINE_DIR, "step_plan.R")
if (!file.exists(plan_file)) {
  cat("run_chain.R: 找不到 step_plan.R：", plan_file, "\n"); quit(save = "no", status = 2)
}
source(plan_file, local = TRUE)

STATUS_DIR <- file.path(RUN_DIR, "status")
dir.create(STATUS_DIR, showWarnings = FALSE, recursive = TRUE)
if (!identical(normalizePath(STATUS_DIR, winslash = "/", mustWork = TRUE),
               paste0(RUN_DIR, "/status"))) {
  cat("run_chain.R: status must be inside run-dir, not a redirected root\n")
  quit(save = "no", status = 3L)
}
LOGF <- file.path(STATUS_DIR, "run.log")
STF  <- file.path(STATUS_DIR, "step_state.json")

say <- function(txt) {
  con <- file(LOGF, open = "ab")
  writeBin(charToRaw(enc2utf8(paste0(txt, "\n"))), con)
  close(con)
}
LAST_FAILURE_CODE <- 1L
write_state <- function(status, step, code) {
  if (identical(status, "failed") && length(code) == 1L && !is.na(code))
    LAST_FAILURE_CODE <<- as.integer(code)
  st <- list(status = status, step = step, code = code,
             started = STARTED, ended = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
             pid = Sys.getpid())
  f <- paste0(STF, ".tmp")
  con <- file(f, open = "wb")
  writeBin(charToRaw(enc2utf8(jsonlite::toJSON(st, auto_unbox = TRUE))), con)
  close(con)
  file.rename(f, STF)
}

STARTED <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

# ---- 决定要跑哪几步 ----
all_ids <- CHAIN_ORDER
if (!is.null(ONLY) && nzchar(ONLY)) {
  if (!ONLY %in% all_ids) { cat("run_chain.R: 未知步骤 ", ONLY, "\n"); quit(save = "no", status = 2) }
  todo <- ONLY
  MODE <- "single"
} else {
  start_i <- if (!is.null(FROM) && nzchar(FROM) && FROM %in% all_ids) match(FROM, all_ids) else 1L
  end_i   <- if (!is.null(TO) && nzchar(TO) && TO %in% all_ids) match(TO, all_ids) else length(all_ids)
  if (end_i < start_i) end_i <- start_i
  todo <- all_ids[start_i:end_i]
  MODE <- "chain"
}

# ---- 闸门检查 ----
#  按「要跑的那几步各自要求的段位」分别检查。
#  为什么不能笼统查「有没有 pending」：
#    B 段那几条（筛群、剔样本、低置信门槛）的依据，只有跑到一次聚类才产生。
#    用笼统检查的话，pipeline1（上段）会被还没到该问的 B 段问题挡住 ——
#    结果就是「永远跑不到能产生依据的那一步」，卡死。
if (CHECK_GATES) {
  cfg0 <- read_cfg(RUN_DIR)
  if (!is.null(cfg0)) {
    ss0 <- build_steps(cfg0, RUN_DIR, ENGINE_DIR)
    names(todo) <- NULL
    for (id in todo) {
      hit <- Filter(function(s) identical(s$id, id), ss0)
      if (!length(hit) || !isTRUE(hit[[1]]$needGate)) next
      ph <- step_gate_phase(id)
      pend <- pending_gate_names(RUN_DIR, ph)
      if (!is.null(pend) && length(pend)) {
        write_state("blocked", id, NULL)
        say("")
        say(sprintf("=== 停住：这一步要先定 %d 件事 ===", length(pend)))
        for (p in pend) say(paste0("  - ", p))
        say("")
        say("请在页面上答完这几项、点「保存我的设置」，再回来点运行。")
        say("（这几条今天要定，是因为后面的做法取决于你的选择。）")
        quit(save = "no", status = 0)
      }
    }
  }
}

# Capture is isolated from analysis execution: failures are visible, never called complete.
source(file.path(ENGINE_DIR, "run_notebook.R"), local = TRUE)
notebook_capture <- tryCatch(rn_init(RUN_DIR, ENGINE_DIR, todo, MODE), error = function(e) {
  say(paste0("[notebook] capture initialization failed; execution refused: ", conditionMessage(e)))
  quit(save = "no", status = 3L)
})

# Feature checkpoint: gate on rn_init before claiming; runner/test are one coupled
# stream; per-attempt IDs isolate mutable state; one owner preserves capture order.
# Nested delegation and Git/PR are excluded by this task's scope.
lifecycle <- NULL
child <- NULL
finish_capture <- function(success) rn_finish(notebook_capture, success)
lifecycle_boot <- tryCatch({
  source(file.path(ENGINE_DIR, "run_lifecycle.R"), local = TRUE)
  lifecycle <- new.env(parent = emptyenv())
  lifecycle$run <- notebook_capture$attempt_id
  lifecycle$attempt <- notebook_capture$attempt_id
  lifecycle$worker <- paste0("runner-", Sys.getpid())
  lifecycle$n <- 0L
  rl_create(STATUS_DIR, lifecycle$run, lifecycle$attempt, "create",
    payload = list(todo = as.list(todo), mode = MODE, evidence_scope = "execution only; scientific validity not verified"))
  rl_claim(STATUS_DIR, lifecycle$run, lifecycle$attempt, lifecycle$worker, "claim")
  TRUE
}, error = function(e) {
  say(paste0("[lifecycle] initialization failed; execution refused: ", conditionMessage(e)))
  FALSE
})
if (!lifecycle_boot) {
  tryCatch(finish_capture(FALSE), error = function(e) say(conditionMessage(e)))
  write_state("failed", todo[1], 3L)
  quit(save = "no", status = 3L)
}
heartbeat <- function() {
  lifecycle$n <- lifecycle$n + 1L
  rl_heartbeat(STATUS_DIR, lifecycle$run, lifecycle$attempt, lifecycle$worker,
    paste0("heartbeat-", lifecycle$n))
}
finish_lifecycle <- function(success, evidence = character(), reason = NULL) {
  rl_finish(STATUS_DIR, lifecycle$run, lifecycle$attempt, lifecycle$worker,
    "finish", if (success) "succeeded" else "failed", evidence, reason)
}
stop_child <- function() {
  if (!is.null(child)) {
    try(child$kill_tree(), silent = TRUE)
    try(child$kill(), silent = TRUE)
    try(child$wait(timeout = 2000), silent = TRUE)
    child <<- NULL
  }
}
# step_plan currently returns raw argv. Older callers may supply shQuote tokens.
# Decode only explicitly quoted tokens, never evaluate shell code or expansions.
command_arg <- function(x) {
  if (!grepl("^[\"']", x)) return(x)
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  out <- character(); quote <- ""; i <- 1L
  while (i <= length(chars)) {
    c <- chars[i]
    if (quote == "'" && c != "'") out <- c(out, c)
    else if (c == quote && nzchar(quote)) quote <- ""
    else if (!nzchar(quote) && c %in% c("'", '\"')) quote <- c
    else if (c == "\\" && quote != "'") {
      i <- i + 1L
      if (i > length(chars)) stop("Incomplete quoted argv")
      out <- c(out, chars[i])
    } else if (!nzchar(quote) && grepl("[[:space:]$`;|&<>]", c)) stop("Ambiguous shell argv refused")
    else if (quote == '\"' && c %in% c("$", "`")) stop("Shell expansion in argv refused")
    else out <- c(out, c)
    i <- i + 1L
  }
  if (nzchar(quote)) stop("Unclosed quoted argv")
  paste0(out, collapse = "")
}

write_state("running", todo[1], NULL)
say("")
if (identical(MODE, "chain")) {
  say(sprintf("########## [%s] 一键全流程开始：%s ##########",
              format(Sys.time(), "%H:%M:%S"), paste(todo, collapse = " -> ")))
} else {
  say(sprintf("########## [%s] 开始：%s ##########", format(Sys.time(), "%H:%M:%S"), todo[1]))
}

run_one <- function(stepId) {
  # 轮到它了，才问「这一步现在跑什么」
  r <- tryCatch(resolve_step_cli(stepId, RUN_DIR, ENGINE_DIR),
                error = function(e) list(error = conditionMessage(e)))
  if (!is.null(r$error)) {
    say(sprintf("\n===== [%s] %s =====", format(Sys.time(), "%H:%M:%S"), stepId))
    say(paste0("跑不了这一步：", r$error))
    write_state("failed", stepId, 2L)
    if (!is.null(notebook_capture)) tryCatch(rn_record(notebook_capture,
      list(step = stepId, title = stepId, exportable = FALSE,
           reason = paste("Command resolution failed:", r$error)), 2L),
      error = function(e) {
        notebook_capture$capture_ok <- FALSE
        say(paste0("[notebook] failure recording failed: ", conditionMessage(e)))
      })
    return(FALSE)
  }

  say(sprintf("\n===== [%s] %s（%s） =====", format(Sys.time(), "%H:%M:%S"), stepId, r$title))
  say(sprintf("$ %s %s", r$cmd, paste(r$args, collapse = " ")))
  write_state("running", stepId, NULL)

  captured <- if (!is.null(notebook_capture)) tryCatch(
    rn_prepare(notebook_capture, stepId, r), error = function(e) {
      notebook_capture$capture_ok <- FALSE
      say(paste0("[notebook] script capture failed: ", conditionMessage(e))); NULL
    }) else NULL

  txt <- ""
  code <- tryCatch({
    if (!requireNamespace("processx", quietly = TRUE))
      stop("processx is required; refusing execution without supervised heartbeats")
    argv <- vapply(as.character(r$args), command_arg, "")
    heartbeat()
    child <<- processx::process$new(r$cmd, argv, wd = r$cwd,
      stdout = "|", stderr = "2>&1", cleanup = TRUE, cleanup_tree = TRUE,
      supervise = TRUE)
    next_beat <- as.numeric(Sys.time()) + 0.5
    drain <- function() {
      chunk <- child$read_output()
      if (nzchar(chunk)) {
        txt <<- paste0(txt, chunk)
        con <- file(LOGF, open = "ab")
        tryCatch(writeBin(charToRaw(enc2utf8(chunk)), con), finally = close(con))
      }
    }
    repeat {
      child$poll_io(100)
      drain()
      if (as.numeric(Sys.time()) >= next_beat) {
        heartbeat()
        next_beat <- as.numeric(Sys.time()) + 0.5
      }
      if (!child$is_alive()) break
    }
    child$wait()
    drain()
    exit <- child$get_exit_status()
    stop_child()
    exit
  }, error = function(e) {
    stop_child()
    say(paste("Supervised execution failed:", conditionMessage(e)))
    127L
  }, interrupt = function(e) {
    stop_child()
    say("Supervised execution interrupted; this invocation's child terminated")
    130L
  })

  code <- as.integer(code)
  say(sprintf("----- [%s] %s 结束，退出码 %s -----", format(Sys.time(), "%H:%M:%S"), stepId, code))
  if (!is.null(captured)) tryCatch(rn_record(notebook_capture, captured, code, txt),
    error = function(e) {
      notebook_capture$capture_ok <- FALSE
      say(paste0("[notebook] recording failed: ", conditionMessage(e)))
    })
  if (code != 0L) { write_state("failed", stepId, code); return(FALSE) }

  # ---- 上段跑完，立刻把人工判断依据刷进闸门 ----
  #   这一步不能少，也不能放到界面上去做。
  #   理由：依据是这次运行的产物（簇大小、样本分布、marker），
  #   只有脚本刚算完的这一刻它才在手上。界面只是一份展示，
  #   让界面负责「把依据接进闸门」等于把一次数据搬运交给两处实现，
  #   早晚出现「算出来了但没显示」或者「显示了但是上一轮的」。
  if (identical(stepId, "pipeline1")) {
    dec <- file.path(ENGINE_DIR, "decision.R")
    if (file.exists(dec)) {
      ok <- tryCatch({
        source(dec, local = TRUE)
        sync_gate_evidence(RUN_DIR, ENGINE_DIR)
      }, error = function(e) { say(paste0("[warn] 依据未能刷进闸门：", conditionMessage(e))); FALSE })
      if (isTRUE(ok)) {
        say("已把本次实测依据写进闸门（簇大小 / 样本分布 / top marker）。")
        say("请到界面上「有数据了再定」那一组里做选择。")
      }
    }
  }

  TRUE
}

ok_all <- TRUE
for (i in seq_along(todo)) {
  ok <- tryCatch(run_one(todo[[i]]), error = function(e) {
    stop_child(); say(paste0("[runner] failed: ", conditionMessage(e)))
    write_state("failed", todo[[i]], 1L); FALSE
  }, interrupt = function(e) {
    stop_child(); write_state("failed", todo[[i]], 130L); FALSE
  })
  if (!ok) {
    ok_all <- FALSE
    if (identical(MODE, "chain")) {
      say("这一步没跑通。全流程在这里停住，不会带着错往下走。")
      say(sprintf("修好之后，可以在页面上单点「%s」这一步继续，或者重新点一键全流程。",
                  if (i < length(todo)) todo[[i + 1]] else todo[[i]]))
    }
    break
  }
}

# Completion is decided only after this attempt's capture has been persisted.
nr <- tryCatch(finish_capture(ok_all), error = function(e) {
  say(paste0("[notebook] build failed: ", conditionMessage(e)))
  NULL
})
evidence <- character()
if (ok_all) {
  proof <- file.path(notebook_capture$attempt_dir, "status", "attempt.json")
  attempt_report <- tryCatch(jsonlite::fromJSON(proof), error = function(e) NULL)
  if (is.null(nr) || !isTRUE(nr$execution_succeeded) ||
      !identical(nr$attempt_id, lifecycle$attempt) ||
      !identical(attempt_report$attempt_id, lifecycle$attempt) ||
      !isTRUE(attempt_report$execution_succeeded) || !is.null(nr$build_error) ||
      !isTRUE(notebook_capture$capture_ok)) {
    ok_all <- FALSE
    say("[notebook] Current attempt completion evidence unavailable; run failed")
  } else {
    evidence <- proof
    notebook <- file.path(notebook_capture$attempt_dir, "reproducible.ipynb")
    if (!is.null(nr$notebook) && file.exists(notebook)) evidence <- c(evidence, notebook)
  }
}
terminal_ok <- tryCatch({
  finish_lifecycle(ok_all, evidence, if (ok_all)
    "Commands exited zero; current attempt execution evidence only. Full replay and scientific validity NOT verified."
    else "Execution or current-attempt notebook finalization failed; see run.log")
  TRUE
}, error = function(e) {
  say(paste0("[lifecycle] terminal write failed: ", conditionMessage(e)))
  FALSE
})
if (!terminal_ok) ok_all <- FALSE
write_state(if (ok_all) "done" else "failed",
  if (ok_all && identical(MODE, "chain")) "all" else todo[[i]], if (ok_all) 0L else LAST_FAILURE_CODE)
if (!is.null(nr)) say(paste0("[notebook] attempt ", lifecycle$attempt,
  if (isTRUE(nr$capture_complete)) " — scripts captured; full replay NOT verified" else " — INCOMPLETE capture"))
if (ok_all) say("Execution completed; current attempt evidence recorded. Scientific validity NOT verified.")
# Export is a separate terminal outcome: never turn completed computation into a retry.
auto_export <- tryCatch({
  auto_env <- new.env(parent = baseenv())
  sys.source(file.path(SELF_DIR, "notebook_auto_export.R"), envir = auto_env)
  auto_env$nae_export(dirname(SELF_DIR), ENGINE_DIR, RUN_DIR, lifecycle$attempt,
    ok_all, terminal_ok, run_path = get_opt("--run-dir"))
}, error = function(e) list(status = "failed", code = "auto_hook_failed"))
say(paste0("[notebook auto export] ", auto_export$status, " (", auto_export$code,
  "); declared dependencies only; scientific validity NOT verified."))
stop_child()
quit(save = "no", status = if (ok_all) 0L else 1L)

} # local CLI guard
