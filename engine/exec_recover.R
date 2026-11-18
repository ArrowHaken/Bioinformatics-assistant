# 误报纠正（候选 v6）：把"无任何执行证据的失败投影"改回运行中
# ---------------------------------------------------------------------------
# 症状（2026-11-14 19:11，figs）：
#   集群作业一直在跑（qstat 为 r，qacct 无记录），页面却显示失败。
#   那份失败投影里 code / log / run_dir / artifacts 全空，也没有退出码。
#
# 边界（刻意做窄）：
#   · 只处理同 session + 同 step + 同 req，且那一环当前有效值就是这条失败投影；
#     若当前有效值已是 done/verified，一律拒绝，绝不回退去读旧 failed。
#   · 该 req 已 verified、已有后继 pending 请求、已有更晚执行任务，一律拒。
#   · 集群事实只能由执行端读取后上报；qacct_terminal 必须是显式 false，
#     心跳只认服务端任务上的 last_seen，不认客户端自报。
#   · 判定依据的查询失败（验收数、最新任务）一律拒绝，不当成"没有"。
#   · 旧 attempt 文件不删不改；纠正写一份新投影 + 一份新终态槽位 + 审计。
#
# 为什么需要新终态槽位：
#   旧 terminal 是第一次结论的不可变事实。恢复之后真实结果还要能写进来，
#   又不能覆盖它，所以由恢复记录指定一个新槽位，那个槽位同样只允许第一次写入。

recover_sha <- function(x) {
  if (requireNamespace("digest", quietly = TRUE))
    return(digest::digest(x, algo = "sha256", serialize = FALSE))
  NULL
}

recover_file_sha <- function(f) {
  if (!file.exists(f)) return("")
  tryCatch(recover_sha(paste(readLines(f, warn = FALSE), collapse = "\n")),
           error = function(e) "")
}

recover_blank <- function(x) {
  v <- as.character(x %||% "")
  if (!length(v) || is.na(v[1])) "" else trimws(v[1])
}

# 投影是否"没有任何执行证据"：四项全空才算。
recover_no_evidence <- function(rec) {
  if (is.null(rec) || !is.list(rec)) return(FALSE)
  if (!recover_blank(rec$status) %in% c("failed", "stopped")) return(FALSE)
  if (nzchar(recover_blank(rec$code))) return(FALSE)
  if (nzchar(recover_blank(rec$log))) return(FALSE)
  if (nzchar(recover_blank(rec$run_dir))) return(FALSE)
  if (nzchar(recover_blank(rec$exit_code))) return(FALSE)
  if (length(rec$artifacts %||% list())) return(FALSE)
  TRUE
}

# 执行端上报的集群事实是否可信。
#   两条硬要求：qacct_terminal 必须是显式 false（缺字段不算）；
#   心跳只认服务端任务上的 last_seen，不认客户端 heartbeat。
recover_facts_ok <- function(facts, ex, now = Sys.time()) {
  if (is.null(facts) || !is.list(facts)) return("缺少集群事实")
  jid <- recover_blank(facts$job_id)
  if (!nzchar(jid)) return("缺少作业号")
  if (!identical(jid, recover_blank(ex$job_id))) return("作业号与在管任务不一致")
  st <- tolower(recover_blank(facts$qstat_state))
  if (!st %in% c("r", "running")) return("qstat 未显示运行中")
  # 缺字段、写成字符串、写成 null 都不算 false。
  if (!isFALSE(facts$qacct_terminal)) return("qacct 状态不明或已有终态，不属误报")
  sampled <- recover_blank(facts$sampled_at)
  if (!nzchar(sampled)) return("缺少采样时间")
  ts <- suppressWarnings(as.POSIXct(sampled, format = "%Y-%m-%dT%H:%M:%S"))
  if (is.na(ts)) return("采样时间格式不对")
  age <- as.numeric(difftime(now, ts, units = "secs"))
  if (!is.finite(age) || age < -120 || age > 900) return("集群事实不是近期的")
  # 心跳：只认服务端记录，且必须存在且新鲜。
  ls_raw <- recover_blank(ex$last_seen)
  if (!nzchar(ls_raw)) return("服务端没有任务心跳记录")
  tls <- suppressWarnings(as.POSIXct(ls_raw, format = "%Y-%m-%dT%H:%M:%S"))
  if (is.na(tls)) return("服务端心跳时间格式不对")
  if (abs(as.numeric(difftime(now, tls, units = "secs"))) > 900) return("服务端心跳过旧")
  ""
}

# 纠正资格：全部条件满足才返回 ""，否则返回原因
#   ctx: list(sid, step, req, rec, ex, latest_ex, latest_err, verified_n, verified_err,
#             pending_req, facts, now)
recover_eligibility <- function(ctx, now = Sys.time()) {
  rec <- ctx$rec; ex <- ctx$ex
  if (is.null(rec)) return("找不到这一环的记录")
  if (!identical(recover_blank(rec$session_id), ctx$sid)) return("会话不一致")
  if (!identical(recover_blank(rec$req_id), ctx$req)) return("请求号不一致")
  if (!recover_no_evidence(rec)) return("这条失败带有执行证据，不属误报，走正常判定")

  # 判定依据读不到就拒，不当成"没有"。
  if (isTRUE(ctx$verified_err)) return("无法确认该请求是否已验收")
  if (is.null(ctx$verified_n) || length(ctx$verified_n) != 1L || is.na(ctx$verified_n))
    return("无法确认该请求是否已验收")
  if (as.integer(ctx$verified_n) > 0L) return("该请求已验收")
  if (isTRUE(ctx$latest_err)) return("无法确认这一环最新的执行任务")
  if (is.null(ctx$latest_ex)) return("找不到在管任务")

  if (is.null(ex)) return("找不到在管任务")
  if (!identical(recover_blank(ex$step), ctx$step)) return("任务与环节不一致")
  if (!identical(recover_blank(ex$status), "running")) return("在管任务不是运行中")
  if (!identical(recover_blank(ctx$latest_ex$id), recover_blank(ex$id)))
    return("该环节已有更晚的执行任务，不纠正旧记录")
  if (!identical(recover_blank(ex$req_id), ctx$req)) return("任务与请求号不一致")
  if (!identical(recover_blank(ex$session_id), ctx$sid)) return("任务与会话不一致")
  if (!nzchar(recover_blank(ex$job_id))) return("任务没有作业号")

  # 后继请求：本步已经有新的待回写号，就不能回头改旧的。
  pend <- recover_blank(ctx$pending_req)
  if (nzchar(pend) && !identical(pend, ctx$req)) return("这一环已有后继请求，不纠正旧记录")

  fwhy <- recover_facts_ok(ctx$facts, ex, now)
  if (nzchar(fwhy)) return(fwhy)
  ""
}

# 写纠正投影 + 新终态槽位 + 审计。任何一步失败都不算恢复成功。
recover_apply <- function(app_dir, sid, step, req, rec, ex, facts, state_dir) {
  d <- file.path(attempt_dir(app_dir, sid, req), "results")
  if (!dir.exists(d)) dir.create(d, showWarnings = FALSE, mode = "0700")
  base <- file.path(d, paste0("step--", step))
  term_f <- paste0(base, ".terminal.json")
  rec_f  <- paste0(base, ".recovered.json")

  # 原件必须存在且有可核对的 sha，否则不写恢复。
  if (!file.exists(term_f)) return(list(ok = FALSE, code = "recover_original_missing",
    error = "找不到原始终态文件，不写纠正。"))
  tsha <- recover_file_sha(term_f)
  if (!nzchar(tsha)) return(list(ok = FALSE, code = "recover_original_unhashed",
    error = "原始终态文件算不出 sha256，不写纠正。"))
  if (file.exists(rec_f)) return(list(ok = FALSE, code = "already_recovered",
    error = "这一环已经纠正过，不重复写"))

  started <- recover_blank(ex$created_at)
  if (!nzchar(started)) started <- recover_blank(ex$claimed_at)
  serial <- substr(tsha, 1, 12)
  slot_rel <- paste0("step--", step, ".terminal-r-", serial, ".json")

  val <- list(
    title = as.character(rec$title %||% step),
    status = "running",
    code = NULL, log = "", run_dir = "",
    source_host = "worker:qstat-recovered",
    exec_pc = "",
    imported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    session_id = sid,
    artifacts = list(),
    started_at = started,
    subs = list(),
    req_id = req,
    recovery = list(
      kind = "false_failure_correction",
      supersedes = list(terminal_file = basename(term_f), terminal_sha256 = tsha,
                        terminal_imported_at = recover_blank(rec$imported_at),
                        terminal_status = recover_blank(rec$status)),
      new_terminal_slot = slot_rel,
      exec_id = recover_blank(ex$id),
      job_id = recover_blank(ex$job_id),
      cluster_facts = facts,
      # 集群状态由执行端采样后上报，服务端只核任务绑定与心跳，
      #   不声称自己查过 qstat/qacct。
      facts_source = "qstat/qacct sampled by worker; server verified task binding and heartbeat only",
      rule = "空证据失败投影 + 在管任务仍运行 + qacct 无终态 + 服务端心跳新鲜 → 改回运行中；旧投影保留",
      corrected_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))

  sess_write_json(rec_f, val); Sys.chmod(rec_f, "0600")

  # 审计写失败就不算恢复成功：先回滚刚写的投影。
  aud_ok <- tryCatch({
    f <- file.path(state_dir, "step_recovery.jsonl")
    cat(paste0(jsonlite::toJSON(list(
      at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), kind = "false_failure_correction",
      session_id = sid, step = step, req_id = req,
      exec_id = recover_blank(ex$id), job_id = recover_blank(ex$job_id),
      superseded = val$recovery$supersedes, new_terminal_slot = slot_rel,
      cluster_facts = facts,
      recovered_file = basename(rec_f), old_files_kept = TRUE,
      wrote_db_done_or_verified = FALSE), auto_unbox = TRUE), "\n"),
      file = f, append = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(aud_ok)) {
    tryCatch(unlink(rec_f), error = function(e) NULL)
    return(list(ok = FALSE, code = "recover_audit_failed",
      error = "纠正审计没能写入，按未恢复处理。"))
  }

  # remote_steps 必须真的写进去，否则不算成功。
  # remote_step_put 成功时返回 invisible(TRUE)，不是 list。
  #   之前按 r$ok 判定会抛 "$ operator is invalid for atomic vectors"，
  #   结果把一次已经写好的恢复判成失败 —— 判定写错和真的没写进去是两件事，
  #   必须分开：这里只认"调用没抛异常"为写入成功，写没写进去由随后的读回校验决定。
  put_ok <- FALSE; put_err <- ""
  tryCatch({
    remote_step_put(step, val, sid = sid)
    put_ok <- TRUE
  }, error = function(e) { put_err <<- conditionMessage(e); put_ok <<- FALSE })
  if (!put_ok) {
    tryCatch(unlink(rec_f), error = function(e) NULL)
    return(list(ok = FALSE, code = "recover_projection_failed",
      error = paste0("纠正投影没能写进当前记录：", put_err, "。按未恢复处理。")))
  }

  list(ok = TRUE, recovered_file = rec_f, new_terminal_slot = slot_rel,
       superseded = val$recovery$supersedes)
}

# ---- 读取侧 ----
#   顺序是显式的，不靠 mtime 猜真假：
#   1) 有效恢复记录（sha 对得上）且它声明的新终态槽位已存在 → 用新槽位；
#   2) 有效恢复记录存在 → 用恢复记录本身（运行中）；
#   3) 旧 terminal；
#   4) 普通投影。
recover_read_recovery <- function(base) {
  rec_f <- paste0(base, ".recovered.json")
  if (!file.exists(rec_f)) return(NULL)
  r <- tryCatch(sess_read_json(rec_f), error = function(e) NULL)
  if (is.null(r) || !is.list(r) || is.null(r$recovery)) return(NULL)
  want <- recover_blank(r$recovery$supersedes$terminal_sha256)
  got <- recover_file_sha(paste0(base, ".terminal.json"))
  if (!nzchar(want) || !identical(want, got)) return(NULL)
  r
}

recover_slot_path <- function(base, rec) {
  slot <- recover_blank(rec$recovery$new_terminal_slot)
  if (!nzchar(slot)) return("")
  file.path(dirname(base), slot)
}

# 某一步当前是否处于"已纠正"状态；返回恢复记录或 NULL
recover_active <- function(app_dir, sid, req, step) {
  base <- file.path(attempt_dir(app_dir, sid, req), "results", paste0("step--", step))
  recover_read_recovery(base)
}

# 恢复后写入的新终态槽位（未恢复则为空字符串）
recover_new_slot <- function(app_dir, sid, req, step) {
  base <- file.path(attempt_dir(app_dir, sid, req), "results", paste0("step--", step))
  rec <- recover_read_recovery(base)
  if (is.null(rec)) return("")
  recover_slot_path(base, rec)
}
