# 活跃执行保护：有在管任务时，只有携带该任务完整结果结构的那条回写能改阶段
# ---------------------------------------------------------------------------
# 为什么要这个文件：
#   2026-11-14 19:08 / 19:11，figs 正在集群上跑（SGE 60773，qstat 为 r），
#   却先后被写进 running 与 failed。写回体里没有退出码、没有产物、
#   脚本指纹为空，started_at 还被重设。作业本身没失败，页面却显示失败。
#
# 为什么"带了 exec_id"不算证明：
#   /api/flow 就能看到任务 id。任何能读页面的客户端都能照抄一个。
#   绑定只是第一道；过了绑定还要核这条回写指向的是不是同一个作业：
#   派工目录、脚本、退出码、日志，四项都要能对上。
#
# 为什么读不到执行任务表必须 fail-closed：
#   把"读不到"当成"没有在管任务"，防线会在故障重演时自己先倒下。
#
# 为什么不一刀切要求 qacct：
#   执行端在准备阶段（提交前）失败时还没有作业号，拿不到 qacct，
#   但它会带明确的准备阶段错误。这条通路必须留着。

EXEC_GUARD_ERROR <- "exec_guard_unavailable"

# 读执行任务表。
#   NULL = 读不到（文件损坏、权限、解析失败）；
#   list() = 读到了，确实没有任务。
import_exec_items <- function() {
  tryCatch({
    d <- exec_read()
    if (is.null(d) || !is.list(d) || is.null(d$items)) return(NULL)
    if (!is.list(d$items)) return(NULL)
    d$items
  }, error = function(e) NULL)
}

txt <- function(x) {
  v <- as.character(x %||% "")
  if (length(v) != 1L || is.na(v)) "" else trimws(v)
}

norm_path <- function(x) {
  p <- txt(x)
  if (!nzchar(p)) return("")
  p <- sub("/+$", "", p)
  tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE), error = function(e) p)
}

# 本环节在管的执行任务（排队中或正在跑）
import_active_items <- function(step, session_id = "", req_id = "", items = NULL) {
  stp <- txt(step)
  if (!nzchar(stp)) return(list())
  if (!length(items)) return(list())
  sid <- txt(session_id)
  rq  <- txt(req_id)
  Filter(function(it) {
    if (!identical(as.character(it$step %||% ""), stp)) return(FALSE)
    if (!as.character(it$status %||% "") %in% c("queued", "running")) return(FALSE)
    ok_s <- !nzchar(sid) || identical(as.character(it$session_id %||% ""), sid)
    ok_r <- !nzchar(rq)  || identical(as.character(it$req_id %||% ""), rq)
    ok_s && ok_r
  }, items)
}

# 回写自报的 exec_id 是否就是这一环在管的那个任务
import_bound_exec <- function(b, items = NULL) {
  exid <- txt(b$exec_id)
  if (!nzchar(exid)) return(NULL)
  act <- import_active_items(b$step, b$session_id, b$req_id, items)
  hit <- Filter(function(it) identical(as.character(it$id %||% ""), exid), act)
  if (!length(hit)) return(NULL)
  hit[[1]]
}

# 派工目录必须与本次作业的工作根严格相等。
#   可接受的只有两个确切来源：该记录的会话根目录、执行任务上记录的 run_dir。
#   outdir 若是工作根之下的子目录，服务端归一到工作根，不能当独立祖先或子目录用。
#   为什么不能用 startsWith 双向匹配：/share/home 是 /share/home/x 的祖先，
#   随手写一个上级目录就能通过，等于没核对。
import_dir_ok <- function(b, ex0, sess_dir = "") {
  rd <- norm_path(b$run_dir)
  if (!nzchar(rd)) return(FALSE)
  roots <- c(norm_path(sess_dir), norm_path(ex0$run_dir))
  roots <- unique(roots[nzchar(roots)])
  if (!length(roots)) return(FALSE)
  rd %in% roots
}

# 脚本绑定：核完整路径，不比 basename。
#   为什么不能用 basename：错误目录下放一个同名脚本就能过。
#   客户端没带脚本时，以服务端派工事实为准（任务上有 script 记录）；
#   两边都没有脚本事实就不放行，不能靠恒真表达式兜底。
import_script_ok <- function(b, ex0) {
  want <- txt(ex0$script)
  if (!nzchar(want)) want <- txt(ex0$script_baseline$script_path)
  if (!nzchar(want)) return(FALSE)
  got <- txt(b$script_path)
  if (!nzchar(got)) got <- txt(b$script)
  if (!nzchar(got)) return(TRUE)
  # 自动物种识别派工的 script 是 -e，回写则记录实际目标脚本。
  # 与正式派工核验共用解析器；必须匹配服务端派工目标和包装代码摘要。
  # 解析器不可用或派工参数不完整时拒收，不用 basename 或猜测路径。
  if (identical(want, "-e")) {
    if (!exists("dc_wrapper_expect", mode = "function")) return(FALSE)
    wx <- tryCatch(dc_wrapper_expect(ex0), error = function(e) NULL)
    if (is.null(wx)) return(FALSE)
    digest <- tolower(txt(b$script_wrapper_md5))
    return(nzchar(txt(wx$target)) &&
      identical(norm_path(got), norm_path(wx$target)) &&
      grepl("^[0-9a-f]{32}$", digest) &&
      identical(digest, tolower(txt(wx$wrapper_md5))))
  }
  identical(norm_path(got), norm_path(want))
}

# 一条失败回写有没有真正的失败证据
#   要同时满足：退出码语义自洽、具体错误、派工目录对得上、脚本对得上。
#   尤其要拒绝"退出码 0 却说失败"这种自相矛盾体。
import_fail_evidence <- function(b, ex0, sess_dir = "") {
  ec_raw <- txt(b$exit_code)
  if (!nzchar(ec_raw)) ec_raw <- txt(b$code)
  ec <- suppressWarnings(as.numeric(ec_raw)[1])
  # 明确给了 0，或明确写着成功，都不能是失败。
  if (!is.na(ec) && ec == 0) return(FALSE)
  code_txt <- txt(b$code)
  if (nzchar(code_txt) && grepl("^(0|ok|done|success)$", tolower(code_txt))) return(FALSE)
  nonzero <- !is.na(ec) && ec != 0
  phase <- tolower(txt(b$phase))
  phase_ok <- phase %in% c("prepare", "precheck", "preflight", "submit", "dispatch", "upload")
  # 准备阶段错误必须有明确错误类型，不能只填一个 phase 名字。
  phase_code_ok <- phase_ok && nzchar(code_txt) &&
    !grepl("^(0|ok|done|success|failed)$", tolower(code_txt))
  lg <- txt(b$log)
  log_ok <- nchar(lg) >= 12L && !lg %in% c("failed", "error", "失败", "报错")
  (nonzero || phase_code_ok) && log_ok &&
    import_dir_ok(b, ex0, sess_dir) && import_script_ok(b, ex0)
}

# 一条成功回写有没有起码的结果结构（正式验收仍由判定器做）
import_done_evidence <- function(b, ex0, sess_dir = "") {
  arts <- b$artifacts %||% list()
  if (!length(arts)) return(FALSE)
  has_art <- any(vapply(arts, function(a) {
    is.list(a) && nzchar(txt(a$rel %||% a$path %||% a$name))
  }, TRUE))
  has_art &&
    import_dir_ok(b, ex0, sess_dir) &&
    import_script_ok(b, ex0)
}

# 返回值：
#   NULL  —— 放行
#   list  —— 拒收（直接作为回执体）
import_active_exec_guard <- function(b, items = NULL, sess_dir = "") {
  stp <- txt(b$step)
  stv <- txt(b$status)
  if (!nzchar(stp)) return(NULL)
  if (!stv %in% c("running", "done", "failed", "stopped")) return(NULL)
  if (is.null(items)) items <- import_exec_items()
  if (is.null(items)) {
    return(list(ok = FALSE, http = 503, code = EXEC_GUARD_ERROR, step = stp, status = stv,
      wrote_state = FALSE, wrote_started_at = FALSE,
      error = "读不到执行任务表，无法判断这一环是否已有执行端在管。按拒收处理，状态与开始时间都不改。"))
  }
  act <- import_active_items(stp, b$session_id, b$req_id, items)
  if (!length(act)) return(NULL)

  ids <- vapply(act, function(it) as.character(it$id %||% ""), "")
  ex0 <- import_bound_exec(b, items)

  reject <- function(code, why) {
    list(ok = FALSE, http = 409, code = code, step = stp, status = stv,
         active_exec = as.list(ids),
         active_status = as.list(vapply(act, function(it) as.character(it$status %||% ""), "")),
         wrote_state = FALSE, wrote_started_at = FALSE, error = why)
  }

  if (is.null(ex0)) {
    return(reject("exec_active_conflict", paste0(
      "「", stp, "」这一环还有执行端的任务在管（", paste(ids, collapse = ", "),
      "）。这条回写既不是从那个任务发出，也不含它的结果结构，",
      "状态与开始时间都不改。计算仍在进行时，阶段状态由服务端按任务事实给出。")))
  }

  if (identical(stv, "failed") || identical(stv, "stopped")) {
    if (!import_fail_evidence(b, ex0, sess_dir)) {
      return(reject("exec_failure_evidence_missing", paste0(
        "「", stp, "」的失败回写缺少可核对的结果：需要真实非零退出码或明确的准备阶段错误码、",
        "具体日志，以及与该任务一致的派工目录与脚本。",
        "退出码为 0 却申报失败属于自相矛盾，同样拒收。",
        "状态与开始时间都不改。")))
    }
  } else if (identical(stv, "done")) {
    if (!import_done_evidence(b, ex0, sess_dir)) {
      return(reject("exec_result_structure_missing", paste0(
        "「", stp, "」的完成回写缺少结果结构：需要与该任务一致的派工目录和带名字的产物清单。",
        "在管任务仍按运行中处理，状态与开始时间都不改；产物齐备后再按原流程申报验收。")))
    }
  }
  NULL
}

# started_at：只要有在管任务，时间一律取服务端任务的既定来源，不采信回写自报。
#   why：19:08 那次写的是 running，状态没被改写，但 started_at 被重设成 19:08，
#   页面上的耗时从此算错。时间属于任务事实，不属于回写方的自述。
import_started_at_guard <- function(b, items = NULL) {
  stp <- txt(b$step)
  if (!nzchar(stp)) return(b)
  if (is.null(items)) items <- import_exec_items()
  if (is.null(items)) return(b)
  act <- import_active_items(stp, b$session_id, b$req_id, items)
  if (!length(act)) return(b)
  client_ts <- txt(b$started_at)
  ex0 <- import_bound_exec(b, items)
  srv_ts <- ""
  if (!is.null(ex0)) {
    srv_ts <- txt(ex0$created_at)
    if (!nzchar(srv_ts)) srv_ts <- txt(ex0$claimed_at)
  }
  if (!nzchar(srv_ts)) {
    srv_ts <- txt(act[[1]]$created_at)
    if (!nzchar(srv_ts)) srv_ts <- txt(act[[1]]$claimed_at)
  }
  if (nzchar(client_ts) && !identical(client_ts, srv_ts))
    b$client_started_at_ignored <- client_ts
  if (nzchar(srv_ts)) b$started_at <- srv_ts
  b
}

# 拒收留痕：只写一行诊断，不改任何状态。
import_guard_audit <- function(state_dir, b, g) {
  tryCatch({
    f <- file.path(state_dir, "import_guard.jsonl")
    line <- jsonlite::toJSON(list(
      at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), kind = as.character(g$code %||% "guard"),
      step = as.character(b$step %||% ""), status = as.character(b$status %||% ""),
      req_id = as.character(b$req_id %||% b$req_id_carried %||% ""),
      session_id = as.character(b$session_id %||% ""),
      exec_id = as.character(b$exec_id %||% ""),
      active_exec = g$active_exec, active_status = g$active_status,
      had_run_dir = nzchar(txt(b$run_dir)),
      had_exit = nzchar(txt(b$exit_code %||% b$code)),
      wrote_state = FALSE, wrote_started_at = FALSE,
      why = "回写未通过绑定与结果结构核对"), auto_unbox = TRUE)
    cat(paste0(line, "\n"), file = f, append = TRUE)
  }, error = function(e) NULL)
  invisible(TRUE)
}

# ---- 过时通知 ----
#   为什么必须在派工前判：注入通道是另起一个会话去"启动计算"，
#   已有执行端在管时再派一次，等于同一件事两个执行者。
#   会话与记录号不能混用：注入里 analysis_id 是记录号，session 是聊天会话。
inject_stale_reason <- function(it, exec_items = NULL, verified_count = NULL) {
  stp <- txt(it$step)
  if (!nzchar(stp)) return("")
  sid <- txt(it$analysis_id)
  rq  <- txt(it$req_id)

  if (is.null(exec_items)) {
    exec_items <- tryCatch(import_exec_items(), error = function(e) NULL)
  }
  if (is.null(exec_items)) return("")   # 读不到就不擅自关，交给下一轮
  act <- import_active_items(stp, sid, rq, exec_items)
  if (length(act)) {
    return(paste0("同环节已有执行任务在管：",
                  as.character(act[[1]]$id %||% ""), "（",
                  as.character(act[[1]]$status %||% ""), "），不再另派"))
  }

  if (is.null(verified_count) && nzchar(rq)) {
    verified_count <- tryCatch({
      if (!isTRUE(DB_READY)) 0L else {
        n <- db_scalar(DB_CON,
          "SELECT COUNT(*) FROM step_attempts WHERE req_id = ? AND step_id = ? AND status = 'verified'",
          list(rq, stp))
        if (length(n) == 1L && !is.na(n)) as.integer(n) else 0L
      }
    }, error = function(e) 0L)
  }
  if (isTRUE(as.integer(verified_count %||% 0L) > 0L))
    return("该请求已有 verified 记录，这条通知已过时")
  ""
}

# 到点通知软关闭：已有执行端在管或已验收的，不再派工，只把通知关掉并留原因。
inject_stale_close <- function(due) {
  if (!length(due)) return(list(kept = list(), closed = list(), closed_n = 0L))
  ids <- vapply(due, function(x) as.character(x$id %||% ""), "")
  d <- tryCatch(inject_read(), error = function(e) NULL)
  if (is.null(d)) return(list(kept = due, closed = list(), closed_n = 0L))
  ex <- tryCatch(import_exec_items(), error = function(e) NULL)
  closed <- list()
  for (i in seq_along(d$items)) {
    it <- d$items[[i]]
    if (!as.character(it$id %||% "") %in% ids) next
    if (!is.null(it$satisfied_at) || !is.null(it$dropped_at)) next
    why <- tryCatch(inject_stale_reason(it, exec_items = ex, verified_count = NULL),
                    error = function(e) "")
    if (!nzchar(why)) next
    d$items[[i]]$dropped_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    d$items[[i]]$drop_reason <- why
    d$items[[i]]$next_attempt_at <- NULL
    closed[[length(closed) + 1L]] <- list(
      id = as.character(it$id %||% ""), step = as.character(it$step %||% ""),
      req_id = as.character(it$req_id %||% ""), reason = why)
  }
  if (length(closed)) {
    tryCatch(inject_write(d), error = function(e) NULL)
    tryCatch({
      f <- file.path(STATE_DIR, "inject_stale_close.jsonl")
      for (cc in closed) {
        cat(paste0(jsonlite::toJSON(list(
          at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), kind = "stale_inject_closed",
          id = cc$id, step = cc$step, req_id = cc$req_id, reason = cc$reason,
          dispatched = FALSE, wrote_step_state = FALSE), auto_unbox = TRUE), "\n"),
          file = f, append = TRUE)
      }
    }, error = function(e) NULL)
  }
  cids <- vapply(closed, function(x) as.character(x$id), "")
  list(kept = Filter(function(x) !as.character(x$id %||% "") %in% cids, due),
       closed = closed, closed_n = length(closed))
}

# 最后一道：claim 前重查。
#   due 是一次读，worker 可能在读与 claim 之间把任务接走；
#   runner 若不重查就直接启动会话，照样是第二个执行者。
#   为什么返结构而不是空字符串：生产 runner 只对非 2xx 停下。
#   如果这里把"读不到执行任务表"或"守卫自己出错"折成空字符串，
#   claim 会返回 200，runner 照样另起 runtime —— 防线等于没接。
#   所以：过时 → stale；读不到/出错 → unavailable；两者都不派工。
inject_claim_check <- function(it, exec_items = NULL, verified_count = NULL) {
  ex <- tryCatch(import_exec_items(), error = function(e) NULL)
  if (is.null(ex) && is.null(exec_items)) {
    return(list(ok = FALSE, http = 503, code = "inject_guard_unavailable",
      reason = "读不到执行任务表，无法确认这一步是否已有执行端在管；不派工。"))
  }
  why <- tryCatch(
    inject_stale_reason(it, exec_items = if (is.null(exec_items)) ex else exec_items,
                        verified_count = verified_count),
    error = function(e) NULL)
  if (is.null(why)) {
    return(list(ok = FALSE, http = 503, code = "inject_guard_unavailable",
      reason = "过时判定出错，无法确认这一步是否已有执行端在管；不派工。"))
  }
  if (nzchar(why)) return(list(ok = FALSE, http = 409, code = "inject_stale_at_claim", reason = why))
  list(ok = TRUE, http = 200L, code = "ok", reason = "")
}
