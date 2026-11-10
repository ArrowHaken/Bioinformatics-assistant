#!/usr/bin/env Rscript
# =====================================================================
#  runs.R -- 「现在有几条分析在跑，各自跑到哪」
#
#  为什么需要这一层
#    原来的结构是一条记录一个 current 指针，页面只画 current 那一条。
#    数据层其实已经按 session 隔离了（remote_steps / flow_progress 都能按 sid 过滤），
#    所以并行不是要重做存储，而是补一个「一眼看全部」的视图。
#
#  为什么放独立文件
#    serve.R 已有七千行。再加一套「读进度 + 算状态 + 排顺序」的规则，
#    下一个读的人分不清哪些是页面收发、哪些是判定。
#    这里只做一件事：把一个 sid 的进度聚成一行摘要。
#
#  一条铁律（沿用已有约定，不在这里破坏它）
#    只报已经发生的事。没回写就是 unknown，不按「上一步完成了」
#    推测「这一步大概在跑」。推测出来的 running 比空白更糟：
#    人看到绿灯就不会去查，而进程可能早就没了。
# =====================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# 心跳三态的时间界。写在常量里而不是散在判断处，
#   是因为这三档的解释文案必须与阈值一起看才说得通。
RUN_BEAT_FRESH_SEC   <- 90L     # 显式运行记录在 90 秒内有心跳才算活跃
RUN_BEAT_STALE_SEC   <- 1800L   # 内部兼容分档；无回写不构成中断证据

runs_now <- function() Sys.time()

# 把 "%Y-%m-%dT%H:%M:%S" 解析成 POSIXct；解析不了就 NA。
#   不用 as.POSIXct 默认格式：它会把 "2026-11-09" 也当合法，
#   于是一个残缺的日期会变成一个「很久以前」的时间点，判成 lost。
runs_parse_time <- function(x) {
  missing <- as.POSIXct(NA)
  if (is.null(x) || length(x) != 1L || is.na(x)) return(missing)
  s <- as.character(x)
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}$", s)) return(missing)
  t <- tryCatch(as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%S", tz = ""),
                error = function(e) missing)
  if (is.na(t) || format(t, "%Y-%m-%dT%H:%M:%S") != s) return(missing)
  t
}

# 由最后一条回写时间算三态。now 传入是为了可测：固定 now 才能断言。
runs_liveness <- function(last_at, now = runs_now()) {
  t <- runs_parse_time(last_at)
  if (is.na(t)) return(list(state = "unknown", age_sec = NA_integer_, last_at = NULL))
  age <- as.numeric(difftime(now, t, units = "secs"))
  if (!is.finite(age) || age < 0) return(list(state = "unknown", age_sec = NA_integer_, last_at = NULL))
  state <- if (age <= RUN_BEAT_FRESH_SEC) "running"
           else if (age <= RUN_BEAT_STALE_SEC) "stalled"
           else "lost"
  list(state = state, age_sec = as.integer(round(age)),
       last_at = format(t, "%Y-%m-%dT%H:%M:%S"))
}

# ---- 单条摘要 -------------------------------------------------------

# 入参尽量做成「纯数据进、纯数据出」：
#   steps   已经按 sid 过滤过的 remote_steps 列表
#   flow    已经按 sid 过滤过的 flow_progress 列表
#   order   流程顺序（flow_spec 的 FLOW_ORDER）
# 这样这个函数不依赖 serve.R 里的全局变量，能被单独调用和断言。
runs_summarize <- function(sess, steps, flow, order, now = runs_now()) {
  sid <- as.character(sess$id %||% "")
  title <- as.character(sess$name %||% sid)

  order <- unique(as.character(order %||% character()))
  by_id <- list()
  flow_id <- list()
  for (s in steps %||% list()) {
    id <- as.character(s$id %||% "")
    if (length(id) == 1L && !is.na(id) && nzchar(id)) by_id[[id]] <- s
  }
  for (s in flow %||% list()) {
    id <- as.character(s$stage %||% s$id %||% "")
    if (length(id) == 1L && !is.na(id) && nzchar(id)) flow_id[[id]] <- s
  }
  status <- function(r) tolower(as.character(r$status %||% "unknown"))
  success <- c("done", "ok", "completed")
  failure <- c("failed", "error", "stopped")
  active <- c("running", "in_progress")
  # Legacy display age remains compatible; contract liveness uses heartbeat_at only.
  beat <- function(records) {
    times <- unlist(lapply(records, function(r) lapply(
      c("heartbeat_at", "updated_at", "ended_at", "started_at", "imported_at"),
      function(k) as.numeric(runs_parse_time(r[[k]])))))
    times <- times[is.finite(times) & times <= as.numeric(now)]
    if (!length(times)) return(runs_liveness(NULL, now))
    runs_liveness(format(as.POSIXct(max(times), origin = "1970-01-01"),
                         "%Y-%m-%dT%H:%M:%S"), now)
  }
  current_stage <- NULL
  current_title <- NULL
  current_subs <- list()
  done_n <- 0L
  fail_n <- 0L
  observed <- FALSE
  live <- runs_liveness(NULL, now)
  latest <- runs_liveness(NULL, now)
  heartbeat <- runs_liveness(NULL, now)
  for (id in order) {
    r <- by_id[[id]]
    g <- flow_id[[id]]
    records <- Filter(Negate(is.null), list(r, g))
    subs <- c(r$subs %||% list(), g$subs %||% list())
    states <- vapply(records, status, "")
    sub_states <- vapply(subs, status, "")
    failed <- any(c(states, sub_states) %in% failure)
    done <- !failed && any(states %in% success)
    if (failed) fail_n <- fail_n + 1L
    if (done) done_n <- done_n + 1L
    observed <- observed || length(records) > 0L
    written <- beat(c(records, subs))
    if (!is.na(written$age_sec) &&
        (is.na(latest$age_sec) || written$age_sec < latest$age_sec)) latest <- written
    if (failed || done) next
    executing <- c(records[states %in% active], subs[sub_states %in% active])
    if (!length(executing)) next
    for (record in executing) {
      candidate <- runs_liveness(record$heartbeat_at, now)
      if (!is.na(candidate$age_sec) &&
          (is.na(heartbeat$age_sec) || candidate$age_sec < heartbeat$age_sec)) heartbeat <- candidate
    }
    stage_live <- beat(executing)
    # Prefer a verified live stage, then the freshest stale execution record.
    if (is.null(current_stage) || (!is.na(stage_live$age_sec) &&
        (is.na(live$age_sec) || stage_live$age_sec < live$age_sec))) {
      current_stage <- id
      current_title <- as.character(r$title %||% g$title %||% id)
      current_subs <- if (length(g$subs)) g$subs else r$subs %||% list()
      live <- stage_live
    }
  }
  state <- if (fail_n > 0L) "failed"
           else if (length(order) > 0L && done_n == length(order)) "completed"
           else if (!is.null(current_stage)) live$state
           else if (observed) "idle" else "unknown"
  if (is.null(current_stage)) live <- latest

  sub_line <- ""
  if (length(current_subs)) {
    part <- lapply(current_subs, function(x) {
      list(id = as.character(x$id %||% ""),
           title = as.character(x$title %||% ""),
           status = as.character(x$status %||% "unknown"))
    })
    pos <- which(vapply(part, function(x) x$status %in% c("running", "in_progress"), TRUE))
    if (length(pos)) {
      sub_line <- sprintf("%s（%d/%d）", part[[pos[1]]]$title, pos[1], length(part))
    } else {
      dn <- sum(vapply(part, function(x) x$status %in% c("done", "ok", "completed"), TRUE))
      sub_line <- sprintf("已完成 %d/%d 个环节", dn, length(part))
    }
  }

  timing <- runs_timing(c(steps %||% list(), flow %||% list()), now)
  list(
    elapsed_seconds = timing$elapsed_seconds,
    eta_seconds = timing$eta_seconds,
    session_id = sid,
    name = title,
    mode = as.character(sess$mode %||% ""),
    run_dir = as.character(sess$run_dir %||% ""),
    state = state,
    heartbeat_at = heartbeat$last_at,
    age_sec = live$age_sec,
    last_at = live$last_at,
    current_stage = current_stage,
    current_title = current_title,
    current_sub = if (nzchar(sub_line)) sub_line else NULL,
    stage_index = if (!is.null(current_stage)) match(current_stage, order) else NULL,
    done_steps = done_n,
    failed_steps = fail_n,
    total_steps = length(order %||% character(0)))
}

# Only exact manifest pointers authorize a local lifecycle read. NULL preserves
# legacy summaries when no binding exists; an invalid bound record is unknown.
runs_local_lifecycle <- function(app_dir, sess, now = runs_now()) {
  scalar <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  id_ok <- function(x) scalar(x) && grepl("^[A-Za-z0-9][A-Za-z0-9_-]{0,127}$", x)
  safe_path <- function(path, directory = FALSE) {
    if (!scalar(path) || !file.exists(path)) stop("missing path")
    # Check every component before normalization can erase symlink provenance.
    p <- path
    repeat {
      link <- Sys.readlink(p)
      if (!is.na(link) && nzchar(link)) stop("symlink path")
      parent <- dirname(p)
      if (identical(parent, p) || identical(p, ".")) break
      p <- parent
    }
    normalized <- normalizePath(path, winslash = "/", mustWork = TRUE)
    if (directory != dir.exists(normalized)) stop("wrong path kind")
    normalized
  }
  json <- function(path) {
    path <- safe_path(path)
    x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    if (!is.list(x)) stop("invalid manifest")
    x
  }
  unknown <- list(state = "unknown", age_sec = NA_integer_, last_at = NULL, heartbeat_at = NULL,
    full_analysis_replay_verified = FALSE, scientific_validity_verified = FALSE,
    notebook_export_state = "unknown")
  if (!identical(sess$mode, "local") || !scalar(sess$run_dir) ||
      !dir.exists(sess$run_dir)) return(NULL)
  tryCatch({
    rd <- safe_path(sess$run_dir, TRUE)
    binding_path <- file.path(rd, "status", "notebook_session.json")
    if (!file.exists(binding_path)) return(NULL)
    root <- safe_path(file.path(rd, "status"), TRUE)
    binding <- json(binding_path)
    if (!identical(binding$session_id, sess$id) ||
        !identical(binding$app_dir, normalizePath(app_dir, winslash = "/", mustWork = TRUE)) ||
        !identical(binding$run_dir, rd) || !id_ok(binding$binding_id)) stop("binding mismatch")
    manifest <- json(file.path(root, "notebook_manifest.json"))
    if (!identical(manifest$binding_id, binding$binding_id)) stop("manifest binding mismatch")
    attempt <- manifest$active_attempt
    if (is.null(attempt)) {
      phases <- manifest$phases
      if (!is.list(phases) || !length(phases)) stop("no explicit attempt")
      valid <- vapply(phases, function(p) is.list(p) && id_ok(p$attempt_id) &&
        scalar(p$step) && scalar(p$state), TRUE)
      if (!all(valid)) stop("invalid phase")
      stale <- vapply(phases, function(p) identical(p$state, "stale"), TRUE)
      # Earlier retries invalidate a suffix, not the earlier retry itself.
      if (any(stale) && any(!stale[seq.int(which(stale)[1L], length(stale))]))
        stop("ambiguous stale phases")
      phases <- phases[!stale]
      if (!length(phases)) stop("only stale phases")
      steps <- vapply(phases, function(p) p$step, "")
      ids <- vapply(phases, function(p) p$attempt_id, "")
      groups <- rle(ids)$values
      if (anyDuplicated(steps) || anyDuplicated(groups)) stop("ambiguous phase order")
      attempt <- tail(ids, 1L)
    }
    if (!id_ok(attempt)) stop("invalid active attempt")
    identity <- json(file.path(root, "notebook_attempts", attempt, "status", "attempt.json"))
    if (!identical(identity$attempt_id, attempt) ||
        !identical(identity$binding_id, binding$binding_id)) stop("attempt binding mismatch")
    safe_path(file.path(root, paste0("lifecycle-", attempt, ".rds")))
    env <- environment(runs_local_lifecycle)
    reader <- get0("rl_read", envir = env, mode = "function", inherits = TRUE)
    if (is.null(reader)) {
      module <- new.env(parent = baseenv())
      sys.source(file.path(app_dir, "engine", "run_lifecycle.R"), envir = module)
      reader <- module$rl_read
    }
    state <- reader(root, attempt, now = now, stale_after = RUN_BEAT_FRESH_SEC)
    if (!is.list(state) || !identical(state$run_id, attempt) ||
        !identical(state$attempt_id, attempt)) stop("lifecycle identity mismatch")
    observed <- state$observed_status
    if (!scalar(observed) || !observed %in% c("queued", "running", "stalled", "unknown",
        "succeeded", "failed", "cancelled")) stop("invalid lifecycle state")
    at <- if (observed %in% c("running", "stalled")) state$heartbeat_at else state$updated_at
    age <- if (is.numeric(at) && length(at) == 1L && is.finite(at)) as.numeric(now) - at else NA_real_
    if (!is.finite(age) || age < 0) stop("invalid lifecycle timestamp")
    # A generation receipt is not a live download check. Never load notebook bytes here.
    export <- tryCatch({
      phases <- manifest$phases
      if (!identical(observed, "succeeded") || !is.null(manifest$active_attempt) ||
          !is.list(phases) || !length(phases) || !is.null(names(phases)) ||
          !all(vapply(phases, function(p) is.list(p) && id_ok(p$attempt_id) &&
            scalar(p$step) && identical(p$state, "succeeded"), TRUE)))
        stop("export not terminal")
      receipt <- json(file.path(root, "notebook_auto_export.json"))
      fields <- c("schema", "status", "code", "attempt_id", "binding_id",
        "full_analysis_replay_verified")
      if (anyDuplicated(names(receipt)) || !setequal(names(receipt), fields) ||
          !identical(receipt$schema, "pa.notebook.auto-export.v1") ||
          !identical(receipt$attempt_id, attempt) ||
          !identical(receipt$binding_id, binding$binding_id) ||
          !identical(receipt$full_analysis_replay_verified, FALSE) ||
          !scalar(receipt$status) || !scalar(receipt$code)) stop("invalid export receipt")
      failed_codes <- c("unsafe_path", "missing_evidence", "invalid_evidence",
        "invalid_attempt", "foreign_engine", "foreign_binding", "foreign_session",
        "incomplete_attempt", "invalid_manifest", "export_failed", "status_write_failed")
      kind <- if (identical(receipt$status, "succeeded") && identical(receipt$code, "exported")) "ready"
        else if (identical(receipt$status, "skipped") && identical(receipt$code, "missing_manifest")) "not_configured"
        else if (identical(receipt$status, "failed") && receipt$code %in% failed_codes) "failed"
        else stop("invalid export combination")
      list(notebook_export_state = kind, notebook_export_code = receipt$code)
    }, error = function(e) list(notebook_export_state = "unknown"))
    c(list(state = if (observed == "succeeded") "execution_completed" else observed,
      heartbeat_at = if (is.numeric(state$heartbeat_at) && length(state$heartbeat_at) == 1L &&
        is.finite(state$heartbeat_at)) format(as.POSIXct(state$heartbeat_at, origin = "1970-01-01"),
          "%Y-%m-%dT%H:%M:%S") else NULL,
      age_sec = as.integer(min(age, .Machine$integer.max)),
      last_at = format(as.POSIXct(at, origin = "1970-01-01"), "%Y-%m-%dT%H:%M:%S"),
      attempt_id = attempt, lifecycle_status = state$status,
      full_analysis_replay_verified = FALSE, scientific_validity_verified = FALSE), export)
  }, error = function(e) unknown)
}

# ---- 全部摘要 -------------------------------------------------------

# 只回属于这个 owner 的记录。共享记录标 readonly，不给跑按钮。
runs_list <- function(app_dir, owner_key, sessions, readonly = character(0),
                      order = character(0), now = runs_now()) {
  out <- list()
  for (s in sessions %||% list()) {
    sid <- as.character(s$id %||% "")
    if (!nzchar(sid)) next
    steps <- tryCatch(remote_steps_read(sid)$steps, error = function(e) list())
    flow <- tryCatch(flow_prog_read(sid)$stages, error = function(e) list())
    row <- tryCatch(runs_summarize(s, steps, flow, order, now),
                    error = function(e) NULL)
    if (is.null(row)) next
    local <- runs_local_lifecycle(app_dir, s, now)
    if (!is.null(local)) {
      for (key in names(local)) row[key] <- local[key]
      row["elapsed_seconds"] <- list(NULL)
      row["eta_seconds"] <- list(NULL)
      row$current_stage <- NULL
      row$current_title <- NULL
      row$current_sub <- NULL
      row$done_steps <- 0L
      row$failed_steps <- if (identical(local$state, "failed")) 1L else 0L
    }
    row <- runs_contract(row, s, now)
    row$readonly <- sid %in% as.character(readonly)
    out[[length(out) + 1L]] <- row
  }
  out
}

# 给状态一句人话。三档文案分开写，是因为处置方式不同：
#   running 不用管；stalled 要去看最后一条；lost 要先确认进程再重跑。
runs_state_sentence <- function(row) {
  st <- as.character(row$state %||% "unknown")
  age <- row$age_sec
  mins <- if (is.null(age) || is.na(age)) NA_integer_ else as.integer(age %/% 60)
  if (identical(row$terminal_evidence, "human_confirmed") && st == "completed")
    return("已结束（人工确认）；无后续回写正常，不代表科学有效性已验证")
  switch(st,
    running = if (!is.null(row$current_title) && nzchar(row$current_title))
                paste0("正在跑 ", row$current_title) else "已确认执行中，心跳有效",
    stalled = "状态待核实；缺少近期执行证据，不代表中断",
    lost = "状态待核实；缺少近期执行证据，不代表中断",
    unverified = "状态待核实；缺少近期执行证据，不代表中断",
    execution_completed = {
      export_state <- row$notebook_export_state
      if (!is.character(export_state) || length(export_state) != 1L || is.na(export_state))
        export_state <- "unknown"
      detail <- switch(export_state,
        ready = "授权清单文件已生成；已生成清单 notebook，下载时重新校验（仅生成记录，非实时可下载确认）",
        not_configured = "未配置清单",
        failed = "清单导出失败",
        "清单生成状态尚未确认")
      paste0("已完成计算；", detail, "；全流程重放和科学有效性均未验证")
    },
    queued = "本次执行已排队，尚未确认开始运行",
    cancelled = "本次执行已取消",
    completed = "全部预期阶段已完成",
    failed = "已有阶段或环节失败，请查看执行记录",
    idle = "暂无已确认的运行；部分阶段可能已完成或等待执行",
    unknown = "无法确认运行状态，缺少有效执行心跳或回写",
    "无法确认运行状态")
}
# Stable-session human confirmation is explicit evidence, not a title heuristic.
runs_confirmed_terminal <- function(sess) {
  x <- sess$human_confirmed_terminal
  scalar <- function(v) is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v)
  if (!is.list(x) || !identical(x$session_id, sess$id) ||
      !scalar(x$lifecycle) || !x$lifecycle %in% c("completed", "failed", "cancelled") ||
      !scalar(x$confirmed_by) || !scalar(x$reason) || !scalar(x$confirmed_at) || is.na(runs_parse_time(x$confirmed_at))) return(NULL)
  x
}

runs_timing <- function(records, now) {
  # Explicit started_at only. Heartbeats and imports are never execution starts.
  starts <- vapply(records, function(r) as.numeric(runs_parse_time(r$started_at)), 0)
  starts <- starts[is.finite(starts) & starts <= as.numeric(now)]
  # ETA accepted only when the producer provides both value and provenance.
  eta <- NULL
  for (r in records) {
    if ((r$status %||% "") %in% c("running", "in_progress")) {
      v <- r$eta_seconds
      if (is.numeric(v) && length(v) == 1L && is.finite(v) && v >= 0 &&
          is.character(r$eta_basis) && length(r$eta_basis) == 1L &&
          r$eta_basis %in% c("producer_estimate", "measured_rate")) eta <- v
    }
  }
  list(elapsed_seconds = if (length(starts)) max(0, as.numeric(now) - min(starts)) else NULL,
       eta_seconds = eta)
}

runs_contract <- function(row, sess, now = runs_now()) {
  confirmation <- runs_confirmed_terminal(sess)
  if (!is.null(confirmation)) {
    row$state <- confirmation$lifecycle
    row$terminal_evidence <- "human_confirmed"
    row$human_confirmed_terminal <- confirmation
  }
  row$lifecycle <- if (row$state %in% c("stalled", "lost")) "unverified" else row$state
  heartbeat <- runs_liveness(row$heartbeat_at, now)
  row$is_actively_running <- identical(row$lifecycle, "running") &&
    identical(heartbeat$state, "running")
  row$stage_total <- if (row$total_steps > 0L) row$total_steps else NULL
  # Current stage is a known ordered stage; completed stages form the lower bound.
  row$stage_index <- if (!is.null(row$current_stage) && row$total_steps > 0L)
    row$stage_index %||% NULL else NULL
  row$progress_fraction <- if (row$lifecycle == "completed") 1 else if (row$total_steps > 0L && (row$done_steps > 0L || !is.null(row$current_stage)))
    row$done_steps / row$total_steps else NULL
  if (row$lifecycle %in% c("completed", "execution_completed", "failed", "cancelled")) {
    # A last heartbeat is not an end timestamp either: do not grow terminal duration.
    row["elapsed_seconds"] <- list(NULL)
    row["eta_seconds"] <- list(NULL)
    row["stage_index"] <- list(if (row$lifecycle == "completed") row$stage_total else NULL)
  }
  for (key in c("stage_index", "stage_total", "elapsed_seconds", "eta_seconds", "progress_fraction"))
    if (is.null(row[[key]])) row[key] <- list(NULL)
  row
}
