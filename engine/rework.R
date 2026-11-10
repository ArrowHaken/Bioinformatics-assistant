#!/usr/bin/env Rscript
# =====================================================================
#  rework.R -- 返工单：拒绝必须给得出下一步
#
#  为什么需要这个文件
#    判定不通过如果只写一句「未通过」，等于把推理的负担又丢回给写代码的人，
#    他很可能再猜一次。返工单要求每条失败都带 expected / actual / evidence
#    与一条能照着敲的命令，并且自动重投、有上限、超时升级。
#
#  三类记录分开存，互不覆盖（J4）
#    ① 送达重试（inject）   —— 消息有没有进会话
#    ② 合规返工（rework）   —— 东西做对了没有
#    ③ 远程轮询（poll_job） —— 服务器上跑完了没有
#    以前这三件事共用一个 attempts 计数，于是「消息发了 8 次」被当成
#    「返工 8 次」，而真正的失败一次都没被计数。
#
#  退避序列：立刻 / 10 分钟 / 30 分钟 / 2 小时
#  max_auto 默认 3，超过转 escalated，等人工决策。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

REWORK_BACKOFF_SECS <- c(0, 600, 1800, 7200)
REWORK_MAX_AUTO <- 3L
REWORK_DUE_SECS <- 7200          # 2 小时内必须回写

# ---- 返工单 ----------------------------------------------------------

# 从未通过的 checks 生成 failed_checks，并给出可照着敲的命令。
rework_build_checks <- function(checks) {
  bad <- Filter(function(c) c$status %in% c("fail", "unknown"), checks)
  lapply(bad, function(c) {
    list(layer = c$layer, name = c$name,
         expected = as.character(c$expected %||% ""),
         actual = as.character(c$actual %||% ""),
         # 不能拿 detail 兜底：detail 几乎永远非空，
         #   一兜底「缺 evidence 视为不合格」这条就永远不触发。
         evidence = as.character(c$evidence %||% ""),
         why = c$detail %||% "")
  })
}

rework_must_do <- function(stage, failed) {
  out <- character(0)
  nm <- vapply(failed, function(x) x$name, "")
  if (any(grepl("^product:", nm)))
    out <- c(out, paste0("补齐缺失产物：",
                         paste(sub("^product:", "", nm[grepl("^product:", nm)]), collapse = "、")))
  if (any(grepl("^columns:", nm)) || any(nm == "required_columns"))
    out <- c(out, "按契约补齐必需列；缺列属于契约不匹配，不是算法失败")
  if (any(nm == "set_id"))
    out <- c(out, "改用该阶段规定的 skill 执行，并在台账登记正确的 set_id")
  if (any(nm == "script_path") || any(nm == "script_digest"))
    out <- c(out, "按规定的脚本执行，不得另写替代脚本；需要改脚本请转人工")
  if (any(nm == "exit_code"))
    out <- c(out, "脚本以非 0 退出，先修错误再回写，不得把失败报成完成")
  if (any(nm %in% c("md5:", "artifact_list_present")))
    out <- c(out, "回写时逐项带 rel / bytes / md5 / mtime")
  if (!length(out)) out <- "按 failed_checks 逐项修正后重跑本环节"
  out
}

rework_command_hint <- function(stage, version = NULL, work_root = NULL) {
  if (is.null(stage)) return(NA_character_)
  sc <- as.character(stage$script %||% "")
  ar <- as.character(stage$args %||% character(0))
  ar <- gsub("\\{work_root\\}", work_root %||% "<work_root>", ar)
  ar <- gsub("\\{engine\\}", "<engine>", ar)
  ar <- gsub("\\{script_root\\}", "<script_root>", ar)
  ar <- gsub("\\{liny_data\\}", "<liny_data>", ar)
  outdir <- as.character(stage$outdir %||% "")
  parts <- c(sc, ar)
  if (nzchar(outdir) && !any(grepl("--outdir", parts)))
    parts <- c(parts, "--outdir", file.path(work_root %||% "<work_root>", outdir))
  paste(parts, collapse = " ")
}

rework_issue <- function(con, app_dir, attempt_id, checks, verdict = "fail",
                         stage = NULL, facts = NULL) {
  att <- db_rows(con, "SELECT * FROM step_attempts WHERE attempt_id = ?", list(attempt_id))
  if (!nrow(att)) stop("没有这个 attempt：", attempt_id)
  att <- as.list(att[1, ])

  failed <- rework_build_checks(checks)
  # 返工单必须每项都带 expected / actual / evidence，缺一样视为不合格，不投递（J1）。
  ok_shape <- length(failed) > 0 && all(vapply(failed, function(f)
    nzchar(f$expected) && nzchar(f$actual) && nzchar(f$evidence), TRUE))
  if (!ok_shape) {
    db_event(con, "rework_ticket_malformed", owner_key = att$owner_key,
             version_id = att$version_id, step_id = att$step_id, attempt_id = attempt_id,
             payload = list(why = "failed_checks 缺 expected/actual/evidence，不允许投递",
                            failed = failed))
    return(NA_character_)
  }

  ticket_id <- paste0("RW-", format(Sys.Date(), "%Y%m%d"), "-",
                      sprintf("%04d", as.integer(db_scalar(
                        con, "SELECT COUNT(*)+1 FROM rework_tickets"))))
  attempt_no <- as.integer(att$attempt_no %||% 1)

  db_tx(con, function() {
    DBI::dbExecute(con,
      "INSERT INTO rework_tickets(ticket_id,attempt_id,run_id,version_id,step_id,owner_key,
         issued_at,due_at,attempt_no,max_auto,state,failed_checks,must_do,command_hint,payload)
       VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
      list(ticket_id, attempt_id, att$run_id, att$version_id, att$step_id, att$owner_key,
           db_now(), db_iso_add(REWORK_DUE_SECS), attempt_no, REWORK_MAX_AUTO,
           if (attempt_no > REWORK_MAX_AUTO) "escalated" else "open",
           db_json(failed), db_json(rework_must_do(stage, failed)),
           rework_command_hint(stage, work_root = att$payload),
           db_json(list(verdict = verdict,
                        stage_title = stage$title %||% att$step_id))))

    # 返工单本身入调度队列：到期要催，催不动要升级。
    DBI::dbExecute(con,
      "INSERT INTO poll_jobs(job_id,kind,owner_key,run_id,version_id,step_id,attempt_id,
         ticket_id,state,next_check_at,created_at,updated_at)
       VALUES(?,?,?,?,?,?,?,?,?,?,?,?)",
      list(paste0("job-", ticket_id), "rework", att$owner_key, att$run_id,
           att$version_id, att$step_id, attempt_id, ticket_id,
           "pending", db_iso_add(REWORK_BACKOFF_SECS[min(attempt_no, length(REWORK_BACKOFF_SECS))]),
           db_now(), db_now()))

    # ---- 返工单进投递队列（I3 / J3）------------------------------
    #   之前只写进 rework_tickets 表，agent 那边什么都收不到 ——
    #   「退回」只退到了数据库里，等于没有敦促。
    #   正文在这里生成一次，落库保留原文：会话送不出去也能复制出来用。
    txt <- tryCatch(rework_instruction_text(con, ticket_id), error = function(e) "")
    if (nzchar(txt)) {
      DBI::dbExecute(con,
        "INSERT OR REPLACE INTO outbox(item_id,owner_key,session_ref,step_id,req_id,ticket_id,
           text,reason,state,attempts,next_attempt_at,created_at,updated_at)
         VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)",
        list(paste0("ob-", ticket_id), att$owner_key, NA_character_, att$step_id,
             att$req_id %||% NA_character_, ticket_id, txt, "rework", "pending",
             0L, db_iso_add(REWORK_BACKOFF_SECS[min(attempt_no, length(REWORK_BACKOFF_SECS))]),
             db_now(), db_now()))
    }

    # 状态流转：rejected 或 escalated，绝不回写成 done。
    new_state <- if (attempt_no > REWORK_MAX_AUTO) "escalated" else "rejected"
    DBI::dbExecute(con, "UPDATE step_attempts SET status=?, ticket_id=? WHERE attempt_id=?",
                   list(new_state, ticket_id, attempt_id))
    db_event(con, if (identical(new_state, "escalated")) "rework_escalated" else "rework_issued",
             owner_key = att$owner_key, version_id = att$version_id, run_id = att$run_id,
             step_id = att$step_id, attempt_id = attempt_id,
             payload = list(ticket_id = ticket_id, attempt_no = attempt_no,
                            max_auto = REWORK_MAX_AUTO))
  })
  ticket_id
}

# ---- 返工指令正文（J3）-----------------------------------------------

rework_instruction_text <- function(con, ticket_id) {
  t <- db_rows(con, "SELECT * FROM rework_tickets WHERE ticket_id = ?", list(ticket_id))
  if (!nrow(t)) return(NULL)
  t <- as.list(t[1, ])
  failed <- db_unjson(t$failed_checks, list())
  must <- as.character(db_unjson(t$must_do, character(0)))
  lines <- c(
    paste0("【来自 个性化分析助手 · 返工单 ", t$ticket_id, "】"),
    paste0("上一轮 ", t$step_id, "（第 ", t$attempt_no, " 次尝试）未通过判定，不能进入下一步。"),
    "通过判定需要满足：")
  for (i in seq_along(failed)) {
    f <- failed[[i]]
    lines <- c(lines,
      paste0("  ", i, ". [", f$layer, " ", f$name, "] ", f$expected),
      paste0("     实际：", f$actual),
      paste0("     证据：", f$evidence))
  }
  if (length(must)) {
    lines <- c(lines, "需要做的事：", paste0("  - ", must))
  }
  if (!is.na(t$command_hint %||% NA) && nzchar(t$command_hint %||% "")) {
    lines <- c(lines, "请照这条命令重跑，不要另写脚本：", paste0("  ", t$command_hint))
  }
  lines <- c(lines,
    "完成后回写 POST /api/step/import，带 ticket_id 与 req_id。",
    if (t$state == "escalated")
      paste0("自动返工已达上限（", t$max_auto, " 次），本次转人工决策。")
    else
      paste0("这是自动返工第 ", t$attempt_no, " 次（上限 ", t$max_auto, " 次），再不过将转人工。"))
  paste(lines, collapse = "\n")
}

# ---- 到期扫描（由 poller.R 调用）-------------------------------------

rework_due <- function(con, now = db_now()) {
  db_rows(con, "SELECT * FROM rework_tickets
                WHERE state IN ('open','queued','sent') AND due_at <= ?
                ORDER BY due_at", list(now))
}

# 已催过一次仍没回写 -> escalated。第一次超时只催一次，不直接升级。
rework_nudge <- function(con, ticket_id, app_dir = NULL) {
  t <- db_rows(con, "SELECT * FROM rework_tickets WHERE ticket_id = ?", list(ticket_id))
  if (!nrow(t)) return(list(ok = FALSE, error = "no_ticket"))
  t <- as.list(t[1, ])
  nudges <- as.integer(t$nudges %||% 0)

  if (nudges >= 1L) {
    DBI::dbExecute(con, "UPDATE rework_tickets SET state='escalated' WHERE ticket_id=?",
                   list(ticket_id))
    db_event(con, "rework_escalated", owner_key = t$owner_key, version_id = t$version_id,
             run_id = t$run_id, step_id = t$step_id, attempt_id = t$attempt_id,
             payload = list(ticket_id = ticket_id, why = "两次超时未回写"))
    return(list(ok = TRUE, action = "escalated"))
  }

  DBI::dbExecute(con,
    "UPDATE rework_tickets SET nudges=nudges+1, last_nudge_at=?, due_at=? WHERE ticket_id=?",
    list(db_now(), db_iso_add(REWORK_DUE_SECS), ticket_id))
  txt <- rework_instruction_text(con, ticket_id)
  db_event(con, "rework_nudged", owner_key = t$owner_key, version_id = t$version_id,
           step_id = t$step_id, attempt_id = t$attempt_id,
           payload = list(ticket_id = ticket_id, nudge = nudges + 1L))
  list(ok = TRUE, action = "nudged", text = txt)
}

# ---- 人工放行（只有人能放行）----------------------------------------

# ---- 人工放行（I5 第 4 条 / L2 场景 6）-------------------------------
#
#  为什么必须校验「调用者」而不是「自报的名字」
#    旧实现只看 who 这个字符串的前缀。于是任何人 POST {"who":"gina"} 就能放行 ——
#    agent 只要不写 "agent" 这个词就过得去。那不是校验，是礼貌。
#    而「agent 不能给自己放行」是这套闭环的最后一道人机分工线：
#    前九道口子都在拦机器，这一道拦的是「机器替人签字」。
#
#  现在凭什么判「是人」
#    1. 调用必须带得出平台身份（owner_key）。身份拿不到一律拒绝，不降级。
#    2. 放行记录同时写两样：verified_actor（平台给出的身份，不可自报）
#       与 claimed_name（调用方自报的名字，仅供参考，不作为依据）。
#       两者不一致时以 verified_actor 为准，并把差异留在事件里。
#    3. 自报名字仍带 agent 字样的一并拒绝（增加摩擦力，不作为唯一依据）。
#    4. 只有记录的所有者能放行别人的记录？—— 这里放开：团队里 reviewer
#       也要能放行。但 verified_actor 一定落在事件里，事后可追。
rework_release <- function(con, attempt_id, who, why, verified_actor = NULL,
                           caller_kind = NULL) {
  if (!nzchar(attempt_id %||% ""))
    stop("缺少 attempt_id")
  if (!nzchar(why %||% ""))
    stop("放行必须填理由：理由是交付清单的一部分，不能空")
  # 身份必须来自调用上下文，不能来自请求体。
  if (is.null(verified_actor) || is.na(verified_actor) || !nzchar(verified_actor))
    stop("身份未识别，不能放行；请从平台会话里重新打开这个应用")
  if (!is.null(caller_kind) && identical(as.character(caller_kind), "agent"))
    stop("agent 不能给自己放行；放行只能由人操作")
  if (grepl("^(agent|assistant|bot|claude|gpt|model|system)", tolower(who %||% "")))
    stop("自报名字是机器身份，不能放行；放行只能由人操作")

  att <- db_rows(con, "SELECT * FROM step_attempts WHERE attempt_id=?", list(attempt_id))
  if (!nrow(att)) stop("没有这个 attempt")
  att <- as.list(att[1, ])

  DBI::dbExecute(con, "UPDATE step_attempts SET status='released', verified_at=? WHERE attempt_id=?",
                 list(db_now(), attempt_id))
  db_event(con, "review_override", owner_key = att$owner_key, version_id = att$version_id,
           run_id = att$run_id, step_id = att$step_id, attempt_id = attempt_id,
           payload = list(
             verified_actor = verified_actor,
             claimed_name = who %||% "",
             name_mismatch = nzchar(who %||% "") && !identical(who, verified_actor),
             why = why, at = db_now(),
             note = "verified_actor 来自平台身份，claimed_name 是自报，不作为依据"))
  invisible(TRUE)
}
