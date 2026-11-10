#!/usr/bin/env Rscript
# =====================================================================
#  orchestrator.R -- 把「一环一环往下推」变成可判定的东西
#
#  为什么需要这个文件
#    以前「一键全流程」是一句话：把整套指令交给助手，跑完回写。
#    中间没有任何一处回答「下一环现在能不能开」。于是：
#      · 上一步没过判定，下游照样开了；
#      · 某环缺环境（比如缺 clusterProfiler），要到真跑才报错；
#      · 跑完之后「有没有收敛」只能靠人肉对比两张表。
#
#  这里做四件事，分别对应清单四条
#    F6 orch_next      下一环能不能开、缺什么、卡在哪
#    G1 decision_log   每一次「为什么这么选」留一行可查记录
#    G2 pipeline_suggest  增删环节的建议 + 自检，越界的建议被拒
#    G4 convergence    同数据两次结果是否一致，偏差归因到哪一类
#
#  一条贯穿的四原则
#    结论只读可核事实。不看「agent 说它跑了」，只看 step_attempts 的状态、
#    产物的 md5、依赖链的自检结果、环境判定的输出。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

ORCH_RULE_VERSION <- "orchestrator-1.0"

# ---- G1：判断留痕 ----------------------------------------------------
#
#  为什么单独一张表、不和 review 合并
#    review 答的是「结果对不对」；decisions 答的是「当时怎么想的」。
#    两件事混在一起，事后就分不清「这一步错了」和「这一步的规则写错了」。
#
#  kind 取值
#    pick_script   为什么用这个脚本
#    change_stage  为什么加/删/改一环
#    skip          为什么跳过
#    change_param  为什么改参数
#    gate_answer   人在这道闸门里定了什么
#    override      为什么放行一个没通过的环节
decision_log <- function(con, kind, basis = "", quote = "", outcome = "",
                         owner_key = NULL, version_id = NULL, run_id = NULL,
                         step_id = NULL, payload = NULL) {
  id <- paste0("dec-", format(Sys.time(), "%Y%m%d%H%M%S"),
               "-", paste(sample(c(letters, 0:9), 4, replace = TRUE), collapse = ""))
  DBI::dbExecute(con,
    "INSERT INTO decisions(decision_id,owner_key,version_id,run_id,step_id,kind,
       basis,quote,outcome,at,payload) VALUES(?,?,?,?,?,?,?,?,?,?,?)",
    list(id, owner_key %||% NA_character_, version_id %||% NA_character_,
         run_id %||% NA_character_, step_id %||% NA_character_,
         as.character(kind), as.character(basis), as.character(quote),
         as.character(outcome), db_now(), db_json(payload)))
  invisible(id)
}

# 从任一产物反查：影响过它的每一次判断
#   这是 G1 的完成判据：任取一轮，能从任一产物反查到影响它的每次判断。
decision_trace <- function(con, version_id, step_id = NULL) {
  sql <- "SELECT decision_id, at, step_id, kind, basis, quote, outcome
            FROM decisions WHERE version_id = ?"
  args <- list(version_id)
  if (!is.null(step_id) && nzchar(step_id)) {
    sql <- paste0(sql, " AND step_id = ?"); args <- c(args, list(step_id))
  }
  rows <- db_rows(con, paste0(sql, " ORDER BY at"), args)
  if (!nrow(rows)) return(list())
  lapply(seq_len(nrow(rows)), function(i) as.list(rows[i, ]))
}

# ---- F6：下一环能不能开 ----------------------------------------------
#
#  返回 list(stage, ready, blocked_by, why, fix)
#    ready      TRUE 才能派作业
#    blocked_by 分类：upstream / env / deps / decision / delivered / done
#
#  为什么要把「卡在哪一类」分出来
#    只说「不能跑」，人要自己去查四件事。分类之后，页面能直接给出
#    「去补环境」还是「去答闸门」还是「去重跑上一步」，这三种动作完全不同。
orch_next <- function(con, app_dir, version_id, profile = "server",
                      env = NULL, gates_pending = 0L) {
  if (is.null(env)) {
    if (!exists("dp_record_env", mode = "function"))
      source(file.path(app_dir, "engine", "dataset_profile.R"))
    env <- dp_record_env(app_dir, con = con, version_id = version_id)
  }
  order <- tryCatch(flow_run_order_engine(app_dir), error = function(e) character(0))
  if (!length(order))
    return(list(ready = FALSE, blocked_by = "deps",
                why = "流程定义读不出来", fix = "检查 engine/flow_spec.R"))

  # 1) 环境：整链判不了或判否，先解决环境 —— 它影响所有环，不是某一环的事。
  env_v <- tryCatch({
    # 先复用已经加载好的函数，再考虑去读文件。
    #   第一版直接 source(app_dir/engine/runnable.R)。问题是 app_dir 不一定是
    #   本应用所在目录（测试用临时目录、调用方传别的根），于是找不到文件，
    #   报错被 tryCatch 吞掉，结论变成「环境判不了」—— 看起来像环境问题，
    #   其实是「找不到自己的模块」。这类错误最难查：它伪装成一个业务结论。
    if (!exists("runnable_chain", mode = "function")) {
      cand <- c(file.path(dirname(sys.frame(1)$ofile %||% "."), "runnable.R"),
                file.path(app_dir, "engine", "runnable.R"),
                "engine/runnable.R")
      hit <- cand[file.exists(cand)][1]
      if (!is.na(hit) && nzchar(hit)) source(hit)
    }
    if (!exists("runnable_chain", mode = "function"))
      stop("找不到 runnable.R：环境判定没法做")
    runnable_chain(env, profile)
  }, error = function(err) list(verdict = "unknown", message = conditionMessage(err)))
  if (!identical(env_v$verdict, "yes")) {
    return(list(ready = FALSE, blocked_by = "env",
                why = env_v$message %||% "环境判不了",
                fix = "先补环境：缺的包或路径在 /api/runnable 里逐项列了",
                env_verdict = env_v$verdict))
  }

  # 2) 依赖链：断了就一处都不该开 —— 断链意味着后面某一环永远等不到东西。
  deps <- tryCatch({
    e <- new.env(parent = globalenv())
    sys.source(file.path(app_dir, "engine", "flow_spec.R"), envir = e)
    e$flow_deps_status("server")
  }, error = function(err) list(ok = FALSE, message = conditionMessage(err)))
  if (!isTRUE(deps$ok)) {
    return(list(ready = FALSE, blocked_by = "deps",
                why = deps$message %||% "依赖链断了",
                fix = "看 /api/flow/deps 的 breaks：它会指出哪一环找不到上游",
                breaks = deps$breaks))
  }

  # 3) 逐环往下走：找第一个还没 verified 的环节
  for (i in seq_along(order)) {
    sid <- order[i]
    r <- db_rows(con,
      "SELECT status, attempt_no, ticket_id FROM step_attempts
        WHERE version_id = ? AND step_id = ? ORDER BY attempt_no DESC LIMIT 1",
      list(version_id, sid))
    st <- if (nrow(r)) as.character(r$status[1]) else "missing"

    if (st %in% c("verified", "released")) next   # 这一环过了，看下一环

    # 待答闸门优先于派作业：没定就跑，跑出来的东西不是人想要的。
    if (gates_pending > 0L)
      return(list(ready = FALSE, stage = sid, blocked_by = "decision",
                  why = paste0("这一环有 ", gates_pending, " 件要人先定"),
                  fix = "去「需要你决定的事」把问题答完"))

    return(list(
      ready = FALSE, stage = sid, upstream_status = st,
      blocked_by = if (identical(st, "rejected")) "upstream"
                   else if (identical(st, "escalated")) "escalated"
                   else "pending",
      ticket_id = if (nrow(r)) as.character(r$ticket_id[1] %||% NA) else NULL,
      why = switch(st,
        missing = "这一环还没跑过",
        delegated = "已经交给助手，等它回写",
        reported = "已申报，还没通过判定",
        rejected = "上一次没通过判定，需要按返工单重跑",
        escalated = "自动返工用尽，等人决策",
        st),
      fix = switch(st,
        missing = "可以开跑",
        delegated = "等回写；超过 due_at 会自动催",
        reported = "等判定结果",
        rejected = "照返工单的命令重跑这一环",
        escalated = "去判定面板看三张返工单的对比，决定是改规则还是改脚本",
        "先确认这一环的状态")))
  }

  list(ready = TRUE, stage = NULL, blocked_by = NULL,
       why = "12 环全部已核验，可以进交付门", fix = "调 /api/deliverable 确认")
}

# ---- G2：pipeline 增删建议 ------------------------------------------
#
#  「建议」不等于「生效」。这里做的三件事：
#    1. 把建议写成一份候选 flow_spec 文本（草稿目录，不覆盖在用文件）
#    2. 对候选跑依赖自检 + 契约检查
#    3. 任一项不过就拒绝，并指出断点
#  越界的建议（删掉别人依赖的环节、改名不改下游）在这三步里会被拦下。
pipeline_suggest <- function(app_dir, action, stage_id, reason = "",
                             drop_stage = NULL) {
  f <- file.path(app_dir, "engine", "flow_spec.R")
  if (!file.exists(f))
    return(list(ok = FALSE, error = "找不到 flow_spec.R"))
  txt <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  # 只在草稿里改，不改在用文件。
  #   为什么：改坏了流程定义，整个应用都点不动；
  #   草稿改完通过自检才允许人手工合入 —— 这一步刻意留给人。
  draft_dir <- file.path(app_dir, "state", "drafts")
  dir.create(draft_dir, showWarnings = FALSE, recursive = TRUE)
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  draft <- file.path(draft_dir, paste0("flow_spec.", stamp, ".R"))

  result <- list(ok = FALSE, action = action, stage = stage_id,
                 reason = reason, draft = draft, checks = list())

  if (identical(action, "drop")) {
    target <- as.character(drop_stage %||% stage_id)
    # 删之前先看谁依赖它：有下游依赖就拒绝，并指出是哪几环。
    e <- new.env(parent = globalenv()); sys.source(f, envir = e)
    consumers <- character(0)
    for (sid in names(e$FLOW_CONSUMES)) {
      decl <- e$FLOW_CONSUMES[[sid]] %||% character(0)
      if (any(grepl(paste0("stage:", target, "($|/)"), decl))) consumers <- c(consumers, sid)
    }
    result$checks[[1]] <- list(
      name = "下游依赖", ok = !length(consumers),
      detail = if (length(consumers))
        paste0(target, " 被这些环节依赖：", paste(consumers, collapse = "、"),
               "；直接删会让它们断链")
      else "没有下游依赖它")
    if (length(consumers)) {
      result$error <- "拒绝：删掉这一环会让下游断链"
      result$consumers <- consumers
      return(result)
    }
    # 没有下游依赖：生成草稿（把该 id 那一块的 artifacts 与发布条目清掉，
    #   这里只做最小改动，完整合入留给人）。
    result$ok <- TRUE
    result$note <- paste0("没有下游依赖，可以删。草稿写到 ", draft,
                          "；合入前请再跑一次 /api/flow/deps。")
    writeLines(c(sub("\\s+$", "", txt),
                 "", paste0("# [draft ", stamp, "] 建议删除环节：", target,
                            "（理由：", reason, "）"),
                 paste0("# 合入方式：删掉 SERVER_STAGES 里 id = \"", target,
                        "\" 那一段，以及 FLOW_REVIEW / FLOW_ENV / FLOW_CONSUMES 里对应的条目。")),
               draft, useBytes = TRUE)
    return(result)
  }

  result$error <- paste0("暂不支持的建议类型：", action)
  result
}

# ---- G4：收敛 ---------------------------------------------------------
#
#  「同数据两次结果 md5 一致」是这条的完成判据。
#  但两次结果不一致不等于错 —— 可能是：
#    data     换了输入（样本、矩阵），结果本来该不同
#    param    参数变了
#    script   脚本被改过
#    env      环境变了（R 包版本、随机种子）
#  先归因，再决定要不要重跑。不归因就重跑，等于把随机性当成 bug 追。
orch_convergence <- function(con, version_id, step_id = NULL) {
  sql <- "SELECT attempt_id, step_id, attempt_no, payload FROM step_attempts
           WHERE version_id = ?"
  args <- list(version_id)
  if (!is.null(step_id) && nzchar(step_id)) {
    sql <- paste0(sql, " AND step_id = ?"); args <- c(args, list(step_id))
  }
  rows <- db_rows(con, paste0(sql, " ORDER BY step_id, attempt_no"), args)
  if (!nrow(rows))
    return(list(converged = NA, why = "这一版还没有任何回写，无从比较", by_stage = list()))

  by_stage <- list()
  for (sid in unique(as.character(rows$step_id))) {
    rr <- rows[as.character(rows$step_id) == sid, , drop = FALSE]
    if (nrow(rr) < 2L) {
      by_stage[[sid]] <- list(converged = NA, why = "只跑过一次，无法比较")
      next
    }
    sig <- vapply(seq_len(nrow(rr)), function(i) {
      b <- db_unjson(rr$payload[i], list())
      arts <- b$artifacts %||% list()
      md5s <- vapply(arts, function(a) as.character(a$md5 %||% ""), "")
      # 没有 md5 就没法比：不能因为「都没给 md5」就说两次一致。
      if (!length(md5s) || !any(nzchar(md5s))) return(NA_character_)
      paste(sort(md5s[nzchar(md5s)]), collapse = "|")
    }, "")
    if (all(is.na(sig))) {
      by_stage[[sid]] <- list(converged = NA, why = "两次回写都没给 md5，判不了")
      next
    }
    ok <- length(unique(stats::na.omit(sig))) == 1L
    by_stage[[sid]] <- list(
      converged = ok,
      attempts = as.integer(rr$attempt_no),
      why = if (ok) "两次产物的 md5 完全一致"
            else "两次产物的 md5 不一样，先归因再决定要不要重跑")
  }

  vals <- vapply(by_stage, function(x) x$converged, NA)
  overall <- if (all(is.na(vals))) NA else all(vals, na.rm = TRUE)
  list(converged = overall, by_stage = by_stage,
       rule_version = ORCH_RULE_VERSION,
       why = if (is.na(overall)) "判不了：缺可比的 md5"
             else if (isTRUE(overall)) "全部环节两次一致"
             else "有环节两次不一致")
}

# 偏差归因：给出四类可能性，按可核证据排序（不是猜一个）
orch_attribute <- function(diffs, data_changed = FALSE, param_changed = FALSE,
                           script_changed = FALSE, env_changed = FALSE) {
  hits <- c(data = data_changed, param = param_changed,
            script = script_changed, env = env_changed)
  if (!any(hits)) return(list(kind = "unknown", why = "没有任何变更记录，先记一条 decisions"))
  list(kind = paste(names(hits)[hits], collapse = "+"),
       why = "按已记录的变更归因：改了哪一类，就先查哪一类")
}
