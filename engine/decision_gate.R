#!/usr/bin/env Rscript
# =====================================================================
#  decision_gate.R — 专业决定闸门（filter / final / all）
#
#  为什么要单独成文件
#    原来的写法把整个决定校验和参数生成包在
#      if (isTRUE(HAS_DECISIONS) && stepId != "all" && !isTRUE(b$force))
#    里面。后果有两个，都不是理论问题：
#      ① 带 force=true 时整段被跳过 —— 专业决定等于可以被强制放行绕过；
#      ② 决策模块不可用时（HAS_DECISIONS=false）也整段跳过，
#         于是这两步会在没有任何决定的情况下照常入队。
#    把它抽出来，就能用真实参数逐条验证「什么情况下必须拒绝」。
#
#  边界
#    这里只回答「能不能往下走」，不判断选得对不对。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# 需要人先定、且不允许强制放行的步骤。
dec_gate_protected <- function() c("filter", "final", "all")

# 返回 NULL 表示放行；否则返回 list(code, step, error)。
dec_gate_block <- function(step_id, has_decisions, force) {
  sid <- trimws(as.character(step_id %||% ""))
  if (!nzchar(sid) || !(sid %in% dec_gate_protected())) return(NULL)

  # 决策模块不可用：连问都问不出来，不能默认按缺省跑。
  if (!isTRUE(has_decisions))
    return(list(code = "decisions_unavailable", step = sid,
                error = paste0("决策模块不可用，无法确认这一步的专业选择，未入队。",
                               "这一步不接受缺省值。")))

  # 强制放行只适用于"重跑一次"，不能用来替人做决定。
  if (isTRUE(force))
    return(list(code = "force_not_allowed", step = sid,
                error = paste0("这一步必须由人确认决定，不能用强制放行绕过，未入队。"),
                hint = "请先在页面上把这一步的决定答完，再点执行。"))

  NULL
}