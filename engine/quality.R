# =====================================================================
#  每一步的质检：生成自己的质检文件，检查上一步的质检文件
# ---------------------------------------------------------------------
#  gina 2026-10-29 的要求：
#    「把应用里的每一步的质检过程给加上，包括生成质检文件和检查上一个环节
#      的质检文件，这个机制都加上，但目前不需要影响现在的数据处理流程，
#      只需要在目前刚刚跑完的结果中补一个流水，给下一步做准备。」
#
#  所以这一版的分工是：
#    · 生成：每一步回写完成/失败时，应用侧就写一份自己的质检结论
#      （不依赖服务器上跑 reviewer.R，因为那条链要 status/ledger.jsonl，
#       而这轮是手动提交的，从来没有那份流水）。
#    · 检查：下一步启动前，读上一步的质检结论并展示出来。
#    · 不阻断：这一版质检只做"记账 + 提示"，不拦任何按钮。
#      要拦的话另开一个开关，不能一边说"不影响流程"一边偷偷拦。
#
#  质检文件落在应用侧（state/quality/<session>.json），是应用自己的账。
#  服务器那份 status/ledger.jsonl 是执行端的账，两者分开记，
#  避免以后又出现"一份账被两处写、谁也不知道哪份是真的"。
# =====================================================================

QUAL_DIR <- local({
  # 走统一开关（B2）：分区立起来了就用分区，否则回退 state/。
  #   为什么用 local({...})：R 在顶层遇到换行后的 else 会报 unexpected 'else'。
  #   这跟逻辑无关，纯粹是语法层面的坑，写成一整块就不会踩。
  if (exists("state_private_root", mode = "function")) {
    file.path(state_private_root(dirname(STATE_DIR)), "quality")
  } else {
    file.path(STATE_DIR, "quality")
  }
})

qual_path <- function(sid) {
  s <- as.character(sid %||% "")
  if (!nzchar(s)) s <- "_default"
  s <- gsub("[^A-Za-z0-9._-]", "_", s)
  file.path(QUAL_DIR, paste0(s, ".json"))
}

qual_read <- function(sid) {
  f <- qual_path(sid)
  if (!file.exists(f)) return(list(session_id = as.character(sid %||% ""), steps = list()))
  d <- tryCatch(jsonlite::fromJSON(read_text_any(f), simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(d) || is.null(d$steps)) return(list(session_id = as.character(sid %||% ""), steps = list()))
  d
}

qual_write <- function(sid, d) {
  dir.create(QUAL_DIR, showWarnings = FALSE, recursive = TRUE)
  d$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  write_json_file(qual_path(sid), d)
  invisible(d)
}

# 给某一步出一份质检结论。
#   checks 里每条是 list(name=, ok=TRUE/FALSE/NA, detail=)
#   ok 的判定：有任一 FALSE 就是没过；全是 NA 就是"没可比的东西"，记 unknown。
#   为什么要分三态：把"没检查"和"检查了不合格"混成一种，
#   人就没法判断该补检查还是该修问题 —— 这一轮踩过太多次。
qual_make <- function(sid, step, checks = list(), artifacts = list(),
                      cells = NULL, note = "") {
  d <- qual_read(sid)
  st <- c()
  for (c0 in checks) {
    st <- c(st, if (isTRUE(c0$ok)) "pass" else if (identical(c0$ok, FALSE)) "fail" else "unknown")
  }
  verdict <- if (any(st == "fail")) "fail" else if (all(st == "unknown") && length(st)) "unknown" else "pass"
  if (!length(st)) verdict <- "unknown"

  item <- list(
    step = as.character(step),
    verdict = verdict,
    checks = checks,
    artifacts = as.list(unlist(artifacts)),
    cells = cells,
    note = as.character(note),
    at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    # 这一版质检不参与放行，写清楚，免得以后有人把它当成闸门
    blocking = FALSE)
  hit <- NULL
  for (i in seq_along(d$steps)) {
    if (identical(as.character(d$steps[[i]]$step %||% ""), as.character(step))) hit <- i
  }
  if (is.null(hit)) d$steps[[length(d$steps) + 1]] <- item else d$steps[[hit]] <- item
  qual_write(sid, d)
  item
}

# 取某一步的质检结论（没有就是 NULL）
qual_of <- function(sid, step) {
  d <- qual_read(sid)
  for (x in d$steps) {
    if (identical(as.character(x$step %||% ""), as.character(step))) return(x)
  }
  NULL
}

# 检查上一步：下一步启动前调用，把结果摆给人看。
#   返回 list(prev=, verdict=, detail=, checked=TRUE/FALSE)
#   checked=FALSE 表示"这一步压根没有上一环质检记录"，
#   要如实说成"没做过质检"，不能当成"通过了"。
qual_prev_check <- function(sid, stepId, order = NULL) {
  ord <- order %||% character(0)
  i <- match(as.character(stepId), ord)
  if (is.na(i) || i <= 1) {
    return(list(prev = "", checked = FALSE, verdict = "none",
                detail = "这是第一步，没有上一环要检查"))
  }
  prev <- ord[[i - 1]]
  x <- qual_of(sid, prev)
  if (is.null(x)) {
    return(list(prev = prev, checked = FALSE, verdict = "missing",
                detail = paste0("上一环「", prev, "」没有质检记录 —— 不是通过了，是没做过")))
  }
  list(prev = prev, checked = TRUE,
       verdict = as.character(x$verdict %||% "unknown"),
       detail = paste0("上一环「", prev, "」质检：", as.character(x$verdict %||% "unknown"),
                       if (nzchar(as.character(x$note %||% ""))) paste0("（", x$note, "）") else ""))
}
