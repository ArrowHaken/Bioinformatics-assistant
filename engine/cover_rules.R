# ---- 覆盖判定：单独成文件，便于用真实反例逐条测 --------------------
#   fullmark / diff 这类环节没有自己的脚本，产出由终版一并算出。
#   "已覆盖"必须同时满足三件事，缺一条都只能显示等待：
#     1) 上游这一环已经跑完（status = done）
#     2) 上游同一 req 已通过严格验收（DB 里有 verified 行）
#     3) 声明的覆盖产物全部出现在上游本次产物清单里
#   为什么不能只看 status：上游刚完成、还没判定，甚至已经被拒时，
#     status 同样是 done。只认 done 会让这两步提前算进"已通过"，
#     页面数字因此虚高，而实际终版并没有被验收。
#
# 这个文件必须自包含：它会被测试脚本单独 source，不能依赖 serve.R 里的
# 辅助函数（曾经因为 %||% 只定义在 serve.R 里，测试直接报"找不到函数"）。
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}

cover_decide <- function(up_status = "", up_verified = FALSE,
                         want = character(0), up_rels = character(0)) {
  want <- as.character(want %||% character(0))
  up_rels <- as.character(up_rels %||% character(0))
  up_done <- identical(as.character(up_status %||% ""), "done")
  hitN <- sum(want %in% up_rels)
  covered <- isTRUE(up_done) && isTRUE(up_verified) &&
    length(want) > 0 && hitN == length(want)
  why <- if (covered) ""
    else if (!isTRUE(up_done)) "上游还没跑完"
    else if (!isTRUE(up_verified)) "等待上游终版验收通过"
    else if (!length(want)) "流程没有声明覆盖产物"
    else paste0("还缺 ", length(want) - hitN, " 个覆盖产物")
  list(covered = covered, hit = hitN, want = want, why = why)
}
