#!/usr/bin/env Rscript
# =====================================================================
#  evidence_check.R -- 逐簇依据的准入校验
#
#  为什么单独成文件
#    这段判断决定「一份簇表能不能拿来给人做筛群决定」。
#    写死在路由里，就只能靠读代码相信它；抽出来才能用真实用例逐条验证。
#
#  要挡住什么
#    · 别的会话留下的簇表（看起来完全正常，但簇号属于另一份对象）
#    · 与本次产物 md5 对不上的表（同名的另一份文件）
#    · 重复簇号、空簇号、缺细胞数、细胞数为负或非整数
#    · 条数与表行数不一致（少读了行，但看着有表）
#
#  边界
#    这里只做形式核对，不判断分群是否合理。
#    任何一项不符都不把依据写进记录：宁可没有依据，
#    也不能让人看着一份来路不明的簇表去决定删哪些细胞。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# 返回 list(ok=TRUE, evidence=ce) 或 list(ok=FALSE, why=<字符向量>)
evidence_check_cluster1 <- function(ce, step_id = "", sid = "",
                                    artifacts = list(),
                                    want_rel = "5.markers/cluster_stats.xls") {
  why <- character(0)
  if (!is.list(ce)) return(list(ok = FALSE, why = "依据不是对象"))

  if (!identical(as.character(step_id), "markers"))
    why <- c(why, "不是 markers 阶段")

  if (!nzchar(as.character(sid)) ||
      !identical(as.character(ce$session_id %||% ""), as.character(sid)))
    why <- c(why, "session 不符")

  rel <- as.character(ce$rel %||% ce$source_table %||% "")
  if (!identical(rel, want_rel))
    why <- c(why, paste0("来源表不是本阶段统计表（", rel, "）"))

  # md5 必须与本次回写里同名产物的 md5 一致，且是 32 位十六进制。
  art_md5 <- ""
  for (a in artifacts) {
    if (identical(as.character(a$rel %||% ""), want_rel)) {
      art_md5 <- as.character(a$md5 %||% "")
      break
    }
  }
  ce_md5 <- as.character(ce$md5 %||% ce$source_md5 %||% "")
  if (!nzchar(art_md5)) {
    why <- c(why, "本次产物清单里没有该表的 md5")
  } else if (!identical(ce_md5, art_md5)) {
    why <- c(why, "证据 md5 与产物 md5 不一致")
  }
  if (nzchar(ce_md5) && !grepl("^[0-9a-f]{32}$", ce_md5))
    why <- c(why, "证据 md5 不是 32 位十六进制")

  cl <- ce$clusters %||% list()
  if (!length(cl)) {
    why <- c(why, "簇列表为空")
  } else {
    ids <- vapply(cl, function(x) as.character(x$cluster %||% ""), "")
    if (any(!nzchar(ids))) why <- c(why, "有簇号为空")
    if (anyDuplicated(ids)) why <- c(why, "簇号重复")
    nums <- suppressWarnings(as.numeric(vapply(
      cl, function(x) as.character(x$n_cells %||% NA), "")))
    if (any(is.na(nums))) {
      why <- c(why, "有簇缺细胞数")
    } else if (any(nums < 0)) {
      why <- c(why, "有簇细胞数为负")
    } else if (any(nums != floor(nums))) {
      why <- c(why, "细胞数不是整数")
    }
    rows_tab <- suppressWarnings(as.integer(ce$table_rows %||% NA))
    if (!is.na(rows_tab) && rows_tab != length(cl))
      why <- c(why, "簇条数与表行数不一致")
  }

  if (length(why)) return(list(ok = FALSE, why = why))
  list(ok = TRUE, evidence = ce)
}
