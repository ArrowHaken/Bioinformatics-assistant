#!/usr/bin/env Rscript
# =====================================================================
#  evidence_merge.R — 已验收同任务「只补决策依据」的准入判断
#
#  为什么单独成文件
#    这段决定「能不能把新采到的逐簇依据并进已有记录」。
#    写死在路由里只能靠读代码相信它；抽出来才能用真实参数逐条验证。
#
#  要挡住什么
#    · 用旧已验收 req 的记录，去覆盖同会话后来重跑的结果
#    · 记录里的 run_dir 与本次不一致
#    · 写回被吞掉错误却仍报成功
#
#  边界
#    只判准入；写回与读回核对由调用方做，失败必须如实返回失败。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# 返回 NULL 表示可以合并；否则 list(code, error)
evidence_merge_check <- function(rec2, req_id, run_dir, sid, step, new_md5) {
  if (!is.list(rec2)) return(list(code = "no_record", error = "找不到已有记录"))

  wr <- trimws(as.character(rec2$req_id %||% ""))
  want_rq <- trimws(as.character(req_id %||% ""))
  if (!nzchar(want_rq) || !identical(wr, want_rq))
    return(list(code = "req_mismatch",
                error = paste0("记录所属请求号与本次不一致（记录 ", wr,
                               " / 本次 ", want_rq, "），拒绝合并")))

  ws <- trimws(as.character(rec2$session_id %||% ""))
  want_sid <- trimws(as.character(sid %||% ""))
  if (nzchar(want_sid) && nzchar(ws) && !identical(ws, want_sid))
    return(list(code = "session_mismatch", error = "记录会话与本次不一致"))

  wr_dir <- trimws(as.character(rec2$run_dir %||% ""))
  want_dir <- trimws(as.character(run_dir %||% ""))
  if (nzchar(want_dir) && nzchar(wr_dir) && !identical(wr_dir, want_dir))
    return(list(code = "run_dir_mismatch",
                error = paste0("记录运行目录与本次不一致（记录 ", wr_dir,
                               " / 本次 ", want_dir, "），拒绝合并")))

  st <- trimws(as.character(rec2$status %||% ""))
  if (nzchar(st) && !st %in% c("done", "verified"))
    return(list(code = "record_not_done", error = "记录不是完成状态，拒绝合并"))

  # 新证据必须与要写进去的那份 md5 一致（防止传了 A 却核了 B）
  ce <- (rec2$evidence %||% list())$cluster1_evidence
  if (is.list(ce) && !is.null(new_md5)) {
    cur <- trimws(as.character(ce$md5 %||% ""))
    if (nzchar(cur) && !identical(cur, trimws(as.character(new_md5))))
      return(list(code = "evidence_conflict",
                  error = "记录里已有另一份逐簇依据，拒绝静默覆盖"))
  }
  NULL
}

# 补 after_drop_stats（逐样本计数 + 分组）时的准入校验。
#   为什么单独一条：分组只有集群侧 RDS 里才有，filter 当初回写时没带；
#   到了 final 决策卡就会显示「分组未提供」，人会误以为分组未知。
#   这里允许补，但必须：同 req / 同 session / 同 run_dir、已完成、
#   计数自洽（样本合计 = 总数）、分组键都能对上样本、
#   且不得与记录里已有的不同数值静默覆盖。
evidence_after_drop_check <- function(rec2, req_id, run_dir, sid, step, ad,
                                      new_artifacts = NULL) {
  if (!is.list(rec2)) return(list(code = "no_record", error = "找不到已有记录"))

  # 身份必须齐全且一致。缺一项就拒：缺证据时放行，等于这条通道没有身份约束。
  wr <- trimws(as.character(rec2$req_id %||% ""))
  want <- trimws(as.character(req_id %||% ""))
  if (!nzchar(want)) return(list(code = "req_missing", error = "本次没有请求号"))
  if (!nzchar(wr)) return(list(code = "req_missing", error = "记录里没有请求号"))
  if (!identical(wr, want))
    return(list(code = "req_mismatch",
                error = paste0("记录请求号与本次不一致（记录 ", wr, " / 本次 ", want, "）")))

  ws <- trimws(as.character(rec2$session_id %||% ""))
  wsid <- trimws(as.character(sid %||% ""))
  if (!nzchar(wsid)) return(list(code = "session_missing", error = "本次没有会话号"))
  if (!nzchar(ws)) return(list(code = "session_missing", error = "记录里没有会话号"))
  if (!identical(ws, wsid))
    return(list(code = "session_mismatch", error = "记录会话与本次不一致"))

  wd <- trimws(as.character(rec2$run_dir %||% ""))
  wdir <- trimws(as.character(run_dir %||% ""))
  if (!nzchar(wdir)) return(list(code = "run_dir_missing", error = "本次没有运行目录"))
  if (!nzchar(wd)) return(list(code = "run_dir_missing", error = "记录里没有运行目录"))
  if (!identical(wd, wdir))
    return(list(code = "run_dir_mismatch", error = "记录运行目录与本次不一致"))

  # 步骤必须是 filter：这条通道只补筛群阶段的逐样本证据。
  wst <- trimws(as.character(rec2$step_id %||% rec2$id %||% ""))
  wstep <- trimws(as.character(step %||% ""))
  if (!nzchar(wstep)) return(list(code = "step_missing", error = "没有指定步骤"))
  if (!identical(wstep, "filter"))
    return(list(code = "step_not_filter", error = paste0("这条通道只补 filter，本次是 ", wstep)))
  if (nzchar(wst) && !identical(wst, "filter"))
    return(list(code = "step_not_filter", error = paste0("记录步骤不是 filter：", wst)))

  st <- trimws(as.character(rec2$status %||% ""))
  if (!st %in% c("done", "verified"))
    return(list(code = "record_not_done",
                error = paste0("记录状态是 ", if (nzchar(st)) st else "(空)",
                               "，只有 done / verified 才能补证据")))

  if (!is.list(ad)) return(list(code = "bad_evidence", error = "证据不是对象"))

  # 产物清单必须给全，且必须有 md5：空清单等于跳过全部文件核验。
  if (!length(new_artifacts))
    return(list(code = "artifacts_missing", error = "本次没有带产物清单，无法证明产物未变"))
  nk <- vapply(new_artifacts, function(a) as.character(a$md5 %||% a$checksum %||% ""), "")
  nm <- vapply(new_artifacts, function(a) as.character(a$rel %||% a$path %||% ""), "")
  if (any(!nzchar(nm)))
    return(list(code = "artifacts_missing", error = "产物清单里有条目不写路径"))
  if (any(nchar(nk) != 32))
    return(list(code = "artifacts_no_md5",
                error = "产物清单里有条目没有 32 位 md5，无法核对是否被改动"))
  names(nk) <- nm

  # 本次输入 RDS 的原 hash 必须与记录一致（这是 final 下游要消费的那份对象）。
  REL_RDS <- "7.after_drop/merged_after_drop.rds"
  ok_art <- rec2$artifacts %||% list()
  okm <- vapply(ok_art, function(a) as.character(a$md5 %||% a$checksum %||% ""), "")
  okn <- vapply(ok_art, function(a) as.character(a$rel %||% a$path %||% ""), "")
  names(okm) <- okn
  rds_new <- nk[REL_RDS]
  rds_old <- okm[REL_RDS]
  if (!length(rds_new) || is.na(rds_new) || !nzchar(rds_new))
    return(list(code = "input_rds_missing", error = "产物清单里没有本次输入 RDS"))
  if (!length(rds_old) || is.na(rds_old) || !nzchar(rds_old))
    return(list(code = "input_rds_missing", error = "记录里没有输入 RDS 的 md5 基线"))
  if (!identical(unname(rds_new), unname(rds_old)))
    return(list(code = "input_rds_changed",
                error = paste0("输入 RDS 的 md5 与记录不一致（记录 ", rds_old,
                               " / 本次 ", rds_new, "）")))

  # 其余产物：旧清单每项都要在，且同一条的 md5 一致。
  if (length(okm)) {
    if (length(setdiff(names(okm), names(nk))))
      return(list(code = "artifacts_changed", error = "本次产物清单缺了记录里的条目"))
    sh <- intersect(names(okm), names(nk))
    if (length(sh) && !identical(unname(okm[sh]), unname(nk[sh])))
      return(list(code = "artifacts_changed", error = "同一条产物的 md5 与记录不一致"))
  }

  tot <- suppressWarnings(as.numeric(ad$cells_total %||% NA))
  smp <- ad$samples
  if (!is.list(smp) || !length(smp))
    return(list(code = "bad_samples", error = "样本计数缺失或为空"))
  vals <- suppressWarnings(as.numeric(unlist(smp)))
  if (any(is.na(vals)) || any(vals < 0))
    return(list(code = "bad_samples", error = "样本计数含非数值或负数"))
  if (is.na(tot) || sum(vals) != tot)
    return(list(code = "samples_sum_mismatch",
                error = paste0("样本合计 ", sum(vals), " 与总数 ", tot, " 不一致")))

  sg <- ad$sample_group %||% list()
  if (length(sg)) {
    if (!is.list(sg)) return(list(code = "bad_group", error = "分组不是键值对"))
    if (!all(names(sg) %in% names(smp)))
      return(list(code = "group_not_in_samples",
                  error = "分组里出现了样本计数中没有的样本名"))
    gv <- trimws(as.character(unlist(sg)))
    if (any(!nzchar(gv)))
      return(list(code = "bad_group", error = "分组值为空"))
    if (length(sg) != length(smp))
      return(list(code = "group_incomplete",
                  error = paste0("分组只覆盖 ", length(sg), " / ", length(smp), " 个样本")))
  }

  old <- (rec2$evidence %||% list())$after_drop_stats
  if (is.list(old) && length(old)) {
    os <- old$samples
    if (is.list(os) && length(os)) {
      a <- sort(vapply(names(os), function(k) paste0(k, "=", as.character(os[[k]])), ""))
      b <- sort(vapply(names(smp), function(k) paste0(k, "=", as.character(smp[[k]])), ""))
      if (!identical(a, b))
        return(list(code = "counts_conflict",
                    error = "记录里已有另一份逐样本计数，拒绝静默覆盖"))
    }
    og <- old$sample_group
    if (is.list(og) && length(og) && length(sg)) {
      a <- sort(vapply(names(og), function(k) paste0(k, "=", as.character(og[[k]])), ""))
      b <- sort(vapply(names(sg), function(k) paste0(k, "=", as.character(sg[[k]])), ""))
      if (!identical(a, b))
        return(list(code = "group_conflict", error = "记录里已有另一份分组，拒绝静默覆盖"))
    }
  }
  NULL
}

# 读回核对：写完必须逐字段相等，才允许报成功。
evidence_after_drop_verify <- function(readback, req_id, run_dir, ad) {
  if (!is.list(readback)) return(list(ok = FALSE, why = "读回为空"))
  if (!identical(trimws(as.character(readback$req_id %||% "")),
                 trimws(as.character(req_id %||% ""))))
    return(list(ok = FALSE, why = "读回的 req_id 与本次不一致"))
  if (!identical(trimws(as.character(readback$run_dir %||% "")),
                 trimws(as.character(run_dir %||% ""))))
    return(list(ok = FALSE, why = "读回的 run_dir 与本次不一致"))
  got <- (readback$evidence %||% list())$after_drop_stats
  if (!is.list(got)) return(list(ok = FALSE, why = "读回里没有 after_drop_stats"))

  gs <- got$samples; ws <- ad$samples
  if (!is.list(gs) || !length(gs)) return(list(ok = FALSE, why = "读回的样本计数为空"))
  a <- sort(vapply(names(ws), function(k) paste0(k, "=", as.character(ws[[k]])), ""))
  b <- sort(vapply(names(gs), function(k) paste0(k, "=", as.character(gs[[k]])), ""))
  if (!identical(a, b)) return(list(ok = FALSE, why = "读回的样本计数与提交不一致"))

  gg <- got$sample_group %||% list(); wg <- ad$sample_group %||% list()
  if (length(wg)) {
    if (!is.list(gg) || !length(gg)) return(list(ok = FALSE, why = "读回里没有分组"))
    a <- sort(vapply(names(wg), function(k) paste0(k, "=", as.character(wg[[k]])), ""))
    b <- sort(vapply(names(gg), function(k) paste0(k, "=", as.character(gg[[k]])), ""))
    if (!identical(a, b)) return(list(ok = FALSE, why = "读回的分组与提交不一致"))
  }
  if (!identical(as.character(got$cells_total %||% ""),
                 as.character(ad$cells_total %||% "")))
    return(list(ok = FALSE, why = "读回的总细胞数与提交不一致"))
  list(ok = TRUE, why = "ok")
}

# 写回后读回核对：证据 md5、req、step、session 都要对上。
evidence_merge_verify <- function(readback, req_id, step, sid, expect_md5) {
  if (!is.list(readback)) return(list(ok = FALSE, why = "读回为空"))
  ce <- (readback$evidence %||% list())$cluster1_evidence
  if (!is.list(ce)) return(list(ok = FALSE, why = "读回里没有 cluster1_evidence"))
  if (!identical(trimws(as.character(ce$md5 %||% "")),
                 trimws(as.character(expect_md5 %||% ""))))
    return(list(ok = FALSE, why = "读回的 md5 与预期不一致"))
  if (!identical(trimws(as.character(readback$req_id %||% "")),
                 trimws(as.character(req_id %||% ""))))
    return(list(ok = FALSE, why = "读回的 req_id 与本次不一致"))
  sid0 <- trimws(as.character(readback$session_id %||% ""))
  sid1 <- trimws(as.character(sid %||% ""))
  if (nzchar(sid0) && nzchar(sid1) && !identical(sid0, sid1))
    return(list(ok = FALSE, why = "读回的 session 与本次不一致"))
  list(ok = TRUE, why = "ok")
}