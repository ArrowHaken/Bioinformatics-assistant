# =====================================================================
#  pack_binding.R -- pack 那个 ZIP 的归属判定
#
#  为什么需要单独一层
#    旧判据是"产物 mtime 必须晚于本题次开始时间"。这条判据跨了两台机器：
#      started_at 由应用主机写，mtime 由计算节点写。
#    实测 2026-11-15 00:44：node02 时钟比提交主机慢十几秒，
#      zip 明明是这次打的（qacct end_time 与 mtime 同一秒 00:43:51），
#      却因为"早于 00:44:00"被判 stale_artifact，整次回写被拒。
#
#    放宽时间窗口是错的解法：窗口一放宽，上一轮遗留的同名 zip 也能进来。
#
#  现在的判据（全部必须成立，缺一条即拒）
#    1. 只对 pack 步骤、且 rel 恰好是打包产物本身生效
#    2. 回写必须带本次作业的 qacct 事实：jobname / jobnumber / hostname /
#       exit_status / failed / start_epoch / end_epoch，以及 ZIP 的实算事实
#    3. **数字作业号必须等于本次 exec 上登记的那个唯一号**（qsub 回执里取到的）。
#       只比作业名不算绑定：作业名按"步骤+时分秒"生成，同一分钟内重试会撞名。
#       因此 jobname 只作辅助核对，主键是 jobnumber。
#    4. exit_status == 0 且 failed == 0
#    5. ZIP 的 mtime_epoch 落在该作业 [start_epoch, end_epoch] 闭区间，
#       三个数都取自同一台节点的同一个时钟，不跨主机
#    6. ZIP 的 rel / bytes / md5 三方一致：
#       回写产物条目、节点实算事实、以及 zip_structure 的 md5 必须逐字相同；
#       产物条目的 mtime_epoch 若给出，必须与节点实算的 mtime 相同
#
#  不满足就返回具体原因，由调用方拒收；不拿"大概"放行。
# =====================================================================

PACK_ZIP_REL <- "11.report/analysis_results.zip"

pb_epoch <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (length(v) != 1L || is.na(v) || v <= 0) return(NA_real_)
  v
}

pb_md5 <- function(x) {
  m <- tolower(trimws(as.character(x %||% "")))
  if (grepl("^[0-9a-f]{32}$", m)) m else ""
}

# 主判定。返回 list(ok, code, why)。
#   art            回写里那一条产物（rel, bytes, md5, mtime_epoch）
#   sge            回写里的 sge_job 事实
#   zipinfo        回写里的 pack_zip 事实（节点实算）
#   exec_job       本次 exec 记录上的作业名（可重复，仅辅助核对）
#   exec_jobnum    本次 exec 记录上的唯一数字作业号（主键）
#   zip_structure  verify_delivery 产出的 zip 结构证据（含 md5）
pack_zip_binding <- function(rel, art, sge, zipinfo, exec_job, step = "pack",
                             exec_jobnum = "", zip_structure = NULL) {
  rel <- gsub("\\\\", "/", as.character(rel %||% ""))
  step <- as.character(step %||% "")

  if (!identical(step, "pack")) return(list(ok = FALSE, code = "not_pack", why = "非 pack 步骤"))
  if (!identical(rel, PACK_ZIP_REL))
    return(list(ok = FALSE, code = "not_zip_rel", why = "不是打包产物本身"))

  if (is.null(sge) || !is.list(sge))
    return(list(ok = FALSE, code = "no_sge", why = "回写没带本次作业的记账事实"))
  if (is.null(zipinfo) || !is.list(zipinfo))
    return(list(ok = FALSE, code = "no_zip_facts", why = "回写没带 ZIP 的实算事实"))

  # 节点实算事实里的 rel 必须是这个产物本身
  zi_rel <- gsub("\\\\", "/", as.character(zipinfo$rel %||% ""))
  if (!identical(zi_rel, PACK_ZIP_REL))
    return(list(ok = FALSE, code = "zip_rel_mismatch",
                why = paste0("实算事实指向的不是打包产物：", zi_rel)))

  jobname <- trimws(as.character(sge$jobname %||% ""))
  jobnum <- trimws(as.character(sge$jobnumber %||% ""))
  host <- trimws(as.character(sge$hostname %||% ""))
  es <- trimws(as.character(sge$exit_status %||% ""))
  fa <- trimws(as.character(sge$failed %||% ""))
  se <- pb_epoch(sge$start_epoch)
  ee <- pb_epoch(sge$end_epoch)

  if (!nzchar(jobname)) return(list(ok = FALSE, code = "no_jobname", why = "缺作业名"))
  if (!grepl("^[0-9]+$", jobnum))
    return(list(ok = FALSE, code = "bad_jobnumber", why = "作业号不是数字"))
  if (!nzchar(host)) return(list(ok = FALSE, code = "no_host", why = "缺作业所在节点"))

  # ---- 唯一作业号绑定（主键）----
  want_num <- trimws(as.character(exec_jobnum %||% ""))
  if (!grepl("^[0-9]+$", want_num))
    return(list(ok = FALSE, code = "exec_jobnum_missing",
                why = "本次执行记录上没有唯一数字作业号，无法绑定"))
  if (!identical(jobnum, want_num))
    return(list(ok = FALSE, code = "jobnumber_mismatch",
                why = sprintf("作业号 %s 与本次登记的 %s 不一致", jobnum, want_num)))

  # 作业名只作辅助核对：同名不算绑定，不同名则说明串了
  want_name <- trimws(as.character(exec_job %||% ""))
  if (nzchar(want_name) && !identical(jobname, want_name))
    return(list(ok = FALSE, code = "job_mismatch",
                why = sprintf("作业名 %s 与本次派工 %s 不一致", jobname, want_name)))

  if (!identical(es, "0"))
    return(list(ok = FALSE, code = "exit_nonzero", why = paste0("exit_status=", es)))
  if (!identical(fa, "0"))
    return(list(ok = FALSE, code = "failed_nonzero", why = paste0("failed=", fa)))

  if (is.na(se) || is.na(ee))
    return(list(ok = FALSE, code = "no_window", why = "作业起止时间换算不出 epoch"))
  if (ee < se)
    return(list(ok = FALSE, code = "bad_window", why = "作业结束早于开始"))
  mz <- pb_epoch(zipinfo$mtime_epoch)
  if (is.na(mz))
    return(list(ok = FALSE, code = "no_zip_mtime", why = "没带 ZIP 的精确 mtime"))
  if (mz < se || mz > ee)
    return(list(ok = FALSE, code = "mtime_outside",
                why = sprintf("ZIP mtime %d 不在作业窗口 [%d, %d] 内", mz, se, ee)))

  # ---- 三方一致：产物条目 / 节点实算 / zip_structure ----
  zb <- suppressWarnings(as.numeric(zipinfo$bytes %||% NA))
  ab <- suppressWarnings(as.numeric(art$bytes %||% NA))
  if (!is.finite(zb) || zb <= 0)
    return(list(ok = FALSE, code = "bad_zip_bytes", why = "节点实算字节数不合法"))
  if (!is.finite(ab) || ab != zb)
    return(list(ok = FALSE, code = "bytes_mismatch",
                why = sprintf("回写字节数 %s 与节点实算 %s 不一致", ab, zb)))

  zm <- pb_md5(zipinfo$md5)
  if (!nzchar(zm)) return(list(ok = FALSE, code = "no_zip_md5", why = "节点没算出 md5"))
  am <- pb_md5(if (is.list(art)) art$md5 else "")
  if (!identical(am, zm))
    return(list(ok = FALSE, code = "md5_mismatch",
                why = sprintf("回写 md5 %s 与节点实算 %s 不一致",
                              substr(am, 1, 12), substr(zm, 1, 12))))

  # 产物自报的 mtime 若给出，必须与实算一致；不给不算错（旧回写没有这个字段）
  amt <- pb_epoch(if (is.list(art)) art$mtime_epoch else NULL)
  if (!is.na(amt) && amt != mz)
    return(list(ok = FALSE, code = "mtime_mismatch",
                why = sprintf("产物 mtime %d 与实算 %d 不一致", amt, mz)))

  # zip_structure 的 md5 也必须与上面两者一致
  # 结构证据必须真的带上来：它是"包内每一份文件与当前源文件逐一相同"的凭证。
  #   曾经写成"给了才校验"，于是 zip_structure 缺失时三方对齐被静默跳过 ——
  #   那正好是最该拦的一种坏包（内容没核过就当成交付物）。
  zs <- zip_structure
  if (is.null(zs) || !is.list(zs))
    return(list(ok = FALSE, code = "no_struct",
                why = "回写没带 ZIP 结构证据，无法确认包内文件与源文件一致"))
  zsm <- pb_md5(zs$md5)
  if (!nzchar(zsm))
    return(list(ok = FALSE, code = "no_struct_md5",
                why = "结构证据里没有 ZIP 的 md5，无法三方对齐"))
  if (!identical(zsm, zm))
    return(list(ok = FALSE, code = "struct_md5_mismatch",
                why = sprintf("结构证据 md5 %s 与实算 %s 不一致",
                              substr(zsm, 1, 12), substr(zm, 1, 12))))
  # 结构自检本身也必须是通过的：只对齐哈希、不看结论，等于把结论藏起来。
  if (identical(zs$ok, FALSE))
    return(list(ok = FALSE, code = "struct_failed",
                why = paste0("结构自检未通过：", as.character(zs$why %||% ""))))

  list(ok = TRUE, code = "bound",
       why = sprintf("已绑定作业 #%s（%s，%s），窗口 [%d,%d] 内含 mtime %d",
                     jobnum, jobname, host, se, ee, mz),
       jobname = jobname, jobnumber = jobnum, host = host,
       start_epoch = se, end_epoch = ee, mtime_epoch = mz,
       bytes = zb, md5 = zm)
}
