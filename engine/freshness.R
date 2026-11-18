# 新鲜度豁免：只有一个窄口子
# ---------------------------------------------------------------------------
# 症状：pack 要压进 analysis_report.pptx.manifest.json，它由 ppt 生成，
#   mtime 必然早于 pack 开始；若按"必须晚于本题次开始"一判，整次打包会被拒。
#
# 为什么必须收窄到一条：
#   先把"同 session 所有已验证产物"整体豁免试过，那是错的 ——
#   本步骤的旧结果、无关步骤、历史版本都会自我背书，
#   旧 PPT 和旧 ZIP 能顶着新题次通过。这不是放宽一点，是把判据取消了。
#
# 现在的口径（缺一条都拒）：
#   · 只有 step == "pack" 才谈豁免
#   · 只有 rel 恰好是 PPT 的来源清单
#   · md5 必须是 32 位且与基线逐字相同
#   · 基线只取该 session 最新一条已验证 ppt 的产物清单
#   · 不接受本步骤旧产物、不从任意历史记录兜底、不跨 session

FRESHNESS_PPT_MANIFEST <- "11.report/analysis_report.pptx.manifest.json"

freshness_manifest_exempt <- function(rel, md5, step, sid, con = NULL) {
  rel <- as.character(rel %||% "")
  step <- as.character(step %||% "")
  sid <- as.character(sid %||% "")
  m <- tolower(trimws(as.character(md5 %||% "")))

  if (!identical(step, "pack")) return(FALSE)
  if (!identical(rel, FRESHNESS_PPT_MANIFEST)) return(FALSE)
  if (!nzchar(m) || nchar(m) != 32L) return(FALSE)
  if (is.null(con) || !nzchar(sid)) return(FALSE)

  row <- tryCatch(DBI::dbGetQuery(con, paste0(
    "SELECT payload FROM step_attempts WHERE step_id = 'ppt' AND status = 'verified' ",
    "AND json_extract(payload, '$.session_id') = ? ",
    "ORDER BY verified_at DESC LIMIT 1"), list(sid)),
    error = function(e) NULL)
  if (is.null(row) || !nrow(row)) return(FALSE)

  p <- tryCatch(jsonlite::fromJSON(row$payload[[1]], simplifyVector = FALSE),
                error = function(e) NULL)
  for (a in (p %||% list())$artifacts %||% list()) {
    if (identical(as.character(a$rel %||% ""), rel) &&
        identical(tolower(as.character(a$md5 %||% "")), m)) return(TRUE)
  }
  FALSE
}
