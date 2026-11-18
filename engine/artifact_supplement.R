# ---- 产物清单补录：唯一安全入口 ----------------------------------------
#   终态已验收后再回写，允许把本次新登记的产物补进记录；但补进去的条目会
#   成为下载与基线的背书，所以准入必须严格：
#     1) 归属：当前会话里这一步的记录必须属于本次请求号。旧请求不得改写
#        新任务同一步的结果 —— 同一步在不同 req 下是两次不同的运行。
#     2) 基线：可信来源只有「本次已验收回写体里的 artifacts[]」，那里有
#        每一条的 rel / md5 / bytes。不用 final_params 里的自报 hash 凑，
#        那些是脚本自己打印的，不能证明文件本身。
#     3) 已有条目：rel / md5 / bytes 三者都不能变。只比 md5 不够 —— 同 md5
#        但字节数不同说明清单被改过。
#     4) 新增条目：必须在基线里存在，且 md5 与 bytes 都一致；重复 rel、
#        空请求号一律拒绝。
#   单独成文件的原因：这些判定要能用真实反例与真实记录逐条测。
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}

# 可信基线：只认已验收回写体里的 artifacts[]（rel + md5 + bytes）。
artifact_bound_pairs <- function(verified_payload) {
  p <- verified_payload %||% list()
  out <- list()
  for (a in (p$artifacts %||% list())) {
    if (!is.list(a)) next
    r <- as.character(a$rel %||% a$path %||% "")
    m <- tolower(as.character(a$md5 %||% a$checksum %||% ""))
    b <- suppressWarnings(as.numeric(a$bytes %||% a$size %||% NA))
    if (nzchar(r) && grepl("^[0-9a-f]{32}$", m) && !is.na(b) && b > 0)
      out[[length(out) + 1L]] <- list(rel = r, md5 = m, bytes = b)
  }
  out
}

# 判定：能不能把 new 清单补进已验收记录。
artifact_supplement_decide <- function(old_arts, new_arts, bound,
                                       target_req, cur_req) {
  out <- function(ok, code = "", added = character(0), error = "")
    list(ok = ok, code = code, added = added, error = error)

  tr <- trimws(as.character(target_req %||% ""))
  cr <- trimws(as.character(cur_req %||% ""))
  if (!nzchar(tr) || !nzchar(cr))
    return(out(FALSE, "empty_req", error = "请求号为空，无法归属"))
  if (!identical(cr, tr))
    return(out(FALSE, "req_mismatch",
      error = "本次回写的请求号与该记录不一致，不能改写这一条的结果"))

  rels <- vapply(new_arts, function(a) as.character(a$rel %||% ""), "")
  if (!length(rels)) return(out(FALSE, "empty_list", error = "产物清单为空"))
  if (any(!nzchar(rels)))
    return(out(FALSE, "bad_rel", error = "产物清单里有空路径"))
  if (anyDuplicated(rels))
    return(out(FALSE, "duplicate_rel",
      error = paste0("产物清单里有重复路径：",
                     paste(unique(rels[duplicated(rels)]), collapse = ", "))))

  idx <- function(xs) {
    r <- vapply(xs, function(a) as.character(a$rel %||% ""), "")
    list(rel = r,
         md5 = setNames(tolower(vapply(xs, function(a)
           as.character(a$md5 %||% ""), "")), r),
         bytes = setNames(suppressWarnings(as.numeric(vapply(xs, function(a)
           as.character(a$bytes %||% NA), ""))), r))
  }
  nw <- idx(new_arts); od <- idx(old_arts %||% list())

  miss <- setdiff(od$rel, nw$rel)
  if (length(miss))
    return(out(FALSE, "artifact_missing",
      error = paste0("已验收产物在本次清单里缺失：", paste(miss, collapse = ", "))))

  sh <- intersect(od$rel, nw$rel)
  if (length(sh)) {
    if (!identical(unname(od$md5[sh]), unname(nw$md5[sh])))
      return(out(FALSE, "artifact_md5_changed",
        error = "已验收产物的 md5 在本次清单里变了"))
    if (!identical(as.numeric(unname(od$bytes[sh])), as.numeric(unname(nw$bytes[sh]))))
      return(out(FALSE, "artifact_bytes_changed",
        error = "已验收产物的字节数在本次清单里变了"))
  }

  added <- setdiff(nw$rel, od$rel)
  if (length(added)) {
    bnd <- vapply(bound %||% list(), function(x) as.character(x$rel %||% ""), "")
    for (r in added) {
      k <- match(r, bnd)
      if (is.na(k))
        return(out(FALSE, "new_not_verified",
          error = paste0("新增条目不在本次已验收清单里，不能进入基线：", r)))
      bb <- bound[[k]]
      if (!identical(tolower(as.character(bb$md5 %||% "")), nw$md5[[r]]))
        return(out(FALSE, "new_md5_mismatch",
          error = paste0("新增条目的 md5 与已验收清单不符：", r)))
      if (!identical(as.numeric(bb$bytes %||% NA), as.numeric(nw$bytes[[r]])))
        return(out(FALSE, "new_bytes_mismatch",
          error = paste0("新增条目的字节数与已验收清单不符：", r)))
    }
  }
  out(TRUE, "", added = added)
}

# 唯一入口：读当前会话记录 → 比归属 → 判定 → 写回并读回核对。
#   put_fn / get_fn 由调用方注入（分别是 remote_step_put / remote_step_get），
#   这样这段逻辑可以脱离进程直接测。
artifact_supplement_apply <- function(cur_req, new_arts, current_rec,
                                      verified_payload, put_fn, get_fn,
                                      now = NULL) {
  res <- function(ok, code = "", wrote = FALSE, added = character(0),
                  error = "", record = NULL)
    list(ok = ok, code = code, wrote = wrote, added = added,
         error = error, record = record)

  if (is.null(current_rec)) return(res(TRUE, "no_record"))
  if (!length(new_arts %||% list())) return(res(TRUE, "no_artifacts"))

  rec_req <- trimws(as.character(current_rec$req_id %||% ""))
  cr <- trimws(as.character(cur_req %||% ""))
  if (!nzchar(cr) || !identical(cr, rec_req))
    return(res(FALSE, "req_mismatch",
      error = "当前记录不属于本次请求号，不改写它的产物清单"))

  bound <- artifact_bound_pairs(verified_payload)
  if (!length(bound))
    return(res(FALSE, "no_baseline",
      error = "本次没有可用的已验收清单，不能补录"))

  d <- artifact_supplement_decide(current_rec$artifacts %||% list(), new_arts,
                                  bound, rec_req, cr)
  if (!isTRUE(d$ok)) return(res(FALSE, d$code, error = d$error))
  if (!length(d$added)) return(res(TRUE, "no_change"))

  r2 <- current_rec
  r2$artifacts <- new_arts
  r2$artifacts_supplemented_at <- now %||% format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  pok <- tryCatch({ put_fn(r2); TRUE }, error = function(e) FALSE)
  if (!isTRUE(pok))
    return(res(FALSE, "put_failed", error = "补录写回失败"))
  rb <- tryCatch(get_fn(), error = function(e) NULL)
  got <- vapply((rb %||% list())$artifacts %||% list(),
                function(a) as.character(a$rel %||% ""), "")
  if (!all(d$added %in% got))
    return(res(FALSE, "readback_mismatch", error = "补录后读回缺少新增条目"))
  res(TRUE, "supplemented", wrote = TRUE, added = d$added, record = rb)
}