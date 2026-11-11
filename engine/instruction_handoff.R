# Immutable instruction packages live only under their authenticated owner's root.
instruction_truncated <- function(text) {
  !is.character(text) || length(text)!=1L || is.na(text) || !nzchar(text) ||
    grepl('已截断|内容截断|正文截断|后续内容省略|\\[truncated\\]|\\.\\.\\.\\s*$',text,ignore.case=TRUE)
}
instruction_root <- function(app_dir,owner) {
  file.path(app_dir,'state','users',digest::digest(owner,algo='sha256',serialize=FALSE),'instructions')
}
instruction_package <- function(app_dir,owner,id) {
  if (!attempt_valid(id)) return(NULL)
  sess_read_json(file.path(instruction_root(app_dir,owner),id,'manifest.json'))
}
instruction_source <- function(app_dir,b,items,owner,authorized) {
  incomplete <- function(missing) list(ok=FALSE,code='instruction_source_incomplete',stage='source',http_status=422,
    missing=as.list(missing),error=paste0('原完整输入无法恢复；缺少：',paste(missing,collapse='、')))
  mismatch <- function() list(ok=FALSE,code='instruction_item_binding_mismatch',stage='binding',http_status=409)
  denied <- function() list(ok=FALSE,code='instruction_not_owner',stage='authorization',http_status=403)
  if (is.null(b$item_id)) {
    if (instruction_truncated(b$text)) return(incomplete('完整正文'))
    return(list(ok=TRUE,body=b))
  }
  hits <- Filter(function(x) identical(sess_chr(x$id),sess_chr(b$item_id)),items)
  if (length(hits)!=1L) return(incomplete('原队列条目'))
  it <- hits[[1]]; sid <- sess_chr(it$analysis_id %||% it$meta$analysisId); rq <- sess_chr(it$req_id)
  if (!nzchar(sid) && nzchar(rq)) {
    matches <- Filter(function(s) attempt_has(app_dir,s$id,rq),sess_read(app_dir)$sessions)
    if (length(matches)==1L) sid <- sess_chr(matches[[1]]$id)
  }
  if (!nzchar(sid)) return(incomplete('服务器可信分析绑定'))
  if (!authorized(sid)) return(denied())
  if ((nzchar(sess_chr(b$session_id)) && !identical(b$session_id,sid)) ||
      (nzchar(sess_chr(b$req_id)) && !identical(b$req_id,rq))) return(mismatch())
  id <- sess_chr(it$instruction_id)
  if (nzchar(id)) {
    src <- instruction_package(app_dir,owner,id)
    if (is.null(src)) return(incomplete('精确 instruction_id 对应的完整包'))
    if (!identical(src$owner_key,owner) || !identical(src$session_id,sid) || !identical(sess_chr(src$req_id),rq) ||
        (nzchar(sess_chr(it$purpose)) && !identical(it$purpose,src$purpose))) return(mismatch())
    text <- src$text; purpose <- src$purpose
  } else {
    # Never recover a legacy truncated pointer from a mutable same-req latest slot.
    text <- it$full_text
    if (is.null(text) && isTRUE(it$text_complete)) text <- it$text
    purpose <- it$purpose %||% 'execute'
  }
  if (instruction_truncated(text)) return(incomplete('原完整正文（无可信精确源）'))
  if (!identical(purpose,'query') && !nzchar(rq)) return(incomplete('请求绑定'))
  b$session_id <- sid; b$req_id <- rq; b$text <- text; b$purpose <- purpose
  b$instruction_id <- if(nzchar(id)) id else NULL
  list(ok=TRUE,body=b)
}
