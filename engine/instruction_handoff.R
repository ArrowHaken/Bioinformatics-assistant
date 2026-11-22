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
# 同一个属主、同一条分析记录、同一个请求号、同一段正文 —— 那就是同一件事。
#   为什么要能找回来：补发（retryDue）走的是 prepare，而 prepare 以前每次都
#   新建一个 instruction_id。正文里写着这个号，于是"同一条指令"每次重试都变成
#   一段新正文、一条新队列项；下一次补发又生成新号…… 页面每刷新一次就多一条，
#   会话里被反复注入同一件事（2026-11-22 实测：8 分钟内攒出 6 条，req_id 全相同）。
#   把已存在的包找回来复用，队列项就会命中合并，条数不再自增。
instruction_existing <- function(app_dir,owner,session_id,req_id,purpose,text) {
  root <- instruction_root(app_dir,owner)
  if (!dir.exists(root)) return(NULL)
  dirs <- list.dirs(root, recursive = FALSE, full.names = FALSE)
  if (!length(dirs)) return(NULL)
  want <- tryCatch(digest::digest(as.character(text),algo='sha256',serialize=FALSE),
                   error=function(e) NULL)
  if (is.null(want)) return(NULL)
  for (id in rev(dirs)) {
    d <- file.path(root,id)
    m <- tryCatch(sess_read_json(file.path(d,'manifest.json')),error=function(e) NULL)
    if (is.null(m)) next
    if (!identical(as.character(m$owner_key %||% ''),as.character(owner))) next
    if (!identical(as.character(m$session_id %||% ''),as.character(session_id))) next
    if (!identical(as.character(m$req_id %||% ''),as.character(req_id))) next
    if (!identical(as.character(m$purpose %||% 'execute'),as.character(purpose))) next
    got <- tryCatch(digest::digest(as.character(m$text %||% ''),algo='sha256',serialize=FALSE),
                    error=function(e) NULL)
    if (is.null(got) || !identical(got,want)) next
    fp <- file.path(d,'instruction.txt'); cp <- file.path(d,'context.json')
    if (!file.exists(fp) || !file.exists(cp)) next
    return(list(instruction_id=id,
                path=normalizePath(fp,winslash='/',mustWork=TRUE),
                context_path=normalizePath(cp,winslash='/',mustWork=TRUE)))
  }
  NULL
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
  # 只读语义只有一种：query 与 observe 同待遇（2026-11-13）。
  #   observe 是"作业已入队，这条只是通知"，不是新任务。
  #   它不需要请求绑定来证明"这是新的一次执行"，因为没有执行发生。
  #   仍要保留 session 归属校验：读进度必须是对这份分析有权限的人。
  read_only <- isTRUE(purpose %in% c('query','observe'))
  if (!read_only && !nzchar(rq)) return(incomplete('请求绑定'))
  b$session_id <- sid; b$req_id <- rq; b$purpose <- purpose
  # 补发路径也带上按钮名。
  #   为什么必须在这里补：直接注入走的是 app.js 的 injectToSession，那里已经加过标题；
  #   而补发走队列（item_id）恢复全文，正文是当初入队时的内部指针，
  #   人看到就是一段不知道在说什么的话。origin 在入队时已存下，这里重建标题。
  origin <- sess_chr(it$origin)
  if (nzchar(origin) && !startsWith(as.character(text), "【本次操作】"))
    text <- paste0("【本次操作】", origin, "\n", as.character(text))
  b$text <- text
  b$instruction_id <- if(nzchar(id)) id else NULL
  list(ok=TRUE,body=b)
}
