# Registered attempts are independent of the mutable profile/archive projection.
attempt_valid <- function(x) is.character(x) && length(x)==1L && !is.na(x) && grepl('^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$',x)
attempt_dir <- function(app_dir,sid,rq) {
  if (!attempt_valid(sid) || !attempt_valid(rq)) stop('invalid attempt binding')
  file.path(sess_dir(app_dir,sid),'attempts',rq)
}
attempt_register <- function(app_dir,sid,rq,origin='commit') {
  if (!attempt_valid(sid)) return(list(ok=FALSE,error='invalid session id'))
  if (!attempt_valid(rq)) return(list(ok=FALSE,error='invalid request id'))
  if (is.null(sess_find(app_dir,sid))) return(list(ok=FALSE,error='session not found'))
  d <- attempt_dir(app_dir,sid,rq); f <- file.path(d,'attempt.json')
  if (file.exists(f)) {
    actual <- sess_read_json(f)
    if (!is.null(actual) && identical(actual$session_id,sid) && identical(actual$req_id,rq))
      return(list(ok=TRUE))
    return(list(ok=FALSE,error='existing attempt binding mismatch'))
  }
  dir.create(d,recursive=TRUE,showWarnings=FALSE,mode='0700')
  if (!dir.exists(d)) return(list(ok=FALSE,error='attempt directory creation failed'))
  x <- list(session_id=sid,req_id=rq,origin=origin,registered_at=format(Sys.time(),'%Y-%m-%dT%H:%M:%S'))
  tmp <- tempfile('.register-', tmpdir=d)
  on.exit(unlink(tmp),add=TRUE)
  sess_write_json(tmp,x); Sys.chmod(tmp,'0600')
  # Hard link commits without replacing an already registered immutable fact.
  linked <- file.link(tmp,f)
  actual <- sess_read_json(f)
  if (!is.null(actual) && identical(actual$session_id,sid) && identical(actual$req_id,rq))
    return(list(ok=TRUE))
  list(ok=FALSE,error=if (isTRUE(linked)) 'attempt readback mismatch' else 'attempt file commit failed')
}
attempt_migrate <- function(app_dir,sid) {
  # Only persisted server bindings may migrate; never register a client supplied req.
  s <- sess_find(app_dir,sid); if (is.null(s)) return(invisible(FALSE))
  p <- sess_profile(app_dir,sid)
  refs <- c(list(p$pending_req,p$pending_identify_req,p$pending_step_req,s$identify$req_id),
            lapply(s$steps %||% list(),function(x)x$req_id))
  for (rq in refs) if (attempt_valid(rq)) attempt_register(app_dir,sid,rq,'trusted_legacy')
  invisible(TRUE)
}
attempt_has <- function(app_dir,sid,rq) {
  if (!attempt_valid(sid) || !attempt_valid(rq)) return(FALSE)
  attempt_migrate(app_dir,sid)
  x <- sess_read_json(file.path(attempt_dir(app_dir,sid,rq),'attempt.json'))
  !is.null(x) && identical(x$session_id,sid) && identical(x$req_id,rq)
}
attempt_selected <- function(app_dir,sid,rq,slot='step') {
  # 槽位要分开读（2026-11-12）：识别回写走 identify 槽的头，
  #   还按步骤头比的话，识别结果一到就被判成"不是这一次的"，
  #   于是 persisted 但 selected=false，记录里那句结论永远写不进去。
  sch <- if (exists('sess_pending_slot',mode='function')) sess_pending_slot(slot) else 'step'
  hn  <- if (exists('attempt_head_name',mode='function')) attempt_head_name(sch) else 'attempt-head.json'
  head <- sess_read_json(file.path(sess_dir(app_dir,sid),hn))
  pend <- if (exists('sess_get_pending',mode='function')) sess_get_pending(app_dir,sid,slot=sch) else NULL
  identical(sess_chr(head$req_id %||% pend),sess_chr(rq))
}
# 一步结果的取值顺序是显式的，不靠 mtime 猜真假：
#   1) 有效恢复记录（旧 terminal 的 sha 对得上）且其新终态槽位已存在 → 用槽位；
#   2) 有效恢复记录存在 → 用恢复记录（运行中）；
#   3) 旧 terminal；
#   4) 普通投影。
#   为什么必须显式：恢复之后再写 running 心跳时，普通投影会变新；
#   若按 mtime 择优，就会把旧 failed 复活。
attempt_base <- function(d, key) file.path(d, key)

attempt_value <- function(app_dir, sid, rq, key) {
  d <- file.path(attempt_dir(app_dir,sid,rq),'results')
  if (startsWith(key,'step--')) {
    stp <- substring(key, 7)
    base <- attempt_base(d, key)
    if (exists('recover_read_recovery', mode='function')) {
      rec <- tryCatch(recover_read_recovery(base), error=function(e) NULL)
      if (!is.null(rec)) {
        slot <- tryCatch(recover_slot_path(base, rec), error=function(e) "")
        if (nzchar(slot) && file.exists(slot)) {
          v <- sess_read_json(slot); if (!is.null(v)) return(v)
        }
        # 恢复之后的运行心跳写在新一代文件里，原始两份文件不动。
        run_f <- sub('\\.terminal-r-', '.running-r-', slot)
        if (nzchar(run_f) && file.exists(run_f)) {
          v <- sess_read_json(run_f); if (!is.null(v)) return(v)
        }
        return(rec)
      }
    }
    term <- paste0(base,'.terminal.json')
    if (file.exists(term)) { v <- sess_read_json(term); if (!is.null(v)) return(v) }
    reg <- paste0(base,'.json')
    if (file.exists(reg)) { v <- sess_read_json(reg); if (!is.null(v)) return(v) }
    return(NULL)
  }
  term <- file.path(d, paste0(key,'.terminal.json'))
  if (file.exists(term)) { v <- sess_read_json(term); if (!is.null(v)) return(v) }
  reg <- file.path(d, paste0(key,'.json'))
  if (file.exists(reg)) sess_read_json(reg) else NULL
}

# 只从真实产物文件名推 key：terminal 与 recovered 归到它们所属的那一代，
# 代际槽位（terminal-r-*）不作为独立步骤出现。
attempt_keys <- function(d) {
  fs <- list.files(d, pattern='\\.json$', full.names=FALSE)
  keys <- character(0)
  for (f in fs) {
    k <- sub('\\.json$','',f)
    if (grepl('\\.terminal$', k)) k <- sub('\\.terminal$','',k)
    else if (grepl('\\.recovered$', k)) k <- sub('\\.recovered$','',k)
    else if (grepl('\\.terminal-r-[0-9a-f]+$', k)) next
    else if (grepl('\\.running-r-[0-9a-f]+$', k)) next
    keys <- c(keys, k)
  }
  unique(keys)
}

attempt_results <- function(app_dir,sid,rq) {
  d <- file.path(attempt_dir(app_dir,sid,rq),'results')
  x <- sess_read_json(file.path(attempt_dir(app_dir,sid,rq),'result.json')) %||% list()
  for (key in attempt_keys(d)) {
    v <- attempt_value(app_dir, sid, rq, key); if (is.null(v)) next
    if (startsWith(key,'step--')) {
      x$steps <- x$steps %||% list(); x$steps[[substring(key,7)]] <- v
    } else x[[key]] <- v
  }
  x
}
attempt_save <- function(app_dir,sid,rq,kind,value,step=NULL) {
  if (!attempt_has(app_dir,sid,rq) || !attempt_valid(kind) || (!is.null(step) && !attempt_valid(step))) stop('invalid result binding')
  d <- file.path(attempt_dir(app_dir,sid,rq),'results'); dir.create(d,showWarnings=FALSE,mode='0700')
  key <- if (identical(kind,'step')) paste0('step--',step) else kind
  f <- file.path(d,paste0(key,'.json'))
  terminal <- file.path(d,paste0(key,'.terminal.json'))
  # 已被纠正的那一代：运行态写入落到新一代文件，
  #   原始 step--<step>.json 与 .terminal.json 两份 hash 必须保持不变 ——
  #   它们是这次故障的原始凭据，改动等于销毁证据。
  rec_slot <- ''
  if (kind=='step' && exists('recover_new_slot', mode='function'))
    rec_slot <- tryCatch(recover_new_slot(app_dir,sid,rq,step), error=function(e) '')
  run_f <- f
  if (nzchar(rec_slot)) run_f <- sub('\\.terminal-r-', '.running-r-', rec_slot)
  old <- attempt_value(app_dir,sid,rq,key)
  if (is.null(old)) { prev <- attempt_results(app_dir,sid,rq); old <- if (kind=='step') prev$steps[[step]] else prev[[kind]] }
  if (kind=='step' && sess_chr(old$status) %in% c('done','failed','stopped')) return(old)
  tmp <- tempfile('.result-',tmpdir=d); on.exit(unlink(tmp),add=TRUE)
  sess_write_json(tmp,value); Sys.chmod(tmp,'0600')
  if (kind=='step' && sess_chr(value$status) %in% c('done','failed','stopped')) {
    # No lock/TTL: first terminal is an immutable atomic hard-link fact.
    # 已被纠正的那一代：新终态写进恢复记录指定的槽位，
    #   旧 terminal 保持不可覆盖；槽位同样只用 file.link，第一次写入即定型。
    slot <- rec_slot
    if (nzchar(slot)) {
      suppressWarnings(file.link(tmp, slot))
      rb <- sess_read_json(slot)
      if (is.null(rb)) stop('recovered terminal commit failed')
      return(rb)
    }
    suppressWarnings(file.link(tmp,terminal))
    rb <- sess_read_json(terminal)
    if (is.null(rb)) stop('terminal commit failed')
    # Result aggregation reads terminal even if this process dies now.
    return(rb)
  }
  if (!file.rename(tmp,run_f)) stop('result commit failed')
  if (kind=='step') { cv <- attempt_value(app_dir,sid,rq,key); if (!is.null(cv)) return(cv) }
  rb <- sess_read_json(f)
  expected <- jsonlite::fromJSON(jsonlite::toJSON(value,auto_unbox=TRUE,null='null',digits=NA),simplifyVector=FALSE)
  if (!isTRUE(all.equal(expected,rb,check.attributes=FALSE))) stop('result readback mismatch')
  rb
}
# Preparation never acquires a shared-workroot lock. Every admitted attempt writes
# ONLY its derived output namespace, never legacy shared artifacts or inputs.
attempt_claim_output <- function(app_dir,sid,rq) {
  s <- sess_find(app_dir,sid); base <- sess_chr(s$run_dir)
  key <- digest::digest(paste(sid,rq,sep='\n'),algo='sha256',serialize=FALSE)
  list(ok=TRUE,mode='isolated',output_directory=if(nzchar(base)) paste0(sub('/+$','',base),'/.pa-attempts/',key) else NULL,
       shared_write_allowed=FALSE,legacy_remote_controlled=FALSE)
}
attempt_release_output <- function(...) invisible(TRUE)
# Run the existing verifier against a private SQLite snapshot and private
# filesystem state. Its DB projections/rework writes cannot touch live latest.
attempt_judge_history <- function(con,app_dir,owner,b,runDir,sid) {
  root <- tempfile('verify-',tmpdir=attempt_dir(app_dir,sid,b$req_id))
  dir.create(root,mode='0700'); dir.create(file.path(root,'state'),mode='0700')
  if (!file.symlink(file.path(app_dir,'engine'),file.path(root,'engine'))) stop('verifier engine unavailable')
  dst <- DBI::dbConnect(RSQLite::SQLite(),file.path(root,'state','verification.sqlite'))
  on.exit(DBI::dbDisconnect(dst),add=TRUE)
  RSQLite::sqliteCopyDatabase(con,dst)
  judge <- db_api_report_step(dst,root,owner,b,runDir,sid)
  attempt_save(app_dir,sid,b$req_id,paste0('verification-',b$step),judge)
  judge
}
