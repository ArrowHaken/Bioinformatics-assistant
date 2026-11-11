# Registered attempts are independent of the mutable profile/archive projection.
attempt_valid <- function(x) is.character(x) && length(x)==1L && !is.na(x) && grepl('^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$',x)
attempt_dir <- function(app_dir,sid,rq) {
  if (!attempt_valid(sid) || !attempt_valid(rq)) stop('invalid attempt binding')
  file.path(sess_dir(app_dir,sid),'attempts',rq)
}
attempt_register <- function(app_dir,sid,rq,origin='commit') {
  if (!attempt_valid(sid) || !attempt_valid(rq) || is.null(sess_find(app_dir,sid))) return(FALSE)
  d <- attempt_dir(app_dir,sid,rq); f <- file.path(d,'attempt.json')
  if (file.exists(f)) return(TRUE)
  dir.create(d,recursive=TRUE,showWarnings=FALSE,mode='0700')
  x <- list(session_id=sid,req_id=rq,origin=origin,registered_at=format(Sys.time(),'%Y-%m-%dT%H:%M:%S'))
  tmp <- tempfile('.register-', tmpdir=d)
  sess_write_json(tmp,x); Sys.chmod(tmp,'0600')
  # Hard link commits without replacing an already registered immutable fact.
  linked <- file.link(tmp,f); unlink(tmp)
  actual <- sess_read_json(f)
  !is.null(actual) && identical(actual$session_id,sid) && identical(actual$req_id,rq)
}
attempt_migrate <- function(app_dir,sid) {
  # Only persisted server bindings may migrate; never register a client supplied req.
  s <- sess_find(app_dir,sid); if (is.null(s)) return(invisible(FALSE))
  p <- sess_profile(app_dir,sid)
  refs <- c(list(p$pending_req,s$identify$req_id),lapply(s$steps %||% list(),function(x)x$req_id))
  for (rq in refs) if (attempt_valid(rq)) attempt_register(app_dir,sid,rq,'trusted_legacy')
  invisible(TRUE)
}
attempt_has <- function(app_dir,sid,rq) {
  if (!attempt_valid(sid) || !attempt_valid(rq)) return(FALSE)
  attempt_migrate(app_dir,sid)
  x <- sess_read_json(file.path(attempt_dir(app_dir,sid,rq),'attempt.json'))
  !is.null(x) && identical(x$session_id,sid) && identical(x$req_id,rq)
}
attempt_selected <- function(app_dir,sid,rq) {
  head <- sess_read_json(file.path(sess_dir(app_dir,sid),'attempt-head.json'))
  identical(sess_chr(head$req_id %||% sess_get_pending(app_dir,sid)),sess_chr(rq))
}
attempt_results <- function(app_dir,sid,rq) {
  d <- file.path(attempt_dir(app_dir,sid,rq),'results')
  x <- sess_read_json(file.path(attempt_dir(app_dir,sid,rq),'result.json')) %||% list()
  for (f in list.files(d,pattern='\\.json$',full.names=TRUE)) {
    if (grepl('\\.terminal\\.json$',f)) next
    terminal <- sub('\\.json$','.terminal.json',f)
    v <- sess_read_json(if(file.exists(terminal)) terminal else f); if (is.null(v)) next
    key <- sub('\\.json$','',basename(f))
    if (startsWith(key,'step--')) { x$steps <- x$steps %||% list(); x$steps[[substring(key,7)]] <- v }
    else x[[key]] <- v
  }
  # A crash may happen after terminal commit but before the ordinary file rename.
  for(f in list.files(d,pattern='\\.terminal\\.json$',full.names=TRUE)) {
    key <- sub('\\.terminal\\.json$','',basename(f)); v <- sess_read_json(f)
    if(startsWith(key,'step--')) { x$steps <- x$steps %||% list(); x$steps[[substring(key,7)]] <- v }
  }
  x
}
attempt_save <- function(app_dir,sid,rq,kind,value,step=NULL) {
  if (!attempt_has(app_dir,sid,rq) || !attempt_valid(kind) || (!is.null(step) && !attempt_valid(step))) stop('invalid result binding')
  d <- file.path(attempt_dir(app_dir,sid,rq),'results'); dir.create(d,showWarnings=FALSE,mode='0700')
  key <- if (identical(kind,'step')) paste0('step--',step) else kind
  f <- file.path(d,paste0(key,'.json'))
  terminal <- file.path(d,paste0(key,'.terminal.json'))
  old <- sess_read_json(if(file.exists(terminal)) terminal else f)
  if (is.null(old)) { prev <- attempt_results(app_dir,sid,rq); old <- if (kind=='step') prev$steps[[step]] else prev[[kind]] }
  if (kind=='step' && sess_chr(old$status) %in% c('done','failed','stopped')) return(old)
  tmp <- tempfile('.result-',tmpdir=d); on.exit(unlink(tmp),add=TRUE)
  sess_write_json(tmp,value); Sys.chmod(tmp,'0600')
  if (kind=='step' && sess_chr(value$status) %in% c('done','failed','stopped')) {
    # No lock/TTL: first terminal is an immutable atomic hard-link fact.
    suppressWarnings(file.link(tmp,terminal))
    rb <- sess_read_json(terminal)
    if (is.null(rb)) stop('terminal commit failed')
    # Result aggregation reads terminal even if this process dies now.
    return(rb)
  }
  if (!file.rename(tmp,f)) stop('result commit failed')
  if (kind=='step' && file.exists(terminal)) return(sess_read_json(terminal))
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
