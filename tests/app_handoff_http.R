#!/usr/bin/env Rscript
# Actual server HTTP, isolated SQLite; fixed gateway response is injected, not live authentication.
run <- function() {
  e <- new.env(parent = globalenv())
  for (x in parse('serve.R')) if (is.call(x) && identical(x[[1]], as.name('<-')) &&
      is.call(x[[3]]) && identical(x[[3]][[1]], as.name('function'))) eval(x, e)
  for (f in c('sessions.R','db.R','agent_gate.R','db_api.R','app_handoff.R','platform_identity.R'))
    source(file.path('engine', f), local=e)
  root <- tempfile('handoff-http-', tmpdir='/tmp'); dir.create(root)
  e$APP_DIR <- root; e$STATE_DIR <- file.path(root,'state'); dir.create(e$STATE_DIR)
  e$STATE_FILE <- file.path(e$STATE_DIR,'state.json')
  e$DB_READY <- TRUE; e$HAS_SESSIONS <- TRUE; e$DB_CON <- e$db_connect(root); e$db_init(e$DB_CON)
  on.exit({if(DBI::dbIsValid(e$DB_CON)) DBI::dbDisconnect(e$DB_CON); unlink(root,recursive=TRUE)}, add=TRUE)
  DBI::dbExecute(e$DB_CON, "INSERT INTO sessions(session_id,owner_key,name,mode,created_at,updated_at) VALUES ('owned','viewer:fixture','owned','local',datetime('now'),datetime('now')),('foreign','viewer:other','foreign','local',datetime('now'),datetime('now'))")
  e$STATUS_TEXT <- c(`200`='OK',`400`='Bad Request',`403`='Forbidden',`503`='Service Unavailable')
  e$db_api_owner <- function(app_dir,req) {
    if (identical(req$headers[['x-fixture-auth']], 'agent')) return(list(key='viewer:fixture',kind='agent'))
    list(key='viewer:fixture',kind='viewer')
  }
  e$pa_handoff_config <- function() list(platform_origin='https://platform.example',app_origin='https://app.example',agent_uid='123')
  e$pa_platform_identity <- function(req) {
    ident <- list(authenticated=TRUE,viewer=list(id='fixture'),topic_id='topic1',agent_uid='123')
    mode <- req$headers[['x-fixture-identity']]
    if (identical(mode,'missing')) ident$topic_id <- NULL
    if (identical(mode,'other-topic')) ident$topic_id <- 'topic2'
    if (identical(mode,'other-agent')) ident$agent_uid <- '124'
    if (identical(mode,'other-viewer')) ident$viewer$id <- 'other'
    ident
  }
  e$IS_WIN <- FALSE; e$PORT <- sample(25000:50000,1); e$AUTOOPEN <- FALSE; e$say <- function(...) invisible(NULL)
  child <- parallel::mcparallel({e$DB_CON <- e$db_connect(root); e$main()},silent=TRUE)
  on.exit({tools::pskill(child$pid); parallel::mccollect(child,wait=FALSE)}, add=TRUE,after=FALSE)
  base <- paste0('http://127.0.0.1:',e$PORT)
  for(i in seq_len(40)) {ready <- tryCatch({curl::curl_fetch_memory(paste0(base,'/api/ping'),curl::new_handle(timeout=1,connecttimeout=1));TRUE},error=function(x) FALSE);if(ready)break;Sys.sleep(.1)}
  if (!ready) { print(parallel::mccollect(child,wait=FALSE)); stop('fixture not ready') }
  env <- list(protocol='catsco.app-handoff.v1',type='handoff',nonce='nonce1',request_id='request1',topic_id='topic1',agent_uid='123',action='initialize',parameters=list(session_id='owned'),expires_at=floor(as.numeric(Sys.time())*1000)+60000)
  request <- function(body=env, origin='https://app.example', identity='valid', auth='viewer') {
    h <- curl::new_handle(timeout=5,customrequest='POST',postfields=as.character(jsonlite::toJSON(body,auto_unbox=TRUE)))
    curl::handle_setheaders(h,Origin=origin,'x-fixture-auth'=auth,'x-fixture-identity'=identity,'Content-Type'='application/json')
    r <- curl::curl_fetch_memory(paste0(base,'/api/app-handoff/initialize'),h)
    list(status=r$status_code,body=jsonlite::fromJSON(rawToChar(r$content),simplifyVector=FALSE))
  }
  good <- request();stopifnot(good$status==200,good$body$session_id=='owned',identical(good$body$supports_run,FALSE))
  for(mode in c('missing','other-topic','other-agent','other-viewer')) stopifnot(request(identity=mode)$status==403)
  stopifnot(request(auth='agent')$status==403, request(origin='https://platform.example')$status==403)
  bad <- env;bad$parameters$session_id <- 'foreign';stopifnot(request(bad)$status==403)
  bad <- env;bad$action <- 'run';stopifnot(request(bad)$status==403)
  bad <- env;bad$expires_at <- floor(as.numeric(Sys.time())*1000)-1;stopifnot(request(bad)$status==403)
  stopifnot(!file.exists(file.path(root,'state','current.json')))
  cat('PASS real HTTP handoff routes; injected trusted gateway only; owner/topic/agent/origin/run/expiry denied before mutation\n')
}
run()
