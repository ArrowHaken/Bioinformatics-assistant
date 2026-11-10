root <- normalizePath('.')
source('engine/db.R'); source('engine/sessions.R'); source('engine/dataset_profile.R'); source('engine/orchestrator.R'); source('engine/stage_reviewer.R')
a <- tempfile('env-isolation-'); dir.create(a); dir.create(file.path(a,'state')); dir.create(file.path(a,'engine'))
file.copy('engine/flow_spec.R',file.path(a,'engine','flow_spec.R'))
con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:'); db_init(con)
cat('schema sessions:',paste(DBI::dbListFields(con,'sessions'),collapse=','),'\n')
mk <- function(id,vid,result=NULL) {
 s <- list(id=id,identify=if(is.null(result)) NULL else list(result=result))
 st <- sess_read(a); st$sessions <- c(st$sessions,list(s)); st$current <- id; sess_write(a,st)
 DBI::dbExecute(con,'INSERT INTO sessions(session_id,owner_key,version_id,created_at) VALUES(?,?,?,?)',list(id,'local:default',vid,'now'))
}
mk('A','vA',list(marker='A',items=list(list(id='pkg',ok=TRUE))))
mk('B','vB',list(marker='B',items=list(list(id='pkg',ok=FALSE))))
mk('new','vNew')
sess_write_json(file.path(a,'state','remote_env.json'),list(marker='GLOBAL',items=list(list(id='pkg',ok=TRUE))))
flow_run_order_engine <- function(...) 'merge'
seen <- NULL
runnable_chain <- function(env,profile) { seen <<- env; list(verdict='unknown',message='captured') }
fails <- 0L
check <- function(label,expr) { ok <- isTRUE(expr); cat(if(ok) 'PASS' else 'FAIL',label,'\n'); if(!ok) fails <<- fails+1L }
for (pair in list(c('vA','A'),c('vB','B'),c('vNew',''))) {
 orch_next(con,a,pair[1]); check(paste('orch own',pair[1]),identical(seen$marker %||% '',pair[2]))
}
orch_next(con,a,'vA',env=list(marker='explicit')); check('explicit env retained',identical(seen$marker,'explicit'))
check('review own A',identical(sr_env_idx(a,'A')$pkg$ok,TRUE))
check('review own B',identical(sr_env_idx(a,'B')$pkg$ok,FALSE))
check('review absent new',is.null(sr_env_idx(a,'new')))
source('engine/db_api.R'); DB_READY <- TRUE
qget <- function(q,key) q[[key]]
api <- function(sid) db_api_handle(con,a,'/api/dataset/profile',list(method='GET'),list(session=sid),function() list(),NULL,list(key='local:default'))
# Exercise the actual profile route rather than its helper alone.
sess_set_identify(a,'A',list(samples=list(names=list('A1','A2'))))
sess_set_identify(a,'B',list(samples=list(names=list('B1'))))
check('profile A samples',identical(api('A')$body$profile$sample_count,2L))
check('profile B samples',identical(api('B')$body$profile$sample_count,1L))
check('profile new absent',is.na(api('new')$body$profile$sample_count))
check('unknown version absent',is.null(dp_record_env(a,con=con,version_id='unknown')))
check('empty session absent',is.null(dp_record_env(a,'')))
source('engine/state_paths.R')
part <- file.path(a,'state','users','owner:B'); dir.create(part,recursive=TRUE)
sess_write_json(file.path(part,'sessions.json'),list(sessions=list(list(id='B',identify=list(result=list(marker='partition-B'))))))
DBI::dbExecute(con,"UPDATE sessions SET owner_key='owner:B' WHERE session_id='B'")
check('version owner partition',identical(dp_record_env(a,con=con,version_id='vB')$marker,'partition-B'))
check('session owner partition',identical(dp_record_env(a,'B',con=con)$marker,'partition-B'))
DBI::dbDisconnect(con)
quit(status=if(fails) 1 else 0)
