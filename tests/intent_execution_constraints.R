#!/usr/bin/env Rscript
source('engine/intent_bridge.R')
root <- tempfile('intent-constraints-',tmpdir='/tmp');dir.create(root)
sid <- 's-fixture'; d <- file.path(root,'state','sessions',sid);dir.create(d,recursive=TRUE)
check <- function(intent, fields) {
 writeLines(as.character(jsonlite::toJSON(list(intent=intent),auto_unbox=TRUE,null='null')),file.path(d,'profile.json'))
 result <- intent_execution_gate(root,sid)
 stopifnot(identical(sort(as.character(unlist(result$fields,use.names=FALSE))),sort(fields)), identical(result$ok,length(fields)==0L))
}
check(list(),character())
check(list(scope=list(run_until='08')), 'scope.run_until')
check(list(scope=list(annotation_gate=TRUE)), 'scope.annotation_gate')
# 2026-11-18：改 cell 与合并相邻群仍算「已保存的要求」，但不再是要人来解的计划，
# 所以不再列入需要转译的字段（它们随指令下发，由执行端判断）。
check(list(reference=list(modified_cells=list(3,5))), character())
check(list(annotation=list(merge_adjacent=FALSE)), character())
check(list(annotation=list(merge_adjacent=TRUE)), character())
check(list(scope=list(annotation_gate=FALSE)),character())
writeLines('{broken',file.path(d,'profile.json'))
r <- intent_execution_gate(root,sid);stopifnot(!r$ok,r$code=='intent_profile_unreadable')
# Use the shared run_enqueue implementation; no launch/delegation hook may be reached.
e<-new.env(parent=globalenv());e$APP_DIR<-root;e$HAS_SESSIONS<-TRUE
e$read_state<-function()list(runDir='/tmp/fixture',runDirRemote=FALSE)
e$record_session_target<-function(q,b,...)list(ok=TRUE,sid=as.character(b$session_id %||% sid))
e$record_access<-function(...)list(ok=TRUE,ctx=list(run_dir='/tmp/fixture',run_dir_remote=FALSE,mode='local',place=list()))
e$DB_CON<-NULL;e$DB_READY<-FALSE;e$HAS_FLOW<-FALSE;e$HAS_CONTRACT<-FALSE;e$HAS_NOTEBOOK<-FALSE
e$launch_step<-function(...)stop('downstream launch must not be reached')
sys.source('engine/run_enqueue.R',envir=e)
# 无命名范围的记录允许旧流程 id 照常执行；「all」仍要有界计划，force 也绕不过。
writeLines('{"intent":{"scope":{"run_until":"08"}}}',file.path(d,'profile.json'))
r<-e$run_enqueue(body=list(step='all',session_id=sid,force=TRUE),owner_ctx=list(key='fixture'),current_sid=sid,via='/api/run')
stopifnot(r$pa_status==409L,r$pa_body$code=='intent_scope_requires_bounded_plan')
# 改 cell / 合并相邻群：不再挡活，但要能随指令下发出去。
r<-e$run_enqueue(body=list(step='all',session_id=sid,force=TRUE),owner_ctx=list(key='fixture'),current_sid=sid,via='/api/run')
stopifnot(identical(r$pa_body$code,'intent_scope_requires_bounded_plan'))
cat('PASS saved restrictions cannot silently execute, force cannot bypass, corrupted profile fails closed\n')
other<-'s-other';od<-file.path(root,'state','sessions',other);dir.create(od,recursive=TRUE)
writeLines('{broken',file.path(od,'profile.json'))
r<-e$run_enqueue(body=list(step='all',session_id=other,force=TRUE),owner_ctx=list(key='fixture'),current_sid=sid,via='/api/run')
stopifnot(r$pa_status==409L,r$pa_body$code=='intent_profile_unreadable')
cat('PASS explicit target SID, not current, drives the intent gate\n')
