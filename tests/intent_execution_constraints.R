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
check(list(reference=list(modified_cells=list(3,5))), 'reference.modified_cells')
check(list(annotation=list(merge_adjacent=FALSE)), 'annotation.merge_adjacent')
check(list(annotation=list(merge_adjacent=TRUE)), 'annotation.merge_adjacent')
check(list(scope=list(annotation_gate=FALSE)),character())
writeLines('{broken',file.path(d,'profile.json'))
r <- intent_execution_gate(root,sid);stopifnot(!r$ok,r$code=='intent_profile_unreadable')
# Use the actual run route prefix; no launch/delegation hook may be reached.
src <- readLines('serve.R',warn=FALSE)
a <- grep('^  if \\(p == "/api/run"',src)[1]
b <- grep('    # ---- 对账不一致',src)[1]
e <- new.env(parent=globalenv());e$APP_DIR<-root;e$HAS_SESSIONS<-TRUE;e$runDir<-'/tmp/fixture';e$p<-'/api/run';e$req<-list(method='POST');e$con<-NULL
e$cur_sid<-function()sid;e$body_json<-function()list(step='all',force=TRUE)
e$send_json<-function(con,status,body)list(status=status,body=body)
f<-eval(parse(text=c('function(){',src[a:(b-1)],'}','}')),envir=e)
writeLines('{"intent":{"scope":{"run_until":"08"}}}',file.path(d,'profile.json'))
r<-f();stopifnot(r$status==409L,r$body$code=='intent_constraints_unimplemented')
cat('PASS saved restrictions cannot silently execute, force cannot bypass, corrupted profile fails closed\n')
e$body_json<-function()list(step='all',session_id='other',force=TRUE)
r<-f();stopifnot(r$status==409L,r$body$code=='session_selection_changed')
cat('PASS different execution SID rejected before profile gate or enqueue\n')
