#!/usr/bin/env Rscript
source('engine/intent_bridge.R')
root <- tempfile('intent-scope-', tmpdir='/tmp'); dir.create(root)
sid <- 's-fixture'
d <- file.path(root, 'state', 'sessions', sid); dir.create(d, recursive=TRUE)
profile <- file.path(d, 'profile.json')
put <- function(intent) writeLines(as.character(jsonlite::toJSON(
  list(intent=intent), auto_unbox=TRUE, null='null')), profile)
expect <- function(step, code=NULL) {
  r <- intent_execution_gate(root, sid, requested_step=step)
  if (is.null(code)) stopifnot(isTRUE(r$ok))
  else stopifnot(identical(r$ok,FALSE), identical(r$code,code))
  invisible(r)
}
chain <- c('env','decide','precheck','pipeline1','pipeline2','plots','ppt','pack')
# Named scope plans use app IDs, not GPU notebook numbers or flow_spec stages.
plan <- parse('engine/step_plan.R')
chain_assignment <- Filter(function(x) is.call(x) && identical(x[[1]],as.name('<-')) &&
  identical(x[[2]],as.name('CHAIN_ORDER')), as.list(plan))
stopifnot(length(chain_assignment)==1L,
  identical(eval(chain_assignment[[1]][[3]]),chain), identical(INTENT_APP_CHAIN,chain))
for (i in seq_along(chain)) {
  put(list(scope=list(run_until=chain[i])))
  for (j in seq_along(chain)) expect(chain[j], if (j<=i) NULL else 'intent_scope_outside_plan')
  expect('all','intent_scope_requires_bounded_plan')
  expect('config','intent_scope_outside_plan')
  old <- intent_execution_gate(root,sid)
  stopifnot(!old$ok, identical(unlist(old$fields),c('scope.run_until')))
}
for (bound in c('08','08之前','before 08','pipeline1 ','annotation','stage08','config','all')) {
  put(list(scope=list(run_until=bound)))
  for (step in c('env','pipeline1','all')) expect(step,'intent_constraints_unimplemented')
}
put(list(scope=list(annotation_gate=TRUE), approval=TRUE,
  annotation=list(approved=TRUE)))
for (step in chain[1:4]) expect(step)
for (step in c(chain[5:8],'config')) expect(step,'annotation_review_required')
expect('all','intent_scope_requires_bounded_plan')
put(list(scope=list(run_until='pipeline1',annotation_gate=TRUE)))
expect('pipeline1'); expect('pipeline2','annotation_review_required')
for (restriction in list(list(reference=list(modified_cells=list(3,5))),
                         list(reference=list(modified_cells=list())),
                         list(reference=list(modified_cells=FALSE)),
                         list(annotation=list(merge_adjacent=TRUE)),
                         list(annotation=list(merge_adjacent=FALSE)),
                         list(annotation=list(merge_adjacent=list())))) {
  for (sc in list(NULL,list(run_until='pipeline1',annotation_gate=TRUE))) {
    restriction$scope <- sc
    put(restriction)
    for (step in c(chain,'config','all','enrich','annotate','read'))
      expect(step,'intent_constraints_unimplemented')
  }
}
put(list(scope=list(annotation_gate=FALSE)))
for (step in c(chain,'config','all')) expect(step)
# Explicit nulls mean absent restrictions. Populated arrays and atomics never do.
writeLines('{"intent":{"scope":{"run_until":null,"annotation_gate":null},"reference":{"modified_cells":null},"annotation":{"merge_adjacent":null}}}',profile)
expect('all')
invalid <- c('{broken','[]','null','1','true','"profile"',
  '{"intent":1}','{"intent":true}','{"intent":"x"}',
  '{"intent":[{"scope":{"run_until":"pipeline1"}}]}',
  '{"intent":{"scope":1}}','{"intent":{"scope":false}}',
  '{"intent":{"scope":"pipeline1"}}','{"intent":{"scope":[{}]}}',
  '{"intent":{"reference":1}}','{"intent":{"reference":[{}]}}',
  '{"intent":{"annotation":true}}','{"intent":{"annotation":[{}]}}',
  '{"intent":{"scope":{"run_until":1}}}',
  '{"intent":{"scope":{"run_until":true}}}',
  '{"intent":{"scope":{"run_until":[]}}}',
  '{"intent":{"scope":{"run_until":{}}}}',
  '{"intent":{"scope":{"run_until":["env","pack"]}}}',
  '{"intent":{"scope":{"run_until":""}}}',
  '{"intent":{"scope":{"annotation_gate":"true"}}}',
  '{"intent":{"scope":{"annotation_gate":1}}}',
  '{"intent":{"scope":{"annotation_gate":[]}}}',
  '{"intent":{"scope":{"annotation_gate":{}}}}',
  '{"intent":{"scope":{"annotation_gate":[true,false]}}}',
  '{"intent":{},"intent":null}',
  '{"intent":{"scope":{"run_until":"env","run_until":"pack"}}}',
  '{"intent":{"scope":{"annotation_gate":true,"annotation_gate":false}}}',
  '{"intent":{"reference":{"modified_cells":[],"modified_cells":null}}}',
  '{"intent":{"annotation":{"merge_adjacent":true,"merge_adjacent":null}}}',
  '{"intent":{"scope":{},"scope":null}}')
for (json in invalid) {
  writeLines(json,profile)
  for (step in list(NULL,'env','all','enrich','annotate','read'))
    expect(step,'intent_profile_unreadable')
}
# Scalar legacy IDs are left to the existing executor when no named scope is consumed.
legacy <- c('enrich','annotate','read','08','annotation','flow.stage',' ENV','ENV')
bad_shape <- list(1,TRUE,NA_character_,character(),c('env','pack'),list('env'),
                  list(step='env'),'')
for (json in list(NULL,'{}','{"intent":null}','{"intent":{}}','{"intent":[]}',
                 '{"intent":{"scope":{"annotation_gate":false}}}')) {
  if (is.null(json)) unlink(profile) else writeLines(json,profile)
  for (step in c(chain,'config','all',legacy)) expect(step)
  for (step in bad_shape) expect(step,'intent_step_invalid')
  expect(NULL)
}
for (scope in list(list(run_until='pipeline1'),list(annotation_gate=TRUE))) {
  put(list(scope=scope))
  for (step in legacy) expect(step,'intent_step_invalid')
  for (step in bad_shape) expect(step,'intent_step_invalid')
}
# Use the actual /api/run prefix, appending a sentinel at the first downstream boundary.
# Never synthesize a replacement gate invocation: this also verifies caller wiring.
src <- readLines('serve.R',warn=FALSE)
a <- grep('^  if \\(p == "/api/run"',src)[1]
b <- grep('    # ---- 对账不一致',src)[1]
stopifnot(!is.na(a),!is.na(b),b>a)
e <- new.env(parent=globalenv())
e$APP_DIR <- root; e$HAS_SESSIONS <- TRUE; e$runDir <- '/tmp/fixture'
e$p <- '/api/run'; e$req <- list(method='POST'); e$con <- NULL
e$cur_sid <- function() sid
e$send_json <- function(con,status,body) list(status=status,body=body)
e$downstream <- 0L
e$enqueue_task <- function(...) stop('enqueue must not be reached by prefix fixture')
e$launch <- e$enqueue_task
f <- eval(parse(text=c('function(){',src[a:(b-1)],
  'downstream <<- downstream + 1L; return(list(status=299L,body=list(ok=TRUE)))','}','}')),envir=e)
route <- function(step,code=NULL,force=FALSE) {
  e$body_json <- function() list(step=step,session_id=sid,force=force)
  e$downstream <- 0L
  r <- f()
  if (is.null(code)) stopifnot(r$status==299L,e$downstream==1L)
  else stopifnot(r$status==409L,r$body$code==code,e$downstream==0L)
}
put(list(scope=list(run_until='pipeline1')))
for (force in c(FALSE,TRUE)) {
  route('pipeline1',force=force)
  route('pipeline2','intent_scope_outside_plan',force)
  route('all','intent_scope_requires_bounded_plan',force)
}
put(list(scope=list(annotation_gate=TRUE),annotation=list(approved=TRUE)))
for (force in c(FALSE,TRUE)) {
  route('pipeline1',force=force)
  route('pipeline2','annotation_review_required',force)
  route('all','intent_scope_requires_bounded_plan',force)
}
for (restriction in list(list(reference=list(modified_cells=list(3,5))),
                         list(annotation=list(merge_adjacent=FALSE)))) {
  put(restriction)
  route('env','intent_constraints_unimplemented',TRUE)
}
put(list(scope=list(run_until='08')))
route('env','intent_constraints_unimplemented',TRUE)
for (json in list(NULL,'{}','{"intent":{}}',
                 '{"intent":{"scope":{"annotation_gate":false}}}')) {
  if (is.null(json)) unlink(profile) else writeLines(json,profile)
  for (step in c('enrich','annotate','read')) route(step)
}
for (scope in list(list(run_until='pipeline1'),list(annotation_gate=TRUE))) {
  put(list(scope=scope))
  for (step in c('enrich','annotate','read')) route(step,'intent_step_invalid',TRUE)
}
put(list())
route(c('env','pack'),'intent_step_invalid',TRUE)
writeLines('{"intent":{"scope":false}}',profile)
route('env','intent_profile_unreadable',TRUE)
cat('PASS exact app-stage scope, review boundary, legacy contract, malformed shapes, and actual route no-enqueue/force boundary\n')
unlink(root,recursive=TRUE)
