e <- new.env(parent=globalenv())
for(x in parse('engine/server_main.R')) if(is.call(x)&&identical(x[[1]],as.name('<-'))&&is.call(x[[3]])&&identical(x[[3]][[1]],as.name('function'))) eval(x,e)
old <- list(status='delegated',delegated_at='2026-11-06T16:10:01',session_id='B',req_id='r')
stopifnot(identical(e$rec_display_status(old),'handoff_unconfirmed'))
confirmed <- old; confirmed$delivered_at <- '2026-11-06T16:10:02'
stopifnot(identical(e$rec_display_status(confirmed),'delegated'))
for(s in c('running','done','failed')) {r<-old;r$status<-s;stopifnot(identical(e$rec_display_status(r),s))}
stopifnot(identical(e$rec_actual_artifacts(NULL),list()))
stopifnot(identical(e$rec_actual_artifacts(list(artifacts=list(list(rel='10.figs/real.png')))),list('10.figs/real.png')))
cat('PASS: unconfirmed handoff is not confirmed delivery; actual artifacts only\n')
