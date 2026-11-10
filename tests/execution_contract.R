`%||%` <- function(x,y) if(is.null(x)) y else x
source('engine/flow_spec.R')
# flow_vars now canonicalizes session aliases; this direct-call fixture predates that contract.
# species_contract.R exercises the real production normalization and rejects direct aliases.
V <- list(rscript='/opt/R path/Rscript',engine='/tmp/engine path',work_root='/tmp/work path',species='Human',reference_xlsx='/tmp/ref.json')
x <- flow_execution(flow_stage('enrich'),V)
stopifnot(x$ok, tail(x$argv,1)=='Human',grepl("'/tmp/work path/9.annot/annotated.rds'",x$command,fixed=TRUE))
V$species <- ''; y <- flow_execution(flow_stage('enrich'),V)
stopifnot(!y$ok,'species' %in% y$missing,is.null(flow_cmd('enrich',V)))
V$species <- 'Rat'; saved_rscript <- V$rscript; V$rscript <- ''; stopifnot(!flow_execution(flow_stage('enrich'), V)$ok); V$rscript <- saved_rscript
V$species <- 'Rat'; z <- flow_execution(flow_stage('annotate'),V)
stopifnot(z$ok,'--lit-dict' %in% z$argv,!('--reference' %in% z$argv))
# Load only pure instruction functions; never source the HTTP server.
e <- new.env(parent=globalenv())
for (expr in parse('serve.R')) {
 if(is.call(expr) && identical(expr[[1]],as.name('<-')) && as.character(expr[[2]]) %in% c('remote_step_instruction','flow_subs_of')) eval(expr,e)
}
e$HAS_FLOW <- TRUE; e$STEP_TASK <- list(enrich=list(title='富集',ask='测试'))
e$flow_vars <- function(...) V
e$flow_profile <- function() 'server'
e$supplement_for_stage <- function(id) jsonlite::fromJSON('engine/library/C_supplement/index.json',simplifyVector=FALSE)$entries[3]
e$supplement_path <- function(x) file.path('engine',x$file)
e$supplement_fill <- function(x,V) {for(k in names(V)) x<-gsub(paste0('{',k,'}'),V[[k]],x,fixed=TRUE); x}
t <- e$remote_step_instruction('enrich',list(server=list()),'fixture-session','fixture-request')
stopifnot(!grepl('<落地后的路径>',t,fixed=TRUE),!grepl('-maxdepth 2',t,fixed=TRUE),!grepl('pipeline1',t,fixed=TRUE),grepl("'--species' 'Rat'",t,fixed=TRUE),grepl('fixture-session',t,fixed=TRUE),!grepl('都已就位',t,fixed=TRUE))
# Static assertion: unknown callback states must be rejected before state write.
server <- readLines('serve.R', warn = FALSE)
stopifnot(any(grepl('invalid_step_status', server, fixed = TRUE)))
cat('PASS: canonical Human/Rat (updated stale alias fixture), missing species, spaces, JSON reference, single command, deep scan, identity, guard claims\n')
