#!/usr/bin/env Rscript
# Extract the actual route only; never initialize the server or production state.
suppressPackageStartupMessages(library(jsonlite))
`%||%` <- function(a,b) if(is.null(a)||!length(a)) b else a
text <- readLines('engine/server_main.R', warn=FALSE)
a <- grep('^  if \\(p == "/api/config" &&',text)
b <- grep('^  if \\(p == "/api/run" &&',text)
stopifnot(length(a)==1L,length(b)==1L,b>a)
handler <- eval(parse(text=c('function(p,req,con) {',text[a:(b-1L)],'}')))
runDir <- tempfile('config-body-',tmpdir='/tmp');dir.create(runDir)
st <- list(runDirRemote=FALSE)
body <- list();body_json <- function() body
send_json <- function(con,status,body) list(status=status,body=body)
cur_sid <- function() 'fixture-session'
record_session_target <- function(...) list(ok=TRUE,sid='fixture-session')
record_access <- function(...) list(ok=TRUE,ctx=list(run_dir=runDir,run_dir_remote=FALSE,place=list()),
  cap=list(session_id='fixture-session',can_read=TRUE,can_write=TRUE,readonly=FALSE))
record_cap_fields <- function(...) list()
result <- handler('/api/config',list(method='POST'),NULL)
stopifnot(result$status==400L,identical(result$body$error,'没有可保存的内容'))
body <- list('pipeline.json'=list(species='fixture_explicit_species',tissue='fixture_explicit_tissue',nested=list(n=7)),
             'gates.json'=list(gates=list(list(id='fixture',status='answered',answer='explicit'))))
result <- handler('/api/config',list(method='POST'),NULL)
stopifnot(result$status==200L)
for(name in names(body)) {
  expected <- fromJSON(toJSON(body[[name]], auto_unbox=TRUE, null='null', digits=NA), simplifyVector=FALSE)
  stopifnot(identical(fromJSON(file.path(runDir,name),simplifyVector=FALSE),expected))
}
cat('PASS local config: empty body rejected, exact pipeline and gates fields saved without undefined b\n')
