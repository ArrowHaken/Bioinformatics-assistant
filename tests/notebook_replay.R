source('engine/notebook_replay.R')
args <- commandArgs(TRUE)
out <- if (length(args)) args[1] else tempfile('explicit-replay-tests-')
dir.create(out, recursive = TRUE, showWarnings = FALSE)
project <- file.path(out, 'source-project')
dir.create(file.path(project, 'scripts'), recursive = TRUE)
dir.create(file.path(project, 'data'))
put <- function(path, bytes) writeBin(charToRaw(bytes), file.path(project, path))
# Real imported module and config, with CRLF and absent trailing newline.
put('scripts/helper.py', 'def total(rows):\r\n    return sum(int(row["squared"]) for row in rows)')
put('config.json', '{"multiplier":2}\n\n')
put('data/input.csv', 'value\n3\n5\n7\n')
put('scripts/first.R', paste0('cfg <- jsonlite::fromJSON("config.json")\n',
 'x <- read.csv("data/input.csv")\n',
 'write.csv(data.frame(value=x$value, squared=x$value^2 * cfg$multiplier), "intermediate.csv", row.names=FALSE)\n',
 'cat("R_INTERMEDIATE_OK\\n")\n'))
put('scripts/final.py', paste0('import csv, json, pathlib\nfrom helper import total\n',
 'rows = list(csv.DictReader(pathlib.Path("intermediate.csv").open()))\n',
 'result = {"count":len(rows),"squares":total(rows)}\n',
 'pathlib.Path("final.json").write_text(json.dumps(result, sort_keys=True))\n',
 'assert result == {"count":3,"squares":166}\n',
 'import sys, os\nassert sys.argv[1:] == ["literal space", "semi;colon", "$(not-a-shell)"]\n',
 'assert "PA_REPLAY_TEST_SENTINEL" not in os.environ\nprint("PY_FINAL_OK")'))
old <- getwd(); setwd(project)
stopifnot(system2('Rscript', c('--vanilla', 'scripts/first.R')) == 0L)
stopifnot(system2('python3', shQuote(c('scripts/final.py', 'literal space', 'semi;colon', '$(not-a-shell)'))) == 0L)
setwd(old)
manifest <- list(files = list(
 list(path='scripts/first.R', role='script'), list(path='scripts/final.py', role='script'),
 list(path='scripts/helper.py', role='script'), list(path='config.json', role='config'),
 list(path='data/input.csv', role='input', embed_authorized=TRUE)),
 steps=list(list(title='R input to intermediate',command='Rscript',argv=list('scripts/first.R'),cwd='.'),
 list(title='Python imported module to final',command='python3',
  argv=list('scripts/final.py','literal space','semi;colon','$(not-a-shell)'),cwd='.')),
 expected_outputs=list('intermediate.csv','final.json'))
built <- nbr_build(normalizePath(project), manifest)
stopifnot(isTRUE(built$ok), identical(built$full_analysis_replay_verified, FALSE),
 identical(built$notebook$metadata$pa$explicit_replay$fixture_replay_verified,FALSE))
nbr_write(built, file.path(out,'replay.ipynb'))
put('scripts/cwd_check.py', 'from pathlib import Path\nfrom helper import total\nassert Path.cwd().name == "scripts"\nassert Path("helper.py").is_file()\nassert total([{"squared":"7"}]) == 7\n')
nested <- manifest
nested$files[[6]] <- list(path='scripts/cwd_check.py',role='script')
nested$steps[[3]] <- list(title='Explicit nested cwd',command='python3',argv=list('cwd_check.py'),cwd='scripts')
nbr_write(nbr_build(normalizePath(project),nested), file.path(out,'nested-cwd.ipynb'))
fail <- function(expr, pattern) {
 e <- tryCatch({force(expr); NULL}, error=function(e) e)
 if (!inherits(e,'error') || !grepl(pattern,conditionMessage(e),ignore.case=TRUE))
   stop('Expected error: ', pattern, '; got ', if (inherits(e,'error')) conditionMessage(e) else 'success')
}
changed <- function(edit) { m <- manifest; edit(m) }
fail(nbr_build(normalizePath(project), changed(function(m) {m$files[[5]]$embed_authorized<-FALSE;m})), 'authorization')
fail(nbr_build(normalizePath(project), manifest, max_file_bytes=2), 'size limit')
fail(nbr_build(normalizePath(project), manifest, max_total_bytes=2), 'size limit')
fail(nbr_build(normalizePath(project), changed(function(m) {m$files[[1]]$path<-'../outside.R';m})), 'Unsafe')
fail(nbr_build(normalizePath(project), changed(function(m) {m$files[[1]]$path<-'missing.R';m})), 'missing')
fail(nbr_build(normalizePath(project), changed(function(m) {m$steps[[1]]$command<-'bash';m})), 'Only direct')
fail(nbr_build(normalizePath(project), changed(function(m) {m$steps[[1]]$argv<-list('-c','print(1)');m})), 'declared')
fail(nbr_build(normalizePath(project), changed(function(m) {m$steps[[1]]$cwd<-'../';m})), 'Unsafe')
fail(nbr_build(normalizePath(project), changed(function(m) {m$steps[[1]]$argv<-list('scripts/first.R','--api-key=abc');m})), 'sensitive')
fail(nbr_build(normalizePath(project), changed(function(m) {m$expected_outputs<-list('../outside.csv');m})), 'Unsafe')
fail(nbr_build(normalizePath(project), changed(function(m) {m$expected_outputs<-list('absent.csv');m})), 'missing')
put('config-secret.json','{}')
fail(nbr_build(normalizePath(project), changed(function(m) {m$files[[4]]$path<-'config-secret.json';m})), 'Sensitive filename')
for (content in c('{"password":"abc"}', '-----BEGIN RSA PRIVATE KEY-----',
                  'https://alice:example@example.org/data', 'AKIAABCDEFGHIJKLMNOP')) {
 put('unsafe.txt',content)
 fail(nbr_build(normalizePath(project), changed(function(m) {m$files[[4]]$path<-'unsafe.txt';m})), 'credential')
}
put('data/binary.dat',rawToChar(as.raw(c(1,2,3))))
# Binary input bytes, including NUL, are restored without text transcoding.
writeBin(as.raw(c(0,255,128,13,10,0)), file.path(project,'data/binary.dat'))
binary_m <- manifest; binary_m$files[[6]] <- list(path='data/binary.dat',role='input',embed_authorized=TRUE)
binary_b <- nbr_build(normalizePath(project),binary_m)
nbr_write(binary_b,file.path(out,'binary.ipynb'))
stopifnot(file.symlink(file.path(project,'config.json'),file.path(project,'link.json')))
fail(nbr_build(normalizePath(project), changed(function(m) {m$files[[4]]$path<-'link.json';m})), 'Symlink')
stopifnot(file.symlink(project, file.path(out,'linked-project')))
fail(nbr_build(file.path(out,'linked-project'),manifest), 'Symlink')
# Produce notebook negative fixtures before moving the entire source project away.
put('scripts/failure.py', 'raise RuntimeError("intentional fixture failure")\n')
bad <- manifest
bad$files[[6]] <- list(path='scripts/failure.py',role='script')
bad$steps[[2]]$argv <- list('scripts/failure.py')
nbr_write(nbr_build(normalizePath(project),bad),file.path(out,'failure.ipynb'))
put('scripts/missing.py','print("no final output")\n')
bad$files[[6]] <- list(path='scripts/missing.py',role='script')
bad$steps[[2]]$argv <- list('scripts/missing.py')
nbr_write(nbr_build(normalizePath(project),bad),file.path(out,'missing-output.ipynb'))
put('scripts/wrong.py','from pathlib import Path\nPath("final.json").write_text("wrong result")\n')
bad$files[[6]] <- list(path='scripts/wrong.py',role='script');bad$steps[[2]]$argv<-list('scripts/wrong.py')
nbr_write(nbr_build(normalizePath(project),bad),file.path(out,'wrong-output.ipynb'))
put('scripts/symlink.py','import pathlib\npathlib.Path("final.json").symlink_to("intermediate.csv")\n')
bad$files[[6]] <- list(path='scripts/symlink.py',role='script');bad$steps[[2]]$argv<-list('scripts/symlink.py')
nbr_write(nbr_build(normalizePath(project),bad),file.path(out,'symlink-output.ipynb'))
# Source outputs saved separately for independent byte comparison, not used by replay.
dir.create(file.path(out,'baselines'))
for (path in c('intermediate.csv','final.json'))
 stopifnot(file.copy(file.path(project,path),file.path(out,'baselines',path)))
stopifnot(file.rename(project,file.path(out,'source-project-unavailable')))
cat('PASS R manifest/security validation and real source R+Python execution:',out,'\n')
