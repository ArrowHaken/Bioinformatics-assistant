#!/usr/bin/env Rscript
# Cheap subprocess contract: real orchestration/guard/config; analysis is stubbed.
stopifnot(requireNamespace("jsonlite", quietly = TRUE))
root <- normalizePath(".", winslash = "/")
evidence <- Sys.getenv("PIPELINE_STAGE_EVIDENCE_DIR", "")
keep <- nzchar(evidence)
if (!keep) evidence <- tempdir()
dir.create(evidence, recursive = TRUE, showWarnings = FALSE)
evidence <- tempfile("pipeline-stage-contract-", tmpdir = evidence)
dir.create(evidence)
evidence <- normalizePath(evidence, winslash = "/")
engine <- file.path(evidence, "engine")
scripts <- file.path(engine, "sc-pipeline", "scripts")
dir.create(file.path(scripts, "lib"), recursive = TRUE, showWarnings = FALSE)
file.copy(file.path(root, "engine/sc-pipeline/scripts/00_run_all.R"), scripts, overwrite = TRUE)
file.copy(file.path(root, "engine/sc-pipeline/scripts/lib/config.R"), file.path(scripts, "lib"), overwrite = TRUE)
file.copy(file.path(root, "engine/workflow"), engine, recursive = TRUE, overwrite = TRUE)
for (f in c("verdict.R", "flow_spec.R")) file.copy(file.path(root, "engine", f), engine, overwrite = TRUE)
dir.create(file.path(engine, "audit"), showWarnings = FALSE)
file.copy(file.path(root, "engine/audit/whitelist.json"), file.path(engine, "audit"), overwrite = TRUE)
source(file.path(engine, "workflow/guard.R"))
step_plan <- new.env(parent = globalenv())
sys.source(file.path(root, "engine/step_plan.R"), envir = step_plan)
writeLines(c(
  'a <- commandArgs(TRUE); opt <- function(k) a[match(k,a)+1L]',
  'out <- opt("--outdir"); stage <- opt("--stage")',
  'dir.create(file.path(out,"status"), recursive=TRUE, showWarnings=FALSE)',
  'writeLines(stage, file.path(out,"preprocess-called"))',
  'mode <- Sys.getenv("FIXTURE_PREPROCESS_MODE")',
  'if (mode == "fail") quit(status=7)',
  'if (mode %in% c("snapshot", "snapshot-fail")) saveRDS(list(source_phase=stage, token=Sys.getenv("FIXTURE_TOKEN")), file.path(out,"status/stage_cluster1.rds"))',
  'if (mode == "snapshot-fail") quit(status=7)',
  'if (mode == "empty") file.create(file.path(out,"status/stage_cluster1.rds"))',
  'if (mode == "directory") dir.create(file.path(out,"status/stage_cluster1.rds"))',
  'if (mode == "processed") saveRDS(list(source_phase=stage), file.path(out,"processed.rds"))'
), file.path(scripts, "01_preprocess.R"))
writeLines(c('a <- commandArgs(TRUE)', 'out <- a[match("--outdir",a)+1L]',
             'writeLines(a[1], file.path(out,"annotation-input"))',
             'saveRDS(list(source_phase="annot"),file.path(out,"annotated.rds"))'),
           file.path(scripts, "02_annotate.R"))
failures <- character()
check <- function(ok, label) {
  cat(if (isTRUE(ok)) "PASS" else "FAIL", label, "\n")
  if (!isTRUE(ok)) failures <<- c(failures, label)
}
run_case <- function(name, stage, mode, old_snapshot = FALSE, old_processed = FALSE,
                     resume = "precheck", annotation = FALSE, dry_run = FALSE,
                     resolved_precheck = FALSE, old_status = FALSE) {
  rd <- file.path(evidence, name); out <- file.path(rd, "results")
  dir.create(file.path(out, "status"), recursive = TRUE)
  dir.create(file.path(out, "01_single"))
  if (old_snapshot) saveRDS(list(source_phase="old",token="old"), file.path(out,"status/stage_cluster1.rds"))
  if (old_processed) saveRDS(list(source_phase="old"), file.path(out,"processed.rds"))
  if (old_processed) jsonlite::write_json(list(stage="all",state="succeeded",source_phase="old"), file.path(out,"run_manifest.json"), auto_unbox=TRUE)
  if (old_status) file.copy(file.path(out,"run_manifest.json"), file.path(out,"status/run_status.json"))
  protected <- c(file.path(out,"status/stage_cluster1.rds"), file.path(out,"run_manifest.json"), file.path(out,"status/run_status.json"))
  before <- tools::md5sum(protected)
  status_files_before <- list.files(file.path(out,"status"), all.files=TRUE)
  cfg <- list(io=list(indir=list(value=rd),outdir=list(value=out)))
  if (annotation) {
    cfg$io$reference_table <- list(value=file.path(rd,"fixture-reference.tsv"))
    writeLines("gene\tcell_type\nfixture\tfixture", cfg$io$reference_table$value)
  }
  cp <- file.path(rd,"pipeline.json"); jsonlite::write_json(cfg,cp,auto_unbox=TRUE)
  sc_write_guard_lock(rd,file.path(engine,"workflow"))
  sc_ledger_append(rd,list(run_id=name,event="start",step="fixture"))
  Sys.setenv(SC_WORKFLOW_RUN=name,SC_WORKFLOW_DIR=rd,SC_WORKFLOW_ENGINE=engine,
             FIXTURE_PREPROCESS_MODE=mode,FIXTURE_TOKEN=name)
  log <- file.path(rd,"orchestration.log")
  cli <- if (resolved_precheck) step_plan$resolve_step_cli("precheck", rd, engine) else
    list(cmd=file.path(R.home("bin"),"Rscript"), args=c(file.path(scripts,"00_run_all.R"),"--config",cp,"--stage",stage,"--resume-from",resume,if(dry_run) "--dry-run"))
  if (resolved_precheck) check(is.null(cli$error) && identical(unname(cli$args[1]),file.path(scripts,"00_run_all.R")) && "--dry-run" %in% cli$args && !"--stage" %in% cli$args, paste(name,"real resolver precheck command, default all"))
  status <- system2(cli$cmd,shQuote(cli$args),stdout=log,stderr=log)
  list(status=status,out=out,log=readLines(log,warn=FALSE),before=before,
       after=tools::md5sum(protected),status_files_before=status_files_before,
       status_files_after=list.files(file.path(out,"status"), all.files=TRUE))
}
for (name in c("fresh-cluster1", "old-processed-cluster1", "old-snapshot-replaced", "resume-cluster1")) {
  f <- run_case(name,"cluster1","snapshot",old_snapshot=name=="old-snapshot-replaced",old_processed=name=="old-processed-cluster1",resume=if(name=="resume-cluster1") "cluster1" else "precheck",annotation=TRUE)
  check(f$status==0L,paste(name,"exit 0"))
  mp <- file.path(f$out,"run_manifest.json")
  m <- if(file.exists(mp)) jsonlite::read_json(mp) else list()
  check(identical(m$stage,"cluster1") && identical(m$state,"succeeded") && identical(m$source_phase,"cluster1"),paste(name,"manifest stage/state/source"))
  artifact <- m$artifacts$snapshot
  check(identical(artifact$path,"status/stage_cluster1.rds") && identical(artifact$source_phase,"cluster1") && identical(artifact$md5,unname(tools::md5sum(file.path(f$out,"status/stage_cluster1.rds")))) && identical(m$run_id,name),paste(name,"manifest current snapshot digest/run"))
  sp <- file.path(f$out,"status/run_status.json")
  check(file.exists(sp) && identical(jsonlite::read_json(sp),m),paste(name,"status equals manifest"))
  check(!dir.exists(file.path(f$out,"annot")) && !any(grepl("===== 全流程完成 =====",f$log,fixed=TRUE)),paste(name,"no annotation/full-pipeline success"))
  snapshot <- if(file.exists(file.path(f$out,"status/stage_cluster1.rds"))) readRDS(file.path(f$out,"status/stage_cluster1.rds")) else list()
  check(identical(snapshot$token,name),paste(name,"snapshot from current invocation"))
}
for (name in c("missing-snapshot", "stale-snapshot", "old-processed-only", "child-failed", "snapshot-fail", "empty", "directory")) {
  mode <- if (name == "child-failed") "fail" else if (name %in% c("snapshot-fail", "empty", "directory")) name else "nothing"
  f <- run_case(name,"cluster1",mode,old_snapshot=name %in% c("stale-snapshot","child-failed"),old_processed=name=="old-processed-only",resume=if(name=="stale-snapshot") "annot" else "precheck")
  check(f$status!=0L,paste(name,"nonzero; no stale-artifact success"))
  check(!any(grepl("===== cluster1 段完成 =====",f$log,fixed=TRUE)),paste(name,"no stage success"))
  m <- jsonlite::read_json(file.path(f$out,"run_manifest.json"))
  check(identical(m$state,"failed") && identical(m$source_phase,"cluster1") && identical(m$run_id,name) && length(m$artifacts)==0L,paste(name,"failed manifest without claimed artifact"))
  check(identical(jsonlite::read_json(file.path(f$out,"status/run_status.json")),m),paste(name,"failed status equals manifest"))
}
for(stage in c("continue","all")) {
  f <- run_case(paste0(stage,"-missing-processed"),stage,"snapshot")
  check(f$status!=0L,paste(stage,"still requires processed"))
  f <- run_case(paste0(stage,"-processed"),stage,"processed",annotation=TRUE)
  check(f$status==0L && file.exists(file.path(f$out,"annot/annotated.rds")),paste(stage,"processed then annotation"))
  check(identical(readLines(file.path(f$out,"annot/annotation-input")),file.path(f$out,"processed.rds")),paste(stage,"annotation source is processed"))
}
for (name in c("precheck-fresh", "precheck-old-processed", "precheck-child-failed",
               "cluster1-dry-fresh", "cluster1-dry-old", "cluster1-dry-failed",
               "all-dry-resume-annot")) {
  resolved <- startsWith(name,"precheck-")
  failed <- grepl("failed",name,fixed=TRUE)
  old <- name %in% c("precheck-old-processed","cluster1-dry-old","cluster1-dry-failed","all-dry-resume-annot")
  f <- run_case(name,if(startsWith(name,"cluster1")) "cluster1" else "all",
                if(failed) "fail" else "nothing",old_snapshot=old,old_processed=old,
                annotation=TRUE,dry_run=TRUE,resolved_precheck=resolved,old_status=old,
                resume=if(name=="all-dry-resume-annot") "annot" else "precheck")
  check(if(failed) f$status!=0L else f$status==0L,paste(name,"exit follows real preprocess dry-run"))
  check(file.exists(file.path(f$out,"preprocess-called")),paste(name,"preprocess invoked even when resume would skip"))
  check(!dir.exists(file.path(f$out,"annot")) && !any(grepl("===== 全流程完成 =====",f$log,fixed=TRUE)) && !any(grepl("===== cluster1 段完成 =====",f$log,fixed=TRUE)),paste(name,"no annotation/science completion"))
  check(identical(f$before,f$after) && identical(f$status_files_before,f$status_files_after),paste(name,"snapshot/manifest/status unchanged, no isolation"))
  check(identical(any(grepl("precheck-only",f$log,fixed=TRUE)),!failed),paste(name,"precheck-only only after child success"))
}
cat("Evidence:",evidence,"\n")
if(length(failures)) stop(length(failures)," contract checks failed",call.=FALSE)
cat("PASS pipeline stage orchestration contract; no scientific validity claim\n")
if(!keep) unlink(evidence,recursive=TRUE)
