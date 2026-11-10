#!/usr/bin/env Rscript
# =====================================================================
#  orchestration_tests.R -- F 组与 G 组的行为测试
#
#  为什么单独一个文件
#    checklist_assertions.R 大多是「查这个符号在不在」。
#    那种检查能证明代码存在，证明不了它算得对。
#    F7「能不能跑」、G4「两次结果一不一致」这类，判错的代价是一整轮结论错，
#    必须真喂数据、看输出。
#
#  测什么
#    F1  数据画像：样本/格式认得出来，物种缺失时如实留空，不猜
#    F4  依赖自检：正常链通过；故意造一条断链必须被抓到
#    F6  下一环：环境判否时不开工；上一步没通过时指出卡在哪个环节
#    F7  环境三档：体检缺失 → unknown（不是 yes）；缺包 → no 并指名
#    G1  留痕：写一条能反查回来
#    G2  建议：删一个被依赖的环节必须被拒，并指出谁依赖它
#    G4  收敛：md5 一致判一致；不一致但不给 md5 时判「判不了」，不是「不一致」
#
#  用法
#    Rscript engine/tests/orchestration_tests.R [--app <dir>]
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

args <- commandArgs(trailingOnly = TRUE)
APP <- local({
  a <- sub("^--app=", "", args[grepl("^--app=", args)])
  if (length(a)) normalizePath(a) else normalizePath(getwd())
})

for (f in c("db.R", "verdict.R", "rework.R", "compliance.R", "poller.R",
            "db_api.R", "dataset_profile.R", "runnable.R", "orchestrator.R")) {
  p <- file.path(APP, "engine", f)
  if (file.exists(p)) source(p)
}

PASS <- 0L; FAIL <- 0L; FAILED <- character(0)
ok <- function(name, cond, detail = "") {
  if (isTRUE(cond)) { PASS <<- PASS + 1L; cat(sprintf("  [ok]   %s\n", name)) }
  else { FAIL <<- FAIL + 1L; FAILED <<- c(FAILED, paste(name, detail))
         cat(sprintf("  [FAIL] %s  %s\n", name, detail)) }
}

TMP <- tempfile("orch-")
dir.create(file.path(TMP, "engine"), recursive = TRUE)
dir.create(file.path(TMP, "state"), recursive = TRUE)
file.copy(file.path(APP, "engine", "flow_spec.R"),
          file.path(TMP, "engine", "flow_spec.R"), overwrite = TRUE)
con <- db_connect(TMP); db_init(con)

# flow_spec 需要加载到全局：runnable/orchestrator 都读 FLOW_* 对象
e <- new.env(parent = globalenv())
sys.source(file.path(TMP, "engine", "flow_spec.R"), envir = e)
for (k in ls(e)) if (is.list(e[[k]]) || is.function(e[[k]])) assign(k, e[[k]], envir = globalenv())

cat("\n=== F1 数据画像 ===\n")
env_ok <- list(ok = TRUE, verdict = "ready",
  samples = list(count = 7L, names = c("a", "b", "c", "d", "e", "f", "g"),
                 matrix_files_each = 3L, note = ""),
  runtime = list(rscript = "/x/Rscript", lib_paths = "/x/lib", queue = "all.q"))
place <- list(server = list(data_root = "/data", script_root = "/script", work_root = "/work"))
p1 <- dp_build(place = place, identify = NULL, env = env_ok, ref = NULL)
ok("样本数从体检读出来", identical(as.integer(p1$sample_count), 7L), as.character(p1$sample_count))
ok("10x 三件套认得出", identical(p1$matrix_format, "10x_mtx"), p1$matrix_format %||% "NA")
ok("没有参考表时物种留空，不猜", is.na(p1$species), as.character(p1$species))
ok("缺物种时 ok=FALSE 并说明", isFALSE(p1$ok) && length(p1$missing) > 0L)

p2 <- dp_build(place = place,
               env = env_ok, ref = list(species = "Mouse", tissue = "Spleen", id = "r1"))
ok("有参考表时物种来自参考表", identical(p2$species, "Mouse") &&
     identical(p2$species_source, "reference"), p2$species %||% "NA")
ok("参考表就位后 ok=TRUE", isTRUE(p2$ok))

cat("\n=== F4 依赖自检 ===\n")
d1 <- flow_deps_status("server")
ok("正常流程依赖链完整", isTRUE(d1$ok), d1$message %||% "")
ok("12 环都在", identical(as.integer(d1$stages), 12L))

# 造一条断链：临时改一份 flow_spec，指向不存在的上游产物
bad <- readLines(file.path(TMP, "engine", "flow_spec.R"), warn = FALSE, encoding = "UTF-8")
i <- grep('^FLOW_CONSUMES', bad)[1]
bad <- append(bad, 'FLOW_CONSUMES$merge <- c("stage:markers/5.markers/nonexistent.xls")',
              after = length(bad))
bf <- file.path(TMP, "engine", "flow_spec_bad.R")
writeLines(bad, bf, useBytes = TRUE)
e2 <- new.env(parent = globalenv()); sys.source(bf, envir = e2)
d2 <- e2$flow_deps_status("server")
ok("故意断链能被抓到", isFALSE(d2$ok), d2$message %||% "")
ok("断链指出是哪一环和哪一处",
   length(d2$breaks) > 0L && nzchar(d2$breaks[[1]]$stage) && nzchar(d2$breaks[[1]]$decl),
   if (length(d2$breaks)) paste(d2$breaks[[1]]$stage, d2$breaks[[1]]$decl) else "无")

cat("\n=== F6 下一环能不能开 ===\n")
# 没有体检结果：必须先卡在环境，不能当成能跑
n1 <- orch_next(con, TMP, "V-not-exist", env = NULL)
ok("没有体检结果时不开工", isFALSE(n1$ready))
ok("卡点归到 env", identical(n1$blocked_by, "env"), n1$blocked_by %||% "NA")

# 写上体检结果，再造一条没通过的回写
writeLines(jsonlite::toJSON(env_ok, auto_unbox = TRUE), file.path(TMP, "state", "remote_env.json"))
pid <- db_ensure_project(con, "local:default", "t")
vid <- db_ensure_version(con, pid, "local:default")
rid <- db_ensure_run(con, vid, "local:default", work_root = "/work")
DBI::dbExecute(con, "INSERT INTO step_attempts(attempt_id,run_id,version_id,owner_key,step_id,
                 attempt_no,status,reported_at,payload) VALUES(?,?,?,?,?,?,?,?,?)",
  list("att-1", rid, vid, "local:default", "doublet", 1L, "rejected", db_now(), "{}"))
n2 <- orch_next(con, TMP, vid, env = env_ok)
ok("上一步没通过时不开工", isFALSE(n2$ready))
ok("指出卡在 doublet", identical(n2$stage, "doublet"), n2$stage %||% "NA")
ok("卡点归到 upstream", identical(n2$blocked_by, "upstream"), n2$blocked_by %||% "NA")

cat("\n=== F7 环境三档 ===\n")
r0 <- runnable_chain(NULL, "server")
ok("没有体检 → unknown（不是 yes）", identical(r0$verdict, "unknown"), r0$verdict)
ok("unknown 时 ok=FALSE", isFALSE(r0$ok))

env_nopkg <- env_ok
env_nopkg$items <- list(list(id = "pkg_Seurat", status = "ok", detail = "v4.3.0"))
                       # DoubletFinder 没给 → 缺
r1 <- runnable_chain(env_nopkg, "server")
ok("缺必需包 → no", identical(r1$verdict, "no"), r1$verdict)
ok("指名缺哪个包、影响哪一环",
   length(r1$blocking) > 0L &&
     any(vapply(r1$blocking, function(s)
       any(vapply(s$gaps, function(g) identical(g$name, "DoubletFinder"), TRUE)), TRUE)))

env_full <- env_ok
env_full$items <- list(
  list(id = "pkg_Seurat", status = "ok", detail = "v4.3.0"),
  list(id = "pkg_DoubletFinder", status = "ok", detail = "v2.0.3"),
  list(id = "pkg_ggplot2", status = "ok", detail = "v3.4"),
  list(id = "pkg_clusterProfiler", status = "ok", detail = "v4.0"),
  list(id = "data_root", status = "ok", detail = "/data"),
  list(id = "script_root", status = "ok", detail = "/script"),
  list(id = "links", status = "ok", detail = "7 个链接都能解析", value = 7L))
r2 <- runnable_chain(env_full, "server")
ok("条件齐备 → yes", identical(r2$verdict, "yes"), r2$verdict)

cat("\n=== G1 判断留痕 ===\n")
did <- decision_log(con, "pick_script", basis = "flow_spec 规定",
                    quote = "清单 E1 要求规则只写一处", outcome = "选用 02_annotate.R",
                    owner_key = "local:default", version_id = vid, step_id = "annotate")
ok("写一条留痕成功", nzchar(did))
tr <- decision_trace(con, vid, "annotate")
ok("能从环节反查回来", length(tr) == 1L && identical(tr[[1]]$kind, "pick_script"))
ok("反查带回依据与原文",
   nzchar(tr[[1]]$basis %||% "") && nzchar(tr[[1]]$quote %||% ""))

cat("\n=== G2 pipeline 建议 ===\n")
# fullmark 被谁依赖？answer: 没有下游依赖它 → 允许
s1 <- pipeline_suggest(TMP, "drop", "fullmark", reason = "终版已覆盖")
ok("删无下游依赖的环节：允许并出草稿", isTRUE(s1$ok) && file.exists(s1$draft),
   s1$error %||% "")
# final 被 annotate 等多个环节依赖 → 必须拒
s2 <- pipeline_suggest(TMP, "drop", "final", reason = "想删")
ok("删有下游依赖的环节：拒绝", isFALSE(s2$ok), "居然允许了")
ok("拒绝时指出谁依赖它", length(s2$consumers %||% character(0)) > 0L,
   paste(s2$consumers %||% character(0), collapse = ","))

cat("\n=== G4 收敛 ===\n")
mk_att <- function(no, md5s) {
  payload <- jsonlite::toJSON(list(artifacts = lapply(md5s, function(m)
    list(rel = "a.rds", md5 = m))), auto_unbox = TRUE)
  DBI::dbExecute(con, "INSERT INTO step_attempts(attempt_id,run_id,version_id,owner_key,step_id,
                   attempt_no,status,reported_at,payload) VALUES(?,?,?,?,?,?,?,?,?)",
    list(paste0("att-c", no), rid, vid, "local:default", "merge", no, "verified",
         db_now(), payload))
}
mk_att(1L, "aaa"); mk_att(2L, "aaa")
c1 <- orch_convergence(con, vid, "merge")
ok("两次 md5 一致 → converged=TRUE",
   isTRUE((c1$by_stage$merge %||% list())$converged), c1$why %||% "")

mk_att(3L, "bbb")
c2 <- orch_convergence(con, vid, "merge")
ok("md5 不一致 → converged=FALSE",
   isFALSE((c2$by_stage$merge %||% list())$converged), c2$why %||% "")

# 没有 md5 的两次：判不了，不能当成「不一致」
DBI::dbExecute(con, "INSERT INTO step_attempts(attempt_id,run_id,version_id,owner_key,step_id,
                 attempt_no,status,reported_at,payload) VALUES(?,?,?,?,?,?,?,?,?)",
  list("att-n1", rid, vid, "local:default", "enrich", 1L, "verified", db_now(),
       '{"artifacts":[{"rel":"x"}]}'))
DBI::dbExecute(con, "INSERT INTO step_attempts(attempt_id,run_id,version_id,owner_key,step_id,
                 attempt_no,status,reported_at,payload) VALUES(?,?,?,?,?,?,?,?,?)",
  list("att-n2", rid, vid, "local:default", "enrich", 2L, "verified", db_now(),
       '{"artifacts":[{"rel":"y"}]}'))
c3 <- orch_convergence(con, vid, "enrich")
ok("没有 md5 时判不了（不是判不一致）",
   is.na((c3$by_stage$enrich %||% list())$converged), c3$why %||% "")

DBI::dbDisconnect(con)
unlink(TMP, recursive = TRUE)

cat(sprintf("\n结果：%d 通过，%d 失败\n", PASS, FAIL))
if (FAIL > 0L) {
  cat("失败项：\n"); cat(paste0("  - ", FAILED, collapse = "\n"), "\n")
  quit(status = 1L)
}
cat("PASS: 画像 / 依赖自检 / 编排 / 三档环境 / 留痕 / 建议 / 收敛\n")
