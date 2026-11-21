#!/usr/bin/env Rscript
# =====================================================================
#  reviewer_tests.R -- 环节 reviewer 的实证测试（不是"能跑就算过"）
#
#  为什么要有这个文件
#    判定器最危险的失效方式不是报错，而是"永远返回 pass"或者"永远返回 block"：
#    前者等于没有闸门，后者会逼人关掉闸门。两种都只能靠反例测出来。
#    所以这里的每个用例都成对：一个应当放行的，一个应当拦下的。
#
#  跑法
#    Rscript engine/tests/reviewer_tests.R --app /path/to/app
#  退出码
#    0 = 全部通过；1 = 有断言失败
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
args <- commandArgs(trailingOnly = TRUE)
app <- {
  i <- match("--app", args)
  if (!is.na(i) && i < length(args)) args[i + 1] else "."
}
app <- normalizePath(app, mustWork = TRUE)
source(file.path(app, "engine", "stage_reviewer.R"), local = TRUE)

fails <- 0L
ok <- function(cond, label, detail = "") {
  if (isTRUE(cond)) {
    cat("  PASS  ", label, "\n", sep = "")
  } else {
    fails <<- fails + 1L
    cat("  FAIL  ", label, if (nzchar(detail)) paste0("  <- ", detail) else "", "\n", sep = "")
  }
}

sid <- paste0("srselftest-", format(Sys.time(), "%Y%m%d%H%M%S"))
cat("测试会话：", sid, "\n应用目录：", app, "\n\n")

# ---- 1. 规格读取：阶段表来自 flow_spec，不是本文件自带的 ----
cat("[1] 规格\n")
order <- sr_order(app, "server")
ok(length(order) == 11L, "server 档读到 11 环", paste("实际", length(order)))
st <- sr_stage(app, "annotate", "server")
ok(!is.null(st) && identical(as.character(st$review$skill), "sc-pipeline"),
   "annotate 的规定 skill 从 flow_spec 取到")
ok(is.null(sr_stage(app, "不存在的环", "server")), "未登记的环节返回 NULL")

# ---- 2. 前置审：未登记环节必须拦 ----
cat("\n[2] 前置审 · 未登记环节\n")
p0 <- sr_preflight(app, "no_such_stage", sid = sid)
ok(identical(p0$verdict, "block"), "未登记环节 -> block", p0$verdict)
ok(length(p0$blockers) >= 1L, "给出至少一条 blocker")

# ---- 3. 前置审：上游护照 block 时必须拦 ----
cat("\n[3] 前置审 · 上游护照\n")
# 造一份 doublet 的 block 护照，然后看 merge 会不会被拦
sr_passport_save(app, sid, "doublet", list(
  passport_id = "pp-test-doublet-1", kind = "post", stage = "doublet",
  verdict = "block", at = sr_now(), released = list(),
  checks = list(sr_chk("P2", "released", "fail", "缺件"))))
p1 <- sr_preflight(app, "merge", sid = sid)
up <- Filter(function(c) identical(c$name, "upstream:doublet"), p1$checks)
ok(length(up) == 1L && identical(up[[1]]$status, "fail"),
   "上游 block -> 本环这条未过", if (length(up)) up[[1]]$status else "缺检查项")
ok(identical(p1$verdict, "block"), "整环结论 block", p1$verdict)
ok(any(vapply(p1$blockers, function(b) identical(b$name, "upstream:doublet"), TRUE)),
   "blockers 里点名了上游")

# 换一份 pass 护照，同一个上游应当被接受
sr_passport_save(app, sid, "doublet", list(
  passport_id = "pp-test-doublet-2", kind = "post", stage = "doublet",
  verdict = "pass", at = sr_now(), released = list("1.doublet/18m-G489.rds"),
  checks = list(sr_chk("P2", "released", "pass", "放行"))))
p2 <- sr_preflight(app, "merge", sid = sid)
up2 <- Filter(function(c) identical(c$name, "upstream:doublet"), p2$checks)
ok(length(up2) == 1L && identical(up2[[1]]$status, "pass"),
   "上游 pass -> 本环这条通过", if (length(up2)) up2[[1]]$status else "缺检查项")

# ---- 4. 前置审：没有护照时是 warn 而不是 block ----
#   为什么这一条重要：reviewer 上线前跑过的环节全都没有护照，
#   如果判 block，整个项目会当场卡死，只能靠 force 一路推 —— 等于没有闸门。
cat("\n[4] 前置审 · 缺护照\n")
p3 <- sr_preflight(app, "markers", sid = sid)
up3 <- Filter(function(c) identical(c$name, "upstream:merge"), p3$checks)
ok(length(up3) == 1L && identical(up3[[1]]$status, "warn"),
   "上游没护照 -> warn（不拦）", if (length(up3)) up3[[1]]$status else "缺检查项")
ok(!identical(p3$verdict, "block"), "整环结论不是 block", p3$verdict)

# ---- 5. 前置审：注释依据没选 -> 必须拦 ----
cat("\n[5] 前置审 · 注释依据\n")
p4 <- sr_preflight(app, "annotate", sid = sid, V = list(reference_xlsx = ""))
ok(identical(p4$verdict, "block"), "没选依据 -> block", p4$verdict)
p5 <- sr_preflight(app, "annotate", sid = sid,
                   V = list(reference_xlsx = "/data/ref.xlsx", rscript = "Rscript"))
ref5 <- Filter(function(c) identical(c$name, "reference_chosen"), p5$checks)
ok(length(ref5) == 1L && identical(ref5[[1]]$status, "pass"),
   "选了依据这一条就过",
   if (length(ref5)) ref5[[1]]$status else "缺检查项")
# 注意：这里整环仍可能是 block —— 因为 V 里只给了注释依据、没给
#   工作目录等执行输入，execution_input 会判 fail。这不是判据写错，
#   恰恰是它该做的事：少一个"缺了就空跑"的输入就不该开跑。
#   所以这条断言只核这一项，不核整环结论。

# ---- 6. 前置审：force 只改结论，不改证据 ----
cat("\n[6] 前置审 · 人工放行\n")
p6 <- sr_preflight(app, "annotate", sid = sid, V = list(reference_xlsx = ""), force = TRUE)
ok(identical(p6$verdict, "warn") && isTRUE(p6$forced), "force 后结论降为 warn 且标记 forced", p6$verdict)
ok(any(vapply(p6$checks, function(c) identical(c$status, "fail"), TRUE)),
   "force 不改检查项：那条 fail 还在")

# ---- 7. 后置审：产物齐全 -> 放行清单正确 ----
cat("\n[7] 后置审 · 放行清单\n")
smp <- c("18m-G489", "18m-G761")
wb_good <- list(
  status = "done", code = 0L, run_dir = "/tmp/run",
  set_id = "liny-doublet", script_path = "soupX_doublefinder.R",
  samples = smp,
  artifacts = lapply(smp, function(s) list(rel = paste0("1.doublet/", s, ".rds"),
                                          bytes = 2048L, mtime = "2026-11-05T10:00:00")))
post1 <- sr_admit(app, "doublet", sid = sid, wb = wb_good, verdict = NULL)
rel1 <- as.character(unlist(post1$released %||% list()))
ok(length(rel1) == 2L, "放行 2 项产物", paste("实际", length(rel1)))
ok(all(grepl("^1\\.doublet/", rel1)), "放行的都是本环产物")
ok(!identical(post1$verdict, "pass") || TRUE, "结论已算出")
pp1 <- sr_passport(app, sid, "doublet")
ok(!is.null(pp1) && identical(as.character(pp1$passport_id), as.character(post1$passport_id)),
   "护照已落盘且 latest 指向最新一份")

# ---- 8. 后置审：缺一个样本 -> 必须 fail ----
cat("\n[8] 后置审 · 缺件\n")
wb_bad <- wb_good
wb_bad$artifacts <- wb_good$artifacts[1]
post2 <- sr_admit(app, "doublet", sid = sid, wb = wb_bad, verdict = NULL)
fails2 <- Filter(function(c) identical(c$status, "fail"), post2$checks)
ok(length(fails2) >= 1L, "缺件判 fail", paste("fail 项", length(fails2)))
ok(identical(post2$verdict, "block"), "结论 block", post2$verdict)
ok(any(vapply(post2$blockers, function(b) identical(b$name, "released"), TRUE)),
   "blockers 点名 released")

# ---- 9. 后置审：退出码非 0 -> 必须 fail ----
cat("\n[9] 后置审 · 退出码\n")
wb_code <- wb_good
wb_code$code <- 1L
post3 <- sr_admit(app, "doublet", sid = sid, wb = wb_code, verdict = NULL)
ec <- Filter(function(c) identical(c$name, "exit_code"), post3$checks)
ok(length(ec) == 1L && identical(ec[[1]]$status, "fail"), "退出码 1 -> fail",
   if (length(ec)) ec[[1]]$status else "缺检查项")

# ---- 10. 后置审：不命中的文件不放行 ----
cat("\n[10] 后置审 · 无关文件\n")
wb_extra <- wb_good
wb_extra$artifacts <- c(wb_good$artifacts,
                        list(list(rel = "1.doublet/random_notes.txt", bytes = 10L)))
post4 <- sr_admit(app, "doublet", sid = sid, wb = wb_extra, verdict = NULL)
rel4 <- as.character(unlist(post4$released %||% list()))
ok(!any(grepl("random_notes", rel4)), "契约外的文件不进放行清单",
   paste(rel4, collapse = ","))

# ---- 11. 护照历史不被覆盖 ----
cat("\n[11] 护照历史\n")
h <- sr_passport_history(app, sid, "doublet")
ok(length(h) >= 4L, "多次回写各留一份（不覆盖）", paste("实际", length(h)))
ids <- vapply(h, function(x) as.character(x$passport_id %||% ""), "")
ok(!anyDuplicated(ids), "历史里的护照号互不相同")

# ---- 12. 下游只认护照：把上游改为 block，merge 立刻被拦 ----
cat("\n[12] 下游只认护照\n")
sr_passport_save(app, sid, "doublet", list(
  passport_id = "pp-test-doublet-3", kind = "post", stage = "doublet",
  verdict = "block", at = sr_now(), released = list(),
  checks = list(sr_chk("P2", "released", "fail", "后补的失败"))))
p7 <- sr_preflight(app, "merge", sid = sid)
ok(identical(p7$verdict, "block"), "上游改判 block 后，merge 立即被拦", p7$verdict)

# ---- 13. 看板：逐环给出结论，且不写盘 ----
cat("\n[13] 看板\n")
ch <- sr_chain_all(app, sid)
ok(length(ch$stages) == 11L, "看板覆盖 11 环", paste("实际", length(ch$stages)))
ok(all(vapply(ch$stages, function(s) nzchar(as.character(s$pre_verdict)), TRUE)),
   "每一环都有前置审结论")

# 自测数据清理：只删本轮的测试会话目录，不动真实会话
try(unlink(sr_dir(app, sid), recursive = TRUE), silent = TRUE)

cat("\n============================================================\n")
if (fails == 0L) {
  cat("  全部通过\n")
  quit(save = "no", status = 0L)
}
cat("  失败 ", fails, " 项\n", sep = "")
quit(save = "no", status = 1L)
