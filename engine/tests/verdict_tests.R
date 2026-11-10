#!/usr/bin/env Rscript
# =====================================================================
#  verdict_tests.R -- 判定器与返工闭环的回归测试
#
#  为什么必须有这个文件（清单 K3 / K4 / L2）
#    如果判定器本身判错，整个闭环会把对的判成错的，或者把错的放过。
#    而它比业务脚本更容易写错：四层、多条件、大量边界。
#    所以这里用固定样本跑，每次改判定逻辑都必须全过。
#
#  固定样本
#    ok            正常产物（应 pass）
#    no_artifact   回写 done 但无产物（应 rejected）
#    missing_file  少一件产物（应 fail，且指出缺哪件）
#    wrong_skill   set_id 不符（L3 fail，且写明规定是 X 你是 Y）
#    wrong_script  另写脚本（L3 fail，指到脚本名）
#    stale_claim   只写「已严格按要求执行」（应 unknown/fail，不通过）
#    recover       第一轮缺列、第二轮补齐（应全量重判，不继承上轮）
#    timeout       超时未回写（应催促一次，再超时 escalated）
#    release       人工放行（应记录 who/why；agent 放行应被拒）
#    immutable     events 追加写（UPDATE 应被触发器拒绝）
#    deliverable   交付门（有 rejected 就必须拒）
#
#  用法
#    Rscript engine/tests/verdict_tests.R      # 全部跑，退出码 0/1
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

args <- commandArgs(trailingOnly = TRUE)
APP <- local({
  a <- sub("^--app=", "", args[grepl("^--app=", args)])
  if (length(a)) normalizePath(a) else {
    # 从 engine/tests/ 往上两级
    normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), "..", ".."))
  }
})
if (!dir.exists(file.path(APP, "engine"))) APP <- normalizePath(getwd())

source(file.path(APP, "engine", "db.R"))
source(file.path(APP, "engine", "verdict.R"))
source(file.path(APP, "engine", "rework.R"))
source(file.path(APP, "engine", "poller.R"))

PASS <- 0L; FAIL <- 0L; NOTES <- character(0)
ok <- function(name, cond, detail = "") {
  if (isTRUE(cond)) {
    PASS <<- PASS + 1L
    cat(sprintf("  [ok]   %s\n", name))
  } else {
    FAIL <<- FAIL + 1L
    cat(sprintf("  [FAIL] %s  %s\n", name, detail))
    NOTES <<- c(NOTES, paste(name, detail))
  }
}

# ---- 独立测试库：不碰生产的 app.db -----------------------------------
TMP <- tempfile("pa-test-")
dir.create(file.path(TMP, "engine"), recursive = TRUE)
file.copy(file.path(APP, "engine", "flow_spec.R"), file.path(TMP, "engine", "flow_spec.R"))
dir.create(file.path(TMP, "state"), recursive = TRUE)
con <- db_connect(TMP)
db_init(con)

OWNER <- "viewer:test-owner"
OWNER2 <- "viewer:other-owner"
DBI::dbExecute(con, "INSERT OR IGNORE INTO owners(owner_key,kind,created_at) VALUES(?,?,?)",
               list(OWNER, "viewer", db_now()))

# 固定夹具目录：造出真的表格文件，表头可读。
#   两个根目录：ROOT 用「表头齐全」的那份，ROOT_BAD 用「缺 Positive Markers」的那份。
#   为什么不用同一份文件改内容：测试之间不能互相影响，否则前一个用例把表头改坏，
#   后面的用例会跟着红，看起来像判定器出了问题。
mk_root <- function(annot_header) {
  r <- tempfile("pa-root-")
  dir.create(file.path(r, "9.annot"), recursive = TRUE)
  writeLines(annot_header, file.path(r, "9.annot", "annot_result.xls"))
  writeLines("x", file.path(r, "9.annot", "annotated.rds"))
  writeLines(c("cluster\tn"), file.path(r, "9.annot", "celltype_count.xls"))
  r
}
ROOT <- mk_root(c("cluster\tcell_type\tPositive Markers", "0\tB cell\tCd79a"))
ROOT_BAD <- mk_root(c("cluster\tcell_type\tscore", "0\tB cell\t0.9"))

mk_attempt <- function(step, b, attempt_no = 1L) {
  pid <- db_ensure_project(con, OWNER, "测试项目")
  vid <- db_ensure_version(con, pid, OWNER)
  rid <- db_ensure_run(con, vid, OWNER, work_root = ROOT)
  aid <- db_new_id("att")
  DBI::dbExecute(con,
    "INSERT INTO step_attempts(attempt_id,run_id,version_id,owner_key,step_id,attempt_no,
       req_id,status,reported_at,payload) VALUES(?,?,?,?,?,?,?,?,?,?)",
    list(aid, rid, vid, OWNER, step, attempt_no, b$req_id %||% NA, "reported", db_now(), db_json(b)))
  list(attempt_id = aid, version_id = vid, run_id = rid)
}

cat("\n=== 1. 正常产物应通过 ===\n")
b_ok <- list(step = "annotate", status = "done", exit_code = 0,
             set_id = "sc-pipeline", species = "mouse", tissue = "spleen",
             script_path = "/x/sc-pipeline/scripts/02_annotate.R",
             script_md5 = "0123456789abcdef0123456789abcdef",
             env_lock = list(R = "4.1.2"),
             artifacts = list(
               list(rel = "9.annot/annot_result.xls", bytes = 200,
                    md5 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", mtime = "2026-11-02T10:00:00"),
               list(rel = "9.annot/annotated.rds", bytes = 2000,
                    md5 = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb", mtime = "2026-11-02T10:00:00"),
               list(rel = "9.annot/celltype_count.xls", bytes = 200,
                    md5 = "cccccccccccccccccccccccccccccccc", mtime = "2026-11-02T10:00:00")))
a1 <- mk_attempt("annotate", b_ok)
r1 <- verdict_step(con, TMP, a1$attempt_id, writeback = b_ok, local_root = ROOT)
ok("annotate 正常产物 verdict=pass 或 warn（warn 不阻断）",
   r1$verdict %in% c("pass", "warn"), paste("got", r1$verdict))
ok("通过时不建返工单", is.na(r1$ticket_id), as.character(r1$ticket_id))

cat("\n=== 2. 谎报完成（done 但无产物）===\n")
b_lie <- list(step = "annotate", status = "done", exit_code = 0, set_id = "sc-pipeline",
              script_path = "02_annotate.R", script_md5 = "ffffffffffffffffffffffffffffffff",
              artifacts = list())
a2 <- mk_attempt("annotate", b_lie, attempt_no = 2L)
r2 <- verdict_step(con, TMP, a2$attempt_id, writeback = b_lie, local_root = ROOT)
ok("无产物 verdict 不是 pass", !identical(r2$verdict, "pass"), paste("got", r2$verdict))
ok("无产物落 rejected", identical(as.character(db_scalar(
  con, "SELECT status FROM step_attempts WHERE attempt_id=?", list(a2$attempt_id))), "rejected"))
ok("无产物生成返工单", !is.na(r2$ticket_id) && nzchar(r2$ticket_id))
tk <- db_rows(con, "SELECT * FROM rework_tickets WHERE ticket_id=?", list(r2$ticket_id))
ok("返工单每项带 expected/actual/evidence",
   nrow(tk) == 1 && {
     fc <- db_unjson(tk$failed_checks[1], list())
     length(fc) > 0 && all(vapply(fc, function(f)
       nzchar(f$expected %||% "") && nzchar(f$actual %||% "") && nzchar(f$evidence %||% ""), TRUE))
   })
ok("返工单带可执行命令", nzchar(tk$command_hint[1] %||% ""))

cat("\n=== 3. 用了错的 skill ===\n")
b_skill <- b_ok; b_skill$set_id <- "liny-annot"
a3 <- mk_attempt("annotate", b_skill, attempt_no = 3L)
r3 <- verdict_step(con, TMP, a3$attempt_id, writeback = b_skill, local_root = ROOT)
l3 <- Filter(function(c) c$layer == "L3" && c$name == "set_id", r3$checks)[[1]]
ok("skill 不符判 fail", identical(r3$verdict, "fail"))
ok("写明规定是 X 实际是 Y",
   grepl("规定是 sc-pipeline", l3$detail) && grepl("liny-annot", l3$detail), l3$detail %||% "")

cat("\n=== 4. 改脚本绕过 ===\n")
b_sc <- b_ok; b_sc$script_path <- "/x/make_my_own_annot.R"
a4 <- mk_attempt("annotate", b_sc, attempt_no = 4L)
r4 <- verdict_step(con, TMP, a4$attempt_id, writeback = b_sc, local_root = ROOT)
ok("另写脚本判 fail", identical(r4$verdict, "fail"))
ok("指到具体脚本名", any(grepl("make_my_own_annot", vapply(r4$checks, function(c) c$actual %||% "", ""))))

cat("\n=== 5. 只写自述、不给可核事实 ===\n")
b_claim <- list(step = "annotate", status = "done",
                self_claim = "已严格按要求执行，全部通过", note = "一切正常",
                artifacts = list())
a5 <- mk_attempt("annotate", b_claim, attempt_no = 5L)
r5 <- verdict_step(con, TMP, a5$attempt_id, writeback = b_claim, local_root = ROOT)
ok("自述不作为通过依据", !identical(r5$verdict, "pass"), paste("got", r5$verdict))
ok("记下 ignored_self_claim",
   nrow(db_rows(con, "SELECT 1 FROM events WHERE type='ignored_self_claim' AND attempt_id=?",
                list(a5$attempt_id))) == 1)

cat("\n=== 6. 补一个便宜的不算过（全量重判）===\n")
#  第一轮：缺 Positive Markers 列
b_r1 <- b_ok
b_r1$artifacts[[1]]$rel <- "9.annot/annot_result.xls"   # 表头只有 cluster/cell_type/score
a6 <- mk_attempt("annotate", b_r1, attempt_no = 6L)
# 用 ROOT_BAD：那份 annot_result.xls 缺 Positive Markers 列。
#   用 ROOT 会读到齐全的表头，这条用例就变成「假装在测缺列」——
#   测不出问题比测出问题更糟，它会让人误以为这一层已经验过。
r6a <- verdict_step(con, TMP, a6$attempt_id, writeback = b_r1, local_root = ROOT_BAD)
ok("第一轮缺列判 fail", identical(r6a$verdict, "fail"), paste("got", r6a$verdict))
ok("缺列时指名是哪一列",
   any(grepl("Positive Markers", vapply(r6a$checks, function(c) c$detail %||% "", ""))))
#  第二轮：列补上了，但少一件产物 —— 不继承上一轮的通过项
b_r2 <- b_r1
b_r2$artifacts <- b_r2$artifacts[1:2]   # 少 celltype_count.xls
b_r2$artifacts[[1]] <- list(rel = "9.annot/annot_result.xls", bytes = 200,
                            md5 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", mtime = "2026-11-02T11:00:00")
a7 <- mk_attempt("annotate", b_r2, attempt_no = 7L)
r6b <- verdict_step(con, TMP, a7$attempt_id, writeback = b_r2, local_root = ROOT)
ok("第二轮仍 fail（不因上一轮某条过了而放行）", identical(r6b$verdict, "fail"), paste("got", r6b$verdict))
ok("指出缺的那一件",
   any(grepl("celltype_count", vapply(r6b$checks, function(c) c$name %||% "", ""))))

cat("\n=== 7. 只回写 running 不销项 / 直接写 verified 被拒 ===\n")
b_ver <- b_ok; b_ver$status <- "verified"
res_ver <- tryCatch(db_api_report_step(con, TMP,
  list(key = OWNER, kind = "viewer"), b_ver, runDir = ROOT, sid = NA),
  error = function(e) list(accepted = FALSE, code = "error", error = conditionMessage(e)))
# db_api_report_step 自身不拦 verified，拦在 HTTP 入口；这里验 HTTP 层有断言
srv <- readLines(file.path(APP, "serve.R"), warn = FALSE)
ok("HTTP 入口拒绝 status=verified", any(grepl("verified_not_writable", srv, fixed = TRUE)))
ok("verified 不在允许的回写状态表里", !any(grepl('c\\("done", "failed", "running", "delegated", "stopped", "verified"', srv)))

cat("\n=== 8. 超时：先催一次，再超时升级 ===\n")
t1 <- rework_nudge(con, r2$ticket_id)
ok("第一次超时只催促", identical(t1$action, "nudged"))
t1b <- rework_nudge(con, r2$ticket_id)
ok("第二次超时转 escalated", identical(t1b$action, "escalated"))
ok("升级后状态落库",
   identical(as.character(db_scalar(con, "SELECT state FROM rework_tickets WHERE ticket_id=?",
                                    list(r2$ticket_id))), "escalated"))

cat("\n=== 9. 人工放行：人可放、agent 不可 ===\n")
# agent 放行：身份来自调用上下文，caller_kind=agent 一律拒。
res_agent <- tryCatch({
  rework_release(con, a2$attempt_id, "claude", "我觉得没问题",
                 verified_actor = "agent:claude", caller_kind = "agent"); "allowed"
}, error = function(e) conditionMessage(e))
ok("agent 给自己放行被拒", grepl("agent 不能给自己放行", res_agent), res_agent)
# 只有自报名字、没有平台身份：也要拒（这是上一轮复核指出的漏洞）。
res_noid <- tryCatch({
  rework_release(con, a2$attempt_id, "gina", "产物已人工核对"); "allowed"
}, error = function(e) conditionMessage(e))
ok("没有平台身份不能放行", grepl("身份未识别", res_noid), res_noid)
# 真·人：平台身份 + 理由，成功；同时记录自报名与平台身份。
res_human <- tryCatch({
  rework_release(con, a2$attempt_id, "gina", "产物已人工核对",
                 verified_actor = "viewer:gina-account"); "allowed"
}, error = function(e) conditionMessage(e))
ok("人放行成功", identical(res_human, "allowed"), res_human)
ok("放行记录 who/why",
   nrow(db_rows(con, "SELECT 1 FROM events WHERE type='review_override' AND attempt_id=?",
                list(a2$attempt_id))) == 1)
ok("放行后状态 released",
   identical(as.character(db_scalar(con, "SELECT status FROM step_attempts WHERE attempt_id=?",
                                    list(a2$attempt_id))), "released"))

cat("\n=== 10. 事件账本只追加 ===\n")
iv <- tryCatch({ DBI::dbExecute(con, "UPDATE events SET type='tampered' WHERE event_id=1"); "updated" },
               error = function(e) conditionMessage(e))
ok("UPDATE events 被拒", grepl("append-only", iv), iv)
iv2 <- tryCatch({ DBI::dbExecute(con, "DELETE FROM events WHERE event_id=1"); "deleted" },
                error = function(e) conditionMessage(e))
ok("DELETE events 被拒", grepl("append-only", iv2), iv2)

cat("\n=== 11. 交付门 ===\n")
d <- db_deliverable(con, a1$version_id)
ok("有未 verified 环节时交付门拒", isFALSE(d$ok))
ok("交付门逐条列出原因", length(d$reasons) > 0)
ok("unknown/missing 单列为未核验项", is.list(d$unverified))

cat("\n=== 12. owner 隔离 ===\n")
DBI::dbExecute(con, "INSERT OR IGNORE INTO sessions(session_id,owner_key,name,created_at)
                     VALUES(?,?,?,?)", list("s-a", OWNER, "A 的记录", db_now()))
DBI::dbExecute(con, "INSERT OR IGNORE INTO sessions(session_id,owner_key,name,created_at)
                     VALUES(?,?,?,?)", list("s-b", OWNER2, "B 的记录", db_now()))
na <- as.integer(db_scalar(con, "SELECT COUNT(*) FROM sessions WHERE owner_key=?", list(OWNER)))
nb <- as.integer(db_scalar(con, "SELECT COUNT(*) FROM sessions WHERE owner_key=?", list(OWNER2)))
ok("各自只看到自己的记录", na == 1 && nb == 1, paste(na, nb))
ok("身份拿不到时 owner_require 抛错",
   inherits(tryCatch(owner_require(list(key = NA_character_)), error = function(e) e),
            "owner_unidentified"))

cat("\n=== 13. 依赖顺序只认 flow_spec ===\n")
ord <- flow_run_order_engine(TMP)
ok("顺序里有 12 环", length(ord) == 12L, paste(length(ord)))
ok("含 ppt 与 pack", all(c("ppt", "pack") %in% ord))

DBI::dbDisconnect(con)
unlink(TMP, recursive = TRUE)

cat(sprintf("\n结果：%d 通过，%d 失败\n", PASS, FAIL))
if (FAIL > 0L) { cat("失败项：\n"); cat(paste0("  - ", NOTES, collapse = "\n"), "\n"); quit(status = 1) }
cat("PASS: verdict / rework / gate / owner / immutable ledger\n")
