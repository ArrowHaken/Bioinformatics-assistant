# 回归用例：回写没带 exec_id 时能否绑定到本环唯一在管任务
#   背景（2026-11-18 18:13 真实事故）：
#     执行端回写 doublet 结果，带了 session_id 与 req_id，唯独没带 exec_id。
#     该环当时只有一条在管任务，且它的 req_id 与回写自报的完全一致，
#     绑定事实上是唯一的、可验证的。旧判据只认客户端自报的 exec_id，
#     于是判成"不知道你是谁"，任务挂在 running 四小时，
#     期间这一环所有回写都被挡住。
#   判据修正后：绑定归绑定，证据归证据。绑定唯一就往下判证据，
#     不能再用"绑定不明"去遮蔽"证据不足"，也不许反过来放行无证据的回写。
#   用法：Rscript regression_bound_exec.R [guard文件]  （默认 engine/exec_guard.R）

args <- commandArgs(trailingOnly = TRUE)
GUARD <- if (length(args)) args[[1]] else "engine/exec_guard.R"

ok <- 0L; bad <- character(0)
chk <- function(cond, label, info = "") {
  if (isTRUE(cond)) { ok <<- ok + 1L; cat("PASS", label, "\n") }
  else { bad <<- c(bad, paste0(label, " | ", info)); cat("FAIL", label, "|", info, "\n") }
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
STATE_DIR <- tempfile("regr_state_"); dir.create(STATE_DIR, recursive = TRUE)

WORK <- "/share/home/hhhong1/rachel_7sample_9_261118"
SCRIPT <- "/share/home/hhhong1/rachel_doublet_audit_v1/scripts/soupX_doublefinder.audit.v1.R"

# 复刻 18:13 的真实形状：一条在管任务，req_id 与回写自报完全一致
EX_ITEMS <- list(
  list(id = "ex-20261118154719-8664", step = "doublet", status = "running",
       session_id = "s-20261118153638-s3wg",
       req_id = "chain-1794988036806-o4z05wkz",
       created_at = "2026-11-18T15:47:19", claimed_at = "2026-11-18T15:48:10",
       run_dir = WORK, script = SCRIPT)
)
exec_read <- function() list(items = EX_ITEMS)
DB_READY <- FALSE; DB_CON <- NULL; db_scalar <- function(...) 0L
source(GUARD)

BASE <- list(step = "doublet", session_id = "s-20261118153638-s3wg",
             req_id = "chain-1794988036806-o4z05wkz", run_dir = WORK)

# --- 1 核心回归：没带 exec_id，但证据齐全的 done 必须放行 ---
b1 <- c(BASE, list(status = "done", exec_id = "",
                   artifacts = list(list(rel = "1.doublet/sample.rds", mtime = 1794988000, bytes = 1024)),
                   script_path = SCRIPT))
g1 <- import_active_exec_guard(b1, sess_dir = WORK)
chk(is.null(g1), "1a 未带 exec_id 且唯一定位的 done 放行", if (!is.null(g1)) as.character(g1$code) else "")

# --- 2 绑定放宽不等于放行无证据 ---
b2 <- c(BASE, list(status = "done", exec_id = "", artifacts = list(), script_path = SCRIPT))
g2 <- import_active_exec_guard(b2, sess_dir = WORK)
chk(!is.null(g2) && identical(g2$code, "exec_result_structure_missing"),
    "2a 未带 exec_id 的空 done 仍被证据层拒收",
    if (!is.null(g2)) as.character(g2$code) else "passed(bad)")

# --- 3 失败回写同样只放宽绑定，不放宽证据 ---
b3 <- c(BASE, list(status = "failed", exec_id = "", run_dir = WORK, script_path = SCRIPT))
g3 <- import_active_exec_guard(b3, sess_dir = WORK)
chk(!is.null(g3) && identical(g3$code, "exec_failure_evidence_missing"),
    "3a 未带 exec_id 的空 failed 仍被证据层拒收",
    if (!is.null(g3)) as.character(g3$code) else "passed(bad)")

b3b <- c(BASE, list(status = "failed", exec_id = "", exit_code = 1, run_dir = WORK,
                    script_path = SCRIPT, log = "Error in DoubletFinder: cannot allocate vector of size 2.1 Gb"))
chk(is.null(import_active_exec_guard(b3b, sess_dir = WORK)), "3b 未带 exec_id 的真实失败放行")

# --- 4 多条在管：绑定不唯一，必须仍拒（不能靠猜） ---
EX2 <- c(EX_ITEMS, list(
  list(id = "ex-second", step = "doublet", status = "queued",
       session_id = "s-20261118153638-s3wg",
       req_id = "chain-1794988036806-o4z05wkz",
       created_at = "2026-11-18T15:49:00", run_dir = WORK, script = SCRIPT)))
old_items <- EX_ITEMS
exec_read <- local({ x <- EX2; function() list(items = x) })
g4 <- import_active_exec_guard(b1, items = EX2, sess_dir = WORK)
chk(!is.null(g4) && identical(g4$code, "exec_active_conflict"),
    "4a 同环两条在管且未带 exec_id 仍拒",
    if (!is.null(g4)) as.character(g4$code) else "passed(bad)")
chk(!is.null(g4) && identical(g4$wrote_state, FALSE) && identical(g4$wrote_started_at, FALSE),
    "4b 拒收声明零写入")

# --- 5 自报 exec_id 仍然照旧优先，且必须真的属于本环 ---
b5 <- c(BASE, list(status = "done", exec_id = "ex-20261118154719-8664",
                   artifacts = list(list(rel = "1.doublet/sample.rds", bytes = 1)),
                   script_path = SCRIPT))
chk(is.null(import_active_exec_guard(b5, items = EX_ITEMS, sess_dir = WORK)),
    "5a 自报 exec_id 命中本环任务放行")
b5b <- c(BASE, list(status = "done", exec_id = "ex-not-mine",
                    artifacts = list(list(rel = "1.doublet/sample.rds", bytes = 1)),
                    script_path = SCRIPT))
g5b <- import_active_exec_guard(b5b, items = EX_ITEMS, sess_dir = WORK)
chk(!is.null(g5b), "5b 自报不属于本环的 exec_id 仍拒",
    if (!is.null(g5b)) as.character(g5b$code) else "passed(bad)")

# --- 6 running 回写不得借绑定改开始时间（时间归服务端任务事实） ---
b6 <- c(BASE, list(status = "running", exec_id = "", started_at = "2026-11-18T19:08:11"))
b6s <- import_started_at_guard(b6, items = EX_ITEMS)
chk(identical(b6s$started_at, "2026-11-18T15:47:19"), "6a 开始时间取服务端任务创建时间")
chk(identical(b6s$client_started_at_ignored, "2026-11-18T19:08:11"), "6b 自报时间只留核对")

# --- 7 别的环/别的记录不得被这条绑定串过去 ---
b7 <- list(step = "figs", session_id = "s-20261118153638-s3wg",
           req_id = "chain-1794988036806-o4z05wkz", status = "running", exec_id = "", run_dir = WORK)
chk(is.null(import_active_exec_guard(b7, items = EX_ITEMS, sess_dir = WORK)),
    "7a 换环节就不命中，不受影响")

cat("\n合计:", ok, "pass,", length(bad), "fail\n")
if (length(bad)) { cat(paste(bad, collapse = "\n"), "\n"); quit(status = 1) }
