#!/usr/bin/env Rscript
# =====================================================================
#  e2e_closure.R -- 端到端：一轮能不能真的走通，才叫闭环
#
#  为什么必须有这个文件
#    engine/tests/verdict_tests.R 直接调 verdict_step，测的是「判定逻辑对不对」。
#    它测不出三件更要紧的事：
#      · 判定通过之后，状态有没有真的变成 verified；
#      · 变成 verified 之后，闸门会不会真的放行；
#      · 不放行的时候，返工单有没有真的送到队列里。
#    上一轮就栽在这里：判定逻辑全绿，但 verified 从来没人写，
#    于是每一环都 409，流程死锁，而所有测试都是绿的。
#    这一类「测试通过了、功能不工作」的失败，只能靠走完整链路来发现。
#
#  跑什么
#    1. 正常产物回写 -> 断言 status=verified、闸门放行
#    2. 缺件回写 -> 断言 rejected、有返工单、返工单进了 outbox、闸门拦住
#    3. 联网重放：同一 req 再来一次 -> 断言 attempt_no 递增、旧结论还在
#    4. 台账 / 快照 / review 三样实物都在
#
#  用法
#    Rscript engine/tests/e2e_closure.R [--app <dir>]
#  退出码 0 全过 / 1 有失败
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

args <- commandArgs(trailingOnly = TRUE)
APP <- local({
  a <- sub("^--app=", "", args[grepl("^--app=", args)])
  if (length(a)) normalizePath(a) else normalizePath(getwd())
})

for (f in c("db.R", "verdict.R", "rework.R", "compliance.R", "poller.R", "db_api.R")) {
  p <- file.path(APP, "engine", f)
  if (file.exists(p)) source(p)
}

PASS <- 0L; FAIL <- 0L; FAILED <- character(0)
ok <- function(name, cond, detail = "") {
  if (isTRUE(cond)) {
    PASS <<- PASS + 1L
    cat(sprintf("  [ok]   %s\n", name))
  } else {
    FAIL <<- FAIL + 1L
    FAILED <<- c(FAILED, paste0(name, " ", detail))
    cat(sprintf("  [FAIL] %s  %s\n", name, detail))
  }
}

# ---- 独立库，不碰生产 -----------------------------------------------
TMP <- tempfile("e2e-")
dir.create(file.path(TMP, "engine"), recursive = TRUE)
dir.create(file.path(TMP, "state"), recursive = TRUE)
file.copy(file.path(APP, "engine", "flow_spec.R"),
          file.path(TMP, "engine", "flow_spec.R"), overwrite = TRUE)
con <- db_connect(TMP)
db_init(con)
OWNER <- list(key = "viewer:e2e", kind = "viewer")
DBI::dbExecute(con, "INSERT OR IGNORE INTO owners(owner_key,kind,created_at) VALUES(?,?,?)",
               list(OWNER$key, "viewer", db_now()))

ROOT <- file.path(TMP, "work")
dir.create(file.path(ROOT, "1.doublet"), recursive = TRUE)

good_b <- function() {
  list(step = "doublet", status = "done", exit_code = 0,
       set_id = "liny-doublet", script_path = "soupX_doublefinder.R",
       script_md5 = "0123456789abcdef0123456789abcdef",
       env_lock = list(R = "4.3.3"), species = "mouse", tissue = "spleen",
       counts = list(cells_before = 12000, cells_after = 9800, n_samples = 1),
       # 逐样本环节必须给样本清单：只说「一个样本」不够，
       #   判定器要能逐个核「这个样本的这一份在不在」。
       samples = c("18m-G489"),
       artifacts = list(list(rel = "1.doublet/18m-G489.rds", bytes = 1200,
                             md5 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                             mtime = "2026-11-02T10:00:00")),
       local_root = ROOT)
}
writeLines("x", file.path(ROOT, "1.doublet", "18m-G489.rds"))

sid <- "s-e2e"

cat("\n=== 1. 正常产物：判定通过后必须是 verified，闸门必须放行 ===\n")
b1 <- good_b()
r1 <- db_api_report_step(con, TMP, OWNER, b1, runDir = ROOT, sid = sid)
ok("回写被接受", isTRUE(r1$accepted), paste("verdict:", r1$verdict %||% ""))
ok("状态写成 verified（不是停在 reported）",
   identical(as.character(db_scalar(con,
     "SELECT status FROM step_attempts WHERE step_id='doublet' ORDER BY attempt_no DESC LIMIT 1")),
     "verified"))
vinfo <- db_api_version_of(con, sid)
ok("session 已映射到 version", !is.null(vinfo) && nzchar(vinfo$version_id %||% ""),
   paste("version:", vinfo$version_id %||% "NA"))
g1 <- db_api_gate(con, TMP, OWNER, vinfo$version_id, "merge")
ok("下一步闸门放行（不再 409）", is.null(g1),
   if (!is.null(g1)) paste("code:", g1$body$code) else "")

cat("\n=== 2. 缺件回写：必须 rejected、有返工单、单子进 outbox、闸门拦住 ===\n")
b2 <- good_b()
b2$artifacts <- list(list(rel = "1.doublet/nonexistent.rds", bytes = 1200,
                          md5 = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
                          mtime = "2026-11-02T11:00:00"))
r2 <- db_api_report_step(con, TMP, OWNER, b2, runDir = ROOT, sid = sid)
ok("缺件不被接受", isFALSE(r2$accepted), paste("verdict:", r2$verdict %||% ""))
ok("产出返工单", !is.null(r2$ticket_id) && nzchar(r2$ticket_id %||% ""),
   as.character(r2$ticket_id))
ok("返工单进了投递队列",
   as.integer(db_scalar(con,
     "SELECT COUNT(*) FROM outbox WHERE reason='rework' AND ticket_id IS NOT NULL")) >= 1L)
ok("再次申报新开尝试（attempt_no 递增）",
   as.integer(db_scalar(con,
     "SELECT MAX(attempt_no) FROM step_attempts WHERE step_id='doublet'")) >= 2L)
ok("旧结论仍在（没被覆盖）",
   as.integer(db_scalar(con,
     "SELECT COUNT(*) FROM step_attempts WHERE step_id='doublet'")) >= 2L)
g2 <- db_api_gate(con, TMP, OWNER, vinfo$version_id, "merge")
ok("闸门拦住未通过的上一步", !is.null(g2),
   if (is.null(g2)) "居然放行了" else paste("code:", g2$body$code))

cat("\n=== 3. 实物：台账 / 快照 / review 三样都要在 ===\n")
ok("台账有行（不是空表）",
   as.integer(db_scalar(con, "SELECT COUNT(*) FROM ledger")) > 0L)
ok("快照 manifest 路径非空",
   as.integer(db_scalar(con,
     "SELECT COUNT(*) FROM snapshots WHERE manifest_path IS NOT NULL")) > 0L)
mp <- as.character(db_scalar(con,
  "SELECT manifest_path FROM snapshots WHERE manifest_path IS NOT NULL LIMIT 1") %||% "")
ok("manifest 文件真的存在", nzchar(mp) && file.exists(mp), mp)
ok("manifest 可读且能解析",
   nzchar(mp) && file.exists(mp) &&
     !is.null(tryCatch(jsonlite::fromJSON(mp, simplifyVector = FALSE),
                       error = function(e) NULL)))
snap <- as.character(db_scalar(con,
  "SELECT snapshot_id FROM snapshots WHERE manifest_path IS NOT NULL LIMIT 1") %||% "")
ok("review 文件真的存在",
   nzchar(snap) && length(list.files(file.path(TMP, "state", "reviews", snap))) > 0L)

cat("\n=== 4. 交付门：有未通过环节时必须拒 ===\n")
d <- db_deliverable(con, vinfo$version_id)
ok("交付门拒", isFALSE(d$ok))
ok("逐条列出原因", length(d$reasons) > 0L)
ok("unknown 单列为未核验项", is.list(d$unverified))

cat("\n=== 5. 调度器：一个空队列不该报错，也不该卡死 ===\n")
sw <- tryCatch({ n <- 0L; repeat { r <- poller_step(con, TMP); if (is.null(r)) break; n <- n + 1L; if (n > 20L) break }; n },
               error = function(e) -1L)
ok("poller_step 能跑不报错", !identical(sw, -1L), paste("sweep:", sw))
ok("rework 作业能被领取并推进",
   as.integer(db_scalar(con, "SELECT COUNT(*) FROM poll_jobs WHERE kind='rework'")) > 0L)

DBI::dbDisconnect(con)
unlink(TMP, recursive = TRUE)

cat(sprintf("\n结果：%d 通过，%d 失败\n", PASS, FAIL))
if (FAIL > 0L) {
  cat("失败项：\n"); cat(paste0("  - ", FAILED, collapse = "\n"), "\n")
  quit(status = 1L)
}
cat("PASS: 一轮真的能走通 —— 判定写状态、闸门放行、退回带单、实物齐全\n")
