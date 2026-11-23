#!/usr/bin/env Rscript
# 派工前建身份契约：记录没有版本号时，必须先建号再派工，否则作业白跑一趟。
#
# 为什么必须有（2026-11-23，12:18 那次真实点击失败后补）：
#   新建的分析记录在库里 project_id 与 version_id 都是空。
#   派工时取不到版本号，db_register_dispatch 直接返回 dispatch_binding_unavailable，
#   attempt 根本建不起来。执行端领了活、跑完来交结果，服务端认不出这属于哪一次尝试，
#   403 run_log_not_owner —— 服务器上一个细胞都没算，8 秒判失败。
#   实测本机 14 条记录里 11 条 version_id 为空，是通病不是偶发。
#
# 八份既有契约没抓到的原因：它们都先 seed 好 version 与 attempt，
#   验的是"身份建好之后能不能用"，没验"身份根本没建起来"这条路。
#
# 判据：
#   1) 对照组：记录没版本时直接派工，必须失败并报 dispatch_binding_unavailable
#      （这一条就是修复前的真实行为，用它证明缺陷存在）
#   2) 修复组：db_ensure_session_version 能把 project/version 建出来
#   3) 建完写回 sessions，不是只在内存里算一次
#   4) 建完之后再派工，attempt 能建起来
#   5) 静态：派工入口确实调用了这个函数
#
# 用法：Rscript tests/dispatch_version_bootstrap_contract.R [app_dir]

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

args <- commandArgs(trailingOnly = TRUE)
app_src <- normalizePath(if (length(args)) args[[1]] else ".", mustWork = TRUE)

fail <- character()
check <- function(label, ok) {
  cat(if (isTRUE(ok)) "PASS:" else "FAIL:", label, "\n")
  if (!isTRUE(ok)) fail <<- c(fail, label)
}

root <- tempfile("dispatch-version-")
dir.create(root)
on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
stopifnot(file.symlink(file.path(app_src, "engine"), file.path(root, "engine")))
if (dir.exists(file.path(app_src, "public")))
  stopifnot(file.symlink(file.path(app_src, "public"), file.path(root, "public")))
stopifnot(file.symlink(file.path(app_src, "serve.R"), file.path(root, "serve.R")))
dir.create(file.path(root, "state"), recursive = TRUE)
dir.create(file.path(root, "config"), recursive = TRUE)

old <- options(pa.serve.definitions_only = TRUE, pa.app_dir = root)
e <- new.env(parent = globalenv())
sys.source(file.path(root, "serve.R"), envir = e)
options(old)

con <- tryCatch(e$db_connect(root), error = function(err) NULL)
if (is.null(con)) {
  check("temporary database opens", FALSE)
  cat("\nFAILED: temporary database opens\n")
  quit(save = "no", status = 1L)
}
okInit <- tryCatch({ e$db_init(con); TRUE }, error = function(err) FALSE)
check("temporary database initialises", isTRUE(okInit))

sid <- "s-contract-noversion"
octx <- "owner-contract"
# 故意不写 project_id / version_id，复现新建记录的真实形态
tryCatch(DBI::dbExecute(con,
  paste0("INSERT INTO sessions(session_id,owner_key,name,visibility,created_at) ",
         "VALUES(?,?,?,'private',?)"),
  list(sid, octx, "契约·无版本记录", e$db_now())),
  error = function(err) NULL)
row0 <- tryCatch(e$db_rows(con, "SELECT project_id, version_id FROM sessions WHERE session_id = ?",
                           list(sid)), error = function(err) NULL)
seeded_null <- !is.null(row0) && nrow(row0) == 1L &&
  (is.na(row0$project_id[1]) || !nzchar(as.character(row0$project_id[1]))) &&
  (is.na(row0$version_id[1]) || !nzchar(as.character(row0$version_id[1])))
check("fixture session has no project/version", isTRUE(seeded_null))

# ---- 1) 对照组：没版本直接派工，必须失败（修复前的真实行为）----------
before <- tryCatch(
  e$db_register_dispatch(con, owner_key = octx, session_id = sid, version_id = "",
                         step_id = "doublet", req_id = "req-before", exec_id = "ex-before"),
  error = function(err) list(ok = FALSE, code = "threw", error = conditionMessage(err)))
check("dispatch without a version is refused",
      !isTRUE(before$ok) &&
        identical(as.character(before$code %||% ""), "dispatch_binding_unavailable"))

# ---- 2) 修复组：建号 -------------------------------------------------
made <- tryCatch(e$db_ensure_session_version(con, sid, octx),
                 error = function(err) list(ok = FALSE, code = conditionMessage(err)))
check("version bootstrap succeeds", isTRUE(made$ok))
check("bootstrap returns a non-empty version id",
      nzchar(as.character(made$version_id %||% "")))
check("bootstrap returns a non-empty project id",
      nzchar(as.character(made$project_id %||% "")))

# ---- 3) 必须写回记录 ------------------------------------------------
row1 <- tryCatch(e$db_rows(con, "SELECT project_id, version_id FROM sessions WHERE session_id = ?",
                           list(sid)), error = function(err) NULL)
check("sessions row is written back",
      !is.null(row1) && nrow(row1) == 1L &&
        identical(as.character(row1$version_id[1]), as.character(made$version_id)) &&
        identical(as.character(row1$project_id[1]), as.character(made$project_id)))

# ---- 4) 建完之后再派工，attempt 能建起来 ----------------------------
after <- tryCatch(
  e$db_register_dispatch(con, owner_key = octx, session_id = sid,
                         version_id = as.character(made$version_id),
                         step_id = "doublet", req_id = "req-after", exec_id = "ex-after"),
  error = function(err) list(ok = FALSE, code = "threw", error = conditionMessage(err)))
check("dispatch succeeds once the version exists", isTRUE(after$ok))
check("dispatch returns an attempt id", nzchar(as.character(after$attempt_id %||% "")))
# 用 attempt_id 直接查：step_attempts 表没有 session_id 列，
#   先前那句 WHERE session_id = ? 会直接 SQL 报错，测出来的"失败"是查询写错，不是实现错。
att_cnt <- tryCatch(e$db_scalar(con,
  "SELECT COUNT(*) FROM step_attempts WHERE attempt_id = ?",
  list(as.character(after$attempt_id %||% ""))), error = function(err) NA_real_)
check("attempt row really exists in the database",
      is.finite(as.numeric(att_cnt)) && as.numeric(att_cnt) >= 1)

# ---- 5) 静态：派工入口确实调用了这个函数 ---------------------------
enq_src <- readLines(file.path(app_src, "engine", "run_enqueue.R"), warn = FALSE)
check("dispatch entry calls db_ensure_session_version",
      any(grepl("db_ensure_session_version", enq_src)))
check("dispatch entry refuses instead of warning when binding fails",
      any(grepl("dispatch_binding_unavailable", enq_src)))

tryCatch(DBI::dbDisconnect(con), error = function(err) NULL)

if (length(fail)) {
  cat("\nFAILED:", paste(fail, collapse = "; "), "\n")
  quit(save = "no", status = 1L)
}
cat("\ndispatch version bootstrap contract\n")