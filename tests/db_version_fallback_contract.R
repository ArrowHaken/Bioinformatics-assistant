#!/usr/bin/env Rscript
# 版本兜底契约：空版本号不许把回写炸成 NOT NULL 失败。
#
# 为什么必须有（2026-11-23，云准助手指出、本机核实）：
#   step_attempts.version_id 是 NOT NULL。会话没有版本号时若把 NA 原样带进去，
#   整份回写会被判 db_report_error —— 作业在集群上真跑完了，结果却写不回来。
#   db_api.R 的注释记录过 2026-11-12 那次丢结果就是这种形态，之后补了建号兜底。
#   兜底在，但没有任何测试钉住它；一改就可能悄悄回退。
#
# 判据：
#   1) 表结构：step_attempts.version_id 必须是 NOT NULL（兜底失效时会立刻炸）
#   2) 报工路径：插入前必须调用 db_ensure_version
#   3) 行为：空串与 NA 都能换到有效版本号，不会原样返回空
#
# 用法：Rscript tests/db_version_fallback_contract.R [app_dir]

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

args <- commandArgs(trailingOnly = TRUE)
app_src <- normalizePath(if (length(args)) args[[1]] else ".", mustWork = TRUE)

fail <- character()
check <- function(label, ok) {
  cat(if (isTRUE(ok)) "PASS:" else "FAIL:", label, "\n")
  if (!isTRUE(ok)) fail <<- c(fail, label)
}

# ---- 1) 表结构 -----------------------------------------------------
db_src <- readLines(file.path(app_src, "engine", "db.R"), warn = FALSE)
st <- grep("CREATE TABLE IF NOT EXISTS step_attempts", db_src)
block <- if (length(st)) db_src[st[1]:min(length(db_src), st[1] + 20L)] else character(0)
check("step_attempts.version_id is NOT NULL",
      any(grepl("version_id\\s+TEXT\\s+NOT NULL", block)))

# ---- 2) 报工路径必须有建号兜底 -------------------------------------
api_src <- readLines(file.path(app_src, "engine", "db_api.R"), warn = FALSE)
check("report path ensures a version before insert",
      any(grepl("db_ensure_version", api_src)))

# ---- 3) 行为：空与 NA 都能换成有效版本 -----------------------------
root <- tempfile("db-version-")
dir.create(root)
on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

ok_load <- TRUE
e <- new.env(parent = globalenv())
options(pa.serve.definitions_only = TRUE, pa.app_dir = app_src)
load_err <- tryCatch({
  sys.source(file.path(app_src, "serve.R"), envir = e)
  NULL
}, error = function(err) conditionMessage(err))
if (!is.null(load_err)) {
  ok_load <- FALSE
  check("engine loads for the behavioural check", FALSE)
  cat("  载入失败：", load_err, "\n")
}

if (ok_load && exists("db_connect", envir = e, inherits = FALSE)) {
  con <- tryCatch(e$db_connect(root), error = function(err) NULL)
  if (is.null(con)) {
    check("temporary database opens", FALSE)
  } else {
    okInit <- tryCatch({ e$db_init(con); TRUE }, error = function(err) FALSE)
    check("temporary database initialises", isTRUE(okInit))
    if (isTRUE(okInit)) {
      pid <- tryCatch(e$db_ensure_project(con, "owner-contract", "contract-project"),
                      error = function(err) NA_character_)
      check("project id is created", nzchar(as.character(pid %||% "")))
      vid_empty <- tryCatch(e$db_ensure_version(con, pid, "owner-contract"),
                            error = function(err) NA_character_)
      check("empty version input yields a real version id",
            nzchar(as.character(vid_empty %||% "")) && !is.na(vid_empty))
      vid_na <- tryCatch(e$db_ensure_version(con, pid, "owner-contract", version_id = NA_character_),
                         error = function(err) NA_character_)
      check("NA version input does not come back as NA",
            nzchar(as.character(vid_na %||% "")) && !is.na(vid_na))
      # 一旦拿到版本号，它必须真的能通过 NOT NULL 落库。
      got <- tryCatch(e$db_scalar(con, "SELECT COUNT(*) FROM versions WHERE version_id = ?",
                                  list(as.character(vid_empty))),
                      error = function(err) NA_integer_)
      check("the created version is persisted", is.finite(as.numeric(got)) && as.numeric(got) >= 1)
    }
    tryCatch(DBI::dbDisconnect(con), error = function(err) NULL)
  }
} else if (ok_load) {
  check("db layer is available for the behavioural check", FALSE)
}

if (length(fail)) {
  cat("\nFAILED:", paste(fail, collapse = "; "), "\n")
  quit(save = "no", status = 1L)
}
cat("\ndb version fallback contract\n")