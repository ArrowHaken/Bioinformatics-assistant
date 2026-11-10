#!/usr/bin/env Rscript
# =====================================================================
#  partition.R -- 私有数据按使用者分区（B2）
#
#  要解决的问题
#    这些全是「全局单份」：state/sessions/、state/quality/、state/decisions/、
#    state/mirror/。谁进来都看到同一批记录。
#    更要紧的是删除与覆盖：清一次缓存，两个人的镜像一起没了。
#
#  为什么必须「复制」而不是「搬走」
#    老的路径仍被 sessions.R / quality.R / decisions.R 直接读。
#    搬走等于把当前能用的功能弄坏，而分区本身还没验证过。
#    所以：先复制一份到分区目录，验证新路径能读，再决定要不要删老的。
#    在这一步之前，老文件一个都不动。删数据是不可逆的，改路径是可逆的。
#
#  归属怎么定
#    老数据产生时还没有使用者边界，无法事后判断属于谁。
#    统一记 legacy，页面上标「历史（未归属）」。
#    硬塞给某个人等于伪造归属 —— 那比不分区更糟。
#
#  用法
#    Rscript engine/partition.R [--app <dir>] [--dry-run]
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

PARTITION_LEGACY_OWNER <- "legacy"

# 哪些目录是私有（换个使用者该看到不同内容）。
#   判据很简单：「换个使用者该不该看到不同内容」。不同的是私有，相同的是共享。
#   共享资产留在 state/ 根下：参考词典、流程定义、白名单、模板。
PARTITION_PRIVATE_DIRS <- c("sessions", "quality", "decisions", "mirror", "reviews", "rework")

partition_owner_dir <- function(app_dir, owner_key) {
  ok <- gsub("[^A-Za-z0-9_.:@-]", "_", as.character(owner_key %||% "anonymous"))
  d <- file.path(app_dir, "state", "users", ok)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

# 递归复制：只复制，不删除；目标已存在则跳过（幂等）。
partition_copy_tree <- function(from, to, dry_run = FALSE) {
  if (!dir.exists(from)) return(list(ok = TRUE, files = 0L, skipped = 0L, why = "源不存在"))
  if (dry_run) {
    fs <- list.files(from, recursive = TRUE, all.files = TRUE, no.. = TRUE)
    return(list(ok = TRUE, files = length(fs), skipped = 0L, why = "dry-run"))
  }
  fs <- list.files(from, recursive = TRUE, all.files = TRUE, no.. = TRUE,
                   full.names = TRUE)
  n <- 0L; skip <- 0L
  for (f in fs) {
    info <- file.info(f)
    if (isTRUE(info$isdir)) next
    rel <- sub(paste0("^", gsub("([\\W])", "\\\\\\1", normalizePath(from, winslash = "/")), "/?"),
               "", normalizePath(f, winslash = "/"))
    dst <- file.path(to, rel)
    if (file.exists(dst) && file.info(dst)$size == info$size) { skip <- skip + 1L; next }
    dir.create(dirname(dst), showWarnings = FALSE, recursive = TRUE)
    ok <- tryCatch(file.copy(f, dst, overwrite = FALSE), error = function(e) FALSE)
    if (isTRUE(ok)) n <- n + 1L else skip <- skip + 1L
  }
  list(ok = TRUE, files = n, skipped = skip, why = "")
}

partition_run <- function(app_dir, dry_run = FALSE, verbose = TRUE) {
  say <- function(...) if (verbose) cat(..., "\n", sep = "")
  dst_root <- partition_owner_dir(app_dir, PARTITION_LEGACY_OWNER)
  say("分区目标：", dst_root)
  out <- list()
  for (d in PARTITION_PRIVATE_DIRS) {
    src <- file.path(app_dir, "state", d)
    if (!dir.exists(src)) { say("  ", d, "：没有这个目录，跳过"); next }
    r <- partition_copy_tree(src, file.path(dst_root, d), dry_run = dry_run)
    say("  ", d, "：复制 ", r$files, " 个，跳过 ", r$skipped, " 个（已存在）")
    out[[d]] <- r
  }
  # 记录一次，说明这些数据是「未归属的历史」
  if (!dry_run) {
    mf <- file.path(dst_root, "PARTITION.md")
    writeLines(c(
      "# 历史数据（未归属）", "",
      paste0("生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      "",
      "这些文件是从 state/ 根目录**复制**过来的，原件仍在原处、没有删除。",
      "",
      "为什么记成未归属：这些数据产生时还没有使用者边界，事后无法判断属于谁。",
      "硬塞给某个人等于伪造归属，所以统一记 legacy，页面上标「历史（未归属）」。",
      "",
      "新数据应该写到这里之外的 state/users/<owner_key>/ 下。",
      "要清理旧位置，先确认新路径能读通，再手动删 —— 这一步刻意没做成自动的。"
    ), mf, useBytes = TRUE)
    say("  说明写入 ", mf)
  }
  invisible(out)
}

partition_cli <- function() {
  a <- commandArgs(trailingOnly = TRUE)
  get <- function(k, d = NULL) { i <- match(k, a); if (is.na(i) || i == length(a)) return(d); a[i + 1] }
  app <- get("--app", getwd())
  partition_run(app, dry_run = "--dry-run" %in% a)
}

if (sys.nframe() == 0L && !interactive()) partition_cli()
