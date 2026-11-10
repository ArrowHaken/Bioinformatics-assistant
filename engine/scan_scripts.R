#!/usr/bin/env Rscript
# =====================================================================
#  scan_scripts.R -- 这台机器上有没有现成的、能直接用的分析脚本
#
#  为什么必须先查这一件事
#    对方的机器上很可能已经有脚本，而且那套是通过验收的。
#    如果我再拷一套过去，两套并行跑同样的数据 —— 早晚分叉。
#    分叉的可怕之处不是「哪套快」，而是同一个问题有了两个答案，
#    事后说不清哪个算数。
#
#    所以：先看有什么，再决定用哪套。这个脚本只读，不动任何文件。
#
#  两种用法
#    命令行（被 decision.R 调用，也方便人工单独查）：
#      Rscript scan_scripts.R [--out <scan.json>] [--budget 20]
#    库（被 env_check.R source 进来复用同一个实现）：
#      source("scan_scripts.R")   # 定义函数，不执行扫描
#      scan_existing_scripts()
#
#  一条函数、两处复用，不是各写一份 —— 两处各写一份迟早不一致，
#  而「体检说没有、判定说有」这种矛盾会直接让人不信任这套工具。
#
#  扫描范围刻意保守：常见用户目录，每个只看一级，限时。
#  宁可漏报让人自己填，也不要为了「扫得全」卡住几分钟 ——
#  体检要是慢到让人等，就不会有人愿意跑它。
#
#  ASCII-only source（R 4.1.2 + 中文 Windows locale 不能解析非 ASCII 源码）。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

# 这个必须放最前面：下面（含命令行分支）要用到它。
# R 虽然是整体先解析，但赋值是按顺序执行的 —— 放在文件末尾，
# 前面的代码在运行时就取不到它，报 "could not find function".
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

IS_WIN <- .Platform$OS.type == "windows"

#' 扫一遍机器上可能是「一套流程」的 R 脚本库
#'
#' @param budget_sec 总时限（秒）。到点就停，返回已找到的。
#' @param extra_dirs 额外要看的目录（比如数据目录附近）
#' @return list，每项含 path / n_r / staged / has_doc / sample
scan_existing_scripts <- function(budget_sec = 20, extra_dirs = character(0)) {
  t0 <- Sys.time()
  elapsed <- function() as.numeric(difftime(Sys.time(), t0, units = "secs"))
  over <- function() elapsed() > budget_sec

  home <- path.expand("~")
  bases <- c(file.path(home, "Desktop"), file.path(home, "Documents"),
             file.path(home, "Downloads"), home)
  if (IS_WIN) {
    for (L in LETTERS) {
      d <- paste0(L, ":/")
      if (dir.exists(d)) bases <- c(bases, d)
    }
  }
  if (length(extra_dirs)) bases <- c(extra_dirs, bases)
  bases <- unique(bases[dir.exists(bases)])

  # 目录名像脚本库才进去看。不符合的直接跳过，
  # 否则会把 data/ 这种成千上万文件的大树拖进来。
  name_pat <- "(?i)(script|脚本|scwork|sc_|seurat|single.?cell|单细胞|pipeline|流程|10x)"

  looks_like_lib <- function(d) {
    fs <- tryCatch(list.files(d, recursive = TRUE, full.names = TRUE,
                              pattern = "\\.(R|r)$"),
                   error = function(e) character(0))
    if (length(fs) < 2) return(NULL)
    # 文件多到离谱的多半是 R 包库，不是流程
    if (length(fs) > 3000) return(NULL)
    nm <- basename(fs)
    # 「一套流程」的形态：有分步编号（01_ / step1 / s1_），或带运行日志与说明
    staged <- sum(grepl("(?i)^(0?\\d[._-]|step_?\\d|s\\d_)", nm))
    has_doc <- any(grepl("(?i)(run_log|readme)",
                         basename(list.files(d, recursive = TRUE))))
    if (staged >= 2 || has_doc) {
      return(list(n_r = length(fs), staged = staged, has_doc = has_doc,
                  sample = utils::head(nm[order(nm)], 8)))
    }
    NULL
  }

  hits <- list()
  for (base in bases) {
    if (over()) break
    lv1 <- tryCatch(list.dirs(base, recursive = FALSE, full.names = TRUE),
                    error = function(e) character(0))
    for (d in lv1) {
      if (over()) break
      nm <- basename(d)
      # 跳过隐藏目录与系统目录
      if (startsWith(nm, ".") || startsWith(nm, "$")) next
      if (!nzchar(nm)) next
      if (!grepl(name_pat, nm)) next
      info <- tryCatch(looks_like_lib(d), error = function(e) NULL)
      if (is.null(info)) next
      hits[[length(hits) + 1L]] <- c(
        list(path = gsub("\\\\", "/", d),
             found_under = gsub("\\\\", "/", base),
             mtime = tryCatch(format(file.info(d)$mtime, "%Y-%m-%d"), error = function(e) NA_character_)),
        info)
    }
  }
  attr(hits, "elapsed_sec") <- round(elapsed(), 1)
  attr(hits, "bases_scanned") <- bases
  hits
}

# ---------------------------------------------------------------------
# 命令行用法：只在「本文件自己是被执行的那个脚本」时生效
#
#   这里必须看 --file= 而不是看有没有命令行参数。
#   因为本文件会被 env_check.R 用 source() 载入复用，
#   那时候命令行的 --out 是 env_check.R 的参数 ——
#   如果按「有参数就当 CLI」判断，source 进来会顺手跑一遍扫描然后 quit()，
#   把宿主脚本直接干掉。这种 bug 在日志里表现为「体检跑一半就没了」。
# ---------------------------------------------------------------------
.cli_args <- commandArgs(trailingOnly = TRUE)
.is_cli <- {
  ca <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", ca[grep("^--file=", ca)])
  length(f) > 0 && grepl("scan_scripts\\.R$", f[1], ignore.case = TRUE)
}

if (.is_cli) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cat("\n[!] scan_scripts.R needs jsonlite. install.packages('jsonlite')\n\n")
    quit(save = "no", status = 1)
  }
  get_opt <- function(k, d = NULL) {
    i <- match(k, .cli_args)
    if (is.na(i)) return(d)
    if (i == length(.cli_args) || grepl("^--", .cli_args[i + 1])) return(d)
    .cli_args[i + 1]
  }
  out_f <- get_opt("--out", NULL)
  budget <- suppressWarnings(as.numeric(get_opt("--budget", "20")))
  if (is.na(budget) || budget <= 0) budget <- 20
  extra <- get_opt("--extra", NULL)
  extra_dirs <- if (is.null(extra) || !nzchar(extra)) character(0) else
    strsplit(extra, "[,;]")[[1]]

  hits <- tryCatch(scan_existing_scripts(budget_sec = budget, extra_dirs = extra_dirs),
                   error = function(e) {
                     cat("[warn] 扫描失败：", conditionMessage(e), "\n")
                     structure(list(), elapsed_sec = NA_real_)
                   })

  cat("\n=== 机器上的分析脚本扫描 ===\n")
  if (!length(hits)) {
    cat("没扫到成体系的 R 流程脚本库。\n")
    cat(sprintf("（看了 %d 个目录，只看一级，用时 %s 秒）\n",
                length(attr(hits, "bases_scanned") %||% character(0)),
                attr(hits, "elapsed_sec") %||% "?"))
  } else {
    cat(sprintf("找到 %d 处：\n", length(hits)))
    for (h in hits) {
      cat(sprintf("  %s\n", h$path))
      cat(sprintf("      R 脚本 %d 个，其中分步脚本 %d 个%s\n",
                  h$n_r, h$staged,
                  if (isTRUE(h$has_doc)) "，带 README/RUN_LOG" else ""))
      if (length(h$sample)) cat(sprintf("      例如：%s\n", paste(h$sample, collapse = ", ")))
    }
    cat("\n注意：机器上已有脚本时，先确认用哪套，不要并行两套。\n")
    cat("     两套跑同样的数据早晚分叉，事后说不清哪个结果算数。\n")
  }

  if (!is.null(out_f)) {
    dir.create(dirname(out_f), showWarnings = FALSE, recursive = TRUE)
    payload <- list(
      ok = TRUE,
      scanned_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      elapsed_sec = attr(hits, "elapsed_sec") %||% NA_real_,
      bases = as.list(attr(hits, "bases_scanned") %||% character(0)),
      n_found = length(hits),
      hits = unname(hits)
    )
    con <- file(out_f, open = "wb")
    writeBin(charToRaw(enc2utf8(jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE,
                                                null = "null", digits = NA))), con)
    close(con)
    cat(sprintf("\n已写出：%s\n", out_f))
  }
  cat("\n")
  quit(save = "no", status = 0L)
}
