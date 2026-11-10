#!/usr/bin/env Rscript
# =====================================================================
#  stat_xls.R —— 生成 1.doublet/<样本>_stat.xls
#  ---------------------------------------------------------------------
#  为什么需要它（这条是根因，不是补充说明）：
#     服务器上那套 soupX_doublefinder.R 只保存 rds 与
#     sample_raw_cellnumber.xls，**它里面没有任何一行写 _stat.xls**。
#     而流程把「写 _stat.xls」列为这一阶段的环节之一。
#     于是这个文件只能靠人临时补 —— 补出来的东西没有出处、没有时间戳、
#     和下一个人跑的结果对不上，也没人知道它是不是这一次的。
#     所以它必须由应用自带、按需下发、跑完带指纹，而不是"上次留下的那份"。
#
#  设计上刻意的两条：
#    1. 只写**能从文件里读出来的数**。不给 raw/kept 之外的中间量编数。
#       服务器脚本没有把 after_df / singlet 落盘，事后无法还原真值，
#       所以这里不输出它们 —— 宁可少一列，也不许有假列。
#    2. 每行都带 rds 的 mtime 与字节数。判断"这份 stat 是不是这次跑的"
#       靠的就是这两个字段，而不是文件自己的创建时间。
#
#  用法：
#    Rscript stat_xls.R --doublet-dir <1.doublet 目录> --samples <逗号分隔>
#                       [--raw-xls <sample_raw_cellnumber.xls>] [--out <输出路径>]
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

die <- function(...) { cat(paste0(...), "\n", file = stderr()); quit(status = 1) }

parse_args <- function(a) {
  out <- list(doublet_dir = "", samples = "", raw_xls = "", out = "")
  i <- 1
  while (i <= length(a)) {
    k <- a[i]
    v <- if (i < length(a)) a[i + 1] else ""
    if (k == "--doublet-dir") out$doublet_dir <- v
    else if (k == "--samples") out$samples <- v
    else if (k == "--raw-xls") out$raw_xls <- v
    else if (k == "--out") out$out <- v
    i <- i + 2
  }
  out
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
if (!nzchar(args$doublet_dir)) die("缺少 --doublet-dir")
if (!nzchar(args$samples))     die("缺少 --samples")

dd <- args$doublet_dir
if (!dir.exists(dd)) die("目录不存在：", dd)

samples <- trimws(strsplit(args$samples, ",")[[1]])
samples <- samples[nzchar(samples)]
if (!length(samples)) die("--samples 为空")

# ---- raw 细胞数：只从服务器脚本真正写出来的那份表里取 ----
raw_map <- list()
if (nzchar(args$raw_xls)) {
  cand <- c(args$raw_xls,
            file.path(dd, basename(args$raw_xls)),
            file.path(dirname(dd), basename(args$raw_xls)))
  hit <- cand[file.exists(cand)]
  if (length(hit)) {
    t0 <- tryCatch(read.delim(hit[1], header = TRUE, check.names = FALSE),
                   error = function(e) NULL)
    if (!is.null(t0) && nrow(t0) >= 1) {
      nm_col <- intersect(c("sample", "Sample", "SAMPLE"), colnames(t0))
      nv_col <- intersect(c("raw_number", "raw", "raw_cells", "raw_n"), colnames(t0))
      if (length(nm_col) && length(nv_col)) {
        for (r in seq_len(nrow(t0))) {
          raw_map[[as.character(t0[[nm_col[1]]][r])]] <- suppressWarnings(
            as.integer(t0[[nv_col[1]]][r]))
        }
      }
    }
  }
}

# 刻意不 requireNamespace("Seurat")：
#   readRDS 反序列化不需要加载 Seurat，取细胞数只用 ncol()。
#   多加载一个包在服务器上要几分钟，而且会把内存顶上去 —— 这一步没必要。
rows <- list()
for (s in samples) {
  f <- file.path(dd, paste0(s, ".rds"))
  if (!file.exists(f)) {
    rows[[length(rows) + 1]] <- data.frame(
      sample = s, raw_number = if (!is.null(raw_map[[s]])) raw_map[[s]] else NA_integer_,
      kept_cells = NA_integer_, rds_bytes = NA_real_, rds_mtime = NA_character_,
      status = "missing_rds", stringsAsFactors = FALSE)
    next
  }
  st <- file.info(f)
  ncell <- NA_integer_
  status <- "ok"
  ncell <- tryCatch({
    obj <- readRDS(f)
    as.integer(ncol(obj))
  }, error = function(e) NA_integer_)
  if (is.na(ncell)) status <- "read_failed"
  rows[[length(rows) + 1]] <- data.frame(
    sample = s,
    raw_number = if (!is.null(raw_map[[s]])) raw_map[[s]] else NA_integer_,
    kept_cells = ncell,
    rds_bytes = as.numeric(st$size),
    rds_mtime = format(st$mtime, "%Y-%m-%dT%H:%M:%S"),
    status = status,
    stringsAsFactors = FALSE)
}

tab <- do.call(rbind, rows)

# ---- 出处字段：写清这份表是谁生成的、跑的是什么 ----
tab$generated_at   <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
tab$generated_by   <- "app:engine/library/C_supplement/stat_xls.R"
tab$doublet_dir    <- normalizePath(dd, mustWork = FALSE)
tab$columns_note   <- "raw_number 来自 sample_raw_cellnumber.xls；kept_cells 为 rds 实际细胞数；服务器脚本未落盘 after_df/singlet，故不输出"

out <- if (nzchar(args$out)) args$out else file.path(dd, "doublet_stat.xls")
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
write.table(tab, file = out, sep = "\t", quote = FALSE, row.names = FALSE)

bad <- tab$status != "ok"
cat(sprintf("写出 %s（%d 行，异常 %d 行）\n", out, nrow(tab), sum(bad)))
if (any(bad)) {
  cat("异常行：\n")
  print(tab[bad, c("sample", "status")], row.names = FALSE)
  quit(status = 2)
}
quit(status = 0)
