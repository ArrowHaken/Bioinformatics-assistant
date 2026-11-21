#!/usr/bin/env Rscript
# =====================================================================
#  extract.R -- sc-report-extract 实现脚本
#
#  从分好型的 Seurat 对象与已落盘的结果表里，抽出要写进交付报告的关键
#  指标，产出两种形态：
#    · 人读：report_text.md、report_key_info.tsv、各明细 xls
#    · 机读：report_stats.json ── 交给 sc-ppt-kit 填 PPT 第 3 页的六行指标
#
#  为什么要有机读那一份：
#    出图阶段落的 cell_number_stat.xls 只有 sample / cell_number 两列，
#    PPT 的 table_spec 却定义了六行指标（原始细胞数 / 本次对象细胞数 /
#    过滤掉细胞数 / 过滤比率 / 基因中位数 / 线粒体中位数）。只有两列时
#    表格只出一行，其余五行只能空着 —— 这就是交付稿"少了文字"的根因。
#    这些数在对象里都算得出来，缺的只是把它们按 PPT 认的格式递过去。
#
#  用法:
#    Rscript extract.R <seurat.rds> --outdir <目录>
#        [--celltype-col annot] [--group-col group] [--sample-col orig.ident]
#        [--raw-table <1.doublet/sample_raw_cellnumber.xls>]
#        [--stats-json <文件名，默认 report_stats.json>]
# =====================================================================

# ---- 包库路径：从环境变量读，不写死 ----
.lib_candidates <- c(Sys.getenv("XIAOBA_R_LIB", ""), Sys.getenv("R_LIBS_USER", ""))
.lib_candidates <- .lib_candidates[nzchar(.lib_candidates) & dir.exists(.lib_candidates)]
if (length(.lib_candidates) > 0) .libPaths(c(.lib_candidates, .libPaths()))

suppressMessages({library(Seurat); library(dplyr)})

args <- commandArgs(trailingOnly = TRUE)
get_opt <- function(k, d = NULL) { i <- match(k, args); if (is.na(i) || i == length(args)) return(d); args[i + 1] }

rds      <- args[1]
outdir   <- get_opt("--outdir", NULL)
ccol     <- get_opt("--celltype-col", "annot")
gcol     <- get_opt("--group-col", "group")
scol     <- get_opt("--sample-col", "orig.ident")
raw_tbl  <- get_opt("--raw-table", NULL)
json_nm  <- get_opt("--stats-json", "report_stats.json")

if (is.null(outdir) || !nzchar(outdir))
  stop("必须给 --outdir。", call. = FALSE)
if (!file.exists(rds))
  stop("找不到 Seurat 对象：", rds, call. = FALSE)
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

obj <- readRDS(rds)
md <- obj@meta.data
cat("载入:", ncol(obj), "细胞,", nrow(md) == ncol(obj), "条元数据\n")
if (!scol %in% colnames(md))
  stop("样本列不存在：", scol, "。可选列：", paste(colnames(md), collapse = ", "), call. = FALSE)

L <- list()
add <- function(k, v) L[[length(L) + 1]] <<- data.frame(item = k, value = as.character(v), stringsAsFactors = FALSE)

# ---- 1 样本与细胞总览 ----
n_sample <- length(unique(md[[scol]]))
n_cell   <- ncol(obj)
gm  <- if ("nFeature_RNA" %in% colnames(md)) ceiling(median(md$nFeature_RNA, na.rm = TRUE)) else NA_real_
mmv <- if ("percent.mt"   %in% colnames(md)) round(median(md$percent.mt, na.rm = TRUE), 2)      else NA_real_
add("分析样本数", n_sample)
add("总细胞数(高质量)", n_cell)
if (!is.na(gm))  add("基因中位数", gm)
if (!is.na(mmv)) add("线粒体中位数", mmv)

# ---- 2 每样本统计 ----
tb <- md %>%
  group_by(sample = .data[[scol]]) %>%
  summarise(cell_number = n(),
            geneMedian = if ("nFeature_RNA" %in% colnames(md)) ceiling(median(.data$nFeature_RNA, na.rm = TRUE)) else NA_real_,
            mtMedian   = if ("percent.mt"   %in% colnames(md)) round(median(.data$percent.mt, na.rm = TRUE), 2)      else NA_real_,
            .groups = "drop")
write.table(tb, file.path(outdir, "qc_stat.xls"), quote = FALSE, sep = "\t", row.names = FALSE)
add("每样本细胞数", paste(tb$sample, tb$cell_number, sep = ":", collapse = "; "))

# ---- 3 细胞类型构成 ----
ct <- NULL
if (ccol %in% colnames(md)) {
  ct <- as.data.frame(table(md[[ccol]]))
  colnames(ct) <- c("cell_type", "cell_number")
  ct$rate <- round(100 * ct$cell_number / sum(ct$cell_number), 2)
  ct <- ct[order(-ct$cell_number), ]
  write.table(ct, file.path(outdir, "celltype_composition.xls"), quote = FALSE, sep = "\t", row.names = FALSE)
  add("细胞类型数", nrow(ct))
  add("各类型占比", paste(ct$cell_type, paste0(ct$rate, "%"), sep = ":", collapse = "; "))
  if (gcol %in% colnames(md)) {
    tg <- as.data.frame(table(md[[ccol]], md[[gcol]])); colnames(tg) <- c("cell_type", "grp", "n")
    tot <- aggregate(n ~ grp, tg, sum)
    tg$rate <- round(100 * tg$n / tot$n[match(tg$grp, tot$grp)], 2)
    write.table(tg, file.path(outdir, "celltype_by_group.xls"), quote = FALSE, sep = "\t", row.names = FALSE)
  }
}

# ---- 4 分群 ----
cl <- NULL
if ("seurat_clusters" %in% colnames(md)) {
  add("聚类群数", length(unique(md$seurat_clusters)))
  cl <- as.data.frame(table(md$seurat_clusters))
  colnames(cl) <- c("cluster", "cell_number")
  write.table(cl, file.path(outdir, "cluster_stat.xls"), quote = FALSE, sep = "\t", row.names = FALSE)
}

# ---- 5 人读摘要 ----
sink(file.path(outdir, "report_text.md"))
cat("# 单细胞分析结果 · 关键信息摘要\n\n## 一、样本与细胞总览\n\n")
cat(sprintf("本次分析样本数共 %d 个，总共获得细胞数 %d 个", n_sample, n_cell))
if (!is.na(gm))  cat(sprintf("，所有样本基因中位数为 %.1f", gm))
if (!is.na(mmv)) cat(sprintf("，线粒体中位数为 %.2f", mmv))
cat("。\n\n## 二、每样本统计\n\n"); print(tb, row.names = FALSE)
cat("\n## 三、细胞类型构成\n\n"); if (!is.null(ct)) print(ct, row.names = FALSE)
cat("\n## 四、分群统计\n\n");     if (!is.null(cl)) print(cl, row.names = FALSE)
sink()

res <- do.call(rbind, L)
write.table(res, file.path(outdir, "report_key_info.tsv"), quote = FALSE, sep = "\t", row.names = FALSE)

# =====================================================================
#  6 机读：report_stats.json（给 PPT 用）
# =====================================================================
wide <- list()
wide[[1]] <- c("sample", as.character(tb$sample))
wide[[length(wide) + 1]] <- c("本次对象细胞数", as.character(tb$cell_number))
if (!all(is.na(tb$geneMedian)))
  wide[[length(wide) + 1]] <- c("基因中位数", as.character(tb$geneMedian))
if (!all(is.na(tb$mtMedian)))
  wide[[length(wide) + 1]] <- c("线粒体中位数(%)", as.character(tb$mtMedian))

raw_note <- ""
if (!is.null(raw_tbl) && nzchar(raw_tbl) && file.exists(raw_tbl)) {
  rt <- tryCatch(read.delim(raw_tbl, stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.null(rt) && nrow(rt) > 0) {
    nm <- names(rt)
    sample_col <- nm[tolower(nm) %in% c("sample", "sampleid", "orig.ident")][1]
    num_col    <- nm[tolower(nm) %in% c("raw_number", "raw", "rawcell", "raw_cell")][1]
    if (!is.na(sample_col) && !is.na(num_col)) {
      m <- match(as.character(tb$sample), as.character(rt[[sample_col]]))
      raw_v <- suppressWarnings(as.numeric(rt[[num_col]][m]))
      if (!all(is.na(raw_v))) {
        wide[[length(wide) + 1]] <- c("原始细胞数", as.character(raw_v))
        removed <- raw_v - as.numeric(tb$cell_number)
        if (all(is.finite(removed) & removed >= 0)) {
          wide[[length(wide) + 1]] <- c("过滤掉细胞数", as.character(removed))
          wide[[length(wide) + 1]] <- c("过滤比率(%)", as.character(round(100 * removed / raw_v, 2)))
        } else {
          raw_note <- "原始细胞数有样本小于保留数或非数值，跳过过滤行"
        }
      } else {
        raw_note <- "raw 表里没有匹配到样本，跳过原始细胞数"
      }
    } else {
      raw_note <- sprintf("raw 表列名认不出（有：%s），跳过原始细胞数", paste(nm, collapse = ","))
    }
  } else {
    raw_note <- "raw 表读不出内容，跳过原始细胞数"
  }
} else if (!is.null(raw_tbl) && nzchar(raw_tbl)) {
  raw_note <- paste0("raw 表不存在：", raw_tbl)
}

table_rows <- lapply(wide, function(x) as.list(unname(x)))
names(table_rows) <- NULL

stats <- list(
  n_samples    = as.integer(n_sample),
  total_cells  = as.integer(n_cell),
  gene_median  = if (is.na(gm))  NULL else as.numeric(gm),
  mt_median    = if (is.na(mmv)) NULL else as.numeric(mmv),
  table_rows   = table_rows,
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
)
if (nzchar(raw_note)) stats$raw_table_note <- raw_note

to_json <- function(x, indent = 0) {
  pad <- strrep(" ", indent)
  pad2 <- strrep(" ", indent + 2)
  if (is.null(x)) return("null")
  if (is.list(x)) {
    if (!is.null(names(x)) && all(nzchar(names(x)))) {
      items <- vapply(seq_along(x), function(i)
        sprintf('%s"%s": %s', pad2, names(x)[i], to_json(x[[i]], indent + 2)), character(1))
      return(paste0("{\n", paste(items, collapse = ",\n"), "\n", pad, "}"))
    }
    items <- vapply(x, function(e) paste0(pad2, to_json(e, indent + 2)), character(1))
    return(paste0("[\n", paste(items, collapse = ",\n"), "\n", pad, "]"))
  }
  if (length(x) != 1) {
    items <- vapply(x, function(e) paste0(pad2, to_json(e, indent + 2)), character(1))
    return(paste0("[\n", paste(items, collapse = ",\n"), "\n", pad, "]"))
  }
  if (is.na(x)) return("null")
  if (is.logical(x)) return(if (isTRUE(x)) "true" else "false")
  if (is.numeric(x)) return(format(x, scientific = FALSE, trim = TRUE))
  s <- gsub("\\\\", "\\\\\\\\", as.character(x))
  s <- gsub('"', '\\\\"', s)
  s <- gsub("\n", "\\\\n", s)
  paste0('"', s, '"')
}
writeLines(to_json(stats), file.path(outdir, json_nm), useBytes = TRUE)

cat("\n提取完成，产出:\n")
cat("  report_key_info.tsv\n  report_text.md\n  qc_stat.xls\n")
if (!is.null(ct)) cat("  celltype_composition.xls\n")
if (ccol %in% colnames(md) && gcol %in% colnames(md)) cat("  celltype_by_group.xls\n")
if (!is.null(cl)) cat("  cluster_stat.xls\n")
cat("  ", json_nm, "（", length(table_rows), " 行 × ", length(wide[[1]]), " 列）\n", sep = "")
if (nzchar(raw_note)) cat("  注意：", raw_note, "\n", sep = "")
cat("\n--- 摘要 ---\n")
cat(sprintf("样本 %d 个，细胞 %d 个，基因中位数 %s，线粒体中位数 %s\n",
            n_sample, n_cell,
            if (is.na(gm)) "NA" else gm,
            if (is.na(mmv)) "NA" else mmv))