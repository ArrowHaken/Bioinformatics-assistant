#!/usr/bin/env Rscript
# sc-report-extract 实现脚本
.libPaths(c("/tmp/repro/Rlibs", .libPaths()))
suppressMessages({library(Seurat); library(dplyr)})

args <- commandArgs(trailingOnly = TRUE)
get_opt <- function(k, d = NULL) { i <- match(k, args); if (is.na(i) || i == length(args)) return(d); args[i + 1] }
rds <- args[1]
outdir <- get_opt("--outdir", "/tmp/work/report")
ccol <- get_opt("--celltype-col", "annot")
gcol <- get_opt("--group-col", "group")
scol <- get_opt("--sample-col", "orig.ident")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

obj <- readRDS(rds)
md <- obj@meta.data
cat("载入:", ncol(obj), "细胞\n")

L <- list()
add <- function(k, v) L[[length(L) + 1]] <<- data.frame(item = k, value = as.character(v), stringsAsFactors = FALSE)

# 1 样本与细胞总览
n_sample <- length(unique(md[[scol]]))
n_cell <- ncol(obj)
gm <- ceiling(median(md$nFeature_RNA))
mmv <- round(median(md$percent.mt), 2)
add("分析样本数", n_sample)
add("总细胞数(高质量)", n_cell)
add("基因中位数", gm)
add("线粒体中位数", mmv)

# 2 每样本统计
tb <- md %>% group_by(sample = .data[[scol]]) %>%
  summarise(cell_number = n(),
            geneMedian = ceiling(median(nFeature_RNA)),
            mtMedian = round(median(percent.mt), 2), .groups = "drop")
write.table(tb, file.path(outdir, "qc_stat.xls"), quote = FALSE, sep = "\t", row.names = FALSE)
add("每样本细胞数", paste(tb$sample, tb$cell_number, sep = ":", collapse = "; "))

# 3 细胞类型构成
if (ccol %in% colnames(md)) {
  ct <- as.data.frame(table(md[[ccol]]))
  colnames(ct) <- c("cell_type", "cell_number")
  ct$rate <- round(100 * ct$cell_number / sum(ct$cell_number), 2)
  ct <- ct[order(-ct$cell_number), ]
  write.table(ct, file.path(outdir, "celltype_composition.xls"), quote = FALSE, sep = "\t", row.names = FALSE)
  add("细胞类型数", nrow(ct))
  add("各类型占比", paste(ct$cell_type, paste0(ct$rate, "%"), sep = ":", collapse = "; "))
  # 按组
  if (gcol %in% colnames(md)) {
    tg <- as.data.frame(table(md[[ccol]], md[[gcol]])); colnames(tg) <- c("cell_type", "grp", "n")
    tot <- aggregate(n ~ grp, tg, sum)
    tg$rate <- round(100 * tg$n / tot$n[match(tg$grp, tot$grp)], 2)
    write.table(tg, file.path(outdir, "celltype_by_group.xls"), quote = FALSE, sep = "\t", row.names = FALSE)
  }
}

# 4 分群
if ("seurat_clusters" %in% colnames(md)) {
  add("聚类群数", length(unique(md$seurat_clusters)))
  cl <- as.data.frame(table(md$seurat_clusters))
  colnames(cl) <- c("cluster", "cell_number")
  write.table(cl, file.path(outdir, "cluster_stat.xls"), quote = FALSE, sep = "\t", row.names = FALSE)
}

# 5 组装文字
sink(file.path(outdir, "report_text.md"))
cat("# 单细胞分析结果 · 关键信息摘要\n\n")
cat("## 一、样本与细胞总览\n\n")
cat(sprintf("本次分析样本数共 %d 个，总共获得细胞数 %d 个，所有样本基因中位数为 %.1f；线粒体中位数为 %.2f。\n\n",
            n_sample, n_cell, gm, mmv))
cat("## 二、每样本统计\n\n")
print(tb, row.names = FALSE)
cat("\n## 三、细胞类型构成\n\n")
if (ccol %in% colnames(md)) print(ct, row.names = FALSE)
cat("\n## 四、分群统计\n\n")
if ("seurat_clusters" %in% colnames(md)) print(cl, row.names = FALSE)
sink()

res <- do.call(rbind, L)
write.table(res, file.path(outdir, "report_key_info.tsv"), quote = FALSE, sep = "\t", row.names = FALSE)

cat("\n提取完成，产出:\n")
cat("  report_key_info.tsv\n  report_text.md\n  qc_stat.xls\n  celltype_composition.xls\n  celltype_by_group.xls\n  cluster_stat.xls\n")
cat("\n--- 摘要 ---\n")
cat(sprintf("样本 %d 个，细胞 %d 个，基因中位数 %d，线粒体中位数 %.2f\n", n_sample, n_cell, gm, mmv))
