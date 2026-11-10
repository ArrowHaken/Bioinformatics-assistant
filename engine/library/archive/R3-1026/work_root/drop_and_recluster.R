
.libPaths(c('/share/apps/R/R-4.1.2-4/lib64/R/library/'))
suppressMessages(library(Seurat))

W   <- '/share/home/hhhong1/rachel_7sample_3_261026'
OUT <- file.path(W, '7.after_drop')
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(file.path(W, '3.merge', 'merge_7sample.rds'))) {
  stop("CHECK_FAIL 找不到输入对象 3.merge/merge_7sample.rds")
}
d <- readRDS(file.path(W, '3.merge', 'merge_7sample.rds'))
cat("BEFORE_CELLS", ncol(d), " CLUSTERS", nlevels(factor(d@meta.data$seurat_clusters)), "\n")

# 剔除依据（本轮 5.markers/cluster_stats.xls 实测，三条客观判据取并集）：
#   ① 极小簇（<300 细胞，占比 <0.3%）：26(102) 27(88) 28(2) 23(238) 24(231) 25(210) 22(289)
#   ② 单样本缺失：G761 细胞数为 0 —— 24、25
#   ③ 单组异常（Microplastics 占比 >95% 或 <10%）：24(100%) 25(95.2%) 14(8.2%) 27(5.7%)
#   取并集：14 22 23 24 25 26 27 28，共 8 个；与上一轮剔除数量一致（上轮剔 8 个）
# ---- 剔除清单：由参数文件决定，不再写死在脚本里 ----
#   为什么改：原来这行是一串写死的字符串，注释还写着“与上一轮剔除数量一致”。
#   结果是每一次重跑都照抄上一轮的号，而那些号属于上一套簇编号 ——
#   换个对象跑，删掉的就不是同一群细胞。清单现在必须来自本次的明确决定。
PARAM <- file.path(W, '7.after_drop', 'drop_params.json')
DROP <- character(0)
DROP_SRC <- "none"
if (file.exists(PARAM)) {
  pj <- tryCatch(jsonlite::fromJSON(PARAM, simplifyVector = FALSE),
                 error = function(e) NULL)
  if (!is.null(pj) && length(pj$drop_clusters)) {
    DROP <- as.character(unlist(pj$drop_clusters))
    DROP_SRC <- "param_file"
  }
}
if (DROP_SRC == "none") {
  # 没有参数文件就停下，不替人猜一个清单。
  stop("CHECK_FAIL 缺少剔除参数：", PARAM,
       "（这一版不再使用写死的清单，请先在应用里定下要删哪些簇）")
}
cat("DROP_SRC", DROP_SRC, "\n")
cat("DROP_LIST", paste(DROP, collapse = ","), "\n")
keep <- !(as.character(d@meta.data$seurat_clusters) %in% DROP)
cat("DROP_CELLS", sum(!keep), "\n")

# 产出剔除清单，事后可核
md0 <- d@meta.data
tb0 <- as.data.frame(table(factor(md0$seurat_clusters)))
colnames(tb0) <- c("cluster","n")
tb0$dropped <- as.character(tb0$cluster) %in% DROP
write.table(tb0, file.path(OUT, 'dropped_clusters.xls'),
            sep = '\t', quote = FALSE, row.names = FALSE)

d <- subset(d, cells = colnames(d)[keep])
cat("AFTER_DROP_CELLS", ncol(d), "\n")
gc()

d <- FindNeighbors(d, reduction = 'harmony', dims = 1:20, verbose = FALSE)
cat("NN_DONE\n"); flush.console()
d <- FindClusters(d, resolution = 0.6, verbose = FALSE)
cat("CLUSTERS_AFTER", nlevels(factor(d@meta.data$seurat_clusters)), "\n"); flush.console()
d <- RunUMAP(d, reduction = 'harmony', dims = 1:20, verbose = FALSE)
cat("UMAP_DONE\n"); flush.console()

d@assays$RNA@scale.data <- matrix()
saveRDS(d, file.path(OUT, 'merged_after_drop.rds'))
cat("SAVED", file.size(file.path(OUT, 'merged_after_drop.rds')), "\n")

md <- d@meta.data
# 簇号是文字标签（0,1,...,23）。转整数必须先过 as.character：
#   直接 as.integer(factor) 拿到的是“第几个”位置序号，0 号簇会整簇变空值。
cl <- factor(as.character(md$seurat_clusters),
             levels = as.character(sort(unique(as.integer(as.character(md$seurat_clusters))))))
tb <- as.data.frame(table(cl))
colnames(tb) <- c("cluster","n")
tb$pct <- round(100 * tb$n / sum(tb$n), 2)

st <- as.data.frame.matrix(table(cl, md$orig.ident))
colnames(st) <- paste0("n_", colnames(st))
st$cluster <- rownames(st)
gt <- as.data.frame.matrix(table(cl, md$group))
colnames(gt) <- paste0("n_", colnames(gt))
gt$cluster <- rownames(gt)

stats <- merge(tb, st, by = "cluster")
stats <- merge(stats, gt, by = "cluster")

# ---- 自检：统计表细胞总数必须等于对象细胞数 ----
#   0 号簇被漏掉时，表看着正常，总数会少一截。这里停下报错，不让错表往后传。
if (sum(as.numeric(tb$n)) != ncol(d)) {
  stop("CHECK_FAIL 簇统计总数 ", sum(as.numeric(tb$n)),
       " != 对象细胞数 ", ncol(d))
}
cat("CHECK_OK cells", ncol(d), "clusters", nrow(tb), "\n")
stats <- stats[order(as.integer(stats$cluster)), ]
write.table(stats, file.path(OUT, 'cluster_stats_after_drop.xls'),
            sep = '\t', quote = FALSE, row.names = FALSE)

cat("--- per sample ---\n"); print(table(md$orig.ident))
cat("--- per group ---\n");  print(table(md$group))
cat("FILTER_DONE\n")
