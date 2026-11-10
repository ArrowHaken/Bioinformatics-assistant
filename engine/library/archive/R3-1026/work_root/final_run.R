
.libPaths(c('/share/apps/R/R-4.1.2-4/lib64/R/library/'))
suppressMessages({library(Seurat); library(dplyr)})

W    <- '/share/home/hhhong1/rachel_7sample_3_261026'
OUT  <- file.path(W, '8.final')
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
DROPCL <- c("17","19","22","23","24","25","26","27")
# 2026-10-28 用户明确：终版要剔除，剔 18m-G761（Microplastics 组）。
#   中途曾按"不删样本"改过一次，用户纠正后恢复为剔除口径。
DROPSP <- "18m-G761"
KEEP_ALL_SAMPLES <- FALSE

f_after <- file.path(W, '7.after_drop', 'merged_after_drop.rds')
if (file.exists(f_after)) {
  d <- readRDS(f_after)
  cat("REUSE_AFTER_DROP", ncol(d), "\n")
} else {
  d <- readRDS(file.path(W, '6.compare', '300', 'merged.rds'))
  keep <- !(as.character(d@meta.data$seurat_clusters) %in% DROPCL)
  d <- subset(d, cells = colnames(d)[keep])
  cat("REBUILT_AFTER_DROP", ncol(d), "\n")
}
gc()

if (!"RNA_snn_res.0.6" %in% colnames(d@meta.data)) {
  d <- FindNeighbors(d, reduction='harmony', dims=1:20, verbose=FALSE)
  d <- FindClusters(d, resolution=0.6, verbose=FALSE)
  d <- RunUMAP(d, reduction='harmony', dims=1:20, verbose=FALSE)
  cat("RECLUSTERED", nlevels(factor(d@meta.data$seurat_clusters)), "\n")
}
d$seurat_clusters <- d$RNA_snn_res.0.6
d$seurat_clusters <- factor(as.character(d$seurat_clusters),
                            levels = as.character(sort(unique(as.integer(
                              as.character(d$seurat_clusters))))))
cat("LEVELS", paste(levels(d$seurat_clusters), collapse = ","), "
")
cat("CLUSTERS_BEFORE_G761", nlevels(d$seurat_clusters), " CELLS", ncol(d), "\n")

if (length(DROPSP)) {
  d <- subset(d, cells = colnames(d)[!(as.character(d@meta.data$orig.ident) %in% DROPSP)])
  cat("AFTER_DROP", paste(DROPSP, collapse = ","), ncol(d), "\n")
} else {
  cat("NO_SAMPLE_DROPPED", ncol(d), "\n")
}
d@meta.data$orig.ident <- factor(d@meta.data$orig.ident)
d@meta.data$group <- factor(d@meta.data$group)
print(table(d@meta.data$orig.ident))
print(table(d@meta.data$group))
flush.console()

md <- d@meta.data
cl <- d@meta.data$seurat_clusters

prop <- as.data.frame(table(cl))
colnames(prop) <- c("cluster", "n_cells")
prop$pct_of_total <- round(100 * prop$n_cells / sum(prop$n_cells), 2)

st <- as.data.frame.matrix(table(cl, md$orig.ident))
colnames(st) <- paste0("n_", colnames(st))
stp <- as.data.frame.matrix(round(100 * prop.table(table(cl, md$orig.ident), 1), 2))
colnames(stp) <- paste0("pct_", colnames(stp))
gt <- as.data.frame.matrix(table(cl, md$group))
colnames(gt) <- paste0("n_", colnames(gt))
gtp <- as.data.frame.matrix(round(100 * prop.table(table(cl, md$group), 1), 2))
colnames(gtp) <- paste0("pct_", colnames(gtp))
st$cluster <- rownames(st); stp$cluster <- rownames(stp)
gt$cluster <- rownames(gt); gtp$cluster <- rownames(gtp)

stats <- prop %>%
  left_join(st, by="cluster") %>% left_join(stp, by="cluster") %>%
  left_join(gt, by="cluster") %>% left_join(gtp, by="cluster")
write.table(stats, file.path(OUT, 'cell_ratio_by_cluster.xls'),
            sep='\t', quote=FALSE, row.names=FALSE)
cat("PROP_DONE\n"); flush.console()
print(stats, row.names=FALSE)

smp <- as.data.frame(table(md$orig.ident, md$group))
colnames(smp) <- c("sample", "group", "n_cells")
smp$pct <- round(100 * smp$n_cells / nrow(md), 2)
write.table(smp, file.path(OUT, 'cell_count_by_sample.xls'),
            sep='\t', quote=FALSE, row.names=FALSE)

DefaultAssay(d) <- 'RNA'
d <- NormalizeData(d, verbose=FALSE)
Idents(d) <- d@meta.data$seurat_clusters

m <- tryCatch(FindAllMarkers(d, only.pos=TRUE, min.pct=0.2,
                             logfc.threshold=0.25, verbose=FALSE),
              error=function(e) { cat("MARKER_ERR", conditionMessage(e), "\n"); NULL })
if (!is.null(m)) {
  m <- m[!grepl(m$gene, pattern='^mt-|^MT-|^Mt-'), ]
  write.table(m, file.path(OUT, 'all_markers.xls'), sep='\t', quote=FALSE, row.names=FALSE)
  top <- m %>% group_by(cluster) %>% arrange(desc(avg_log2FC), .by_group=TRUE) %>% slice_head(n=15)
  write.table(as.data.frame(top), file.path(OUT, 'top15_markers.xls'),
              sep='\t', quote=FALSE, row.names=FALSE)
  cat("MARKER_DONE", nrow(m), "\n")
  cat("--- 每簇 top5 ---\n")
  for (k in levels(d$seurat_clusters)) {
    g <- as.character(top$gene[top$cluster == k])
    if (length(g)) cat(k, ":", paste(head(g,5), collapse=", "), "\n")
  }
} else {
  cat("MARKER_SKIPPED\n")
}
flush.console()


# ---- 自检：簇号与真实标签必须一一对上 ----
lv <- levels(d$seurat_clusters)
cnt <- as.integer(table(d$seurat_clusters))
if (any(cnt == 0)) stop("CHECK_FAIL 空簇等级: ", paste(lv[cnt == 0], collapse = ","))
if (sum(cnt) != ncol(d)) stop("CHECK_FAIL 各簇之和 ", sum(cnt), " != 细胞数 ", ncol(d))
cat("CHECK_OK clusters", length(lv), "cells", ncol(d), "\n")

res_diff <- list()
for (k in levels(d@meta.data$seurat_clusters)) {
  sel <- as.character(d@meta.data$seurat_clusters) == k
  sub <- tryCatch(subset(d, cells = colnames(d)[sel]),
                  error = function(e) {
                    cat("DIFF_SKIP cluster", k, "reason", conditionMessage(e), "\n")
                    NULL
                  })
  if (is.null(sub)) next
  n_mock <- sum(sub@meta.data$group == 'Mock')
  n_mp   <- sum(sub@meta.data$group == 'Microplastics')
  if (n_mock < 3 || n_mp < 3) {
    cat("DIFF_SKIP cluster", k, "mock", n_mock, "mp", n_mp, "\n"); next
  }
  Idents(sub) <- sub@meta.data$group
  dm <- tryCatch(FindMarkers(sub, ident.1='Microplastics', ident.2='Mock',
                             min.pct=0.1, logfc.threshold=0.25, verbose=FALSE),
                 error=function(e) {
                   cat("DIFF_ERR cluster", k, "reason", conditionMessage(e), "\n")
                   NULL
                 })
  if (!is.null(dm) && nrow(dm) > 0) {
    dm$cluster <- k
    dm$gene <- rownames(dm)
    res_diff[[k]] <- dm
    cat("DIFF_OK cluster", k, "genes", nrow(dm), "\n")
  }
  rm(sub); gc(FALSE)
}
if (length(res_diff)) {
  alld <- do.call(rbind, res_diff)
  write.table(alld, file.path(OUT, 'diff_MP_vs_Mock_by_cluster.xls'),
              sep='\t', quote=FALSE, row.names=FALSE)
  cat("DIFF_TOTAL", nrow(alld), "\n")
}
flush.console()

d@assays$RNA@scale.data <- matrix()
saveRDS(d, file.path(OUT, 'final_object.rds'))
cat("FINAL_CELLS", ncol(d), " CLUSTERS", nlevels(d$seurat_clusters), "\n")
cat("FINAL_DONE\n")
