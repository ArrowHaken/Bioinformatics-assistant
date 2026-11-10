
.libPaths(c('/share/apps/R/R-4.1.2-4/lib64/R/library/'))
suppressMessages({library(Seurat);library(ggplot2);library(dplyr);library(patchwork)})

W <- '/share/home/hhhong1/rachel_7sample_3_261026'
OUT <- file.path(W, '5.markers')
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

d <- readRDS(file.path(W, '3.merge', 'merge_7sample.rds'))
DefaultAssay(d) <- 'RNA'
d <- NormalizeData(d, verbose = FALSE)
Idents(d) <- d@meta.data$seurat_clusters
cat("CELLS", ncol(d), "CLUSTERS", nlevels(d@meta.data$seurat_clusters), "\n")

md <- d@meta.data
tab <- data.frame(cluster = levels(md$seurat_clusters),
                  n_cells = as.integer(table(md$seurat_clusters)))
tab$pct_of_total <- round(100 * tab$n_cells / sum(tab$n_cells), 2)

st  <- as.data.frame.matrix(table(md$seurat_clusters, md$orig.ident))
colnames(st) <- paste0('n_', colnames(st))
stp <- as.data.frame.matrix(round(100 * prop.table(table(md$seurat_clusters, md$orig.ident), 1), 1))
colnames(stp) <- paste0('pct_', colnames(stp))
gt  <- as.data.frame.matrix(table(md$seurat_clusters, md$group))
colnames(gt) <- paste0('n_', colnames(gt))
gtp <- as.data.frame.matrix(round(100 * prop.table(table(md$seurat_clusters, md$group), 1), 1))
colnames(gtp) <- paste0('pct_', colnames(gtp))
st$cluster <- rownames(st); stp$cluster <- rownames(stp)
gt$cluster <- rownames(gt); gtp$cluster <- rownames(gtp)
stats <- tab %>% left_join(st, by='cluster') %>% left_join(stp, by='cluster') %>%
  left_join(gt, by='cluster') %>% left_join(gtp, by='cluster')
write.table(stats, file = file.path(OUT, 'cluster_stats.xls'),
            sep = '\t', quote = FALSE, row.names = FALSE)
cat("STATS_DONE\n")

m <- FindAllMarkers(d, only.pos = TRUE, min.pct = 0.2, logfc.threshold = 0.25)
write.table(m, file = file.path(OUT, 'all_markers.xls'), sep = '\t', quote = FALSE, row.names = FALSE)
cat("MARKERS_DONE", nrow(m), "\n")

top <- m %>% group_by(cluster) %>% arrange(desc(avg_log2FC), .by_group = TRUE) %>% slice_head(n = 15)
write.table(as.data.frame(top), file = file.path(OUT, 'top15_markers.xls'),
            sep = '\t', quote = FALSE, row.names = FALSE)

saveRDS(top, file = file.path(OUT, 'top15_markers.rds'))

p1 <- DimPlot(d, label = TRUE, raster = FALSE, pt.size = 0.25, repel = TRUE) +
  NoLegend() + ggtitle('7-sample merge, clusters (res 0.5)')
ggsave(file.path(OUT, 'umap_cluster.png'), p1, width = 8, height = 7, dpi = 110)

p2 <- DimPlot(d, group.by = 'orig.ident', raster = FALSE, pt.size = 0.25, ncol = 3) +
  ggtitle('by sample')
ggsave(file.path(OUT, 'umap_sample.png'), p2, width = 13, height = 9, dpi = 105)

p3 <- DimPlot(d, group.by = 'group', split.by = 'group', raster = FALSE, pt.size = 0.25, ncol = 2) +
  ggtitle('by group')
ggsave(file.path(OUT, 'umap_group.png'), p3, width = 11, height = 5.5, dpi = 110)

p4 <- DimPlot(d, label = TRUE, raster = FALSE, pt.size = 0.25, split.by = 'orig.ident', ncol = 4)
ggsave(file.path(OUT, 'umap_cluster_by_sample.png'), p4, width = 16, height = 9, dpi = 100)

cat("PLOTS_DONE\n")
cat("CLUSTER_INFO_DONE\n")
