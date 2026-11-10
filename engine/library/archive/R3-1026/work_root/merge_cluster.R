#!/usr/bin/env Rscript
# merge_cluster.R —— 合并 7 个样本 + 标准化 + 周期回归 + Harmony + 一次聚类
# 与 1_261016 里那份逻辑逐行一致，只把输入/输出目录指到本轮工作目录
.libPaths(c('/share/apps/R/R-4.1.2-4/lib64/R/library/'))
suppressMessages({library(dplyr);library(cowplot);library(ggplot2);library(plyr)
  library(Seurat);library(SeuratWrappers);library(harmony);library(stringr)})

S <- c('18m-G489','18m-G490','18m-G491','18m-G756','18m-G757','18m-G760','18m-G761')
D   <- '/share/home/hhhong1/rachel_7sample_3_261026/1.doublet'
OUT <- '/share/home/hhhong1/rachel_7sample_3_261026/3.merge'
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

objs <- lapply(S, function(s) readRDS(file.path(D, paste0(s,'.rds'))))
for (i in seq_along(objs)) cat("READ", S[i], ncol(objs[[i]]), "\n")

merge_data <- merge(x = objs[[1]], y = objs[-1], add.cell.ids = S)
rm(objs); gc()
cat("MERGED_RAW", ncol(merge_data), "\n")

s.genes   <- str_to_title(cc.genes$s.genes)
g2m.genes <- str_to_title(cc.genes$g2m.genes)

merge_data <- merge_data %>%
  Seurat::NormalizeData(verbose = FALSE) %>%
  FindVariableFeatures(selection.method = 'vst', nfeatures = 2000) %>%
  CellCycleScoring(s.features = s.genes, g2m.features = g2m.genes) %>%
  ScaleData(verbose = FALSE, vars.to.regress = c('S.Score','G2M.Score')) %>%
  RunPCA(verbose = FALSE)
cat("PCA_DONE\n")

integrated_data <- merge_data %>% RunHarmony('orig.ident')
cat("HARMONY_DONE\n")

sg <- data.frame(sample = S,
                 group = c('Mock','Mock','Mock','Microplastics','Microplastics','Microplastics','Microplastics'))
integrated_data@meta.data$group <- mapvalues(integrated_data@meta.data$orig.ident, sg$sample, sg$group)

integrated_data <- FindNeighbors(integrated_data, reduction = 'harmony', dims = 1:20)
integrated_data <- FindClusters(integrated_data, resolution = 0.5)
integrated_data <- RunTSNE(integrated_data, reduction = 'harmony', dims = 1:20)
integrated_data <- RunUMAP(integrated_data, reduction = 'harmony', dims = 1:20)
cat("CLUSTER_DONE\n")

integrated_data@meta.data$orig.ident <- factor(integrated_data@meta.data$orig.ident, levels = S)
integrated_data@meta.data$group <- factor(integrated_data@meta.data$group, levels = c('Mock','Microplastics'))
integrated_data@assays$RNA@scale.data <- matrix()

saveRDS(integrated_data, file = file.path(OUT, 'merge_7sample.rds'))

cat("MERGED_CELLS", ncol(integrated_data), "\n")
cat("N_CLUSTERS", nlevels(integrated_data@meta.data$seurat_clusters), "\n")
cat("--- per sample ---\n"); print(table(integrated_data@meta.data$orig.ident))
cat("--- per group ---\n");  print(table(integrated_data@meta.data$group))
cat("--- clusters ---\n");    print(table(integrated_data@meta.data$seurat_clusters))
cat("MERGE_DONE\n")
