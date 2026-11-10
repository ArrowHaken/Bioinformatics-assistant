#!/usr/bin/env Rscript
# sc-plot-kit v2 —— 按客户原报告 PPT 的版式复刻六段图
# 用法: Rscript plot_all_v2.R <seurat.rds> --outdir <目录> --stage qc,cluster,markgene,annot,ratio,diff,enrich
#
# v2 相对 v1 的九处改动（对齐原报告）：
#  1 补 DotPlot 点图（注释段）
#  2 比例图补误差线、个体散点、显著性 p 值
#  3 富集图改横向柱状图 + 通路全名
#  4 比例图按细胞类型分面
#  5 火山图改三色 + 带计数图例
#  6 质控图改每样本一张小图拼网格 + 单色
#  7 轴标签用大写 UMAP_1 / UMAP_2
#  8 组别配色 Mock 红 / Microplastics 青
#  9 比例图纵轴 Percentage(%)
#
# 复刻规格来源：/tmp/work/replica_spec.md（逐张核对原报告 19 张图得出）

# ---- 包路径：按需改成你自己的 ----
if (dir.exists("D:/soft/R/library")) .libPaths(c("D:/soft/R/library", .libPaths()))
suppressMessages({
  library(Seurat); library(Matrix); library(dplyr)
  library(ggplot2); library(patchwork); library(ggrepel)
})

args <- commandArgs(trailingOnly = TRUE)
get_opt <- function(k, d = NULL) { i <- match(k, args); if (is.na(i) || i == length(args)) return(d); args[i + 1] }
rds     <- args[1]
outdir  <- get_opt("--outdir", "D:/soft/result")
stages  <- strsplit(get_opt("--stage", "qc,cluster,markgene,annot,ratio,diff,enrich"), ",")[[1]]
gcol    <- get_opt("--group-col", "group")
scol    <- get_opt("--sample-col", "orig.ident")
ccol    <- get_opt("--celltype-col", "annot")
compare <- get_opt("--compare", NULL)

# ---- 配色（按原报告）----
CORAL    <- "#F8766D"                 # 质控图单一珊瑚红
GRP_COL  <- c(Mock = "#E64B35", Microplastics = "#4DBBD5")  # 组别：红 / 青
VOLC_COL <- c(UP = "#E64B35", DOWN = "#00A087", NO = "#3C5488")
color_panel <- c('#53A85F','#58A4C3','#AB3282','#8C549C','#BD956A','#57C3F3','#6778AE',
                 '#F3B1A0','#F1BB72','#DCC1DD','#E95C59','#625D9E','#F7F398','#E63863',
                 '#5F3D69','#C5DEBA','#CCE0F5','#B53E2B','#AA9A59','#E39A35','#91D0BE',
                 '#23452F','#E4C755','#585658','#C1E6F3','#D6E7A3','#712820','#CCC9E6',
                 '#3A6963','#68A180','#61B934','#476D87','#9FA3A8','#968175')

obj <- readRDS(rds)
cat("载入对象：", ncol(obj), "细胞\n")
if (!gcol %in% colnames(obj@meta.data)) gcol <- "orig.ident"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# 统一出图：白底（原报告 PNG 是透明底，导出会变黑，必须显式白底）
save_plot <- function(p, path, w = 10, h = 8, dpi = 300) {
  ggsave(paste0(path, ".png"), p, width = w, height = h, dpi = dpi,
         bg = "white", limitsize = FALSE)
  cat("   ->", basename(path), "\n")
}

# ============================== 0. QC ==============================
# 原报告：7 个样本各一张小图，拼成 3x3 网格，单色珊瑚红，每图带样本名图例
if ("qc" %in% stages) {
  cat("\n== 0.QC ==\n")
  d <- file.path(outdir, "0.QC"); dir.create(d, showWarnings = FALSE)
  samples <- levels(factor(obj@meta.data[[scol]]))
  for (feat in c("nFeature_RNA", "nCount_RNA", "percent.mt")) {
    plist <- lapply(samples, function(s) {
      cells <- rownames(obj@meta.data)[obj@meta.data[[scol]] == s]
      o <- subset(obj, cells = cells)
      Idents(o) <- scol
      VlnPlot(o, features = feat, pt.size = 0, cols = CORAL) +
        labs(x = "Identity", y = NULL) +
        theme(legend.position = "right",
              axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
              plot.title = element_text(size = 10, hjust = 0.5))
    })
    p <- wrap_plots(plist, ncol = 3)
    save_plot(p, file.path(d, paste0(feat, "_violin")), 12, 10)
  }
  st <- data.frame(sample = names(table(obj@meta.data[[scol]])),
                   cell_number = as.integer(table(obj@meta.data[[scol]])))
  write.table(st, file.path(d, "cell_number_stat.xls"),
              quote = FALSE, sep = "\t", row.names = FALSE)
}

# ============================== 1. cluster ==============================
# 原报告：UMAP_1 / UMAP_2 大写；总体 / 按样本 / 按分组分面 / 按样本分面
if ("cluster" %in% stages) {
  cat("\n== 1.cluster ==\n")
  d <- file.path(outdir, "1.cluster"); dir.create(d, showWarnings = FALSE)
  umap_lab <- function(p) p + labs(x = "UMAP_1", y = "UMAP_2")
  save_plot(umap_lab(DimPlot(obj, label = TRUE, raster = FALSE, pt.size = 0.4,
                             cols = color_panel) & NoAxes() & theme_void()),
            file.path(d, "merge_umap"), 10, 8)
  # 按样本着色：原报告无标题、图上不写样本名标签、用高区分度色板
  SAMPLE_COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
                  "#E6AB02", "#A6761D", "#666666", "#377EB8", "#984EA3")
  save_plot(DimPlot(obj, group.by = scol, label = FALSE, raster = FALSE, pt.size = 0.4,
                    cols = SAMPLE_COL) +
              labs(x = "UMAP_1", y = "UMAP_2") + theme(plot.title = element_blank()),
            file.path(d, "merge_sample_umap"), 10, 8)
  save_plot(DimPlot(obj, split.by = gcol, ncol = 2, label = TRUE, raster = FALSE, pt.size = 0.4,
                    cols = color_panel) + labs(x = "UMAP_1", y = "UMAP_2"),
            file.path(d, "merge_group_umap"), 14, 7)
  save_plot(DimPlot(obj, split.by = scol, ncol = 4, label = TRUE, raster = FALSE, pt.size = 0.3,
                    cols = color_panel) + labs(x = "UMAP_1", y = "UMAP_2"),
            file.path(d, "merge_sample_split_umap"), 16, 9)
}

# ============================== 2. markgene ==============================
# 原报告：热图（列按 cluster、顶部注释条）+ FeaturePlot 3x4 拼板 + 小提琴 3x4 拼板
if ("markgene" %in% stages) {
  cat("\n== 2.markgene ==\n")
  d <- file.path(outdir, "2.markgene"); dir.create(d, showWarnings = FALSE)
  Idents(obj) <- "seurat_clusters"
  f_all <- file.path(d, "all_markgene.xls"); f_sig <- file.path(d, "sig_markgene.xls")
  if (file.exists(f_all) && file.exists(f_sig)) {
    mk  <- read.delim(f_all, stringsAsFactors = FALSE)
    sig <- read.delim(f_sig, stringsAsFactors = FALSE)
    cat("   复用已算好的 marker 表\n")
  } else {
    mk <- FindAllMarkers(obj, only.pos = TRUE, min.pct = 0.2, logfc.threshold = 0.25, verbose = FALSE)
    mk <- mk[!grepl(mk$gene, pattern = "^mt-|^MT-|^Mt-"), ]
    sig <- subset(mk, p_val_adj < 0.05)
    write.table(mk,  f_all, quote = FALSE, sep = "\t", row.names = FALSE)
    write.table(sig, f_sig, quote = FALSE, sep = "\t", row.names = FALSE)
  }
  cat("   marker:", nrow(mk), " 显著:", nrow(sig), "\n")
  top10 <- sig %>% group_by(cluster) %>% slice_max(avg_log2FC, n = 10)
  top12 <- sig %>% group_by(cluster) %>% slice_max(avg_log2FC, n = 12)
  need_gene <- unique(c(as.character(top10$gene), as.character(top12$gene)))
  obj <- ScaleData(obj, features = need_gene, verbose = FALSE)

  # 热图（顶部带 cluster 注释条）
  p_hm <- DoHeatmap(obj, features = as.character(unique(top10$gene)), group.colors = color_panel) +
          scale_fill_gradient2(low = "#3B4CC0", mid = "#F7F7F7", high = "#D7301F") +
          theme(axis.text.y = element_text(size = 4))
  save_plot(p_hm, file.path(d, "top10_heatmap"), 14, 14)

  # FeaturePlot / 小提琴：按群出 12 基因的拼板（原报告是 3x4）
  for (i in levels(obj$seurat_clusters)) {
    g <- as.character(top12$gene[top12$cluster == i])
    if (length(g) == 0) next
    fp <- FeaturePlot(obj, features = g, ncol = 4, raster = FALSE) &
          scale_colour_gradientn(colours = c("lightblue", "lightyellow", "orange", "red"))
    save_plot(fp, file.path(d, paste0("featureplot_cluster", i)), 14, 9)
    vp <- VlnPlot(obj, features = g, pt.size = 0, ncol = 4, cols = color_panel,
                  fill.by = "ident") & labs(y = "Expression Level")
    save_plot(vp, file.path(d, paste0("violin_cluster", i)), 14, 9)
    cat("   群", i, "完成\n")
  }
}

# ============================== 3. annot ==============================
# 原报告：注释 UMAP（标题 annot / annot_cluster）+ DotPlot 点图
if ("annot" %in% stages && ccol %in% colnames(obj@meta.data)) {
  cat("\n== 3.annot ==\n")
  d <- file.path(outdir, "5.annot"); dir.create(d, showWarnings = FALSE)
  obj$annot_cluster <- factor(paste0(obj$seurat_clusters, ":", obj@meta.data[[ccol]]))
  save_plot(DimPlot(obj, group.by = ccol, label = TRUE, repel = TRUE, raster = FALSE,
                    pt.size = 0.5, cols = color_panel) +
              labs(x = "UMAP_1", y = "UMAP_2", title = ccol),
            file.path(d, "merge_umap_annot"), 11, 8)
  save_plot(DimPlot(obj, group.by = "annot_cluster", label = TRUE, repel = TRUE,
                    raster = FALSE, pt.size = 0.5, cols = color_panel) +
              labs(x = "UMAP_1", y = "UMAP_2", title = "annot_cluster"),
            file.path(d, "merge_umap_cluster_annot"), 12, 9)
  # DotPlot：每类取前 2 个代表 marker（从 marker 表读，兼容只跑 annot 段的情况）
  f_sig <- file.path(outdir, "2.markgene", "sig_markgene.xls")
  if (file.exists(f_sig)) {
    sig <- read.delim(f_sig, stringsAsFactors = FALSE)
    sig$cluster <- as.character(sig$cluster)
    reps <- sig %>% group_by(cluster) %>% slice_max(avg_log2FC, n = 2)
    Idents(obj) <- ccol
    p_dot <- DotPlot(obj, features = unique(as.character(reps$gene)),
                     cols = c("blue", "lightpink", "red")) +
             RotatedAxis() + labs(x = "", y = "")
    save_plot(p_dot, file.path(d, "genelist_dotplot"), 16, 8)
  } else {
    cat("   未找到", f_sig, "，跳过 DotPlot\n")
  }
}

# ============================== 4. cell_ratio ==============================
# 原报告：4x4 分面、14 面；按细胞类型分面；纵轴 Percentage(%)；
#         图1 按 group 2 柱（Mock 红 / Micro 青）；图2 按 sample 7 柱；图3 带误差线+散点+p值
if ("ratio" %in% stages) {
  cat("\n== 4.cell_ratio ==\n")
  d <- file.path(outdir, "3.cell_ratio"); dir.create(d, showWarnings = FALSE)
  md <- obj@meta.data
  ct <- if (ccol %in% colnames(md)) ccol else "seurat_clusters"
  md$.ct <- md[[ct]]

  # 表：按组、按样本
  tbl <- function(gv) {
    t <- as.data.frame(table(md$.ct, md[[gv]]))
    colnames(t) <- c("cell_type", "lvl", "cell_number")
    tot <- aggregate(cell_number ~ lvl, t, sum)
    t$rate <- round(100 * t$cell_number / tot$cell_number[match(t$lvl, tot$lvl)], 2)
    t
  }
  tg <- tbl(gcol); write.table(tg, file.path(d, "cell_number_group.xls"),
                               quote = FALSE, sep = "\t", row.names = FALSE)
  ts <- tbl(scol); write.table(ts, file.path(d, "cell_number_sample.xls"),
                               quote = FALSE, sep = "\t", row.names = FALSE)

  base_thm <- theme_bw(base_size = 9) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid = element_blank(), strip.background = element_blank())

  # 图1：按组 2 柱
  tg$lvl <- factor(tg$lvl, levels = names(GRP_COL))
  p1 <- ggplot(tg, aes(x = lvl, y = rate, fill = lvl)) +
        geom_bar(stat = "identity", width = 0.7, color = "black") +
        facet_wrap(~ cell_type, scales = "free_y", ncol = 4) +
        scale_fill_manual(values = GRP_COL, name = gcol) +
        labs(x = "", y = "Percentage(%)") + base_thm
  save_plot(p1, file.path(d, "cell_number_rate_group"), 14, 11)

  # 图2：按样本 7 柱
  p2 <- ggplot(ts, aes(x = lvl, y = rate, fill = lvl)) +
        geom_bar(stat = "identity", width = 0.7, color = "black") +
        facet_wrap(~ cell_type, scales = "free_y", ncol = 4) +
        scale_fill_manual(values = color_panel, name = scol) +
        labs(x = "", y = "Percentage(%)") + base_thm
  save_plot(p2, file.path(d, "cell_number_rate_sample"), 16, 11)

  # 图3：按组 + 误差线 + 个体散点 + p 值
  # 用样本级比例算均值±标准误，并做组间检验
  smp2grp <- unique(data.frame(sample = md[[scol]], grp = md[[gcol]]))
  ts2 <- merge(ts, smp2grp, by.x = "lvl", by.y = "sample")
  ts2$grp <- factor(ts2$grp, levels = names(GRP_COL))
  summ <- ts2 %>% group_by(cell_type, grp) %>%
    summarise(mean = mean(rate), se = sd(rate) / sqrt(n()), n = n(), .groups = "drop")
  pval <- ts2 %>% group_by(cell_type) %>%
    summarise(p = tryCatch(t.test(rate ~ grp)$p.value, error = function(e) NA_real_), .groups = "drop")
  pval$lab <- ifelse(is.na(pval$p), "",
                     ifelse(pval$p < 0.001, "p<0.001", paste0("p=", signif(pval$p, 2))))
  save_plot(
    ggplot() +
      geom_bar(data = summ, aes(x = grp, y = mean, fill = grp), stat = "identity",
               width = 0.7, color = "black") +
      geom_errorbar(data = summ, aes(x = grp, ymin = mean - se, ymax = mean + se),
                    width = 0.2, colour = "black") +
      geom_point(data = ts2, aes(x = grp, y = rate), colour = "black", size = 1) +
      geom_text(data = pval, aes(x = 1.5, y = Inf, label = lab), vjust = 1.6, size = 2.6) +
      facet_wrap(~ cell_type, scales = "free_y", ncol = 4) +
      scale_fill_manual(values = GRP_COL, name = gcol) +
      labs(x = "", y = "Percentage(%)") + base_thm,
    file.path(d, "cell_number_rate_group_with_pvalue"), 14, 12)
}

# ============================== 5. diff ==============================
# 原报告：火山图三色（UP 红 / DOWN 绿 / NO 蓝）+ 带计数图例 + 竖虚线 x=0 + 横虚线 1.301
if ("diff" %in% stages && !is.null(compare) && ccol %in% colnames(obj@meta.data)) {
  cat("\n== 5.diff ==\n")
  d <- file.path(outdir, "4.diff_enrich", "diff"); dir.create(d, showWarnings = FALSE, recursive = TRUE)
  a <- strsplit(compare, "vs")[[1]][1]; b <- strsplit(compare, "vs")[[1]][2]
  md <- obj@meta.data
  cnt <- table(md[[ccol]], md[[gcol]])
  ok <- rownames(cnt)[cnt[, a] >= 3 & cnt[, b] >= 3]
  cat("   纳入细胞类型:", length(ok), "\n")
  Idents(obj) <- ccol
  for (t in ok) {
    r <- try(FindMarkers(obj, ident.1 = a, ident.2 = b, group.by = gcol,
                         subset.ident = t, logfc.threshold = 0, verbose = FALSE), silent = TRUE)
    if (inherits(r, "try-error") || nrow(r) == 0) next
    r$gene <- rownames(r)
    rd <- d
    write.table(r, file.path(rd, paste0(t, "_", compare, "_diffgene.xls")),
                quote = FALSE, sep = "\t", row.names = FALSE)
    r$type <- ifelse(r$p_val_adj < 0.05 & r$avg_log2FC > 0, "UP",
                     ifelse(r$p_val_adj < 0.05 & r$avg_log2FC < 0, "DOWN", "NO"))
    n <- table(r$type)
    lab <- paste0(names(n), " ", as.integer(n))
    r$grp <- factor(lab[match(r$type, names(n))], levels = lab)
    lab2 <- r[order(-abs(r$avg_log2FC)), ][1:min(40, nrow(r)), "gene"]
    p <- ggplot(r, aes(x = avg_log2FC, y = -log10(p_val_adj))) +
      geom_point(aes(colour = grp), size = 0.7) +
      scale_colour_manual(values = setNames(VOLC_COL[names(n)], lab),
                          name = "p_val_adj<0.05") +
      geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
      geom_hline(yintercept = -log10(0.05), linetype = 2, colour = "grey50") +
      geom_text_repel(data = subset(r, gene %in% lab2), aes(label = gene),
                      size = 2, max.overlaps = 25) +
      labs(x = "avg_log2FC", y = "-log10(p_val_adj)", title = paste0(t, "  ", compare)) +
      theme_bw(base_size = 10)
    save_plot(p, file.path(rd, paste0(t, "_", compare, "_volcano")), 8, 7)
  }
}

# ============================== 6. enrich ==============================
# 原报告：横向柱状图（不是气泡图），纵轴写通路英文全称，横轴 Count，
#         柱色按 p.adjust 映射（红→洋红→紫→蓝），浅灰网格，无数值标注
#
# 注意（踩过的坑）：新版 clusterProfiler 的 DESCRIPTION 要求 ggplot2 >= 4.0，
#   而 Seurat 的绘图在 ggplot2 4.x 下会崩（+.gg 对 S4 对象 deparse 报错），
#   两者放同一库路径会冲突。所以这里不依赖 clusterProfiler，
#   改用 org.Mm.eg.db + 超几何检验的轻量实现，出图风格与原报告一致。
if ("enrich" %in% stages) {
  cat("\n== 6.enrich ==\n")
  d <- file.path(outdir, "4.diff_enrich", "enrich"); dir.create(d, showWarnings = FALSE, recursive = TRUE)
  ddir <- file.path(outdir, "4.diff_enrich", "diff")
  fs <- if (dir.exists(ddir)) list.files(ddir, pattern = "_diffgene\\.xls$", full.names = TRUE) else character(0)
  if (!requireNamespace("org.Mm.eg.db", quietly = TRUE)) {
    cat("   org.Mm.eg.db 不可用，跳过富集\n")
  } else if (length(fs) == 0) {
    cat("   未找到差异基因表（需先跑 diff 段），跳过富集\n")
  } else {
    suppressMessages(library(org.Mm.eg.db))
    db <- org.Mm.eg.db
    BG <- keys(db, keytype = "SYMBOL"); N <- length(BG)

    # 背景：GO(BP) 每个 term 的基因数
    bgGo <- suppressMessages(select(db, keys = BG, columns = c("GO", "ONTOLOGY"), keytype = "SYMBOL"))
    bgGo <- bgGo[!is.na(bgGo$GO) & bgGo$ONTOLOGY == "BP", ]
    bgGoN <- table(bgGo$GO)
    # 背景：KEGG(mmu) 每个通路编号的基因数
    bgKe <- suppressMessages(select(db, keys = BG, columns = "PATH", keytype = "SYMBOL"))
    bgKe <- bgKe[!is.na(bgKe$PATH), ]
    bgKeN <- table(bgKe$PATH)
    # KEGG 通路英文全称（注意 keggList("mmu") 给的是基因/CDS 列表，
    # 取通路名必须用 keggList("pathway", "mmu")，且值尾部带 " - Mus musculus (house mouse)" 需去掉）
    keName <- try({
      kl <- KEGGREST::keggList("pathway", "mmu")
      nm <- sub(" - Mus musculus \\(house mouse\\)$", "", as.character(kl))
      stats::setNames(nm, sub("^mmu", "", names(kl)))
    }, silent = TRUE)
    if (inherits(keName, "try-error")) keName <- NULL

    # GO(BP) 编号 -> 英文全称
    goName <- try({
      suppressMessages(library(GO.db))
      ids <- unique(names(bgGoN))
      g <- suppressMessages(AnnotationDbi::select(GO.db, keys = ids, columns = "TERM",
                                                 keytype = "GOID"))
      stats::setNames(g$TERM, g$GOID)
    }, silent = TRUE)
    if (inherits(goName, "try-error")) goName <- NULL

    cat("   背景:", N, "基因 | GO(BP)", length(bgGoN), "term | KEGG", length(bgKeN), "pathway",
        "| GO名", if (is.null(goName)) "未取到(用编号)" else paste0(length(goName), "条"),
        "| KEGG名", if (is.null(keName)) "未取到(用编号)" else paste0(length(keName), "条"), "\n")

    # 出横向柱状图（原报告样式）
    draw_bar <- function(res, fn) {
      if (is.null(res) || nrow(res) == 0) return(invisible(NULL))
      res <- res[order(res$p.adjust), , drop = FALSE]
      res <- res[1:min(30, nrow(res)), , drop = FALSE]
      write.table(res, paste0(fn, ".xls"), quote = FALSE, sep = "\t", row.names = FALSE)
      res$term <- factor(res$term, levels = rev(res$term))
      p <- ggplot(res, aes(x = Count, y = term, fill = p.adjust)) +
        geom_col() +
        scale_fill_gradientn(colours = c("#E64B35", "#C51B7D", "#7B3294", "#3C5488")) +
        labs(x = "Count", y = NULL, fill = "p.adjust") +
        theme_bw(base_size = 9) +
        theme(panel.grid.major.y = element_line(colour = "grey90"),
              panel.grid.minor = element_blank())
      save_plot(p, fn, 10, 7)
      cat("   ->", basename(fn), ":", nrow(res), "条\n")
    }

    # 超几何富集（col 用 GO 或 PATH）
    run_hyper <- function(genes, bgN, col, nameMap = NULL) {
      g <- intersect(unique(genes), BG)
      if (length(g) < 5) return(NULL)
      m <- suppressMessages(select(db, keys = g, columns = col, keytype = "SYMBOL"))
      m <- m[!is.na(m[[col]]), ]
      m <- m[m[[col]] %in% names(bgN), ]
      if (nrow(m) == 0) return(NULL)
      hit <- table(m[[col]]); terms <- names(hit); q <- as.integer(hit)
      ms <- as.integer(bgN[terms]); ms[is.na(ms)] <- 0
      keep <- ms >= 5 & ms <= 500
      terms <- terms[keep]; q <- q[keep]; ms <- ms[keep]
      if (length(terms) == 0) return(NULL)
      pv <- phyper(q - 1, ms, N - ms, length(g), lower.tail = FALSE)
      pa <- p.adjust(pv, "BH")
      res <- data.frame(term = terms, Count = q, pvalue = pv, p.adjust = pa,
                        GeneRatio = round(q / length(g), 4), stringsAsFactors = FALSE)
      if (!is.null(nameMap)) {
        nm <- nameMap[terms]
        res$term <- ifelse(is.na(nm), terms, as.character(nm))
      }
      # GO 编号不在 nameMap 时，单独补一次（背景 GO 名映射可能未覆盖）
      if (col == "GO" && !is.null(goName)) {
        nm <- goName[terms]
        res$term <- ifelse(is.na(nm), res$term, as.character(nm))
      }
      res <- res[res$p.adjust < 0.05, , drop = FALSE]
      res
    }

    for (f in fs) {
      ct <- sub("_diffgene\\.xls$", "", basename(f))
      df <- read.delim(f, stringsAsFactors = FALSE)
      sig <- df$gene[!is.na(df$p_val_adj) & df$p_val_adj < 0.05 & abs(df$avg_log2FC) > 0.5]
      sig <- sig[!grepl("^mt-|^MT-|^Mt-", sig)]
      if (length(sig) < 10) { cat("  ", ct, "差异基因不足，跳过\n"); next }
      draw_bar(run_hyper(sig, bgGoN, "GO"),  file.path(d, paste0(ct, "_GO_BP")))
      draw_bar(run_hyper(sig, bgKeN, "PATH", keName), file.path(d, paste0(ct, "_KEGG")))
    }
  }
}

cat("\n===== v2 出图完成 ->", outdir, "=====\n")
