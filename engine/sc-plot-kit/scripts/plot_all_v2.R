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

# ---- R 包库路径：从环境变量或配置读，不写死 ----
#   以前写死 D:/soft/R/library，换一台机器就找不到包。
#   优先顺序：环境变量 R_LIBS_USER / XIAOBA_R_LIB → 配置里的 r_lib_paths → 交给 R 自己找。
.lib_candidates <- c(Sys.getenv("XIAOBA_R_LIB", ""), Sys.getenv("R_LIBS_USER", ""))
.lib_candidates <- .lib_candidates[nzchar(.lib_candidates) & dir.exists(.lib_candidates)]
if (length(.lib_candidates) > 0) .libPaths(c(.lib_candidates, .libPaths()))

# 脚本自己的位置：守卫要靠它往上找到 workflow/。
#   必须放在守卫块之前 —— 守卫块和 sc_guard_banner 都要用这个变量。
.self_dir <- local({
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(f) && length(f) && nzchar(f)) return(dirname(normalizePath(f)))
  ca <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", ca, value = TRUE)
  if (length(m) > 0) {
    p <- sub("^--file=", "", m[1])
    if (nzchar(p) && file.exists(p)) return(dirname(normalizePath(p)))
  }
  "."
})

# ---- 入口守卫：拒绝被直接调用 ----
#   这不是装饰。它让「必须走统一入口」这件事由代码保证，而不是靠文档里的约定。
#   守卫位置：脚本往上找 workflow/guard_load.R。找不到就停 —— 不静默跳过。
#   为什么必须停：守卫缺失时继续跑，等于约束失效而没人知道，比直接报错危险得多。
.guard_load <- local({
  cands <- c(file.path(.self_dir, "..", "..", "workflow", "guard_load.R"),
             file.path(.self_dir, "..", "workflow", "guard_load.R"),
             file.path(.self_dir, "..", "..", "..", "workflow", "guard_load.R"))
  hit <- cands[file.exists(cands)]
  if (!length(hit)) {
    stop("\n  拒绝运行：找不到入口守卫 workflow/guard_load.R。\n",
         "  本脚本必须经由统一入口执行，守卫缺失时不能继续。\n",
         "  找过：\n    ", paste(cands, collapse = "\n    "), "\n", call. = FALSE)
  }
  hit[1]
})
source(.guard_load, local = FALSE)
# 跳过记录的小库：必须在生产入口里真 source，不能靠测试预先加载。
.diff_skip_lib <- file.path(.self_dir, "lib", "diff_skip.R")
if (!file.exists(.diff_skip_lib))
  stop("\n  拒绝运行：找不到 lib/diff_skip.R（差异跳过记账依赖它）。\n",
       "  期望位置：", .diff_skip_lib, "\n", call. = FALSE)
source(.diff_skip_lib, local = FALSE)
sc_guard_banner("plots", .self_dir)

suppressMessages({
  library(Seurat); library(Matrix); library(dplyr)
  library(ggplot2); library(patchwork); library(ggrepel)
})

args <- commandArgs(trailingOnly = TRUE)
get_opt <- function(k, d = NULL) { i <- match(k, args); if (is.na(i) || i == length(args)) return(d); args[i + 1] }
has_flag <- function(k) k %in% args
rds     <- args[1]
outdir  <- get_opt("--outdir", NULL)          # 必填，不再给本机默认路径
stages  <- strsplit(get_opt("--stage", "qc,cluster,markgene,annot,ratio,diff,enrich"), ",")[[1]]
gcol    <- get_opt("--group-col", "group")
scol    <- get_opt("--sample-col", "orig.ident")
ccol    <- get_opt("--celltype-col", "annot")
compare <- get_opt("--compare", NULL)
# 物种：命令行直接给，优先级高于配置文件。
#   为什么要这个参数：富集段必须知道用哪个物种的注释库。
#   原来只能从 --config 的 plots.species 读，而下发命令不带 --config，
#   于是每次都读到空 → 富集段跳过 → 一个文件都不产。
#   结果是"点击、跑完、没结果"，人只能来问为什么。
#   物种属于"这批数据是什么"，调度方（应用）知道，就该由它传。
species_opt <- get_opt("--species", NULL)

# ---- 版式配置：从 pipeline.json 读（若给了 --config）----
#   支持两种来源，命令行优先，其次是配置文件里的 plots 段。
PLOT_CONFIG <- list()
cfg_path_plot <- get_opt("--config", NULL)
if (!is.null(cfg_path_plot) && file.exists(cfg_path_plot)) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("给了 --config 但缺少 jsonlite 包（读配置用）。安装：install.packages('jsonlite')", call. = FALSE)
  }
  .pc <- jsonlite::fromJSON(cfg_path_plot, simplifyVector = FALSE)
  if (!is.null(.pc$plots)) PLOT_CONFIG <- .pc$plots
  cat("已读版式配置: ", cfg_path_plot, "\n")
}
# 命令行覆盖单值
if (!is.null(get_opt("--dpi"))) PLOT_CONFIG$dpi <- as.numeric(get_opt("--dpi"))

# ---------------------------------------------------------------------
# 版式与配色：一律从配置读，脚本里不再写死。
#   为什么：以前把「Mock 红 / Microplastics 青」写死在这里，
#   换一组分组名（比如 Treatment/Control）就会静默画错颜色或直接报错。
#   现在颜色按「分组名的顺序」取自一个配色方案，方案名由配置给。
#   配置缺失时用一份中性默认方案，并在日志里点名说明用了默认。
# ---------------------------------------------------------------------
PLOT_CFG <- if (!is.null(PLOT_CONFIG) && is.list(PLOT_CONFIG)) PLOT_CONFIG else list()

# ---- 富集段用哪个物种的库：从配置读，脚本里不写死 ----
#   原来写死 org.Mm.eg.db + KEGG mmu。换人类数据时它不会报错，
#   而是拿小鼠库去比人类基因 —— 错得看不出来，比直接失败更糟。
#   读不到可识别的物种时，富集段跳过并说明，不拿小鼠库顶上。
SPECIES_MAP <- list(
  mouse = list(db = "org.Mm.eg.db", kegg = "mmu", label = "Mus musculus \\(house mouse\\)"),
  human = list(db = "org.Hs.eg.db", kegg = "hsa", label = "Homo sapiens \\(human\\)"),
  rat   = list(db = "org.Rn.eg.db", kegg = "rno", label = "Rattus norvegicus \\(rat\\)")
)
.norm_species <- function(x) {
  if (is.null(x) || !length(x)) return("")
  x <- tolower(trimws(as.character(x)[1]))
  if (x %in% names(SPECIES_MAP)) return(x)
  if (grepl("mouse|mus", x)) return("mouse")
  if (grepl("human|homo", x)) return("human")
  if (grepl("rat|rattus", x)) return("rat")
  x
}
ENRICH_SPECIES <- local({
  v <- if (!is.null(species_opt) && nzchar(trimws(as.character(species_opt)))) {
    species_opt
  } else PLOT_CFG$species
  if (is.null(v) && exists("cfg_path_plot") && !is.null(cfg_path_plot) &&
      nzchar(cfg_path_plot) && file.exists(cfg_path_plot)) {
    .p <- tryCatch(jsonlite::fromJSON(cfg_path_plot, simplifyVector = FALSE),
                   error = function(e) NULL)
    if (!is.null(.p$params$species$value)) v <- .p$params$species$value
    else if (!is.null(.p$params$species)) v <- .p$params$species
  }
  .norm_species(v)
})
ENRICH_DB <- SPECIES_MAP[[ENRICH_SPECIES]]



pick <- function(keys, default = NULL) {
  for (k in keys) {
    v <- PLOT_CFG[[k]]
    if (!is.null(v)) return(v)
  }
  default
}

# 出图尺寸（英寸）与 dpi：逐个图位可在配置里覆盖
SIZE <- list(
  qc      = pick(c("size_qc"),      c(10, 8)),
  cluster = pick(c("size_cluster"), c(10, 8)),
  markgene= pick(c("size_markgene"),c(10, 8)),
  annot   = pick(c("size_annot"),   c(10, 8)),
  ratio   = pick(c("size_ratio"),   c(10, 8)),
  diff    = pick(c("size_diff"),    c(10, 10)),
  enrich  = pick(c("size_enrich"),  c(10, 10))
)
DPI <- pick(c("dpi"), 300)

# 分组配色：按实际出现的分组名顺序分配；配置可用 group_colors 指定映射
GRP_MAP <- pick(c("group_colors"), NULL)
GRP_PALETTE <- pick(c("group_palette"), c("#E64B35", "#4DBBD5", "#00A087", "#3C5488",
                                          "#F39B7F", "#8491B4", "#91D1C2", "#DC0000"))
VOLC_MAP <- pick(c("volcano_colors"), c(UP = "#E64B35", DOWN = "#00A087", NO = "#3C5488"))
CORAL   <- pick(c("qc_color"), "#F8766D")
CT_PALETTE <- pick(c("celltype_palette"), c('#53A85F','#58A4C3','#AB3282','#8C549C','#BD956A','#57C3F3','#6778AE',
                 '#F3B1A0','#F1BB72','#DCC1DD','#E95C59','#625D9E','#F7F398','#E63863',
                 '#5F3D69','#C5DEBA','#CCE0F5','#B53E2B','#AA9A59','#E39A35','#91D0BE',
                 '#23452F','#E4C755','#585658','#C1E6F3','#D6E7A3','#712820','#CCC9E6',
                 '#3A6963','#68A180','#61B934','#476D87','#9FA3A8','#968175'))

if (is.null(outdir) || is.na(outdir)) stop("必填: --outdir <输出目录>", call. = FALSE)
if (is.null(rds) || is.na(rds))       stop("必填: 第一个位置参数 <seurat.rds>", call. = FALSE)
if (!file.exists(rds))                stop("rds 不存在: ", rds, call. = FALSE)

obj <- readRDS(rds)
cat("载入对象：", ncol(obj), "细胞\n")

# 铁律：不做静默降级。分组列不存在就报错，不偷偷改用 orig.ident
if (!gcol %in% colnames(obj@meta.data)) {
  stop("分组列 '", gcol, "' 不在对象元数据里。\n",
       "  现有列: ", paste(colnames(obj@meta.data), collapse = ", "), "\n",
       "  请显式指定正确列名（--group-col）；本脚本不会自动改用其他列。", call. = FALSE)
}
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# 分组配色：按本次实际出现的分组名现场构建，不预设名字
if (is.null(GRP_MAP)) {
  lv <- sort(unique(as.character(obj@meta.data[[gcol]])))
  lv <- lv[!is.na(lv) & nzchar(lv)]
  GRP_MAP <- setNames(rep_len(GRP_PALETTE, length(lv)), lv)
  cat("分组配色（按本次实际分组名自动分配）: ",
      paste(names(GRP_MAP), GRP_MAP, sep = "=", collapse = "  "), "\n")
} else {
  cat("分组配色（来自配置）: ",
      paste(names(GRP_MAP), unlist(GRP_MAP), sep = "=", collapse = "  "), "\n")
}
GRP_COL   <- unlist(GRP_MAP)
VOLC_COL  <- unlist(VOLC_MAP)
color_panel <- CT_PALETTE
SAMPLE_COL <- grep("^sample_", names(PLOT_CFG), value = TRUE)
SAMPLE_COL <- if (length(SAMPLE_COL) > 0) unlist(PLOT_CFG[[SAMPLE_COL[1]]]) else
  c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D")

# 统一出图：白底（原报告 PNG 是透明底，导出会变黑，必须显式白底）
save_plot <- function(p, path, w = 10, h = 8, dpi = DPI) {
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
  # 这里不能再传 group.colors：新版 ggplot 下它会让 DoHeatmap 内部
  #   走 as.raster.matrix 那条路并报错，marker 热图整段挂掉。
  #   配色由下面的 scale_fill_gradient2 控制。
  p_hm <- DoHeatmap(obj, features = as.character(unique(top10$gene))) +
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

    # ---- 2026-11-03 修：DotPlot 的 invalid character indexing ----
    # 报错原文：
    #   Warning: cells.idents 中 NA 项替换长度不匹配
    #   Error in intI(j, n = d[2L], dn[[2L]], give.dn = FALSE) : invalid character indexing
    # 根因两条，都在画图前的取数上，不在 DotPlot 本身：
    #   ① 基因名不干净：marker 表里有 NA / 空串 / 带引号或换行的名字。
    #      Seurat 按名字去 assay 里取行，拿到 NA 就往 intI 里传，
    #      那里只接受合法字符索引，于是直接 abort。
    #      同一批数据里 Cyp4b1 只在 RNA assay 里找到、不在默认 search 位置，
    #      正是「名字对不上」的另一种表现，同源问题。
    #   ② 分群标签里有 NA：annot 列在底妆阶段没打上类型的细胞是 NA，
    #      转成 factor 后 DotPlot 按 ident 名去配色取子集，NA 名字对不上长度。
    # 修法：画之前把这两类脏数据挡掉，并且**不因为一张点图毁掉整段出图**。
    genes <- unique(as.character(sig$gene))
    genes <- genes[!is.na(genes) & nzchar(trimws(genes))]
    genes <- genes[!grepl("[\r\n\t\"']", genes)]
    genes <- genes[genes %in% rownames(obj)]
    reps <- sig %>% dplyr::filter(gene %in% genes) %>%
      group_by(cluster) %>% slice_max(avg_log2FC, n = 2)
    dot_genes <- unique(as.character(reps$gene))
    dot_genes <- dot_genes[!is.na(dot_genes) & nzchar(dot_genes)]

    # 细胞层面：把 annot 为 NA 的细胞排除，而不是带着 NA 去画。
    #   为什么排除而不是填个"未分型"：填字符串会让图多出一个不存在的类型，
    #   读图的人会以为那是真的一个群。排除并在日志里写明排了多少，更诚实。
    idv <- as.character(obj@meta.data[[ccol]])
    n_na <- sum(is.na(idv) | !nzchar(trimws(idv)))
    if (n_na > 0) {
      cat("   annot 列为空/NA 的细胞:", n_na, "个，画点图时排除\n")
    }
    keep <- !(is.na(idv) | !nzchar(trimws(idv)))
    obj_dot <- obj[, keep]
    Idents(obj_dot) <- factor(as.character(obj_dot@meta.data[[ccol]]))

    if (length(dot_genes) >= 1 && ncol(obj_dot) > 0) {
      p_dot <- tryCatch({
        DotPlot(obj_dot, features = dot_genes,
                cols = c("blue", "lightpink", "red")) +
          RotatedAxis() + labs(x = "", y = "")
      }, error = function(e) {
        # 退一步：只取前 30 个基因再试一次。
        #   有些基因名合法但在此对象里取不出表达值，整体取数仍会炸；
        #   缩短到一屏能画完的量能把这类边缘情况绕过去。
        cat("   !! DotPlot 首次失败:", conditionMessage(e), "\n   -> 改用前 30 个基因重试\n")
        g2 <- head(dot_genes, 30)
        tryCatch(DotPlot(obj_dot, features = g2,
                         cols = c("blue", "lightpink", "red")) +
                   RotatedAxis() + labs(x = "", y = ""),
                 error = function(e2) {
                   cat("   !! DotPlot 仍然失败，跳过这张图：", conditionMessage(e2), "\n")
                   NULL
                 })
      })
      if (!is.null(p_dot)) {
        save_plot(p_dot, file.path(d, "genelist_dotplot"), 16, 8)
      } else {
        # 写一份说明，别让下游以为"图忘了画"。
        #   PPT 那一步会按这份说明报"缺哪张、为什么缺"，而不是干巴巴一句缺图。
        writeLines(c(
          "genelist_dotplot.png 本次未生成",
          paste0("时间: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
          paste0("原因: DotPlot 两次尝试均失败，标记基因数 ", length(dot_genes),
                 "，可用细胞 ", ncol(obj_dot)),
          "处理: 出图其余各段不受影响；PPT 会按缺图策略继续或跳过该页"
        ), file.path(d, "genelist_dotplot.SKIPPED.txt"))
      }
    } else {
      cat("   没有可用的标记基因画点图，跳过\n")
      writeLines("genelist_dotplot.png 未生成：可用标记基因为 0",
                 file.path(d, "genelist_dotplot.SKIPPED.txt"))
    }
  } else {
    cat("   未找到", f_sig, "，跳过 DotPlot\n")
    writeLines(paste0("genelist_dotplot.png 未生成：找不到 ", f_sig),
               file.path(d, "genelist_dotplot.SKIPPED.txt"))
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
  # 未定型细胞必须留在分母里。
  #   table() 默认丢 NA，注释后没判出类型的细胞会被静默排除，
  #   于是每个类型的比例都被抬高，而总数看起来还是"干净"的 ——
  #   这是最容易被当成正常结果的一类错。
  md$.ct <- as.character(md[[ct]])
  md$.ct[is.na(md$.ct) | !nzchar(md$.ct)] <- "unassigned"

  # 表：按组、按样本
  tbl <- function(gv) {
    t <- as.data.frame(table(md$.ct, md[[gv]]))
    colnames(t) <- c("cell_type", "lvl", "cell_number")
    tot <- aggregate(cell_number ~ lvl, t, sum)
    t$rate <- round(100 * t$cell_number / tot$cell_number[match(t$lvl, tot$lvl)], 2)
    t
  }
  # 比例表落盘时把未定型一并写出，便于核对分母。
  cat("   比例分母含 unassigned:", sum(md$.ct == "unassigned"), " 个细胞\n")
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
  # 跳过的类型要留记录，不能只在日志里一句"跳过"。
  #   否则事后无法区分「本来就没有差异」与「跑了但失败被跳过」。
  skipped <- diff_skip_init()
  for (t0 in setdiff(rownames(cnt), ok)) {
    skipped <- diff_skip_add(skipped, t0, "skipped_lt3",
                             "某一组细胞数 < 3", cnt[t0, a], cnt[t0, b])
  }
  cat("   纳入细胞类型:", length(ok), "\n")
  Idents(obj) <- ccol
  # 先落一份（此时可能只有 skipped_lt3），循环里遇错会再写一次并停。
  diff_skip_write(skipped, file.path(d, "diff_skipped.xls"))
  for (t in ok) {
    r <- try(FindMarkers(obj, ident.1 = a, ident.2 = b, group.by = gcol,
                         subset.ident = t, logfc.threshold = 0, verbose = FALSE), silent = TRUE)
    if (inherits(r, "try-error")) {
      cat("   !! 差异分析失败，跳过该细胞类型:", t, "|", as.character(r), "\n")
      # 取真实原因，不要把整段堆栈写进表：as.character(try-error) 带堆栈与换行。
      err_msg <- tryCatch(conditionMessage(attr(r, "condition")),
                          error = function(e) as.character(r))
      skipped <- diff_skip_add(skipped, t, "error",
        paste0("FindMarkers 报错：", substr(err_msg, 1, 160)),
        cnt[t, a], cnt[t, b])
      diff_skip_write(skipped, file.path(d, "diff_skipped.xls"))
      # 计算异常不能当整步成功：写清记录后停下来，交给人判断。
      stop("差异分析异常：", t, " | ", substr(err_msg, 1, 160), call. = FALSE)
    }
    if (nrow(r) == 0) {
      cat("   !! 该细胞类型无差异基因，跳过:", t, "\n")
      skipped <- diff_skip_add(skipped, t, "no_sig",
        "该类型没有通过阈值的差异基因（执行成功，不是失败）",
        cnt[t, a], cnt[t, b])
      next
    }
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
  # 循环结束后必须再写一次：no_sig 这类行是在循环里追加的，
  #   只在循环前写那一份的话，它们永远落不了盘。
  #   异常路径已在循环内「写表后 stop」，不会走到这里。
  diff_skip_write(skipped, file.path(d, "diff_skipped.xls"))
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
  # 这三种情况都算「这一步没做成」，不能 cat 一句就 exit 0：
  #   退出码为 0 会让下游把"没跑"当成"跑了但没显著"。
  if (is.null(ENRICH_DB))
    stop("富集缺物种/注释库：读到物种=",
         if (nzchar(ENRICH_SPECIES)) ENRICH_SPECIES else "空",
         "。补法：命令行加 --species mouse（或 human / rat），",
         "或在 --config 的 plots.species 里写。", call. = FALSE)
  if (!requireNamespace(ENRICH_DB$db, quietly = TRUE))
    stop("富集注释库不可用：", ENRICH_DB$db, call. = FALSE)
  if (length(fs) == 0)
    stop("富集找不到差异基因表（需先跑 diff 段）：", ddir, call. = FALSE)
  {
    suppressMessages(library(ENRICH_DB$db, character.only = TRUE))
    db <- get(ENRICH_DB$db)
    BG <- keys(db, keytype = "SYMBOL"); N <- length(BG)

    # 背景：GO(BP) 每个 term 的基因数
    bgGo <- suppressMessages(select(db, keys = BG, columns = c("GO", "ONTOLOGY"), keytype = "SYMBOL"))
    bgGo <- bgGo[!is.na(bgGo$GO) & bgGo$ONTOLOGY == "BP", ]
    # 必须按唯一 (SYMBOL, GO) 计：org.Mm.eg.db 里同一基因同一 term 会有多条
    #   EVIDENCE 记录，直接 table() 会把这些重复算成"更多基因"，
    #   于是 q 与 ms 都虚高，超几何检验的分母失真。
    bgGo <- unique(bgGo[, c("SYMBOL", "GO")])
    bgGoN <- table(bgGo$GO)
    # 背景：KEGG 每个通路编号的基因数（物种由配置决定）
    bgKe <- suppressMessages(select(db, keys = BG, columns = "PATH", keytype = "SYMBOL"))
    bgKe <- bgKe[!is.na(bgKe$PATH), ]
    bgKe <- unique(bgKe[, c("SYMBOL", "PATH")])
    bgKeN <- table(bgKe$PATH)
    # KEGG 通路英文全称（注意 keggList("mmu") 给的是基因/CDS 列表，
    # 取通路名必须用 keggList("pathway", "mmu")，且值尾部带 " - Mus musculus (house mouse)" 需去掉）
    keName <- try({
      kl <- KEGGREST::keggList("pathway", ENRICH_DB$kegg)
      nm <- sub(paste0(" - ", ENRICH_DB$label, "$"), "", as.character(kl))
      stats::setNames(nm, sub(paste0("^", ENRICH_DB$kegg), "", names(kl)))
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
      fn_xls <- paste0(fn, ".xls")
      # 0 条也要落一份带表头的表：文件不存在与"确实没有显著条目"是两件事。
      if (is.null(res) || nrow(res) == 0) {
        empty <- data.frame(term = character(0), Count = integer(0),
                            pvalue = numeric(0), p.adjust = numeric(0),
                            GeneRatio = numeric(0), stringsAsFactors = FALSE)
        write.table(empty, fn_xls, quote = FALSE, sep = "\t", row.names = FALSE)
        return(list(rows_written = 0L, sig_total = 0L,
                    result_md5 = tryCatch(unname(tools::md5sum(fn_xls)),
                                          error = function(e) "")))
      }
      sig_total <- nrow(res)
      res <- res[order(res$p.adjust), , drop = FALSE]
      res <- res[1:min(30, nrow(res)), , drop = FALSE]
      write.table(res, fn_xls, quote = FALSE, sep = "\t", row.names = FALSE)
      # 落盘行数才是 hits；显著总数另记，别让截断影响状态表的自洽。
      rows_written <- nrow(res)
      res$term <- factor(res$term, levels = rev(res$term))
      p <- ggplot(res, aes(x = Count, y = term, fill = p.adjust)) +
        geom_col() +
        scale_fill_gradientn(colours = c("#E64B35", "#C51B7D", "#7B3294", "#3C5488")) +
        labs(x = "Count", y = NULL, fill = "p.adjust") +
        theme_bw(base_size = 9) +
        theme(panel.grid.major.y = element_line(colour = "grey90"),
              panel.grid.minor = element_blank())
      save_plot(p, fn, 10, 7)
      cat("   ->", basename(fn), ":", rows_written, "条（显著共", sig_total, "条）\n")
      list(rows_written = rows_written, sig_total = sig_total,
           result_md5 = tryCatch(unname(tools::md5sum(fn_xls)), error = function(e) ""))
    }

    # 超几何富集（col 用 GO 或 PATH）
    # 返回 list(res, status, reason, n_in, n_map, n_terms)：
    #   以前无结果就 return(NULL)，下游无法区分"没映射上""阈值不够""算失败"。
    run_hyper <- function(genes, bgN, col, nameMap = NULL) {
      g <- intersect(unique(genes), BG)
      n_in <- length(unique(genes)); n_map <- length(g)
      if (n_map < 5)
        return(list(res = NULL, status = "lt5_mapped", n_in = n_in, n_map = n_map,
                    n_terms = 0L, reason = "映射到注释库的基因少于 5 个"))
      m <- suppressMessages(select(db, keys = g, columns = col, keytype = "SYMBOL"))
      m <- m[!is.na(m[[col]]), ]
      m <- unique(m[, c("SYMBOL", col)])
      m <- m[m[[col]] %in% names(bgN), ]
      if (nrow(m) == 0)
        return(list(res = NULL, status = "no_mapping", n_in = n_in, n_map = n_map,
                    n_terms = 0L, reason = "这些基因在背景里没有对应条目"))
      hit <- table(m[[col]]); terms <- names(hit); q <- as.integer(hit)
      ms <- as.integer(bgN[terms]); ms[is.na(ms)] <- 0
      keep <- ms >= 5 & ms <= 500
      terms <- terms[keep]; q <- q[keep]; ms <- ms[keep]
      if (length(terms) == 0)
        return(list(res = NULL, status = "no_term_in_range", n_in = n_in, n_map = n_map,
                    n_terms = 0L, reason = "没有落在背景基因数区间内的条目"))
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
      res_all <- res
      res <- res[res$p.adjust < 0.05, , drop = FALSE]
      list(res = res, status = if (nrow(res)) "ok" else "no_sig",
           n_in = n_in, n_map = n_map, n_terms = nrow(res_all),
           reason = if (nrow(res)) "" else "有映射但无条目通过 p.adjust < 0.05")
    }

    # 文本规范化：TSV 里出现换行会把一条记录拆成两行。
    enr_one <- function(x) trimws(gsub("[\r\n\t]+", " ", as.character(x)))
    status_rows <- list()
    for (f in fs) {
      ct <- sub("_diffgene\\.xls$", "", basename(f))
      if (!file.exists(f)) stop("差异基因表不存在：", f, call. = FALSE)
      df <- read.delim(f, stringsAsFactors = FALSE)
      need <- c("gene", "p_val_adj", "avg_log2FC")
      miss <- setdiff(need, names(df))
      if (length(miss))
        stop("差异基因表缺列：", paste(miss, collapse = ", "),
             "（", basename(f), "）", call. = FALSE)
      dm5 <- tryCatch(unname(tools::md5sum(f)), error = function(e) "")
      sig <- df$gene[!is.na(df$p_val_adj) & df$p_val_adj < 0.05 & abs(df$avg_log2FC) > 0.5]
      sig <- sig[!grepl("^mt-|^MT-|^Mt-", sig)]
      for (an in c("GO_BP", "KEGG")) {
        res_rel <- file.path("4.diff_enrich", "enrich", paste0(ct, "_", an, ".xls"))
        if (length(sig) < 10) {
          w0 <- draw_bar(NULL, file.path(d, paste0(ct, "_", an)))
          status_rows[[length(status_rows) + 1L]] <- data.frame(
            cell_type = enr_one(ct), analysis = an, input_genes = length(sig),
            mapped_genes = 0L, terms = 0L, sig_total = 0L,
            rows_written = w0$rows_written, status = "lt10_sig",
            diff_rel = enr_one(file.path("4.diff_enrich", "diff", basename(f))),
            diff_md5 = dm5, result_rel = enr_one(res_rel),
            result_md5 = w0$result_md5,
            reason = "显著差异基因少于 10 个", stringsAsFactors = FALSE)
          next
        }
        r <- tryCatch(
          if (an == "GO_BP") run_hyper(sig, bgGoN, "GO")
          else run_hyper(sig, bgKeN, "PATH", keName),
          error = function(e) stop("富集计算异常（", ct, " / ", an, "）：",
                                   conditionMessage(e), call. = FALSE))
        w <- draw_bar(r$res, file.path(d, paste0(ct, "_", an)))
        status_rows[[length(status_rows) + 1L]] <- data.frame(
          cell_type = enr_one(ct), analysis = an, input_genes = r$n_in,
          mapped_genes = r$n_map, terms = r$n_terms,
          sig_total = w$sig_total, rows_written = w$rows_written,
          status = r$status,
          diff_rel = enr_one(file.path("4.diff_enrich", "diff", basename(f))),
          diff_md5 = dm5, result_rel = enr_one(res_rel),
          result_md5 = w$result_md5, reason = enr_one(r$reason),
          stringsAsFactors = FALSE)
      }
    }
    if (length(status_rows)) {
      st <- do.call(rbind, status_rows)
      write.table(st, file.path(d, "enrich_status.xls"), quote = FALSE, sep = "\t",
                  row.names = FALSE)
      cat("   状态表:", nrow(st), "行（", paste(unique(st$status), collapse = "/"), "）\n")
    }
  }
}

cat("\n===== v2 出图完成 ->", outdir, "=====\n")
