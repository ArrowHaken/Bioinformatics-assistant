#!/usr/bin/env Rscript
# =====================================================================
# process_data.R
# 单细胞转录组「数据处理」一体化参数化脚本
#
# 整合来源（只读取参考，未改动原文件）：
#   01_process.R —— 读 10x 矩阵 / 建对象 / 质控 / 降采样 / 合并 / 归一化 / PCA
#   02_cluster.R —— harmony 去批次 / 邻居 / 聚类 / UMAP / 保存
#
# 一句话流程：
#   读矩阵 -> 建 Seurat 对象 -> 质控过滤 -> 按需降采样 -> 合并
#   -> 标准化 -> 高变基因 -> PCA -> harmony 去批次(按样本)
#   -> 邻居 -> 聚类 -> UMAP -> 保存 rds
#
# 兼容性：Seurat v4 / v5（v5 用 Assay5 分层存储，合并后需 JoinLayers）
# 跨平台：Windows / Linux 均可，路径统一用 file.path 拼接
#
# 用法（Linux / macOS）：
#   Rscript process_data.R \
#     --indir   /path/to/matrix \
#     --outdir  /path/to/out \
#     --samples samples_example.tsv \
#     --exclude 18m-G761 \
#     --downsample 1500
#
# 用法（Windows PowerShell）：
#   Rscript .\process_data.R --indir D:\data\matrix --outdir D:\data\out `
#     --samples .\samples_example.tsv --exclude 18m-G761 --downsample 1500
# =====================================================================

## ---------------------------------------------------------------------
## 0. 库路径与依赖
## ---------------------------------------------------------------------

# Windows 用户机的本地包库（目录存在时优先使用）；Linux 上该目录不存在会自动跳过
if (dir.exists("D:/soft/R/library")) .libPaths(c("D:/soft/R/library", .libPaths()))

suppressMessages({
  library(Seurat)
  library(Matrix)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
})

# harmony 用于按样本去批次；若未安装，脚本给出警告并在该步自动回退到 PCA
HAS_HARMONY <- suppressWarnings(suppressMessages(requireNamespace("harmony", quietly = TRUE)))
if (HAS_HARMONY) suppressMessages(library(harmony))

# Seurat v4 / v5 兼容开关：v5 使用 Assay5 分层存储，读矩阵前必须先声明
if (utils::packageVersion("Seurat") >= "5.0.0") {
  options(Seurat.object.assay.version = "v5")
} else {
  options(Seurat.object.assay.version = "v4")
}

# 固定随机种子，保证降采样 / UMAP / 聚类结果可复现
set.seed(42)

## ---------------------------------------------------------------------
## 1. 日志工具：每步打印 时间戳 + 当前步骤 + 细胞数
## ---------------------------------------------------------------------

LOG <- function(...) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste0(...)))
}
# 步骤标题 + 可选的对象（有对象时顺带打印细胞数）
STEP <- function(title, obj = NULL) {
  cat(sprintf("\n[%s] ===== %s =====\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), title))
  if (!is.null(obj)) LOG("  当前细胞数: ", ncol(obj))
}
# 实时刷新输出，便于在 Windows / Linux 控制台看到进度
flush.console <- function() invisible(NULL)

## ---------------------------------------------------------------------
## 2. 命令行参数解析（--key value 与 --key=value 两种写法都支持）
## ---------------------------------------------------------------------

parse_args <- function(argv) {
  out <- list()
  i <- 1
  while (i <= length(argv)) {
    a <- argv[i]
    if (!grepl("^--", a)) { i <- i + 1; next }   # 忽略位置参数
    if (grepl("=", a, fixed = TRUE)) {
      # 形如 --dims=30
      key <- sub("^--", "", sub("=.*$", "", a))
      val <- sub("^[^=]*=", "", a)
    } else {
      # 形如 --dims 30（下一个 token 若仍以 -- 开头，则该参数记作 TRUE）
      key <- sub("^--", "", a)
      if (i < length(argv) && !grepl("^--", argv[i + 1])) {
        i <- i + 1
        val <- argv[i]
      } else {
        val <- "TRUE"
      }
    }
    key <- gsub("-", "_", key)   # --mt-cutoff -> mt_cutoff
    out[[key]] <- val
    i <- i + 1
  }
  out
}

# 取值并转数值；未提供时用默认值
num_opt <- function(x, default) {
  if (is.null(x) || identical(x, "TRUE")) return(default)
  as.numeric(x)
}

opt <- parse_args(commandArgs(trailingOnly = TRUE))

# 参数列表（含默认值）
args <- list(
  indir        = opt$indir,                                   # 10x 矩阵根目录（每样本一个子目录）
  outdir       = opt$outdir,                                  # 输出目录
  samples      = opt$samples,                                 # 样本表文件（两列：样本目录名 + 分组名），可选
  exclude      = opt$exclude,                                 # 要剔除的样本名，逗号分隔，可选
  downsample   = as.integer(num_opt(opt$downsample,   0)),    # 每样本降采样细胞数，0 = 不降采样
  nfeatures    = as.integer(num_opt(opt$nfeatures,  2000)),   # 高变基因数
  dims         = as.integer(num_opt(opt$dims,         30)),   # PCA 维度
  resolution   = num_opt(opt$resolution,            0.5),     # 聚类分辨率
  mt_cutoff    = num_opt(opt$mt_cutoff,             20),      # 线粒体比例上限(%)
  min_features = as.integer(num_opt(opt$min_features, 200)),  # 基因数下限
  max_features = as.integer(num_opt(opt$max_features, 6000))  # 基因数上限
)

# 必填参数校验：缺 indir / outdir 时打印用法并退出
if (is.null(args$indir) || is.null(args$outdir)) {
  cat("单细胞数据处理一体化脚本 process_data.R\n\n")
  cat("必填参数:\n")
  cat("  --indir        10x 矩阵所在目录（每个样本一个子目录）\n")
  cat("  --outdir       输出目录\n\n")
  cat("可选参数:\n")
  cat("  --samples      样本表文件（两列制表符分隔：样本目录名 分组名），默认自动扫描 indir 子目录\n")
  cat("  --exclude      要剔除的样本名，逗号分隔，默认不剔除\n")
  cat("  --downsample   每样本降采样细胞数，默认 0（不降采样）\n")
  cat("  --nfeatures    高变基因数，默认 2000\n")
  cat("  --dims         PCA 维度，默认 30\n")
  cat("  --resolution   聚类分辨率，默认 0.5\n")
  cat("  --mt-cutoff    线粒体比例上限(%)，默认 20\n")
  cat("  --min-features 基因数下限，默认 200\n")
  cat("  --max-features 基因数上限，默认 6000\n")
  quit(status = 1)
}

indir  <- args$indir
outdir <- args$outdir
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# 输出目录不存在或不可写时直接报错，避免跑到一半才发现
if (!dir.exists(outdir)) stop("无法创建输出目录: ", outdir)

LOG("Seurat 版本: ", as.character(utils::packageVersion("Seurat")),
    " | harmony: ", ifelse(HAS_HARMONY, "已安装", "未安装(将跳过去批次)"))
LOG("输入目录: ", indir)
LOG("输出目录: ", outdir)

## ---------------------------------------------------------------------
## 3. 确定样本列表与分组（支持样本表，也支持自动扫描目录）
## ---------------------------------------------------------------------

# 剔除列表：--exclude 逗号分隔
exclude_samples <- character(0)
if (!is.null(args$exclude) && nzchar(args$exclude)) {
  exclude_samples <- trimws(strsplit(args$exclude, ",")[[1]])
  exclude_samples <- exclude_samples[nzchar(exclude_samples)]
}

# 判断一个目录里是否有 CellRanger 三件套（支持 .gz 压缩）
is_10x_dir <- function(d) {
  any(file.exists(file.path(d, c("barcodes.tsv", "barcodes.tsv.gz",
                                 "features.tsv", "features.tsv.gz",
                                 "matrix.mtx",  "matrix.mtx.gz"))))
}

# 扫描 indir 下的样本子目录
scan_samples <- function(root) {
  dirs <- list.dirs(root, recursive = FALSE, full.names = FALSE)
  dirs[vapply(file.path(root, dirs), is_10x_dir, logical(1))]
}

sample_names <- character(0)   # 样本目录名（也是 orig.ident）
group_map    <- NULL           # 命名向量：样本名 -> 分组名

if (!is.null(args$samples) && nzchar(args$samples)) {
  # 使用样本表：第一列样本目录名，第二列分组名
  if (!file.exists(args$samples)) stop("样本表文件不存在: ", args$samples)
  st <- read.delim(args$samples, header = TRUE, sep = "\t",
                   colClasses = "character", check.names = FALSE,
                   stringsAsFactors = FALSE)
  if (ncol(st) < 2) stop("样本表至少需要两列：样本目录名 + 分组名")
  st <- st[nzchar(st[[1]]), , drop = FALSE]
  sample_names <- st[[1]]
  group_map    <- setNames(st[[2]], st[[1]])
  LOG("从样本表读取样本 ", length(sample_names), " 个: ", paste(sample_names, collapse = ", "))
} else {
  # 未提供样本表：自动扫描目录，分组名默认等于样本名
  sample_names <- sort(scan_samples(indir))
  if (length(sample_names) == 0) stop("在 ", indir, " 下没有找到任何 10x 矩阵子目录")
  group_map <- setNames(sample_names, sample_names)
  LOG("未提供样本表，自动扫描到 ", length(sample_names), " 个样本: ",
      paste(sample_names, collapse = ", "))
}

# 执行剔除
if (length(exclude_samples) > 0) {
  hit <- intersect(exclude_samples, sample_names)
  sample_names <- setdiff(sample_names, exclude_samples)
  LOG("剔除样本: ", paste(if (length(hit) > 0) hit else exclude_samples, collapse = ", "),
      " | 保留 ", length(sample_names), " 个样本")
}
if (length(sample_names) == 0) stop("剔除后没有剩余样本，请检查 --exclude 与 --samples")

# 检查矩阵目录是否齐全
missing_dir <- sample_names[!vapply(file.path(indir, sample_names), dir.exists, logical(1))]
if (length(missing_dir) > 0) stop("以下样本目录不存在: ", paste(missing_dir, collapse = ", "))

## ---------------------------------------------------------------------
## 4. 读入矩阵 + 建对象 + 质控过滤 + 按需降采样（逐样本）
## ---------------------------------------------------------------------

STEP("STEP 1 读入矩阵与质控")

qc_tab <- data.frame()   # 逐样本质控统计表
objs   <- list()         # 每个样本一个 Seurat 对象

for (s in sample_names) {
  sdir <- file.path(indir, s)

  # --- 读 10x 三件套（Read10X 自动识别 .gz 与新旧目录结构）---
  d <- Read10X(data.dir = sdir)
  # 多模态输出时只取基因表达矩阵
  if (is.list(d) && !is.matrix(d) && !inherits(d, "dgCMatrix")) {
    nm <- names(d)
    pick <- if ("Gene Expression" %in% nm) "Gene Expression" else nm[1]
    LOG("  ", s, " 检测到多模态矩阵，取: ", pick)
    d <- d[[pick]]
  }
  # 兜底：转成稀疏矩阵
  if (!inherits(d, "dgCMatrix")) d <- as(as.matrix(d), "CsparseMatrix")

  # --- 建 Seurat 对象（min.cells 过滤极低表达基因）---
  o <- CreateSeuratObject(counts = d, project = s,
                          min.cells = 3, min.features = args$min_features)
  rm(d); gc(verbose = FALSE)

  # --- 线粒体比例：自动识别人或小鼠的基因命名 ---
  feats <- rownames(o)
  mt_pattern <- if (any(grepl("^MT-", feats))) "^MT-" else "^mt-"
  o[["percent.mt"]] <- PercentageFeatureSet(o, pattern = mt_pattern)

  # --- 质控过滤：基因数上下限 + 线粒体比例上限 ---
  n0 <- ncol(o)
  o <- subset(o, subset = nFeature_RNA > args$min_features &
                          nFeature_RNA < args$max_features &
                          percent.mt   < args$mt_cutoff)
  n1 <- ncol(o)
  if (n1 == 0) stop("样本 ", s, " 质控后细胞数为 0，请放宽 --min-features / --mt-cutoff")

  # --- 按需降采样：每样本最多保留 --downsample 个细胞（0 = 不降采样）---
  if (args$downsample > 0 && ncol(o) > args$downsample) {
    o <- subset(o, cells = sample(colnames(o), args$downsample))
  }

  o$orig.ident <- s
  o$sample     <- s
  objs[[s]] <- o

  # 记录质控统计
  qc_tab <- rbind(qc_tab, data.frame(
    sample     = s,
    raw        = n0,
    filtered   = n1,
    used       = ncol(o),
    geneMedian = ceiling(median(o$nFeature_RNA)),
    mtMedian   = round(median(o$percent.mt), 2)
  ))
  LOG(sprintf("  %-12s raw=%6d  filtered=%6d  used=%5d  mtPattern=%s",
              s, n0, n1, ncol(o), mt_pattern))
}

# 质控表落盘（制表符分隔，Excel 可直接打开）
qc_file <- file.path(outdir, "qc_stat.tsv")
write.table(qc_tab, qc_file, quote = FALSE, sep = "\t", row.names = FALSE)
LOG("质控统计已写出: ", qc_file)

## ---------------------------------------------------------------------
## 5. 合并样本（Seurat v5 需 JoinLayers 合并分层）
## ---------------------------------------------------------------------

STEP("STEP 2 合并样本")

if (length(objs) == 1) {
  merged <- objs[[1]]
} else {
  merged <- merge(objs[[1]], y = objs[2:length(objs)], add.cell.ids = names(objs))
}
# v5 兼容：合并后 counts 会分 layer 存放，JoinLayers 合并；v4 无此函数则跳过
merged <- tryCatch({ JoinLayers(merged) }, error = function(e) merged)

# 分组信息（来自样本表；未提供样本表时等于样本名）
merged$group <- unname(group_map[as.character(merged$orig.ident)])
if (any(is.na(merged$group))) {
  LOG("  警告: 有样本未在样本表中匹配到分组，已回退为样本名")
  merged$group[is.na(merged$group)] <- as.character(merged$orig.ident)[is.na(merged$group)]
}
LOG("  合并后细胞数: ", ncol(merged), " | 样本数: ", length(unique(merged$orig.ident)))

## ---------------------------------------------------------------------
## 6. 标准化 -> 高变基因 -> 归一化 -> PCA
## ---------------------------------------------------------------------

STEP("STEP 3 标准化 / 高变基因 / PCA", merged)

merged <- NormalizeData(merged, verbose = FALSE)
LOG("  标准化完成")

merged <- FindVariableFeatures(merged, selection.method = "vst",
                               nfeatures = args$nfeatures, verbose = FALSE)
LOG("  高变基因数: ", length(VariableFeatures(merged)))

merged <- ScaleData(merged, verbose = FALSE)
LOG("  缩放完成")

merged <- RunPCA(merged, verbose = FALSE)
LOG("  PCA 完成，可用 PC 数: ", ncol(merged[["pca"]]))

## ---------------------------------------------------------------------
## 7. harmony 去批次（按样本 orig.ident）
## ---------------------------------------------------------------------

STEP("STEP 4 harmony 去批次", merged)

dims_use <- seq_len(min(args$dims, ncol(merged[["pca"]])))
reduction_use <- "pca"   # 去批次成功后改为 "harmony"

if (HAS_HARMONY && length(unique(merged$orig.ident)) > 1) {
  merged <- tryCatch({
    RunHarmony(merged, group.by.vars = "orig.ident", verbose = FALSE)
  }, error = function(e) {
    LOG("  harmony 运行失败(", conditionMessage(e), ")，回退使用 PCA")
    merged
  })
  if ("harmony" %in% names(merged@reductions)) {
    reduction_use <- "harmony"
    LOG("  harmony 去批次完成，降维用 harmony，维度 1:", max(dims_use))
  }
} else {
  LOG("  跳过 harmony（未安装或只有 1 个样本），降维用 PCA")
}

## ---------------------------------------------------------------------
## 8. 邻居 -> 聚类 -> UMAP
## ---------------------------------------------------------------------

STEP("STEP 5 邻居 / 聚类 / UMAP", merged)

merged <- FindNeighbors(merged, reduction = reduction_use, dims = dims_use, verbose = FALSE)
merged <- FindClusters(merged, resolution = args$resolution, verbose = FALSE)
n_clusters <- length(levels(merged$seurat_clusters))
LOG("  聚类完成，resolution=", args$resolution, "，簇数: ", n_clusters)

merged <- RunUMAP(merged, reduction = reduction_use, dims = dims_use, verbose = FALSE)
LOG("  UMAP 完成")

## ---------------------------------------------------------------------
## 9. 保存结果（rds + 元数据表）
## ---------------------------------------------------------------------

STEP("STEP 6 保存结果", merged)

rds_file <- file.path(outdir, "processed.rds")
saveRDS(merged, rds_file)
LOG("  已保存 Seurat 对象: ", rds_file)

meta_cols <- intersect(c("orig.ident", "sample", "group", "seurat_clusters"),
                       colnames(merged@meta.data))
meta_file <- file.path(outdir, "meta.tsv")
write.table(data.frame(cell = rownames(merged@meta.data), merged@meta.data[, meta_cols, drop = FALSE]),
            meta_file, quote = FALSE, sep = "\t", row.names = FALSE)
LOG("  已保存元数据: ", meta_file)

## ---------------------------------------------------------------------
## 10. 出图（统一白底 bg="white"，与原报告一致）
## ---------------------------------------------------------------------

STEP("STEP 7 出图")

# 所有 ggsave 统一加 bg="white"，保证图片背景为白色
save_plot <- function(plot, filename, width = 7, height = 6) {
  f <- file.path(outdir, filename)
  ok <- tryCatch({
    ggsave(f, plot, width = width, height = height, dpi = 150, bg = "white")
    TRUE
  }, error = function(e) { LOG("  出图失败 ", filename, ": ", conditionMessage(e)); FALSE })
  if (ok) LOG("  已输出: ", f)
}

# UMAP：按 cluster / 分组 / 样本着色
save_plot(DimPlot(merged, reduction = "umap", group.by = "seurat_clusters", label = TRUE) +
            ggtitle("UMAP by cluster"), "umap_cluster.png")
save_plot(DimPlot(merged, reduction = "umap", group.by = "group", label = FALSE) +
            ggtitle("UMAP by group"), "umap_group.png")
save_plot(DimPlot(merged, reduction = "umap", group.by = "orig.ident", label = FALSE) +
            ggtitle("UMAP by sample"), "umap_sample.png")

# 质控小提琴图：基因数 / 线粒体比例（按样本分组）
save_plot(VlnPlot(merged, features = c("nFeature_RNA", "percent.mt"),
                  group.by = "orig.ident", pt.size = 0) +
            plot_annotation(title = "QC after filtering"),
          "qc_violin.png", width = 10, height = 5)

# 质控统计柱状图：raw / filtered / used 对比
qc_long <- reshape(qc_tab[, c("sample", "raw", "filtered", "used")],
                   direction = "long", varying = c("raw", "filtered", "used"),
                   v.names = "cells", timevar = "stage",
                   times = c("raw", "filtered", "used"), idvar = "sample")
qc_bar <- ggplot(qc_long, aes(x = sample, y = cells, fill = stage)) +
  geom_col(position = "dodge") +
  theme_bw() + labs(title = "Cell counts per sample", x = NULL, y = "cells")
save_plot(qc_bar, "qc_barplot.png", width = 8, height = 5)

## ---------------------------------------------------------------------
## 11. 收尾统计
## ---------------------------------------------------------------------

LOG("")
LOG("每样本细胞数:")
print(table(merged$orig.ident))
LOG("每分组细胞数:")
print(table(merged$group))
LOG("每簇细胞数:")
print(table(merged$seurat_clusters))

# 记录运行环境，便于复现排查
tryCatch({
  writeLines(capture.output(sessionInfo()),
             file.path(outdir, "session_info.txt"))
}, error = function(e) NULL)

LOG("")
LOG("===== 全部完成 =====")
LOG("输出目录: ", outdir)
LOG("主产物: processed.rds / meta.tsv / qc_stat.tsv / 若干 png")
