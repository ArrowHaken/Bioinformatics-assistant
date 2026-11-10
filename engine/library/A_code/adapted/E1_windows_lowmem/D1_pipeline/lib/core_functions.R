# =====================================================================
# core_functions.R — 官方流程核心函数复刻
#
# 来源：对方交付的 script/01_packages.R（运行环境 R 4.1.2 + Seurat v4）
# 复刻原则：
#   1. 逻辑、参数、取值范围一律照抄官方，不做任何"优化"或降级
#   2. 官方未指定的量必须由调用方显式传入，不许用隐藏默认值顶替
#   3. 出现任何异常一律 stop()，不许 tryCatch 后继续
# =====================================================================

# ---------------------------------------------------------------------
# 版本闸门
# 官方脚本使用了 Seurat v4 的槽访问写法（@assays$RNA@scale.data 等）。
# 在 v5 下这些写法语义已变，结果不可比。因此默认要求 v4；
# 确需在 v5 上跑，必须显式传 allow_v5 = TRUE，并在运行报告中记录。
# ---------------------------------------------------------------------
check_seurat_version <- function(allow_v5 = FALSE) {
  v <- as.character(utils::packageVersion("Seurat"))
  major <- as.integer(strsplit(v, "\\.")[[1]][1])
  if (major >= 5 && !isTRUE(allow_v5)) {
    stop(
      "检测到 Seurat ", v, "，而官方流程基于 Seurat v4。\n",
      "  在 v5 下槽访问语义已变（@assays$RNA@scale.data / @data），结果无法与官方对齐。\n",
      "  做法二选一：\n",
      "    1) 安装与官方一致的 Seurat v4（推荐，用于交付对齐）\n",
      "    2) 显式传 allow_v5 = TRUE 接受兼容差异（须在报告中标注）\n",
      call. = FALSE
    )
  }
  invisible(v)
}

# ---------------------------------------------------------------------
# RunDF — 去双细胞（官方 01_packages.R:204-230 逐行复刻）
#
# 官方原样：
#   NormalizeData -> ScaleData -> FindVariableFeatures(vst, nfeatures) -> RunPCA
#   -> FindNeighbors(dims=1:dims) -> FindClusters(resolution) -> RunUMAP(dims=1:dims)
#   -> 清空 scale.data
#   -> paramSweep_v3(PCs=1:PCs, sct=T)
#   -> summarizeSweep(GT=FALSE) -> find.pK -> pK = BCmetric 最大处
#   -> DoubletRate = ncol * 8e-6 ; nExp = round(DoubletRate * ncol)
#   -> nExp = round(nExp * (1 - homotypic.prop))
#   -> doubletFinder_v3(PCs=1:PCs, pN=pN, pK=pK, nExp=nExp, sct=T)
#
# 注意：官方未传 pK，pK 由 paramSweep 自动选出。此处照抄，不许写死。
# ---------------------------------------------------------------------
RunDF <- function(before_rds, nfeatures = 2000, dims = 20, resolution = 0.5,
                  PCs = 20, pN = 0.25, allow_v5 = FALSE) {

  for (p in c("Seurat", "DoubletFinder")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("缺少必需包: ", p, "。请先安装后再跑，不提供替代方案。", call. = FALSE)
    }
  }
  check_seurat_version(allow_v5)

  obj <- before_rds
  obj <- Seurat::NormalizeData(obj, verbose = FALSE)
  obj <- Seurat::ScaleData(obj, verbose = FALSE)
  obj <- Seurat::FindVariableFeatures(obj, selection.method = "vst",
                                      nfeatures = nfeatures, verbose = FALSE)
  obj <- Seurat::RunPCA(obj, verbose = FALSE)
  obj <- Seurat::FindNeighbors(obj, dims = 1:dims, verbose = FALSE)
  obj <- Seurat::FindClusters(obj, resolution = resolution, verbose = FALSE)
  obj <- Seurat::RunUMAP(obj, dims = 1:dims, verbose = FALSE)

  # 官方在此清空 scale.data（v4 写法）。v5 下改为删除 scale.data layer。
  is_v5 <- as.integer(strsplit(as.character(utils::packageVersion("Seurat")), "\\.")[[1]][1]) >= 5
  if (!is_v5) {
    obj@assays$RNA@scale.data <- matrix()
  } else {
    obj[["RNA"]]@layers[["scale.data"]] <- NULL
  }

  # ---- paramSweep 自动选 pK（官方 sct = TRUE，照抄）----
  sweep_res  <- DoubletFinder::paramSweep(obj, PCs = 1:PCs, sct = TRUE)
  sweep_stat <- DoubletFinder::summarizeSweep(sweep_res, GT = FALSE)
  bcmvn      <- DoubletFinder::find.pK(sweep_stat)
  mpK        <- as.numeric(as.vector(bcmvn$pK[which.max(bcmvn$BCmetric)]))
  if (is.na(mpK)) stop("paramSweep 未能选出 pK（BCmetric 全为 NA），停止，不设默认值。", call. = FALSE)

  # ---- nExp（官方公式，照抄）----
  annotations    <- obj@meta.data$seurat_clusters
  homotypic_prop <- DoubletFinder::modelHomotypic(annotations)
  n_cells        <- ncol(obj)
  DoubletRate    <- n_cells * 8 * 1e-6
  nExp_poi       <- round(DoubletRate * n_cells)
  nExp_poi       <- round(nExp_poi * (1 - homotypic_prop))

  obj <- DoubletFinder::doubletFinder(obj, PCs = 1:PCs, pN = pN, pK = mpK,
                                      nExp = nExp_poi, reuse.pANN = FALSE, sct = TRUE)
  # 官方把最后一列（DF.classifications_*）命名为 Doublet
  obj@meta.data$Doublet <- obj@meta.data[, ncol(obj@meta.data)]

  attr(obj, "df_params") <- list(nfeatures = nfeatures, dims = dims, resolution = resolution,
                                 PCs = PCs, pN = pN, pK = mpK, nExp = nExp_poi,
                                 DoubletRate = DoubletRate, homotypic_prop = homotypic_prop,
                                 n_cells = n_cells)
  obj
}

# ---------------------------------------------------------------------
# RunFindAllMarkers — marker 基因（官方 01_packages.R:433-450 复刻）
#   ScaleData(features = 全部基因) -> FindAllMarkers(only.pos=TRUE, min.pct, logfc.threshold)
#   -> 按物种过滤线粒体基因 -> sig = p_val_adj < 0.05
#   -> 每群 top_n(top_gene, wt=avg_log2FC) -> DoHeatmap(蓝-米黄-红)
# 官方 min.pct = 0.1（不是 0.2）
# ---------------------------------------------------------------------
RunFindAllMarkers <- function(seu, min.pct = 0.1, species = NULL,
                              logfc.threshold = 0.25, top_gene = 10) {
  if (is.null(species)) stop("species 必须显式指定（Human/Mouse/Rat），官方按物种过滤线粒体基因。", call. = FALSE)
  Seurat::DefaultAssay(seu) <- "RNA"
  all_genes <- rownames(seu)
  seu <- Seurat::ScaleData(seu, features = all_genes, verbose = FALSE)
  allmarkers <- Seurat::FindAllMarkers(seu, only.pos = TRUE, min.pct = min.pct,
                                       logfc.threshold = logfc.threshold, verbose = FALSE)
  if (nrow(allmarkers) == 0) {
    stop("FindAllMarkers 返回 0 行，停止，不用近似结果顶替。", call. = FALSE)
  }
  pat <- switch(species,
                Human = "^MT-", Mouse = "^mt-", Rat = "^Mt-",
                stop("species 只支持 Human / Mouse / Rat，收到: ", species, call. = FALSE))
  allmarkers <- allmarkers[!grepl(allmarkers$gene, pattern = pat), , drop = FALSE]

  sig_marker <- subset(allmarkers, p_val_adj < 0.05)
  top <- sig_marker %>% dplyr::group_by(cluster) %>%
    dplyr::top_n(n = top_gene, wt = avg_log2FC)
  p1 <- Seurat::DoHeatmap(seu, features = as.character(top$gene)) +
    ggplot2::scale_fill_gradient2(low = "#0000FF", mid = "#FFF8DC", high = "#FF0000")
  list(sig_marker = sig_marker, heatmap = p1, all_markers = allmarkers)
}

# ---------------------------------------------------------------------
# 逐样本去双 + 取 Singlet 重建（官方 soupX_doublefinder.R:35-81 复刻）
# 官方在该脚本里 --soupx N（不跑 SoupX），只跑 DoubletFinder。
# 本函数同样只做 DoubletFinder，保持与官方一致。
# ---------------------------------------------------------------------
process_one_sample <- function(matrix_dir, sample_name, species = "Mouse",
                               nfeatures = 2000, dims = 20, resolution = 0.5,
                               PCs = 20, pN = 0.25, allow_v5 = FALSE) {
  if (!dir.exists(matrix_dir)) stop("样本目录不存在: ", matrix_dir, call. = FALSE)
  if (!requireNamespace("Seurat", quietly = TRUE)) stop("缺少必需包: Seurat", call. = FALSE)

  d <- Seurat::Read10X(data.dir = matrix_dir)
  if (is.list(d) && !is.matrix(d) && !inherits(d, "dgCMatrix")) {
    nm <- names(d)
    pick <- if ("Gene Expression" %in% nm) "Gene Expression" else nm[1]
    message("  检测到多模态矩阵，取: ", pick)
    d <- d[[pick]]
  }
  obj <- Seurat::CreateSeuratObject(counts = d, project = sample_name,
                                    min.cells = 3, min.features = 0)
  rm(d); gc(verbose = FALSE)

  mt_pat <- switch(species, Human = "^MT-", Mouse = "^mt-", Rat = "^Mt-",
                   stop("species 只支持 Human / Mouse / Rat", call. = FALSE))
  obj[["percent.mt"]] <- Seurat::PercentageFeatureSet(obj, pattern = mt_pat)
  raw_n <- ncol(obj)

  obj <- RunDF(obj, nfeatures = nfeatures, dims = dims, resolution = resolution,
               PCs = PCs, pN = pN, allow_v5 = allow_v5)
  dfp <- attr(obj, "df_params")

  # 取 Singlet 并用 counts 重建（官方写法）
  n_singlet <- sum(obj@meta.data$Doublet == "Singlet")
  if (n_singlet == 0) stop("样本 ", sample_name, " 无 Singlet，停止。", call. = FALSE)
  keep <- colnames(obj)[obj@meta.data$Doublet == "Singlet"]
  counts <- if (as.integer(strsplit(as.character(utils::packageVersion("Seurat")), "\\.")[[1]][1]) >= 5) {
    Seurat::LayerData(obj, assay = "RNA", layer = "counts")[, keep, drop = FALSE]
  } else {
    obj@assays$RNA@counts[, keep, drop = FALSE]
  }
  out <- Seurat::CreateSeuratObject(counts = counts, project = sample_name,
                                    min.cells = 3, min.features = 200)
  out[["percent.mt"]] <- Seurat::PercentageFeatureSet(out, pattern = mt_pat)
  out$orig.ident <- sample_name
  out$sample <- sample_name

  attr(out, "sample_stats") <- list(sample = sample_name, raw = raw_n,
                                    singlet = n_singlet, after_rebuild = ncol(out),
                                    pK = dfp$pK, nExp = dfp$nExp,
                                    homotypic_prop = dfp$homotypic_prop)
  out
}

# ---------------------------------------------------------------------
# 逐样本质控（官方 analysis.R:5 复刻）
#   nFeature_RNA > 300 & nFeature_RNA < 5000 & percent.mt < 10
# ---------------------------------------------------------------------
qc_filter_sample <- function(obj, min_features = 300, max_features = 5000, mt_cutoff = 10) {
  n0 <- ncol(obj)
  obj <- subset(obj, subset = nFeature_RNA > min_features &
                          nFeature_RNA < max_features &
                          percent.mt < mt_cutoff)
  n1 <- ncol(obj)
  if (n1 == 0) {
    stop("质控后细胞数为 0。阈值: nFeature ", min_features, "-", max_features,
         ", percent.mt < ", mt_cutoff, "。请调整阈值，不要跳过。", call. = FALSE)
  }
  attr(obj, "qc_stats") <- list(before = n0, after = n1, dropped = n0 - n1,
                                min_features = min_features, max_features = max_features,
                                mt_cutoff = mt_cutoff)
  obj
}
