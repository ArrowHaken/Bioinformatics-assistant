#!/usr/bin/env Rscript
# =====================================================================
#  evidence_functions.R -- 把「人做决定需要的依据」算出来
#
#  为什么要有这个文件
#    一次聚类跑完，脚本知道 20 个簇各有几个细胞；但「要不要剔掉这个簇」
#    「要不要剔掉这个样本」是人来定的决定。人做这个决定需要材料：
#       · 每个簇多少细胞、占多少
#       · 这个簇里各个样本各有多少细胞（判断是不是某个样本特有的）
#       · 这个簇靠什么基因聚到一起（判断它是什么细胞）
#    以前只写了簇大小，人看不到后两项，于是「判断」就变成了猜。
#    这个文件负责把这三件事都算出来，交给闸门去展示。
#
#  设计约束
#    · 只读，不改任何对象，不做任何过滤决定
#    · marker 计算失败不能让主流程崩：降级为「只给细胞数和样本分布」并说明
#    · 输出是给人看的原始数字，不做「建议剔除」这类判断
#      —— 是否剔除由人定，脚本不给倾向性结论
# =====================================================================

#' 一次聚类后生成人工判断依据
#'
#' @param obj        Seurat 对象（已做过一次聚类，含 seurat_clusters 与 orig.ident）
#' @param outdir     输出根目录（会写到 <outdir>/status/ 下）
#' @param top_n      每簇展示多少个 top marker
#' @param want_markers 是否计算 marker（大对象上较慢，可关掉）
#' @return           实际上就是写出的那份 list（方便调用方复用）
evidence_cluster1 <- function(obj, outdir, top_n = 10, want_markers = TRUE) {
  sdir <- file.path(outdir, "status")
  dir.create(sdir, showWarnings = FALSE, recursive = TRUE)

  md <- obj@meta.data
  if (!"seurat_clusters" %in% colnames(md)) {
    stop("evidence_cluster1: 对象里没有 seurat_clusters 列，无法生成依据。", call. = FALSE)
  }
  clu <- as.character(md$seurat_clusters)
  smp <- if ("orig.ident" %in% colnames(md)) as.character(md$orig.ident) else rep("all", length(clu))
  n_total <- length(clu)

  clu_levels <- sort(unique(as.integer(clu)))
  sample_levels <- sort(unique(smp))

  # ---- 每簇：细胞数、占比、各样本分布 ----
  clusters <- vector("list", length(clu_levels))
  for (i in seq_along(clu_levels)) {
    k <- clu_levels[i]
    idx <- which(as.integer(clu) == k)
    by_s <- table(factor(smp[idx], levels = sample_levels))
    clusters[[i]] <- list(
      cluster = k,
      n_cells = length(idx),
      pct = round(100 * length(idx) / n_total, 2),
      # 必须保留样本名，不能只留数字。
      #   踩过坑：写成 as.list(as.integer(by_s)) 会把名字丢掉，
      #   于是界面上「样本分布」那一列永远是空的 ——
      #   而这一列正是判断「这个簇是不是某个样本特有」的唯一依据。
      #   setNames 之后再转 list，序列化成 JSON 就是 {"S1":200,...}。
      by_sample = as.list(setNames(as.integer(by_s), sample_levels)),
      top_markers = list(),
      marker_note = ""
    )
  }
  names(clusters) <- as.character(clu_levels)

  # ---- 每样本：细胞数、占比 ----
  samples <- list()
  for (s in sample_levels) {
    n <- sum(smp == s)
    samples[[length(samples) + 1L]] <- list(
      sample = s, n_cells = n, pct = round(100 * n / n_total, 2)
    )
  }

  # ---- 每簇 top marker ----
  #   算不出来不影响主流程：给不出「这簇是什么」，但细胞数和样本分布仍然有效。
  markers_ok <- FALSE
  markers_note <- ""
  if (isTRUE(want_markers)) {
    res <- tryCatch({
      old_id <- SeuratObject::Idents(obj)
      on.exit(try(SeuratObject::Idents(obj) <- old_id, silent = TRUE), add = TRUE)
      SeuratObject::Idents(obj) <- clu
      mk <- Seurat::FindAllMarkers(obj, only.pos = TRUE, min.pct = 0.25,
                                   logfc.threshold = 0.25, verbose = FALSE)
      if (is.null(mk) || nrow(mk) == 0) stop("FindAllMarkers 返回空")
      # 过滤线粒体与核糖体，避免这些基因占满列表
      mk <- mk[!grepl("^mt-|^MT-|^Mt-|^Rpl|^Rps", mk$gene), , drop = FALSE]
      if (nrow(mk) == 0) stop("过滤后无 marker")
      mk
    }, error = function(e) e)

    if (inherits(res, "error")) {
      markers_note <- paste0("marker 计算未成功：", conditionMessage(res),
                             "。本表只给细胞数与样本分布，不足以判断细胞类型。")
    } else {
      mk <- res
      markers_ok <- TRUE
      # 按簇取 top_n
      mk <- mk[order(mk$cluster, -mk$avg_log2FC), , drop = FALSE]
      for (i in seq_along(clusters)) {
        k <- clusters[[i]]$cluster
        sub <- mk[mk$cluster == k, , drop = FALSE]
        if (nrow(sub) == 0) next
        n <- min(top_n, nrow(sub))
        clusters[[i]]$top_markers <- as.list(as.character(sub$gene[seq_len(n)]))
      }
      n_missing <- sum(vapply(clusters, function(x) length(x$top_markers) == 0, logical(1)))
      if (n_missing > 0) {
        markers_note <- sprintf("有 %d 个簇没算出 marker（可能是细胞太少或与其它簇差异不足）。", n_missing)
      }
    }
  } else {
    markers_note <- "本次未计算 marker（已关闭）。"
  }

  out <- list(
    stage = "cluster1",
    time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    total_cells = n_total,
    n_clusters = length(clu_levels),
    n_samples = length(sample_levels),
    samples = samples,
    clusters = unname(clusters),
    markers_ok = markers_ok,
    markers_note = markers_note
  )

  tmp <- file.path(sdir, "cluster1_evidence.json.tmp")
  fin <- file.path(sdir, "cluster1_evidence.json")
  con <- file(tmp, open = "wb")
  writeBin(charToRaw(enc2utf8(jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE,
                                              null = "null", digits = NA))), con)
  close(con)
  file.rename(tmp, fin)
  out
}
