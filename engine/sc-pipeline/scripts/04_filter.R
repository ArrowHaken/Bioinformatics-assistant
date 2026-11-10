#!/usr/bin/env Rscript
# =====================================================================
# 04_filter.R — 筛群与剔样本后重聚类（参数从 drop_params.json 读）
#
# 为什么有这个文件（2026-11-03，审核第 9 条的收尾）
#   原来这一步用的是服务器上那份 drop_and_recluster.R，里面写着：
#     FindNeighbors(d, reduction = 'harmony', dims = 1:20)
#     FindClusters(d, resolution = 0.6)
#   而应用页面上有 dims / resolution 的参数面板，人选了、存进了
#   variants_<sid>.json，命令里却永远不出现 —— 脚本不读它。
#   于是"选了 0.9"和"实际跑 0.6"之间隔着一层看不见的假象，
#   而且这种错最难发现：结果看着正常，只是不是你要的那一档。
#
#   现在这一步改由应用下发，dims / resolution 从参数文件读，
#   读不到才退回默认值，并且在日志里写明"用了哪个、来自哪里"。
#
# 与其他下发脚本保持一致的地方
#   ① 入口守卫：必须能找到 workflow/guard_load.R，找不到就停。
#      （不是装饰：台账、参数来源、产物指纹都靠它，绕过它跑出来的东西事后查不清）
#   ② 缺参数就停，不替人猜：剔除清单为空且不是显式 keep_all 时，停下报错。
#   ③ 自检：簇统计细胞总数必须等于对象细胞数，不等就停，不让错表往后传。
#
# 用法
#   Rscript 04_filter.R <work_root> [--param <drop_params.json>] [--dims 1:20]
#                       [--resolution 0.6] [--keep-all]
# =====================================================================

suppressMessages({
  library(Seurat)
})

# ---- 脚本目录定位：source() 与 Rscript 直接调用两种方式都要支持 ----
.this_dir <- local({
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(f) && nzchar(f)) return(dirname(normalizePath(f)))
  ca <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", ca, value = TRUE)
  if (length(m) > 0) {
    p <- sub("^--file=", "", m[1])
    if (nzchar(p) && file.exists(p)) return(dirname(normalizePath(p)))
  }
  "."
})

# ---- 入口守卫 ----
#   与 02_annotate.R 同一套查找顺序。找不到就停，不静默跳过：
#   守卫缺失还继续跑，等于约束失效而没人知道，比直接报错危险得多。
.guard_load <- local({
  cands <- c(file.path(.this_dir, "..", "..", "workflow", "guard_load.R"),
             file.path(.this_dir, "..", "workflow", "guard_load.R"),
             file.path(.this_dir, "..", "..", "..", "workflow", "guard_load.R"))
  hit <- cands[file.exists(cands)]
  if (!length(hit)) {
    stop("\n  拒绝运行：找不到入口守卫 workflow/guard_load.R。\n",
         "  本脚本必须经由统一入口执行，守卫缺失时不能继续。\n",
         "  找过：\n    ", paste(cands, collapse = "\n    "), "\n", call. = FALSE)
  }
  hit[1]
})
source(.guard_load, local = FALSE)
if (exists("sc_guard_banner", mode = "function")) sc_guard_banner("filter", .this_dir)

LOG  <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))
STEP <- function(t) cat(sprintf("\n[%s] ===== %s =====\n", format(Sys.time(), "%H:%M:%S"), t))

# ---------------------------------------------------------------------
# 参数
# ---------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
get_opt <- function(k, d = NULL) {
  i <- match(k, args)
  if (is.na(i)) return(d)
  if (i == length(args) || grepl("^--", args[i + 1])) return(d)
  args[i + 1]
}
has_flag <- function(k) k %in% args

W <- if (length(args) >= 1 && !grepl("^--", args[1])) args[1] else get_opt("--work-root")
if (is.null(W) || is.na(W) || !nzchar(W)) stop("必须给第一个位置参数：<work_root>", call. = FALSE)

PARAM <- get_opt("--param", file.path(W, "7.after_drop", "drop_params.json"))
OUT   <- file.path(W, "7.after_drop")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------
# 1. 读参数文件：剔除清单 + dims + resolution
#
#   三样都从同一个文件来，理由一致：它们是"这一轮怎么跑"的决定，
#   必须能被复盘 —— 事后看这个文件就知道当时剔了哪些群、按什么分辨率聚的。
# ---------------------------------------------------------------------
STEP("STEP 1 读取参数文件")

pj <- NULL
if (file.exists(PARAM)) {
  pj <- tryCatch(jsonlite::fromJSON(PARAM, simplifyVector = FALSE),
                 error = function(e) {
                   stop("CHECK_FAIL 参数文件不是合法 JSON：", PARAM,
                        "\n  错误：", conditionMessage(e), call. = FALSE)
                 })
} else {
  LOG("参数文件不存在：", PARAM)
}

# ---- 剔除清单 ----
DROP <- character(0)
if (!is.null(pj) && length(pj$drop_clusters)) {
  DROP <- as.character(unlist(pj$drop_clusters))
  DROP <- DROP[nzchar(trimws(DROP))]
  LOG("剔除清单来自参数文件：", if (length(DROP)) paste(DROP, collapse = ",") else "（空 = 全部保留）")
} else if (has_flag("--keep-all")) {
  LOG("命令行给了 --keep-all：不剔除任何簇")
} else {
  # 没有清单、也没说保留谁，就停。不替人猜。
  stop("CHECK_FAIL 缺少剔除参数：", PARAM,
       "\n  这一版不使用写死的清单：照抄上一轮的簇号，换个对象跑删掉的就是别的细胞。\n",
       "  请在应用里定下要删哪些簇，重新下发；确实一个都不删，请显式给 --keep-all。",
       call. = FALSE)
}

# ---- dims ----
#   接受 "1:20" / "1-20" / "20"（1:20 的简写）/ c(1,2,...) 这几种写法里前三种。
#   写不清就停：猜出来的维度会悄悄改变聚类结果，事后没人能复现。
parse_dims <- function(x, fallback) {
  s <- trimws(as.character(x %||% ""))
  if (!nzchar(s)) return(list(v = fallback, src = "默认"))
  s2 <- gsub("[ \\[\\]()]", "", s)
  if (grepl("^[0-9]+$", s2)) {
    n <- as.integer(s2)
    return(list(v = seq_len(n), src = paste0("参数文件（", s, " → 1:", n, "）")))
  }
  m <- regmatches(s2, regexec("^([0-9]+)[:-]([0-9]+)$", s2))[[1]]
  if (length(m) == 3) {
    a <- as.integer(m[2]); b <- as.integer(m[3])
    if (a >= 1 && b >= a) return(list(v = a:b, src = paste0("参数文件（", s, "）")))
  }
  stop("CHECK_FAIL dims 写法看不懂：", s,
       "\n  可接受：1:20 或 1-20 或 20（表示 1:20）。\n",
       "  不猜：猜出来的维度会改变聚类结果，事后无法复现。", call. = FALSE)
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---- resolution ----
parse_res <- function(x, fallback) {
  s <- trimws(as.character(x %||% ""))
  if (!nzchar(s)) return(list(v = fallback, src = "默认"))
  v <- suppressWarnings(as.numeric(s))
  if (is.na(v) || v <= 0 || v > 10)
    stop("CHECK_FAIL resolution 不合法：", s, "（要求 0 < resolution <= 10）", call. = FALSE)
  list(v = v, src = paste0("参数文件（", s, "）"))
}

# 默认值沿用这一条链原本的口径（1:20 / 0.6）。
#   为什么不是别的数：换默认值等于悄悄改了历史结果的口径，
#   下一轮和上一轮就不可比了。要改口径，改参数文件，不改默认。
DEF_DIMS <- 1:20
DEF_RES  <- 0.6

dims <- parse_dims(if (is.null(pj)) NULL else pj$dims, DEF_DIMS)
res  <- parse_res(if (is.null(pj)) NULL else pj$resolution, DEF_RES)

LOG("dims       = ", paste(range(dims$v), collapse = ":"), "  ← ", dims$src)
LOG("resolution = ", res$v, "  ← ", res$src)
cat("PARAM_DIMS", paste(range(dims$v), collapse = ":"), "\n")
cat("PARAM_RES", res$v, "\n")

# ---------------------------------------------------------------------
# 2. 读对象、按清单剔除
# ---------------------------------------------------------------------
STEP("STEP 2 读入并剔除")

FIN <- file.path(W, "3.merge", "merge_7sample.rds")
if (!file.exists(FIN)) {
  stop("CHECK_FAIL 找不到输入对象 ", FIN,
       "\n  这一步的上游是「合并与批次校正」，先把它跑完再来。", call. = FALSE)
}
d <- readRDS(FIN)
if (!"seurat_clusters" %in% colnames(d@meta.data))
  stop("CHECK_FAIL 对象里没有 seurat_clusters 列，无法按簇剔除。", call. = FALSE)

cat("BEFORE_CELLS", ncol(d),
    " CLUSTERS", nlevels(factor(d@meta.data$seurat_clusters)), "\n")

keep <- !(as.character(d@meta.data$seurat_clusters) %in% DROP)
cat("DROP_CELLS", sum(!keep), "\n")

# 剔除清单里的簇号如果在对象里根本不存在，要说出来：
#   那通常意味着清单是上一轮抄来的，或者上游聚类变了。
present <- unique(as.character(d@meta.data$seurat_clusters))
ghost <- setdiff(DROP, present)
if (length(ghost))
  LOG("注意：剔除清单里有 ", length(ghost), " 个簇号在本对象里不存在（",
      paste(ghost, collapse = ","), "）—— 请确认清单不是照抄上一轮的。")

md0 <- d@meta.data
tb0 <- as.data.frame(table(factor(md0$seurat_clusters)))
colnames(tb0) <- c("cluster", "n")
tb0$dropped <- as.character(tb0$cluster) %in% DROP
write.table(tb0, file.path(OUT, "dropped_clusters.xls"),
            sep = "\t", quote = FALSE, row.names = FALSE)

d <- subset(d, cells = colnames(d)[keep])
cat("AFTER_DROP_CELLS", ncol(d), "\n")
gc()

# ---------------------------------------------------------------------
# 3. 用参数文件里的 dims / resolution 重聚类
# ---------------------------------------------------------------------
STEP("STEP 3 二次聚类（dims / resolution 按参数文件）")

d <- FindNeighbors(d, reduction = "harmony", dims = dims$v, verbose = FALSE)
cat("NN_DONE\n"); flush.console()

d <- FindClusters(d, resolution = res$v, verbose = FALSE)
cat("CLUSTERS_AFTER", nlevels(factor(d@meta.data$seurat_clusters)), "\n"); flush.console()

d <- RunUMAP(d, reduction = "harmony", dims = dims$v, verbose = FALSE)
cat("UMAP_DONE\n"); flush.console()

d@assays$RNA@scale.data <- matrix()
saveRDS(d, file.path(OUT, "merged_after_drop.rds"))
cat("SAVED", file.size(file.path(OUT, "merged_after_drop.rds")), "\n")

# ---------------------------------------------------------------------
# 4. 簇统计（含自检）
# ---------------------------------------------------------------------
STEP("STEP 4 簇统计与自检")

md <- d@meta.data
# 簇号是文字标签（0,1,...,23）。转整数必须先过 as.character：
#   直接 as.integer(factor) 拿到的是"第几个"位置序号，0 号簇会整簇变空值。
cl <- factor(as.character(md$seurat_clusters),
             levels = as.character(sort(unique(as.integer(as.character(md$seurat_clusters))))))
tb <- as.data.frame(table(cl))
colnames(tb) <- c("cluster", "n")
tb$pct <- round(100 * tb$n / sum(tb$n), 2)

st <- as.data.frame.matrix(table(cl, md$orig.ident))
colnames(st) <- paste0("n_", colnames(st))
st$cluster <- rownames(st)
gt <- as.data.frame.matrix(table(cl, md$group))
colnames(gt) <- paste0("n_", colnames(gt))
gt$cluster <- rownames(gt)

stats <- merge(tb, st, by = "cluster")
stats <- merge(stats, gt, by = "cluster")

# 自检：统计表细胞总数必须等于对象细胞数。
#   0 号簇被漏掉时，表看着正常，总数会少一截 —— 让错表往后传，下游全错。
if (sum(as.numeric(tb$n)) != ncol(d)) {
  stop("CHECK_FAIL 簇统计总数 ", sum(as.numeric(tb$n)),
       " != 对象细胞数 ", ncol(d), call. = FALSE)
}
cat("CHECK_OK cells", ncol(d), "clusters", nrow(tb), "\n")

stats <- stats[order(as.integer(stats$cluster)), ]
write.table(stats, file.path(OUT, "cluster_stats_after_drop.xls"),
            sep = "\t", quote = FALSE, row.names = FALSE)

# 把这一轮实际用的口径写进产物旁边：事后复盘时不用再猜当时是 0.6 还是 0.9。
write.table(data.frame(
  item = c("dims", "resolution", "dims_source", "resolution_source",
           "drop_clusters", "before_cells", "after_cells", "clusters_after"),
  value = c(paste(range(dims$v), collapse = ":"), as.character(res$v),
            dims$src, res$src,
            if (length(DROP)) paste(DROP, collapse = ",") else "（全部保留）",
            as.character(sum(tb0$n)), as.character(ncol(d)), as.character(nrow(tb)))
), file.path(OUT, "recluster_params_used.xls"),
  sep = "\t", quote = FALSE, row.names = FALSE)

cat("--- per sample ---\n"); print(table(md$orig.ident))
cat("--- per group ---\n");  print(table(md$group))
cat("FILTER_DONE\n")
