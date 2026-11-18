#!/usr/bin/env Rscript
# =====================================================================
# 02_annotate.R — 文献驱动细胞类型注释
#
# 逻辑（照官方 markgene_celltype.R 的思路，加上文献打分）：
#   每个聚类取 top-N 上调 marker → 与文献词典求交集打分 → 得分最高者判为该群类型
#   → 写回对象 annot 列 → 输出注释结果表与细胞类型计数
#
# 铁律：
#   1. 不做降级。词典缺失、打分全部为 0 → 停下报告，不许硬套近似类型。
#   2. 词典的两个来源（reference 表 / lit-dict.json）汇成同一结构后使用：
#      {细胞类型: {markers:[基因…], refs:[…], full:"全称"}}
# =====================================================================

suppressMessages({
  library(Seurat); library(dplyr); library(jsonlite)
})

# 脚本目录定位：要同时支持 source() 和 Rscript 直接调用两种方式。
#   只用 sys.frame(1)$ofile 时，Rscript 直接跑会拿到 NULL，
#   表现为「cannot open file './lib/core_functions.R'」——很难排查。
.this_dir <- local({
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(f) && nzchar(f)) return(dirname(normalizePath(f)))
  ca <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", ca, value = TRUE)
  if (length(m) > 0) {
    p <- sub("^--file=", "", m[1])
    if (nzchar(p) && file.exists(p)) return(dirname(normalizePath(p)))
  }
  if (dir.exists("lib") && file.exists(file.path("lib", "core_functions.R"))) return(normalizePath("."))
  "."
})

# ---- 入口守卫：拒绝被直接调用 ----
#   这不是装饰。它让「必须走统一入口」这件事由代码保证，而不是靠文档里的约定。
#   守卫位置：脚本往上找 workflow/guard_load.R。找不到就停 —— 不静默跳过。
#   为什么必须停：守卫缺失时继续跑，等于约束失效而没人知道，比直接报错危险得多。
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
sc_guard_banner("annotate", .this_dir)

source(file.path(.this_dir, "lib", "core_functions.R"))

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

rds_path  <- args[1]
outdir    <- get_opt("--outdir")
reference <- get_opt("--reference")     # xlsx / tsv，与 --lit-dict 二选一
lit_dict  <- get_opt("--lit-dict")      # json
# ---- 2026-11-04 改：候选池从 30 提到 100，并引入加权打分 ----
#   为什么改（上一轮实测：24 个簇只判出 7 类，82.6% 细胞未定型）
#   旧逻辑是「每簇只看 top30 上调基因，与词典求交集，命中 >=1 就判」。
#   两个问题叠在一起：
#     ① 候选池太小：脾脏里的红系、基质、浆细胞这类群，
#        最显著的前 30 个基因往往落在核糖体、线粒体、血红蛋白这些通用基因上，
#        真正的类型 marker 排在第 50 名之后 —— 交集自然是 0。
#     ② 命中 1 个就判：只要有一个偶合的基因，也可能给出一个错的类型，
#        而"错类型"比"未定型"危险得多（它会被当成结论往下游传）。
#   现在：
#     · 候选池放大到 100，但按排名加权 —— 排在前面的基因权重高，
#       这样既扩大了召回，又没有把噪声当信号。
#     · 要求「命中数 >= min_score」且「得分显著高于第二名」才判，
#       否则宁可留空并在报告里写明原因。
#   注意：这一版提高的是「判定方法的严谨度」，不是「凑出更多类型」。
#     词典本身不够厚，仍然会有一部分群判不出来 —— 那是词典的问题，
#     如实报出来比硬凑一个名字更负责。
top_n     <- as.integer(get_opt("--top-n", "100"))
min_score <- as.integer(get_opt("--min-score", "2"))
min_pct   <- as.numeric(get_opt("--min-pct", "0.1"))
logfc     <- as.numeric(get_opt("--logfc", "0.25"))
# 置信比：第一名得分至少是第二名这么多倍才采纳，否则记"分不开"。
#   1.0 = 不要求（退回旧行为）；默认 1.2 表示第一名要明显领先。
min_ratio <- as.numeric(get_opt("--min-ratio", "1.2"))
# 未定型细胞占比超过这个值，就把质量判为 low 并写清原因。
bad_na_pct <- as.numeric(get_opt("--max-untyped-pct", "50"))

if (is.null(rds_path) || is.na(rds_path)) stop("必须给第一个位置参数：<processed.rds>", call. = FALSE)
if (is.null(outdir) || is.na(outdir))     stop("必须给 --outdir", call. = FALSE)
if (!file.exists(rds_path))               stop("rds 不存在: ", rds_path, call. = FALSE)
if (is.null(reference) && is.null(lit_dict)) {
  stop("必须提供 --reference 或 --lit-dict 之一。\n",
       "  没有文献依据就无法判定细胞类型；本脚本不会凭空猜测。", call. = FALSE)
}
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------
# 1. 加载词典 → 统一结构
# ---------------------------------------------------------------------
STEP("STEP 1 加载文献词典")
norm_dict <- function(lst) {
  # 统一成 {类型: list(markers=..., refs=..., full=...)}
  out <- list()
  for (nm in names(lst)) {
    e <- lst[[nm]]
    if (is.null(e)) next
    mk <- if (!is.null(e$markers)) e$markers else if (!is.null(e$Positive.Markers)) e$Positive.Markers else character(0)
    mk <- toupper(trimws(as.character(mk))); mk <- mk[nzchar(mk)]
    if (length(mk) == 0) next
    out[[nm]] <- list(markers = unique(mk),
                      refs    = as.character(if (!is.null(e$refs)) e$refs else ""),
                      full    = as.character(if (!is.null(e$full)) e$full else nm))
  }
  out
}

dict <- list()
if (!is.null(lit_dict)) {
  if (!file.exists(lit_dict)) stop("词典文件不存在: ", lit_dict, call. = FALSE)
  dict <- norm_dict(jsonlite::fromJSON(lit_dict, simplifyVector = FALSE))
  LOG("从 JSON 载入词典: ", lit_dict)
} else {
  if (!file.exists(reference)) stop("reference 表不存在: ", reference, call. = FALSE)
  ext <- tolower(tools::file_ext(reference))
  if (ext %in% c("xlsx", "xls")) {
    # ---- 2026-11-13 改：Excel 解析统一走 lib/ref_table.R ----
    #   为什么挪出去：这里的旧逻辑是按"行"认表的 —— 先是固定表头行号，后来改成
    #   "表头上面一行就是简称行"。后者遇到这张表直接把列名（Positive Markers、
    #   Literature Marker References 之类）当成细胞类型收进了词典。
    #   这类错不会报错，只会让注释结果悄悄变错，事后极难发现。
    #   现在解析只有一份实现（lib/ref_table.R::ref_table_parse），生产脚本与
    #   测试脚本跑同一份代码，口径写在函数里：
    #     按列名定位列 → 类型 key 优先 Subclass Level 3、为空才回退全称 →
    #     只取 Positive Markers → 同名不同层级不合并 → 参考文献区不进词典。
    #   打分、阈值、输出列一概不动；这里只把解析结果换成统一结构。
    source(file.path(.this_dir, "lib", "ref_table.R"), local = FALSE)
    ref_tbl <- ref_table_parse(reference)
    LOG("表头行: ", ref_tbl$meta$header_row,
        " | 数据区行数: ", ref_tbl$meta$n_data_rows,
        " | 参考文献区起: ", ref_tbl$meta$ref_block_row,
        " | 类型 key: ", ref_tbl$meta$n_keys)
    if (length(ref_tbl$meta$conflicts) > 0) {
      LOG("注意：", length(ref_tbl$meta$conflicts), " 个 key 的全称不一致，未合并（详见 meta$conflicts）")
    }
    if (ref_tbl$meta$n_unmarked > 0) {
      LOG("注意：", ref_tbl$meta$n_unmarked, " 个类型 Positive Markers 为空，保留但不参与打分：",
          paste(head(ref_tbl$meta$unmarked_keys, 8), collapse = ", "))
    }
    lst <- list()
    for (e in ref_tbl$entries) {
      lst[[e$key]] <- list(
        markers = if (is.na(e$positive[1])) character(0) else as.character(e$positive),
        refs    = if (length(e$refs)) paste(e$refs, collapse = ", ") else "",
        full    = if (is.na(e$full_name)) e$key else e$full_name
      )
    }
    dict <- norm_dict(lst)
    # 参考表解析元数据落成真实产物，不只留在日志里：
    #   报告要能回答「这份词典来自哪张表、哪一行、按哪些列读出来的」，
    #   只在内存与日志里就没法事后核对。
    tryCatch({
      meta_out <- ref_tbl$meta
      meta_out$entries <- lapply(ref_tbl$entries, function(e) list(
        key = e$key, full_name = e$full_name, level3 = e$level3,
        level1 = e$level1, source_rows = as.integer(e$source_rows),
        # 阳性 marker 原文一并写出：判定层要按类型逐条核对命中是否属于本类型，
        #   只有条数等于没给证据。
        positive = as.character(e$positive),
        n_positive = length(e$positive), refs = as.character(e$refs)))
      meta_out$reference_path <- reference
      writeLines(jsonlite::toJSON(meta_out, auto_unbox = TRUE, pretty = TRUE,
                                  null = "null"),
                 file.path(outdir, "reference_parse.json"))
      LOG("参考表解析元数据已写出: ", file.path(outdir, "reference_parse.json"))
    }, error = function(e) LOG("参考表元数据写出失败（不影响主流程）: ", conditionMessage(e)))
  } else {
    tb <- read.delim(reference, stringsAsFactors = FALSE, check.names = FALSE)
    if (!all(c("celltype", "markers") %in% tolower(colnames(tb)))) {
      stop("reference 表需要至少两列：celltype 与 markers（逗号分隔）。\n",
           "  现有列: ", paste(colnames(tb), collapse = ", "), call. = FALSE)
    }
    cn <- tolower(colnames(tb)); ci <- which(cn == "celltype")[1]; mi <- which(cn == "markers")[1]
    lst <- list()
    for (i in seq_len(nrow(tb))) {
      key <- as.character(tb[i, ci])
      if (is.na(key) || !nzchar(key)) next
      mk <- unlist(strsplit(as.character(tb[i, mi]), "[,;，；\\s]+"))
      lst[[key]] <- list(markers = unique(mk), refs = "", full = key)
    }
    dict <- norm_dict(lst)
  }
  LOG("从表格载入词典: ", reference)
}
if (length(dict) == 0) stop("词典为空，停止。请检查参考表或词典文件。", call. = FALSE)
LOG("词典条目数: ", length(dict))

# ---------------------------------------------------------------------
# 2. 每个聚类的 marker
# ---------------------------------------------------------------------
STEP("STEP 2 计算每个聚类的 marker")
obj <- readRDS(rds_path)
if (!"seurat_clusters" %in% colnames(obj@meta.data)) {
  stop("对象里没有 seurat_clusters 列，无法按聚类注释。", call. = FALSE)
}
LOG("细胞数: ", ncol(obj), " | 聚类数: ", length(unique(obj$seurat_clusters)))
Idents(obj) <- "seurat_clusters"

mk <- Seurat::FindAllMarkers(obj, only.pos = TRUE, min.pct = min_pct,
                             logfc.threshold = logfc, verbose = FALSE)
if (is.null(mk) || nrow(mk) == 0) {
  stop("FindAllMarkers 返回空，停止。请检查 min.pct / logfc 阈值或上游聚类结果。", call. = FALSE)
}
mk <- mk[!grepl(mk$gene, pattern = "^mt-|^MT-|^Mt-|^Rpl|^Rps"), , drop = FALSE]
write.table(mk, file.path(outdir, "all_markgene.xls"), quote = FALSE, sep = "\t", row.names = FALSE)
sig <- subset(mk, p_val_adj < 0.05)
write.table(sig, file.path(outdir, "sig_markgene.xls"), quote = FALSE, sep = "\t", row.names = FALSE)
LOG("marker 总数: ", nrow(mk), " | 显著: ", nrow(sig))
if (nrow(sig) == 0) stop("没有显著 marker（p_val_adj < 0.05），无法注释，停止。", call. = FALSE)

top <- sig %>% group_by(cluster) %>% slice_max(avg_log2FC, n = top_n)
dict_genes <- unique(unlist(lapply(dict, function(e) e$markers)))

# ---------------------------------------------------------------------
# 3. 打分判定
# ---------------------------------------------------------------------
STEP("STEP 3 与词典比对打分")

# ---- 2026-11-04 改：加权打分 + 置信比 + 写明不判的理由 ----
#   改之前（上一轮实测）：24 个簇只判出 7 类，82.6% 的细胞没有类型。
#   旧逻辑是「每簇只看 top30 上调基因，与词典求交集，命中 >= 1 就判」。
#   两个问题叠在一起：
#     ① 候选池太小。脾脏里的红系、基质、浆细胞这类群，最显著的前 30 个基因
#        往往落在核糖体 / 线粒体 / 血红蛋白这些通用基因上，真正的类型 marker
#        排在第 50 名之后 —— 交集当然是 0。
#     ② 命中 1 个就判。只要有一个偶合基因也可能给出错的类型，
#        而「错类型」比「未定型」危险得多：它会被当成结论写进报告和 PPT。
#
#   现在：
#     · 候选池放到 100，但按排名加权（第 1 名 1.0 线性衰减到第 100 名 0.5）。
#       既扩大召回，又不把噪声当信号。
#     · 命中数 >= min_score 且第一名得分 / 第二名 >= min_ratio 才判，
#       否则留空，并在 annot_result.xls 的 reason 列写清为什么不判。
#     · 留空不是失败。表里一眼能看出哪些群没定，比硬凑一个名字负责。
#   注意：这一版提的是判定方法的严谨度，不是"凑出更多类型"。
#     词典本身不够厚，仍会有一部分群判不出来 —— 那是词典的问题。
rank_w <- function(n) {
  if (n <= 0) return(numeric(0))
  1 - (seq_len(n) - 1) / max(n, 1) * 0.5
}

score_one <- function(genes, entry) {
  idx <- which(genes %in% entry$markers)
  if (!length(idx)) return(list(score = 0, n = 0L, hit = character(0)))
  w <- rank_w(length(genes))[idx]
  list(score = sum(w), n = length(idx), hit = genes[idx])
}

res <- data.frame()
unmatched <- character(0)
low_conf  <- character(0)

# 遍历真实对象的簇全集，而不是只遍历「差异表里出现过的簇」。
#   差异表只收有显著 marker 的簇；某簇在对象里存在、但没达到显著阈值时，
#   它根本不出现在 top 里。按 top 遍历会让这种簇直接从结果中消失 ——
#   下游按簇号对齐时看到的是「少了几个簇」，而「少了」和「判不出类型」
#   是两件完全不同的事，混在一起就没法追。
all_cl <- sort(unique(as.character(obj$seurat_clusters)))
for (cl in all_cl) {
  g <- toupper(as.character(top$gene[top$cluster == cl]))
  g <- g[!is.na(g) & nzchar(g)]
  if (length(g) == 0) {
    # 簇存在但没有可用 marker：写一行 NA 并说明原因，不跳过。
    unmatched <- c(unmatched, cl)
    res <- rbind(res, data.frame(cluster = cl, celltype = NA_character_, score = 0,
                                 second = NA_character_, score2 = 0, n_hit = 0L,
                                 hit_genes = "", hit_genes_all = "",
                                 full_name = "", refs = "",
                                 reason = "该簇没有可用于打分的 marker（未进入显著差异表）",
                                 stringsAsFactors = FALSE))
    next
  }

  sc <- lapply(dict, function(e) score_one(g, e))
  score_v <- vapply(sc, function(x) x$score, numeric(1))
  nhit_v  <- vapply(sc, function(x) as.integer(x$n), integer(1))

  if (!length(score_v) || max(score_v) <= 0) {
    unmatched <- c(unmatched, cl)
    res <- rbind(res, data.frame(cluster = cl, celltype = NA_character_, score = 0,
                                 second = NA_character_, score2 = 0, n_hit = 0L,
                                 hit_genes = "", hit_genes_all = "",
                                 full_name = "", refs = "",
                                 reason = "词典里没有任何一个 marker 命中",
                                 stringsAsFactors = FALSE))
    next
  }

  ord <- order(score_v, decreasing = TRUE)
  b1  <- names(score_v)[ord[1]]
  s1  <- score_v[ord[1]]
  n1  <- nhit_v[ord[1]]
  b2  <- if (length(ord) > 1) names(score_v)[ord[2]] else NA_character_
  s2  <- if (length(ord) > 1) score_v[ord[2]] else 0

  reason <- ""
  if (n1 < min_score) {
    reason <- paste0("命中数 ", n1, " < 阈值 ", min_score)
  } else if (s2 > 0 && s1 / s2 < min_ratio) {
    reason <- paste0("第一名 ", b1, "（", round(s1, 2), "）与第二名 ", b2,
                     "（", round(s2, 2), "）分不开，比值 ", round(s1 / s2, 2),
                     " < ", min_ratio)
  }

  if (nzchar(reason)) {
    if (n1 < min_score) unmatched <- c(unmatched, cl) else low_conf <- c(low_conf, cl)
    res <- rbind(res, data.frame(cluster = cl, celltype = NA_character_,
                                 score = round(s1, 3), second = b2, score2 = round(s2, 3),
                                 n_hit = n1,
                                 hit_genes = paste(head(sc[[b1]]$hit, 8), collapse = ","),
                                 # 完整命中串：判定层要看"是不是本类型的基因"，
                                 #   截断到 8 个会把后面的证据丢掉。
                                 hit_genes_all = paste(sc[[b1]]$hit, collapse = ","),
                                 full_name = "", refs = "", reason = reason,
                                 stringsAsFactors = FALSE))
    next
  }

  res <- rbind(res, data.frame(cluster = cl, celltype = b1,
                               score = round(s1, 3), second = b2, score2 = round(s2, 3),
                               n_hit = n1,
                               hit_genes = paste(head(sc[[b1]]$hit, 8), collapse = ","),
                               hit_genes_all = paste(sc[[b1]]$hit, collapse = ","),
                               full_name = dict[[b1]]$full,
                               refs = paste(dict[[b1]]$refs, collapse = ","),
                               reason = "",
                               stringsAsFactors = FALSE))
}
# 展示前 8 个命中基因，n_hit 仍是完整命中数。
#   把「展示条数」当成「命中条数」验收，会逼人要么少报命中、要么拉长展示，
#   两种都是错的。这里把展示条数与是否截断明确记下。
res$n_hit_shown <- vapply(seq_len(nrow(res)), function(i) {
  v <- as.character(res$hit_genes[i])
  if (is.na(v) || !nzchar(v)) 0L else length(strsplit(v, ",", fixed = TRUE)[[1]])
}, integer(1))
res$hit_genes_truncated <- as.integer(res$n_hit > res$n_hit_shown)
# hit_genes_all 的条数应与 n_hit 一致；不一致说明命中串被截过，属实现错误。
res$n_hit_all <- vapply(seq_len(nrow(res)), function(i) {
  v <- as.character(res$hit_genes_all[i])
  if (is.na(v) || !nzchar(v)) 0L else length(strsplit(v, ",", fixed = TRUE)[[1]])
}, integer(1))
if (any(res$n_hit_all != as.integer(res$n_hit))) {
  LOG("注意：有 ", sum(res$n_hit_all != as.integer(res$n_hit)),
      " 行 hit_genes_all 条数与 n_hit 不一致（应为完整命中串）")
}
write.table(res, file.path(outdir, "annot_result.xls"), quote = FALSE, sep = "\t", row.names = FALSE)
if (length(unmatched) > 0) {
  LOG("以下 ", length(unmatched), " 个群未匹配到任何词典类型（命中数 < ", min_score, "）：",
      paste(unmatched, collapse = ", "))
}
if (length(low_conf) > 0) {
  LOG("以下 ", length(low_conf), " 个群有候选但分不开（第一名不够领先）：",
      paste(low_conf, collapse = ", "))
}


# ---------------------------------------------------------------------
# 4. 写回对象
# ---------------------------------------------------------------------
STEP("STEP 4 写回 annot 列")
map <- setNames(res$celltype, res$cluster)
obj$annot <- factor(unname(map[as.character(obj$seurat_clusters)]))
n_na <- sum(is.na(obj$annot))
if (n_na > 0) {
  LOG("注意：", n_na, " 个细胞所在群未判出类型，annot 为 NA（保留 NA，不用近似类型填充）。")
}
# 计数表按契约输出 celltype / n_cells / typed 三列。
#   原来直接写 table() 结果，列名是 Var1 / Freq —— R 的内部命名，不是契约；
#   且未定型细胞容易漏在外，加起来对不上总数。
cnt <- as.data.frame(table(obj$annot, useNA = "ifany"), stringsAsFactors = FALSE)
names(cnt) <- c("celltype", "n_cells")
cnt$celltype <- ifelse(is.na(cnt$celltype), "unassigned", as.character(cnt$celltype))
cnt$typed <- ifelse(cnt$celltype == "unassigned", "no", "yes")
cnt <- cnt[order(cnt$celltype != "unassigned", cnt$celltype), ]
write.table(cnt, file.path(outdir, "celltype_count.xls"), quote = FALSE, sep = "\t", row.names = FALSE)
LOG("计数表合计 ", sum(cnt$n_cells), " 细胞（未定型 ",
    sum(cnt$n_cells[cnt$celltype == "unassigned"]), "），对象总数 ", ncol(obj), " 细胞")

saveRDS(obj, file.path(outdir, "annotated.rds"))
LOG("已保存: ", file.path(outdir, "annotated.rds"))

# ---------------------------------------------------------------------
# 5. 报告
# ---------------------------------------------------------------------
# 5. 报告（含质量判定）
#
#   为什么报告里要有"质量判 LOW"这一句
#     上一轮 annotate 的状态是 done、产物齐全，可 24 个簇只判出 7 类、
#     82.6% 的细胞没有类型。页面上看不出任何异常 —— 状态完成、文件都在。
#     结果这份注释被当成结论往 PPT 里排。
#     "跑完了"和"结果能用"是两件事，必须分开说，而且要留下痕迹。
# ---------------------------------------------------------------------
n_clu   <- length(unique(obj$seurat_clusters))
# 两个量必须分开算，历史上混成一个变量，导致报告写错：
#   n_typed_clusters = 判出类型的簇数（报告里的「x/32 个群已定类型」用它）
#   n_celltypes      = 不同细胞类型个数（报告里的「判出类型数」用它）
# 实测一轮：32 簇里 20 簇定型、归入 15 种类型、12 簇未定型。
#   若只写 15/32，读者会以为只有 15 个簇定了型。
# 显式掩码：R 里 nzchar(NA) 默认是 TRUE，直接 sum(nzchar(...)) 会把
#   未定型（NA）当成"已定型"计入，本轮会算出 32 而不是 20。
#   空串同样不算类型，所以两个量都只在 !is.na 且非空白 的掩码内统计。
.ct_raw <- trimws(as.character(res$celltype))
.ct_ok  <- !is.na(.ct_raw) & nzchar(.ct_raw)
n_typed_clusters <- sum(.ct_ok)
n_celltypes <- length(unique(.ct_raw[.ct_ok]))
# 兼容旧变量名：质量分档历史上按「类型数」判断，保持同一含义。
n_typed <- n_celltypes
na_pct  <- if (ncol(obj) > 0) round(100 * n_na / ncol(obj), 1) else NA_real_

# 质量分档：只看能确定性判定的东西，不猜"生物学上对不对"。
# 注意 R 的换行规则：} else if (...) 里的 else 不能单独起一行，
#   否则解析器在上一行就已经把 if 语句结束了，会报 unexpected 'else'。
qual <- if (!is.na(na_pct) && na_pct > bad_na_pct) {
  "LOW"
} else if (n_typed <= 1) {
  "LOW"
} else if (na_pct > bad_na_pct / 2) {
  "PARTIAL"
} else {
  "OK"
}

qual_note <- switch(qual,
  LOW = paste0("未定型细胞 ", na_pct, "%（阈值 ", bad_na_pct, "%），",
               "只有 ", n_typed_clusters, "/", n_clu, " 个群判出类型（共 ",
               n_celltypes, " 种类型）。",
               "这份注释只能当参考，不能作为交付结论；要改善需补厚词典或提高 marker 质量。"),
  PARTIAL = paste0("未定型细胞 ", na_pct, "%，部分群没定下来。可作为参考，",
                   "涉及未定型群的差异与富集结论要谨慎。"),
  OK = paste0("未定型细胞 ", na_pct, "%，", n_typed_clusters, "/", n_clu,
              " 个群已定类型，归入 ", n_celltypes, " 种细胞类型。"))

LOG("质量判定: ", qual, " —— ", qual_note)
cat("ANNOT_QUALITY", qual, "\n")
cat("ANNOT_UNTYPED_PCT", na_pct, "\n")

rep_lines <- c(
  "# 注释运行报告", "",
  paste0("- 时间: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("- 输入对象: ", rds_path),
  paste0("- 词典来源: ", if (!is.null(lit_dict)) lit_dict else reference),
  paste0("- 词典条目数: ", length(dict)),
  paste0("- marker 参数: min.pct=", min_pct, ", logfc.threshold=", logfc,
         ", 候选池 top_n=", top_n, ", 最少命中=", min_score, ", 置信比=", min_ratio),
  paste0("- 聚类数: ", n_clu),
  paste0("- 已定类型群数: ", n_typed_clusters, "/", n_clu,
         "（未定型 ", n_clu - n_typed_clusters, " 群）"),
  paste0("- 不同细胞类型数: ", n_celltypes),
  paste0("- 未匹配群（命中不足）: ", if (length(unmatched)) paste(unmatched, collapse = ", ") else "无"),
  paste0("- 分不开的群（第一名不领先）: ", if (length(low_conf)) paste(low_conf, collapse = ", ") else "无"),
  paste0("- 未判型细胞数: ", n_na, "（", na_pct, "%）"),
  paste0("- **质量判定: ", qual, "** —— ", qual_note),
  "", "## 逐群判定结果", "",
  paste0("| 群 | 类型 | 得分 | 次优 | 命中数 | 命中基因 | 不判原因 |"),
  paste0("|---|---|---|---|---|---|---|"),
  apply(res, 1, function(r) paste0("| ", r[["cluster"]], " | ",
                                   if (is.na(r[["celltype"]])) "（未定型）" else r[["celltype"]],
                                   " | ", r[["score"]], " | ",
                                   if (is.na(r[["second"]])) "" else r[["second"]], " | ",
                                   r[["n_hit"]], " | ", r[["hit_genes"]], " | ",
                                   if (is.null(r[["reason"]])) "" else r[["reason"]], " |")),
  "", "## 怎么读这份结果", "",
  "- 「未定型」不是漏跑，是判定没有达到置信标准时主动留空。",
  "  一个名字错的细胞类型会被当成结论写进报告和 PPT，事后极难发现；",
  "  留空在表里一眼就能看出来，这是更负责的做法。",
  "- 想减少未定型，两个方向：① 补厚词典（每类 marker 更多、都有文献出处）；",
  "  ② 提高上游聚类质量（簇分得更干净，marker 才更有辨识度）。",
  "- 调参不能提高准确度上限：候选池与阈值只能决定「多判还是少判」，",
  "  不能把错的 marker 变成对的。"
)
writeLines(rep_lines, file.path(outdir, "annot_report.md"))

# 质量判定单独落一份，给页面和判定器读。
#   为什么不塞进 annot_result.xls：那是逐群明细，机器要读的结论应当独立成文件，
#   免得下游为了取一个结论去解析一张表。
writeLines(jsonlite::toJSON(list(
  quality = qual,
  note = qual_note,
  clusters = n_clu,
  typed_clusters = n_typed_clusters,
  celltypes = n_celltypes,
  untyped_cells = n_na,
  untyped_pct = na_pct,
  threshold_pct = bad_na_pct,
  unmatched = as.list(unmatched),
  low_confidence = as.list(low_conf),
  params = list(top_n = top_n, min_score = min_score,
                min_ratio = min_ratio, min_pct = min_pct, logfc = logfc),
  dictionary_entries = length(dict),
  at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
), auto_unbox = TRUE, pretty = TRUE), file.path(outdir, "annot_quality.json"))

STEP("注释完成")
LOG("输出目录: ", outdir)
