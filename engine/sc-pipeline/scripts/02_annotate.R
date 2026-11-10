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
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("读 Excel 需要 readxl 包，未安装。请先 install.packages('readxl')，\n",
           "  或把 reference 另存为 tsv 后用同样参数传入。不提供静默替代。", call. = FALSE)
    }
    sheets <- readxl::excel_sheets(reference)
    raw <- readxl::read_excel(reference, sheet = sheets[1], col_names = FALSE)
    raw <- as.data.frame(raw, stringsAsFactors = FALSE)
    # ---- 表头不再写死第 6 行 ----
    #   为什么改：原来固定 hdr_idx <- 6，理由是"官方表表头在第 6 行"。
    #   而 read_excel 会先去掉顶部空行，于是同一份文件读进来的行号整体上移：
    #   数据框的第 6 行其实是 Excel 的第 7 行（Podocyte 那行数据），
    #   全称行里自然找不到 'Positive Markers'，报"请确认表结构"。
    #   这不是表的问题，是"按位置找"这种写法的必然结果 ——
    #   换一份带小标题、多一行少一行空行的表，同样的错会再来一次。
    #   现在改成按内容找：哪一行写着 Positive Markers，哪一行就是表头。
    find_hdr <- function(df) {
      for (i in seq_len(min(nrow(df), 30))) {
        v <- as.character(unlist(df[i, ]))
        v <- v[!is.na(v)]
        if (any(grepl("Positive Markers", v, ignore.case = TRUE))) return(i)
      }
      NA_integer_
    }
    hdr_idx <- find_hdr(raw)
    if (is.na(hdr_idx))
      stop("在 reference 表里找不到 'Positive Markers' 列，请确认表结构。\n",
           "  （已按内容逐行查找前 30 行；这说明这份表确实不是注释依据表，\n",
           "    或者它的 marker 列名不是 Positive Markers。）", call. = FALSE)

    full_names <- as.character(unlist(raw[hdr_idx, ]))
    prev_names <- if (hdr_idx > 1) as.character(unlist(raw[hdr_idx - 1, ])) else rep(NA_character_, ncol(raw))
    # 上一行是不是"简称行"，看它有没有实质内容；全是空或只有一两个词，就说明没有简称行。
    prev_ok <- sum(!is.na(prev_names) & nzchar(trimws(prev_names))) >= max(3, ncol(raw) / 3)
    short_names <- if (prev_ok) prev_names else full_names
    pos_row <- which(grepl("Positive Markers", full_names, ignore.case = TRUE))[1]
    LOG("表头所在行（数据框内）：", hdr_idx, "；简称行", if (prev_ok) "有" else "没有（用全称）")
    lst <- list()
    for (j in seq_along(short_names)) {
      key <- if (!is.na(short_names[j]) && nzchar(short_names[j])) short_names[j] else full_names[j]
      if (is.na(key) || !nzchar(key)) next
      cells <- as.character(unlist(raw[(hdr_idx + 1):nrow(raw), j]))
      cells <- cells[!is.na(cells) & nzchar(cells) & cells != "NA"]
      mk <- unlist(strsplit(paste(cells, collapse = ","), "[,;，；\\s]+"))
      lst[[key]] <- list(markers = unique(mk), refs = "", full = full_names[j])
    }
    dict <- norm_dict(lst)
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

for (cl in levels(factor(as.character(top$cluster)))) {
  g <- toupper(as.character(top$gene[top$cluster == cl]))
  g <- g[!is.na(g) & nzchar(g)]
  if (length(g) == 0) { unmatched <- c(unmatched, cl); next }

  sc <- lapply(dict, function(e) score_one(g, e))
  score_v <- vapply(sc, function(x) x$score, numeric(1))
  nhit_v  <- vapply(sc, function(x) as.integer(x$n), integer(1))

  if (!length(score_v) || max(score_v) <= 0) {
    unmatched <- c(unmatched, cl)
    res <- rbind(res, data.frame(cluster = cl, celltype = NA_character_, score = 0,
                                 second = NA_character_, score2 = 0, n_hit = 0L,
                                 hit_genes = "", full_name = "", refs = "",
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
                                 full_name = "", refs = "", reason = reason,
                                 stringsAsFactors = FALSE))
    next
  }

  res <- rbind(res, data.frame(cluster = cl, celltype = b1,
                               score = round(s1, 3), second = b2, score2 = round(s2, 3),
                               n_hit = n1,
                               hit_genes = paste(head(sc[[b1]]$hit, 8), collapse = ","),
                               full_name = dict[[b1]]$full,
                               refs = paste(dict[[b1]]$refs, collapse = ","),
                               reason = "",
                               stringsAsFactors = FALSE))
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
cnt <- as.data.frame(table(obj$annot, useNA = "ifany"))
write.table(cnt, file.path(outdir, "celltype_count.xls"), quote = FALSE, sep = "\t", row.names = FALSE)

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
n_typed <- length(unique(na.omit(as.character(res$celltype))))
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
               "只有 ", n_typed, "/", n_clu, " 个群判出类型。",
               "这份注释只能当参考，不能作为交付结论；要改善需补厚词典或提高 marker 质量。"),
  PARTIAL = paste0("未定型细胞 ", na_pct, "%，部分群没定下来。可作为参考，",
                   "涉及未定型群的差异与富集结论要谨慎。"),
  OK = paste0("未定型细胞 ", na_pct, "%，", n_typed, "/", n_clu, " 个群已定类型。"))

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
  paste0("- 判出类型数: ", n_typed),
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
  typed_clusters = n_typed,
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
