#!/usr/bin/env Rscript
# =====================================================================
# 03_verify.R — 数值核对（我方产物 vs 官方交付 result）
#
# 回答一个问题：到底对齐了没有，差在哪。
#
# 只读。绝不修改任何一边的文件。
#   读不到的文件明确写「缺失」，不用 0 / 空值顶替。
#   数值按原始精度比对，不做四舍五入后再比。
#
# 用法：
#   Rscript 03_verify.R \
#     --official <官方 result 目录> \
#     --mine     <我方结果目录>     \
#     --out      <报告输出目录>     \
#     [--indir   <原始 matrix 目录>]
#
# 输出：
#   <out>/verify_report.md   人读报告（分节 + 结论）
#   <out>/verify_diff.tsv    机器可读逐项差异表
#
# 依赖：只用基础 R。若本机装了 Seurat，则优先从 annotated.rds 直读
#       群数 / 细胞类型数；装不了则退回 meta.tsv / 结果表。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, scipen = 999, warn = -1))

# ---------------------------------------------------------------------
# 0. 参数解析（严格：缺参报错，不静默用默认路径）
# ---------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
prog <- "03_verify.R"

USAGE <- paste0(
  "用法: Rscript ", prog, " --official <官方result目录> --mine <我方结果目录> ",
  "--out <报告输出目录> [--indir <原始matrix目录>]\n",
  "  必填: --official  --mine  --out\n",
  "  可选: --indir   给了就核对各样本原始细胞数与官方 raw 列\n",
  "  说明: 本脚本只读，不改任何一边的文件。\n"
)

KNOWN <- c("--official", "--mine", "--out", "--indir", "--help", "-h")

# 解析器：flag 存在但后面没有取值 → 直接报错，不猜默认
get_opt <- function(k) {
  i <- match(k, args)
  if (is.na(i)) return(NULL)
  if (i == length(args) || grepl("^--", args[i + 1])) {
    stop("参数 ", k, " 后面缺少取值。\n\n", USAGE, call. = FALSE)
  }
  args[i + 1]
}

if (any(args %in% c("--help", "-h"))) { cat(USAGE); quit(status = 0) }

# 未识别的参数 → 报错（避免把路径当 flag 吞掉）
unknown <- args[grepl("^--", args) & !(args %in% KNOWN)]
if (length(unknown) > 0) {
  stop("无法识别的参数: ", paste(unknown, collapse = ", "), "\n\n", USAGE, call. = FALSE)
}

official_arg <- get_opt("--official")
mine_arg     <- get_opt("--mine")
out_arg      <- get_opt("--out")
indir_arg    <- get_opt("--indir")

miss_param <- c(
  if (is.null(official_arg)) "--official",
  if (is.null(mine_arg))     "--mine",
  if (is.null(out_arg))      "--out"
)
if (length(miss_param) > 0) {
  stop("缺少必填参数: ", paste(miss_param, collapse = ", "), "\n\n", USAGE, call. = FALSE)
}

# ---------------------------------------------------------------------
# 小工具
# ---------------------------------------------------------------------
LOG <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))

norm_path <- function(p) {
  if (is.null(p) || is.na(p)) return(NA_character_)
  p <- path.expand(p)
  p <- gsub("\\\\", "/", p)                      # Windows 反斜杠 → 正斜杠
  suppressWarnings(normalizePath(p, winslash = "/", mustWork = FALSE))
}

# 目录存在性 + 若下面还套了一层 result/ 则自动下探一层（并明确记录）
resolve_root <- function(p, label) {
  if (is.na(p) || !dir.exists(p)) {
    stop(label, " 目录不存在: ", p, call. = FALSE)
  }
  segs <- c("0.QC", "1.cluster")
  has <- function(d) any(vapply(segs, function(s) dir.exists(file.path(d, s)), logical(1)))
  if (!has(p) && dir.exists(file.path(p, "result")) && has(file.path(p, "result"))) {
    LOG(label, " 目录下未直接看到六段结构，自动下探到: ", file.path(p, "result"))
    p <- file.path(p, "result")
  }
  p
}

# .xls 实为制表符文本；先看头两行确认分隔符
read_text_table <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return(NULL)
  lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) readLines(path, warn = FALSE))
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) < 1) return(NULL)
  hdr <- lines[1]
  sep <- if (grepl("\t", hdr, fixed = TRUE)) "\t" else if (grepl(";", hdr, fixed = TRUE)) ";" else ","
  tryCatch(
    read.delim(text = paste(lines, collapse = "\n"), sep = sep, header = TRUE,
               check.names = FALSE, stringsAsFactors = FALSE, quote = "\"", comment.char = ""),
    error = function(e) NULL
  )
}

# 大小写不敏感地在目录里找文件名
find_file <- function(dir, pattern) {
  if (is.null(dir) || is.na(dir) || !dir.exists(dir)) return(NA_character_)
  fs <- list.files(dir, full.names = TRUE)
  hit <- fs[grepl(pattern, basename(fs), ignore.case = TRUE)]
  if (length(hit) == 0) NA_character_ else hit[1]
}

# 按列名（大小写/空白不敏感）取列
pick_col <- function(df, cand) {
  if (is.null(df)) return(NULL)
  cn <- gsub("[^a-z0-9]", "", tolower(colnames(df)))
  for (c0 in cand) {
    k <- gsub("[^a-z0-9]", "", tolower(c0))
    j <- which(cn == k)
    if (length(j) > 0) return(df[[j[1]]])
  }
  NULL
}

fmt_num <- function(x) {
  if (length(x) == 0 || is.na(x)) return("")
  if (x == 0) return("0")
  sprintf("%.15g", x)
}

# 原始精度比对 + 差值
cmp_cell <- function(o, m) {
  o_na <- is.na(o) || !nzchar(trimws(as.character(o)))
  m_na <- is.na(m) || !nzchar(trimws(as.character(m)))
  if (o_na && m_na) return(list(diff = "", consistent = "一致(双方均空)"))
  if (o_na || m_na) return(list(diff = "", consistent = "不一致(一方缺失)"))
  os <- trimws(as.character(o)); ms <- trimws(as.character(m))
  on <- suppressWarnings(as.numeric(os)); mn <- suppressWarnings(as.numeric(ms))
  if (!is.na(on) && !is.na(mn)) {
    d <- mn - on
    return(list(diff = fmt_num(d), consistent = if (d == 0) "一致" else "不一致"))
  }
  list(diff = "", consistent = if (identical(os, ms)) "一致" else "不一致")
}

sanitize <- function(x) gsub("[\t\r\n]+", " ", as.character(x))

diff_rows <- list()
add_diff <- function(item, off, min_, d = "", ok = "不一致") {
  diff_rows[[length(diff_rows) + 1]] <<- data.frame(
    项目 = sanitize(item), 官方值 = sanitize(off), 我方值 = sanitize(min_),
    差值 = sanitize(d), 是否一致 = sanitize(ok), stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------
# 1. 定位两侧根目录与 QC 表
# ---------------------------------------------------------------------
official_root <- resolve_root(norm_path(official_arg), "--official")
mine_root     <- resolve_root(norm_path(mine_arg), "--mine")
indir_root    <- if (!is.null(indir_arg)) norm_path(indir_arg) else NULL

if (!is.null(indir_root) && !dir.exists(indir_root)) {
  stop("--indir 目录不存在: ", indir_root, call. = FALSE)
}
out_dir <- norm_path(out_arg)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
if (!dir.exists(out_dir)) stop("输出目录创建失败: ", out_dir, call. = FALSE)

LOG("官方目录: ", official_root)
LOG("我方目录: ", mine_root)
LOG("报告目录: ", out_dir)

o_qc <- file.path(official_root, "0.QC", "total_stat.xls")
o_qc <- if (file.exists(o_qc)) o_qc else find_file(file.path(official_root, "0.QC"), "^total_stat")
m_qc <- file.path(mine_root, "0.QC", "total_stat.xls")
m_qc <- if (file.exists(m_qc)) m_qc else find_file(file.path(mine_root, "0.QC"), "^total_stat")

o_tab <- read_text_table(o_qc)
m_tab <- read_text_table(m_qc)

o_missing_note <- if (is.null(o_tab)) if (is.na(o_qc)) "缺失(找不到 0.QC/total_stat.xls)" else "读取失败" else ""
m_missing_note <- if (is.null(m_tab)) if (is.na(m_qc)) "缺失(找不到 0.QC/total_stat.xls)" else "读取失败" else ""

# ---------------------------------------------------------------------
# 2. 质控表逐样本逐列对比
# ---------------------------------------------------------------------
QC_COLS <- c("raw", "residue", "filter", "ratio", "geneMedian", "MtMedian")

qc_detail <- data.frame()          # 逐样本 × 逐列明细（供报告）
qc_summary <- data.frame()         # 逐列不一致统计

o_samples <- if (!is.null(o_tab)) trimws(as.character(pick_col(o_tab, c("sample")))) else character(0)
m_samples <- if (!is.null(m_tab)) trimws(as.character(pick_col(m_tab, c("sample")))) else character(0)

if (is.null(o_tab)) add_diff("QC/官方表", o_missing_note, "", "", "无法比对")
if (is.null(m_tab)) add_diff("QC/我方表", "存在", m_missing_note, "", "无法比对")

if (!is.null(o_tab) && !is.null(m_tab)) {
  only_o <- setdiff(o_samples, m_samples)
  only_m <- setdiff(m_samples, o_samples)
  common <- intersect(o_samples, m_samples)
  if (length(only_o) > 0) add_diff("QC/样本", paste(only_o, collapse = ","), "缺失", "", "不一致")
  if (length(only_m) > 0) add_diff("QC/样本", "缺失", paste(only_m, collapse = ","), "", "不一致")

  o_idx <- setNames(seq_along(o_samples), o_samples)
  m_idx <- setNames(seq_along(m_samples), m_samples)

  for (col in QC_COLS) {
    oc <- pick_col(o_tab, c(col)); mc <- pick_col(m_tab, c(col))
    if (is.null(oc)) { add_diff(paste0("QC/列/", col), "官方无此列", "有", "", "无法比对"); next }
    if (is.null(mc)) { add_diff(paste0("QC/列/", col), "有", "我方无此列", "", "无法比对"); next }
    n_bad <- 0
    for (s in common) {
      r <- cmp_cell(oc[o_idx[[s]]], mc[m_idx[[s]]])
      if (r$consistent != "一致") n_bad <- n_bad + 1
      qc_detail <- rbind(qc_detail, data.frame(
        sample = s, column = col, official = as.character(oc[o_idx[[s]]]),
        mine = as.character(mc[m_idx[[s]]]), diff = r$diff,
        consistent = r$consistent, stringsAsFactors = FALSE))
      # QC 是「数值对齐」的核心，逐样本逐列全部落表（含一致项），便于逐条核对
      add_diff(paste0("QC/", s, "/", col), as.character(oc[o_idx[[s]]]),
               as.character(mc[m_idx[[s]]]), r$diff, r$consistent)
    }
    qc_summary <- rbind(qc_summary, data.frame(
      column = col, n_cmp = length(common), n_bad = n_bad,
      stringsAsFactors = FALSE))
  }
}

# ---------------------------------------------------------------------
# 3. 总量对比
# ---------------------------------------------------------------------
stat_sum <- function(tab) {
  if (is.null(tab)) return(NULL)
  list(
    n_sample = nrow(tab),
    raw      = suppressWarnings(sum(as.numeric(pick_col(tab, c("raw"))), na.rm = TRUE)),
    residue  = suppressWarnings(sum(as.numeric(pick_col(tab, c("residue"))), na.rm = TRUE)),
    filter   = suppressWarnings(sum(as.numeric(pick_col(tab, c("filter"))), na.rm = TRUE))
  )
}
o_sum <- stat_sum(o_tab); m_sum <- stat_sum(m_tab)

totals <- list()
for (item in c("样本数", "raw合计", "residue合计(高质量细胞)", "filter合计(剔除)")) {
  key <- switch(item, "样本数" = "n_sample", "raw合计" = "raw",
                "residue合计(高质量细胞)" = "residue", "filter合计(剔除)" = "filter")
  ov <- if (is.null(o_sum)) NA else o_sum[[key]]
  mv <- if (is.null(m_sum)) NA else m_sum[[key]]
  r  <- if (is.null(o_sum) || is.null(m_sum)) list(diff = "", consistent = "无法比对")
       else cmp_cell(ov, mv)
  totals[[item]] <- list(official = ov, mine = mv, diff = r$diff, consistent = r$consistent)
  add_diff(paste0("总量/", item), ifelse(is.null(o_sum), "缺失", as.character(ov)),
           ifelse(is.null(m_sum), "缺失", as.character(mv)), r$diff, r$consistent)
}

# ---------------------------------------------------------------------
# 4. 群数 / 细胞类型数对比
# ---------------------------------------------------------------------
CANON_SEG <- c("0.QC", "1.cluster", "2.markgene", "3.cell_ratio", "4.diff_enrich", "5.annot")
seg_key <- function(x) gsub("[^a-z0-9]", "", tolower(x))

match_segments <- function(root) {
  dirs <- basename(list.dirs(root, recursive = FALSE, full.names = TRUE))
  keys <- seg_key(dirs)
  out <- setNames(vector("list", length(CANON_SEG)), CANON_SEG)
  for (cs in CANON_SEG) {
    hit <- dirs[keys == seg_key(cs)]
    if (length(hit) == 0) {
      num <- sub("^([0-9]+).*$", "\\1", cs)
      cand <- dirs[grepl(paste0("^0*", num, "([^0-9]|$)"), dirs)]
      if (length(cand) == 1) hit <- cand
    }
    out[[cs]] <- if (length(hit) >= 1) hit[1] else NA_character_
  }
  out
}
o_seg <- match_segments(official_root)
m_seg <- match_segments(mine_root)

# 从表的某列里数水平数（数不到返回 NULL）
count_levels_in_dir <- function(dir, cand) {
  if (is.na(dir) || !dir.exists(dir)) return(NULL)
  fs <- list.files(dir, full.names = TRUE, recursive = TRUE)
  fs <- fs[grepl("\\.(xls|txt|tsv|csv)$", fs, ignore.case = TRUE)]
  for (f in fs) {
    tb <- read_text_table(f)
    if (is.null(tb) || ncol(tb) == 0) next
    v <- pick_col(tb, cand)
    if (!is.null(v)) {
      u <- unique(trimws(as.character(v)))
      u <- u[nzchar(u) & !is.na(u)]
      if (length(u) > 0) return(list(n = length(u), src = basename(f), col = "column"))
    }
    # 兜底：两列表（字符列 + 计数列）→ 按字符列数水平
    if (ncol(tb) == 2) {
      ch <- which(vapply(tb, function(x) is.character(x) || is.factor(x), logical(1)))
      if (length(ch) == 1) {
        u <- unique(trimws(as.character(tb[[ch]])))
        u <- u[nzchar(u) & !is.na(u)]
        if (length(u) > 1 && length(u) < 200) {
          return(list(n = length(u), src = basename(f), col = paste0("fallback:", colnames(tb)[ch])))
        }
      }
    }
  }
  NULL
}

# 官方侧
o_cl  <- count_levels_in_dir(file.path(official_root, o_seg[["1.cluster"]]),
                             c("seurat_clusters", "cluster", "clusters"))
o_ct  <- count_levels_in_dir(file.path(official_root, o_seg[["5.annot"]]),
                             c("celltype", "cell_type", "annot", "type"))
o_cl_note <- if (is.null(o_cl)) "官方此项无法从文件推断" else paste0("来自 ", o_cl$src, " (", o_cl$col, ")")
o_ct_note <- if (is.null(o_ct)) "官方此项无法从文件推断" else paste0("来自 ", o_ct$src, " (", o_ct$col, ")")
if (is.null(o_cl)) o_cl <- list(n = NA, src = NA, col = NA)
if (is.null(o_ct)) o_ct <- list(n = NA, src = NA, col = NA)

# 我方侧：annotated.rds → meta.tsv → 结果表
read_mine_meta <- function(root) {
  cands <- c(
    file.path(root, "annot", "annotated.rds"),
    file.path(root, "annotated.rds"),
    file.path(root, "annot", "meta.tsv"),
    file.path(root, "meta.tsv"),
    file.path(root, "annot", "meta.txt"),
    file.path(root, "annot", "celltype_count.xls")
  )
  for (f in cands) if (file.exists(f)) return(f)
  NA_character_
}
m_meta_path <- read_mine_meta(mine_root)

m_cl <- NULL; m_ct <- NULL; m_cl_src <- "缺失"; m_ct_src <- "缺失"
if (!is.na(m_meta_path) && grepl("\\.rds$", m_meta_path, ignore.case = TRUE)) {
  if (requireNamespace("Seurat", quietly = TRUE)) {
    obj <- tryCatch(readRDS(m_meta_path), error = function(e) NULL)
    md  <- tryCatch(as.data.frame(obj[[]]), error = function(e) NULL)
    if (is.null(md)) md <- tryCatch(obj@meta.data, error = function(e) NULL)
    if (!is.null(md)) {
      if ("seurat_clusters" %in% colnames(md)) {
        m_cl <- length(unique(trimws(as.character(md$seurat_clusters))))
        m_cl_src <- paste0("来自 ", basename(m_meta_path), " (seurat_clusters)")
      }
      if ("annot" %in% colnames(md)) {
        a <- trimws(as.character(md$annot)); a <- a[!is.na(a) & nzchar(a)]
        m_ct <- length(unique(a))
        m_ct_src <- paste0("来自 ", basename(m_meta_path), " (annot)")
      }
    } else {
      m_cl_src <- m_ct_src <- "annotated.rds 读取失败"
    }
  } else {
    m_cl_src <- m_ct_src <- "本机无 Seurat 包，无法读 rds（可改用 meta.tsv）"
  }
} else if (!is.na(m_meta_path)) {
  tb <- read_text_table(m_meta_path)
  if (!is.null(tb)) {
    v <- pick_col(tb, c("seurat_clusters", "cluster", "clusters"))
    if (!is.null(v)) m_cl <- length(unique(trimws(as.character(v))))
    a <- pick_col(tb, c("annot", "celltype", "cell_type", "type"))
    if (!is.null(a)) {
      u <- trimws(as.character(a)); u <- u[!is.na(u) & nzchar(u)]
      m_ct <- length(unique(u))
    }
    m_cl_src <- m_ct_src <- paste0("来自 ", basename(m_meta_path))
  }
} else {
  m_cl_src <- m_ct_src <- "缺失(无 annotated.rds / meta.tsv)"
}
if (is.null(m_cl)) m_cl <- NA
if (is.null(m_ct)) m_ct <- NA

cl_cmp <- if (is.na(o_cl$n) || is.na(m_cl)) list(diff = "", consistent = "无法比对") else cmp_cell(o_cl$n, m_cl)
ct_cmp <- if (is.na(o_ct$n) || is.na(m_ct)) list(diff = "", consistent = "无法比对") else cmp_cell(o_ct$n, m_ct)
add_diff("群数/seurat_clusters", as.character(o_cl$n), as.character(m_cl), cl_cmp$diff, cl_cmp$consistent)
add_diff("群数/细胞类型数",     as.character(o_ct$n), as.character(m_ct), ct_cmp$diff, ct_cmp$consistent)

# ---------------------------------------------------------------------
# 5. 图表清单对比（六段目录）
# ---------------------------------------------------------------------
KEEP_EXT <- c("png", "pdf", "xls", "xlsx", "txt", "csv", "tsv", "jpg", "jpeg", "svg")
list_seg_files <- function(root, seg) {
  if (is.na(seg)) return(NULL)
  d <- file.path(root, seg)
  if (!dir.exists(d)) return(NULL)
  fs <- list.files(d, recursive = TRUE, full.names = FALSE)
  fs <- fs[grepl(paste0("\\.(", paste(KEEP_EXT, collapse = "|"), ")$"), fs, ignore.case = TRUE)]
  sort(fs)
}
inv <- list()
for (cs in CANON_SEG) {
  of <- list_seg_files(official_root, o_seg[[cs]])
  mf <- list_seg_files(mine_root, m_seg[[cs]])
  miss_mine <- if (is.null(mf)) NULL else setdiff(of, mf)
  only_off  <- if (is.null(mf)) NULL else setdiff(mf, of)
  inv[[cs]] <- list(o = of, m = mf, miss_mine = miss_mine, only_off = only_off)
  if (is.null(mf)) {
    add_diff(paste0("文件清单/", cs), if (is.null(of)) "缺失" else paste0(length(of), " 个文件"),
             "缺失(我方无此段目录)", "", "无法比对")
  } else if (is.null(of)) {
    add_diff(paste0("文件清单/", cs), "缺失(官方无此段目录)",
             paste0(length(mf), " 个文件"), "", "无法比对")
  } else {
    add_diff(paste0("文件清单/", cs), paste0(length(of), " 个文件"), paste0(length(mf), " 个文件"),
             as.character(length(mf) - length(of)),
             if (identical(of, mf)) "一致" else "不一致")
    for (f in miss_mine) add_diff(paste0("文件清单/", cs, "/我方缺"), f, "无", "", "不一致")
    for (f in only_off)  add_diff(paste0("文件清单/", cs, "/官方无"), "无", f, "", "不一致")
  }
}

# ---------------------------------------------------------------------
# 6. 原始数据核对（--indir）
# ---------------------------------------------------------------------
raw_cmp <- data.frame()
if (!is.null(indir_root)) {
  barcodes_of <- function(sd) {
    cands <- c("barcodes.tsv", "barcodes.tsv.gz",
               "outs/filtered_feature_bc_matrix/barcodes.tsv",
               "outs/filtered_feature_bc_matrix/barcodes.tsv.gz",
               "outs/raw_feature_bc_matrix/barcodes.tsv",
               "outs/raw_feature_bc_matrix/barcodes.tsv.gz")
    for (c0 in cands) {
      f <- file.path(sd, c0)
      if (file.exists(f)) return(f)
    }
    NA_character_
  }
  count_lines <- function(f) {
    if (grepl("\\.gz$", f, ignore.case = TRUE)) {
      con <- gzfile(f, "rt"); on.exit(close(con))
      return(length(readLines(con, warn = FALSE)))
    }
    length(readLines(f, warn = FALSE))
  }
  subdirs <- basename(list.dirs(indir_root, recursive = FALSE, full.names = TRUE))
  for (s in sort(subdirs)) {
    sd <- file.path(indir_root, s)
    bf <- barcodes_of(sd)
    n_bc <- if (is.na(bf)) NA_integer_ else tryCatch(count_lines(bf), error = function(e) NA_integer_)
    o_raw <- if (!is.null(o_tab) && s %in% o_samples) as.character(pick_col(o_tab, "raw")[match(s, o_samples)]) else NA_character_
    r <- cmp_cell(o_raw, n_bc)
    raw_cmp <- rbind(raw_cmp, data.frame(
      sample = s, official_raw = o_raw, barcodes = n_bc, diff = r$diff,
      consistent = r$consistent, file = if (is.na(bf)) "缺失" else basename(bf),
      stringsAsFactors = FALSE))
    add_diff(paste0("原始数据/", s, "/raw"),
             ifelse(is.na(o_raw), "官方表中无此样本", o_raw),
             ifelse(is.na(n_bc), "barcodes 缺失", as.character(n_bc)),
             r$diff,
             ifelse(is.na(n_bc), "无法比对", r$consistent))
  }
  if (length(subdirs) == 0) {
    add_diff("原始数据/样本扫描", paste0(indir_root, " 下无子目录"), "", "", "无法比对")
  }
}

# ---------------------------------------------------------------------
# 7. 汇总判定
# ---------------------------------------------------------------------
df_diff <- if (length(diff_rows) == 0) {
  data.frame(项目 = character(0), 官方值 = character(0), 我方值 = character(0),
             差值 = character(0), 是否一致 = character(0), stringsAsFactors = FALSE)
} else do.call(rbind, diff_rows)

n_row     <- nrow(df_diff)
n_bad     <- sum(df_diff$是否一致 %in% c("不一致(一方缺失)", "不一致"))
n_cannot  <- sum(grepl("无法比对", df_diff$是否一致))
n_ok      <- sum(df_diff$是否一致 %in% c("一致", "一致(双方均空)"))

qc_all_ok <- !is.null(o_tab) && !is.null(m_tab) &&
  (nrow(qc_summary) > 0) && all(qc_summary$n_bad == 0)

verdict <- if (is.null(o_tab) || is.null(m_tab)) {
  "无法给出总判定：至少一侧的 0.QC/total_stat.xls 缺失或读取失败。"
} else if (!qc_all_ok) {
  "未对齐：质控表存在数值不一致项（见第 1 节与 verify_diff.tsv）。"
} else if (n_bad > 0) {
  "部分未对齐：质控表逐项一致，但其他项存在差异（见 verify_diff.tsv）。"
} else {
  "已对齐：本次核对的全部项目一致。"
}

# ---------------------------------------------------------------------
# 8. 写 verify_diff.tsv
# ---------------------------------------------------------------------
diff_path <- file.path(out_dir, "verify_diff.tsv")
write.table(df_diff, diff_path, sep = "\t", quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8")

# ---------------------------------------------------------------------
# 9. 写 verify_report.md
# ---------------------------------------------------------------------
L <- character(0)
add <- function(...) L <<- c(L, paste0(...))

hdr <- function(t) { add(""); add("## ", t); add("") }

add("# 数值核对报告")
add("")
add("- 生成时间: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
add("- 官方 result 目录: ", official_root)
add("- 我方结果目录: ", mine_root)
add("- 原始 matrix 目录: ", if (is.null(indir_root)) "(未提供 --indir，跳过第 5 节)" else indir_root)
add("- 逐项差异表: verify_diff.tsv")
add("")
add("**总判定：", verdict, "**")
add("")
add("统计：比对项 ", n_row, " 条，一致 ", n_ok, " 条，不一致 ", n_bad, " 条，无法比对 ", n_cannot, " 条。")

add(""); add("## 1. 质控表逐样本对比"); add("")
add("官方表: ", if (is.na(o_qc)) "缺失" else o_qc, if (nzchar(o_missing_note)) paste0("（", o_missing_note, "）") else "")
add("我方表: ", if (is.na(m_qc)) "缺失" else m_qc, if (nzchar(m_missing_note)) paste0("（", m_missing_note, "）") else "")
if (!is.null(o_tab) && !is.null(m_tab)) {
  add("")
  add("官方样本数: ", length(o_samples), "；我方样本数: ", length(m_samples), "；共同样本: ", length(common), "。")
  add("")
  add("| 列 | 比对样本数 | 不一致个数 |")
  add("|---|---|---|")
  for (i in seq_len(nrow(qc_summary))) {
    add("| ", qc_summary$column[i], " | ", qc_summary$n_cmp[i], " | ", qc_summary$n_bad[i], " |")
  }
  bad <- qc_detail[qc_detail$consistent != "一致", , drop = FALSE]
  add("")
  if (nrow(bad) == 0) {
    add("结论：所有共同样本的全部被核对列逐项一致（保留原始精度比对，无四舍五入）。")
  } else {
    add("不一致明细（共 ", nrow(bad), " 项）：")
    add("")
    add("| 样本 | 列 | 官方值 | 我方值 | 差值(我方-官方) |")
    add("|---|---|---|---|---|")
    for (i in seq_len(nrow(bad))) {
      add("| ", bad$sample[i], " | ", bad$column[i], " | ", bad$official[i], " | ", bad$mine[i], " | ", bad$diff[i], " |")
    }
  }
  if (length(only_o) > 0) add("", "仅官方有的样本: ", paste(only_o, collapse = ", "))
  if (length(only_m) > 0) add("", "仅我方有的样本: ", paste(only_m, collapse = ", "))
} else {
  add("")
  add("结论：无法进行逐样本对比（至少一侧缺失）。")
}

add(""); add("## 2. 总量对比"); add("")
add("| 项目 | 官方 | 我方 | 差值 | 是否一致 |")
add("|---|---|---|---|---|")
for (item in names(totals)) {
  t0 <- totals[[item]]
  add("| ", item, " | ", ifelse(is.null(o_sum), "缺失", as.character(t0$official)), " | ",
      ifelse(is.null(m_sum), "缺失", as.character(t0$mine)), " | ", t0$diff, " | ", t0$consistent, " |")
}

add(""); add("## 3. 群数与细胞类型数"); add("")
add("| 项目 | 官方 | 我方 | 差值 | 是否一致 |")
add("|---|---|---|---|---|")
add("| 群数(seurat_clusters) | ", as.character(o_cl$n), " | ", as.character(m_cl), " | ", cl_cmp$diff, " | ", cl_cmp$consistent, " |")
add("| 细胞类型数(annot) | ", as.character(o_ct$n), " | ", as.character(m_ct), " | ", ct_cmp$diff, " | ", ct_cmp$consistent, " |")
add("")
add("- 官方群数来源: ", o_cl_note)
add("- 官方类型数来源: ", o_ct_note)
add("- 我方来源: 群数 ", m_cl_src, "；类型数 ", m_ct_src)

add(""); add("## 4. 图表清单对比（六段目录）"); add("")
add("| 段 | 官方文件数 | 我方文件数 | 我方缺 | 官方没有 |")
add("|---|---|---|---|---|")
for (cs in CANON_SEG) {
  e <- inv[[cs]]
  add("| ", cs, " | ", if (is.null(e$o)) "缺失" else length(e$o), " | ",
      if (is.null(e$m)) "缺失" else length(e$m), " | ",
      if (is.null(e$miss_mine)) "缺失" else length(e$miss_mine), " | ",
      if (is.null(e$only_off)) "缺失" else length(e$only_off), " |")
}
add("")
for (cs in CANON_SEG) {
  e <- inv[[cs]]
  if (is.null(e$m)) { add("### ", cs, ""); add("我方无此段目录（目录名: ", ifelse(is.na(m_seg[[cs]]), "未匹配", m_seg[[cs]]), "）。"); add(""); next }
  add("### ", cs, "")
  add("- 官方文件名: ", if (is.null(e$o) || length(e$o) == 0) "(无/目录缺失)" else paste(e$o, collapse = ", "))
  add("- 我方文件名: ", if (length(e$m) == 0) "(无)" else paste(e$m, collapse = ", "))
  add("- 我方缺: ", if (is.null(e$miss_mine) || length(e$miss_mine) == 0) "无" else paste(e$miss_mine, collapse = ", "))
  add("- 官方没有: ", if (is.null(e$only_off) || length(e$only_off) == 0) "无" else paste(e$only_off, collapse = ", "))
  add("")
}

add(""); add("## 5. 原始数据核对"); add("")
if (is.null(indir_root)) {
  add("未提供 --indir，跳过。")
} else if (nrow(raw_cmp) == 0) {
  add("在 ", indir_root, " 下未发现样本子目录。")
} else {
  add("| 样本 | 官方 raw | barcodes 行数 | 差值 | 是否一致 | 来源文件 |")
  add("|---|---|---|---|---|---|")
  for (i in seq_len(nrow(raw_cmp))) {
    add("| ", raw_cmp$sample[i], " | ", ifelse(is.na(raw_cmp$official_raw[i]), "缺失", raw_cmp$official_raw[i]),
        " | ", ifelse(is.na(raw_cmp$barcodes[i]), "缺失", raw_cmp$barcodes[i]), " | ",
        raw_cmp$diff[i], " | ", raw_cmp$consistent[i], " | ", raw_cmp$file[i], " |")
  }
}

add(""); add("## 附：缺失与无法推断清单"); add("")
add("- 官方 QC 表: ", if (is.null(o_tab)) o_missing_note else "已读到")
add("- 我方 QC 表: ", if (is.null(m_tab)) m_missing_note else "已读到")
add("- 官方群数: ", o_cl_note)
add("- 官方类型数: ", o_ct_note)
add("- 我方群数: ", m_cl_src)
add("- 我方类型数: ", m_ct_src)
add("")
add("说明：所有「缺失」均如实标注，未用 0 或空值顶替；数值比较使用原始字符串对应的全精度值，不做四舍五入后再比。")
add("")

report_path <- file.path(out_dir, "verify_report.md")
writeLines(L, report_path, useBytes = TRUE)

LOG("报告已写出: ", report_path)
LOG("差异表已写出: ", diff_path)
LOG("总判定: ", verdict)
cat("\n")
cat(verdict, "\n")
