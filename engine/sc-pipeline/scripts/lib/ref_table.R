# =====================================================================
# lib/ref_table.R — 参考表（细胞类型 marker 表）解析，单函数、纯计算
#
# 为什么单独成文件：
#   原先这段逻辑写在 02_annotate.R 里，而且两次都栽在"按位置找表"上：
#   第一次固定表头行号，第二次固定"上一行是简称行"，于是把列名当成了类型名。
#   09 月的实测跑出一堆叫 "Positive Markers" / "Literature Marker References"
#   的细胞类型，就是这么来的。
#   解析口径一旦散落在脚本里，改一次错一次；所以提到 lib 下，让生产脚本和
#   测试脚本跑同一份代码，避免"测试通过、生产还是老逻辑"。
#
# 支持的表的形态（按列名识别，不依赖任何固定行号）：
#   A/第1列  Subclass (Full Name)            —— 全称
#   ...      Subclass Level 3 / Level 1      —— 细类代码 / 大类代码
#   ...      Positive Markers / Negative Markers
#   ...      Literature Marker References    —— 文献编号（如 "1, 2, 5"）
#   表头行上方可以有标题、说明、空行；表尾 References / Additional Resources /
#   Note / Supplementary Note 起的文字一律不进词典。
#
# 口径（gina 2026-11-13 明确要求）：
#   1. 列名识别，不用固定 raw 行号
#   2. 类型 key：Subclass Level 3 有值优先用它，为空回退全称；
#      不许按全称强行合并不同层级（VSMC/P 广义行与 VSMC/P 细类行是两个类型）
#   3. 只把 Positive Markers 当阳性词典；Negative 单独保留，不混用；
#      状态列（Degenerative/Adaptive/Cycling）、参考文献、计算来源都不进 marker
#   4. 同一 key 且全称相符的多行合并 marker，保留 source_rows（表内行号）
#   5. marker 只按 [,;，；] 切分并 trim，原符号原样保留
#      （HLA_DRB1 不改成 HLA-DRB1，HLA-DQA1 也不拆成 HLA / DQA1）
#   6. 没有 marker 的类型保留，positive 写 NA，并在 reason 里写明
#   7. 参考文献区不进入词典
#
# 行号口径（实测踩过的坑，务必看清）：
#   读表时显式锚定 A1（range = cell_limits(c(1,1), c(NA,NA))），
#   这样 source_rows 就是 Excel 原行号，可以直接在 Excel 里对行。
#   为什么必须锚定：这张真表的 A 列整列空、第 1 行整行空。readxl 不指定 range 时
#   会把开头空行和空列一起裁掉，结果同一份文件读成 87 行 13 列，表头从 Excel 第 6 行
#   变成第 5 行，后面所有行号整体 -1。谁按 readxl 默认读出来的行号去 Excel 里找，
#   都会差一行（gina 给的 B45/B49 是 Excel 原行号：第 45 行 VSMC/P 广义行、
#   第 49 行 VSMC/P 细类行）。
#   所以本函数的行号一律是 Excel 原行号，meta$row_basis 会写明，
#   meta$readxl_default_dim 里同时记录不锚定时的行列数，便于和旧记录对照。
# =====================================================================

ref_table_parse <- function(path,
                            sheet = NULL,
                            header_scan_rows = 40L,
                            keep_unmarked = TRUE) {
  REF_TABLE_PARSER_VERSION <- "1.1.0"

  # ---------------- 基础工具 ----------------
  chr0 <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x)] <- ""
    x[x == "NA"] <- ""
    x
  }
  nows <- function(x) tolower(gsub("[[:space:]]+", " ", trimws(chr0(x))))
  excel_col <- function(j) {
    out <- character(0)
    while (j > 0) {
      r <- (j - 1L) %% 26L
      out <- c(intToUtf8(65L + r), out)
      j <- (j - 1L) %/% 26L
    }
    paste(out, collapse = "")
  }
  split_markers <- function(s) {
    s <- chr0(s)
    if (!nzchar(s)) return(character(0))
    parts <- unlist(strsplit(s, "[,;，；]", perl = TRUE), use.names = FALSE)
    parts <- trimws(parts)
    unique(parts[nzchar(parts)])
  }

  # ---------------- 读文件 ----------------
  if (missing(path) || is.null(path) || !nzchar(path))
    stop("ref_table_parse(): 必须给参考表路径", call. = FALSE)
  if (!file.exists(path))
    stop("ref_table_parse(): 文件不存在: ", path, call. = FALSE)
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("xlsx", "xls"))
    stop("ref_table_parse(): 只支持 xlsx / xls，收到: ", ext, call. = FALSE)
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("ref_table_parse(): 读 Excel 需要 readxl 包，未安装。\n",
         "  不提供静默替代：装 readxl，或把表另存为 tsv 走另一条分支。", call. = FALSE)

  sheet_names <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
  sheet_used <- if (is.null(sheet)) {
    if (length(sheet_names)) sheet_names[1] else 1
  } else sheet

  # 显式锚定 A1：不锚定时 readxl 会裁掉开头空行空列，行号整体偏移，
  #   source_rows 就对不上 Excel 了（见文件头"行号口径"）。
  raw <- suppressMessages(readxl::read_excel(
    path, sheet = sheet_used, col_names = FALSE, .name_repair = "minimal",
    range = readxl::cell_limits(c(1, 1), c(NA, NA))))
  # 记录不锚定时的行列数：旧记录（"87 行 13 列"）就是这么来的，留作对照。
  .default_dim <- tryCatch({
    d <- suppressMessages(readxl::read_excel(
      path, sheet = sheet_used, col_names = FALSE, .name_repair = "minimal"))
    c(nrow(d), ncol(d))
  }, error = function(e) c(NA_integer_, NA_integer_))

  m <- as.matrix(as.data.frame(raw, stringsAsFactors = FALSE))
  n_row <- nrow(m); n_col <- ncol(m)
  cell <- function(i, j) {
    if (is.na(j) || j < 1L || j > n_col || i < 1L || i > n_row) return("")
    chr0(m[i, j])
  }
  row_cells <- function(i) if (i < 1L || i > n_row) character(0) else chr0(m[i, ])

  # ---------------- 1) 按列名找表头行 ----------------
  hdr_alias <- list(
    full  = c("subclass (full name)", "subclass full name", "cell type (full name)"),
    level3 = c("subclass level 3", "subclass l3"),
    level1 = c("subclass level 1", "subclass l1"),
    pos   = c("positive markers", "positive marker"),
    neg   = c("negative markers", "negative marker"),
    refs  = c("literature marker references", "literature marker reference",
              "literature references")
  )
  col_find <- function(cells, key) {
    v <- nows(cells)
    hit <- which(v %in% hdr_alias[[key]])
    if (length(hit)) hit[1] else NA_integer_
  }

  scan_to <- min(n_row, max(1L, as.integer(header_scan_rows)))
  header_row <- NA_integer_
  header_partial <- FALSE
  for (i in seq_len(scan_to)) {
    cells <- row_cells(i)
    if (!length(cells)) next
    pos_hit <- col_find(cells, "pos")
    if (is.na(pos_hit)) next
    if (!is.na(col_find(cells, "full"))) {
      # 找到完整表头：之前若有"部分表头"候选，必须把标志显式复位，
      # 否则下面 header_partial 的判断会把这张本来完整的表拒掉。
      header_row <- i
      header_partial <- FALSE
      break
    }
    if (is.na(header_row)) { header_row <- i; header_partial <- TRUE }  # 继续往下找更完整的
  }
  if (is.na(header_row))
    stop("ref_table_parse(): 在前 ", scan_to, " 行里找不到 'Positive Markers' 表头。\n",
         "  这说明这份表不是注释依据表，或者 marker 列名不叫 Positive Markers。\n",
         "  不猜、不兜底：解析失败比解析出一张错表安全。", call. = FALSE)
  if (header_partial)
    stop("ref_table_parse(): 找到 'Positive Markers' 列，但没有 'Subclass (Full Name)' 全称列，\n",
         "  无法确定类型全称，拒绝继续。表头行: ", header_row, call. = FALSE)

  hdr <- row_cells(header_row)
  cols <- list(full = col_find(hdr, "full"), level3 = col_find(hdr, "level3"),
               level1 = col_find(hdr, "level1"), pos = col_find(hdr, "pos"),
               neg = col_find(hdr, "neg"), refs = col_find(hdr, "refs"))
  if (is.na(cols$level3))
    stop("ref_table_parse(): 表头里没有 'Subclass Level 3' 列，无法按层级取 key。\n",
         "  不做「只有全称就按全称合并」的兜底：那正是把不同层级混成一个类型的成因。",
         call. = FALSE)

  # ---------------- 2) 划出参考文献区（按内容识别，不按行号） ----------------
  is_ref_start <- function(cells) {
    v <- chr0(cells)
    v <- v[!is.na(v) & nzchar(v)]
    if (!length(v)) return(FALSE)
    s <- v[1]
    grepl("^references?\\b", s, ignore.case = TRUE) ||
      grepl("^参考文献", s) ||
      grepl("^additional resources?\\b", s, ignore.case = TRUE) ||
      grepl("^supplementary (note|notes|table)\\b", s, ignore.case = TRUE) ||
      grepl("^notes?\\b", s, ignore.case = TRUE) ||
      grepl("^[0-9]{1,3}[:.]\\s*\\S", s)   # "1: Lake et al. ..." 这类编号文献行
  }

  ref_block_row <- NA_integer_
  if (header_row < n_row) {
    for (i in (header_row + 1L):n_row) {
      if (is_ref_start(row_cells(i))) { ref_block_row <- i; break }
    }
  }
  data_end <- if (is.na(ref_block_row)) n_row else ref_block_row - 1L
  data_scan <- if (data_end >= header_row + 1L) (header_row + 1L):data_end else integer(0)

  # ---------------- 3) 逐行解析 + 同 key 同全称合并 ----------------
  entries <- list()
  ord_keys <- character(0)
  conflicts <- list()
  skipped_blank <- integer(0)
  skipped_no_type <- integer(0)

  for (i in data_scan) {
    full <- cell(i, cols$full)
    l3   <- cell(i, cols$level3)
    l1   <- cell(i, cols$level1)
    if (!nzchar(full) && !nzchar(l3)) { skipped_blank <- c(skipped_blank, i); next }
    key <- if (nzchar(l3)) l3 else full
    if (!nzchar(key)) { skipped_no_type <- c(skipped_no_type, i); next }

    pos <- split_markers(cell(i, cols$pos))
    neg <- if (is.na(cols$neg)) character(0) else split_markers(cell(i, cols$neg))
    refs <- if (is.na(cols$refs)) character(0) else split_markers(cell(i, cols$refs))
    fname <- if (nzchar(full)) full else NA_character_

    if (is.null(entries[[key]])) {
      entries[[key]] <- list(
        key         = key,
        full_name   = fname,
        level3      = if (nzchar(l3)) l3 else NA_character_,
        level1      = if (nzchar(l1)) l1 else NA_character_,
        level1_all  = if (nzchar(l1)) l1 else character(0),
        positive    = if (length(pos)) pos else NA_character_,
        negative    = if (length(neg)) neg else NA_character_,
        refs        = refs,
        source_rows = as.integer(i),
        reason      = if (length(pos)) "" else "Positive Markers 列为空：保留该类型，但不参与打分"
      )
      ord_keys <- c(ord_keys, key)
      next
    }

    e <- entries[[key]]
    same_name <- identical(nows(e$full_name), nows(fname)) ||
      (is.na(e$full_name) && is.na(fname))
    if (!same_name) {
      # 同一个 key 全称不同：不静默合并，也不静默丢弃。
      # 冲突双方的 key、全称、source_rows（Excel 原行号）都记进 meta$conflicts，
      # 由调用方取全信息后显式决定（本函数不替人做合并决定）。
      conflicts[[length(conflicts) + 1L]] <- list(
        key             = key,
        full_name_kept  = e$full_name,
        full_name_other = fname,
        source_rows_kept  = as.integer(e$source_rows),
        source_rows_other = as.integer(i),
        positive_other  = pos,
        action = "两条来源都保留在 meta$conflicts；本行未合并进 entries，调用方需显式决定")
      next
    }

    e$positive <- if (is.na(e$positive[1]) && !length(pos)) NA_character_ else
      unique(c(if (is.na(e$positive[1])) character(0) else e$positive, pos))
    if (is.na(e$positive[1])) e$positive <- NA_character_
    e$negative <- if (is.na(e$negative[1]) && !length(neg)) NA_character_ else
      unique(c(if (is.na(e$negative[1])) character(0) else e$negative, neg))
    if (is.na(e$negative[1])) e$negative <- NA_character_
    e$refs        <- unique(c(e$refs, refs))
    e$source_rows <- c(e$source_rows, as.integer(i))
    e$level1_all  <- unique(c(e$level1_all, if (nzchar(l1)) l1 else character(0)))
    # 合并后有了 marker，就不能再留着"列为空"的说明，否则 reason 会撒谎。
    if (!is.na(e$positive[1])) e$reason <- ""
    entries[[key]] <- e
  }

  if (keep_unmarked) {
    # 默认保留：一个类型没有 marker 是表的事实，丢掉它会让"表里有多少类型"对不上账
  } else {
    entries <- entries[!vapply(entries, function(e) is.na(e$positive[1]), TRUE)]
    ord_keys <- intersect(ord_keys, names(entries))
  }
  entries <- entries[ord_keys]

  # ---------------- 4) meta ----------------
  n_rows_vec <- vapply(entries, function(e) length(e$source_rows), integer(1))
  unmarked <- names(entries)[vapply(entries, function(e) is.na(e$positive[1]), TRUE)]
  col_letters <- lapply(cols, function(j) if (is.na(j)) NA_character_ else excel_col(j))

  list(
    entries = entries,
    meta = list(
      parser          = "ref_table_parse",
      parser_version  = REF_TABLE_PARSER_VERSION,
      path            = normalizePath(path, mustWork = FALSE),
      size_bytes      = unname(file.size(path)),
      md5             = unname(tools::md5sum(path)),
      sheet_names     = sheet_names,
      sheet           = sheet_used,
      n_rows_raw      = n_row,
      n_cols_raw      = n_col,
      readxl_default_dim = as.integer(.default_dim),
      header_row      = as.integer(header_row),
      # 能走到这里说明表头是完整的；留个自检字段，便于调用方核对"部分表头未误判"。
      header_partial  = header_partial,
      header          = hdr,
      columns         = cols,
      column_letters  = col_letters,
      row_basis       = "Excel 原行号（读取显式锚定 A1，不裁掉顶部空行空列，可直接对照 Excel）",
      data_rows       = as.integer(data_scan),
      n_scanned_rows  = length(data_scan),
      n_data_rows     = length(data_scan) - length(skipped_blank) - length(skipped_no_type),
      ref_block_row   = if (is.na(ref_block_row)) NA_integer_ else as.integer(ref_block_row),
      skipped_blank_rows   = skipped_blank,
      skipped_typeless_rows = skipped_no_type,
      n_entries       = length(entries),
      n_keys          = length(unique(names(entries))),
      n_unmarked      = length(unmarked),
      unmarked_keys   = unmarked,
      merged_keys     = names(entries)[n_rows_vec > 1L],
      conflicts       = conflicts,
      n_conflicts     = length(conflicts),
      # 明确标志：出现同 key 不同全称的冲突即为 TRUE。
      # 本函数不替人做合并决定，调用方看到 TRUE 必须显式决定（拒绝或人工裁定），
      # 不允许"日志里有一行、产物里什么都没有"的静默丢行。
      has_conflict    = length(conflicts) > 0L,
      conflict_keys   = unique(vapply(conflicts, function(z) z$key, character(1))),
      kept_unmarked   = keep_unmarked,
      parsed_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    )
  )
}
