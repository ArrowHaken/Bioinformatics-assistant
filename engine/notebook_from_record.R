#!/usr/bin/env Rscript
# =====================================================================
#  notebook_from_record.R -- build an ipynb from the scripts a record
#  actually ran
#
#  WHY THIS EXISTS
#    Two buttons in the page were dead ends:
#      「执行快照 -> 生成执行快照」 and 「打包notebook -> 生成 notebook」
#    Both call /api/notebook/build, which needs
#    state/notebooks/<sid>/status/cells.jsonl -- a ledger only the agent
#    can push, and only for local-mode records (nb_sync_local returns
#    FALSE for mode=server). For a server-mode record that file never
#    exists, so both buttons answered "no_cells_file". From the outside
#    that reads as a broken feature, not as "this run had no ledger".
#
#  WHAT IT DOES INSTEAD
#    A verified step attempt already carries step_id, script_path and
#    script_digest. Those are facts about what really ran. So when there
#    is no ledger, the notebook is assembled from those facts instead of
#    failing. Nothing is invented:
#      - a script whose content cannot be found byte-for-byte is listed
#        as a heading plus an explicit note, never as fake code;
#      - ordering follows the workflow order, then attempt order.
#
#  FORMAT
#    Mirrors zjz's template (scRNA_analysis_GPU.ipynb):
#      "# <main title>"
#      "## NN. <English>｜<Chinese>"
#    Same heading style and same "｜" separator, nbformat 4.5.
#
#  ASCII-only code identifiers; wording lives in the cells.
# =====================================================================

if (!exists("%||%", mode = "function"))
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# Section numbering and names. Order is the workflow order; env comes first
# because that is where a real run starts.
nfr_sections <- function() {
  list(
    env      = list(no = "01", en = "ImportPackages",   cn = "环境体检与通用初始化"),
    doublet  = list(no = "02", en = "DoubletQC",        cn = "逐样本去双与质控"),
    merge    = list(no = "03", en = "MergeCluster",     cn = "合并与批次校正"),
    markers  = list(no = "04", en = "ClusterMarker",    cn = "一次聚类 marker"),
    filter   = list(no = "05", en = "FilterRecluster",  cn = "筛群与剔样本后重聚类"),
    final    = list(no = "06", en = "CellRatio",        cn = "终版比例统计"),
    fullmark = list(no = "07", en = "FullMarker",       cn = "全量 marker"),
    diff     = list(no = "08", en = "DiffEnrich",       cn = "组间差异与富集"),
    annotate = list(no = "09", en = "Annotation",       cn = "细胞类型注释"),
    figs     = list(no = "10", en = "Figures",          cn = "出图"),
    enrich   = list(no = "11", en = "GOEnrich",         cn = "GO 富集"),
    ppt      = list(no = "12", en = "PPT",              cn = "生成交付 PPT"),
    pack     = list(no = "13", en = "Pack",             cn = "打包交付物"))
}

nfr_order <- function() names(nfr_sections())
nfr_rank <- function(stage) {
  k <- match(as.character(stage %||% ""), nfr_order())
  if (is.na(k)) 99L else k
}

# Language of a script, decided by extension only.
nfr_lang <- function(path) {
  ext <- tolower(tools::file_ext(as.character(path %||% "")))
  if (ext %in% c("r")) return("R")
  if (ext %in% c("py")) return("python")
  if (ext %in% c("sh", "bash")) return("bash")
  "python"
}

# Verified step attempts for one analysis record, oldest first.
#   Only verified rows: a rejected attempt is evidence of a try, not of the
#   script that produced the accepted result. A run that was rejected and
#   then fixed should export the fixed one.
nfr_attempt_rows <- function(con, sid) {
  if (is.null(con)) return(NULL)
  tryCatch(DBI::dbGetQuery(con,
    "SELECT a.step_id AS step_id, a.script_path AS script_path,
            a.script_digest AS script_digest, a.status AS status,
            a.reported_at AS reported_at, a.rowid AS rowid
       FROM step_attempts a
       JOIN runs r ON r.run_id = a.run_id
       JOIN sessions s ON s.run_dir = r.work_root
      WHERE s.session_id = ? AND a.status = 'verified'
      ORDER BY a.rowid ASC", list(as.character(sid))),
    error = function(e) NULL)
}

# Resolve a script's bytes.
#   Order matters: the archive copy is what was registered for this record;
#   the engine copy is the shipped one. md5 must match when we have one, and
#   we never return content whose digest disagrees with what the runner
#   reported.
nfr_content <- function(engine_dir, archive_root, remote_path, want_md5) {
  want <- as.character(want_md5 %||% "")
  want <- if (!length(want) || is.na(want[1])) "" else tolower(trimws(want[1]))
  if (identical(want, "na")) want <- ""
  bn <- basename(gsub("\\\\", "/", as.character(remote_path %||% "")))
  cands <- character(0)
  if (nzchar(archive_root) && dir.exists(archive_root) && nzchar(bn)) {
    cands <- c(cands, list.files(archive_root, pattern = paste0("^", gsub("\\.", "\\\\.", bn), "$"),
                                 recursive = TRUE, full.names = TRUE))
  }
  if (exists("sa_local_candidates", mode = "function"))
    cands <- c(cands, sa_local_candidates(engine_dir, remote_path))
  cands <- unique(cands[nzchar(cands)])
  if (!length(cands)) return(list(ok = FALSE, why = "no_local_copy"))
  if (nzchar(want)) {
    for (p in cands) {
      d <- tryCatch(unname(tools::md5sum(p)), error = function(e) "")
      if (identical(tolower(as.character(d)), want)) return(list(ok = TRUE, path = p))
    }
    return(list(ok = FALSE, why = "digest_mismatch"))
  }
  list(ok = TRUE, path = cands[1])
}

# Dependency note, same shape as nb_dependency_cell so both notebooks read alike.
nfr_dependency_md <- function(has_r, has_bash) {
  lines <- c("## 运行前提", "",
             "- 内核：Python 3（本文件用 ipynb 标准格式，nbformat 4.5）", "")
  if (isTRUE(has_r))
    lines <- c(lines,
      "- R 段落用 `%%R` 魔法执行，需安装 R 与 rpy2：`pip install rpy2`；",
      "- 第一段代码 cell 会加载 `%load_ext rpy2.ipython`。",
      "- 没有 rpy2 时 R 段落会直接失败，不会静默跳过。", "")
  if (isTRUE(has_bash))
    lines <- c(lines, "- shell 段落用 `%%bash` 执行，Windows 上不可用。", "")
  c(lines,
    "- 本文件由「脚本数据库」按记录实际登记过的脚本汇编，只含脚本正文。",
    "- 路径与参数在这些脚本内部，换机器运行前需按实际环境核对。", "")
}

# Assemble cells. Each entry: list(step=, title=, code=, lang=, missing=)
nfr_cells <- function(items, main_title, subtitle = NULL) {
  cells <- list()
  cells[[length(cells) + 1L]] <- nb_md_cell(paste0("# ", main_title))
  if (!is.null(subtitle) && nzchar(subtitle))
    cells[[length(cells) + 1L]] <- nb_md_cell(subtitle)

  langs <- unique(vapply(items, function(x) as.character(x$lang %||% "python"), ""))
  has_r <- any(toupper(langs) == "R")
  has_bash <- any(tolower(langs) == "bash")
  cells[[length(cells) + 1L]] <- nb_md_cell(paste(nfr_dependency_md(has_r, has_bash), collapse = "\n"))

  if (has_r)
    cells[[length(cells) + 1L]] <- nb_code_cell("%load_ext rpy2.ipython\n", "python", "python3")

  secs <- nfr_sections()
  for (it in items) {
    st <- as.character(it$step %||% "")
    sec <- secs[[st]]
    head <- if (!is.null(sec))
      paste0("## ", sec$no, ". ", sec$en, "\uFF5C", sec$cn)
    else
      paste0("## ", as.character(it$title %||% st))
    body <- head
    if (nzchar(as.character(it$script_name %||% "")))
      body <- paste0(body, "\n\n", "`", as.character(it$script_name), "`")
    cells[[length(cells) + 1L]] <- nb_md_cell(body)
    if (isTRUE(it$missing)) {
      cells[[length(cells) + 1L]] <- nb_md_cell(paste0(
        "> 这一段只登记了来源路径与指纹，没有取到逐字节一致的正文，因此这里不放代码。",
        "\n>\n> 来源：`", as.character(it$source_path %||% ""), "`",
        if (nzchar(as.character(it$digest %||% "")))
          paste0("\n> 指纹：`", as.character(it$digest), "`") else ""))
    } else {
      cells[[length(cells) + 1L]] <- nb_code_cell(it$code %||% "",
                                                  as.character(it$lang %||% "python"),
                                                  "python3")
    }
  }
  cells
}

# Write the notebook object. Same writer as notebook_build so both paths
# produce identical JSON framing.
nfr_write <- function(nb, path) nb_write(nb, path)

# Metadata block: mirrors the template's kernelspec block, and records
# where this notebook came from so it is not mistaken for a replay proof.
nfr_metadata <- function(items, source_label) {
  list(
    # kernelspec 与 zjz 那份模板逐字段对齐：display_name 与 name 都照抄，
    #   这样两份 notebook 在 Jupyter 里显示同一个内核名，能并排对照。
    kernelspec = list(display_name = "Python (RAPIDS GPU)", language = "python",
                      name = "rapids-sc"),
    language_info = list(name = "python", version = "3"),
    pa = list(schema = "pa.notebook.v1",
              built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
              source = as.character(source_label %||% ""),
              assembled_from = "script archive (registered scripts only)",
              full_analysis_replay_verified = FALSE,
              code_cells = sum(vapply(items, function(x) !isTRUE(x$missing), TRUE)),
              missing_cells = sum(vapply(items, function(x) isTRUE(x$missing), TRUE)),
              langs = as.list(unique(vapply(items, function(x) as.character(x$lang %||% "python"), "")))))
}

# ---- entry 1: from one archive record (the per-record download button) ----
#   items come straight from archive/index.json, so what is exported is
#   exactly what the record says ran.
nfr_from_archive <- function(engine_dir, record, main_title = "\u5355\u7ec6\u80de\u5206\u6790\u6d41\u7a0b") {
  files <- record$files %||% list()
  if (!length(files))
    return(list(ok = FALSE, code = "empty_record", error = "\u8fd9\u6761\u8bb0\u5f55\u6ca1\u6709\u767b\u8bb0\u811a\u672c"))
  rid <- as.character(record$id %||% "")
  arc_root <- file.path(engine_dir, "library", "archive", rid)

  # One item per (stage, name). A record can carry two versions of the same
  # name with different digests; both are kept because both really ran.
  # One item per stage, matching what the page shows.
  #   A record can carry two versions of the same stage (a rejected run then a
  #   fixed one). Exporting both produces two "## 12. PPT" headings, which
  #   reads like a mistake. Keep the one with content; among those, the later
  #   registration wins -- it is the one that actually produced the result.
  items <- list()
  for (f in files) {
    st <- as.character(f$stage %||% "")
    if (!nzchar(st)) st <- "env"
    rel <- as.character(f$rel %||% "")
    nm <- as.character(f$name %||% basename(rel))
    fp <- if (nzchar(rel)) file.path(arc_root, rel) else ""
    has_file <- nzchar(fp) && file.exists(fp)
    code <- ""
    if (isTRUE(has_file)) {
      code <- tryCatch(rawToChar(readBin(fp, "raw", n = file.size(fp))),
                       error = function(e) "")
      if (!validUTF8(code)) code <- iconv(code, from = "GBK", to = "UTF-8", sub = "?")
    }
    item <- list(
      step = st, title = nm, script_name = nm,
      source_path = as.character(f$source_path %||% rel),
      digest = as.character(f$md5 %||% ""),
      lang = nfr_lang(nm), code = code,
      missing = !nzchar(code))
    prev <- items[[st]]
    if (is.null(prev)) { items[[st]] <- item; next }
    # 有正文的版本优先；同为有正文（或同为无正文）时后者覆盖前者。
    if (isTRUE(prev$missing) || !isTRUE(item$missing)) items[[st]] <- item
  }
  items <- unname(items)
  # Workflow order, env first; stable within a stage.
  ord <- order(vapply(items, function(x) nfr_rank(x$step), 1L),
               seq_along(items))
  items <- items[ord]

  sub <- paste0("> \u8bb0\u5f55\uff1a", as.character(record$name %||% rid),
                " \uFF5C \u65e5\u671f\uff1a", as.character(record$when %||% ""),
                "\n>\n> \u53ea\u6c47\u7f16\u8fd9\u6761\u8bb0\u5f55\u91cc\u767b\u8bb0\u8fc7\u3001\u4e14\u5b58\u6863\u4e2d\u6709\u6b63\u6587\u7684\u811a\u672c\u3002",
                "\u6ca1\u6709\u6b63\u6587\u7684\u53ea\u5217\u6807\u9898\u5e76\u6807\u660e\u7f3a\u5931\u3002")
  nb <- list(cells = nfr_cells(items, main_title, sub),
             metadata = nfr_metadata(items, paste0("archive:", rid)),
             nbformat = 4L, nbformat_minor = 5L)
  for (i in seq_along(nb$cells)) nb$cells[[i]]$id <- paste0("pa-cell-", i)
  list(ok = TRUE, notebook = nb, items = length(items),
       missing = sum(vapply(items, function(x) isTRUE(x$missing), TRUE)),
       readable = sum(vapply(items, function(x) !isTRUE(x$missing), TRUE)))
}

# ---- entry 2: from a session's verified attempts (fixes no_cells_file) ----
#   Returns cells in the ledger shape so nb_build can consume them, which
#   keeps one assembler instead of two.
nfr_cells_for_session <- function(app_dir, engine_dir, con, sid) {
  rows <- nfr_attempt_rows(con, sid)
  if (is.null(rows) || !nrow(rows)) return(list(ok = FALSE, code = "no_attempts"))
  rec <- tryCatch(sess_find(app_dir, sid), error = function(e) NULL)
  arc_root <- if (exists("sa_record_for", mode = "function"))
    file.path(engine_dir, "library", "archive", paste0("SA-", sa_safe(as.character(sid), "unknown")))
  else ""

  seen <- character(0)
  cells <- list()
  for (i in seq_len(nrow(rows))) {
    r <- rows[i, ]
    st <- as.character(r$step_id %||% "")
    sp <- as.character(r$script_path %||% "")
    dg <- as.character(r$script_digest %||% "")
    if (!nzchar(st) || is.na(sp) || !nzchar(sp)) next
    if (!grepl("\\.(R|r|py|sh|pl)$", sp)) next
    key <- paste(st, tolower(if (identical(dg, "NA")) "" else dg), basename(sp), sep = "\u0001")
    if (key %in% seen) next
    seen <- c(seen, key)

    got <- nfr_content(engine_dir, arc_root, sp, dg)
    nm <- basename(gsub("\\\\", "/", sp))
    missing <- !isTRUE(got$ok)
    code <- if (isTRUE(got$ok))
      tryCatch(paste(readLines(got$path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
               error = function(e) "") else ""
    if (nzchar(code) == FALSE && !missing) missing <- TRUE
    secs <- nfr_sections()
    sec <- secs[[st]]
    # 标题按模板写成「01. ImportPackages｜通用初始化」：
    #   nb_build 的 nb_heading_text 会把 level + step + ". " + title 拼起来，
    #   所以这里 step 放章节编号、title 放「英文名｜中文名」。
    #   以前 step 放的是阶段名（doublet），拼出来是「## doublet. DoubletQC｜…」，
    #   跟模板对不上，也没了章节序号。
    sec_no <- if (!is.null(sec)) sec$no else st
    title <- if (!is.null(sec)) paste0(sec$en, "\uFF5C", sec$cn) else st
    note <- if (isTRUE(missing))
      paste0("\u672c\u6bb5\u53ea\u767b\u8bb0\u4e86\u6765\u6e90\u4e0e\u6307\u7eb9\uff08",
             as.character(dg), "\uff09\uff0c\u6ca1\u6709\u53d6\u5230\u9010\u5b57\u8282\u4e00\u81f4\u7684\u6b63\u6587\uff0c",
             "\u56e0\u6b64\u8fd9\u91cc\u4e0d\u653e\u4ee3\u7801\u3002\u6765\u6e90\uff1a", sp) else ""
    cells[[length(cells) + 1L]] <- list(
      seq = length(cells) + 1L, level = 2L, step = sec_no, title = title,
      note = note, lang = nfr_lang(nm), code = code,
      event_id = paste0("fromrecord-", st, "-", substr(gsub("[^A-Za-z0-9]", "", key), 1, 12)),
      cell_type = "code")
  }
  if (!length(cells)) return(list(ok = FALSE, code = "no_script_content"))
  list(ok = TRUE, cells = cells, session_name = as.character((rec %||% list())$name %||% ""))
}
