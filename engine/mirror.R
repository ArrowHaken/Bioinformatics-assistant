# =====================================================================
#  mirror.R -- 把服务器上的产物取回应用，让人在应用里直接看
#
#  为什么需要这一层
#    应用跑在公网这一侧，数据在对方内网。到目前为止，产物只能在
#    应用里"列出来"：文件名、大小、时间，然后标一句「需从服务器取」。
#    人要判断这一步做得对不对，必须自己登服务器、或者让助手传文件 ——
#    于是"应用里跑通了"和"我能看见结果"是两件事。
#
#  做法
#    应用不能主动连服务器，只能反过来：应用登记"想取哪个文件"，
#    执行端下一轮来取任务时看到清单，下载后回传，应用落到本地镜像目录。
#    之后同一个文件就从本地读，不再需要服务器。
#
#  几个刻意的选择
#    · 只镜像"值得看"的类型（图 / PDF / 表格 / 报告），不镜像 rds 与 xlsx 大表。
#      rds 几百兆，镜像过来也只能看大小，占满磁盘没有意义。
#    · 大小上限单文件 40MB。超过的不镜像，明说"太大，请从服务器取"。
#    · 落盘用 md5 核对。对不上就删掉、报错 ——
#      "大概是同一份"这种话在结果核查里等于没有。
#    · 镜像目录按分析记录分开，不共用：两批数据同名文件很常见
#      （都叫 umap_cluster.png），共用一个目录会互相覆盖。
# =====================================================================

MIRROR_ROOT <- local({
  # 同上：走统一开关，写成整块避免顶层 else 的语法坑。
  if (exists("state_private_root", mode = "function")) {
    file.path(state_private_root(dirname(STATE_DIR)), "mirror")
  } else {
    file.path(STATE_DIR, "mirror")
  }
})
MIRROR_REQ_FILE <- file.path(STATE_DIR, "artifact_requests.json")

# 值得取回来看的类型。键是扩展名，值是给人看的中文说法。
#
#  为什么必须扩：这份白名单原来只有图 / PDF / 表格 / 说明，
#  恰好漏掉了分析结果和交付物本身 —— rds（分析对象）、pptx（交付 PPT）、
#  zip（整包）。也就是说「取不回结果」，只能取回结果的截图。
#
#  分类与 artifacts.kind 对齐（A3），决定取回门槛：
#    preview  图 / 表 / 日志   直接取，不需要确认
#    large    rds / zip / pptx 允许取，但必须用户显式点击（体积大）
#    secret   含样本标识的原始表 默认拒绝，需授权
MIRROR_KINDS <- list(
  ".png"  = "图",
  ".jpg"  = "图",
  ".jpeg" = "图",
  ".svg"  = "图",
  ".pdf"  = "PDF",
  ".xls"  = "表格",
  ".xlsx" = "表格",
  ".csv"  = "表格",
  ".tsv"  = "表格",
  ".md"   = "说明",
  ".txt"  = "说明",
  ".html" = "报告",
  ".json" = "清单",
  ".rds"  = "分析对象",
  ".pptx" = "交付 PPT",
  ".zip"  = "整包"
)

# 小文件走 base64 一次带完；大文件走分片（见 db_api.R 的 /api/artifact/chunk）。
#   为什么不再用一个全局 40 MB 上限：40 MB 对表格够用，对 1 GB 的 rds 是死路，
#   而 1 GB 的 merge 产物恰好是最需要取回来看的那一件。
MIRROR_MAX_BYTES <- 8 * 1024 * 1024        # base64 通道单次上限
MIRROR_LARGE_BYTES <- 4 * 1024 * 1024 * 1024  # 分片通道上限（4 GB）

# 大文件必须显式点击才取：不弹确认就自动拉 1 GB，用户会以为页面卡死。
MIRROR_LARGE_EXTS <- c("rds", "zip", "pptx", "tar", "gz", "xz")

# 这些类默认不放行，要单独授权（含样本标识的原始矩阵最容易泄露受试者信息）。
MIRROR_SECRET_PATTERNS <- c("^raw/", "^matrix/", "^data/", "sample_sheet")

mirror_safe <- function(x) {
  s <- as.character(x %||% "")
  s <- gsub("\\\\", "/", s)
  gsub("[^A-Za-z0-9._/-]", "_", s)
}

mirror_dir <- function(sid) {
  s <- mirror_safe(if (nzchar(as.character(sid %||% ""))) sid else "_default")
  s <- gsub("/", "_", s)
  file.path(MIRROR_ROOT, s)
}

# 相对路径 -> 本地镜像文件的绝对路径。
mirror_path <- function(sid, rel) {
  r <- mirror_safe(rel)
  # 不许 ../ 逃出镜像目录：这是外部给进来的字符串，必须挡住。
  if (grepl("\\.\\.", r)) return("")
  file.path(mirror_dir(sid), r)
}

mirror_has <- function(sid, rel) {
  p <- mirror_path(sid, rel)
  nzchar(p) && file.exists(p)
}

mirror_kind <- function(rel) {
  ext <- tolower(paste0(".", tools::file_ext(as.character(rel %||% ""))))
  k <- MIRROR_KINDS[[ext]]
  if (is.null(k)) "" else k
}

mirror_can <- function(rel, bytes = NULL) {
  if (!nzchar(mirror_kind(rel))) return(FALSE)
  if (!is.null(bytes) && is.finite(suppressWarnings(as.numeric(bytes))) &&
      as.numeric(bytes) > MIRROR_MAX_BYTES) return(FALSE)
  TRUE
}

mirror_put <- function(sid, rel, b64, want_md5 = "", bytes = NULL) {
  if (!nzchar(mirror_kind(rel)))
    return(list(ok = FALSE, error = "这个类型不取回（只取图 / PDF / 表格 / 说明）"))
  raw <- tryCatch(jsonlite::base64_dec(as.character(b64 %||% "")), error = function(e) NULL)
  if (is.null(raw) || !length(raw)) return(list(ok = FALSE, error = "内容为空或不是 base64"))
  if (length(raw) > MIRROR_MAX_BYTES)
    return(list(ok = FALSE, error = paste0("超过单文件上限（", round(length(raw) / 1048576), "MB）")))

  p <- mirror_path(sid, rel)
  if (!nzchar(p)) return(list(ok = FALSE, error = "路径不合法"))
  dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE)
  con <- file(p, open = "wb")
  writeBin(raw, con)
  close(con)

  got <- unname(tools::md5sum(p))
  if (nzchar(as.character(want_md5 %||% "")) && !identical(got, as.character(want_md5))) {
    unlink(p)
    return(list(ok = FALSE, error = paste0("md5 对不上（服务器 ", want_md5, "，落地 ", got, "），已删除")))
  }
  list(ok = TRUE, rel = as.character(rel), bytes = length(raw), md5 = got,
       path = p, stored_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
}

# ---- 待取清单 --------------------------------------------------------
mirror_req_read <- function() {
  if (!file.exists(MIRROR_REQ_FILE)) return(list(items = list()))
  d <- tryCatch(jsonlite::fromJSON(read_text_any(MIRROR_REQ_FILE), simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(d) || is.null(d$items)) return(list(items = list()))
  d
}

mirror_req_write <- function(d) {
  dir.create(dirname(MIRROR_REQ_FILE), showWarnings = FALSE, recursive = TRUE)
  d$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  write_json_file(MIRROR_REQ_FILE, d)
}

# 登记一个"想看"的文件。同一条重复登记不新增，只把时间往前推。
mirror_request <- function(sid, rel, bytes = NULL, run_dir = "") {
  if (!nzchar(mirror_kind(rel)))
    return(list(ok = FALSE, error = "这个类型不支持在应用里查看"))
  if (mirror_has(sid, rel))
    return(list(ok = TRUE, ready = TRUE, rel = as.character(rel)))
  d <- mirror_req_read()
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  hit <- FALSE
  for (i in seq_along(d$items)) {
    it <- d$items[[i]]
    if (identical(as.character(it$session %||% ""), as.character(sid)) &&
        identical(as.character(it$rel %||% ""), as.character(rel))) {
      d$items[[i]]$requested_at <- now
      d$items[[i]]$done <- NULL
      hit <- TRUE
    }
  }
  if (!hit) {
    d$items[[length(d$items) + 1]] <- list(
      session = as.character(sid), rel = as.character(rel),
      bytes = bytes, run_dir = as.character(run_dir %||% ""),
      requested_at = now, done = NULL)
  }
  mirror_req_write(d)
  list(ok = TRUE, ready = FALSE, rel = as.character(rel),
       msg = "已登记，等执行端取回")
}

# 执行端来取：给它还没取回的清单。
mirror_pending <- function(limit = 12) {
  d <- mirror_req_read()
  out <- Filter(function(it) is.null(it$done), d$items %||% list())
  if (length(out) > limit) out <- out[seq_len(limit)]
  lapply(out, function(it) list(session = it$session, rel = it$rel,
                                bytes = it$bytes, run_dir = it$run_dir))
}

mirror_done <- function(sid, rel) {
  d <- mirror_req_read()
  for (i in seq_along(d$items)) {
    if (identical(as.character(d$items[[i]]$session %||% ""), as.character(sid)) &&
        identical(as.character(d$items[[i]]$rel %||% ""), as.character(rel))) {
      d$items[[i]]$done <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    }
  }
  mirror_req_write(d)
}

# ---- 表格预览：读前 N 行，转成 JSON 给前端渲染 ------------------------
#   .xls 在这里多半是制表符分隔的文本（脚本用 write.table(sep="\t") 写的），
#   不是真正的 Excel 二进制。所以先按文本试，失败再走 readxl。
mirror_table_preview <- function(path, max_rows = 200L, max_cols = 40L) {
  if (!file.exists(path)) return(list(ok = FALSE, error = "文件不在"))
  ext <- tolower(tools::file_ext(path))
  read_tsv <- function() {
    d <- tryCatch(utils::read.delim(path, nrows = max_rows + 1L,
                                    check.names = FALSE, stringsAsFactors = FALSE,
                                    quote = "", comment.char = ""),
                  error = function(e) NULL)
    d
  }
  d <- NULL
  if (ext %in% c("csv", "tsv", "txt", "xls")) d <- read_tsv()
  if (is.null(d) && ext %in% c("xlsx", "xls") && requireNamespace("readxl", quietly = TRUE)) {
    d <- tryCatch(as.data.frame(
      readxl::read_excel(path, n_max = max_rows + 1L, col_names = TRUE),
      stringsAsFactors = FALSE), error = function(e) NULL)
  }
  if (is.null(d) || !ncol(d))
    return(list(ok = FALSE, error = "读不出这张表（可能不是文本表格，或编码不认识）"))
  truncated_cols <- ncol(d) > max_cols
  if (truncated_cols) d <- d[, seq_len(max_cols), drop = FALSE]
  truncated_rows <- nrow(d) > max_rows
  if (truncated_rows) d <- d[seq_len(max_rows), , drop = FALSE]
  cells <- lapply(d, function(col) {
    v <- as.character(col)
    v[is.na(v)] <- ""
    v
  })
  list(ok = TRUE,
       columns = as.list(names(d)),
       rows = unname(cells),
       nrow_shown = nrow(d),
       ncol_shown = ncol(d),
       truncated = truncated_rows || truncated_cols,
       note = if (truncated_rows || truncated_cols)
         paste0("只显示前 ", nrow(d), " 行", if (truncated_cols) paste0("、前 ", max_cols, " 列") else "")
       else "")
}
