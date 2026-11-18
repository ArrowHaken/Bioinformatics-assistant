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

# ---- 缓存有效性：必须绑到"最新已验收的那一份" --------------------------
#   实测事故：同 session 同一 rel 重新生成后，镜像目录里还躺着上一次的文件，
#   而旧判据只看文件在不在，于是列表写已取回、真下载拿到旧版（11,788,069 字节），
#   新验收那份（12,023,784 字节）根本下不到。
#   三条硬约束：
#     一，大小与 mtime 都不算判据 —— 重生成完全可能同大小；只认内容 md5。
#     二，sidecar（.md5）只是缓存，不能盲信：文件可能被单独替换而指纹没更新，
#         所以每次判定都实算一次当前文件 md5，和基准比。
#     三，拿不到最新基准 md5 时，不能"缺基准就放行"，必须报未核实、不可下载。
mirror_sidecar <- function(p) paste0(p, ".md5")

mirror_record_md5 <- function(p, md5) {
  ok <- tryCatch({ writeLines(as.character(md5), mirror_sidecar(p)); TRUE },
                 error = function(e) FALSE)
  invisible(ok)
}

mirror_stored_md5 <- function(p) {
  s <- mirror_sidecar(p)
  if (!file.exists(s)) return("")
  v <- tryCatch(paste(readLines(s, warn = FALSE), collapse = ""), error = function(e) "")
  tolower(trimws(v))
}

mirror_actual_md5 <- function(p) {
  if (!nzchar(p) || !file.exists(p)) return("")
  tolower(unname(tools::md5sum(p)))
}

# 判定结果四态，调用方按 state 说话，不要自己再猜：
#   ok         内容与最新基准逐字相同，可下载
#   stale      文件在，但与最新基准不同 -> 旧缓存，挪去 _stale 后可重新取回
#   unverified 查不到合法基准 md5 -> 未核实，不给下载
#   missing    盘上没有这个文件
mirror_validity <- function(sid, rel, want_md5 = "") {
  p <- mirror_path(sid, rel)
  if (!nzchar(p) || !file.exists(p))
    return(list(ok = FALSE, state = "missing", actual = "", want = ""))
  want <- tolower(trimws(as.character(want_md5 %||% "")))
  if (!grepl("^[0-9a-f]{32}$", want))
    return(list(ok = FALSE, state = "unverified", actual = "", want = ""))
  actual <- mirror_actual_md5(p)
  if (!nzchar(actual))
    return(list(ok = FALSE, state = "missing", actual = "", want = want))
  if (!identical(actual, want))
    return(list(ok = FALSE, state = "stale", actual = actual, want = want))
  # 内容与基准一致：顺手把 sidecar 校正成基准值，别留下过期指纹。
  if (!identical(mirror_stored_md5(p), want)) mirror_record_md5(p, want)
  list(ok = TRUE, state = "ok", actual = actual, want = want)
}

mirror_valid <- function(sid, rel, want_md5 = "")
  isTRUE(mirror_validity(sid, rel, want_md5)$ok)

# 失效的旧缓存不删，挪到 _stale 下留审计。
#   为什么留：它曾经是某次已验收产物的镜像，事后要能查当时下到的到底是什么。
mirror_quarantine <- function(sid, rel) {
  p <- mirror_path(sid, rel)
  if (!nzchar(p) || !file.exists(p)) return("")
  dst <- file.path(dirname(p), "_stale",
                   paste0(basename(p), ".", format(Sys.time(), "%Y%m%d-%H%M%S")))
  dir.create(dirname(dst), showWarnings = FALSE, recursive = TRUE)
  if (!isTRUE(file.rename(p, dst))) return("")
  sc <- mirror_sidecar(p)
  if (file.exists(sc)) tryCatch(file.rename(sc, paste0(dst, ".md5")),
                                error = function(e) NULL)
  dst
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
  want <- tolower(as.character(want_md5 %||% ""))
  # 小文件也必须先写临时件、核完再替换：直接写正式路径再 unlink，
  #   等于校验失败时把原本完好的旧文件一起删掉。
  if (!grepl("^[0-9a-f]{32}$", want))
    return(list(ok = FALSE, error = "缺合法 md5，拒收"))
  if (!is.null(bytes) && !is.na(suppressWarnings(as.numeric(bytes))) &&
      as.numeric(bytes) != length(raw))
    return(list(ok = FALSE, error = "字节数与登记值不符，拒收"))
  dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(p, ".incoming")
  con <- file(tmp, open = "wb")
  writeBin(raw, con)
  close(con)
  got <- unname(tools::md5sum(tmp))
  if (!identical(got, want)) {
    unlink(tmp)
    return(list(ok = FALSE, error = paste0("md5 对不上（服务器 ", want, "，落地 ", got, "），旧文件未动")))
  }
  if (!file.rename(tmp, p)) {
    unlink(tmp)
    return(list(ok = FALSE, error = "替换失败，旧文件未动"))
  }
  mirror_record_md5(p, got)
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
mirror_request <- function(sid, rel, bytes = NULL, run_dir = "", want_md5 = "", preserve_existing = FALSE, recover = FALSE, owner_key = "") {
  if (!nzchar(mirror_kind(rel)))
    return(list(ok = FALSE, error = "这个类型不支持在应用里查看"))
  if (isTRUE(preserve_existing)) {
    d <- mirror_req_read()
    for (i in seq_along(d$items)) {
      it <- d$items[[i]]
      if (identical(as.character(it$session %||% ""), as.character(sid)) &&
          identical(as.character(it$rel %||% ""), as.character(rel)) &&
          identical(it$status, "running"))
        return(list(ok = TRUE, ready = FALSE, rel = as.character(rel), preserved = TRUE,
                    deferred = "in_flight"))
      if (identical(as.character(it$session %||% ""), as.character(sid)) &&
          identical(as.character(it$rel %||% ""), as.character(rel)) &&
          identical(as.character(it$run_dir %||% ""), as.character(run_dir)) &&
          identical(as.character(it$want_md5 %||% ""), as.character(want_md5)) &&
          identical(as.character(it$bytes %||% ""), as.character(bytes %||% ""))) {
        valid <- mirror_valid(sid, rel, want_md5)
        # Recovery requires the caller's reviewed source. Never reset a live job.
        if (isTRUE(recover) && !valid && !identical(it$status, "running")) {
          failures <- suppressWarnings(as.integer(it$attempts %||% 0L))
          failed <- suppressWarnings(as.POSIXct(it$failed_at %||% "", format = "%Y-%m-%dT%H:%M:%S"))
          age <- as.numeric(difftime(Sys.time(), failed, units = "secs"))
          retry <- nzchar(as.character(it$error %||% "")) && !is.na(failures) &&
            failures < 3L && is.finite(age) && age >= min(300, 30 * 2^max(0, failures - 1L))
          if (!is.null(it$done) || retry) {
            d$items[[i]]$done <- NULL
            d$items[[i]]$error <- NULL
            d$items[[i]]$owner_key <- as.character(owner_key)
            mirror_req_write(d)
          }
        }
        return(list(ok = TRUE, ready = valid, rel = as.character(rel), preserved = TRUE))
      }
    }
  }
  v <- mirror_validity(sid, rel, want_md5)
  if (isTRUE(v$ok))
    return(list(ok = TRUE, ready = TRUE, rel = as.character(rel)))
  # 有文件但内容不是最新那一份：挪去 _stale 留审计，然后照常重新登记。
  if (identical(v$state, "stale")) {
    mv <- tryCatch(mirror_quarantine(sid, rel), error = function(e) "")
    if (nzchar(mv)) message("mirror_stale_quarantined ", rel, " -> ", mv)
  }
  d <- mirror_req_read()
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  hit <- FALSE
  for (i in seq_along(d$items)) {
    it <- d$items[[i]]
    if (identical(as.character(it$session %||% ""), as.character(sid)) &&
        identical(as.character(it$rel %||% ""), as.character(rel))) {
      # ---- 显式重新取回时，元数据必须换成"这一次要取的那一份" ----
      #   实测事故：同一 session 同一 rel 重新生成后再点取回，复用了首版那条记录，
      #   只清了 done/error、推了时间，bytes 还是 11,788,069；
      #   执行端于是按旧大小去核新文件（12,023,784），直接判"磁盘大小与登记不一致"失败。
      #   所以复用可以，但 bytes / run_dir / want_md5 必须按本次基准刷新。
      oldSnap <- list(
        bytes = d$items[[i]]$bytes,
        run_dir = d$items[[i]]$run_dir,
        want_md5 = d$items[[i]]$want_md5,
        done = d$items[[i]]$done,
        error = d$items[[i]]$error,
        requested_at = d$items[[i]]$requested_at)
      changed <- !identical(as.character(oldSnap$bytes %||% ""), as.character(bytes %||% ""))
      if (changed || !is.null(oldSnap$done) || nzchar(as.character(oldSnap$error %||% ""))) {
        # 旧请求留档：事后要能查"上一次登记的是什么版本"。
        hist <- d$items[[i]]$history %||% list()
        hist[[length(hist) + 1L]] <- oldSnap
        d$items[[i]]$history <- hist
      }
      d$items[[i]]$requested_at <- now
      d$items[[i]]$done <- NULL
      d$items[[i]]$attempts <- 0L
      d$items[[i]]$failed_at <- NULL
      d$items[[i]]$status <- NULL
      d$items[[i]]$owner_key <- as.character(owner_key)
      # 人明确再点一次，才清掉上次的失败原因重新排队。
      d$items[[i]]$error <- NULL
      if (!is.null(bytes) && !is.na(suppressWarnings(as.numeric(bytes))))
        d$items[[i]]$bytes <- as.numeric(bytes)
      d$items[[i]]$run_dir <- as.character(run_dir %||% "")
      d$items[[i]]$want_md5 <- if (nzchar(as.character(want_md5 %||% ""))) as.character(want_md5) else NULL
      hit <- TRUE
    }
  }
  if (!hit) {
    d$items[[length(d$items) + 1]] <- list(
      session = as.character(sid), rel = as.character(rel), owner_key = as.character(owner_key),
      bytes = bytes, run_dir = as.character(run_dir %||% ""),
      want_md5 = if (nzchar(as.character(want_md5 %||% ""))) as.character(want_md5) else NULL,
      requested_at = now, done = NULL)
  }
  mirror_req_write(d)
  list(ok = TRUE, ready = FALSE, rel = as.character(rel),
       msg = "已登记，等执行端取回")
}

# 执行端来取：给它还没取回的清单。
# 单条取回请求的状态：done / error / pending。
#   页面要能区分"还在路上"和"取失败了"，否则人只能一直点重试。
# 分片落盘最后一步：拼到临时件、核大小与 md5，全对上才原子替换。
#   旧实现直接写正式路径、校验不过就 unlink，于是旧的好文件一起没了；
#   缺 md5 时还能通过；也没有总量约束。现在任何一项不符只清临时件。
mirror_chunk_finalize <- function(fin, tot, md5, total_bytes) {
  if (!identical(as.character(md5), as.character(md5)) ||
      length(md5) != 1L || is.na(md5) || !grepl("^[0-9a-f]{32}$", as.character(md5)))
    return(list(ok = FALSE, code = "bad_md5", error = "缺合法 md5"))
  if (!is.numeric(total_bytes) || length(total_bytes) != 1L ||
      is.na(total_bytes) || total_bytes <= 0 || total_bytes != round(total_bytes))
    return(list(ok = FALSE, code = "bad_size", error = "缺合法总量"))
  parts <- paste0(fin, ".part", sprintf("%04d", seq_len(tot) - 1L))
  if (!all(file.exists(parts)))
    return(list(ok = FALSE, code = "missing_chunks", error = "有分片没到"))
  sz <- file.info(parts)$size
  if (any(is.na(sz)) || any(sz < 0))
    return(list(ok = FALSE, code = "bad_chunk", error = "分片大小读不到"))
  if (sum(sz) != total_bytes)
    return(list(ok = FALSE, code = "size_mismatch",
                error = sprintf("分片合计 %s 与登记 %s 不符", sum(sz), total_bytes)))
  tmp <- paste0(fin, ".assembling")
  con <- file(tmp, open = "wb")
  okw <- tryCatch({
    for (x in parts) writeBin(readBin(x, "raw", file.info(x)$size), con)
    TRUE
  }, error = function(e) FALSE)
  tryCatch(close(con), error = function(e) NULL)
  if (!okw) { unlink(tmp); return(list(ok = FALSE, code = "write_failed", error = "拼装失败")) }
  got <- unname(tools::md5sum(tmp))
  if (!identical(as.character(got), as.character(md5))) {
    unlink(tmp)
    return(list(ok = FALSE, code = "md5_mismatch", error = "拼接后校验不符",
                expected = as.character(md5), actual = got))
  }
  if (!file.rename(tmp, fin)) {
    unlink(tmp)
    return(list(ok = FALSE, code = "rename_failed", error = "替换失败，旧文件保持"))
  }
  unlink(parts)
  mirror_record_md5(fin, got)
  list(ok = TRUE, bytes = as.numeric(file.info(fin)$size), md5 = got)
}

mirror_item_state <- function(sid, rel, want_md5 = "") {
  v <- tryCatch(mirror_validity(sid, rel, want_md5), error = function(e) NULL)
  d <- mirror_req_read()
  rec <- NULL
  for (it in (d$items %||% list())) {
    if (identical(as.character(it$session %||% ""), as.character(sid)) &&
        identical(as.character(it$rel %||% ""), as.character(rel))) rec <- it
  }
  recState <- if (is.null(rec)) "" else
    if (!is.null(rec$done) && nzchar(as.character(rec$done))) "done"
    else if (nzchar(as.character(rec$error %||% ""))) "error" else "pending"
  # 历史 done 不能遮蔽取回按钮：记录说取回过，但当前文件已失效或不存在时，
  #   done 一律不成立，交回"还能不能重新取回"的判断。
  if (identical(recState, "done") && !isTRUE(v$ok)) recState <- ""
  list(state = recState,
       error = if (is.null(rec)) "" else as.character(rec$error %||% ""),
       requested_at = if (is.null(rec)) "" else as.character(rec$requested_at %||% ""),
       done = if (is.null(rec)) "" else as.character(rec$done %||% ""),
       cache = if (is.null(v)) "missing" else as.character(v$state %||% ""),
       cached_md5 = if (is.null(v)) "" else as.character(v$actual %||% ""),
       expected_md5 = if (is.null(v)) "" else as.character(v$want %||% ""))
}

# 取不回来就记下原因并停在那里。
#   缺文件的旧请求原来每轮重试一次、永远失败，既刷日志也不会有结果；
#   标成 error 之后不再进 pending，只有人明确重试（mirror_request 会清 error）
#   才会重新排队。原始记录与原因都保留。
mirror_fail <- function(sid, rel, msg) {
  d <- mirror_req_read()
  hit <- FALSE
  for (i in seq_along(d$items)) {
    if (identical(as.character(d$items[[i]]$session %||% ""), as.character(sid)) &&
        identical(as.character(d$items[[i]]$rel %||% ""), as.character(rel)) &&
        is.null(d$items[[i]]$done) && !nzchar(as.character(d$items[[i]]$error %||% ""))) {
      # attempts counts terminal failure reports for this version, not polls/chunks.
      d$items[[i]]$error <- substr(as.character(msg %||% ""), 1, 300)
      d$items[[i]]$status <- NULL
      d$items[[i]]$failed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      d$items[[i]]$attempts <- as.integer(d$items[[i]]$attempts %||% 0L) + 1L
      hit <- TRUE
    }
  }
  if (hit) mirror_req_write(d)
  list(ok = hit)
}

mirror_pending <- function(limit = 24L) {
  d <- mirror_req_read()
  # 已失败的不再自动重试：没有 error 的才算待办。
  out <- Filter(function(it) is.null(it$done) && !nzchar(as.character(it$error %||% "")),
                d$items %||% list())
  # 排序：最新登记的先取。
  #   为什么不是先来先服务：这是人点的"我要看这个"，不是后台批处理。
  #   实测事故 2026-11-18 —— 59 条积压时按数组顺序取，人刚点的 16 张富集图
  #   排在 44~59 位，前 12 条是前一天 16:58 登记的一批审计 tsv；
  #   于是人盯着"正在取回…"十几分钟没有任何变化，看起来像按钮坏了。
  #   同一秒登记的保持原有先后，避免顺序抖动。
  ord <- order(vapply(out, function(it) as.character(it$requested_at %||% ""), ""),
               decreasing = TRUE)
  if (length(ord)) out <- out[ord]
  if (length(out) > limit) out <- out[seq_len(limit)]
  lapply(out, function(it) list(session = it$session, rel = it$rel,
                                bytes = it$bytes, run_dir = it$run_dir,
                                 want_md5 = it$want_md5))
}

# 某条请求在待取队列里的位次（1 起）。给页面显示"前面还有几项"用。
mirror_queue_rank <- function(sid, rel) {
  d <- mirror_req_read()
  out <- Filter(function(it) is.null(it$done) && !nzchar(as.character(it$error %||% "")),
                d$items %||% list())
  if (!length(out)) return(0L)
  ord <- order(vapply(out, function(it) as.character(it$requested_at %||% ""), ""),
               decreasing = TRUE)
  out <- out[ord]
  for (i in seq_along(out)) {
    if (identical(as.character(out[[i]]$session %||% ""), as.character(sid)) &&
        identical(as.character(out[[i]]$rel %||% ""), as.character(rel))) return(as.integer(i))
  }
  0L
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
