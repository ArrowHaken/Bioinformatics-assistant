# =====================================================================
#  脚本溯源与一致性核对
# ---------------------------------------------------------------------
#  写这个文件的原因（2026-10-28 事故）：
#    第 3 轮重写脚本时，注释写着"与 1_261016 里那份逻辑逐行一致"，
#    而 1_261016 是上一轮自己写的脚本，不是 liny 的原始脚本。
#    原始脚本里没有 as.integer(factor) 这行，上一轮加进去的；
#    那一行会把 0 号簇整簇变成空值，统计表看起来正常但总数少了 15.8%。
#    连续两轮没人发现，因为"以为参照的是原始版"这件事从来没被核对过。
#  所以这里做两件事：
#    ① 每个脚本登记它参照的是哪份文件、核验过没有。没核验就在界面上标出来，
#       不让人以为"跑通了"等于"跑对了"。
#    ② 回写结论时做总数核对：统计表的细胞总数必须等于对象细胞数，
#       对不上就拒绝把这一步记成完成。
# =====================================================================

PROV_FILE <- function() file.path(STATE_DIR, "script_provenance.json")

prov_read <- function() {
  f <- PROV_FILE()
  if (!file.exists(f)) return(list(items = list()))
  d <- tryCatch(jsonlite::fromJSON(read_text_any(f), simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(d) || is.null(d$items)) return(list(items = list()))
  d
}

prov_write <- function(d) {
  d$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  write_json_file(PROV_FILE(), d)
  invisible(d)
}

# 登记/更新一个脚本的来源
#   rel      : 脚本相对路径（如 drop_and_recluster.R）
#   origin   : original | previous_round | new | unknown
#   reference: 参照的那份文件的绝对路径
#   verified : TRUE/FALSE  是否已逐条与原始脚本比对过
#   diffs    : 与参照版之间的实质差异（字符串数组）
prov_put <- function(rel, origin, reference = "", verified = FALSE, diffs = list(), note = "") {
  d <- prov_read()
  hit <- NULL
  for (i in seq_along(d$items)) {
    if (identical(as.character(d$items[[i]]$rel %||% ""), as.character(rel))) hit <- i
  }
  item <- list(
    rel = as.character(rel),
    origin = as.character(origin),
    reference = as.character(reference),
    verified = isTRUE(verified),
    diffs = as.list(unlist(diffs)),
    note = as.character(note),
    checked_at = if (isTRUE(verified)) format(Sys.time(), "%Y-%m-%dT%H:%M:%S") else NULL)
  if (is.null(hit)) d$items[[length(d$items) + 1]] <- item else d$items[[hit]] <- item
  prov_write(d)
  item
}

# 未核验的脚本清单：界面要能看见"哪些脚本来路不明"
prov_unverified <- function() {
  d <- prov_read()
  Filter(function(x) !isTRUE(x$verified), d$items)
}

# 首次写库：把已经查清的事实登记进去。
#   这些是 2026-10-28 逐条比对服务器文件得出的，不是推测。
#   没做过比对的脚本，verified 一律 FALSE —— 不允许"看起来没问题"就标成已核验。
prov_seed <- function() {
  if (file.exists(PROV_FILE())) return(invisible(FALSE))
  ORIG <- "/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/6sample_data_script/script"
  d <- list(items = list())
  add <- function(rel, origin, reference, verified, diffs, note) {
    d$items[[length(d$items) + 1]] <<- list(
      rel = rel, origin = origin, reference = reference,
      verified = isTRUE(verified), diffs = as.list(unlist(diffs)),
      note = note,
      checked_at = if (isTRUE(verified)) format(Sys.time(), "%Y-%m-%dT%H:%M:%S") else NULL)
  }
  add("drop_and_recluster.R", "previous_round", file.path(ORIG, "seurat_merge_cluster.R"),
      TRUE,
      c("簇号转换已修正：as.integer(factor) -> as.integer(as.character(...))；原始版无此写法",
        "剔除清单改为从 7.after_drop/drop_params.json 读取，不再写死",
        "新增两条总数自检（统计表总数 == 对象细胞数）"),
      "2026-10-28 与原始脚本逐条比对；修正了导致 0 号簇整簇丢失的写法")
  add("final_run.R", "previous_round", file.path(ORIG, "markgene.R"),
      TRUE,
      c("簇号转换同上已修正",
        "FindAllMarkers 的 min.pct 用 0.25，原始 markgene.R 用 0.2",
        "新增空簇等级自检与各簇之和自检"),
      "2026-10-28 已比对；min.pct 差异待确认是否有意为之")
  add("merge_cluster.R", "previous_round", file.path(ORIG, "seurat_merge_cluster.R"),
      FALSE,
      c("注释称与 1_261016 逐行一致，但 1_261016 是上一轮自写脚本，不是原始脚本",
        "尚未与原始脚本逐条比对"),
      "未核验：仅做过参数抽查（分辨率 0.5 一致）")
  add("cluster_info.R", "previous_round", file.path(ORIG, "markgene.R"),
      FALSE,
      c("FindAllMarkers min.pct 用 0.25，原始用 0.2",
        "尚未逐条比对绘图与统计部分"),
      "未核验：只查了差异分析阈值")
  prov_write(d)
  invisible(TRUE)
}

# =====================================================================
#  原始流程回溯
# ---------------------------------------------------------------------
#  为什么要换掉"脚本存在不存在"这种判断：
#    2026-10-28 的教训 —— 只看"工作目录里有没有 full_marker.R"，
#    结论是"服务器上没有脚本、这一步没法跑"，于是停在那里等人补脚本。
#    实际情况是：这一步要的东西，原始项目里由 markgene.R 做，
#    而且我们自己的终版已经算过一遍、产物就在 8.final。
#    也就是说，"文件不存在"根本不能推出"这件事没做过"。
#  现在的判据换成三问：
#    ① 这一套原始流程里，这一步对应哪个脚本（服务器上 liny 那套）？
#    ② 那份脚本做过这件事吗？
#    ③ 我们这轮有没有已经产出同样的东西？
#  三者里只要有一个成立，就不该判"没法跑"，更不该让人去补一个
#  和原始流程对不上的新脚本。
#
#  注意：这里只读判断，不改任何已有记录、不改动已跑完的步骤。
# =====================================================================

ORIGIN_ROOT <- "/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/6sample_data_script/script"

ORIGIN_MAP <- list(
  doublet = list(
    original = "soupX_doublefinder.R",
    where = "逐样本 SoupX / DoubletFinder，产物 1.doublet",
    ours = "soupX_doublefinder.R（服务器的原脚本，未改写）"),
  merge = list(
    original = "seurat_merge_cluster.R",
    where = "merge 7 样本 + 标准化 + 周期回归 + Harmony + 一次聚类（res 0.5）",
    ours = "merge_cluster.R（按原始逻辑重写，路径与样本数改了）"),
  markers = list(
    original = "markgene.R",
    where = "FindAllMarkers(only.pos, min.pct=0.2) + 簇统计 + 4 张 UMAP",
    ours = "cluster_info.R（同逻辑，产物拆成独立文件）"),
  filter = list(
    original = "analysis.R（第 13-17 行）",
    where = "subset(seurat_clusters %in% c(0:15,23,24)) 后 FindNeighbors/FindClusters(res 0.6)",
    ours = "drop_and_recluster.R（改为读参数文件的剔除清单）"),
  final = list(
    original = "analysis.R（第 30 行剔 G761）+ seurat_cell_numer.R",
    where = "剔 G761、算每簇比例/每样本细胞数、FindAllMarkers、按簇 FindMarkers",
    ours = "final_run.R（同逻辑）"),
  fullmark = list(
    original = "markgene.R（第 33 行 FindAllMarkers）",
    where = "全量 marker 与 top15，原始产物写 2.markgene/all_markgene.xls",
    ours = "不需要单独跑：final_run.R 已算 FindAllMarkers，产物在 8.final/all_markers.xls 与 top15_markers.xls",
    done_in = "final"),
  diff = list(
    original = "analysis.R（第 152 行 FindMarkers，按 group 分组）",
    where = "Microplastics vs Mock 差异，原始产物在 4.diff_enrich",
    ours = "不需要单独跑：final_run.R 已按簇逐个算过，产物 8.final/diff_MP_vs_Mock_by_cluster.xls",
    done_in = "final"),
  figs = list(
    original = "analysis.R / 01_packages.R 里的 plot_* 函数",
    where = "UMAP、dotplot、violin、火山图、热图等",
    ours = "make_figs3.R（本工作目录里没有，需确认是否已随引擎下发）"),
  enrich = list(
    original = "cluster_profile_enrich.R",
    where = "GO / KEGG 富集",
    ours = "make_enrich2.R（本工作目录里没有）")
)

origin_of <- function(stepId) ORIGIN_MAP[[as.character(stepId)]]

# 回溯一步：它在这套流程里有没有出处、有没有已经做过。
#   files: 本次运行目录里已有的产物清单（相对路径）
#   返回 list(found=, original=, where=, ours=, done_in=, done_evidence=)
origin_trace <- function(stepId, files = character(0)) {
  e <- origin_of(stepId)
  if (is.null(e)) {
    return(list(found = FALSE, original = "", where = "", ours = "",
                done_in = "", done_evidence = ""))
  }
  doneIn <- as.character(e$done_in %||% "")
  ev <- ""
  if (nzchar(doneIn) && length(files)) {
    # 覆盖它的那一步有没有真的产出
    up <- switch(doneIn,
      final = c("8.final/all_markers.xls", "8.final/top15_markers.xls",
                "8.final/diff_MP_vs_Mock_by_cluster.xls"),
      character(0))
    hit <- intersect(up, as.character(files))
    if (length(hit)) ev <- paste0("已确认产出：", paste(hit, collapse = "、"))
  }
  list(found = TRUE,
       original = as.character(e$original %||% ""),
       where = as.character(e$where %||% ""),
       ours = as.character(e$ours %||% ""),
       done_in = doneIn,
       done_evidence = ev)
}

# ---- 一致性核对 -----------------------------------------------------
#  两类核对，容错尺度不同，不能混为一谈：
#    mode="internal"：同一份对象内部，各簇之和 == 对象细胞数。
#       这是"数"出来的，不是"测"出来的：每个细胞恰好有一个簇标签，
#       加起来必须严格相等。这里给容差是错的 —— 差一个就是真的漏了一群细胞。
#       所以 internal 用严格相等，不放宽。
#    mode="cross"：跨文件/跨轮次比对（例如本次表与上一轮表、日志与表）。
#       两边口径可能本来就不同（剔了样本、换了筛选条件），
#       这时用相对容差，超了先报警告，不直接停 —— 免得正常的口径差异把流程堵死。
#   默认 internal：宁可严格，也不要"看起来差不多就放过去"。
consistency_check <- function(cells = NULL, table_sum = NULL,
                              by_sample = NULL, by_cluster = NULL,
                              label = "统计表", mode = c("internal", "cross"),
                              tol = 0.001) {
  mode <- match.arg(mode)
  problems <- character(0)
  warns <- character(0)
  cn <- suppressWarnings(as.numeric(cells %||% NA))
  ts <- suppressWarnings(as.numeric(table_sum %||% NA))

  same <- function(a, b) {
    if (mode == "internal") return(a == b)
    ref <- max(abs(a), abs(b), 1)
    abs(a - b) / ref <= tol
  }

  if (!is.na(cn) && !is.na(ts) && !same(cn, ts)) {
    msg <- paste0(label, "的细胞总数 ", ts, " 与对象细胞数 ", cn,
                  " 不一致（差 ", cn - ts, "）")
    if (mode == "internal") problems <- c(problems, msg) else warns <- c(warns, msg)
  }
  chk <- function(part, name) {
    if (is.null(part)) return(invisible(NULL))
    v <- suppressWarnings(as.numeric(unlist(part)))
    v <- v[!is.na(v)]
    if (!length(v)) return(invisible(NULL))
    s <- sum(v)
    ref <- if (!is.na(ts)) ts else cn
    if (is.na(ref)) return(invisible(NULL))
    if (!same(s, ref)) {
      msg <- paste0(name, "分项之和 ", s, " 与总数 ", ref, " 不一致（差 ", ref - s, "）")
      if (mode == "internal") problems <<- c(problems, msg) else warns <<- c(warns, msg)
    }
  }
  chk(by_sample, "按样本")
  chk(by_cluster, "按簇")

  list(ok = !length(problems), mode = mode,
       problems = as.list(problems), warnings = as.list(warns))
}
