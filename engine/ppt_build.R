# =====================================================================
#  ppt_build.R -- 在应用本机生成交付 PPT
#
#  为什么要这一层
#    原来 PPT 只能在服务器上跑：下发 make_ppt.py + 模板 + 22 页素材清单，
#    等执行端跑完回写 pptx。三个前提缺一不可 —— 有 python、模板在、
#    素材齐。任何一个不满足，按钮点下去就停在那，人看到的是"点了没反应"。
#    而 261026 那一批的实际状态是：素材早在 10.figs 里，模板就在引擎里，
#    唯一缺的是"有人去跑"。
#
#  做法
#    素材已经通过镜像取回应用本机了（图与表都在 mirror 下）。
#    所以这里直接把镜像里的素材按 make_ppt.py 认识的六个目录名摆一份，
#    调本机 python3 生成 pptx，落到镜像目录，登记成产物。
#    之后下载走的是已修好的本地命中路径，不再需要服务器。
#
#  几个刻意的选择
#    · 用 --on-missing skip 而不是 error：缺哪几页要说出来并跳过，
#      而不是整份不出。人拿到 18 页 + 一页缺失清单，比拿到 0 页有用。
#    · 素材缺就先停下并列出缺什么，不拿别的图顶替。
#      make_ppt 自己有 fallback 通配，那是它的宽容；这里不能拿它当"齐了"。
#    · 生成结果与跳过清单都返回给页面，不静默成功。
# =====================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# 六个结果目录：spec 里的逻辑名 -> 镜像里可能出现的目录名（按优先级）
PPT_DIRMAP <- list(
  QC          = c("10.figs/0.QC", "10.figs/QC", "0.QC", "QC"),
  cluster     = c("10.figs/1.cluster", "10.figs/cluster", "1.cluster", "cluster"),
  markgene    = c("10.figs/2.markgene", "10.figs/markgene", "2.markgene", "markgene"),
  annot       = c("10.figs/5.annot", "10.figs/annot", "5.annot", "annot"),
  cell_ratio  = c("10.figs/3.cell_ratio", "10.figs/cell_ratio", "3.cell_ratio", "cell_ratio"),
  diff_enrich = c("10.figs/4.diff_enrich", "10.figs/diff_enrich", "4.diff_enrich", "diff_enrich")
)

ppt_find_py <- function() {
  for (p in c("/usr/bin/python3", "/usr/local/bin/python3", Sys.which("python3"))) {
    if (nzchar(p) && file.exists(p)) return(p)
  }
  ""
}

# 把镜像里的素材摆成 make_ppt 认识的目录结构。
#   返回 list(ok, stage_dir, copied, missing_dirs)
ppt_stage_assets <- function(mirror_root, stage_dir) {
  dir.create(stage_dir, showWarnings = FALSE, recursive = TRUE)
  copied <- 0L
  missing <- character(0)
  for (logical in names(PPT_DIRMAP)) {
    src <- NULL
    for (cand in PPT_DIRMAP[[logical]]) {
      p <- file.path(mirror_root, cand)
      if (dir.exists(p)) { src <- p; break }
    }
    if (is.null(src)) { missing <- c(missing, logical); next }
    dst <- file.path(stage_dir, logical)
    dir.create(dst, showWarnings = FALSE, recursive = TRUE)
    fs <- list.files(src, recursive = TRUE, full.names = TRUE, all.files = FALSE)
    fs <- fs[!dir.exists(fs)]
    for (f in fs) {
      rel <- sub(paste0("^", gsub("([\\W])", "\\\\\\1", normalizePath(src, winslash = "/")), "/?"),
                 "", normalizePath(f, winslash = "/"))
      to <- file.path(dst, rel)
      dir.create(dirname(to), showWarnings = FALSE, recursive = TRUE)
      if (isTRUE(file.copy(f, to, overwrite = TRUE))) copied <- copied + 1L
    }
  }
  list(ok = TRUE, stage_dir = stage_dir, copied = copied, missing_dirs = missing)
}

# 主流程：从镜像素材生成 pptx。
#   mirror_root：该分析记录在应用本机的镜像根
#   out_rel：相对镜像根的输出路径（默认 11.report/analysis_report.pptx）
#   on_missing：默认 error —— 缺图就停下，不拿别的图顶上。
#     以前默认 skip，于是缺图时它会拿目录里第一张 png 充数，
#     出一份"看着完整、其实有 7 页放错图"的 PPT。那比不出更糟：
#     不出，人知道去补图；出错版，人会拿去交。
ppt_build_from_mirror <- function(engine_dir, mirror_root, out_rel = "11.report/analysis_report.pptx",
                                 title = "", date_str = "", on_missing = "error") {
  py <- ppt_find_py()
  script <- file.path(engine_dir, "sc-ppt-kit", "scripts", "make_ppt.py")
  tpl <- file.path(engine_dir, "sc-ppt-kit", "templates", "template.pptx")
  if (!nzchar(py)) return(list(ok = FALSE, code = "no_python",
    error = "这台机器上没有 python3，生成不了 PPT。"))
  if (!file.exists(script)) return(list(ok = FALSE, code = "no_script",
    error = paste0("找不到生成脚本：", script)))
  if (!file.exists(tpl)) return(list(ok = FALSE, code = "no_template",
    error = paste0("找不到 PPT 模板：", tpl)))
  if (!dir.exists(mirror_root)) return(list(ok = FALSE, code = "no_mirror",
    error = "这批结果还没有取回到应用本机，先取回素材再生成。"))

  stage <- file.path(tempdir(), paste0("ppt-stage-", format(Sys.time(), "%Y%m%d%H%M%S")))
  st <- ppt_stage_assets(mirror_root, stage)
  if (!st$copied)
    return(list(ok = FALSE, code = "no_assets",
      error = "镜像里没有可用的图表素材，先生成图或先取回。",
      missing_dirs = st$missing_dirs))

  out_abs <- file.path(mirror_root, out_rel)
  dir.create(dirname(out_abs), showWarnings = FALSE, recursive = TRUE)
  # 先删掉上一次的产物。
  #   为什么必须删：生成失败时旧文件还在，`file.exists(size>20000)` 照样为真，
  #   于是"这次没生成成功"会被判成"成功"，页面给出下载，
  #   人拿到的却是上一轮的 PPT —— 而且看不出来。
  #   这类"旧结果冒充新结果"比直接失败危险得多。
  if (file.exists(out_abs)) unlink(out_abs)
  rep <- file.path(stage, "ppt-report.txt")
  pyargs <- c(shQuote(script),
              "--result-dir", shQuote(stage),
              "--template", shQuote(tpl),
              "--out", shQuote(out_abs),
              "--on-missing", on_missing,
              "--report", shQuote(rep))
  if (nzchar(title)) pyargs <- c(pyargs, "--title", shQuote(title))
  if (nzchar(date_str)) pyargs <- c(pyargs, "--date", shQuote(date_str))
  # system2 的第一个参数必须是「可执行文件」，第二是参数数组。
  #   以前把整条命令拼成一个字符串传给 system2，它会当成一个文件名去找，
  #   直接失败并回一句 error in running command —— 看起来像脚本崩了，
  #   其实是调用方式错了。
  #   stderr 必须一起收：缺图时脚本走 raise SystemExit，说明在 stderr 里；
  #   只收 stdout 会读到空串，"缺 5 样"被读成"什么都不缺"。
  out <- suppressWarnings(system2(py, pyargs, stdout = TRUE, stderr = TRUE))
  code <- attr(out, "status") %||% 0L

  okk <- file.exists(out_abs) && file.info(out_abs)$size > 20000
  rep_txt <- if (file.exists(rep)) paste(readLines(rep, warn = FALSE), collapse = "\n") else ""
  skipped <- character(0)
  if (nzchar(rep_txt)) {
    ls <- grep("跳过|missing|缺", readLines(textConnection(rep_txt), warn = FALSE), value = TRUE)
    if (length(ls)) skipped <- utils::head(ls, 30)
  }

  # 缺了什么：两种来源都要读。
  #   加了 --on-missing error 时，脚本在写报告之前就退出了，
  #   所以缺项只在命令输出里，报告文件是空的。
  #   以前只看报告文件，于是"缺 5 样"被判成"missing 为空"，
  #   后面的自动补图一步都没派下去 —— 错在这里。
  miss <- character(0)
  scan_lines <- function(ls) {
    for (ln in ls) {
      m <- regmatches(ln, regexec("^\\s*-\\s*图片\\s+(\\S+)", ln, perl = TRUE))[[1]]
      if (length(m) >= 2L) { miss <<- c(miss, m[2]); next }
      if (grepl("^\\s*-\\s*样本统计表", ln)) miss <<- c(miss, "样本统计表")
    }
  }
  if (length(out)) scan_lines(as.character(out))
  if (!length(miss) && nzchar(rep_txt)) scan_lines(readLines(textConnection(rep_txt), warn = FALSE))
  miss <- unique(miss)

  list(ok = isTRUE(okk), code = if (isTRUE(okk)) "ok" else "build_failed",
       exit_code = as.integer(code),
       out_rel = if (isTRUE(okk)) out_rel else NULL,
       bytes = if (isTRUE(okk)) file.info(out_abs)$size else NA,
       md5 = if (isTRUE(okk)) unname(tools::md5sum(out_abs)) else NA,
       copied = st$copied, missing_dirs = st$missing_dirs,
       skipped = as.list(skipped),
       log_tail = paste(utils::tail(out, 20), collapse = "\n"),
       report_tail = substr(rep_txt, 1, 4000),
       missing = as.list(miss))
}

# 缺哪张图 -> 该重跑哪一环。
#   为什么要有这个映射：出图脚本一跑能补好几张，但重跑哪一环是不同的。
#   markgene / ratio / diff 都在 figs 一环里（plot_all_v2.R 的 stage 参数）；
#   dotplot 在 annot；GO/KEGG 在 enrich。
#   补图不是"再点一次 PPT"，是"回服务器把那几张图真的画出来"。
PPT_MISSING_STAGE <- function(name) {
  n <- tolower(as.character(name %||% ""))
  if (!nzchar(n)) return("")
  # 注意：dotplot 看着像注释那一步的产物，其实不是。
  #   genelist_dotplot 是出图脚本 plot_all_v2.R 的 annot 段画的，
  #   而 figs 环节的命令里已经带了 --stage ...,annot,...。
  #   如果映射到 annotate 环节，那一环要 {reference_xlsx}，缺依据就派不下去，
  #   于是"该补的没补、不该动的被拉进来"。
  if (grepl("heatmap|featureplot|violin_cluster|top10", n)) return("figs")
  if (grepl("dotplot", n)) return("figs")
  if (grepl("cell_number_rate|all_cluster", n)) return("figs")
  if (grepl("volcano", n)) return("figs")
  if (grepl("go_bp|kegg|enrich", n)) return("enrich")
  # 样本统计表不是出图脚本产出的，重跑 figs 也补不出来 —— 必须单独说清，
  #   否则会让人反复点重跑，而那张表永远不会出现。
  if (grepl("sample_stat|qc_stat|sample_info", n)) return("")
  ""
}

# 把缺图整理成"重跑哪几环 + 哪几样补不出来"
ppt_autofix_plan <- function(missing) {
  ms <- as.character(unlist(missing %||% character(0)))
  ms <- ms[nzchar(ms)]
  st <- vapply(ms, PPT_MISSING_STAGE, "")
  list(stages = as.list(unique(st[nzchar(st)])),
       unproducible = as.list(ms[!nzchar(st)]),
       missing = as.list(ms))
}
