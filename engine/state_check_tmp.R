#!/usr/bin/env Rscript
# =====================================================================
#  flow_spec.R -- 「这套流程按什么顺序、用什么脚本跑」的唯一出处
#
#  两套档位，不能混：
#    server 档 = 服务器上那套已经跑通并确认过的脚本（liny 那套 + 工作目录里的
#                merge_cluster.R / cluster_info.R / ...）。这是用户真正在用的。
#    local  档 = 应用自带的 sc-pipeline（00_run_all.R），供没有服务器、数据在
#                本机时用。
#
#  为什么要分档：以前只写了一套（应用自带的），但服务器上根本没有
#  00_run_all.R，也没有 pipeline.json 与 results/ 目录。按那套发指令，
#  执行端只会找不到文件 —— 这就是"处理偏移"最典型的样子：
#  指令看着很具体，实际一条都跑不了。
#
#  2026-10-22 核对依据（在 master01 上实测）：
#    · 工作目录 /share/home/hhhong1/rachel_7sample 下有 1.doublet 3.merge
#      5.markers 7.after_drop 8.final 9.full 10.figs 这些真实产物目录
#    · 编排脚本是 work_root/chain.sh（qsub 提交，等产物再提交下一步）
#    · 脚本根目录 /share/home/liny/.../6sample_data_script/script/ 下是
#      soupX_doublefinder.R 与几个 .sh 包装
#    · 服务器上没有 00_run_all.R、没有 pipeline.json、没有 _engine
#
#  约定：本文件只描述，不执行、不做 IO。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

# ---------------------------------------------------------------------
#  server 档：真实在跑的那套
#    目录约定（相对 work_root）：1.doublet → 3.merge → 5.markers →
#                                7.after_drop → 8.final → 9.full → 10.figs
# ---------------------------------------------------------------------
SERVER_STAGES <- list(
  list(
    id = "doublet", title = "逐样本去双与质控", skill = "liny-doublet",
    note = "7 个样本各跑一次 SoupX(关)/DoubletFinder(开)，流水线细胞落 1.doublet。",
    script = "{script_root}/script/soupX_doublefinder.R",
    args = c("--datadir", "{liny_data}/", "--sample", "{samples}",
             "--outdir", "{work_root}/", "--soupx", "N",
             "--doublefinder", "Y", "--species", "Mouse"),
    artifacts = c("1.doublet/18m-G489.rds", "1.doublet/18m-G761.rds"),
    per_sample = TRUE,
    subs = list(
      list(id = "load", title = "逐样本读入"),
      list(id = "doublet", title = "DoubletFinder"),
      list(id = "stat", title = "写 _stat.xls")
    )
  ),
  list(
    id = "merge", title = "合并与批次校正", skill = "liny-merge",
    note = "读 1.doublet 的 7 个 rds，合并、标准化、周期回归、Harmony、一次聚类。",
    script = "{work_root}/merge_cluster.R",
    args = character(0),
    qsub = TRUE, qsub_name = "merge7",
    artifacts = c("3.merge/merge_7sample.rds"),
    deps = c("1.doublet"),
    subs = list(
      list(id = "read", title = "读入 7 个 rds"),
      list(id = "merge", title = "merge"),
      list(id = "harmony", title = "Harmony 批次校正"),
      list(id = "cluster", title = "FindClusters"),
      list(id = "tsne", title = "RunTSNE")
    )
  ),
  list(
    id = "markers", title = "一次聚类 marker", skill = "liny-markers",
    note = "算簇大小、簇 top15 marker，供筛群判断用。",
    script = "{work_root}/cluster_info.R",
    args = character(0),
    qsub = TRUE, qsub_name = "cinfo",
    artifacts = c("5.markers/cluster_stats.xls", "5.markers/top15_markers.xls"),
    deps = c("3.merge/merge_7sample.rds"),
    subs = list(
      list(id = "stats", title = "簇大小统计"),
      list(id = "markers", title = "top15 marker"),
      list(id = "umap", title = "UMAP 三张")
    )
  ),
  list(
    id = "filter", title = "筛群与剔样本后重聚类", skill = "liny-filter",
    note = "按人定的筛群与剔样本结果重跑，产出 7.after_drop。",
    script = "{work_root}/drop_and_recluster.R",
    args = character(0),
    artifacts = c("7.after_drop/merged_after_drop.rds"),
    deps = c("5.markers/cluster_stats.xls"),
    subs = list(
      list(id = "drop", title = "剔除指定簇/样本"),
      list(id = "recluster", title = "二次聚类"),
      list(id = "stats", title = "重算簇大小")
    )
  ),
  list(
    id = "final", title = "终版比例统计", skill = "liny-final",
    note = "基于剔完的对象算每样本细胞数、每簇比例。",
    script = "{work_root}/final_run.R",
    args = character(0),
    artifacts = c("8.final/cell_ratio_by_cluster.xls", "8.final/cell_count_by_sample.xls"),
    deps = c("7.after_drop/merged_after_drop.rds"),
    subs = list(
      list(id = "ratio", title = "簇比例"),
      list(id = "count", title = "样本细胞数"),
      list(id = "save", title = "存最终对象")
    )
  ),
  list(
    id = "fullmark", title = "全量 marker", skill = "liny-markers",
    note = "对终版对象算全量 marker 与 top15。",
    script = "{work_root}/full_marker.R",
    args = character(0),
    artifacts = c("9.full/all_markers_full.xls", "9.full/top15_markers_full.xls"),
    deps = c("8.final"),
    subs = list(
      list(id = "all", title = "全量 marker"),
      list(id = "top", title = "top15")
    )
  ),
  list(
    id = "diff", title = "组间差异", skill = "liny-diff",
    note = "Microplastics vs Mock 的全量差异表。",
    script = "{work_root}/full_diff.R",
    args = character(0),
    artifacts = c("9.full/diff_MP_vs_Mock_full.xls"),
    deps = c("8.final"),
    subs = list(
      list(id = "diff", title = "FindAllMarkers 组间"),
      list(id = "write", title = "写差异表")
    )
  ),
  list(
    id = "figs", title = "出图", skill = "liny-figs",
    note = "UMAP / dotplot / violin / feature / 比例 / 火山 / 质控 / 热图，落 10.figs。",
    script = "{work_root}/make_figs3.R",
    args = character(0),
    artifacts = c("10.figs"),
    deps = c("9.full"),
    scripts_extra = c("{work_root}/make_figs.R", "{work_root}/make_figs2.R",
                      "{work_root}/make_figs3.R"),
    subs = list(
      list(id = "umap", title = "UMAP 三张"),
      list(id = "dot", title = "dotplot / violin / feature"),
      list(id = "ratio", title = "细胞比例"),
      list(id = "volcano", title = "火山图"),
      list(id = "qc", title = "质控三张"),
      list(id = "heat", title = "marker 热图")
    )
  ),
  list(
    id = "enrich", title = "GO 富集", skill = "liny-enrich",
    note = "对差异基因做 GO-BP 富集并出 dotplot。",
    script = "{work_root}/make_enrich2.R",
    args = character(0),
    artifacts = c("10.figs/enrich_summary.xls", "10.figs/GO_BP_up.xls"),
    deps = c("9.full/diff_MP_vs_Mock_full.xls"),
    scripts_extra = c("{work_root}/make_enrich.R"),
    subs = list(
      list(id = "go", title = "GO-BP 富集"),
      list(id = "plot", title = "出 dotplot")
    )
  )
)

SERVER_RUN_ORDER <- vapply(SERVER_STAGES, function(s) s$id, "")

# 服务器上没有 PPT 与打包这一步 —— 就不写它。
#   写一个"应该存在"的阶段，页面会一直显示"待跑"，而实际根本没有对应脚本，
#   人只能反复点、反复等。缺什么就说缺什么。
SERVER_MISSING <- c("ppt", "pack")

# ---------------------------------------------------------------------
#  local 档：应用自带的 sc-pipeline（数据在本机时用）
# ---------------------------------------------------------------------
LOCAL_STAGES <- list(
  list(id = "env", title = "环境体检", skill = "sc-pipeline",
       note = "只读：查 R、包、内存、Python。",
       artifacts = c("status/env_check.json"), deps = character(0),
       subs = list(list(id = "r", title = "R 与包"),
                   list(id = "mem", title = "内存与磁盘"),
                   list(id = "py", title = "Python 依赖"))),
  list(id = "decide", title = "自动判定", skill = "sc-pipeline",
       note = "判断机器上有没有现成脚本、够不够跑。",
       artifacts = c("status/decision.json"), deps = character(0),
       subs = list(list(id = "scripts", title = "扫已有脚本"),
                   list(id = "verdict", title = "给判定结论"))),
  list(id = "precheck", title = "容量预检", skill = "sc-pipeline", phase = "A",
       note = "dry-run，不写结果。",
       artifacts = character(0), deps = character(0),
       subs = list(list(id = "files", title = "核对输入文件"),
                   list(id = "est", title = "估算内存"))),
  list(id = "pipeline1", title = "跑主流程 · 上段", skill = "sc-pipeline",
       phase = "A", stage_arg = "cluster1",
       note = "读 matrix → 去双 → 质控 → 合并 → 批次校正 → 一次聚类 → 停住。",
       artifacts = c("status/stage_cluster1.rds"), deps = character(0),
       subs = list(list(id = "dep", title = "依赖检查"),
                   list(id = "sample", title = "样本清单"),
                   list(id = "precheck", title = "容量预检"),
                   list(id = "doublet_qc", title = "逐样本去双与质控"),
                   list(id = "merge", title = "合并与批次校正"),
                   list(id = "cluster1", title = "一次聚类"),
                   list(id = "basis", title = "生成判断依据并停住"))),
  list(id = "pipeline2", title = "跑主流程 · 下段", skill = "sc-pipeline",
       phase = "B", stage_arg = "continue",
       note = "筛群 → 二次聚类 → 剔样本 → 文献注释 → 保存。",
       artifacts = c("processed.rds"), deps = c("status/stage_cluster1.rds"),
       subs = list(list(id = "filter", title = "筛群"),
                   list(id = "cluster2", title = "二次聚类"),
                   list(id = "dropsample", title = "剔除样本"),
                   list(id = "annot", title = "文献注释"),
                   list(id = "save", title = "保存 processed.rds"))),
  list(id = "plots", title = "出图", skill = "sc-plot-kit",
       note = "六套图。", artifacts = c("plots"), deps = c("processed.rds"),
       subs = list(list(id = "qc", title = "质控图"),
                   list(id = "cluster", title = "聚类 UMAP"),
                   list(id = "markgene", title = "marker 热图"),
                   list(id = "annot", title = "注释图"),
                   list(id = "ratio", title = "细胞比例"),
                   list(id = "diff", title = "组间差异"),
                   list(id = "enrich", title = "富集图"))),
  list(id = "ppt", title = "出 PPT", skill = "sc-ppt-kit",
       note = "按交付版式组装 pptx。", artifacts = c("analysis_report.pptx"),
       deps = c("plots"),
       subs = list(list(id = "collect", title = "收集图"),
                   list(id = "assemble", title = "按母版组装"))),
  list(id = "pack", title = "打包结果", skill = "workflow",
       note = "压成一个 zip。", artifacts = c("analysis_results.zip"),
       deps = c("analysis_report.pptx"),
       subs = list(list(id = "zip", title = "压缩")))
)

LOCAL_RUN_ORDER <- c("pipeline1", "pipeline2", "plots", "ppt", "pack")

LOCAL_CMDS <- list(
  env = "{rscript} {engine}/env_check.R --out {run_dir}/status/env_check.json",
  decide = "{rscript} {engine}/decide_cli.R --dir {run_dir} --engine {engine}",
  precheck = paste0("{rscript} {scripts}/00_run_all.R --config {run_dir}/pipeline.json",
                    " --indir {indir} --outdir {outdir} --dry-run"),
  pipeline1 = paste0("{rscript} {scripts}/00_run_all.R --config {run_dir}/pipeline.json",
                     " --indir {indir} --outdir {outdir} --stage cluster1",
                     " --gates {run_dir}/gates.json"),
  pipeline2 = paste0("{rscript} {scripts}/00_run_all.R --config {run_dir}/pipeline.json",
                     " --indir {indir} --outdir {outdir} --stage continue",
                     " --gates {run_dir}/gates.json"),
  plots = paste0("{rscript} {engine}/sc-plot-kit/scripts/plot_all_v2.R {outdir}/processed.rds",
                 " --outdir {outdir}/plots --config {run_dir}/pipeline.json",
                 " --stage qc,cluster,markgene,annot,ratio,diff,enrich"),
  ppt = paste0("{python} {engine}/sc-ppt-kit/scripts/make_ppt.py --result-dir {outdir}/plots",
               " --template {engine}/sc-ppt-kit/templates/template.pptx",
               " --config {run_dir}/pipeline.json --out {outdir}/analysis_report.pptx"),
  pack = "zip -r -q {outdir}/analysis_results.zip {outdir}/plots {outdir}/analysis_report.pptx"
)

# ---------------------------------------------------------------------
#  统一入口：按档位取
# ---------------------------------------------------------------------
FLOW_STAGES <- function(profile = c("server", "local")) {
  profile <- match.arg(profile)
  if (identical(profile, "server")) SERVER_STAGES else LOCAL_STAGES
}
FLOW_ORDER <- function(profile = c("server", "local")) {
  profile <- match.arg(profile)
  if (identical(profile, "server")) SERVER_RUN_ORDER else LOCAL_RUN_ORDER
}

flow_stage <- function(id, profile = c("server", "local")) {
  profile <- match.arg(profile)
  hit <- Filter(function(s) identical(as.character(s$id), as.character(id)),
                FLOW_STAGES(profile))
  if (!length(hit)) NULL else hit[[1]]
}

# 服务器档的命令：脚本 + 参数，直白拼出来。
#   qsub 的包装与 chain.sh 里一致：-q all.q -cwd -o/-e 写 log/ -v LD_LIBRARY_PATH。
flow_server_cmd <- function(st, V, use_qsub = TRUE) {
  if (is.null(st) || is.null(st$script)) return(NULL)
  # fixed=TRUE 时 pattern 是字面量：不能写 "\\{k\\}"（那代表反斜杠加大括号），
  #   只能写 "{k}"。写错的表现是占位符原样留在指令里发出去 ——
  #   执行端拿到一条带 {script_root} 的命令，等于没给命令。
  repl <- function(x) {
    out <- as.character(x)
    for (k in names(V)) out <- gsub(paste0("{", k, "}"), as.character(V[[k]]), out, fixed = TRUE)
    out
  }
  script <- repl(st$script)
  args <- repl(st$args %||% character(0))
  base <- paste(c(V$rscript, script, args), collapse = " ")
  if (isTRUE(use_qsub) && isTRUE(st$qsub)) {
    return(paste0(V$qsub, " -q ", V$queue, " -N ", st$qsub_name,
                  " -cwd -o ", V$work_root, "/log/", st$qsub_name, ".o",
                  " -e ", V$work_root, "/log/", st$qsub_name, ".e",
                  " -v LD_LIBRARY_PATH=", V$hdf5,
                  " -b y \"", base, "\""))
  }
  base
}

flow_cmd <- function(id, V, profile = c("server", "local")) {
  profile <- match.arg(profile)
  if (identical(profile, "server")) {
    return(flow_server_cmd(flow_stage(id, "server"), V))
  }
  tpl <- LOCAL_CMDS[[as.character(id)]]
  if (is.null(tpl)) return(NULL)
  out <- tpl
  # 同上：fixed=TRUE 要配 "{k}"，不能带反斜杠转义。
  for (k in names(V)) out <- gsub(paste0("{", k, "}"), as.character(V[[k]]), out, fixed = TRUE)
  out
}

# ---------------------------------------------------------------------
#  质检判据：用户要的三件事
# ---------------------------------------------------------------------
FLOW_GATES <- list(
  list(id = "on_server", title = "确实在服务器上跑",
       how = paste0("核对回写里的执行机器与登录主机，和位置设置里填的同一台；",
                    "且产物时间戳落在这一轮的时间窗内。")),
  list(id = "right_scripts", title = "脚本与流程对得上",
       how = paste0("用的是 flow_spec 里登记的那几个脚本（soupX_doublefinder.R / ",
                    "merge_cluster.R / cluster_info.R / ...），不是另写一份。")),
  list(id = "no_drift", title = "处理没有偏移",
       how = paste0("每一步的前置产物必须已经存在：3.merge 之前要有 1.doublet，",
                    "5.markers 之前要有 3.merge/merge_7sample.rds，依此类推。",
                    "缺一个就是没到这一步，不是可以跳过。"))
)

flow_gate <- function(id) {
  hit <- Filter(function(s) identical(as.character(s$id), as.character(id)), FLOW_GATES)
  if (!length(hit)) NULL else hit[[1]]
}

# ---------------------------------------------------------------------
#  自检
# ---------------------------------------------------------------------
flow_spec_selfcheck <- function(engineDir = NULL) {
  problems <- character(0)
  for (prof in c("server", "local")) {
    for (s in FLOW_STAGES(prof)) {
      if (is.null(s$id) || is.null(s$title) || is.null(s$skill))
        problems <- c(problems, paste0(prof, ": 有阶段缺 id/title/skill"))
      if (!length(s$subs %||% list()))
        problems <- c(problems, paste0(prof, "/", s$id, ": 没有登记环节"))
    }
    ids <- vapply(FLOW_STAGES(prof), function(s) s$id, "")
    if (anyDuplicated(ids)) problems <- c(problems, paste0(prof, ": 阶段 id 重复"))
  }
  if (!is.null(engineDir) && dir.exists(engineDir)) {
    need <- c("reviewer.R", "sc-plot-kit/scripts/plot_all_v2.R",
              "sc-ppt-kit/scripts/make_ppt.py")
    for (f in need) {
      if (!file.exists(file.path(engineDir, f)))
        problems <- c(problems, paste0("引擎里找不到：", f))
    }
  }
  list(ok = !length(problems), problems = problems)
}
