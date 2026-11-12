FLOW_SPEC_DIR <- local({
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(f)) dirname(normalizePath(f, mustWork = TRUE)) else file.path(getwd(), 'engine')
})
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

# 本文件必须自带 %||%：它会被 serve.R / reviewer.R / verdict.R / contract.R
# 单独 sys.source 到各自的空环境里，那边的定义传不过来。
#   缺了它的表现是「加载到文件末尾才报 could not find function」——
#   前面的定义全都成立，看起来像文件没问题。这种错最难查，
#   所以宁可在这里重复定义一次，也不依赖调用方提供。
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}

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
    # --outdir 必须指到 1.doublet，不能给 work_root 根目录。
    #   为什么：soupX_doublefinder.R 只做 paste0(outdir,'/',sample,'.rds') 直接写，
    #   它自己不会建子目录、也不会把产物分拣进 1.doublet。
    #   给根目录的后果是 7 个 rds 平铺在结果根下，1.doublet 里还是上一次的旧文件，
    #   而 artifacts 校验的是 1.doublet/<sample>.rds —— 于是"跑完了但产物没刷新"。
    args = c("--datadir", "{liny_data}/", "--sample", "{samples}",
             "--outdir", "{work_root}/1.doublet/", "--soupx", "N",
             "--doublefinder", "Y",
             # 物种不能写死。
             #   原来这里就是 "Mouse"：换成人样本时它不会报错，
             #   而是拿小鼠的参数去跑人类数据 —— 错得看不出来。
             #   物种属于"这批数据是什么"，由分析记录给，随命令下发。
             "--species", "{species}"),
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
    id = "filter", title = "筛群与剔样本后重聚类", skill = "liny-filter", source = "app",
    note = "按人定的筛群与剔样本结果重跑，产出 7.after_drop。dims / resolution 从参数文件读。",
    # ---- 2026-11-03 改：这一步改由应用下发 ----
    #   原来指向服务器上的 {work_root}/drop_and_recluster.R，里面写着
    #     FindNeighbors(dims = 1:20)
    #     FindClusters(resolution = 0.6)
    #   而应用页面上有参数面板，人选了、存进了 variants_<sid>.json，
    #   命令里却永远不出现 —— 脚本根本不读它。
    #   于是"选了 0.9"和"实际跑 0.6"之间隔着一层看不见的假象，
    #   而且结果看着正常，是最难发现的一类错。
    #   现在用应用自带的 04_filter.R：剔除清单、dims、resolution
    #   全部从 7.after_drop/drop_params.json 读，读不到才退默认值，
    #   并把"实际用了哪个、来自哪里"写进 recluster_params_used.xls 供复盘。
    script = "{engine}/sc-pipeline/scripts/04_filter.R",
    args = c("{work_root}",
             "--param", "{work_root}/7.after_drop/drop_params.json"),
    artifacts = c("7.after_drop/merged_after_drop.rds",
                  "7.after_drop/cluster_stats_after_drop.xls",
                  "7.after_drop/recluster_params_used.xls"),
    deps = c("5.markers/cluster_stats.xls", "3.merge/merge_7sample.rds"),
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
    # final_object.rds 必须列进来：注释环（annotate）吃的就是它，
    #   而且它真实存在于服务器上（798 MB，见历史 artifacts.json）。
    #   原来这里只写了两个 xls，于是「传对象给下游」变成了隐式约定 ——
    #   依赖自检一跑就报「final 不产出 final_object.rds」。
    #   这正是断链自检存在的意义：不是下游写错，而是上游漏登记。
    artifacts = c("8.final/cell_ratio_by_cluster.xls",
                  "8.final/cell_count_by_sample.xls",
                  "8.final/final_object.rds",
                  # ---- 2026-11-03 补登：这三件一直都在，只是没登记 ----
                  #   06 fullmark 与 07 diff 都写了 cover_by = "final"、
                  #   covered_artifacts 指向下面这三件。可是 final 自己的
                  #   artifacts 里从来没有它们 —— 于是出现一个自相矛盾的状态：
                  #   「这一环的产出由上游覆盖」这句声明，引用了上游没声明过的产物。
                  #   断链自检之所以没报，是因为它核的是「下游要的东西上游有没有声明」，
                  #   而 cover_by 这条路径没走那次检查。
                  #   实测确认这三件真实存在（本地镜像 8.final/ 下）：
                  #     all_markers.xls                  20,545 行 / 1,369,861 B
                  #     top15_markers.xls                   361 行 /    17,847 B
                  #     diff_MP_vs_Mock_by_cluster.xls   72,348 行 / 5,337,870 B
                  #   注意：登记它们只是让声明自洽，不等于「口径已经对得上」。
                  #   实测这三件与 06/07 原本要的东西并不完全等价，见两处 cover_by 的注释。
                  "8.final/all_markers.xls",
                  "8.final/top15_markers.xls",
                  "8.final/diff_MP_vs_Mock_by_cluster.xls"),
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
    # ---- 已由终版覆盖：保留流程位置，但不再重算 ----
    #   为什么保留而不是删掉：
    #     下游 figs / enrich 的 deps 还指着 9.full，删掉会让依赖链断成一截，
    #     也会让历史记录里少一步、事后对不上"本来该有几步"。
    #   为什么不再重算：
    #     final_run.R 里已经跑过 FindAllMarkers，产物落在
    #     8.final/all_markers.xls 与 8.final/top15_markers.xls。
    #   2026-11-03 更正：上面这句「和这一步要的东西是同一份」当时没核过，现在核了，
    #     并不完全成立。三点实测事实：
    #     ① full_marker.R 在服务器上不存在、在脚本库里也没有 —— 也就是说这一步
    #        「原本要产什么」从来没有被定义过，cover_by 是事后追认的，不是比对结论。
    #     ② 8.final/all_markers.xls 实测 20,544 个(簇,基因)对；而注释环节
    #        02_annotate.R 内部又跑了一次 FindAllMarkers，产 9.annot/all_markgene.xls
    #        24,169 个(簇,基因)对。两份共有 20,490、只在注释那份里多 3,679、
    #        只在终版那份里多 54 —— 阈值痕迹也对不上（终版最小 pct.1=0.046 /
    #        pct.2=0.001，注释那份 pct.1=0.02 / pct.2=0）。差一个数量级的
    #        过滤强度，说明两次调用不是同一组参数。
    #     ③ 所以现在真实在用的是两份表，不是一份：终版的给报告用，注释的给分型用。
    #        这本身是可接受的（用途不同），但不能再写成「就是同一份」。
    #   结论：cover_by 保留（真的不必再算第三遍），但措辞改成「终版已产出可用的
    #     marker 表」，不再声称与 9.full 的同名产物口径一致。要真正对齐口径，
    #     得先把 full_marker.R 写出来并定参，那是另一件事，不在本轮。
    cover_by = "final",
    covered_artifacts = c("8.final/all_markers.xls", "8.final/top15_markers.xls"),
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
    # 同上：终版已按簇逐个做过组间差异（8.final/diff_MP_vs_Mock_by_cluster.xls），
    #   而且比这里定义的"一张全量表"更细。不再重算。
    # 2026-11-03 核对后要补一句更要紧的：这里的「覆盖」是口径不同，不是同一份。
    #   ① full_diff.R 同样在服务器上不存在、库里也没有登记 —— 这一步原本要产什么，
    #      从来没有被定义过。cover_by 是事后追认，不是比对结果。
    #   ② 实测 8.final/diff_MP_vs_Mock_by_cluster.xls：72,347 行，24 个簇，每簇一份，
    #      是按「簇 × 组别」的 FindMarkers，不是一张总表。
    #   ③ 出图与富集真正消费的是另一批：10.figs/4.diff_enrich/diff/ 下 6 份
    #      *_{群}_MicroplasticsvsMock_diffgene.xls（按细胞类型 × 组别重算的）。
    #      也就是说「按簇的两组差异」（8.final）与「按细胞类型的组间差异」（10.figs）
    #      是两套不同粒度的东西，都在用，互不替代。
    #   结论：07 不重算可以保留（那张 9.full 总表没人真正需要），但必须承认
    #     「组间差异」这件事在这条流程里被做成了两份、粒度不同，而不是"一份已覆盖"。
    #     要统一口径，得等注释稳定后按细胞类型重跑一次，那是后续的事。
    cover_by = "final",
    covered_artifacts = c("8.final/diff_MP_vs_Mock_by_cluster.xls"),
    subs = list(
      list(id = "diff", title = "FindAllMarkers 组间"),
      list(id = "write", title = "写差异表")
    )
  ),
  list(
    id = "annotate", title = "细胞类型注释", skill = "sc-pipeline",
    note = "用参考表的 marker 给每个簇定细胞类型，写回 annot 列。出图的差异段与富集段都要求这一列。",
    # 为什么要有这一步：
    #   出图脚本的 diff 段与 enrich 段都按细胞类型分组，条件写的是
    #   ccol（默认 annot）必须在该对象的 meta.data 里。终版对象没有这一列，
    #   两段就被静默跳过 —— 图和比例有，火山图和富集没有，看起来像脚本坏了。
    #   所以注释不能省，它不是一个可选的补充步骤，是这两段的前提。
    script = "{engine}/sc-pipeline/scripts/02_annotate.R",
    # 参数必须由应用给出，不能留给执行端猜。
    #   原来这里是 character(0)，理由是"脚本自己知道默认值"。
    #   实际后果：执行端只能靠它自己那张表去拼，而那张表是另一份，
    #   拼出来的路径是 <结果目录>/02_annotate.R —— 那里从来没有这个脚本，
    #   于是每次点击都报「模板脚本不存在」，人看到按钮"点了没用"。
    #   这里给全：输入对象、输出目录、注释依据。
    # 打分参数必须逐项写在 args 里，不能靠脚本默认值。
    #   为什么：flow_spec 是下发的唯一权威，脚本默认值改了而这里没改，
    #   实际跑的还是旧口径 —— 而"实际用了多少"没人看得出来。
    #   2026-11-04：候选池 30→100（并按排名加权）、命中门槛 1→2、
    #   新增置信比 1.2（第一名/第二名不够领先就留空），未定型超过 50% 判 LOW。
    args = c("{work_root}/8.final/final_object.rds",
             "--outdir", "{work_root}/9.annot",
             "--top-n", "100",
             "--min-score", "2",
             "--min-ratio", "1.2",
             "--max-untyped-pct", "50",
             "--reference", "{reference_xlsx}"),
    outdir = "9.annot",
    # annot_quality.json 必须登记：它是这一环「结果能不能用」的结论，
    #   下一步出图、以及最后的 PPT 都要读它来决定要不要把注释当结论。
    #   不登记的话，下游只能靠 annot_result.xls 去猜 —— 而那是一张逐群明细表。
    artifacts = c("9.annot/annotated.rds", "9.annot/annot_result.xls",
                  "9.annot/celltype_count.xls", "9.annot/annot_quality.json"),
    deps = c("8.final/final_object.rds"),
    subs = list(
      list(id = "dict", title = "载入文献词典"),
      list(id = "score", title = "按 marker 打分判类型"),
      list(id = "write", title = "写回 annot 列")
    )
  ),
  list(
    # skill 名必须是"实际跑的是谁"，不能填一个听起来像官方的。
    #   这里实际跑的是应用自带的 sc-plot-kit，不是 liny 官方脚本。
    #   以前写 liny-figs，等于在元数据里说了一件不真实的事：
    #   审计时按 liny-figs 去核，永远核不出问题，因为压根没跑那个。
    id = "figs", title = "出图", skill = "sc-plot-kit", source = "app",
    note = "UMAP / dotplot / violin / 比例 / 火山 / 质控 / 热图，落 10.figs。",
    # 脚本名改成应用里真实存在的那个。
    #   原来写 {work_root}/make_figs3.R —— 这个名字在任何地方都不存在，
    #   是流程定义时臆造的，于是这一步被误判成"服务器上没有脚本、跑不了"。
    #   实际出图能力一直在应用里：engine/sc-plot-kit/scripts/plot_all_v2.R，
    #   qc/cluster/markgene/annot/ratio/diff/enrich 七段俱全。
    #   现在由 C_supplement 按需下发（见 C_supplement/index.json 的 C_plot_figs）。
    #   2026-11-03 更正：这里原写 library/B_analysis/plots/plot_all_v2.R，还写了
    #     "562 行" —— 行数早已不对，而且库里那份是 2026-10-21 的旧副本，
    #     不认识 --species，从它取参数富集段必然静默跳过。权威副本只有
    #     sc-plot-kit/scripts/ 这一份，library 下那份已同步为同一文件（md5 相同），
    #     仅作库登记与分发用，不再是第二个实现。
    script = "{engine}/sc-plot-kit/scripts/plot_all_v2.R",
    args = c("{work_root}/9.annot/annotated.rds",
             "--outdir", "{work_root}/10.figs",
             # markgene 这一段原来漏在参数里 —— 脚本支持它（2.markgene 里出
             #   热图、featureplot、violin 三张），但命令从来不带这个 stage，
             #   于是这三张图从来没被生成过。而 PPT 有整整三页要用它们，
             #   到出 PPT 那一步才发现缺，只能拿别的图顶上或跳过。
             #   缺的图要在出图这一步就出全，不能留到出 PPT 再发现。
             "--stage", "qc,cluster,markgene,annot,ratio,diff",
             "--group-col", "group",
             "--sample-col", "orig.ident",
             "--compare", "MicroplasticsvsMock",
             # 物种随命令下去：脚本不该靠"读不到就跳过"来决定跑不跑。
             "--species", "{species}"),
    outdir = "10.figs",
    artifacts = c("10.figs/0.QC", "10.figs/1.cluster", "10.figs/2.markgene",
                  "10.figs/3.cell_ratio", "10.figs/4.diff_enrich"),
    # 输入改成注释后的对象：差异段与富集段都要 annot 列，
    #   直接读终版对象会静默跳过那两段（图有、火山图和富集没有）。
    deps = c("9.annot/annotated.rds"),
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
    # 同上：富集用的是应用自带的 sc-plot-kit，官方那套是
    #   cluster_diff_enrich.sh + cluster_profile_enrich.R，这里没在跑它。
    #   要换成官方实现，得先确认它与 annot 列、分组字段对得上。
    id = "enrich", title = "GO 富集", skill = "sc-plot-kit", source = "app",
    note = "对差异基因做 GO-BP 富集并出 dotplot。",
    # 同上：富集段本来就在 plot_all_v2.R 里（enrich 分支），
    #   依赖上一步产出的差异表；不是另有一个 make_enrich2.R。
    script = "{engine}/sc-plot-kit/scripts/plot_all_v2.R",
    args = c("{work_root}/9.annot/annotated.rds",
             "--outdir", "{work_root}/10.figs",
             "--stage", "enrich",
             "--group-col", "group",
             "--sample-col", "orig.ident",
             "--compare", "MicroplasticsvsMock",
             # 富集必须知道物种：库按物种选，读不到就只能跳过。
             #   以前靠 --config 传，而下发命令从来不带 --config，
             #   于是每次都"跑完、零产物"。这就是富集没有结果的直接原因。
             "--species", "{species}"),
    outdir = "10.figs",
    artifacts = c("10.figs/4.diff_enrich/enrich"),
    deps = c("10.figs/4.diff_enrich/diff"),
    subs = list(
      list(id = "go", title = "GO-BP 富集"),
      list(id = "plot", title = "出 dotplot")
    )
  )
)

# ---------------------------------------------------------------------
#  第 11、12 环：PPT 与打包
#
#  为什么以前没有这两步、现在必须有
#    旧理由写在 SERVER_MISSING 里：「服务器上没有 PPT 与打包这一步，
#    就不写它。写一个应该存在的阶段，页面会一直显示待跑，
#    而实际根本没有对应脚本。」
#    这个理由不成立。脚本一直在应用里，本来就不需要在服务器上：
#      engine/sc-ppt-kit/scripts/make_ppt.py   （纯标准库）
#      engine/sc-ppt-kit/templates/template.pptx
#    同一个文件里的 figs 与 enrich 就是靠 source="app" 声明
#    「脚本在应用侧、由应用下发」跑起来的，PPT 与它们同类。
#    真正的后果不是「页面显示待跑」，而是「这两步根本没有被声明，
#    所以应用不会自动补 —— 不是忘了补，是流程定义里没有这一格」。
#
#  PPT 的参数契约与 local 档不同，不能照搬（清单 D3）
#    local 档给的是 --result-dir {outdir}/plots 加 --config pipeline.json。
#    server 档没有 plots/、也没有 pipeline.json；图落在
#    10.figs/1.cluster、3.cell_ratio、4.diff_enrich。
#    照搬 local 命令的结果是「跑完了但一张图都没进 PPT」或直接报目录不存在。
#    所以这里改成 --result-dir 指 10.figs，不传 --config，用 --title 给封面标题。
#    make_ppt.py 的 --config 本来就是可选的，--spec 有默认值，这条路可行。
# ---------------------------------------------------------------------
SERVER_STAGES <- c(SERVER_STAGES, list(
  list(
    id = "ppt", title = "生成交付 PPT", skill = "sc-ppt-kit", source = "app",
    note = "把 10.figs 的图排成交付 PPT，落 11.report/analysis_report.pptx。",
    script = "{engine}/sc-ppt-kit/scripts/make_ppt.py",
    args = c("--result-dir", "{work_root}/10.figs",
             "--template", "{engine}/sc-ppt-kit/templates/template.pptx",
             "--out", "{work_root}/11.report/analysis_report.pptx",
             "--title", "单细胞转录组个性化分析"),
    outdir = "11.report",
    artifacts = c("11.report/analysis_report.pptx"),
    # 输入是出图这一步的产物，不是 9.annot 的对象：
    #    PPT 要的是图，不是表达矩阵。
    deps = c("10.figs/1.cluster", "10.figs/3.cell_ratio", "10.figs/4.diff_enrich"),
    subs = list(
      list(id = "plan", title = "读图清单与页序"),
      list(id = "layout", title = "套模板排版"),
      list(id = "write", title = "写 pptx 并自检")
    )
  ),
  list(
    id = "pack", title = "打包交付物", skill = "sc-ppt-kit", source = "app",
    note = "把出图与 PPT 打成一个 zip，落 11.report/analysis_results.zip。",
    script = "zip",
    args = c("-r", "-q", "{work_root}/11.report/analysis_results.zip",
             "{work_root}/10.figs", "{work_root}/11.report/analysis_report.pptx"),
    outdir = "11.report",
    artifacts = c("11.report/analysis_results.zip"),
    deps = c("10.figs", "11.report/analysis_report.pptx"),
    subs = list(
      list(id = "stage", title = "收集产物"),
      list(id = "zip", title = "打 zip 并校验")
    )
  )
))

SERVER_RUN_ORDER <- vapply(SERVER_STAGES, function(s) s$id, "")

# 服务器上确实永远不做的项，与「漏登记」分开记。
#   清空不是「没有缺项」，而是「一个都不该缺」：
#     ppt / pack 现在由应用侧下发脚本，不需要服务器上有对应程序。
#    以后真出现「这件事服务器上物理做不到」的环节，登记在这里并写清原因，
#     不要用「写了会显示待跑」当理由把它藏起来 —— 那是页面观感问题，
#     应该修页面，不该从流程里删掉一步。
SERVER_MISSING <- character(0)

# ---------------------------------------------------------------------
#  每环的验收契约（清单 E1 / I2 / L1 第 1 项）
#
#  为什么放在 flow_spec 里、而且只放一处
#    以前五处各写一份阶段信息：flow_spec（权威）、step_plan（老八名）、
#    reviewer.STAGE_SKILL（两套十八个）、C_supplement/index.json（for_stage）、
#    serve.R 的 STEP_TASK（中文标题）。
#    于是出现「PPT 补不上」这类问题：不是补不上，是有一处根本没登记。
#    对账器 contract.R 早就写好了，但没接进任何流程，所以差异一直存在。
#    本段的唯一作用就是让 L1–L4 四层判定只读这一处。
#
#  字段含义
#    products          这一步必须产出的全集，缺一件即 fail，不做「部分完成」
#    min_bytes         小于这个体积基本是空文件或写了一半
#    required_columns  表头必需列（只对表格类产物生效）
#    invariants        守恒约束的文字描述；有可核计数时才会真的判
#    skill             规定使用的 skill（L3 合规层比对 set_id）
#    analysis          分析层规则（L4）；声明了但没有可核数据时判 unknown
# ---------------------------------------------------------------------
FLOW_REVIEW <- list(
  doublet = list(
    products = c("1.doublet/{samples}.rds"),
    # 逐样本环节：产物件数必须等于样本数，且每个样本各一份。
    #   为什么必须声明这一条：只写 {samples} 通配的话，判定器只能回答
    #   「有没有至少一件 .rds」，回答不了「七份是不是都交齐了」，
    #   更抓不住「拿一个名字不在样本清单里的文件冒充产物」。
    #   而缺一份、拿旧文件改名顶替，正是最常见的假完成形态。
    per_sample = TRUE,
    samples_key = "samples",
    min_bytes = 1024,
    required_columns = character(0),
    invariants = c("去双后细胞数 <= 输入细胞数", "每个样本各一份 rds，件数等于样本数"),
    skill = "liny-doublet",
    analysis = list(rules = c("doublet_rate_in_range")),
    env_required = TRUE
  ),
  merge = list(
    products = c("3.merge/merge_7sample.rds"),
    min_bytes = 1048576,
    invariants = c("合并后细胞数 = 各样本之和（允许 ±0）"),
    skill = "liny-merge",
    analysis = list(rules = c("cluster_count_in_range", "batch_corrected"))
  ),
  markers = list(
    products = c("5.markers/cluster_stats.xls", "5.markers/top15_markers.xls"),
    min_bytes = 128,
    required_columns = c("cluster"),
    invariants = c("簇数 = cluster_stats 行数"),
    skill = "liny-markers",
    analysis = list(rules = c("markers_not_empty", "markers_not_all_genes"))
  ),
  filter = list(
    products = c("7.after_drop/merged_after_drop.rds"),
    min_bytes = 1048576,
    invariants = c("剔除后细胞数 <= 剔除前"),
    skill = "liny-filter",
    analysis = list(rules = c("dropped_samples_recorded"))
  ),
  final = list(
    products = c("8.final/cell_ratio_by_cluster.xls",
                 "8.final/cell_count_by_sample.xls",
                 "8.final/final_object.rds"),
    min_bytes = 128,
    required_columns = c("cluster"),
    invariants = c("各样本细胞数合计 = 总细胞数", "各簇比例合计 = 1"),
    skill = "liny-final",
    analysis = list(rules = c("ratio_sums_to_one"))
  ),
  fullmark = list(
    products = c("9.full/all_markers_full.xls", "9.full/top15_markers_full.xls"),
    min_bytes = 128,
    required_columns = c("cluster"),
    invariants = c("marker 表非空"),
    skill = "liny-markers",
    analysis = list(rules = c("markers_not_all_genes")),
    # 已由 final 覆盖：这一环的产物可以由 8.final 的同名内容顶替。
    covered_by = "final"
  ),
  diff = list(
    products = c("9.full/diff_MP_vs_Mock_full.xls"),
    min_bytes = 128,
    required_columns = c("cluster"),
    invariants = c("差异基因非空"),
    skill = "liny-diff",
    analysis = list(rules = c("diff_not_all_genes")),
    covered_by = "final"
  ),
  annotate = list(
    products = c("9.annot/annotated.rds", "9.annot/annot_result.xls",
                 "9.annot/celltype_count.xls"),
    min_bytes = 128,
    # 这一条是本清单里唯一被真实数据打红过的列检查：
    #   注释环 RC=1 的直接原因就是 annot_result.xls 缺 Positive Markers 列。
    required_columns = c("cluster", "cell_type", "Positive Markers"),
    # 列检查只针对这一张表。
    #   为什么不写这一行会出事：required_columns 会被套到本环全部产物上，
    #   annotated.rds 是二进制对象、没有表头，于是「正常结果」也会被判缺列。
    #   判据写错比产物错更危险 —— 它会把对的判成错的，逼着人放宽规则。
    columns_file = "annot_result.xls",
    invariants = c("每个簇都有且只有一个细胞类型", "细胞类型数 <= 簇数"),
    skill = "sc-pipeline",
    analysis = list(rules = c("markers_in_dictionary", "celltype_count_in_range"),
                    species_match = TRUE),
    env_required = TRUE
  ),
  figs = list(
    products = c("10.figs/1.cluster", "10.figs/3.cell_ratio", "10.figs/4.diff_enrich"),
    min_bytes = 1024,
    invariants = c("每套图至少一张 png", "UMAP 与 dotplot 都存在"),
    skill = "sc-plot-kit",
    analysis = list(rules = c("figures_non_empty", "umap_present"))
  ),
  enrich = list(
    products = c("10.figs/4.diff_enrich/enrich"),
    min_bytes = 512,
    invariants = c("富集条目非空"),
    skill = "sc-plot-kit",
    analysis = list(rules = c("enrich_not_empty"))
  ),
  ppt = list(
    products = c("11.report/analysis_report.pptx"),
    min_bytes = 102400,
    invariants = c("pptx 可被 zipfile 打开", "ppt/slides/slide*.xml 数量 > 0",
                   "每张 slide 至少一个图片引用"),
    skill = "sc-ppt-kit",
    analysis = list(rules = c("slides_match_fig_count"))
  ),
  pack = list(
    products = c("11.report/analysis_results.zip"),
    min_bytes = 1024,
    invariants = c("zip 内含 10.figs 全部图", "zip 内含 analysis_report.pptx"),
    skill = "sc-ppt-kit",
    analysis = list(rules = c("zip_manifest_consistent"))
  )
)

# 把 review 段挂到每个 server 阶段上。
#   为什么用挂载而不是在 12 个 list 里各写一遍：
#     写成内联的话，同一个字段会在 12 处出现，改一处忘一处就是新的不一致来源。
#     挂载也仍然是「flow_spec 里声明」，判据的唯一权威没有变。
for (.i in seq_along(SERVER_STAGES)) {
  .id <- SERVER_STAGES[[.i]]$id
  SERVER_STAGES[[.i]]$review <- FLOW_REVIEW[[.id]]
}

FLOW_REVIEW_OF <- function(id, profile = c("server", "local")) {
  profile <- match.arg(profile)
  st <- flow_stage(id, profile)
  if (is.null(st)) return(NULL)
  st$review
}

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
# One resolver for prompt and queue. It performs no analysis or remote IO.
flow_execution <- function(st, V) {
  if (is.null(st) || is.null(st$script))
    return(list(ok = FALSE, errors = "阶段未登记可执行脚本"))
  # ---- 已被上游覆盖的环节：不发命令 -------------------------------------
  #   为什么放在这里判（2026-11-03，审核第 2 条）
  #     fullmark / diff 写着 cover_by = "final"，意思是「终版已有可用的表，
  #     这两步不必再算」。但它们同时留着 script = "{work_root}/full_marker.R"，
  #     而那个脚本在服务器上、在脚本库里都不存在（flow_spec 自己的注释里
  #     也承认了）。于是变成一个死循环：
  #       页面显示"已由终版覆盖" → 按钮还在 → 点一次 → 脚本不存在 → 失败
  #       → 失败记录盖掉"已覆盖" → 看起来像退步了 → 再点一次 → 再失败。
  #     exec_tasks.json 里 annotate 连续 9 次失败就是同一个模式（同一原因重复点）。
  #   放在 flow_execution 而不是 page 上判，是因为这里是所有下发路径的必经口：
  #     brief、/api/run、执行端自取，都从这里过。判一次，全部生效。
  if (nzchar(as.character(st$cover_by %||% ""))) {
    cb <- as.character(st$cover_by)
    arts <- as.character(st$covered_artifacts %||% character(0))
    return(list(ok = FALSE, covered_by = cb, covered = TRUE,
      errors = paste0("这一环已由「", cb, "」覆盖",
                      if (length(arts)) paste0("（", paste(arts, collapse = "、"), "）") else "",
                      "，不需要再执行；它不是可运行的一步，点它只会重复失败。")))
  }
  templates <- c(st$script, st$args %||% character(0))
  tokens <- unique(unlist(regmatches(templates, gregexpr("\\{[A-Za-z_][A-Za-z0-9_]*\\}", templates))))
  keys <- unique(c("rscript", gsub("[{}]", "", tokens)))
  # Only a genuinely empty scalar enables detection, and only on a stage that consumes species.
  # Keep the original target/argv inside the common command resolver so brief and queue agree.
  auto_species <- "species" %in% keys && is.character(V$species) && !is.object(V$species) &&
    is.null(dim(V$species)) && length(V$species) == 1L && !is.na(V$species) &&
    !nzchar(trimws(V$species))
  if (auto_species) {
    if (is.null(V$engine) || length(V$engine) != 1L || is.na(V$engine) || !nzchar(trimws(V$engine)))
      return(list(ok = FALSE, errors = "自动识别物种需要运行引擎目录", missing = "engine"))
    V$species <- "auto"
  }
  # flow_vars canonicalizes aliases; direct callers must not lose extra values or object shape.
  if ("species" %in% keys &&
      (!is.character(V$species) || is.object(V$species) ||
       !is.null(dim(V$species)) || length(V$species) != 1L))
    return(list(ok = FALSE, errors = "物种必须是单个规范字符串：Mouse / Human / Rat。",
                missing = "species"))
  missing <- keys[vapply(keys, function(k) is.null(V[[k]]) ||
    !length(V[[k]]) || is.na(V[[k]][1]) || !nzchar(trimws(as.character(V[[k]][1]))), logical(1))]
  if (length(missing)) return(list(ok = FALSE, errors = paste0("缺少运行输入：", missing), missing = missing))

  # ---- 值不合法也要拦，不只是"没填" --------------------------------
  #   为什么加这一条（2026-11-03，审核第 7 条）
  #    species 为空时上面能拦住，但填了「小鼠」这种中文、或拼错的 "mouce"，
  #    会原样下发：脚本的库映射表认不得，于是富集段静默跳过，
  #    跑完零产物、退出码 0、页面显示完成 —— 这是最难查的一类失败。
  #    实测 10-30 那次「跑完、零产物」就是没带物种导致的。
  #  只对少数"定生死"的参数做值校验：它们错了不是结果差一点，是整段不算数。
  if ("species" %in% keys) {
    sp <- as.character(V$species[1])
    if (!grepl("^(Mouse|Human|Rat)$", sp) && !auto_species)
      return(list(ok = FALSE,
        errors = paste0("物种值不可用：", if (nzchar(sp)) paste0("「", sp, "」") else "（空）",
                        "。富集与去双都按物种选注释库，认不得的值会让那几段静默跳过、",
                        "跑完零产物却报成功。执行输入只接受 Mouse / Human / Rat；会话别名须先由 flow_vars 规范化。"),
        missing = "species"))
  }
  repl <- function(x) {
    out <- as.character(x)
    for (k in keys) out <- gsub(paste0("{", k, "}"), as.character(V[[k]][1]), out, fixed = TRUE)
    out
  }
  script <- repl(st$script)
  argv <- repl(st$args %||% character(0))
  if (identical(st$id, "annotate")) {
    i <- match("--reference", argv)
    if (!is.na(i) && length(argv) > i && grepl("\\.json$", argv[i + 1], ignore.case = TRUE)) argv[i] <- "--lit-dict"
  }
  if (auto_species) {
    detector <- file.path(FLOW_SPEC_DIR, "species_exec.R")
    if (!file.exists(detector)) return(list(ok = FALSE, errors = "自动识别程序缺失", missing = "species_detector"))
    code <- paste(readLines(detector, warn = FALSE), collapse = "\n")
    # Source-safe definitions plus explicit entry; queue and brief carry identical bytes.
    code <- strsplit(code, "if (sys.nframe() == 0L) {", fixed = TRUE)[[1L]][1L]
    code <- paste0(code, "\nquit(status = tryCatch(species_exec_main(commandArgs(TRUE)), error = function(e) { message(conditionMessage(e)); 1L }))")
    raw <- if (nzchar(as.character(V$liny_data %||% "")) && nzchar(as.character(V$samples %||% "")))
      c("--species-raw-input", V$liny_data, V$samples) else character(0)
    argv <- c(code, "--args", "--target", script, "--", argv, raw)
    script <- "-e"
  }
  list(ok = TRUE, step = st$id, script = script, argv = argv,
       command = paste(vapply(c(flow_interp(script, V), script, argv),
                              shQuote, "", type = "sh"), collapse = " "))
}

# ---------------------------------------------------------------------
#  解释器按「脚本是什么」选，不按「档位是什么」选
#
#  为什么必须这样（2026-11-03，代码级审核第 1 条）
#    原来这里无条件用 V$rscript 前缀。十二个阶段里十个是 .R，看不出问题；
#    另外两个不是：
#      ppt  → make_ppt.py    → 实际下发成  Rscript make_ppt.py
#      pack → zip            → 实际下发成  Rscript zip -r -q ...
#    两条命令都必然失败，而且报错是 R 的语法错，看起来像"脚本坏了"，
#    不像"解释器用错了"。一整套流程永远走不到交付物，根子在这四个字。
#
#  判定顺序
#    1. 应用自带命令（zip 这类无后缀的）：直接当命令执行，不加任何解释器。
#    2. 有 .R/.r 后缀：用配置里的 Rscript。
#    3. 有 .py 后缀：用配置里的 python，没配就退 python3。
#    4. 有 .sh 后缀：用 bash / sh。
#    5. 其余纯文件名（服务器上的 .R 脚本也可能不带后缀？不会）：
#       按 R 处理，保持与改动前一致，避免把没想到的情况改坏。
# ---------------------------------------------------------------------
flow_interp <- function(script, V = list()) {
  s <- as.character(script %||% "")
  base <- basename(s)
  if (identical(s, "-e")) return(as.character(V$rscript %||% "Rscript"))
  # 应用侧命令：zip / tar 这类，脚本位就是命令本身，不存在解释器。
  if (!grepl("\\.", base))
    return(character(0))
  if (grepl("\\.(R|r)$", base))
    return(as.character(V$rscript %||% "Rscript"))
  if (grepl("\\.(py)$", base, ignore.case = TRUE)) {
    py <- as.character(V$python %||% "")
    return(if (nzchar(py)) py else "python3")
  }
  if (grepl("\\.(sh)$", base, ignore.case = TRUE))
    return("bash")
  as.character(V$rscript %||% "Rscript")
}

flow_server_cmd <- function(st, V, use_qsub = TRUE) {
  if (is.null(st) || is.null(st$script)) return(NULL)
  execution <- flow_execution(st, V)
  if (!isTRUE(execution$ok)) return(NULL)
  base <- execution$command
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

# ---------------------------------------------------------------------
#  依赖声明与断链自检（F4 / G3）
#
#  要解决的问题
#    deps 原来是一串静态字符串，比如 c("8.final/final_object.rds")。
#    它只能表达「我要这个」，表达不了「谁给的」。于是：
#      · 删掉一个冗余环节，下游的 deps 仍然写着那条路径，没人发现它已经没人产出；
#      · 改一个产物名，下游不会跟着改，也不会报错 —— 它只是永远找不到。
#    这两种情况的共同点是「静默」：流程照样能点，点下去才失败，
#    而且失败信息说的是「找不到文件」，不说「上游断了」。
#
#  现在怎么表达
#    produces  这一环产出什么（就是 artifacts，语义更明确的一份视图）
#    consumes  这一环吃谁的什么（可写成三种形式）
#      stage:<id>          要某个上游环节整环就绪（最常见）
#      stage:<id>/<rel>    要某个上游环节的某一件产物
#      <rel>               直接写路径时，由解析器反查是哪个环节产出的
#    三种形式最后都解析到「(生产者环节, 产物)」，解析不到就是断链。
#
#  为什么不用真正的表达式求值
#    这是配置文件，不是代码。允许任意表达式意味着「流程定义可以被执行」，
#    那是另一类风险（注入、隐式 IO）。这里只做有限形式的解析，
#    覆盖面足够，且每一条都能被人读懂。
# ---------------------------------------------------------------------
FLOW_CONSUMES <- list(
  # 第一环不依赖任何上游：它吃的是原始矩阵与样本表，不在这条链里产出。
  doublet  = character(0),
  merge    = c("stage:doublet"),
  markers  = c("stage:merge/3.merge/merge_7sample.rds"),
  filter   = c("stage:markers/5.markers/cluster_stats.xls"),
  final    = c("stage:filter/7.after_drop/merged_after_drop.rds"),
  fullmark = c("stage:final"),
  diff     = c("stage:final"),
  annotate = c("stage:final/8.final/final_object.rds"),
  figs     = c("stage:annotate/9.annot/annotated.rds"),
  enrich   = c("stage:figs/10.figs/4.diff_enrich/diff"),
  ppt      = c("stage:figs/10.figs/1.cluster",
               "stage:figs/10.figs/3.cell_ratio",
               "stage:figs/10.figs/4.diff_enrich"),
  pack     = c("stage:figs", "stage:ppt/11.report/analysis_report.pptx")
)

# 把 consumes 声明解析成「生产者环节 + 产物」
#   返回 list(ok, producer, artifact, why)
flow_deps_resolve <- function(one, order, produces_of) {
  one <- trimws(as.character(one %||% ""))
  if (!nzchar(one)) return(list(ok = FALSE, why = "空声明"))
  if (startsWith(one, "stage:")) {
    rest <- sub("^stage:", "", one)
    sp <- regexpr("/", rest, fixed = TRUE)
    if (sp < 0) {
      # 整环依赖：该环节存在即可
      if (rest %in% order) return(list(ok = TRUE, producer = rest, artifact = NULL))
      return(list(ok = FALSE, why = paste0("引用了不存在的上游环节：", rest)))
    }
    sid <- substr(rest, 1, sp - 1)
    rel <- substr(rest, sp + 1, nchar(rest))
    if (!sid %in% order)
      return(list(ok = FALSE, why = paste0("引用了不存在的上游环节：", sid)))
    pd <- produces_of[[sid]] %||% character(0)
    # 产物可以是模板（含 {samples}），比对时按前缀/通配放宽
    hit <- any(vapply(pd, function(p) {
      identical(p, rel) || startsWith(rel, paste0(p, "/")) ||
        startsWith(p, paste0(rel, "/")) ||
        grepl("^1\\.doublet/", rel) && grepl("\\{samples\\}", p)
    }, TRUE))
    if (!hit)
      return(list(ok = FALSE, producer = sid, artifact = rel,
                  why = paste0(sid, " 不产出 ", rel,
                               "；它产出的是：", paste(pd, collapse = "、"))) )
    return(list(ok = TRUE, producer = sid, artifact = rel))
  }
  # 直接写路径：反查谁产出
  for (sid in order) {
    pd <- produces_of[[sid]] %||% character(0)
    if (any(vapply(pd, function(p) identical(p, one) || startsWith(one, paste0(p, "/")), TRUE)))
      return(list(ok = TRUE, producer = sid, artifact = one))
  }
  list(ok = FALSE, why = paste0("没有任何上游环节产出 ", one))
}

# 整条链的依赖自检
#   返回 list(ok, order, breaks, table)
#     breaks 每一项：stage / decl / why / fix
flow_deps_check <- function(profile = "server") {
  stages <- FLOW_STAGES(profile)
  order <- vapply(stages, function(s) as.character(s$id %||% ""), "")
  produces_of <- list()
  for (s in stages) produces_of[[as.character(s$id)]] <- as.character(s$artifacts %||% character(0))

  breaks <- list(); table <- list()
  for (i in seq_along(order)) {
    sid <- order[i]
    # 只允许依赖「严格上游」：依赖自己或下游一定是写错了。
    ups <- if (i > 1L) order[seq_len(i - 1L)] else character(0)
    decls <- if (identical(profile, "server")) FLOW_CONSUMES[[sid]] %||% as.character(FLOW_STAGES(profile)[[i]]$deps %||% character(0))
             else as.character(FLOW_STAGES(profile)[[i]]$deps %||% character(0))
    if (!length(decls)) {
      table[[length(table) + 1L]] <- list(stage = sid, consumes = list(), ok = TRUE)
      next
    }
    rows <- list()
    for (d in decls) {
      r <- flow_deps_resolve(d, ups, produces_of)
      # 整环依赖：还要确认那个环节不是自己
      if (!isTRUE(r$ok) && is.null(r$why)) r$why <- "解析失败"
      rows[[length(rows) + 1L]] <- list(decl = d, ok = isTRUE(r$ok),
                                        producer = r$producer %||% NULL,
                                        artifact = r$artifact %||% NULL,
                                        why = r$why %||% "")
      if (!isTRUE(r$ok)) {
        breaks[[length(breaks) + 1L]] <- list(
          stage = sid, decl = d, why = r$why %||% "",
          fix = paste0("两条路选一条：① 把 ", d, " 指向真实存在的上游产物；",
                       "② 如果这一环本来就该拆掉，连同它的下游声明一起改。"))
      }
    }
    table[[length(table) + 1L]] <- list(stage = sid, consumes = rows, ok = all(vapply(rows, function(x) isTRUE(x$ok), TRUE)))
  }
  list(ok = length(breaks) == 0L, profile = profile, order = as.list(order),
       breaks = breaks, table = table)
}

# 页面/接口用：只回结论与断点，不回整张表（表太大）
flow_deps_status <- function(profile = "server") {
  r <- flow_deps_check(profile)
  list(ok = r$ok, stages = length(r$order),
       breaks = r$breaks,
       message = if (r$ok) "依赖链完整：每一环要的东西都有上游产出"
                 else paste0("依赖链断了 ", length(r$breaks), " 处"))
}

# ---------------------------------------------------------------------
#  每环的环境条件声明（F7）
#
#  为什么放在 flow_spec
#    这一轮在清理的正是「同一件事写在五个地方」。什么包、什么路径、几个链接，
#    属于「这一环需要什么」，和产物、skl 一样是流程定义的一部分。
#    放这里，runnable.R 只负责合成，不负责决定。
#
#  字段
#    requires_pkgs   缺一个就判「跑不了」
#    optional_pkgs   缺了只提醒，不影响能跑不能跑
#    requires_paths  指向体检条目 id（data_root / script_root / links 等）
#    requires_links  需要存在的符号链接条数
#
#  怎么定的
#    按各环实际脚本用到的包列的，不是猜的：
#      doublet  跑 SoupX/DoubletFinder，读矩阵
#      merge    Seurat 合并、Harmony 批次校正
#      markers/final/diff   差异基因用 presto（缺了走慢速路径，不阻断）
#      annotate 读参考表打分，要 Seurat 与矩阵
#      figs     出图要 ggplot2/patchwork
#      enrich   GO 富集要 clusterProfiler 与对应物种库
#      ppt      纯 Python 标准库，只要求模板文件在
# ---------------------------------------------------------------------
FLOW_ENV <- list(
  doublet = list(
    # 只列体检真的探过的包。Matrix 没被探到，列成必需会让整条链恒 unknown；
    # 它随 Seurat 一起装，缺了 Seurat 也起不来。
    requires_pkgs = c("Seurat", "DoubletFinder"),
    optional_pkgs_matrix = c("Matrix"),
    optional_pkgs = c("SoupX"),
    requires_paths = c("data_root", "script_root"),
    requires_links = 1L
  ),
  merge = list(
    requires_pkgs = c("Seurat"),
    optional_pkgs_matrix = c("Matrix", "dplyr"),
    optional_pkgs = c("harmony"),
    requires_paths = c("data_root", "script_root")
  ),
  markers = list(
    requires_pkgs = c("Seurat"),
    optional_pkgs = c("presto"),
    requires_paths = c("data_root", "script_root")
  ),
  filter = list(
    requires_pkgs = c("Seurat"),
    optional_pkgs = c("presto")
  ),
  final = list(
    requires_pkgs = c("Seurat"),
    optional_pkgs = c("presto")
  ),
  fullmark = list(
    requires_pkgs = c("Seurat"),
    optional_pkgs = c("presto")
  ),
  diff = list(
    requires_pkgs = c("Seurat"),
    optional_pkgs = c("presto")
  ),
  annotate = list(
    requires_pkgs = c("Seurat"),
    requires_paths = c("data_root")
  ),
  figs = list(
    requires_pkgs = c("Seurat", "ggplot2"),
    optional_pkgs_matrix = c("patchwork"),
    optional_pkgs = c("ggrepel")
  ),
  enrich = list(
    requires_pkgs = c("clusterProfiler"),
    # 物种库按物种取其一即可：这里都列成可选，缺两个才提醒。
    optional_pkgs = c("org.Mm.eg.db", "org.Hs.eg.db")
  ),
  # PPT 与打包用的是应用侧脚本，不依赖服务器上的 R 包。
  #   requires_pkgs 留空不是「没想过」，而是事实：这一步只用 Python 标准库。
  ppt = list(requires_pkgs = character(0)),
  pack = list(requires_pkgs = character(0))
)

FLOW_ENV_OF <- function(id) FLOW_ENV[[as.character(id)]] %||% list()
