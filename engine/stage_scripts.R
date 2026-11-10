#!/usr/bin/env Rscript
# =====================================================================
#  stage_scripts.R -- 把「这个环境里到底有哪些脚本」按环节列清楚
#
#  为什么单独一个文件
#    原来的脚本库页签把条目平铺成一张表，看上去像一堆积木：
#    分不清哪条是主流程、哪条是出图、哪条是出 PPT，也看不出
#    这条脚本到底属于哪个环节、能不能直接用。
#    这里按「环节」归组，并把路径、指纹、是否就位一起给出。
#
#  只读，不写、不改任何脚本。
# =====================================================================

# 环节定义：id、中文名、所属 skill、匹配的相对目录、后缀、这个环节干什么
#
#   desc 是给页面用的：光有环节名，看的人不知道它在流程里处于哪一步、
#   为什么要调用这个 skill。一句话说清用途，比再多的层级样式都管用。
#
#   covers 是「这个环节补的是哪一类活」的关键词，页面拿它和服务器那套
#   脚本对账：服务器已经覆盖的环节，页面就不必再列一遍应用自带的。
#     词取宽不取窄 —— 漏判只会多列几条，错判会让人以为缺东西却看不到。
#
#   infra = 机制类，不是一项能力。页面列「补充脚本」时跳过它 ——
#     人要看的是「还缺哪块活没人干」，而入口守卫是保证脚本能跑的基础设施，
#     列出来只会变成噪音。
STAGE_DEF <- list(
  list(id = "entry",   name = "统一入口", skill = "workflow",
       dir = "workflow",      ext = "\\.R$",
       covers = c("入口", "启动", "guard"),
       infra = TRUE,
       desc = "所有流程都必须从这里启动。绕过它直接跑脚本，参数来源、执行记录、产物指纹都留不下来，事后查不出用的是哪一套。"),
  list(id = "pipeline", name = "主流程（细胞层面分析）", skill = "sc-pipeline",
       dir = "sc-pipeline/scripts", ext = "\\.R$",
       covers = c("装包", "去双", "质控", "合并", "聚类", "批次", "marker", "markgene",
                  "比例", "差异", "富集", "注释", "analysis", "preprocess", "soupx"),
       desc = "逐样本读入 → 去双 → 质控 → 合并 → 批次校正 → 一次聚类 → 筛群 → 二次聚类 → 剔样本 → 文献注释。"),
  list(id = "plots",   name = "出图（六套图）", skill = "sc-plot-kit",
       dir = "sc-plot-kit/scripts", ext = "\\.R$",
       covers = c("出图", "绘图", "画图", "plot", "图片", "可视化", "火山图"),
       desc = "质控、聚类、marker、注释、细胞比例、组间差异富集。版式与配色逐张对齐客户交付 PPT。"),
  list(id = "ppt",     name = "出 PPT（交付稿）", skill = "sc-ppt-kit",
       dir = "sc-ppt-kit/scripts", ext = "\\.(py|json)$",
       covers = c("ppt", "幻灯", "slide", "交付稿", "报告"),
       desc = "把六套结果按客户那份 PPT 的版式与页序组装成 22 页 .pptx。")
 )

# 每个脚本在环节里扮演什么角色。
#   同一行里写着文件名和路径，但「这是总调度还是子模块」看不出来 ——
#   子模块不能单独跑（它要靠别人 source），这个区别直接决定能不能点「检查」。
SCRIPT_ROLE <- list(
  "workflow/guard.R"                              = "入口守卫：判定是不是从统一入口进来",
  "workflow/guard_load.R"                         = "守卫加载器：每个底层脚本的第一行",
  "sc-pipeline/scripts/00_run_all.R"              = "总调度：按顺序调下面的步骤，失败即停",
  "sc-pipeline/scripts/01_preprocess.R"           = "主流程：读入 / 去双 / 质控 / 合并 / 聚类 / 筛群",
  "sc-pipeline/scripts/02_annotate.R"             = "文献注释：marker 交集打分判定细胞类型",
  "sc-pipeline/scripts/03_verify.R"              = "数值核对：我方产物与官方交付逐项对账",
  "sc-pipeline/scripts/lib/config.R"              = "子模块：配置读取与闸门检查",
  "sc-pipeline/scripts/lib/core_functions.R"      = "子模块：去双 / 质控 / 聚类等核心函数",
  "sc-pipeline/scripts/lib/evidence_functions.R"  = "子模块：生成人工判断依据（簇大小 / 样本分布 / marker）",
  "sc-pipeline/scripts/lib/param_sweep_blocked.R" = "子模块：内存不足时的分块参数",
  "sc-pipeline/scripts/lib/test_rdist_equiv.R"    = "子模块：分块与原生路径的等价性测试",
  "sc-plot-kit/scripts/plot_all_v2.R"             = "出图主脚本（当前版）",
  "sc-plot-kit/scripts/plot_all.R"                = "出图旧版（保留备查，不再用于交付）",
  "sc-ppt-kit/scripts/make_ppt.py"                = "PPT 生成：按母版重打包",
  "sc-ppt-kit/scripts/slide_spec.json"            = "版式配置：每页放什么图、写什么话"
 )

script_role <- function(rel) {
  hit <- SCRIPT_ROLE[[rel]]
  if (!is.null(hit)) return(hit)
  if (grepl("/lib/", rel)) return("子模块：被主脚本 source 调用")
  if (grepl("^sc-plot-kit/", rel)) return("出图脚本")
  if (grepl("^sc-ppt-kit/", rel)) return("PPT 组装")
  "脚本"
}


stage_scripts <- function(engine_dir, run_dir = NULL) {
  out <- list()
  for (st in STAGE_DEF) {
    full <- file.path(engine_dir, st$dir)
    files <- character(0)
    if (dir.exists(full)) {
      files <- list.files(full, pattern = st$ext, recursive = TRUE, full.names = TRUE)
      files <- files[!grepl("/legacy_reference/|/audit/", files)]
      files <- sort(files)
    }
    items <- lapply(files, function(f) {
      rel <- sub(paste0("^", gsub("([\\W])", "\\\\\\1", normalizePath(engine_dir, winslash = "/")), "/?"),
                 "", normalizePath(f, winslash = "/"))
      list(
        name  = basename(f),
        path  = rel,
        md5   = unname(tools::md5sum(f)),
        size  = as.integer(file.info(f)$size),
        # 这个脚本在环节里是什么角色：总调度 / 主流程 / 子模块 / 配置。
        #   子模块不能单独跑，和能跑的脚本混在一列里，看的人分不出来。
        role  = script_role(rel),
        # 底层脚本是否带头守卫：出图/主流程脚本必须带，否则就是可绕过的
        guarded = {
          l <- tryCatch(readLines(f, n = 80, warn = FALSE), error = function(e) character(0))
          any(grepl("guard_load|sc_guard_banner", l))
        }
      )
    })
    out[[length(out) + 1L]] <- list(
      id = st$id, name = st$name, skill = st$skill, desc = st$desc,
      dir = st$dir, count = length(items), scripts = items,
      # 环节的「补位关键词」与「是否机制类」。页面拿这两个字段决定
      #   第三段里该显示哪些环节。
      covers = st$covers,
      infra  = isTRUE(st$infra)
    )
  }
  list(ok = TRUE, engine = engine_dir, run_dir = run_dir,
       stages = out, scanned_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
}
