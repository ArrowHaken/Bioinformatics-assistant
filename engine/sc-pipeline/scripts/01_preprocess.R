#!/usr/bin/env Rscript
# =====================================================================
# 01_preprocess.R — 单细胞预处理主流程
#
# 严格照官方脚本复刻（script/soupX_doublefinder.R + seurat_merge_cluster.R + analysis.R）
# 顺序：逐样本去双 -> 取Singlet重建 -> 逐样本质控 -> 合并 -> 标准化/周期回归/PCA
#       -> Harmony -> 一次聚类(res=0.5) -> 筛群 -> 二次聚类(res=0.6) -> 剔样本 -> 保存
#
# 铁律：不做降级。依赖缺失、步骤失败、结果异常 -> stop()，并写入运行报告。
# =====================================================================

suppressMessages({
  library(Seurat); library(Matrix); library(dplyr)
  library(ggplot2); library(patchwork); library(argparser)
})

# harmony 必须在跑之前真正挂进命名空间：RunHarmony() 是被直接调用的。
#   原来只在依赖检查里用 requireNamespace() 确认「装没装」，并不会挂载。
#   在别的脚本碰巧先挂过 harmony 的机器上看不出来，换一台干净机器
#   就会在合并那一步报 could not find function "RunHarmony"。
#   预检模式不跑 RunHarmony，缺包时不在这一行中断，与依赖检查的口径一致。
if (requireNamespace("harmony", quietly = TRUE)) {
  suppressMessages(library(harmony))
}

# ---- 载入核心函数（RunDF / RunFindAllMarkers / 逐样本处理 / 质控）----
# 脚本目录定位：要同时支持 source() 和 Rscript 直接调用两种方式。
#   只用 sys.frame(1)$ofile 时，Rscript 直接跑会拿到 NULL，
#   表现为「cannot open file './lib/core_functions.R'」——很难排查。
#   所以补一条从 --file= 取路径的退路。
.this_dir <- local({
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(f) && nzchar(f)) return(dirname(normalizePath(f)))
  ca <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", ca, value = TRUE)
  if (length(m) > 0) {
    p <- sub("^--file=", "", m[1])
    if (nzchar(p) && file.exists(p)) return(dirname(normalizePath(p)))
  }
  if (dir.exists("lib") && file.exists("lib/core_functions.R")) return(normalizePath("."))
  "."
})

# ---- 入口守卫：拒绝被直接调用 ----
#   这不是装饰。它让「必须走统一入口」这件事由代码保证，而不是靠文档里的约定。
#   守卫位置：脚本往上找 workflow/guard_load.R。找不到就停 —— 不静默跳过。
#   为什么必须停：守卫缺失时继续跑，等于约束失效而没人知道，比直接报错危险得多。
.guard_load <- local({
  cands <- c(file.path(.this_dir, "..", "..", "workflow", "guard_load.R"),
             file.path(.this_dir, "..", "workflow", "guard_load.R"),
             file.path(.this_dir, "..", "..", "..", "workflow", "guard_load.R"))
  hit <- cands[file.exists(cands)]
  if (!length(hit)) {
    stop("\n  拒绝运行：找不到入口守卫 workflow/guard_load.R。\n",
         "  本脚本必须经由统一入口执行，守卫缺失时不能继续。\n",
         "  找过：\n    ", paste(cands, collapse = "\n    "), "\n", call. = FALSE)
  }
  hit[1]
})
source(.guard_load, local = FALSE)
sc_guard_banner("pipeline", .this_dir)

source(file.path(.this_dir, "lib", "core_functions.R"))
source(file.path(.this_dir, "lib", "config.R"))
source(file.path(.this_dir, "lib", "evidence_functions.R"))

LOG <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste0(...)))
STEP <- function(t, obj = NULL) {
  cat(sprintf("\n[%s] ===== %s =====\n", format(Sys.time(), "%H:%M:%S"), t))
  if (!is.null(obj)) LOG("  当前细胞数: ", ncol(obj))
}

# =====================================================================
# 0. 参数
#
# 业务参数一律来自配置文件，脚本里不再有默认值。
#   为什么：以前把「剔除 16-22 号簇」「剔除 18m-G761」这类值写在代码里，
#   换一批数据就静默出错（不报错，只是结果不对）。现在值全在 pipeline.json，
#   每个值都带来源标记，遇到未确认的项直接停下来要答案。
#
# 命令行只保留「这次跑什么」这类不带业务含义的开关。
# =====================================================================
p <- arg_parser("单细胞预处理（配置驱动，不再自带业务参数）")
p <- add_argument(p, "--config",   help = "配置文件路径（pipeline.json）。必填。")
p <- add_argument(p, "--indir",    help = "matrix 根目录（覆盖配置里的 io.indir）", default = NA_character_)
p <- add_argument(p, "--outdir",   help = "输出目录（覆盖配置里的 io.outdir）", default = NA_character_)
p <- add_argument(p, "--gates",    help = "待确认问题清单路径（gates.json），用于报错时指路", default = NA_character_)
p <- add_argument(p, "--allow-v5", help = "允许 Seurat v5 运行（结果可能与官方有差异，须在报告标注）", flag = TRUE)
p <- add_argument(p, "--dry-run",  help = "只做容量预检，不实际计算", flag = TRUE)
p <- add_argument(p, "--init-config", help = "把内置默认配置写到 --config 指定路径后退出（不跑分析）", flag = TRUE)
# --stage 让主流程可以分两段跑，中间停下来等人做决定。
#   为什么必须能停：筛群、剔样本这类事是人定的，不是脚本猜的。脚本硬跑完，
#   等于把「人做的判断」这一环抹掉了；而人做判断需要依据（簇大小、样本分布、
#   marker），依据只有跑到一次聚类才产生。所以顺序必然是：
#     跑到一次聚类 → 出依据 → 停 → 人定 → 继续
#   all      一次跑完（没有人需要插手时用）
#   cluster1 跑到一次聚类、写出依据后停住
#   continue 读回那次一次聚类的结果，接着做筛群、二次聚类、剔样本、注释
p <- add_argument(p, "--stage", help = "跑到哪一段：all / cluster1 / continue", default = "all")
argv <- parse_args(p)
STAGE <- if (is.null(argv$stage) || is.na(argv$stage)) "all" else as.character(argv$stage)
if (!STAGE %in% c("all", "cluster1", "continue")) {
  stop("--stage 取值非法: ", STAGE, "（允许 all / cluster1 / continue）", call. = FALSE)
}

if (is.na(argv$config)) {
  stop("必填参数: --config <pipeline.json>\n",
       "  首次使用可先执行：Rscript 01_preprocess.R --config <路径> --init-config\n",
       "  该命令会生成一份带注释说明的默认配置，再由工作台或人工填写。", call. = FALSE)
}
gates_path <- if (is.na(argv$gates)) NULL else argv$gates

# ---- --init-config：写出默认配置，方便起步 ----
if (isTRUE(argv$init_config)) {
  tpl <- file.path(.this_dir, "..", "templates", "pipeline.default.json")
  if (!file.exists(tpl)) stop("找不到默认配置模板: ", tpl, call. = FALSE)
  dir.create(dirname(argv$config), showWarnings = FALSE, recursive = TRUE)
  file.copy(tpl, argv$config, overwrite = FALSE)
  cat("已写出默认配置: ", normalizePath(argv$config, mustWork = FALSE), "\n",
      "下一步：打开工作台填写 environment / io / 各闸门项，或直接编辑该文件。\n", sep = "")
  quit(status = 0)
}

cfg <- cfg_load(argv$config)

# ---- 先跑闸门检查：有未确认项就停，不许兜底 ----
#
#   按段位查，不笼统查。
#     上段（all/cluster1）只查 A 段要用的：数据在哪、注释依据、质控门槛、读入前剔样本。
#     下段（continue）才查 B 段：筛群、剔样本、低置信门槛。
#   为什么必须分：B 段的依据（簇大小、样本分布、marker）只有跑到一次聚类才产生。
#   上段之前就要求答完 B，等于逼人在没有依据的情况下猜，
#   而且上段永远跑不起来，也就永远产生不了依据 —— 死锁。
GATE_PHASE <- if (identical(STAGE, "continue")) "B" else "A"
cfg_require_resolved(cfg, gates_path, GATE_PHASE)

# ---- 取值：路径允许命令行覆盖（方便临时换目录）----
INDIR  <- if (!is.na(argv$indir))  argv$indir  else cfg_get(cfg, "io", "indir")
OUTDIR <- if (!is.na(argv$outdir)) argv$outdir else cfg_get(cfg, "io", "outdir")
if (is.null(INDIR) || !nzchar(INDIR))  stop("配置里 io.indir 为空，且命令行未给 --indir。", call. = FALSE)
if (is.null(OUTDIR) || !nzchar(OUTDIR)) stop("配置里 io.outdir 为空，且命令行未给 --outdir。", call. = FALSE)

# =====================================================================
# 分段运行：continue 时先把一次聚类的结果读回来
#
#  为什么要有「快照」这个机制：
#    人在界面上做决定可能要隔一会儿，甚至隔天。把对象一直留在内存里等，
#    既不现实也占内存。所以落一份快照到磁盘，停得住、也接得上。
#    （SQLite 存不下 Seurat 对象，用 rds 是最直接的。）
# =====================================================================
STAGE_SNAP <- file.path(OUTDIR, "status", "stage_cluster1.rds")
need_compute <- !identical(STAGE, "continue")

if (!need_compute) {
  STEP("继续执行：读回一次聚类的快照")
  if (!file.exists(STAGE_SNAP)) {
    stop("--stage continue 需要一次聚类的快照，但没找到：\n  ", STAGE_SNAP,
         "\n  请先用 --stage cluster1 跑到一次聚类、生成依据，再回来继续。", call. = FALSE)
  }
  snap <- readRDS(STAGE_SNAP)
  merged      <- snap$merged
  sample_tab  <- snap$sample_tab
  n_after_clu1 <- snap$n_after_clu1
  dims_use    <- 1:P_DIMS
  clu1_tab <- as.data.frame(table(as.integer(as.character(merged$seurat_clusters))))
  colnames(clu1_tab) <- c("cluster", "n_cells")
  clu1_tab$pct <- round(100 * clu1_tab$n_cells / sum(clu1_tab$n_cells), 2)
  LOG("已读回一次聚类结果：细胞数 ", ncol(merged),
      "，群数 ", length(levels(merged$seurat_clusters)),
      "，快照时间 ", snap$saved_at)
  rm(snap); gc(verbose = FALSE)
}

# ---- 业务参数：全部来自配置 ----
P_SPECIES  <- cfg_get(cfg, "params", "species")
P_NFEAT    <- as.integer(cfg_get(cfg, "params", "nfeatures"))
P_DIMS     <- as.integer(cfg_get(cfg, "params", "dims"))
P_PCS_DF   <- as.integer(cfg_get(cfg, "params", "pcs_doublet"))
P_PN       <- as.numeric(cfg_get(cfg, "params", "pN"))
P_QCMIN    <- as.numeric(cfg_get(cfg, "params", "qc_min_features"))
P_QCMAX    <- as.numeric(cfg_get(cfg, "params", "qc_max_features"))
P_QCMT     <- as.numeric(cfg_get(cfg, "params", "qc_mt"))
P_RES1     <- as.numeric(cfg_get(cfg, "params", "res1"))
P_RES2     <- as.numeric(cfg_get(cfg, "params", "res2"))
P_SEED     <- as.integer(cfg_get(cfg, "params", "seed"))

# ---- 步骤策略：也来自配置 ----
S_READEXCL <- cfg_step_get(cfg, "read_exclusion", "samples", list())
S_CLUSMODE <- cfg_step_get(cfg, "cluster_filter", "mode", "none")
S_CLUSKEEP <- cfg_step_get(cfg, "cluster_filter", "by_name_keep", list())
S_CLUSMAP  <- cfg_step_get(cfg, "cluster_filter", "mapping_file", NULL)
S_DROPSMP  <- cfg_step_get(cfg, "post_cluster_exclusion", "samples", list())
S_LOWMODE  <- cfg_step_get(cfg, "lowconfidence", "mode", "flag_only")
S_LOWMIN   <- cfg_step_get(cfg, "lowconfidence", "min_cells", 200)
S_MEMMODE  <- cfg_step_get(cfg, "memory", "mode", "auto")

# 配置自查：把「历史真出过错」的形态当回归测试
sc_problems <- cfg_selfcheck(cfg)
if (length(sc_problems) > 0) {
  stop("配置自查未通过：\n  - ", paste(sc_problems, collapse = "\n  - "), call. = FALSE)
}

dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUTDIR, "01_single"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUTDIR, "02_qc"),     showWarnings = FALSE, recursive = TRUE)
set.seed(P_SEED)

# 参数来源记录：由配置生成，不手抄
param_table <- cfg_param_table(cfg)
param_log <- list(
  config_path = normalizePath(argv$config, mustWork = FALSE),
  params = setNames(as.list(param_table$value), paste(param_table$scope, param_table$key, sep = ".")),
  sources = setNames(as.list(param_table$source), paste(param_table$scope, param_table$key, sep = "."))
)
stop_log <- list()   # 记录被跳过的环节（本脚本不应产生；由外部脚本汇总）

# =====================================================================
# 1. 依赖检查（缺一个就停，不降级）
#
#   dry-run 时例外：只警告不中断。
#     理由：容量预检的用途就是「不跑分析、只回答内存够不够」。
#     如果它先死在缺包上，使用者就永远看不到内存结论 ——
#     而他在装包之前，最想知道的恰恰是「这台机器到底能不能跑」。
#     缺包照样报出来（写在 precheck.json 和终端），只是不拦。
#     真跑（非 dry-run）时仍然严格停住：不降级、不跳过。
# =====================================================================
STEP("STEP 0 依赖检查")
required <- c("Seurat", "DoubletFinder", "harmony", "Matrix", "dplyr", "ggplot2", "patchwork")
missing <- required[!vapply(required, function(x) requireNamespace(x, quietly = TRUE), logical(1))]
if (length(missing) > 0) {
  if (isTRUE(argv$dry_run)) {
    LOG("  预检模式：缺少包 ", paste(missing, collapse = ", "), " —— 只记录，不中断。")
    LOG("  正式跑之前必须先装齐，否则会在这里停住。")
  } else {
    stop("缺少必需包: ", paste(missing, collapse = ", "),
         "\n  请先安装后再运行。本流程不提供替代方案。", call. = FALSE)
  }
}
seurat_ver <- tryCatch(as.character(utils::packageVersion("Seurat")), error = function(e) NA_character_)
seurat_major <- if (is.na(seurat_ver)) NA_integer_ else as.integer(strsplit(seurat_ver, "\\.")[[1]][1])
if (!is.na(seurat_ver)) {
  LOG("Seurat 版本: ", seurat_ver, " （官方基于 v4）")
} else {
  LOG("Seurat 未安装，版本无法判断。")
}
if (!is.na(seurat_major) && seurat_major >= 5 && !isTRUE(argv$allow_v5)) {
  if (isTRUE(argv$dry_run)) {
    LOG("  预检模式：本机 Seurat 是 v5，与官方 v4 语义不同，结果不可比。")
    LOG("  正式跑需要装 v4，或加 --allow-v5 并接受差异（须在报告标注）。")
  } else {
    stop("Seurat v5 与官方 v4 的槽访问语义不同，结果不可比。\n",
         "  方案一：装 Seurat v4（推荐，用于交付对齐）\n",
         "  方案二：加 --allow-v5 显式接受兼容差异（须在报告中标注）", call. = FALSE)
  }
}
has_presto <- requireNamespace("presto", quietly = TRUE)
LOG("presto（Wilcoxon C++ 加速）: ", ifelse(has_presto, "已安装", "未安装（不影响结果，仅影响速度）"))

# =====================================================================
# 2. 样本清单与剔除（读入之前生效）
# =====================================================================
STEP("STEP 1 样本清单")
is_10x_dir <- function(d) {
  any(file.exists(file.path(d, c("barcodes.tsv", "barcodes.tsv.gz",
                                 "features.tsv", "features.tsv.gz",
                                 "matrix.mtx",  "matrix.mtx.gz")))) ||
  dir.exists(file.path(d, "outs", "filtered_feature_bc_matrix"))
}
sample_names <- character(0); group_map <- NULL
S_SAMPLES <- cfg_get(cfg, "io", "samples_file", default = NULL)
if (!is.null(S_SAMPLES) && !is.na(S_SAMPLES) && nzchar(S_SAMPLES)) {
  if (!file.exists(S_SAMPLES)) stop("样本表不存在: ", S_SAMPLES, call. = FALSE)
  st <- read.delim(S_SAMPLES, header = TRUE, sep = "\t", colClasses = "character",
                   check.names = FALSE, stringsAsFactors = FALSE)
  if (ncol(st) < 2) stop("样本表至少两列：样本目录名 + 分组名", call. = FALSE)
  st <- st[nzchar(st[[1]]), , drop = FALSE]
  sample_names <- st[[1]]; group_map <- setNames(st[[2]], st[[1]])
  LOG("从样本表读入 ", length(sample_names), " 个样本: ", S_SAMPLES)
} else {
  dirs <- list.dirs(INDIR, recursive = FALSE, full.names = FALSE)
  sample_names <- sort(dirs[vapply(file.path(INDIR, dirs), is_10x_dir, logical(1))])
  if (length(sample_names) == 0) stop("在 ", INDIR, " 下没找到任何 10x 矩阵子目录", call. = FALSE)
  group_map <- setNames(sample_names, sample_names)
  LOG("配置未给样本表，自动扫描到 ", length(sample_names), " 个样本（分组名=样本名）")
  LOG("  注意：没有样本表时无法做组间差异分析。需要时请在配置 io.samples_file 指定。")
}

# 读入前剔除：来自配置 steps.read_exclusion.samples
if (length(S_READEXCL) > 0) {
  ex <- as.character(unlist(S_READEXCL)); ex <- ex[nzchar(ex)]
  hit <- intersect(ex, sample_names)
  if (length(hit) == 0) {
    stop("配置要求读入前剔除样本 ", paste(ex, collapse = ", "),
         "，但扫描到的样本里没有这些名字。\n  扫描到: ", paste(sample_names, collapse = ", "),
         "\n  请核对配置，不要让它静默不生效。", call. = FALSE)
  }
  sample_names <- setdiff(sample_names, ex)
  LOG("读入前剔除样本: ", paste(hit, collapse = ", "), " | 保留 ", length(sample_names), " 个")
}
if (length(sample_names) == 0) stop("剔除后无剩余样本，请检查配置的 read_exclusion。", call. = FALSE)

resolve_matrix_dir <- function(root, s) {
  if (dir.exists(file.path(root, s, "outs", "filtered_feature_bc_matrix"))) {
    return(file.path(root, s, "outs", "filtered_feature_bc_matrix"))
  }
  file.path(root, s)
}
missing_dir <- sample_names[!vapply(sample_names, function(s) dir.exists(resolve_matrix_dir(INDIR, s)), logical(1))]
if (length(missing_dir) > 0) stop("以下样本目录不存在: ", paste(missing_dir, collapse = ", "), call. = FALSE)

# =====================================================================
# 3. 容量预检（不够就停，不许抽样）
# =====================================================================
STEP("STEP 2 容量预检")
avail_gb <- tryCatch({
  if (.Platform$OS.type == "windows") {
    out <- suppressWarnings(system2("wmic", "OS get FreePhysicalMemory /Value", stdout = TRUE))
    kb <- as.numeric(sub("[^0-9]", "", grep("FreePhysicalMemory", out, value = TRUE)))
    kb / 1024 / 1024
  } else {
    as.numeric(sub(".*?([0-9]+).*", "\\1", system("free -g | awk '/Mem:/{print $7}'", intern = TRUE)))
  }
}, error = function(e) NA_real_)

est_gb <- NA_real_
est_note <- ""
# 内存需求按实际细胞数实算，不用经验常数。
#   以前这里写 est_gb <- 2.5，但注释说峰值约 10GB —— 闸门实际算 avail < 2.25 才拦，
#   等于形同虚设。现在改成：按各样本 barcode 数估 rdist 矩阵峰值，再取一个下限。
bc_files <- character(0)
n_cells_est <- NA_integer_
for (s in sample_names) {
  md <- resolve_matrix_dir(INDIR, s)
  cand <- file.path(md, c("barcodes.tsv.gz", "barcodes.tsv"))
  hit <- cand[file.exists(cand)]
  if (length(hit) > 0) bc_files <- c(bc_files, hit[1])
}
if (length(bc_files) > 0) {
  nb <- tryCatch({
    gz <- grepl("\\.gz$", bc_files, ignore.case = TRUE)
    n <- if (any(gz)) {
      # gz 用 R 自身读，不依赖 zcat（跨平台）
      con <- gzfile(bc_files[which(gz)[1]], "rt"); on.exit(close(con), add = TRUE)
      length(readLines(con, warn = FALSE))
    } else {
      length(readLines(bc_files[1], warn = FALSE))
    }
    n
  }, error = function(e) NA_integer_)
  n_cells_est <- nb
  if (!is.na(nb) && nb > 0) {
    doubled <- nb * 2                          # 真实 + 人造双细胞
    est_rdist_gb <- (doubled^2 * 8) / 1024^3   # n^2 个 double
    # 去双一步的峰值 ≈ 距离矩阵 + 对象/中间副本，取 2 倍矩阵大小做保守估计
    est_gb <- est_rdist_gb * 2
    est_note <- sprintf("按最大单样本 %d 个 barcode 实算（距离矩阵 %.1f GB × 2）", nb, est_rdist_gb)
  }
}
if (is.na(est_gb)) {
  # 连 barcode 都读不到时不能瞎猜，明确报出来
  stop("容量预检无法估算内存需求：读不到任何样本的 barcodes 文件。\n",
       "  检查过的样本: ", paste(sample_names, collapse = ", "),
       "\n  不要用默认值蒙过去——请确认 matrix 目录结构。", call. = FALSE)
}
LOG(sprintf("预估峰值内存 ≈ %.1f GB（%s）；系统可用 ≈ %s GB",
            est_gb, est_note,
            ifelse(is.na(avail_gb), "未知", sprintf("%.1f", avail_gb))))

# 分块路径判定（结果等价，已实测：排名一致、差 0）
USE_BLOCKED <- identical(S_MEMMODE, "force_blocked")
mem_decision <- "原生路径"
if (is.na(avail_gb)) {
  LOG("  可用内存读不到，按内存充足处理（原生路径）。若随后失败，请改配置 steps.memory.mode 为 force_blocked。")
  mem_decision <- "原生路径（内存未知）"
} else if (identical(S_MEMMODE, "force_native")) {
  if (avail_gb < est_gb * 0.9) {
    stop(sprintf("配置要求强制走原生路径，但可用内存 %.1f GB < 需求 %.1f GB，注定失败。\n",
                 avail_gb, est_gb),
         "  要么改 steps.memory.mode 为 auto / force_blocked，要么换机器。", call. = FALSE)
  }
  mem_decision <- "原生路径（用户强制）"
} else if (avail_gb < est_gb * 0.9) {
  if (identical(S_MEMMODE, "auto")) {
    USE_BLOCKED <- TRUE
    mem_decision <- sprintf("分块路径（自动：可用 %.1f < 需求 %.1f GB）", avail_gb, est_gb)
  } else {
    stop(sprintf("可用内存 %.1f GB < 需求 %.1f GB，且配置 steps.memory.mode = %s 不允许自动降级。\n",
                 avail_gb, est_gb, S_MEMMODE),
         "  本流程不抽样、不降精度。请改配置或换机器。", call. = FALSE)
  }
}
LOG("  内存路径决策: ", mem_decision)

# 把决策写进状态文件，供工作台读取（不需要人管，但要留痕）
dir.create(file.path(OUTDIR, "status"), showWarnings = FALSE, recursive = TRUE)
writeLines(jsonlite::toJSON(list(
  stage = "precheck", time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  available_gb = avail_gb, required_gb = est_gb, estimate_note = est_note,
  mode = S_MEMMODE, decision = mem_decision, use_blocked = USE_BLOCKED,
  max_barcodes = n_cells_est
), auto_unbox = TRUE, pretty = TRUE), file.path(OUTDIR, "status", "precheck.json"))

if (isTRUE(argv$dry_run)) {
  LOG("--dry-run 已指定，预检完成，退出。")
  LOG("  决策：USE_BLOCKED = ", USE_BLOCKED)
  quit(status = 0)
}

# =====================================================================
# 4. 逐样本：去双 + 取 Singlet 重建 + 质控
# =====================================================================
STEP("STEP 3 逐样本去双与质控")
objs <- list(); sample_rows <- list()
for (s in sample_names) {
  mdir <- resolve_matrix_dir(INDIR, s)
  LOG("处理样本 ", s, " ...")
  t0 <- Sys.time()
  o <- process_one_sample(mdir, s, species = P_SPECIES,
                          nfeatures = P_NFEAT,
                          dims = P_DIMS,
                          resolution = P_RES1,
                          PCs = P_PCS_DF, pN = P_PN,
                          use_blocked = USE_BLOCKED,
                          allow_v5 = isTRUE(argv$allow_v5))
  st1 <- attr(o, "sample_stats")
  o <- qc_filter_sample(o, min_features = P_QCMIN,
                        max_features = P_QCMAX,
                        mt_cutoff = P_QCMT)
  st2 <- attr(o, "qc_stats")
  sample_rows[[s]] <- data.frame(
    sample = s, raw = st1$raw, singlet = st1$singlet, after_rebuild = st1$after_rebuild,
    qc_pass = st2$after, qc_dropped = st2$dropped,
    geneMedian = ceiling(median(o$nFeature_RNA)),
    mtMedian = round(median(o$percent.mt), 2),
    pK = round(st1$pK, 4), nExp = st1$nExp,
    minutes = round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1),
    stringsAsFactors = FALSE)
  saveRDS(o, file.path(OUTDIR, "01_single", paste0(s, ".rds")))
  objs[[s]] <- o
  LOG(sprintf("  %s: raw=%d singlet=%d 质控后=%d (pK=%.4f, nExp=%d)",
              s, st1$raw, st1$singlet, st2$after, st1$pK, st1$nExp))
  rm(o); gc(verbose = FALSE)
}
sample_tab <- do.call(rbind, sample_rows)
write.table(sample_tab, file.path(OUTDIR, "02_qc", "qc_stat.tsv"),
            quote = FALSE, sep = "\t", row.names = FALSE)
LOG("合计质控后细胞数: ", sum(sample_tab$qc_pass))

# =====================================================================
# 5. 合并 + 标准化 + 周期回归 + PCA + Harmony
# =====================================================================
STEP("STEP 4 合并与批次校正")
merged <- if (length(objs) == 1) objs[[1]] else
  merge(objs[[1]], y = objs[2:length(objs)], add.cell.ids = names(objs))
if (seurat_major >= 5) merged <- JoinLayers(merged)
merged$group <- unname(group_map[as.character(merged$orig.ident)])
if (any(is.na(merged$group))) {
  bad <- unique(as.character(merged$orig.ident)[is.na(merged$group)])
  stop("以下样本在样本表中没有分组: ", paste(bad, collapse = ", "),
       "\n  不自动回退为样本名（那会让差异分析失效）。请补全样本表。", call. = FALSE)
}
rm(objs); gc(verbose = FALSE)
LOG("合并后细胞数: ", ncol(merged), " | 样本数: ", length(unique(merged$orig.ident)))

s.genes   <- stringr::str_to_title(Seurat::cc.genes$s.genes)
g2m.genes <- stringr::str_to_title(Seurat::cc.genes$g2m.genes)
merged <- merged %>%
  NormalizeData(verbose = FALSE) %>%
  FindVariableFeatures(selection.method = "vst", nfeatures = P_NFEAT, verbose = FALSE) %>%
  CellCycleScoring(s.features = s.genes, g2m.features = g2m.genes) %>%
  ScaleData(verbose = FALSE, vars.to.regress = c("S.Score", "G2M.Score")) %>%
  RunPCA(verbose = FALSE)
merged <- RunHarmony(merged, group.by.vars = "orig.ident", verbose = FALSE)
if (!"harmony" %in% names(merged@reductions)) {
  stop("Harmony 未产生 harmony 降维结果，停止。不退回 PCA。", call. = FALSE)
}

# =====================================================================
# 6. 一次聚类 -> 筛群 -> 二次聚类 -> 剔样本
#
# 筛群这一步历史上出过事：以前照抄对方脚本里写死的
#   subset(seurat_clusters %in% c(0:15,23,24))
# 但簇号是本次运行的产物（对方 0-19，我方 0-30），同一句删掉的
# 是完全不同的细胞，静默误删了 1590 个。所以现在：
#   默认 none（不剔除）；
#   要剔除必须由配置显式给出，且优先按细胞类型名（换数据安全）；
#   按簇号只允许配合映射表使用，并在 reason 里写明。
# =====================================================================
STEP("STEP 5 一次聚类（res=", P_RES1, "）")
dims_use <- 1:P_DIMS
merged <- FindNeighbors(merged, reduction = "harmony", dims = dims_use, verbose = FALSE)
merged <- FindClusters(merged, resolution = P_RES1, verbose = FALSE)
merged <- RunUMAP(merged, reduction = "harmony", dims = dims_use, verbose = FALSE)
merged <- RunTSNE(merged, reduction = "harmony", dims = dims_use, verbose = FALSE)
LOG("一次聚类群数: ", length(levels(merged$seurat_clusters)))
n_after_clu1 <- ncol(merged)

# ---- 把一次聚类结果落盘，供工作台/人工看，并写「筛群」闸门的证据 ----
clu1_tab <- as.data.frame(table(as.integer(as.character(merged$seurat_clusters))))
colnames(clu1_tab) <- c("cluster", "n_cells")
clu1_tab$pct <- round(100 * clu1_tab$n_cells / sum(clu1_tab$n_cells), 2)
write.table(clu1_tab, file.path(OUTDIR, "status", "cluster1_sizes.tsv"),
            quote = FALSE, sep = "\t", row.names = FALSE)
writeLines(jsonlite::toJSON(list(
  stage = "cluster1", time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  n_clusters = nrow(clu1_tab), total_cells = sum(clu1_tab$n_cells),
  sizes = setNames(as.list(clu1_tab$n_cells), clu1_tab$cluster)
), auto_unbox = TRUE, pretty = TRUE), file.path(OUTDIR, "status", "cluster1.json"))

# ---- 生成人工判断依据 ----
#   人要决定「剔除哪个簇 / 剔除哪个样本」，需要的不只是簇大小，还要：
#     · 每个簇在各样本的分布（是不是某个样本特有）
#     · 每个簇靠哪些基因聚在一起（这簇大概是什么细胞）
#     · 每个样本的细胞数与占比（剔样本会不会把某组削平）
#   这些算出来写进 status/cluster1_evidence.json，闸门会把它展开给人看。
#   marker 算不出来不算失败：降级为「只给细胞数与样本分布」并在文件里写明。
STEP("STEP 5b 生成人工判断依据（簇大小 / 样本分布 / top marker）")
ev <- tryCatch(
  evidence_cluster1(merged, OUTDIR, top_n = 10, want_markers = TRUE),
  error = function(e) {
    LOG("  依据生成失败：", conditionMessage(e))
    LOG("  这不会中止流程，但筛群闸门里将只有簇大小，判断依据不足。")
    NULL
  }
)
if (!is.null(ev)) {
  LOG(sprintf("  依据已写出：%d 个簇 / %d 个样本 / marker %s",
              ev$n_clusters, ev$n_samples, if (isTRUE(ev$markers_ok)) "已算" else "未算"))
  if (nzchar(ev$markers_note)) LOG("  说明：", ev$markers_note)
}

# ---- 分段点：把一次聚类的结果落快照，然后停住等人做决定 ----
#
#   为什么必须在这里停：
#     接下来的「筛群」和「剔样本」是人定的决定，依据是刚算出来的这几张表。
#     脚本替人做这个决定，等于把判断这一环抹掉；而人做决定需要先看到依据。
#     所以顺序只能是：算出依据 → 停 → 人定 → 接着跑。
#   快照用 rds：Seurat 对象不是一个表，存不进数据库；rds 最直接。
if (identical(STAGE, "cluster1")) {
  STEP("分段点：落快照并停住")
  saveRDS(list(
    merged = merged, sample_tab = sample_tab,
    n_after_clu1 = n_after_clu1,
    n_samples = length(unique(as.character(merged$orig.ident))),
    cluster1_sizes = clu1_tab,
    saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    note = "一次聚类后的完整对象。人工定完筛群与剔样本后，用 --stage continue 接着跑。"
  ), STAGE_SNAP)
  LOG("快照已保存: ", STAGE_SNAP)
  LOG("")
  LOG("===== 停在这里，等人做决定 =====")
  LOG("下一步（人在界面上做完选择后）：")
  LOG("  Rscript 01_preprocess.R --config <pipeline.json> --indir <...> --outdir ", OUTDIR,
      " --stage continue")
  LOG("")
  LOG("要人定的事（界面上的闸门）：")
  LOG("  · 筛群：剔除哪些簇（依据见 status/cluster1_evidence.json）")
  LOG("  · 剔样本：要不要去掉某个样本（依据同上）")
  LOG("  · 低置信门槛：多小的簇不强行命名")
  quit(status = 0)
}

STEP("STEP 6 筛群（策略: ", S_CLUSMODE, "）")
n_before_filter <- ncol(merged)
if (identical(S_CLUSMODE, "none")) {
  LOG("  配置为 none：不做任何筛群，全部簇进入后续分析。")
} else if (identical(S_CLUSMODE, "by_name")) {
  keep_names <- as.character(unlist(S_CLUSKEEP)); keep_names <- keep_names[nzchar(keep_names)]
  if (length(keep_names) == 0) {
    stop("筛群策略为 by_name，但配置里 by_name_keep 是空的。\n",
         "  要么填上要保留的细胞类型名，要么把 mode 改成 none。", call. = FALSE)
  }
  if (!"annot" %in% colnames(merged@meta.data)) {
    stop("筛群策略为 by_name，但这一步还没有 annot 列。\n",
         "  按类型名筛群需要在注释之后做；现在只能 none 或 by_cluster。", call. = FALSE)
  }
  hit <- intersect(keep_names, unique(as.character(merged$annot)))
  if (length(hit) == 0) {
    stop("按类型名筛群：配置里的名字一个都没匹配上。\n",
         "  配置: ", paste(keep_names, collapse = ", "),
         "\n  实际: ", paste(sort(unique(as.character(merged$annot))), collapse = ", "),
         "\n  不静默跳过。", call. = FALSE)
  }
  miss <- setdiff(keep_names, unique(as.character(merged$annot)))
  if (length(miss) > 0) LOG("  注意：配置里有 ", length(miss), " 个类型名在本次结果中不存在: ",
                            paste(miss, collapse = ", "))
  merged <- subset(merged, cells = colnames(merged)[as.character(merged$annot) %in% keep_names])
} else if (identical(S_CLUSMODE, "by_cluster")) {
  map_file <- S_CLUSMAP
  if (is.null(map_file) || is.na(map_file) || !nzchar(map_file)) {
    stop("筛群策略为 by_cluster，但配置里没有 mapping_file。\n",
         "  按簇号筛群必须附一份「本次簇号 → 细胞类型」的映射表，否则无法追溯、\n",
         "  换一次运行簇号就全变。历史上就是这里误删了 1590 个细胞。", call. = FALSE)
  }
  if (!file.exists(map_file)) stop("mapping_file 不存在: ", map_file, call. = FALSE)
  mp <- read.delim(map_file, header = TRUE, sep = "\t", colClasses = "character",
                   stringsAsFactors = FALSE)
  need <- c("cluster", "keep")
  if (!all(need %in% colnames(mp))) {
    stop("mapping_file 必须含 cluster 与 keep 两列。实际列: ",
         paste(colnames(mp), collapse = ", "), call. = FALSE)
  }
  keep_clu <- as.integer(mp$cluster[grepl("^(y|Y|true|TRUE|1)$", trimws(mp$keep))])
  keep_clu <- keep_clu[!is.na(keep_clu)]
  LOG("  按映射表筛群，保留簇: ", paste(sort(keep_clu), collapse = ","))
  clu1 <- as.integer(as.character(merged$seurat_clusters))
  merged <- subset(merged, cells = colnames(merged)[clu1 %in% keep_clu])
  stop_log[[length(stop_log) + 1]] <-
    paste0("筛群按簇号执行，映射表=", basename(map_file),
           "；换数据必须重做这份映射表（reason: ",
           cfg_step_get(cfg, "cluster_filter", "reason", ""), "）")
} else {
  stop("未知的筛群策略: ", S_CLUSMODE, "（允许 none / by_name / by_cluster）", call. = FALSE)
}
if (ncol(merged) == 0) stop("筛群后细胞数为 0，停止。", call. = FALSE)
LOG("筛群后细胞数: ", ncol(merged), "（筛除 ", n_before_filter - ncol(merged), "）")

STEP("STEP 7 二次聚类（res=", P_RES2, "）")
merged <- FindNeighbors(merged, reduction = "harmony", dims = dims_use, verbose = FALSE)
merged <- FindClusters(merged, resolution = P_RES2, verbose = FALSE)
merged <- RunUMAP(merged, reduction = "harmony", dims = dims_use, verbose = FALSE)
merged <- RunTSNE(merged, reduction = "harmony", dims = dims_use, verbose = FALSE)
LOG("二次聚类群数: ", length(levels(merged$seurat_clusters)))

# ---- 剔除样本：来自配置，且样本名必须真实存在于本次结果 ----
S_DS <- as.character(unlist(S_DROPSMP)); S_DS <- S_DS[nzchar(S_DS)]
if (length(S_DS) > 0) {
  STEP("STEP 8 剔除样本")
  present_ds <- intersect(S_DS, unique(as.character(merged$orig.ident)))
  missing_ds <- setdiff(S_DS, unique(as.character(merged$orig.ident)))
  if (length(present_ds) == 0) {
    stop("配置要求剔除样本 ", paste(S_DS, collapse = ", "),
         "，但当前对象里一个都没有。\n  实际样本: ",
         paste(sort(unique(as.character(merged$orig.ident))), collapse = ", "),
         "\n  不静默跳过——剔除没生效但你以为生效了，是最坏的情况。", call. = FALSE)
  }
  if (length(missing_ds) > 0) {
    LOG("  注意：配置里有 ", length(missing_ds), " 个样本名不存在于本次结果: ",
        paste(missing_ds, collapse = ", "))
  }
  merged <- subset(merged, cells = colnames(merged)[!as.character(merged$orig.ident) %in% present_ds])
  LOG("  剔除 ", paste(present_ds, collapse = ","), " 后细胞数: ", ncol(merged))
} else {
  LOG("STEP 8 剔除样本：配置为空，不剔除任何样本。")
}

# =====================================================================
# 7. 保存 + 运行报告
# =====================================================================
STEP("STEP 9 保存")
merged@meta.data$orig.ident <- factor(merged@meta.data$orig.ident,
                                      levels = intersect(names(group_map), unique(as.character(merged$orig.ident))))
if (seurat_major < 5) merged@assays$RNA@scale.data <- matrix()
saveRDS(merged, file.path(OUTDIR, "processed.rds"))
LOG("已保存: ", file.path(OUTDIR, "processed.rds"))

manifest <- list(
  time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  config_path = param_log$config_path,
  params = param_log$params,
  param_sources = param_log$sources,
  seurat_version = seurat_ver,
  allow_v5 = isTRUE(argv$allow_v5),
  presto = has_presto,
  harmony = as.character(utils::packageVersion("harmony")),
  doubletfinder = as.character(utils::packageVersion("DoubletFinder")),
  seed = P_SEED,
  memory = list(mode = S_MEMMODE, use_blocked = USE_BLOCKED, decision = mem_decision,
                available_gb = avail_gb, required_gb = est_gb, estimate_note = est_note),
  step_modes = list(
    read_exclusion = list(mode = if (length(S_READEXCL) > 0) "exclude" else "none",
                          samples = as.character(unlist(S_READEXCL))),
    cluster_filter = list(mode = S_CLUSMODE, by_name_keep = as.character(unlist(S_CLUSKEEP)),
                          mapping_file = S_CLUSMAP),
    post_cluster_exclusion = list(mode = if (length(S_DS) > 0) "exclude" else "none",
                                  samples = S_DS),
    lowconfidence = list(mode = S_LOWMODE, min_cells = S_LOWMIN)
  ),
  steps = list(
    samples_used = sample_names,
    cells_after_qc = sum(sample_tab$qc_pass),
    cells_after_cluster1 = n_after_clu1,
    clusters_after_res1 = nrow(clu1_tab),
    cells_after_filter = ncol(merged) + if (length(S_DS) > 0) sum(!as.character(merged$orig.ident) %in% S_DS) else 0,
    clusters_after_res2 = length(levels(merged$seurat_clusters)),
    cells_final = ncol(merged),
    final_samples = sort(unique(as.character(merged$orig.ident)))
  ),
  skipped_or_failed = stop_log
)
writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE),
           file.path(OUTDIR, "run_manifest.json"))
writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE),
           file.path(OUTDIR, "status", "run_status.json"))

# 参数来源表：由配置生成，不手抄。分成三段便于核对。
src_tab <- cfg_param_table(cfg)
write.table(src_tab, file.path(OUTDIR, "status", "param_sources.tsv"),
            quote = FALSE, sep = "\t", row.names = FALSE)

report <- c(
  "# 运行报告", "",
  paste0("- 时间: ", manifest$time),
  paste0("- 配置: ", manifest$config_path),
  paste0("- Seurat: ", seurat_ver, ifelse(isTRUE(argv$allow_v5), "（--allow-v5 兼容模式）", "（与官方一致）")),
  paste0("- presto: ", ifelse(has_presto, "已启用", "未安装（仅影响速度）")),
  paste0("- 随机种子: ", P_SEED, "（来源: ", cfg_source(cfg, "params", "seed"), "）"),
  paste0("- 内存路径: ", mem_decision), "",
  "## 参数来源", "",
  "每个参数的取值与来源（official=官方口径 / instance=本机适配 / user=用户指定 / gate=人工确认）。",
  "完整表见 status/param_sources.tsv。", "",
  paste0("- 配置文件: ", param_log$config_path),
  paste0("- 参数总数: ", nrow(src_tab),
         "（official ", sum(src_tab$source == "official"),
         " / instance ", sum(src_tab$source == "instance"),
         " / user ", sum(src_tab$source == "user"),
         " / gate ", sum(src_tab$source == "gate"), "）"), "",
  "## 步骤策略", "",
  paste0("- 读入前剔除: ", if (length(S_READEXCL) > 0) paste(as.character(unlist(S_READEXCL)), collapse = ", ") else "无"),
  paste0("- 筛群: ", S_CLUSMODE,
         if (identical(S_CLUSMODE, "by_name")) paste0("（保留类型: ", paste(as.character(unlist(S_CLUSKEEP)), collapse = ", "), "）") else "",
         if (identical(S_CLUSMODE, "by_cluster")) "（按簇号，换数据必须重做映射表）" else ""),
  paste0("- 聚类后剔除样本: ", if (length(S_DS) > 0) paste(S_DS, collapse = ", ") else "无"),
  paste0("- 低置信门槛: ", S_LOWMODE, " / ", S_LOWMIN, " 个细胞"), "",
  "## 细胞数流转", "",
  paste0("- 质控后合计: ", sum(sample_tab$qc_pass)),
  paste0("- 一次聚类后: ", n_after_clu1, "，群数 ", nrow(clu1_tab)),
  paste0("- 筛群后: ", n_before_filter - (n_after_clu1 - ncol(merged)) + 0, "（筛除 ", n_before_filter - (n_after_clu1 - ncol(merged)) - 0, "）"),
  paste0("- 最终: ", ncol(merged), "，群数 ", length(levels(merged$seurat_clusters))),
  paste0("- 最终样本: ", paste(sort(unique(as.character(merged$orig.ident))), collapse = ", ")), "",
  "## 被跳过或失败的环节", "",
  if (length(stop_log) == 0) "- 无" else paste0("- ", unlist(stop_log))
)
writeLines(report, file.path(OUTDIR, "run_report.md"))

STEP("全部完成")
LOG("输出目录: ", OUTDIR)
