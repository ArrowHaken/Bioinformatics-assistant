#!/usr/bin/env Rscript
# =====================================================================
# 00_run_all.R — 一键跑全流程：预处理 → 注释
#
# 本脚本只做编排与检查，不重复实现任何分析逻辑。
# 每一步失败即中止，并把失败原因原样打印（不做降级、不跳过继续）。
#
# 参数一律走配置文件（pipeline.json）。脚本里不再有业务默认值。
#
# 用法：
#   Rscript 00_run_all.R --config <pipeline.json> [--resume-from <步骤>] [--stage <段>]
#
# --stage 控制主流程分几段跑：
#   all       一次跑完（没有人需要插手的场景）
#   cluster1  跑到一次聚类、写出人工判断依据后停住等人定
#   continue  读回快照，接着做筛群、二次聚类、剔样本与注释
#   为什么要有分段：筛群与剔样本是人定的，依据只有跑到一次聚类才产生。
#   顺序只能是「跑到那里 → 出依据 → 停 → 人定 → 接着跑」。
#
# 首次使用：
#   Rscript 00_run_all.R --config <路径> --init-config
#   （生成一份带注释的默认配置，再由工作台或人工填写）
#
# --resume-from 让长流程可以从中间步骤继续（细胞数大时很有用）：
#   precheck | read | doublet | qc | merge | cluster1 | filter | cluster2 | final | annot
# =====================================================================

args <- commandArgs(trailingOnly = TRUE)
get_opt <- function(k, d = NULL) {
  i <- match(k, args)
  if (is.na(i)) return(d)
  if (i == length(args) || grepl("^--", args[i + 1])) return(d)
  args[i + 1]
}
has_flag <- function(k) k %in% args

LOG <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))

# 脚本目录定位：要同时支持 source() 和 Rscript 直接调用两种方式。
#   只用 sys.frame(1)$ofile 时，Rscript 直接跑会拿到 NULL，
#   表现为「找不到流程脚本: ./01_preprocess.R」，很难排查。
#   所以补一条从 --file= 取路径的退路。
script_dir <- local({
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(f) && nzchar(f)) return(dirname(normalizePath(f)))
  ca <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", ca, value = TRUE)
  if (length(m) > 0) {
    p <- sub("^--file=", "", m[1])
    if (nzchar(p) && file.exists(p)) return(dirname(normalizePath(p)))
  }
  "."
})
# ---- 入口守卫：拒绝被直接调用 ----
#   00_run_all.R 是编排入口，绕过它等于所有约束失效：
#   谁调的、用了哪套脚本、产物指纹是什么，事后都查不到。
#   守卫位置：往上找 workflow/guard_load.R。找不到就停 —— 不静默跳过。
#   为什么必须停：守卫缺失时继续跑，等于约束失效而没人知道，比直接报错危险得多。
.guard_load <- local({
  cands <- c(file.path(script_dir, "..", "..", "workflow", "guard_load.R"),
             file.path(script_dir, "..", "workflow", "guard_load.R"),
             file.path(script_dir, "..", "..", "..", "workflow", "guard_load.R"))
  hit <- cands[file.exists(cands)]
  if (!length(hit)) {
    stop("\n  拒绝运行：找不到入口守卫 workflow/guard_load.R。\n",
         "  本脚本必须经由统一入口执行，守卫缺失时不能继续。\n",
         "  找过：\n    ", paste(cands, collapse = "\n    "), "\n", call. = FALSE)
  }
  hit[1]
})
source(.guard_load, local = FALSE)
sc_guard_banner("orchestrate", script_dir)

step1 <- file.path(script_dir, "01_preprocess.R")
step2 <- file.path(script_dir, "02_annotate.R")

for (f in c(step1, step2)) {
  if (!file.exists(f)) stop("找不到流程脚本: ", f, "（脚本目录: ", script_dir, "）", call. = FALSE)
}

cfg_path <- get_opt("--config")
if (is.null(cfg_path) || is.na(cfg_path)) {
  stop("必填: --config <pipeline.json>\n",
       "  首次使用可加 --init-config 生成默认配置：\n",
       "  Rscript 00_run_all.R --config <路径> --init-config", call. = FALSE)
}
rscript_bin <- file.path(R.home("bin"), "Rscript")

# ---------------------------------------------------------------------
# --init-config：只生成配置，不跑分析
# ---------------------------------------------------------------------
if (has_flag("--init-config")) {
  st0 <- system2(rscript_bin, args = shQuote(c(step1, "--config", cfg_path, "--init-config")))
  if (st0 != 0) stop("生成默认配置失败（退出码 ", st0, "）", call. = FALSE)
  LOG("下一步：打开工作台填写 environment / io / 各闸门项，或直接编辑该文件。")
  quit(status = 0)
}

# ---------------------------------------------------------------------
# 读配置，先看有没有未确认的闸门
# ---------------------------------------------------------------------
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("缺少 jsonlite 包（读取配置用）。安装：install.packages('jsonlite')", call. = FALSE)
}
if (!file.exists(cfg_path)) stop("配置文件不存在: ", cfg_path, call. = FALSE)
cfg <- jsonlite::fromJSON(cfg_path, simplifyVector = FALSE)

gates_file <- get_opt("--gates", file.path(dirname(cfg_path), "gates.json"))
indir  <- get_opt("--indir",  if (is.null(cfg$io$indir$value))  NULL else cfg$io$indir$value)
outdir <- get_opt("--outdir", if (is.null(cfg$io$outdir$value)) NULL else cfg$io$outdir$value)
if (is.null(indir) || !nzchar(indir))   stop("配置里 io.indir 为空，且命令行未给 --indir。", call. = FALSE)
if (is.null(outdir) || !nzchar(outdir)) stop("配置里 io.outdir 为空，且命令行未给 --outdir。", call. = FALSE)

# ---- 分段：把 --stage 原样交给 01_preprocess.R ----
#   注意：这个解析必须在闸门检查之前，因为闸门检查要知道查哪一段。
STAGE <- get_opt("--stage", "all")
if (!STAGE %in% c("all", "cluster1", "continue")) {
  stop("--stage 取值非法: ", STAGE, "（允许 all / cluster1 / continue）", call. = FALSE)
}
if (!identical(STAGE, "all")) LOG("分段模式: --stage ", STAGE)

# 闸门检查：交给 01_preprocess.R 里的同一套逻辑，避免两处实现不一致
source(file.path(script_dir, "lib", "config.R"))
# 按段位检查：cluster1 段只需 A，continue 段需要 B。
#   理由同 01_preprocess.R：B 段的依据要跑完上段才有。
GATE_PHASE <- if (identical(STAGE, "continue")) "B" else "A"
cfg_require_resolved(cfg, if (file.exists(gates_file)) gates_file else NULL, GATE_PHASE)
LOG("配置闸门检查通过（第 ", GATE_PHASE, " 段）。")

# 该跑哪几步
resume_from <- get_opt("--resume-from", "precheck")
VALID_STEPS <- c("precheck", "read", "doublet", "qc", "merge", "cluster1", "filter", "cluster2", "final", "annot")
if (!resume_from %in% VALID_STEPS) {
  stop("--resume-from 取值非法: ", resume_from, "\n  允许: ", paste(VALID_STEPS, collapse = " / "), call. = FALSE)
}
LOG("本次从步骤 [", resume_from, "] 开始。")

# ---------------------------------------------------------------------
# 第 1 步：预处理（读矩阵 → 去双 → 质控 → 合并 → 聚类 → 筛群 → 剔样本）
#   只有从 precheck 开始时才需要跑；后续步骤依赖 01_single 下的中间对象。
# ---------------------------------------------------------------------
run_preprocess <- !resume_from %in% c("cluster1", "filter", "cluster2", "final", "annot") ||
                  !file.exists(file.path(outdir, "01_single"))
# continue 段：前面已经算过并落了快照，不重跑，直接读回来接着做。
if (identical(STAGE, "continue")) run_preprocess <- TRUE
if (run_preprocess) {
  LOG("===== 第 1 步：预处理 =====")
  cmd1 <- c(step1, "--config", cfg_path, "--indir", indir, "--outdir", outdir)
  if (file.exists(gates_file)) cmd1 <- c(cmd1, "--gates", gates_file)
  for (k in c("--allow-v5", "--dry-run")) if (has_flag(k)) cmd1 <- c(cmd1, k)
  cmd1 <- c(cmd1, "--stage", STAGE)
  LOG("调用: ", rscript_bin, " ", paste(cmd1, collapse = " "))
  t0 <- Sys.time()
  st1 <- system2(rscript_bin, args = shQuote(cmd1))
  if (st1 != 0) {
    stop("第 1 步预处理失败（退出码 ", st1, "）。\n",
         "  请查看上面的报错。本编排不做降级、不跳过继续。", call. = FALSE)
  }
  LOG("第 1 步完成，用时 ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " 分钟")
} else {
  LOG("按 --resume-from 跳过预处理，沿用已有中间对象。")
}

processed <- file.path(outdir, "processed.rds")
if (!file.exists(processed)) stop("找不到产物: ", processed, call. = FALSE)

# ---------------------------------------------------------------------
# 第 2 步：注释（reference / lit-dict 只在配置里给）
# ---------------------------------------------------------------------
# cluster1 段只跑到一次聚类，注释放在 continue 段做
run_annot <- !identical(STAGE, "cluster1") && (identical(resume_from, "annot") || run_preprocess)
if (identical(STAGE, "cluster1")) {
  LOG("分段模式 cluster1：本次只跑到一次聚合并写出人工判断依据，注释留到 continue 段。")
}
if (run_annot) {
  reference <- if (is.null(cfg$io$reference_table$value)) NULL else cfg$io$reference_table$value
  lit_dict  <- if (is.null(cfg$io$lit_dict$value))       NULL else cfg$io$lit_dict$value
  if (is.null(reference) && is.null(lit_dict)) {
    LOG("配置未给 io.reference_table 或 io.lit_dict，跳过注释。")
    LOG("注意：没有 annot 列，出图 skill 的「注释段 / 差异富集段」无法产出。")
  } else {
    LOG("===== 第 2 步：文献驱动注释 =====")
    anno_out <- file.path(outdir, "annot")
    dir.create(anno_out, showWarnings = FALSE, recursive = TRUE)
    cmd2 <- c(step2, processed, "--outdir", anno_out)
    if (!is.null(reference)) cmd2 <- c(cmd2, "--reference", reference)
    if (!is.null(lit_dict))  cmd2 <- c(cmd2, "--lit-dict", lit_dict)
    LOG("调用: ", rscript_bin, " ", paste(cmd2, collapse = " "))
    t1 <- Sys.time()
    st2 <- system2(rscript_bin, args = shQuote(cmd2))
    if (st2 != 0) stop("第 2 步注释失败（退出码 ", st2, "）。", call. = FALSE)
    LOG("第 2 步完成，用时 ", round(as.numeric(difftime(Sys.time(), t1, units = "mins")), 1), " 分钟")
    annotated <- file.path(anno_out, "annotated.rds")
    if (!file.exists(annotated)) stop("第 2 步声称成功但找不到产物: ", annotated, call. = FALSE)
    LOG("可交给出图 skill（sc-plot-kit）的对象: ", annotated)
  }
} else {
  LOG("按 --resume-from 跳过注释。")
}

LOG("===== 全流程完成 =====")
LOG("输出目录: ", outdir)
LOG("核对这几份产物：")
LOG("  run_report.md              运行报告（含参数来源）")
LOG("  run_manifest.json          机器可读的参数+细胞数流转")
LOG("  status/param_sources.tsv   每个参数的值与来源（official/instance/user/gate）")
LOG("  status/precheck.json       容量预检的判定过程")
LOG("  status/cluster1.json       一次聚类结果（供工作台做筛群决策）")
LOG("注意：outputs 里的每个参数都能追到来源，不要凭记忆判断用了什么参数。")
