#!/usr/bin/env Rscript
# =====================================================================
#  decide_cli.R -- 自动判定的命令行入口
#
#  为什么单独一个文件：
#    decision.R 是「怎么判」的库（可被复用、可被测试），
#    decide_cli.R 是「跑一次判定并把结论打到屏幕上」的入口。
#    两件事混在一个文件里，以后想在别处复用判定逻辑就得连命令行一起搬。
#
#  用法：
#    Rscript engine/decide_cli.R --dir <运行目录> [--engine <引擎目录>]
#
#  它做什么：
#    1. 读一下这台机器：R 包齐不齐、内存够不够、数据目录在不在
#    2. 扫一遍机器上有没有现成的分析脚本（有的话要先定用哪套）
#    3. 把结论写进 <运行目录>/status/decision.json
#    4. 顺手把「机器上已有脚本」的位置写进闸门，让人在界面上能看到
#
#  它不做什么：
#    不替人做取舍决定（剔除哪些簇、剔哪个样本）。那些只把依据摆出来，由人定。
#
#  ASCII-only source（R 4.1.2 + 中文 Windows locale 不能解析非 ASCII 源码）。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, scipen = 999, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---- 找自己的位置（只用 sys.frame(1)$ofile 在 Rscript 下会拿到 NULL）----
SELF_DIR <- local({
  f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(f) && length(f) && nzchar(f)) return(dirname(normalizePath(f)))
  ca <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", ca, value = TRUE)
  if (length(m) > 0) {
    p <- sub("^--file=", "", m[1])
    if (nzchar(p) && file.exists(p)) return(dirname(normalizePath(p)))
  }
  "."
})

argv <- commandArgs(trailingOnly = TRUE)
get_opt <- function(k, d = NULL) {
  i <- match(k, argv)
  if (is.na(i)) return(d)
  if (i == length(argv) || grepl("^--", argv[i + 1])) return(d)
  argv[i + 1]
}

RUN_DIR    <- get_opt("--dir", NULL)
ENGINE_DIR <- get_opt("--engine", SELF_DIR)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("\n[!] jsonlite is required. install.packages('jsonlite')\n\n")
  quit(save = "no", status = 1)
}
if (is.null(RUN_DIR) || !nzchar(RUN_DIR) || !dir.exists(RUN_DIR)) {
  cat("\n[!] --dir needs an existing run directory\n\n")
  quit(save = "no", status = 2)
}
RUN_DIR <- normalizePath(RUN_DIR, winslash = "/", mustWork = FALSE)
if (!dir.exists(ENGINE_DIR)) {
  cat("\n[!] --engine is not a directory:", ENGINE_DIR, "\n\n")
  quit(save = "no", status = 2)
}
ENGINE_DIR <- normalizePath(ENGINE_DIR, winslash = "/", mustWork = FALSE)

# step_plan.R 提供 read_cfg / cfg_val（判定要用），decision.R 提供 auto_decide
PLAN <- file.path(ENGINE_DIR, "step_plan.R")
DEC  <- file.path(ENGINE_DIR, "decision.R")
for (f in c(PLAN, DEC)) {
  if (!file.exists(f)) {
    cat("\n[!] missing:", f, "\n    the package may be incomplete.\n\n")
    quit(save = "no", status = 1)
  }
}
source(PLAN, local = TRUE)
source(DEC, local = TRUE)

say <- function(...) { cat(enc2native(paste0(...)), "\n", sep = ""); flush(stdout()) }
msg <- function(key) {
  v <- dec_msg(key, ENGINE_DIR)
  if (is.null(v)) return(NULL)
  v
}

say("")
say(msg("head") %||% "auto decision")

res <- tryCatch(auto_decide(RUN_DIR, ENGINE_DIR),
                error = function(e) { say(paste0("[!] 判定失败：", conditionMessage(e))); NULL })
if (is.null(res)) {
  # 判定失败不能伪装成成功：界面要能看出是「没判成」而不是「判完说没问题」
  quit(save = "no", status = 1)
}

# 把依据接进闸门：判定结论本身也是人做决定时要看的材料
tryCatch(sync_gate_evidence(RUN_DIR, ENGINE_DIR), error = function(e) NULL)

say("")
switch(res$path,
  go           = say(msg("verdict_go") %||% "verdict: go"),
  blocked      = say(msg("verdict_blocked") %||% "verdict: blocked"),
  need_choice  = say(msg("verdict_need_choice") %||% "verdict: need choice"),
  say(paste0("verdict: ", res$path))
)

if (length(res$gaps)) {
  say("")
  say(msg("gaps_head") %||% "to fix:")
  for (g in res$gaps) say(paste0("  - ", g))
}

say("")
say(msg("checks_head") %||% "checks:")
for (x in res$checks %||% list()) {
  mark <- switch(as.character(x$status),
                 ok = "[ OK ]", warn = "[WARN]",
                 fail = "[FAIL]", unknown = "[ ?? ]", "[    ]")
  say(sprintf("  %s %-18s %s", mark, as.character(x$label), as.character(x$detail)))
}

say("")
say(sprintf("判定结果：%s", file.path(RUN_DIR, "status", "decision.json")))
say("")
quit(save = "no", status = 0L)
