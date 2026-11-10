#!/usr/bin/env Rscript
# =====================================================================
#  sc_workflow.R -- THE ENTRY POINT. Nothing else may run the pipeline.
#
#  WHY A HARD ENTRY POINT
#    A rule written in a document cannot stop anyone -- including an AI
#    assistant -- from calling 01_preprocess.R directly. Documentation is
#    advice. This file is a gate.
#
#    Every low-level script starts by calling sc_guard_banner(). That
#    function checks three environment variables which ONLY this file
#    sets. Run a script by hand and it refuses, loudly, with the reason.
#    Bypassing means editing guard.R -- which changes a file hash and
#    breaks the ledger chain, so it shows up instead of hiding.
#
#  WHAT THE GATE ACTUALLY PROTECTS  (not ceremony)
#    1. parameter provenance -- every value carries where it came from
#       (official / instance / user / gate). Unknown provenance is not
#       "probably fine", it is a result you cannot audit.
#    2. stop-on-unresolved -- if a parameter is still undecided the run
#       halts. Guessing does not throw an error later; it produces a
#       plausible-looking wrong answer.
#    3. an append-only ledger -- what ran, when, with which parameters,
#       exit code, output fingerprints. Without it, "which numbers did we
#       ship and could we regenerate them" has no answer.
#
#  USAGE
#    Rscript engine/sc_workflow.R <cmd> --dir <run-dir> [options]
#
#    init      generate default config; register any pre-existing results
#    status    per-step state, what is missing, what is blocked
#    gates     list the decisions that require a human
#    answer    record one decision   --gate <id> --value <v> [--detail <d>]
#    run       run one step          --step <env|precheck|pipeline|plots|ppt|pack> [--force]
#    run-all   run every step in order, stop on first failure
#    ledger    dump the execution ledger
#    verify    re-check that ledgered outputs still exist and are unchanged
#
#  ASCII-only source: R 4.1.2 on a Chinese Windows locale cannot parse
#  non-ASCII source. All Chinese strings live in workflow/messages.json.
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, scipen = 999, warn = -1))

# ---- locate self ----
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
ENGINE_DIR <- SELF_DIR
GUARD_DIR <- file.path(ENGINE_DIR, "workflow")

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("\n[!] jsonlite package is required. Install: install.packages('jsonlite')\n\n")
  quit(save = "no", status = 1)
}

PLAN_FILE <- file.path(ENGINE_DIR, "step_plan.R")
if (!file.exists(PLAN_FILE)) {
  cat("\n[!] missing engine/step_plan.R -- the package may be incomplete.\n\n")
  quit(save = "no", status = 1)
}
source(PLAN_FILE, local = TRUE)

GUARD_FILE <- file.path(GUARD_DIR, "guard.R")
if (!file.exists(GUARD_FILE)) {
  cat("\n[!] missing engine/workflow/guard.R\n\n")
  quit(save = "no", status = 1)
}
source(GUARD_FILE, local = TRUE)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---------------------------------------------------------------------
# 查表：这一步该用库里哪一套
#
#  台账缺的三个字段（环境 / 需求 / 取的是哪套）就从这里来。
#  缺了它们，台账能回答「这一步跑没跑成」，回答不了
#  「这批结果用的是哪一套脚本」—— 而后者才是复现的关键。
#
#  设计上只调 pick.R 一个入口，不在这里重写判定逻辑。
#  两处各写一份判定，早晚出现「报告说用 A、实际用了 B」。
# ---------------------------------------------------------------------
PICK_FILE <- file.path(ENGINE_DIR, "pick.R")
pick_for_ledger <- function(category = "A", demand = "D1_pipeline",
                            purpose = NULL, local_scripts_found = FALSE) {
  if (!file.exists(PICK_FILE)) {
    return(list(env = "unknown", demand = demand, set_id = "unknown",
                reason = "engine/pick.R 不存在，无法查表"))
  }
  r <- tryCatch({
    source(PICK_FILE, local = TRUE)
    pick_set(ENGINE_DIR, category = category, demand = demand,
             purpose = purpose, local_scripts_found = local_scripts_found)
  }, error = function(e) NULL)
  if (is.null(r)) {
    return(list(env = "unknown", demand = demand, set_id = "unknown",
                reason = "查表出错"))
  }
  if (!is.null(r$error)) {
    return(list(env = r$env$id %||% "unknown", demand = demand, set_id = "none",
                reason = r$error))
  }
  list(
    env = r$env$id %||% "unknown",
    env_why = r$env$why %||% "",
    memory_gb = r$env$memory_gb %||% NA_real_,
    demand = demand,
    set_id = r$id %||% (if (!is.null(r$entries)) "B-group" else "none"),
    set_title = r$title %||% "",
    set_status = r$status %||% "",
    set_source = r$source %||% "",
    set_usable = isTRUE(r$usable),
    set_reason = if (!is.null(r$reason)) r$reason else "",
    requires_why = if (!is.null(r$requires_why)) as.list(r$requires_why) else list()
  )
}

# 每一步对应哪个需求坐标。
#   写死在这里，不靠临场判断 —— 判断一次错一次，台账就废了。
step_demand <- function(stepId) {
  switch(stepId,
         env       = "D0_env",
         decide    = "D0_env",
         precheck  = "D1_pipeline",
         pipeline1 = "D1_pipeline",
         pipeline2 = "D1_pipeline",
         plots     = "D2_plots",
         ppt       = "D2_ppt",
         pack      = "D2_pack",
         "D1_pipeline")
}
# 这一步属于哪个大类：A 代码处理 / B 分析绘图
step_category <- function(stepId) {
  if (stepId %in% c("plots", "ppt")) "B" else "A"
}
step_purpose <- function(stepId) {
  switch(stepId, plots = "plots", ppt = "ppt", NULL)
}

# ---------------------------------------------------------------------
# 自检：入口在动手之前，先确认「守卫代码本身没被改过」
#
#  为什么这段不能靠 guard.R 里的函数来做：
#    如果把守卫整个换成「永远放行」，那么调用守卫里的检查函数也没意义 ——
#    它已经不可信了。所以这一小段 md5 计算直接写在这里，不依赖 guard.R。
#
#  这不是防破解，是防「悄悄改」。改过就停，且台账里留着 init 时登记的
#  原始 md5，事后能对出来。
# ---------------------------------------------------------------------
GUARD_RS <- file.path(GUARD_DIR, "guard.R")

verify_guard_intact <- function() {
  lock_f <- file.path(RUN_DIR, "status", "guard.lock.json")
  if (!file.exists(lock_f)) {
    out("")
    out("  拒绝执行：这个项目没有登记过守卫指纹（status/guard.lock.json 不存在）。")
    out("  先跑一次：Rscript engine/sc_workflow.R init --dir ", RUN_DIR)
    out("")
    return(FALSE)
  }
  if (!file.exists(GUARD_RS)) {
    out("")
    out("  拒绝执行：找不到 engine/workflow/guard.R。包不完整，不能带着不确定的守卫往下跑。")
    out("")
    return(FALSE)
  }
  lock <- tryCatch({
    txt <- paste(readLines(lock_f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    jsonlite::fromJSON(txt, simplifyVector = FALSE)
  }, error = function(e) NULL)
  if (is.null(lock) || is.null(lock$guard_md5)) {
    out(""); out("  拒绝执行：守卫指纹文件读不出来或格式不对。"); out("")
    return(FALSE)
  }
  now <- tryCatch(unname(tools::md5sum(GUARD_RS)), error = function(e) NA_character_)
  if (!identical(now, lock$guard_md5)) {
    out("")
    out("  拒绝执行：守卫代码被改过。")
    out(paste0("    登记时：", lock$guard_md5))
    out(paste0("    现在：  ", now))
    out("")
    out("  这意味着「必须先通过入口」这条约束可能已经失效，所以一律停下。")
    out("  如果你确实是有意修改守卫，请重新 init 登记，并在台账里说明为什么。")
    out("")
    # 即使返回 FALSE 也要留痕：改过这件事必须能查到
    sc_ledger_append(RUN_DIR, list(
      run_id = paste0("guard-tamper-", format(Sys.time(), "%Y%m%d%H%M%S")),
      event = "blocked", step = "guard",
      time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      reason = "guard.R modified", locked = lock$guard_md5, now = now))
    return(FALSE)
  }
  TRUE
}

# ---------------------------------------------------------------------
# messages
# ---------------------------------------------------------------------
M <- function(key) {
  v <- sc_msg(GUARD_DIR, key)
  if (is.null(v)) return(paste0("<missing message: ", key, ">"))
  v
}
Mf <- function(key, ...) {
  txt <- M(key)
  a <- list(...)
  if (length(a)) {
    for (nm in names(a)) {
      v <- a[[nm]]
      # NULL / 长度为 0 的值直接跳过。
      #   踩过坑：as.character(NULL) 是 character(0)，gsub 的 replacement
      #   不接受长度 0，会抛 "invalid 'replacement' argument" ——
      #   而这个报错出现在「打状态」这种最基础的路径上，很难联想到是占位符。
      if (is.null(v) || length(v) == 0L) next
      txt <- gsub(paste0("{", nm, "}"), as.character(v)[1], txt, fixed = TRUE)
    }
  }
  txt
}
out <- function(...) { cat(enc2native(paste0(...)), "\n", sep = ""); flush(stdout()) }
outl <- function(v) if (length(v)) for (x in v) out(x)

# ---------------------------------------------------------------------
# args
# ---------------------------------------------------------------------
argv <- commandArgs(trailingOnly = TRUE)
get_opt <- function(k, d = NULL) {
  i <- match(k, argv)
  if (is.na(i)) return(d)
  if (i == length(argv) || grepl("^--", argv[i + 1])) return(d)
  argv[i + 1]
}
has_flag <- function(k) k %in% argv
RUN_DIR_ARG <- get_opt("--dir", NULL)

VALID_CMDS <- c("init", "status", "gates", "answer", "run", "run-all", "ledger", "verify", "help")

# 子命令定位：找第一个「像命令」的词，跳过选项和它们的值。
#   为什么不直接取 argv[1]：调用方可能写成 `--dir <目录> run`，
#   那样 argv[1] 是 "--dir"，直接取会静默退化成 status ——
#   表现是「点了运行，却只打印了一份状态」，很难看出是参数顺序问题。
#   扫一遍，两种顺序都能用。
detect_cmd <- function() {
  opts_with_value <- c("--dir", "--step", "--gate", "--value", "--detail")
  i <- 1L
  while (i <= length(argv)) {
    a <- argv[i]
    if (a %in% opts_with_value) { i <- i + 2L; next }
    if (grepl("^--", a)) { i <- i + 1L; next }
    if (a %in% VALID_CMDS) return(a)
    i <- i + 1L
  }
  "status"
}
CMD <- detect_cmd()

# ---------------------------------------------------------------------
# run-dir resolution
#   --dir wins; otherwise walk up from cwd looking for a project marker.
#   Never guesses a data directory: guessing a path is how you end up
#   analysing the wrong dataset without noticing.
# ---------------------------------------------------------------------
find_project_dir <- function(start = getwd()) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in 1:8) {
    if (file.exists(file.path(d, "pipeline.json"))) return(d)
    if (file.exists(file.path(d, "status", "ledger.jsonl"))) return(d)
    p <- dirname(d)
    if (identical(p, d)) break
    d <- p
  }
  NULL
}

RUN_DIR <- NULL
if (!is.null(RUN_DIR_ARG) && nzchar(RUN_DIR_ARG)) {
  RUN_DIR <- normalizePath(RUN_DIR_ARG, winslash = "/", mustWork = FALSE)
} else if (!CMD %in% c("help")) {
  RUN_DIR <- find_project_dir()
}

if (CMD == "help" || !CMD %in% VALID_CMDS) {
  outl(M("cli_usage"))
  quit(save = "no", status = if (CMD == "help") 0L else 2L)
}

if (is.null(RUN_DIR) || !nzchar(RUN_DIR)) {
  out("")
  out("需要指定运行目录：Rscript engine/sc_workflow.R status --dir <运行目录>")
  out("（或者 cd 到项目目录里再运行）")
  out("")
  quit(save = "no", status = 2L)
}
if (!dir.exists(RUN_DIR)) {
  out(sprintf("运行目录不存在：%s", RUN_DIR)); quit(save = "no", status = 2L)
}

status_dir <- file.path(RUN_DIR, "status")
ledger_path <- file.path(status_dir, "ledger.jsonl")

# ---------------------------------------------------------------------
# init
# ---------------------------------------------------------------------
if (CMD == "init") {
  dir.create(status_dir, showWarnings = FALSE, recursive = TRUE)
  written <- init_config(RUN_DIR, ENGINE_DIR, isTRUE(has_flag("--overwrite")))

  # 把守卫自身的指纹锁下来。
  #   锁的是「检查代码有没有被改过」。改 guard.R 而不重跑 init，
  #   之后所有运行都会停 —— 包括我自己去改的情况。
  lock <- sc_write_guard_lock(RUN_DIR, GUARD_DIR)

  # 接管已有产物：目录里本来就有结果时，把它们登记成「接手前就有」，
  # 而不是假装是我们跑出来的。这一步是为了让台账说真话。
  pre <- character(0)
  for (sid in names(STEP_OUTPUTS)) {
    for (rel in STEP_OUTPUTS[[sid]]) {
      if (file.exists(file.path(RUN_DIR, rel))) pre <- c(pre, rel)
    }
  }
  rid <- paste0("init-", format(Sys.time(), "%Y%m%d%H%M%S"))
  sc_ledger_append(RUN_DIR, list(
    run_id = rid, event = "init", step = "init",
    time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    engine = ENGINE_DIR, run_dir = RUN_DIR,
    guard_md5 = lock$guard_md5,
    written = as.list(written), pre_existing = as.list(pre)
  ))

  out("")
  out(M("cli_title"))
  out(paste0("  运行目录：", RUN_DIR))
  out("")
  out(Mf("cli_init_done", written = paste(written, collapse = ", ")))
  if (length(pre)) out(Mf("cli_init_existing", found = paste(pre, collapse = ", ")))
  out("")
  out(paste0("下一步：Rscript engine/sc_workflow.R gates --dir ", RUN_DIR))
  out("")
  quit(save = "no", status = 0L)
}

# ---------------------------------------------------------------------
# shared loaders
# ---------------------------------------------------------------------
load_cfg <- function() read_cfg(RUN_DIR)
load_gates <- function() read_json_file(file.path(RUN_DIR, "gates.json"))
save_gates <- function(g) {
  f <- file.path(RUN_DIR, "gates.json")
  con <- file(f, open = "wb")
  writeBin(charToRaw(enc2utf8(jsonlite::toJSON(g, auto_unbox = TRUE, pretty = TRUE,
                                              null = "null", digits = NA))), con)
  close(con)
}
save_cfg <- function(cfg) {
  f <- file.path(RUN_DIR, "pipeline.json")
  con <- file(f, open = "wb")
  writeBin(charToRaw(enc2utf8(jsonlite::toJSON(cfg, auto_unbox = TRUE, pretty = TRUE,
                                              null = "null", digits = NA))), con)
  close(con)
}

# 每一步的状态：把台账 + 产物是否存在 + 依赖 合成一个判断
step_state_of <- function(cfg) {
  led_st <- sc_ledger_state(RUN_DIR)
  res <- list()
  for (sid in CHAIN_ORDER) {
    e <- led_st[[sid]]
    stat <- if (is.null(e)) "not_run"
            else if (identical(e$event, "finish") && identical(as.integer(e$code %||% 1L), 0L)) "done"
            else if (identical(e$event, "start")) "running"
            else if (identical(e$event, "blocked")) "blocked"
            else "failed"
    # 产物在不在（台账说成功但文件没了 → 状态回落）
    outs <- (STEP_OUTPUTS[[sid]] %||% character(0))
    present <- if (length(outs)) outs[file.exists(file.path(RUN_DIR, outs))] else character(0)
    if (identical(stat, "done") && length(outs) && !length(present)) stat <- "failed"

    # 依赖没完成 → 不能跑
    deps <- (STEP_DEPS[[sid]] %||% character(0))
    dep_block <- character(0)
    for (d in deps) {
      ds <- res[[d]]$state
      if (is.null(ds) || !identical(ds, "done")) dep_block <- c(dep_block, d)
    }
    res[[sid]] <- list(state = stat, present = present, missing = setdiff(outs, present),
                       deps = deps, dep_block = dep_block, ledger = e)
  }
  res
}

state_label <- function(s) {
  switch(s,
    done = M("cli_step_done"), running = M("cli_step_running"),
    failed = M("cli_step_failed"), blocked = M("cli_step_blocked"),
    not_run = M("cli_step_not_run"), pre_existing = M("cli_step_pre_existing"),
    M("cli_step_not_run"))
}

# ---------------------------------------------------------------------
# status
# ---------------------------------------------------------------------
if (CMD == "status") {
  cfg <- load_cfg()
  if (is.null(cfg)) { out(""); out(Mf("cli_no_project", dir = RUN_DIR)); out(""); quit(save = "no", status = 2L) }
  st <- step_state_of(cfg)
  pendA <- pending_gate_names(RUN_DIR, "A") %||% character(0)
  pendB <- pending_gate_names(RUN_DIR, "B") %||% character(0)

  out("")
  out(M("cli_title"))
  out(paste0("  运行目录：", RUN_DIR))
  out("")
  out("  步骤            状态          说明")
  for (sid in CHAIN_ORDER) {
    s <- st[[sid]]
    note <- ""
    # 依赖没满足优先说依赖 —— 那比闸门更靠前，先解决它才有意义
    if (length(s$dep_block)) {
      note <- Mf("cli_step_note_dep", dep = s$dep_block[1],
                 dep_state = state_label(st[[s$dep_block[1]]]$state))
    } else if (step_needs_gate(sid)) {
      # 只报「这一步真正卡住的那几条」。
      #   以前是笼统报「还有 N 件没答」，于是上段会显示被下段的问题挡住 ——
      #   而那时候还没有依据去答下段，看的人只会觉得莫名其妙。
      ph <- step_gate_phase(sid)
      p <- if (identical(ph, "B")) pendB else pendA
      if (length(p)) {
        note <- sprintf("要先定 %d 件事：%s", length(p), paste(utils::head(p, 2), collapse = "；"))
      } else if (identical(ph, "B")) {
        note <- "上段跑完、依据出来后再定这里的事"
      } else {
        note <- "可以跑"
      }
      if (!length(p) && identical(s$state, "done")) note <- "可以跑（已跑过）"
    } else if (identical(s$state, "done")) {
      note <- "可以跑（已跑过）"
    } else {
      note <- M("cli_step_note_ready")
    }
    out(sprintf("  %-14s  %-12s  %s", sid, state_label(s$state), note))
  }
  out("")
  if (length(pendA)) {
    out(sprintf("跑主流程之前要定（%d 件）：", length(pendA)))
    for (p in pendA) out(paste0("    - ", p))
  } else {
    out("跑主流程之前要定的：都定好了")
  }
  if (length(pendB)) {
    out(sprintf("一次聚类之后、继续往下跑之前要定（%d 件）：", length(pendB)))
    for (p in pendB) out(paste0("    - ", p))
    out("    这几件的依据，要等上段跑完才会算出来（簇大小、样本分布、marker）。")
  }
  out("")
  quit(save = "no", status = 0L)
}

# ---------------------------------------------------------------------
# gates
# ---------------------------------------------------------------------
if (CMD == "gates") {
  g <- load_gates()
  if (is.null(g) || is.null(g$gates)) { out(""); out(Mf("cli_no_project", dir = RUN_DIR)); out(""); quit(save = "no", status = 2L) }
  out("")
  out("必须由人决定的事")
  out("")
  for (one in g$gates) {
    stt <- one$status %||% "pending"
    mark <- if (identical(stt, "pending")) "[待答]" else if (identical(stt, "answered")) "[已答]" else "[自动]"
    out(sprintf("%s  %-26s  %s", mark, one$id %||% "?", one$title %||% ""))
    if (identical(stt, "pending")) {
      if (!is.null(one$question)) out(paste0("        ", one$question))
      sug <- one$suggested %||% NULL
      if (!is.null(sug)) out(paste0("        建议：", sug))
    } else {
      out(paste0("        答复：", one$answer %||% "-",
                 if (!is.null(one$answer_detail) && length(one$answer_detail))
                   paste0("（", paste(unlist(one$answer_detail), collapse = ", "), "）") else ""))
    }
  }
  out("")
  out("回答问题：")
  out(paste0("  Rscript engine/sc_workflow.R answer --dir ", RUN_DIR, " --gate <id> --value <值>"))
  out("")
  quit(save = "no", status = 0L)
}

# ---------------------------------------------------------------------
# answer
# ---------------------------------------------------------------------
if (CMD == "answer") {
  gid <- get_opt("--gate", NULL)
  val <- get_opt("--value", NULL)
  det <- get_opt("--detail", NULL)
  if (is.null(gid) || is.null(val)) {
    out("用法：answer --dir <目录> --gate <id> --value <值> [--detail <补充>]")
    quit(save = "no", status = 2L)
  }
  g <- load_gates()
  if (is.null(g) || is.null(g$gates)) { out(Mf("cli_no_project", dir = RUN_DIR)); quit(save = "no", status = 2L) }

  hit <- 0L
  for (i in seq_along(g$gates)) {
    if (identical(g$gates[[i]]$id, gid)) {
      g$gates[[i]]$status <- "answered"
      g$gates[[i]]$answer <- val
      if (!is.null(det)) {
        parts <- trimws(strsplit(det, ",")[[1]])
        parts <- parts[nzchar(parts)]
        g$gates[[i]]$answer_detail <- if (length(parts) > 1) as.list(parts) else (if (length(parts)) parts[1] else NULL)
      }
      g$gates[[i]]$answered_by <- "workflow-entry"
      g$gates[[i]]$answered_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      hit <- i
    }
  }
  if (hit == 0L) { out(paste0("没有这一项：", gid)); quit(save = "no", status = 2L) }
  g$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  save_gates(g)

  # 同步写回 pipeline.json，让脚本直接能读到（网页入口也走同一套映射）
  cfg <- load_cfg()
  if (!is.null(cfg)) {
    sync_gate_into_cfg <- function(cfg, gid, val, det) {
      if (identical(gid, "qc_threshold") && !identical(val, "keep")) {
        cfg$params$qc_min_features <- list(value = suppressWarnings(as.numeric(val)),
                                           source = "user", why = "经入口确认")
      }
      if (identical(gid, "cluster_filter")) {
        cfg$steps$cluster_filter$source <- "user"
        if (identical(val, "none")) { cfg$steps$cluster_filter$mode <- "none"; cfg$steps$cluster_filter$by_name_keep <- list() }
        if (identical(val, "by_name")) {
          cfg$steps$cluster_filter$mode <- "by_name"
          cfg$steps$cluster_filter$by_name_keep <- as.list(strsplit(det %||% "", ",")[[1]])
        }
        if (identical(val, "by_cluster")) { cfg$steps$cluster_filter$mode <- "by_cluster"; cfg$steps$cluster_filter$reason <- "经入口确认（簇号仅本次有效）" }
      }
      if (identical(gid, "read_exclusion")) {
        cfg$steps$read_exclusion$source <- "user"
        cfg$steps$read_exclusion$mode <- if (identical(val, "none")) "none" else "exclude"
        cfg$steps$read_exclusion$samples <- if (identical(val, "none")) list() else as.list(strsplit(det %||% "", ",")[[1]])
      }
      if (identical(gid, "post_cluster_exclusion")) {
        cfg$steps$post_cluster_exclusion$source <- "user"
        cfg$steps$post_cluster_exclusion$mode <- if (identical(val, "none")) "none" else "exclude"
        cfg$steps$post_cluster_exclusion$samples <- if (identical(val, "none")) list() else as.list(strsplit(det %||% "", ",")[[1]])
      }
      if (identical(gid, "lowconfidence")) {
        cfg$steps$lowconfidence$source <- "user"
        if (identical(val, "flag_only")) { cfg$steps$lowconfidence$mode <- "flag_only" }
        else { cfg$steps$lowconfidence$mode <- "threshold"; cfg$steps$lowconfidence$min_cells <- suppressWarnings(as.numeric(val)) }
      }
      if (identical(gid, "data_indir") && !identical(val, "keep")) {
        cfg$io$indir <- list(value = det, source = "user", why = "经入口确认")
      }
      if (identical(gid, "reference_source")) {
        cfg$io$reference_table$source <- "user"
        cfg$io$lit_dict$source <- "user"
        if (identical(val, "reference_table")) { cfg$io$reference_table$value <- det; cfg$io$lit_dict$value <- NULL }
        if (identical(val, "lit_dict")) { cfg$io$lit_dict$value <- det; cfg$io$reference_table$value <- NULL }
        if (identical(val, "skip")) { cfg$io$reference_table$value <- NULL; cfg$io$lit_dict$value <- NULL
                                      cfg$io$reference_table$why <- "本次跳过注释"; cfg$io$lit_dict$why <- "本次跳过注释" }
      }
      cfg
    }
    cfg <- sync_gate_into_cfg(cfg, gid, val, det)
    cfg$case$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    save_cfg(cfg)
  }

  sc_ledger_append(RUN_DIR, list(
    run_id = sc_run_id() %||% "", event = "answer", step = "answer",
    time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    gate = gid, value = val, detail = det %||% ""
  ))
  out("")
  out(Mf("cli_answer_done", gate = gid, value = val))
  out("")
  quit(save = "no", status = 0L)
}

# ---------------------------------------------------------------------
# ledger
# ---------------------------------------------------------------------
if (CMD == "ledger") {
  entries <- sc_ledger_read(RUN_DIR)
  out("")
  out(M("cli_ledger_head"))
  out(paste0("  ", ledger_path))
  out("")
  if (!length(entries)) { out(M("cli_ledger_empty")); out(""); quit(save = "no", status = 0L) }
  for (e in entries) {
    ev <- e$event %||% "?"
    if (identical(ev, "finish")) {
      secs <- e$secs %||% ""
      out(sprintf("  %s  %-10s  exit=%s  %ss  %s", e$time %||% "", e$step %||% "", e$code %||% "", secs, e$run_id %||% ""))
    } else if (identical(ev, "start")) {
      out(sprintf("  %s  %-10s  START     %s", e$time %||% "", e$step %||% "", e$run_id %||% ""))
    } else if (identical(ev, "blocked")) {
      out(sprintf("  %s  %-10s  BLOCKED   %s", e$time %||% "", e$step %||% "", e$reason %||% ""))
    } else {
      out(sprintf("  %s  %-10s  %s", e$time %||% "", e$step %||% "", toupper(ev)))
    }
  }
  out("")
  quit(save = "no", status = 0L)
}

# ---------------------------------------------------------------------
# verify -- re-check ledgered outputs
#   The point: a ledger that nobody ever re-reads is decoration. This
#   turns "we recorded it" into "we can show it still matches".
# ---------------------------------------------------------------------
if (CMD == "verify") {
  entries <- sc_ledger_read(RUN_DIR)
  fin <- Filter(function(e) identical(e$event, "finish") && identical(as.integer(e$code %||% 1L), 0L), entries)
  # 只按「每一步最后一次成功」比对。
  #   为什么不是比对全部历史记录：重跑一步会合法地改变产物，
  #   拿新旧两份指纹比对必然报「被改动」，那是噪音不是发现。
  #   真正要回答的是：现在磁盘上的产物，和台账里最后一次登记的，
  #   是不是同一个东西。
  latest <- list()
  for (e in fin) latest[[e$step %||% "?"]] <- e
  fin <- unname(latest)
  out("")
  out(M("cli_verify_head"))
  out("")
  if (!length(fin)) { out(M("cli_verify_none")); out(""); quit(save = "no", status = 0L) }
  n_ok <- 0L; n_miss <- 0L; n_chg <- 0L
  for (e in fin) {
    fps <- e$fingerprints
    if (is.null(fps) || !length(fps)) next
    for (fp in fps) {
      p <- file.path(RUN_DIR, fp$rel %||% "")
      if (!file.exists(p)) { out(Mf("cli_verify_missing", path = fp$rel %||% "?")); n_miss <- n_miss + 1L; next }
      now <- sc_digest(p)
      if (identical(fp$kind, "file")) {
        if (!identical(now$md5, fp$md5)) {
          out(Mf("cli_verify_bad", path = fp$rel, want = fp$md5 %||% "-", got = now$md5 %||% "-")); n_chg <- n_chg + 1L
        } else { out(Mf("cli_verify_ok", path = fp$rel)); n_ok <- n_ok + 1L }
      } else {
        if (!identical(now$n_files, fp$n_files)) {
          out(Mf("cli_verify_bad", path = fp$rel, want = paste0(fp$n_files, " files"),
                got = paste0(now$n_files, " files"))); n_chg <- n_chg + 1L
        } else { out(Mf("cli_verify_ok", path = fp$rel)); n_ok <- n_ok + 1L }
      }
    }
  }
  out("")
  out(Mf("cli_verify_summary", ok = n_ok, missing = n_miss, changed = n_chg))
  out("")
  quit(save = "no", status = if (n_miss + n_chg > 0L) 1L else 0L)
}

# ---------------------------------------------------------------------
# run / run-all
#   This is the only code path that may launch work. It:
#     sets the three guard env vars, checks gates, checks deps, records
#     a ledger start entry, executes, records fingerprints and exit code.
# ---------------------------------------------------------------------
run_one_step <- function(sid, force = FALSE) {
  # 动手之前先确认守卫本身没被改过。
  #   顺序很重要：如果先跑再查，被改过的守卫已经放行了。
  if (!verify_guard_intact()) return(FALSE)

  cfg <- load_cfg()
  if (is.null(cfg)) { out(Mf("cli_no_project", dir = RUN_DIR)); return(FALSE) }
  if (!sid %in% CHAIN_ORDER) { out(Mf("cli_unknown_step", step = sid)); return(FALSE) }

  st <- step_state_of(cfg)
  s <- st[[sid]]

  # 闸门：按这一步要求的段位检查
  #   上段要 A 段（数据在哪、注释依据、质控门槛、读入前剔样本）
  #   下段要 B 段（筛群、剔样本、低置信门槛）
  #   B 段那几条的依据只有跑到一次聚类才产生，所以不能在上段之前就要求答完 ——
  #   否则上段永远跑不起来，也就永远产生不了依据，卡死。
  if (step_needs_gate(sid)) {
    ph <- step_gate_phase(sid)
    pend <- pending_gate_names(RUN_DIR, ph) %||% character(0)
    if (length(pend)) {
      rid <- paste0("blocked-", format(Sys.time(), "%Y%m%d%H%M%S"))
      sc_ledger_append(RUN_DIR, list(run_id = rid, event = "blocked", step = sid,
                                     time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                                     reason = "unresolved gates", phase = ph,
                                     pending = as.list(pend)))
      out("")
      outl(Mf("cli_refused_blocked", n = length(pend),
              list = paste0("    - ", pend, collapse = "\n"), dir = RUN_DIR))
      out("")
      return(FALSE)
    }
  }

  # 依赖
  if (length(s$dep_block)) {
    d <- s$dep_block[1]
    out("")
    out(Mf("cli_refused_dep", step = sid, dep = d, state = state_label(st[[d]]$state), dir = RUN_DIR))
    out("")
    return(FALSE)
  }

  # 已有产物
  if (!force && identical(s$state, "done") && length(s$present)) {
    out("")
    out(Mf("cli_refused_existing", path = s$present[1]))
    out("")
    return(FALSE)
  }

  # 正在跑
  #
  #  这里有个必须小心的地方：状态文件里写着 running，不等于真的在跑。
  #  两种假运行要区分清楚：
  #    a) pid 是空的 —— 说明是「即将启动」的占位，不是已经在跑
  #    b) pid 有，但进程已经没了 —— 被强杀或崩了，来不及改状态
  #  两种情况都当成旧状态清掉、放行。
  #
  #  为什么必须这样：之前网页入口会先写一条 running/pid=null 再启动入口，
  #  入口读到它就认为「上一步还在跑」并拒绝 —— 等于自己把门锁上再敲门，
  #  现象是点按钮毫无反应，台账里什么都没有，极难查。
  pid_alive <- function(p) {
    if (is.null(p) || length(p) == 0L) return(FALSE)
    p <- suppressWarnings(as.integer(p))
    if (is.na(p)) return(FALSE)
    if (.Platform$OS.type == "windows") {
      o <- tryCatch(suppressWarnings(system2("tasklist",
             c("/FI", paste0("PID eq ", p), "/NH"), stdout = TRUE, stderr = FALSE)),
             error = function(e) character(0))
      return(any(grepl(as.character(p), o)))
    }
    file.exists(file.path("/proc", as.character(p)))
  }

  ss <- read_json_file(file.path(status_dir, "step_state.json"))
  if (!is.null(ss) && identical(ss$status, "running")) {
    if (pid_alive(ss$pid)) {
      out("")
      out(Mf("cli_refused_running", pid = ss$pid))
      out("")
      return(FALSE)
    }
    # 假运行：清掉，继续
    out(paste0("（上一条「运行中」是旧状态（pid=", ss$pid %||% "空",
               "，进程已不在），已清掉继续。）"))
    tryCatch(writeLines(jsonlite::toJSON(list(status = "stale", step = ss$step %||% sid,
                                              code = NULL, ended = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
                                        auto_unbox = TRUE),
                        file.path(status_dir, "step_state.json"), useBytes = TRUE),
             error = function(e) NULL)
  }

  rid <- paste0(sid, "-", format(Sys.time(), "%Y%m%d%H%M%S"))
  t0 <- Sys.time()

  # 查表：这一次用的是哪个环境、哪个需求、哪一套脚本。
  #   台账原先只记「这一步跑没跑成」，回答不了「这批结果用的是哪一套」。
  #   后者才是复现的关键 —— 同一件事在不同环境要用不同脚本，
  #   不记下用的是哪套，换台机器就没法解释为什么数不一样。
  pick <- pick_for_ledger(category = step_category(sid),
                          demand   = step_demand(sid),
                          purpose  = step_purpose(sid))

  sc_ledger_append(RUN_DIR, list(run_id = rid, event = "start", step = sid,
                                 time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                                 engine = ENGINE_DIR, run_dir = RUN_DIR,
                                 cmd = paste0("sc_workflow.R run --step ", sid),
                                 # ↓ 台账三字段
                                 env = pick$env, env_why = pick$env_why,
                                 memory_gb = pick$memory_gb,
                                 demand = pick$demand,
                                 set_id = pick$set_id, set_title = pick$set_title,
                                 set_status = pick$set_status,
                                 set_source = pick$set_source,
                                 set_usable = pick$set_usable,
                                 set_reason = pick$set_reason,
                                 set_requires_why = pick$requires_why))
  if (nzchar(pick$set_id) && !identical(pick$set_id, "unknown")) {
    out(paste0("  环境 ", pick$env, "｜需求 ", pick$demand, "｜取用 ", pick$set_id))
  }
  if (length(pick$requires_why)) {
    for (w in pick$requires_why) out(paste0("    · ", w))
  }

  out("")
  out(Mf("cli_starting", step = sid))
  out("")

  # 关键：只有这里设置三个环境变量。低层脚本读到它们才肯运行。
  chain <- file.path(ENGINE_DIR, "run_chain.R")
  env <- c(paste0("SC_WORKFLOW_RUN=", rid),
           paste0("SC_WORKFLOW_DIR=", RUN_DIR),
           paste0("SC_WORKFLOW_ENGINE=", ENGINE_DIR))
  rs <- rscript_bin(cfg)
  old <- Sys.getenv(c("SC_WORKFLOW_RUN", "SC_WORKFLOW_DIR", "SC_WORKFLOW_ENGINE"), names = TRUE)
  Sys.setenv(SC_WORKFLOW_RUN = rid, SC_WORKFLOW_DIR = RUN_DIR, SC_WORKFLOW_ENGINE = ENGINE_DIR)
  code <- tryCatch({
    system2(rs, c("--vanilla", chain, "--run-dir", RUN_DIR, "--engine-dir", ENGINE_DIR, "--only", sid),
            stdout = "", stderr = "")
  }, error = function(e) { out(paste0("启动失败：", conditionMessage(e))); 127L })

  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

  # 指纹
  fps <- list()
  for (rel in (STEP_OUTPUTS[[sid]] %||% character(0))) {
    p <- file.path(RUN_DIR, rel)
    if (!file.exists(p)) next
    dg <- sc_digest(p)
    fps[[length(fps) + 1L]] <- c(list(rel = rel), dg)
  }
  # 参数快照：这一跑用的是哪些值、来源是什么
  src_tab <- tryCatch(cfg_param_table(cfg), error = function(e) NULL)
  src_rows <- list()
  if (!is.null(src_tab) && nrow(src_tab)) {
    for (i in seq_len(nrow(src_tab))) {
      src_rows[[length(src_rows) + 1L]] <- list(scope = src_tab$scope[i], key = src_tab$key[i],
                                                value = src_tab$value[i], source = src_tab$source[i])
    }
  }

  sc_ledger_append(RUN_DIR, list(
    run_id = rid, event = "finish", step = sid,
    time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    code = as.integer(code), secs = secs,
    fingerprints = fps, params = src_rows
  ))

  if (identical(as.integer(code), 0L)) {
    out(Mf("cli_finished_ok", step = sid))
    # 顺便清掉「blocked」残留，让 status 反映最新
    TRUE
  } else {
    out(Mf("cli_finished_fail", step = sid, code = code))
    out(paste0("  详细日志：", file.path(RUN_DIR, "status", "run.log")))
    FALSE
  }
}

if (CMD %in% c("run", "run-all")) {
  if (CMD == "run") {
    sid <- get_opt("--step", NULL)
    if (is.null(sid)) { out("用法：run --dir <目录> --step <步骤>"); quit(save = "no", status = 2L) }
    ok <- run_one_step(sid, isTRUE(has_flag("--force")))
    out("")
    quit(save = "no", status = if (ok) 0L else 1L)
  } else {
    # run-all：跑完的步骤跳过，不重跑。
    #   为什么不重跑：主流程动辄几小时。已经在的步骤再算一遍，
    #   既浪费时间，也可能覆盖掉已经核对过的中间产物。
    #   想重跑就单点那一步（那才是明确的「重跑」意图）。
    all_ok <- TRUE
    cfg0 <- load_cfg()
    st0 <- if (is.null(cfg0)) list() else step_state_of(cfg0)
    for (sid in CHAIN_ORDER) {
      if (isTRUE(has_flag("--force")) == FALSE &&
          !is.null(st0[[sid]]) && identical(st0[[sid]]$state, "done")) {
        out("")
        out(paste0("跳过 ", sid, "：已经跑完了。"))
        next
      }
      ok <- run_one_step(sid, isTRUE(has_flag("--force")))
      if (!ok) { all_ok <- FALSE; break }
    }
    out("")
    if (all_ok) out("全流程完成。")
    else out("已停止（上一步未成功）。修好后可单独重跑那一步。")
    out("")
    quit(save = "no", status = if (all_ok) 0L else 1L)
  }
}

out("未知命令：", CMD)
outl(M("cli_usage"))
quit(save = "no", status = 2L)
