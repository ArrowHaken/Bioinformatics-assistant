#!/usr/bin/env Rscript
# =====================================================================
#  guard.R -- entry guard + execution ledger
#
#  WHY THIS EXISTS
#    A rule written in documentation cannot stop anyone from running the
#    analysis scripts directly. So the rule lives in code instead: every
#    low-level script first asks "who is calling me?", and stops if the
#    answer is not the workflow entry point.
#
#  THE THREE ENV VARS  (only sc_workflow.R sets them)
#    SC_WORKFLOW_RUN     id of this run
#    SC_WORKFLOW_DIR     run directory
#    SC_WORKFLOW_ENGINE  engine directory
#
#  Running `Rscript 01_preprocess.R` by hand has none of them -> refused.
#  Bypassing means editing this file, which shows up as a broken ledger
#  chain plus a changed file hash.
#
#  ASCII-only source on purpose: R 4.1.2 on a Chinese Windows locale
#  cannot parse non-ASCII source, and a garbled refusal message is worse
#  than no refusal at all. Every Chinese string is read from
#  workflow/messages.json.
# =====================================================================

SC_GUARD_VERSION <- "1.0"

# ---------------------------------------------------------------------
# 文案
# ---------------------------------------------------------------------
sc_msg <- function(guard_dir, key) {
  f <- file.path(guard_dir, "messages.json")
  if (!file.exists(f)) return(NULL)
  j <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(j) || is.null(j[[key]])) return(NULL)
  v <- j[[key]]
  out <- if (is.list(v)) vapply(v, function(x) as.character(x)[1], character(1)) else as.character(v)
  enc2native(out)
}

# 占位符替换：{name} -> value
sc_fill <- function(txt, ...) {
  a <- list(...)
  if (!length(a)) return(txt)
  for (nm in names(a)) txt <- gsub(paste0("{", nm, "}"), as.character(a[[nm]]), txt, fixed = TRUE)
  txt
}

# ---------------------------------------------------------------------
# 拒绝
# ---------------------------------------------------------------------
sc_refuse <- function(guard_dir, why) {
  L <- sc_msg(guard_dir, "guard_refuse")
  if (is.null(L)) {
    L <- c("REFUSED: this script must be invoked through the workflow entry point.",
           "reason: {why}",
           "",
           "use: Rscript engine/sc_workflow.R status --dir <run-dir>")
  }
  L <- sc_fill(L, why = why)
  stop(paste0("\n", paste0("  ", L, collapse = "\n"), "\n"), call. = FALSE)
}

# ---------------------------------------------------------------------
# 环境
# ---------------------------------------------------------------------
sc_run_id <- function() Sys.getenv("SC_WORKFLOW_RUN", "")
sc_run_dir <- function() Sys.getenv("SC_WORKFLOW_DIR", "")
sc_engine_dir <- function() Sys.getenv("SC_WORKFLOW_ENGINE", "")

sc_status_dir <- function(run_dir) file.path(run_dir, "status")
sc_ledger_path <- function(run_dir) file.path(sc_status_dir(run_dir), "ledger.jsonl")

sc_ledger_has <- function(led, run_id) {
  if (!file.exists(led)) return(FALSE)
  ln <- tryCatch(readLines(led, warn = FALSE), error = function(e) character(0))
  any(grepl(run_id, ln, fixed = TRUE))
}

# ---------------------------------------------------------------------
# 台账：只追加，不覆盖。
#   为什么只追加：事后要能看出「什么时候跑的、谁跑的、结果如何」。
#   能改的审计记录等于没有审计记录。
# ---------------------------------------------------------------------
sc_ledger_append <- function(run_dir, entry) {
  led <- sc_ledger_path(run_dir)
  dir.create(dirname(led), showWarnings = FALSE, recursive = TRUE)
  line <- jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null", digits = NA)
  con <- file(led, open = "ab")
  writeBin(charToRaw(paste0(enc2utf8(line), "\n")), con)
  close(con)
  invisible(TRUE)
}

sc_ledger_read <- function(run_dir) {
  led <- sc_ledger_path(run_dir)
  if (!file.exists(led)) return(list())
  ln <- tryCatch(readLines(led, warn = FALSE), error = function(e) character(0))
  out <- list()
  for (l in ln) {
    if (!nzchar(trimws(l))) next
    e <- tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(e)) out[[length(out) + 1L]] <- e
  }
  out
}

# 台账里每一步的最后状态（后写的覆盖先写的）
sc_ledger_state <- function(run_dir) {
  st <- list()
  for (e in sc_ledger_read(run_dir)) {
    sid <- e$step %||% ""
    if (!nzchar(sid)) next
    if (identical(e$event, "finish") || identical(e$event, "start") || identical(e$event, "blocked")) {
      st[[sid]] <- e
    }
  }
  st
}

# ---------------------------------------------------------------------
# 指纹
#   用 R 自带的 md5（tools::md5sum），不引外部依赖。
#   这不是密码学强度，用途是「事后发现文件有没有被改过」，够用且诚实。
# ---------------------------------------------------------------------
sc_digest <- function(path) {
  if (!file.exists(path)) return(NULL)
  if (dir.exists(path)) {
    fs <- list.files(path, recursive = TRUE, full.names = TRUE)
    fs <- fs[!dir.exists(fs)]
    return(list(kind = "dir", n_files = length(fs),
                size = sum(file.info(fs)$size, na.rm = TRUE)))
  }
  md5 <- tryCatch(unname(tools::md5sum(path)), error = function(e) NA_character_)
  list(kind = "file", size = unname(file.info(path)$size), md5 = md5)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---------------------------------------------------------------------
# 守卫自身指纹
#
#  防的是「把检查删掉」这种绕过方式 —— 包括我自己去删。
#  init 时把 guard.R 的 md5 写进 guard.lock.json；之后每次运行都对照。
#  改了这个文件，所有运行立刻停下，不会悄悄继续。
#  这不是密码学强度，只是让「改过」这件事藏不住。
# ---------------------------------------------------------------------
sc_guard_file <- function(guard_dir) file.path(guard_dir, "guard.R")

sc_guard_self_md5 <- function(guard_dir) {
  f <- sc_guard_file(guard_dir)
  if (!file.exists(f)) return(NA_character_)
  tryCatch(unname(tools::md5sum(f)), error = function(e) NA_character_)
}

sc_guard_lock_path <- function(run_dir) file.path(run_dir, "status", "guard.lock.json")

# 判定规则的指纹范围（K2）
#
#  为什么要扩大范围：guard.R 原来只管自己一个文件。
#  可是「为了让判定通过而改判定」比 agent 谎报更危险 ——
#  改一行 verdict.R 里的判断，所有步骤都能过，而且台账看起来完全正常。
#  所以锁的范围必须包含判定规则本身：
#    · guard.R            守卫本体
#    · verdict.R          四层判定器
#    · flow_spec.R        每环的 review 段（products / required_columns / invariants）
#    · audit/whitelist.json  脚本白名单
#  任一项变化 -> 拒绝运行，提示「判定规则被改过，重新 init 并说明原因」。
#
#  为什么不用整文件 md5 锁 flow_spec.R：它同时承载流程定义与命令模板，
#  日常会改。只锁 review 段，才能区分「改了流程」和「改了判据」。
sc_guard_engine_dir <- function(guard_dir) {
  # guard_dir 是 engine/workflow；宿主是 engine/
  normalizePath(file.path(guard_dir, ".."), winslash = "/", mustWork = FALSE)
}

sc_review_section_digest <- function(engine_dir) {
  f <- file.path(engine_dir, "flow_spec.R")
  if (!file.exists(f)) return(NA_character_)
  txt <- tryCatch(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                  error = function(e) "")
  # 从 FLOW_REVIEW 开始到文件末尾：review 声明整段。
  i <- regexpr("FLOW_REVIEW <- list(", txt, fixed = TRUE)
  seg <- if (i > 0) substr(txt, i, nchar(txt)) else ""
  if (!nzchar(seg)) return(NA_character_)
  tf <- tempfile()
  writeLines(seg, tf, useBytes = TRUE)
  on.exit(unlink(tf), add = TRUE)
  unname(tools::md5sum(tf))
}

sc_rule_digests <- function(guard_dir) {
  eng <- sc_guard_engine_dir(guard_dir)
  list(
    guard_md5 = sc_guard_self_md5(guard_dir),
    verdict_md5 = {
      f <- file.path(eng, "verdict.R")
      if (file.exists(f)) unname(tools::md5sum(f)) else NA_character_
    },
    flow_review_md5 = sc_review_section_digest(eng),
    whitelist_md5 = {
      f <- file.path(eng, "audit", "whitelist.json")
      if (file.exists(f)) unname(tools::md5sum(f)) else NA_character_
    }
  )
}

sc_write_guard_lock <- function(run_dir, guard_dir) {
  f <- sc_guard_lock_path(run_dir)
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  dg <- sc_rule_digests(guard_dir)
  obj <- list(
    guard_version = SC_GUARD_VERSION,
    guard_md5 = dg$guard_md5,
    rules = dg,
    locked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    note = paste0("entry-guard + judgment-rule fingerprint; if guard.R / verdict.R / ",
                  "flow_spec review section / whitelist change, runs refuse until init is re-run")
  )
  con <- file(f, open = "wb")
  writeBin(charToRaw(enc2utf8(jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE))), con)
  close(con)
  invisible(obj)
}

sc_read_guard_lock <- function(run_dir) {
  f <- sc_guard_lock_path(run_dir)
  if (!file.exists(f)) return(NULL)
  tryCatch({
    txt <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    jsonlite::fromJSON(txt, simplifyVector = FALSE)
  }, error = function(e) NULL)
}

# ---------------------------------------------------------------------
# 守卫本体
#
#  四道检查，缺一不可：
#    1. 三个环境变量都在 —— 只有入口会设
#    2. 这个 run_id 在台账里 —— 入口登记过
#    3. guard.lock.json 在，且守卫文件没被改过
#    4. 引擎目录可用
# ---------------------------------------------------------------------
sc_guard <- function(step_id, guard_dir) {
  rid  <- sc_run_id()
  rdir <- sc_run_dir()
  eng  <- sc_engine_dir()

  if (!nzchar(rid))
    sc_refuse(guard_dir, "SC_WORKFLOW_RUN is empty -- not launched by the entry point")
  if (!nzchar(rdir) || !dir.exists(rdir))
    sc_refuse(guard_dir, "SC_WORKFLOW_DIR is missing or not a directory")
  if (!nzchar(eng) || !dir.exists(eng))
    sc_refuse(guard_dir, "SC_WORKFLOW_ENGINE is missing or not a directory")

  led <- sc_ledger_path(rdir)
  if (!file.exists(led))
    sc_refuse(guard_dir, "ledger file missing -- this run was never registered")
  if (!sc_ledger_has(led, rid))
    sc_refuse(guard_dir, paste0("run id not found in ledger: ", rid))

  lock <- sc_read_guard_lock(rdir)
  if (is.null(lock))
    sc_refuse(guard_dir, "guard.lock.json missing -- run 'sc_workflow.R init' to register this project")
  # ---- 比对范围：守卫本体 + 判定规则（K2）--------------------------
  #   为什么必须比后面三项：只比 guard.R 自己，等于允许「改判定器让所有步骤通过」。
  #   这类绕过比 agent 谎报更危险 —— 它不需要撒谎，改一行判断就行，
  #   而且台账看起来完全正常。
  #   实测过旧版本的问题：写了 4 个 md5 进 lock$rules，但比对那一步只比 guard_md5，
  #   于是篡改 verdict.R 之后 sc_guard() 仍然放行。指纹算了不比，
  #   比不做更糟 —— 它会让人以为改了判定器会停。
  now <- sc_rule_digests(guard_dir)
  if (!identical(now$guard_md5, lock$guard_md5))
    sc_refuse(guard_dir, paste0("guard.R has been modified (locked=", lock$guard_md5,
                                " now=", now$guard_md5, ") -- re-run init and say why"))

  locked_rules <- lock$rules %||% list()
  # 老锁文件没有 rules 段：不能当成"一致"，也不能直接拒。
  #   判 unknown 更危险（等于悄悄放行），所以按"规则没锁过"拒绝运行，
  #   并明确给出重跑 init 的方法。这一条会一次性影响所有老项目，
  #   但比"以为锁了其实没锁"更安全。
  if (is.null(locked_rules$verdict_md5))
    sc_refuse(guard_dir, paste0("judgment rules were never fingerprinted in this lock ",
      "-- re-run 'sc_workflow.R init' to register verdict.R / flow_spec review / whitelist"))

  changed <- character(0)
  for (k in c("verdict_md5", "flow_review_md5", "whitelist_md5")) {
    a <- as.character(locked_rules[[k]] %||% NA)
    b <- as.character(now[[k]] %||% NA)
    if (!identical(a, b)) changed <- c(changed, k)
  }
  if (length(changed))
    sc_refuse(guard_dir, paste0("judgment rules have been modified (",
      paste(changed, collapse = ", "), ") -- re-run init and say why"))

  invisible(TRUE)
}

# ---------------------------------------------------------------------
# 底层分析脚本的接入点
#
#  每个脚本开头两行：
#      source(file.path(.this_dir, "lib", "guard_load.R"))   # 定位并载入守卫
#      sc_guard_banner("pipeline", .this_dir)                # 校验并打印
#
#  guard_dir 从 script_dir 推出来，脚本不用自己拼路径。
# ---------------------------------------------------------------------
sc_guard_banner <- function(step_id, script_dir) {
  guard_dir <- normalizePath(file.path(script_dir, "..", "..", "workflow"),
                             winslash = "/", mustWork = FALSE)
  if (!file.exists(file.path(guard_dir, "guard.R"))) {
    stop("\n  REFUSED: cannot locate workflow/guard.R from ", script_dir, "\n", call. = FALSE)
  }
  sc_guard(step_id, guard_dir)
  m <- sc_msg(guard_dir, "guard_ok")
  if (!is.null(m)) cat(sc_fill(m, run_id = sc_run_id(), step = step_id), "\n", sep = "")
  invisible(TRUE)
}
