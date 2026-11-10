#!/usr/bin/env Rscript
# =====================================================================
#  notebook_build.R -- 把「实际跑过的东西」汇编成一个能重跑的 ipynb
#
#  为什么要在跑的时候记，而不是事后重写
#    事后凭记忆重写脚本，写出来的是「应该这么跑」，不是「实际这么跑」。
#    两者差一个参数就是两批结果。所以执行端每跑一步追加一行流水，
#    最后只做汇编，不改内容。
#
#  流水契约（唯一出处）：<run_dir>/status/cells.jsonl
#    每行一个 JSON 对象，字段固定：
#      seq      整数，顺序
#      level    1=一级标题 2=二级标题 3=三级标题
#      step     步骤编号（"02"）
#      title    "ProjectConfig｜项目、输入与资源路径"
#      note     可选，说明段落，写进 markdown
#      lang     "R" | "python" | "bash"
#      code     该段真实执行的代码正文
#      inputs   可选，字符数组
#      outputs  可选，字符数组
#      exit_code 可选，整数
#    没写 level 的行按代码 cell 处理（level=0）。
#
#  为什么用 jsonl 而不是 sqlite 表
#    这是执行端写的、按顺序追加的流水。jsonl 追加是一行 writeLines，
#    崩了最多丢最后一行；表要事务、要迁移、要跟执行端对齐 schema。
#    而且这份流水要能被人直接打开看。
#
#  这个文件不执行任何分析，只读流水、写 notebook、写校验报告。
# =====================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

NB_SCHEMA_VERSION <- "pa.notebook.v1"
NB_MAIN_TITLE_DEFAULT <- "单细胞分析流程"

# ---- 流水读取 -------------------------------------------------------

nb_cells_file <- function(run_dir) file.path(run_dir, "status", "cells.jsonl")

# 应用侧暂存位置。
#   为什么要暂存一份：run_dir 可能在服务器上，应用到不了那块磁盘；
#   而人是从应用里下载 notebook 的。所以助手把流水推给应用，
#   应用存在自己这里，汇编与下载都在本机完成。
# 应用侧暂存目录（按 session 分开）。
#   为什么要暂存一份：run_dir 可能在服务器上，应用到不了那块磁盘；
#   而人是从应用里下载 notebook 的。所以助手把流水推给应用，
#   应用存在自己这里，汇编与下载都在本机完成。
#   为什么必须按 sid 分目录：共用一个文件时，两条记录会往同一份流水里追加，
#   汇编出来的 notebook 会混进另一条分析的步骤 —— 看起来完整，实际是错的。
nb_store_dir <- function(app_dir, sid) {
  sid <- as.character(sid)
  if (length(sid) != 1L || is.na(sid) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", sid))
    stop("Invalid notebook session id", call. = FALSE)
  file.path(app_dir, "state", "notebooks", sid)
}

nb_validate_cell <- function(cell) {
  if (!is.list(cell) || is.null(names(cell)) || any(!nzchar(names(cell))) || anyDuplicated(names(cell)))
    stop("cell must be a JSON object", call. = FALSE)
  for (key in c("code", "lang", "title", "note", "step", "event_id", "received_at", "cell_type")) {
    value <- cell[[key]]
    if (!is.null(value) && (!is.character(value) || length(value) != 1L || is.na(value)))
      stop(paste(key, "must be a string"), call. = FALSE)
  }
  for (key in c("seq", "level", "exit_code")) {
    value <- cell[[key]]
    if (!is.null(value) && (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value != trunc(value)))
      stop(paste(key, "must be an integer"), call. = FALSE)
  }
  if (!is.null(cell$seq) && cell$seq < 1) stop("seq must be positive")
  if (!is.null(cell$level) && !cell$level %in% 0:6) stop("level must be between 0 and 6")
  if (!is.null(cell$lang) && !tolower(cell$lang) %in% c("r", "python", "bash")) stop("unsupported language")
  if (!is.null(cell$event_id) && !nzchar(trimws(cell$event_id))) stop("event_id must not be empty")
  for (key in c("inputs", "outputs")) if (!is.null(cell[[key]])) {
    values <- cell[[key]]
    if (!(is.character(values) || is.list(values)) ||
        any(!vapply(as.list(values), function(x) is.character(x) && length(x) == 1L && !is.na(x), TRUE)))
      stop(paste(key, "must be an array of strings"))
  }
  invisible(TRUE)
}

nb_read_cells <- function(run_dir) {
  f <- nb_cells_file(run_dir)
  if (!file.exists(f)) return(NULL)
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  out <- list()
  for (i in seq_along(lines)) {
    ln <- trimws(lines[i])
    if (!nzchar(ln)) next
    obj <- tryCatch({
      if (!startsWith(ln, "{")) stop("expected JSON object")
      x <- jsonlite::fromJSON(ln, simplifyVector = FALSE)
      nb_validate_cell(x); x
    }, error = function(e) stop(sprintf("Invalid cells.jsonl line %d: %s", i, conditionMessage(e)), call. = FALSE))
    out[[length(out) + 1L]] <- obj
  }
  ids <- vapply(out, function(x) x$event_id %||% "", "")
  if (anyDuplicated(ids[nzchar(ids)])) stop("Duplicate event_id in cells.jsonl")
  out
}

nb_with_lock <- function(run_dir, action, timeout = 5) {
  f <- nb_cells_file(run_dir)
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  lock <- paste0(f, ".lock")
  deadline <- Sys.time() + timeout
  repeat {
    if (dir.create(lock, showWarnings = FALSE)) break
    if (Sys.time() >= deadline) stop("Notebook ledger lock timeout", call. = FALSE)
    Sys.sleep(0.02)
  }
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  action()
}

nb_payload <- function(cell) {
  cell$seq <- NULL; cell$received_at <- NULL
  canonical <- function(x) {
    if (is.list(x)) {
      if (!is.null(names(x))) x <- x[order(names(x))]
      x <- lapply(x, canonical)
    }
    x
  }
  as.character(jsonlite::toJSON(canonical(cell), auto_unbox = TRUE, null = "null", digits = NA))
}

# Lock covers validation, idempotence and replacement; a bad ledger is never repaired by overwrite.
nb_append_cell <- function(run_dir, cell, event_id = NULL) {
  tryCatch({
    if (!is.null(event_id)) {
      if (!is.null(cell$event_id) && !identical(cell$event_id, event_id)) stop("Conflicting event_id")
      cell$event_id <- event_id
    }
    nb_validate_cell(cell)
    nb_with_lock(run_dir, function() {
      f <- nb_cells_file(run_dir)
      existing <- nb_read_cells(run_dir) %||% list()
      if (!is.null(cell$event_id)) {
        hit <- Filter(function(x) identical(x$event_id, cell$event_id), existing)
        if (length(hit)) {
          if (!identical(nb_payload(hit[[1]]), nb_payload(cell))) stop("event_id payload conflict")
          return(TRUE)
        }
      }
      cell$seq <- length(existing) + 1L
      cell$received_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      line <- as.character(jsonlite::toJSON(cell, auto_unbox = TRUE, null = "null", digits = NA))
      old <- if (file.exists(f)) readLines(f, warn = FALSE, encoding = "UTF-8") else character()
      tmp <- tempfile(".cells-", tmpdir = dirname(f))
      on.exit(unlink(tmp), add = TRUE)
      writeLines(enc2utf8(c(old, line)), tmp, useBytes = TRUE)
      if (!file.rename(tmp, f)) stop("Atomic ledger replacement failed")
      # Any new cell invalidates a previously built snapshot.
      unlink(file.path(run_dir, "reproducible.ipynb"))
      TRUE
    })
  }, error = function(e) FALSE)
}

# ---- cell 组装 ------------------------------------------------------

nb_code_cell <- function(code, lang, kernel = "python3") {
  src <- nb_split_lines(as.character(code %||% ""))
  # R 内核直接运行 R；Python 内核用已加载的 rpy2 extension 运行 R。
  if (identical(toupper(as.character(lang %||% "python")), "R") &&
      !identical(kernel, "ir"))
    src <- nb_split_lines(paste0("%%R\n", paste(src, collapse = "")))
  if (identical(tolower(as.character(lang %||% "")), "bash"))
    src <- nb_split_lines(paste0("%%bash\n", paste(src, collapse = "")))
  list(cell_type = "code", execution_count = NULL, metadata = structure(list(), names = character()),
       outputs = list(), source = as.list(src))
}

# nbformat 规定 source 是"行数组"，除最后一行外每行以 \n 结尾。
#   为什么必须自己拼：strsplit() 会把分隔符吃掉，直接用它的结果写进
#   notebook，各段文字会在渲染时粘成一行（实测："## 运行前提- 内核：python3"）。
#   这种错在 JSON 里看不出来，只有打开 notebook 才看得见。
nb_split_lines <- function(text) {
  s <- as.character(text %||% "")
  if (!nzchar(s)) return(character(0))
  # 用哨兵保留 strsplit 通常会丢掉的末尾换行和空行。
  parts <- strsplit(paste0(s, "\001"), "\n", fixed = TRUE)[[1]]
  n <- length(parts)
  parts[n] <- sub("\001$", "", parts[n])
  if (n > 1L) parts[-n] <- paste0(parts[-n], "\n")
  parts[nzchar(parts)]
}

nb_md_cell <- function(text) {
  list(cell_type = "markdown", metadata = structure(list(), names = character()),
       source = as.list(nb_split_lines(as.character(text %||% ""))))
}

# 二级标题的标准形状：编号 + 「名称｜一句话用途」。
#   参考文件就是这么写的（## 01. ImportPackages｜通用初始化），
#   保持一致人才能把两份 notebook 并排对。
nb_heading_text <- function(level, step, title) {
  hashes <- strrep("#", as.integer(level))
  t <- as.character(title %||% "")
  if (nzchar(step %||% "")) paste0(hashes, " ", step, ". ", t) else paste0(hashes, " ", t)
}

# ---- 依赖说明与路径检查 --------------------------------------------

# 首 cell 必须说清运行前提。说不清就等于把「跑不通」留给用户去猜。
nb_dependency_cell <- function(langs, kernel) {
  has_r <- any(toupper(langs) == "R")
  lines <- c("## 运行前提", "",
             paste0("- 内核：", kernel), "")
  if (has_r && !identical(kernel, "ir")) {
    lines <- c(lines,
      "- 本 notebook 使用 Python 内核，R 段落用 `%%R` 魔法执行。",
      "- 需安装 R 和 Python 包 rpy2：`pip install rpy2`；启动 cell 会加载 `%load_ext rpy2.ipython`。",
      "- 没有 rpy2 时 R 段落会直接失败，不会静默跳过；导出代码尚未经过全流程复现验证。", "")
  } else if (has_r) {
    lines <- c(lines, "- 全部为 R，需要 IRkernel：`install.packages('IRkernel')` 后 `IRkernel::installspec()`。", "")
  } else {
    lines <- c(lines, "- 全部为 Python，装齐 requirements 即可。", "")
  }
  lines <- c(lines,
    "- 路径变量只在第 2 节（ProjectConfig）修改；其余段落全部引用变量。",
    "- 数据与中间产物不在 notebook 里，需先按第 2 节填好路径。", "")
  nb_md_cell(paste(lines, collapse = "\n"))
}

# 硬编码绝对路径的检查。
#   「可复现」的第一个敌人是路径散落：改一处漏一处，跑起来用的是旧数据。
#   判据：除 ProjectConfig 那一节之外，任何 code cell 出现 "/share/" 或
#   盘符开头的字面路径，都记一条问题。
nb_check_paths <- function(cells) {
  issues <- list()
  for (c in cells) {
    if (!is.null(c$cell_type) && !identical(c$cell_type, "code")) next
    if (!nzchar(trimws(c$code %||% ""))) next
    if (identical(as.character(c$step %||% ""), "02")) next
    code <- as.character(c$code %||% "")
    hits <- regmatches(code, gregexpr('"(/[A-Za-z0-9_.-]+){2,}|[A-Za-z]:[/\\\\]',
                                      code, perl = TRUE))[[1]]
    if (length(hits)) {
      issues[[length(issues) + 1L]] <- list(
        step = as.character(c$step %||% ""),
        title = as.character(c$title %||% ""),
        sample = unique(hits)[1:min(3, length(unique(hits)))])
    }
  }
  issues
}

# ---- 汇编 -----------------------------------------------------------

# 返回 list(notebook, issues, stats)
nb_build <- function(run_dir, main_title = NULL, kernel = NULL) {
  raw <- tryCatch(nb_read_cells(run_dir), error = function(e) e)
  if (inherits(raw, "error"))
    return(list(ok = FALSE, code = "invalid_cells_file", error = conditionMessage(raw)))
  if (is.null(raw))
    return(list(ok = FALSE, code = "no_cells_file",
                error = paste0("没有 ", nb_cells_file(run_dir),
                               "：这一轮不是经带流水的入口跑的，补不出来")))

  executable <- Filter(function(c) nzchar(trimws(c$code %||% "")), raw)
  if (!length(executable))
    return(list(ok = FALSE, code = "empty_cells", error = "流水没有可执行代码"))
  langs <- vapply(executable, function(c) tolower(c$lang %||% "python"), "")
  langs[langs == "r"] <- "R"
  if (any(!langs %in% c("R", "python", "bash")))
    return(list(ok = FALSE, code = "unsupported_language", error = "流水含不支持的代码语言"))
  if (is.null(kernel) || !nzchar(kernel)) {
    kernel <- if (all(langs == "R")) "ir" else "python3"
  }
  if (identical(kernel, "ir") && any(langs != "R"))
    return(list(ok = FALSE, code = "incompatible_kernel", error = "R 内核不能执行 Python/bash 流水"))

  title <- main_title %||% NB_MAIN_TITLE_DEFAULT
  cells <- list()
  cells[[length(cells) + 1L]] <- nb_md_cell(paste0("# ", title))
  cells[[length(cells) + 1L]] <- nb_dependency_cell(langs, kernel)

  if (any(langs == "R") && !identical(kernel, "ir"))
    cells[[length(cells) + 1L]] <- nb_code_cell("%load_ext rpy2.ipython\n", "python", kernel)

  for (c in raw) {
    lvl <- suppressWarnings(as.integer(c$level %||% 0L))
    code <- as.character(c$code %||% "")
    has_code <- nzchar(trimws(code))
    # 一条流水可以同时带标题和代码。
    #   为什么要允许：一个「小节」天然是「标题 + 这段代码」。
    #   强制拆成两条，执行端漏一条就会得到一个只有标题没有代码的 notebook ——
    #   而那种文件打开看着完整，跑起来什么都没有。
    #   所以这里两者都给：有 level 出 markdown，有 code 再出 code。
    if (!is.na(lvl) && lvl >= 1L) {
      txt <- nb_heading_text(lvl, as.character(c$step %||% ""),
                             as.character(c$title %||% ""))
      note <- as.character(c$note %||% "")
      body <- if (nzchar(note)) paste0(txt, "\n\n", note) else txt
      cells[[length(cells) + 1L]] <- nb_md_cell(body)
    } else if (nzchar(as.character(c$title %||% ""))) {
      # 没有 level 但给了 title：当成二级标题，不丢标题信息。
      cells[[length(cells) + 1L]] <- nb_md_cell(
        nb_heading_text(2L, as.character(c$step %||% ""), as.character(c$title %||% "")))
    }
    if (has_code)
      cells[[length(cells) + 1L]] <- nb_code_cell(code, c$lang %||% "python", kernel)
  }

  for (i in seq_along(cells)) cells[[i]]$id <- paste0("pa-cell-", i)

  issues <- nb_check_paths(raw)
  nb <- list(
    cells = cells,
    metadata = list(
      kernelspec = list(display_name = if (identical(kernel, "ir")) "R" else "Python 3",
                        language = if (identical(kernel, "ir")) "R" else "python",
                        name = kernel),
      language_info = list(name = if (identical(kernel, "ir")) "R" else "python"),
      pa = list(schema = NB_SCHEMA_VERSION,
                built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                 full_analysis_replay_verified = FALSE,
                cells_source = basename(nb_cells_file(run_dir)),
                code_cells = sum(vapply(cells, function(x) identical(x$cell_type, "code"), TRUE)),
                md_cells = sum(vapply(cells, function(x) identical(x$cell_type, "markdown"), TRUE)),
                langs = as.list(unique(langs)))),
    nbformat = 4L,
    nbformat_minor = 5L)

  list(ok = TRUE, notebook = nb, issues = issues,
       stats = list(code_cells = nb$metadata$pa$code_cells,
                    md_cells = nb$metadata$pa$md_cells,
                    langs = unique(langs)))
}

# 写文件。返回路径与大小，供调用方核对真的落盘了。
nb_write <- function(nb, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  txt <- as.character(jsonlite::toJSON(nb, auto_unbox = TRUE, null = "null",
                                       pretty = TRUE, digits = NA))
  tmp <- tempfile(".notebook-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  con <- file(tmp, open = "wb")
  writeBin(charToRaw(enc2utf8(txt)), con)
  close(con)
  if (!file.rename(tmp, path)) stop("Atomic notebook replacement failed")
  list(path = path, size = file.info(path)$size)
}

# 可选 R 片段导出：仅供检查，不是完整 notebook 的替代品。
#   只抽 R 段落，按顺序拼成一个 .R 文件，段落之间用标题注释隔开。
nb_write_r_script <- function(nb, path) {
  cells <- nb$cells %||% list()
  lines <- c("# 由 notebook 自动生成：只含 R 段落，不含 %%R 魔法",
             "# 与 notebook 同源，逐段对应",
             "")
  for (c in cells) {
    if (identical(c$cell_type, "markdown")) {
      txt <- paste(unlist(c$source), collapse = "")
      if (nzchar(txt)) lines <- c(lines, paste0("# ", sub("^#+\\s*", "", txt)), "")
      next
    }
    src <- unlist(c$source)
    if (length(src) && (identical(trimws(src[1]), "%%R") ||
                        identical(nb$metadata$kernelspec$name, "ir"))) {
      if (identical(trimws(src[1]), "%%R")) src <- src[-1]
      lines <- c(lines, paste(src, collapse = ""), "")
    }
  }
  con <- file(path, open = "wb")
  writeBin(charToRaw(enc2utf8(paste(lines, collapse = "\n"))), con)
  close(con)
  list(path = path, size = file.info(path)$size)
}

nb_build_export <- function(store, title = NULL) {
  nb_with_lock(store, function() {
    out <- nb_build(store, main_title = title)
    if (!isTRUE(out$ok)) return(out)
    raw <- nb_read_cells(store)
    executable <- Filter(function(x) nzchar(trimws(x$code %||% "")), raw)
    failed <- any(vapply(raw, function(x) !is.null(x$exit_code) && x$exit_code != 0L, TRUE))
    missing <- any(vapply(raw, function(x) !is.null(x$exit_code) && !nzchar(trimws(x$code %||% "")), TRUE))
    evidenced <- all(vapply(executable, function(x) identical(as.integer(x$exit_code), 0L), TRUE))
    capture <- list(capture_complete = FALSE,
      submitted_execution_succeeded = !failed && !missing && evidenced,
      full_analysis_replay_verified = FALSE,
      note = "Only submitted execution evidence is checked; full workflow completeness and replay are not verified.")
    selection <- nb_read_json(file.path(store, "status", "local_selection.json"))
    if (!is.null(selection)) {
      capture$submitted_execution_succeeded <- isTRUE(capture$submitted_execution_succeeded) &&
        isTRUE(selection$submitted_execution_succeeded)
      capture$lineage <- selection
    }
    out$notebook$metadata$pa$capture <- capture
    w <- nb_write(out$notebook, file.path(store, "reproducible.ipynb"))
    list(ok = TRUE, path = w$path, size = w$size, stats = out$stats, issues = out$issues,
      capture_complete = capture$capture_complete,
      submitted_execution_succeeded = capture$submitted_execution_succeeded, full_analysis_replay_verified = FALSE,
      note = paste(if (length(out$issues)) "发现硬编码路径。" else "静态路径检查未发现匹配项。",
        "路径检查不是可复现验证；全流程复现未验证。"))
  })
}

# Persistent identities, never file timestamps, define local capture provenance.
nb_read_json <- function(path) {
  if (!file.exists(path)) return(NULL)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}
nb_write_json <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(".manifest-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  writeLines(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)), tmp, useBytes = TRUE)
  if (!file.rename(tmp, path)) stop("Cannot replace notebook manifest")
}
nb_manifest_file <- function(rd) file.path(rd, "status", "notebook_manifest.json")
nb_attempt_dir <- function(rd, id) {
  if (!is.character(id) || length(id) != 1L || !grepl("^[A-Za-z0-9_-]+$", id)) stop("Invalid attempt id")
  file.path(rd, "status", "notebook_attempts", id)
}
nb_identity <- function() paste0("a-", Sys.getpid(), "-", basename(tempfile()))

nb_bind_local <- function(run_dir, app_dir, sid) {
  nb_store_dir(app_dir, sid)
  nb_with_lock(run_dir, function() {
    f <- file.path(run_dir, "status", "notebook_session.json")
    expected <- list(session_id = sid, app_dir = normalizePath(app_dir, mustWork = TRUE),
      run_dir = normalizePath(run_dir, mustWork = TRUE))
    old <- nb_read_json(f)
    if (!is.null(old)) {
      if (!identical(old[c("session_id", "app_dir", "run_dir")], expected)) stop("Local notebook directory belongs to another session")
      # Upgrade old bindings without admitting their unscoped history.
      if (!is.null(old$binding_id)) return(TRUE)
    }
    expected$binding_id <- nb_identity()
    nb_write_json(expected, f)
    TRUE
  })
}

nb_sync_local <- function(app_dir, session) {
  if (!identical(session$mode %||% "local", "local")) return(FALSE)
  rd <- session$run_dir %||% ""
  if (!nzchar(rd) || !dir.exists(rd)) return(FALSE)
  f <- file.path(rd, "status", "notebook_session.json")
  if (!file.exists(f)) return(FALSE)
  store <- nb_store_dir(app_dir, session$id)
  nb_with_lock(rd, function() {
    binding <- nb_read_json(f)
    if (!identical(binding$session_id, session$id) ||
        !identical(binding$app_dir, normalizePath(app_dir, mustWork = TRUE)) ||
        !identical(binding$run_dir, normalizePath(rd, mustWork = TRUE))) stop("Notebook session binding mismatch")
    manifest <- nb_read_json(nb_manifest_file(rd))
    scoped <- !is.null(binding$binding_id) && !is.null(manifest) && identical(manifest$binding_id, binding$binding_id)
    phases <- if (scoped) manifest$phases %||% list() else list()
    all <- list(); contexts <- character()
    for (phase in phases) {
      if (identical(phase$state, "stale")) next
      attempt <- nb_attempt_dir(rd, phase$attempt_id)
      identity <- nb_read_json(file.path(attempt, "status", "attempt.json"))
      if (is.null(identity) || !identical(identity$attempt_id, phase$attempt_id) ||
          !identical(identity$binding_id, binding$binding_id))
        stop("Missing or mismatched notebook attempt manifest")
      raw <- nb_read_cells(attempt)
      if (is.null(raw)) stop("Missing notebook attempt ledger")
      for (cell in raw) {
        if (!identical(cell$attempt_id, phase$attempt_id)) stop("Notebook cell attempt mismatch")
        context <- identical(cell$step, "00")
        if ((context && !phase$attempt_id %in% contexts) || (!context && identical(cell$step, phase$step))) {
          cell$seq <- length(all) + 1L
          if (!is.null(cell$event_id)) cell$event_id <- paste0(phase$attempt_id, ":", cell$event_id)
          all[[length(all) + 1L]] <- cell
        }
      }
      contexts <- unique(c(contexts, phase$attempt_id))
    }
    selection <- list(binding_id = binding$binding_id, phases = phases,
      submitted_execution_succeeded = length(phases) > 0L &&
        all(vapply(phases, function(x) identical(x$state, "succeeded"), TRUE)),
      note = "Latest phase attempts only. Earlier-phase retries invalidate all later phases conservatively.")
    nb_with_lock(store, function() {
      dst <- nb_cells_file(store)
      tmp <- tempfile(".sync-", tmpdir = dirname(dst)); on.exit(unlink(tmp), add = TRUE)
      lines <- vapply(all, function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)), "")
      writeLines(lines, tmp, useBytes = TRUE)
      # Remove cached success even when the new attempt contains no executable code.
      unlink(file.path(store, "reproducible.ipynb"))
      if (!file.rename(tmp, dst)) stop("Cannot synchronize notebook ledger")
      nb_write_json(selection, file.path(store, "status", "local_selection.json"))
    })
    result <- nb_build_export(store, session$name %||% NULL)
    if (!isTRUE(result$ok) && !identical(result$code, "empty_cells")) stop(result$error)
    isTRUE(result$ok)
  })
}
