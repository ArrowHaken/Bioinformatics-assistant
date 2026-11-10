#!/usr/bin/env Rscript
# =====================================================================
# config.R — 配置层：所有参数的唯一来源
#
# 为什么要有这一层
#   原来的脚本把值直接写在代码里（删哪些簇、Mock/Microplastics 什么颜色、
#   质控 300）。换一批数据这些值就全错，而且错得静默——不报错，只是结果不对。
#   本层把「值」和「代码」分开：代码只读值，值全在 pipeline.json 里，
#   并且每个值都必须标清来源。
#
# 四个来源（source），含义不同，不许混用
#   official  官方口径。照抄对方脚本里的值。换数据通常仍适用。
#   instance  本机实例适配。我方为在这台机器上跑通做的改动（如内存相关）。
#   user      用户显式指定。优先级最高。
#   gate      未定，需要人工确认。读到 gate 且没答案 → 停下要答案，不许兜底。
#
# 优先级：user > instance > official > gate（gate 无值时必须停）
#
# 用法
#   source("lib/config.R")
#   cfg <- cfg_load("path/to/pipeline.json")     # 读配置
#   x   <- cfg_get(cfg, "params", "qc_min_features")   # 取值
#   cfg_note(cfg, "params", "qc_min_features")         # 取来源说明（写进报告）
#
# 只读 JSON，不改配置。改配置由工作台或用户完成。
#
# ASCII-only 源码（R 4.1.2 + 中文 Windows locale 不能解析非 ASCII 源码）。
# =====================================================================

suppressMessages({
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("缺少 jsonlite 包，无法读取配置。安装：install.packages('jsonlite')", call. = FALSE)
  }
  library(jsonlite)
})

CFG_SOURCES <- c("official", "instance", "user", "gate")

# 空值兜底。定义在这里，因为这个文件是最早被 source 的几个之一。
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---------------------------------------------------------------------
# 读取
# ---------------------------------------------------------------------
cfg_load <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path)) {
    stop("config.R: 未提供配置文件路径。", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("config.R: 配置文件不存在: ", path, call. = FALSE)
  }
  raw <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) stop("config.R: 配置不是合法 JSON: ", path, "\n  原因: ", conditionMessage(e), call. = FALSE)
  )
  if (is.null(raw$schema_version)) {
    stop("config.R: 配置缺少 schema_version 字段，拒绝使用（可能是旧格式）。", call. = FALSE)
  }
  raw$.path <- path
  raw
}

# ---------------------------------------------------------------------
# 取值：返回裸值（去掉 value/source 那层壳）
# ---------------------------------------------------------------------
cfg_get <- function(cfg, section, key, default = NULL) {
  node <- cfg[[section]][[key]]
  if (is.null(node)) {
    if (is.null(default)) {
      stop("config.R: 配置里没有 ", section, ".", key,
           "\n  这不是可以默认的东西。请在 pipeline.json 里补上，或由工作台填写。", call. = FALSE)
    }
    return(default)
  }
  # 带壳的写法：{value, source, why}。
  #   这里必须用 "value" %in% names(node) 判断，不能写 !is.null(node$value)。
  #   踩过坑：值本身是 NULL（配置里写 "value": null 表示「还没定」）时，
  #   !is.null(node$value) 为 FALSE，于是代码掉到下面把整个壳（长度 3 的 list）
  #   当成值返回。调用方拿到的就不是 NULL 而是一个 list，
  #   接着做长度/类型判断就会报「'length = 3' in coercion to 'logical(1)'」，
  #   而报错位置离真正的原因很远，极难查。
  if (is.list(node) && "value" %in% names(node)) return(node$value)
  if (is.list(node) && length(node) == 0) return(default)
  node
}

# 取整节点（含 source / locked / why），用于报告与检查
cfg_node <- function(cfg, section, key) {
  node <- cfg[[section]][[key]]
  if (is.null(node)) return(NULL)
  if (!is.list(node) || is.null(node$source)) {
    # 裸值：视为已给出，来源未知，明确标出而不是假装知道
    return(list(value = node, source = "unspecified", locked = FALSE, why = ""))
  }
  list(value = node$value, source = node$source,
       locked = isTRUE(node$locked), why = if (is.null(node$why)) "" else node$why)
}

# 取来源，用于运行报告
cfg_source <- function(cfg, section, key) {
  n <- cfg_node(cfg, section, key)
  if (is.null(n)) return("missing")
  if (!n$source %in% CFG_SOURCES && n$source != "unspecified") {
    warning("config.R: ", section, ".", key, " 的 source 取值非法: ", n$source,
            "（允许: ", paste(CFG_SOURCES, collapse = " / "), "）", call. = FALSE)
  }
  n$source
}

cfg_locked <- function(cfg, section, key) {
  n <- cfg_node(cfg, section, key)
  if (is.null(n)) return(FALSE)
  n$locked
}

# ---------------------------------------------------------------------
# 闸门 id → 配置路径 的映射
#
#  为什么要有这张表：
#    配置里的项（params.qc_min_features）和界面上的闸门（qc_threshold）是两套
#    命名。以前靠人工记得「答了 qc_threshold 要顺手改 qc_min_features」，
#    漏一次就是「界面显示已答、脚本还在报未确认」。
#    把映射写在一处，两边共用。
# ---------------------------------------------------------------------
GATE_TO_CFG <- list(
  data_indir             = list(sec = "io",        key = "indir"),
  reference_source       = list(sec = "io",        key = "reference_table"),
  qc_threshold           = list(sec = "params",    key = "qc_min_features"),
  read_exclusion         = list(sec = "steps",     key = "read_exclusion"),
  cluster_filter         = list(sec = "steps",     key = "cluster_filter"),
  post_cluster_exclusion = list(sec = "steps",     key = "post_cluster_exclusion"),
  lowconfidence          = list(sec = "steps",     key = "lowconfidence"),
  memory                 = list(sec = "steps",     key = "memory"),
  seurat_version         = list(sec = "params",    key = "expected_seurat_major")
)

# 读闸门清单（没有文件就返回 NULL）
cfg_read_gates <- function(gates_path) {
  if (is.null(gates_path) || is.na(gates_path) || !nzchar(gates_path) || !file.exists(gates_path)) {
    return(NULL)
  }
  g <- tryCatch(jsonlite::fromJSON(gates_path, simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(g) || is.null(g$gates)) return(NULL)
  g$gates
}

# 某一阶段（A / B）还没答完的闸门
#   A = 跑容量预检与主流程上半段之前（数据在哪、注释依据、质控门槛、读入前剔样本）
#   B = 跑主流程下半段之前（筛群、剔样本、低置信门槛）
#   为什么分两段：B 那几条的依据（簇大小、样本在各簇的分布、marker）
#   只有跑到一次聚类才产生。把 B 提前问，就是逼人在没有依据的情况下瞎猜。
cfg_gates_pending_phase <- function(gates_path, phase) {
  gs <- cfg_read_gates(gates_path)
  if (is.null(gs)) return(NULL)
  out <- list()
  for (g in gs) {
    ph <- if (is.null(g$phase)) "A" else as.character(g$phase)
    st <- if (is.null(g$status)) "pending" else as.character(g$status)
    if (identical(st, "pending") && identical(ph, phase)) {
      out[[length(out) + 1]] <- list(
        id = if (is.null(g$id)) "?" else g$id,
        title = if (is.null(g$title)) (g$id %||% "?") else g$title
      )
    }
  }
  out
}

# 把某个闸门的答复写回配置（值 + 来源）
#   值和来源必须同时改：只改值不改来源，配置里就出现
#   「值有了但标着 gate」的矛盾状态，后续检查会一直报警。
cfg_apply_gate_answer <- function(cfg, gate_id, value, detail = NULL) {
  m <- GATE_TO_CFG[[gate_id]]
  if (is.null(m)) return(list(cfg = cfg, applied = FALSE, why = "no mapping for this gate"))

  sec <- m$sec; key <- m$key
  if (is.null(cfg[[sec]])) cfg[[sec]] <- list()

  if (identical(sec, "steps")) {
    node <- cfg[[sec]][[key]]
    if (is.null(node) || !is.list(node)) node <- list()
    node$source <- "user"
    if (identical(gate_id, "read_exclusion") || identical(gate_id, "post_cluster_exclusion")) {
      node$mode <- if (identical(as.character(value), "none")) "none" else "exclude"
      node$samples <- if (identical(as.character(value), "none")) list() else
        as.list(strsplit(as.character(detail %||% ""), "[,，;；]")[[1]])
    } else if (identical(gate_id, "cluster_filter")) {
      v <- as.character(value)
      node$mode <- if (v %in% c("none", "by_name", "by_cluster")) v else "none"
      if (identical(v, "by_name")) {
        node$by_name_keep <- as.list(strsplit(as.character(detail %||% ""), "[,，;；]")[[1]])
      }
    } else if (identical(gate_id, "lowconfidence")) {
      v <- as.character(value)
      if (identical(v, "flag_only")) {
        node$mode <- "flag_only"
      } else {
        node$mode <- "threshold"
        node$min_cells <- suppressWarnings(as.numeric(v))
      }
    } else if (identical(gate_id, "memory")) {
      node$mode <- as.character(value)
    }
    cfg[[sec]][[key]] <- node
    return(list(cfg = cfg, applied = TRUE))
  }

  # 普通 {value, source} 项
  val <- value
  if (identical(gate_id, "data_indir")) {
    if (identical(as.character(value), "keep")) return(list(cfg = cfg, applied = FALSE, why = "chose to wait"))
    val <- as.character(detail %||% value)
  }
  if (identical(gate_id, "reference_source")) {
    v <- as.character(value)
    cfg$io$reference_table <- list(value = NULL, source = "user",
                                   why = "本次跳过注释")
    if (is.null(cfg$io$lit_dict)) cfg$io$lit_dict <- list(value = NULL, source = "user")
    if (identical(v, "reference_table")) {
      cfg$io$reference_table <- list(value = as.character(detail %||% ""), source = "user",
                                     why = "选定的注释依据")
      cfg$io$lit_dict <- list(value = NULL, source = "user", why = "改用文献参考表")
    } else if (identical(v, "lit_dict")) {
      cfg$io$lit_dict <- list(value = as.character(detail %||% ""), source = "user",
                              why = "选定的注释依据")
    } else if (identical(v, "skip")) {
      cfg$io$reference_table <- list(value = NULL, source = "user", why = "本次跳过注释")
      cfg$io$lit_dict <- list(value = NULL, source = "user", why = "本次跳过注释")
    }
    return(list(cfg = cfg, applied = TRUE))
  }
  if (identical(gate_id, "qc_threshold")) {
    if (identical(as.character(value), "keep")) return(list(cfg = cfg, applied = FALSE, why = "chose to wait"))
    val <- suppressWarnings(as.numeric(value))
  }
  if (identical(gate_id, "seurat_version")) {
    val <- if (identical(as.character(value), "v5_allow")) 5 else 4
  }

  cfg[[sec]][[key]] <- list(value = val, source = "user",
                            why = paste0("经入口确认（", gate_id, "）"))
  list(cfg = cfg, applied = TRUE)
}

# ---------------------------------------------------------------------
# 闸门检查：配置里凡是 source=gate 却还没有答案的，一律不许继续
#
#  phase 决定查哪些项：
#    NULL / "all"  全都查（最保守）
#    "A"           只查上段要用的
#    "B"           只查下段要用的
#
#  哪些项属于哪一段，由 GATE_TO_CFG 推出来，不再手写第二份名单。
#  手写第二份的结果是：改了映射忘了改名单，于是「明明答了还报未确认」。
# ---------------------------------------------------------------------
cfg_gate_blanks <- function(cfg, phase = NULL) {
  # 从映射表反推：某一段包含哪些 sec.key
  keys_in_phase <- function(ph) {
    out <- character(0)
    for (gid in names(GATE_TO_CFG)) {
      # 段位取自 gates 模板；模板读不到时一律算 A（多查不漏查）
      gph <- "A"
      m <- GATE_TO_CFG[[gid]]
      if (!is.null(m$phase) && length(m$phase)) gph <- as.character(m$phase)
      if (identical(gph, ph)) out <- c(out, paste0(m$sec, ".", m$key))
    }
    out
  }
  want <- if (is.null(phase) || identical(as.character(phase), "all")) NULL else keys_in_phase(as.character(phase))

  blanks <- character(0)
  for (sec in c("environment", "io", "params")) {
    items <- cfg[[sec]]
    if (is.null(items)) next
    for (k in names(items)) {
      node <- items[[k]]
      if (is.list(node) && identical(node$source, "gate")) {
        full <- paste0(sec, ".", k)
        if (!is.null(want) && !full %in% want) next
        v <- node$value
        empty <- is.null(v) || (length(v) == 1 && (is.na(v) || (is.character(v) && !nzchar(v))))
        if (empty) blanks <- c(blanks, full)
      }
    }
  }
  if (!is.null(cfg$steps)) {
    for (k in names(cfg$steps)) {
      node <- cfg$steps[[k]]
      if (is.list(node) && identical(node$source, "gate")) {
        full <- paste0("steps.", k)
        if (!is.null(want) && !full %in% want) next
        m <- node$mode
        if (is.null(m) || (is.character(m) && !nzchar(m))) blanks <- c(blanks, full)
      }
    }
  }
  blanks
}

# 有未决闸门就停。调用点：任何一次实际计算之前。
#
#  phase 参数决定查哪一段：
#    NULL / "all"  所有未决项都算（最保守，适合「一次跑完」的场景）
#    "A"           只查上段要用的（数据在哪、注释依据、质控门槛、读入前剔样本）
#    "B"           只查下段要用的（筛群、剔样本、低置信门槛）
#
#  为什么必须能分段查：
#    B 段那几条的依据（簇大小、样本在各簇的分布、marker）只有跑到一次聚类
#    才产生。如果上段之前就要求答完 B，那上段永远跑不起来，
#    也就永远产生不了依据 —— 死锁。而且那种情况下人只能瞎猜，等于白问。
cfg_require_resolved <- function(cfg, gates_path = NULL, phase = NULL) {
  blanks <- cfg_gate_blanks(cfg, phase)
  if (length(blanks) > 0) {
    msg <- c(
      "配置里还有未确认的项，不能开始计算。",
      paste0("  待确认: ", paste(blanks, collapse = ", ")),
      "  请打开发置界面，或直接编辑 pipeline.json，把这些项的 source 改成",
      "  user / official / instance 之一，并给出 value。",
      if (!is.null(gates_path)) paste0("  待确认问题清单: ", gates_path) else NULL,
      if (!is.null(phase)) paste0("  （本次只检查第 ", phase, " 段要用的项）") else NULL
    )
    stop(paste(msg, collapse = "\n"), call. = FALSE)
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------
# 导出：供运行报告用。列出每个参数的 值 / 来源 / 是否锁定
#   用程序生成，不手抄，避免报告和实际参数不一致。
# ---------------------------------------------------------------------
cfg_param_table <- function(cfg) {
  # 注意：这里不能用 <<- 。<<- 会跳过函数自己的帧去找上层，
  # 在函数顶层用 <<- 会直接跑到全局环境，报 object 'rows' not found。
  # 所以 add() 内部用 <<- 改这个 rows（它的上层正是本函数帧），
  # 本函数自己则一律用 <- 收集。
  rows <- list()
  add <- function(scope, key, node) {
    if (is.null(node)) return(invisible(NULL))
    rows[[length(rows) + 1]] <<- data.frame(
      scope = scope, key = key,
      value = paste(format(node$value), collapse = ","),
      source = node$source,
      locked = ifelse(node$locked, "locked", ""),
      why = node$why,
      stringsAsFactors = FALSE
    )
  }
  for (sec in c("environment", "io", "params")) {
    items <- cfg[[sec]]
    if (is.null(items)) next
    for (k in names(items)) add(sec, k, cfg_node(cfg, sec, k))
  }
  if (!is.null(cfg$steps)) {
    for (k in names(cfg$steps)) {
      n <- cfg$steps[[k]]
      if (is.null(n)) next
      rows <- c(rows, list(data.frame(
        scope = "steps", key = k,
        value = paste(c(mode = if (is.null(n$mode)) "" else n$mode,
                        samples = paste(unlist(n$samples), collapse = "|"),
                        by_name_keep = paste(unlist(n$by_name_keep), collapse = "|"),
                        min_cells = if (is.null(n$min_cells)) "" else n$min_cells),
                      collapse = " "),
        source = if (is.null(n$source)) "unspecified" else n$source,
        locked = "", why = if (is.null(n$reason)) "" else n$reason,
        stringsAsFactors = FALSE
      )))
    }
  }
  if (length(rows) == 0) {
    return(data.frame(scope = character(0), key = character(0), value = character(0),
                      source = character(0), locked = character(0), why = character(0),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------
# 分段模式：某个 step 的 mode 是不是这个值
#   例：cfg_step_is(cfg, "cluster_filter", "none")
# ---------------------------------------------------------------------
cfg_step_is <- function(cfg, step, mode) {
  n <- cfg$steps[[step]]
  if (is.null(n)) return(FALSE)
  identical(n$mode, mode)
}

cfg_step_get <- function(cfg, step, field, default = NULL) {
  n <- cfg$steps[[step]]
  if (is.null(n) || is.null(n[[field]])) return(default)
  v <- n[[field]]
  if (is.list(v) && length(v) == 0) return(default)
  v
}

# ---------------------------------------------------------------------
# 自查：配置里绝对不许出现的「写死痕迹」
#   这些是历史上真出过错的地方（代表性一行），留着当回归测试。
# ---------------------------------------------------------------------
cfg_selfcheck <- function(cfg) {
  problems <- character(0)

  # 1. 筛群：如果 final 出战
  if (is.null(cfg$steps$cluster_filter)) {
    problems <- c(problems, "steps.cluster_filter 缺失（筛群策略必须在配置里显式给出）")
  } else if (is.null(cfg$steps$cluster_filter$mode)) {
    problems <- c(problems, "steps.cluster_filter.mode 缺失")
  } else if (identical(cfg$steps$cluster_filter$mode, "by_cluster")) {
    # 允许，但必须有理由和说明——按簇号是数据相关决定
    if (is.null(cfg$steps$cluster_filter$reason) || !nzchar(cfg$steps$cluster_filter$reason)) {
      problems <- c(problems, "steps.cluster_filter 用了 by_cluster 但没写 reason（必须说明为什么按簇号，且换数据要重做）")
    }
  }

  # 2. 样本名不许散落在别处（只能在 steps 的 samples 里）
  #    这里只做提示，不做拦截
  # 3. 质控阈值必须来自配置，不许是 NA
  for (k in c("qc_min_features", "qc_max_features", "qc_mt")) {
    v <- cfg_get(cfg, "params", k, default = NULL)
    if (is.null(v) || (length(v) == 1 && is.na(v))) {
      problems <- c(problems, paste0("params.", k, " 没有值"))
    }
  }

  # 4. 种子必须有
  if (is.null(cfg_get(cfg, "params", "seed", default = NULL))) {
    problems <- c(problems, "params.seed 缺失（可复现的前提）")
  }

  problems
}
