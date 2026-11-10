#!/usr/bin/env Rscript
# =====================================================================
#  pick.R -- 查表：按「环境 × 需求」从库里取出该用的那一套
#
#  这是「存取」里的「取」。四步：
#    1. 判环境（内存、系统、包）        detect_env()
#    2. 在库表里按坐标找条目            pick_entry()
#    3. 校验收用条件（requires）        check_requires()
#    4. 给结论：用哪套、从哪来、为什么  pick_set()
#
#  为什么写成代码而不是写在一张说明里：
#    「先做容量预检，再按环境查表」如果只是文档里的一句话，
#    换个人、换台机器就会各按各的理解选脚本。选错脚本不会报错，
#    只会算出一份看起来正常的结果 —— 这正是最难查的那类问题。
#
#  一个要点：库里有条目 ≠ 能取。
#    status 不是 ready 的条目（比如还没导入的）一律不许当作可用，
#    要如实返回「位置已定但文件不在」，而不是挑一套相近的顶上去。
#
#  ASCII-only source（R 4.1.2 + 中文 Windows locale 不能解析非 ASCII 源码）。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, scipen = 999, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---------------------------------------------------------------------
# 读库表
# ---------------------------------------------------------------------
read_library_index <- function(engine_dir) {
  f <- file.path(engine_dir, "library", "index.json")
  if (!file.exists(f)) return(NULL)
  tryCatch(jsonlite::fromJSON(readLines(f, warn = FALSE, encoding = "UTF-8"),
                              simplifyVector = FALSE),
           error = function(e) NULL)
}

# ---------------------------------------------------------------------
# 一、判环境
#
#   三档，和库表里的 env_profiles 对应：
#     E1_windows_lowmem  小内存 Windows
#     E2_hpc             大内存 Linux
#     E3_unknown         说不清
#
#   内存读不出来时判 E3，不硬猜。
#   为什么：猜成 E2 会去取大内存那套，在小机器上必然爆内存；
#   猜成 E1 又要多绕一层分块。宁可标 E3 走探针。
# ---------------------------------------------------------------------
mem_total_gb <- function() {
  if (.Platform$OS.type == "windows") {
    tot <- tryCatch(suppressWarnings(system2("wmic",
      c("OS", "get", "TotalVisibleMemorySize", "/value"),
      stdout = TRUE, stderr = FALSE)), error = function(e) character(0))
    hit <- grep("TotalVisibleMemorySize", tot, value = TRUE)
    if (length(hit)) {
      kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", hit[1])))
      if (!is.na(kb)) return(kb / 1024 / 1024)
    }
    tot2 <- tryCatch(suppressWarnings(system2("powershell",
      c("-NoProfile", "-Command", "(Get-CimInstance Win32_ComputerSystem).TotalPhysicalMemory"),
      stdout = TRUE, stderr = FALSE)), error = function(e) character(0))
    n <- suppressWarnings(as.numeric(gsub("[^0-9]", "", tot2[1])))
    if (!is.na(n)) return(n / 1024^3)
    return(NA_real_)
  }
  mi <- tryCatch(readLines("/proc/meminfo"), error = function(e) character(0))
  hit <- grep("^MemTotal", mi, value = TRUE)
  if (length(hit)) {
    kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", hit[1])))
    if (!is.na(kb)) return(kb / 1024 / 1024)
  }
  NA_real_
}

detect_env <- function(mem_gb = NULL) {
  os <- if (.Platform$OS.type == "windows") "windows" else "linux"
  if (is.null(mem_gb)) mem_gb <- mem_total_gb()
  if (is.na(mem_gb)) {
    return(list(id = "E3_unknown", os = os, memory_gb = NA_real_,
                why = "内存读不出来，无法判定环境（不猜）"))
  }
  if (identical(os, "windows") && mem_gb < 20) {
    return(list(id = "E1_windows_lowmem", os = os, memory_gb = round(mem_gb, 1),
                why = sprintf("Windows + %.1f GB，属小内存机器", mem_gb)))
  }
  if (identical(os, "linux") && mem_gb >= 32) {
    return(list(id = "E2_hpc", os = os, memory_gb = round(mem_gb, 1),
                why = sprintf("Linux + %.1f GB，属大内存机器", mem_gb)))
  }
  list(id = "E3_unknown", os = os, memory_gb = round(mem_gb, 1),
       why = sprintf("%s + %.1f GB，不落在已知两档里，需实测后归类", os, mem_gb))
}

# ---------------------------------------------------------------------
# 二、校验收用条件
#
#   requires 里现在支持：
#    os            取值 "windows" / "linux"
#    memory_gb_min 内存下限
#    memory_gb_max 内存上限
#    packages      必须装好的 R 包
#    python        true 表示需要 Python
#
#  返回 list(ok, reasons) —— reasons 逐条写明哪一条没过。
#  不合并成一句「不满足」，因为「是内存不够还是缺包」决定下一步怎么办。
# ---------------------------------------------------------------------
check_requires <- function(req, env) {
  reasons <- character(0)
  if (is.null(req) || !length(req)) return(list(ok = TRUE, reasons = reasons))

  if (!is.null(req$os) && length(req$os)) {
    want <- as.character(req$os)
    if (!identical(want, "*") && !identical(want, env$os)) {
      reasons <- c(reasons, sprintf("系统要求 %s，实际 %s", want, env$os))
    }
  }
  if (!is.null(req$memory_gb_min) && !is.na(env$memory_gb)) {
    mn <- suppressWarnings(as.numeric(req$memory_gb_min))
    if (!is.na(mn) && env$memory_gb < mn) {
      reasons <- c(reasons, sprintf("内存要求至少 %.0f GB，实际 %.1f GB", mn, env$memory_gb))
    }
  }
  if (!is.null(req$memory_gb_max) && !is.na(env$memory_gb)) {
    mx <- suppressWarnings(as.numeric(req$memory_gb_max))
    if (!is.na(mx) && env$memory_gb > mx) {
      reasons <- c(reasons, sprintf("这套是为内存上限 %.0f GB 的机器适配的，本机 %.1f GB 用不上它",
                                    mx, env$memory_gb))
    }
  }
  if (!is.null(req$packages) && length(req$packages)) {
    pk <- as.character(unlist(req$packages))
    miss <- pk[!vapply(pk, function(p) requireNamespace(p, quietly = TRUE), logical(1))]
    if (length(miss)) reasons <- c(reasons, paste0("缺包：", paste(miss, collapse = ", ")))
  }
  if (isTRUE(req$python)) {
    py <- Sys.which("python3"); if (!nzchar(py)) py <- Sys.which("python")
    if (!nzchar(py)) reasons <- c(reasons, "需要 Python，本机没找到")
  }
  list(ok = length(reasons) == 0L, reasons = reasons)
}

# ---------------------------------------------------------------------
# 三、查表
#
#  category:
#    "A"  代码处理 —— 按坐标（环境 × 需求）找，还要看本机有没有甲方脚本
#    "B"  分析绘图 —— 不分环境，任何机器上都要从库取
#
#  local_scripts_found:
#    A 类专用。本机（跟数据放一起的地方）如果已经扫到甲方脚本，
#    就用本机的，不需要从库取 —— 这是您定的第一优先。
# ---------------------------------------------------------------------
pick_entry <- function(index, category, env, demand = NULL,
                       local_scripts_found = FALSE, purpose = NULL) {
  if (is.null(index) || is.null(index$entries)) return(NULL)
  entries <- index$entries
  in_cat <- Filter(function(e) identical(as.character(e$category), category), entries)
  if (!length(in_cat)) return(NULL)

  # B 类不按环境挑；A 类先按坐标筛
  if (identical(category, "A")) {
    # 第一优先：本机已有甲方脚本（跟数据放一起的那套）
    if (isTRUE(local_scripts_found)) {
      hit <- Filter(function(e) identical(as.character(e$role), "official"), in_cat)
      if (length(hit)) {
        return(list(entry = hit[[1]], source = "local",
                    why = "本机已有这套脚本（与数据同放），直接用本机的"))
      }
    }
    # 按环境找？
    #   E3（说不清）时不硬挑 —— 挑错不会报错，只会算错。
    if (identical(env$id, "E3_unknown")) {
      fb <- Filter(function(e) identical(as.character(e$role), "fallback"), in_cat)
      if (length(fb)) {
        return(list(entry = fb[[1]], source = "fallback",
                    why = paste0("环境判不出来（", env$why, "），按规程先做探针")))
      }
      return(NULL)
    }
    # 环境匹配的条目
    cand <- Filter(function(e) identical(as.character(e$env), env$id), in_cat)
    if (length(cand)) {
      # 有 status=ready 的优先；pending_import 的也返回，但要标明取不出来
      ready <- Filter(function(e) identical(as.character(e$status), "ready"), cand)
      chosen <- if (length(ready)) ready[[1]] else cand[[1]]
      return(list(entry = chosen, source = "library",
                  why = sprintf("按环境 %s 查表取到", env$id)))
    }
    fb <- Filter(function(e) identical(as.character(e$role), "fallback"), in_cat)
    if (length(fb)) {
      return(list(entry = fb[[1]], source = "fallback",
                  why = sprintf("库里没有匹配环境 %s 的脚本，按规程走兜底", env$id)))
    }
    return(NULL)
  }

  # B 类：按 role 挑（plots / ppt / reference / extract）
  if (!is.null(purpose)) {
    hit <- Filter(function(e) identical(as.character(e$role), purpose), in_cat)
    if (length(hit)) return(list(entry = hit[[1]], source = "library",
                                 why = "分析绘图类，任何机器上都从库取"))
  }
  # 没指定 purpose 就列出全部
  list(entries = in_cat, source = "library", why = "分析绘图类全部从库取")
}

# ---------------------------------------------------------------------
# 四、落地判定：拿到条目后，能不能真的用
#
#  这一步必须单独做，因为它和「库里有没有」是两件事：
#    库里登记了 → 不代表文件已经在；
#    文件在       → 不代表本机环境满足。
#  混在一起说「取到了」，最后会在跑的时候才发现不对。
# ---------------------------------------------------------------------
resolve_entry <- function(engine_dir, entry) {
  if (is.null(entry)) return(list(ok = FALSE, reason = "库里没有匹配的条目"))
  status <- as.character(entry$status %||% "unknown")
  rel <- as.character(entry$path %||% "")
  abs <- file.path(engine_dir, "library", rel)

  # ---- 2026-11-03 三层结构：skill 条目的落地判定 ----
  #   L1 的 skill 条目没有 path（它们不住在 library 下），只有 files 清单，
  #   每条带绝对路径。按老逻辑走，path 为空 -> files_ok=FALSE -> 一律判不可用，
  #   而实际上 skill 就在运行体上能跑。这里补一条分支：有 files 就逐个核存在性。
  fl <- entry$files
  if ((is.null(rel) || !nzchar(rel)) && is.list(fl) && length(fl)) {
    paths <- vapply(fl, function(f) {
      if (is.list(f)) as.character(f$path %||% "") else as.character(f)
    }, "")
    paths <- paths[nzchar(paths)]
    have <- vapply(paths, function(p) isTRUE(file.exists(p)), TRUE)
    n_ok <- sum(have)
    all_ok <- length(paths) > 0L && all(have)
    return(list(
      ok = identical(status, "ready") && all_ok,
      id = as.character(entry$id %||% ""),
      title = as.character(entry$title %||% ""),
      kind = as.character(entry$kind %||% "skill"),
      layer = as.character(entry$layer %||% ""),
      capability = as.character(entry$capability %||% ""),
      skill_name = as.character(entry$skill_name %||% ""),
      status = status,
      path_rel = "",
      path_abs = "",
      files_ok = all_ok,
      files_total = length(paths),
      files_found = n_ok,
      missing = paths[!have],
      fetchable = isTRUE(entry$fetchable),
      reason = if (!identical(status, "ready")) {
        sprintf("登记状态是 %s，现在取不出来", status)
      } else if (!all_ok) {
        sprintf("登记了 %d 个文件，实际找到 %d 个（缺：%s）",
                length(paths), n_ok, paste(basename(paths[!have]), collapse = "、"))
      } else ""
    ))
  }

  # kind 区分两类条目：
  #   set      一套脚本（要有文件才叫可用）
  #   playbook 一份处理规程（本身就不该有脚本文件，别拿「没有文件」当故障）
  # 不区分的话，兜底规程永远显示「库表与实际脱节」——
  # 那是假警报，会让人去修一个不存在的问题。
  kind <- as.character(entry$kind %||% "set")

  files_ok <- FALSE
  if (nzchar(rel) && dir.exists(abs)) {
    files_ok <- length(list.files(abs, recursive = TRUE)) > 0L
  }
  needs_files <- identical(kind, "set")

  usable_files <- if (needs_files) files_ok else TRUE

  list(
    ok = identical(status, "ready") && usable_files,
    id = as.character(entry$id %||% ""),
    title = as.character(entry$title %||% ""),
    kind = kind,
    layer = as.character(entry$layer %||% ""),
    capability = as.character(entry$capability %||% ""),
    mirrors = as.character(entry$mirrors %||% ""),
    status = status,
    path_rel = rel,
    path_abs = abs,
    files_ok = files_ok,
    fetchable = isTRUE(entry$fetchable),
    reason = if (!identical(status, "ready")) {
      sprintf("库里登记了位置，但状态是 %s —— 文件还没导进来，现在取不出来", status)
    } else if (needs_files && !files_ok) {
      "登记为可用，但目录里没有文件（库表与实际脱节）"
    } else {
      ""
    }
  )
}

# ---------------------------------------------------------------------
# 顶层：给一个环境和一个需求，返回「该用哪套 + 为什么 + 能不能用」
#
#  这是给流程调用的唯一入口。上面的函数不要在外面直接用。
# ---------------------------------------------------------------------
pick_set <- function(engine_dir, category = "A", demand = NULL,
                     purpose = NULL, local_scripts_found = FALSE, mem_gb = NULL) {
  index <- read_library_index(engine_dir)
  if (is.null(index)) {
    return(list(ok = FALSE, error = "读不到库表 library/index.json，无法查表"))
  }
  env <- detect_env(mem_gb)

  p <- pick_entry(index, category, env, demand = demand,
                  local_scripts_found = local_scripts_found, purpose = purpose)
  if (is.null(p)) {
    return(list(ok = FALSE, env = env,
                error = "库里没有匹配这套「环境 × 需求」的条目，也没有兜底规程"))
  }

  # B 类没指定 purpose 时，返回整组
  if (!is.null(p$entries)) {
    return(list(ok = TRUE, env = env, category = category,
                entries = lapply(p$entries, function(e) resolve_entry(engine_dir, e)),
                source = p$source, why = p$why))
  }

  r <- resolve_entry(engine_dir, p$entry)
  rq <- check_requires(p$entry$requires, env)
  r$requires_ok <- rq$ok
  r$requires_why <- rq$reasons
  r$env <- env
  r$source <- p$source
  r$why <- p$why
  r$usable <- isTRUE(r$ok) && isTRUE(rq$ok)
  r
}
