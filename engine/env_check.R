#!/usr/bin/env Rscript
# =====================================================================
#  env_check.R -- 环境体检
#
#  只读检查，不改任何数据、不写任何分析中间文件。
#  回答一个问题：这台机器现在能不能把这套流程跑起来。
#
#  用法：
#    Rscript env_check.R [--out <env_check.json>]
#
#  输出：
#    终端可读的逐项结果 + 一份 JSON（给工作台展示）
#
#  注意：本文件只用 ASCII 写控制台文本，中文放在 JSON 里。
#        Windows 控制台编码不一，乱码会让人误以为程序坏了。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, scipen = 999, warn = -1))

args <- commandArgs(trailingOnly = TRUE)
get_opt <- function(k, d = NULL) {
  i <- match(k, args)
  if (is.na(i)) return(d)
  if (i == length(args) || grepl("^--", args[i + 1])) return(d)
  args[i + 1]
}
OUT <- get_opt("--out", NULL)
IS_WIN <- .Platform$OS.type == "windows"

say <- function(...) { cat(enc2native(paste0(...)), "\n", sep = ""); flush(stdout()) }

# ---- 必需包（缺一个就跑不通）----
PKG_REQUIRED <- c("Seurat", "Matrix", "dplyr", "ggplot2", "patchwork", "jsonlite")
# ---- 可选包（缺了只影响某一段，不影响主流程）----
# 注意：这里必须用 list(...) 而不是 c(list(...), list(...))，
#       c() 会把 list 摊平成原子向量，后面的 o$name 就会报错。
PKG_OPTIONAL <- list(
  list(name = "DoubletFinder", why = "去双。缺了主流程「去双」这一步会停", stage = "去双"),
  list(name = "harmony",       why = "批次校正（多样本时才需要）", stage = "合并"),
  list(name = "presto",        why = "快速差异基因（缺了走慢速路径，结果一致）", stage = "差异"),
  list(name = "ggrepel",       why = "图上的标签避让", stage = "出图"),
  list(name = "org.Mm.eg.db",  why = "小鼠基因注释（富集分析用）", stage = "富集"),
  list(name = "org.Hs.eg.db",  why = "人基因注释（富集分析用）", stage = "富集"),
  list(name = "clusterProfiler", why = "GO/KEGG 富集。缺了差异富集段出不来", stage = "富集"),
  list(name = "SoupX",         why = "环境 RNA 校正（可选）", stage = "去双")
)

pkg_version <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) return(NA_character_)
  v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  v
}

results <- list()
add <- function(id, label, status, detail, value = NULL, stage = NULL) {
  results[[length(results) + 1L]] <<- list(
    id = id, label = label, status = status, detail = detail,
    value = value, stage = stage
  )
}

# ---------------------------------------------------------------------
# 1. R 本体
# ---------------------------------------------------------------------
add("r_version", "R 版本", "ok",
    paste0("R ", R.version$major, ".", R.version$minor, "  (", R.version$platform, ")"),
    value = paste0(R.version$major, ".", R.version$minor))

add("r_arch", "运行架构", if (grepl("x86_64|amd64|aarch64|arm64", R.version$arch, ignore.case = TRUE)) "ok" else "warn",
    paste0(R.version$arch, " / ", .Platform$OS.type), value = R.version$arch)

# ---------------------------------------------------------------------
# 2. 包
# ---------------------------------------------------------------------
for (p in PKG_REQUIRED) {
  v <- pkg_version(p)
  if (is.na(v)) add(paste0("pkg_", p), p, "fail", "缺失（必需）。主流程会用到它", stage = "必需")
  else add(paste0("pkg_", p), p, "ok", paste0("v", v), value = v)
}
for (o in PKG_OPTIONAL) {
  v <- pkg_version(o$name)
  if (is.na(v)) add(paste0("pkg_", o$name), o$name, "warn", paste0("缺失：", o$why), stage = o$stage)
  else add(paste0("pkg_", o$name), o$name, "ok", paste0("v", v), value = v)
}

# ---------------------------------------------------------------------
# 3. Seurat 大版本（决定后面的代码路径）
# ---------------------------------------------------------------------
sv <- pkg_version("Seurat")
seurat_major <- NA_integer_
if (!is.na(sv)) seurat_major <- as.integer(sub("\\..*$", "", sv))
if (!is.na(seurat_major)) {
  if (seurat_major >= 5) {
    add("seurat_major", "Seurat 大版本", "warn",
        paste0("本机是 v", seurat_major, "。官方基于 v4，槽访问语义不同，结果不可直接比对。",
               "要跑需要显式接受差异。"), value = as.character(seurat_major), stage = "依赖")
  } else {
    add("seurat_major", "Seurat 大版本", "ok",
        paste0("v", seurat_major, "，与官方一致"), value = as.character(seurat_major), stage = "依赖")
  }
}

# ---------------------------------------------------------------------
# 4. 内存（这套流程最常卡的地方）
# ---------------------------------------------------------------------
mem_gb <- NA_real_
if (IS_WIN) {
  tot <- tryCatch(suppressWarnings(system2("wmic", c("OS", "get", "TotalVisibleMemorySize", "/value"),
                    stdout = TRUE, stderr = FALSE)), error = function(e) character(0))
  hit <- grep("TotalVisibleMemorySize", tot, value = TRUE)
  if (length(hit)) {
    kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", hit[1])))
    if (!is.na(kb)) mem_gb <- kb / 1024 / 1024
  }
  if (is.na(mem_gb)) {
    tot2 <- tryCatch(suppressWarnings(system2("powershell",
      c("-NoProfile", "-Command", "(Get-CimInstance Win32_ComputerSystem).TotalPhysicalMemory"),
      stdout = TRUE, stderr = FALSE)), error = function(e) character(0))
    n <- suppressWarnings(as.numeric(gsub("[^0-9]", "", tot2[1])))
    if (!is.na(n)) mem_gb <- n / 1024^3
  }
} else {
  mi <- tryCatch(readLines("/proc/meminfo"), error = function(e) character(0))
  hit <- grep("^MemTotal", mi, value = TRUE)
  if (length(hit)) {
    kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", hit[1])))
    if (!is.na(kb)) mem_gb <- kb / 1024 / 1024
  }
}

if (!is.na(mem_gb)) {
  st <- if (mem_gb >= 32) "ok" else if (mem_gb >= 12) "warn" else "fail"
  det <- if (mem_gb >= 32) paste0(round(mem_gb, 1), " GB，原路径可跑")
         else if (mem_gb >= 12) paste0(round(mem_gb, 1), " GB，接近下限；大样本可能要分块")
         else paste0(round(mem_gb, 1), " GB，明显不够；必须走分块路径")
  add("memory_gb", "物理内存", st, det, value = round(mem_gb, 1), stage = "容量")
} else {
  add("memory_gb", "物理内存", "warn", "读不出来。请在「全部设置」里手填 memory_gb", stage = "容量")
}

# ---------------------------------------------------------------------
# 5. 磁盘剩余（输出目录要写 rds，占得不少）
# ---------------------------------------------------------------------
for (d in c(get_opt("--check-dir", NULL))) {
  if (is.null(d) || !dir.exists(d)) next
}
probe <- if (IS_WIN) "C:/" else "/"
free_gb <- NA_real_
if (IS_WIN) {
  o <- tryCatch(suppressWarnings(system2("wmic",
    c("logicaldisk", "get", "size,freespace,caption", "/value"),
    stdout = TRUE, stderr = FALSE)), error = function(e) character(0))
  if (length(o)) {
    free <- suppressWarnings(as.numeric(gsub("[^0-9]", "", grep("^FreeSpace", o, value = TRUE))))
    if (length(free) && !is.na(free[1])) free_gb <- free[1] / 1024^3
  }
} else {
  df <- tryCatch(suppressWarnings(system2("df", c("-Pk", probe), stdout = TRUE, stderr = FALSE)),
                 error = function(e) character(0))
  if (length(df) >= 2) {
    f <- strsplit(trimws(df[2]), "\\s+")[[1]]
    if (length(f) >= 4) free_gb <- suppressWarnings(as.numeric(f[4])) / 1024 / 1024
  }
}
if (!is.na(free_gb)) {
  add("disk_free_gb", "系统盘剩余", if (free_gb >= 20) "ok" else if (free_gb >= 8) "warn" else "fail",
      paste0(round(free_gb, 1), " GB 可用。输出目录建议放在同一块盘"), value = round(free_gb, 1), stage = "容量")
}

# ---------------------------------------------------------------------
# 6. 出 PPT 用的 Python
# ---------------------------------------------------------------------
find_py <- function() {
  p <- get_opt("--python", NULL)
  if (!is.null(p) && file.exists(p)) return(p)
  for (c1 in if (IS_WIN) c("python.exe", "python3.exe") else c("python3", "python")) {
    h <- Sys.which(c1); if (nzchar(h)) return(unname(h))
  }
  NULL
}
py <- find_py()
if (is.null(py)) {
  add("python", "Python", "warn",
      "没找到。「出 PPT」这一步需要它；其余步骤不受影响。装了之后在「全部设置」里填 python_path", stage = "出 PPT")
} else {
  add("python", "Python", "ok", enc2native(py), value = enc2native(py), stage = "出 PPT")
}

# ---------------------------------------------------------------------
# 7. 打包用的工具
# ---------------------------------------------------------------------
zbin <- if (IS_WIN) Sys.which("tar") else { z <- Sys.which("zip"); if (nzchar(z)) z else Sys.which("tar") }
if (!nzchar(zbin)) {
  add("ziptool", "打包工具", "warn", "没找到 zip/tar。「打包结果」那一步用不了，其余不受影响", stage = "打包")
} else {
  add("ziptool", "打包工具", "ok", enc2native(unname(zbin)), stage = "打包")
}

# ---------------------------------------------------------------------
# 8. 机器上已有的分析脚本
#
#  为什么必须先查这一步：
#    不能默认「这台机器上没有脚本，所以要拷一套过去」。对方很可能已经有，
#    而且那套是通过验收的。再拷一套并行，两套早晚分叉 —— 到时候
#    「哪个结果算数」都说不清。
#
#  所以：先看有什么，再决定用哪套。这里只读，不动任何文件。
#
#  扫描范围刻意保守：只扫常见用户目录，每个目录只看一级，且限时。
#  宁可漏报让人自己填，也不要为了「扫得全」而卡住几分钟 ——
#  体检这一步如果慢到让人等，就不会有人愿意跑它。
# ---------------------------------------------------------------------
scan_existing_scripts <- function(budget_sec = 20) {
  t0 <- Sys.time()
  home <- path.expand("~")
  bases <- c(file.path(home, "Desktop"), file.path(home, "Documents"),
             file.path(home, "Downloads"), home)
  if (IS_WIN) {
    for (L in LETTERS) {
      d <- paste0(L, ":/")
      if (dir.exists(d)) bases <- c(bases, d)
    }
  }
  bases <- unique(bases[dir.exists(bases)])

  # 名字像脚本库才进去看；不符合的直接跳过，避免把 data/ 这种大树拖进来
  name_pat <- "(?i)(script|脚本|scwork|sc_|seurat|single.?cell|单细胞|pipeline|流程)"

  # 目录里是不是「一套流程」：至少两个分步脚本，或带运行日志/说明
  looks_like_lib <- function(d) {
    fs <- tryCatch(list.files(d, recursive = TRUE, full.names = TRUE,
                              pattern = "\\.(R|r)$"), error = function(e) character(0))
    if (length(fs) < 2) return(NULL)
    if (length(fs) > 3000) return(NULL)   # 大得离谱的多半是包库，不是流程
    nm <- basename(fs)
    staged <- sum(grepl("(?i)^(0?\\d[._-]|step_?\\d|s\\d_)", nm))
    has_doc <- any(grepl("(?i)(run_log|readme)", basename(list.files(d, recursive = TRUE))))
    if (staged >= 2 || has_doc) {
      return(list(n_r = length(fs), staged = staged, has_doc = has_doc,
                  sample = utils::head(nm[order(nm)], 8)))
    }
    NULL
  }

  hits <- list()
  for (base in bases) {
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > budget_sec) break
    lv1 <- tryCatch(list.dirs(base, recursive = FALSE, full.names = TRUE),
                    error = function(e) character(0))
    for (d in lv1) {
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > budget_sec) break
      nm <- basename(d)
      if (startsWith(nm, ".") || startsWith(nm, "$")) next
      if (!grepl(name_pat, nm)) next
      info <- tryCatch(looks_like_lib(d), error = function(e) NULL)
      if (is.null(info)) next
      hits[[length(hits) + 1L]] <- c(list(path = gsub("\\\\", "/", d)), info)
    }
  }
  hits
}

existing <- tryCatch(scan_existing_scripts(), error = function(e) list())
if (length(existing)) {
  n <- length(existing)
  paths <- vapply(existing, function(h) h$path, character(1))
  add("existing_scripts", "机器上已有的脚本", "warn",
      paste0("发现 ", n, " 处现成的分析脚本库。先确认用哪套 —— ",
             "并行两套早晚分叉，到时候说不清哪个结果算数：",
             paste0("  ", paths, collapse = "")),
      value = as.character(n), stage = "先决")
  # 详细清单写进 JSON，页面可以展开看
  attr(results[[length(results)]], "hits") <- existing
} else {
  add("existing_scripts", "机器上已有的脚本", "ok",
      "没扫到成体系的 R 流程脚本库（只看了桌面/文档/下载/家目录与盘根一级，限时 20 秒）",
      value = "0", stage = "先决")
}

# ---------------------------------------------------------------------
# 汇总
# ---------------------------------------------------------------------
n_fail <- sum(vapply(results, function(x) identical(x$status, "fail"), logical(1)))
n_warn <- sum(vapply(results, function(x) identical(x$status, "warn"), logical(1)))
n_ok   <- sum(vapply(results, function(x) identical(x$status, "ok"), logical(1)))

verdict <- if (n_fail > 0) "blocked" else if (n_warn > 0) "runnable_with_notes" else "ready"
verdict_cn <- switch(verdict,
  blocked = "跑不通：有必需项缺失，先补齐",
  runnable_with_notes = "能跑，但有提醒项",
  ready = "环境齐备，可以直接跑")

say("")
say("================ Environment check ================")
for (r in results) {
  mark <- switch(r$status, ok = "[ OK ]", warn = "[WARN]", fail = "[FAIL]", "[ ?? ]")
  say(sprintf("%s %-22s %s", mark, r$label, r$detail))
}
say("----------------------------------------------------")
say(sprintf("ok=%d  warn=%d  fail=%d", n_ok, n_warn, n_fail))
say(sprintf("verdict: %s", verdict))
say("====================================================")
say("")

if (!is.null(OUT)) {
  dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
  payload <- list(
    ok = TRUE, generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    verdict = verdict, verdict_cn = verdict_cn,
    counts = list(ok = n_ok, warn = n_warn, fail = n_fail),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    r_path = enc2native(file.path(R.home("bin"), if (IS_WIN) "Rscript.exe" else "Rscript")),
    platform = .Platform$OS.type,
    items = results
  )
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    txt <- jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE, null = "null")
  } else {
    txt <- "{\"ok\":false,\"error\":\"jsonlite missing\"}"
  }
  con <- file(OUT, open = "wb"); writeBin(charToRaw(enc2utf8(txt)), con); close(con)
  say(sprintf("written: %s", OUT))
}

# 注意：这一步永远以 0 退出。
#   体检的结论（跑不通 / 能跑 / 有提醒）写在 JSON 的 verdict 里，由页面用颜色表达。
#   如果用它自己返回非 0，会把「体检这个动作没做完」和「体检结论是不合格」混为一谈，
#   页面上会显示成「这一步失败」，反而看不清真正的结论。
quit(save = "no", status = 0L)
