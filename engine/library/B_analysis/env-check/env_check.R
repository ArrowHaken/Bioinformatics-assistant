#!/usr/bin/env Rscript
# ============================================================================
# sc-env-check —— 单细胞流水线环境清点器（只读）
#
# 用法:
#   Rscript env_check.R --lib <R包库路径> [--lib <另一个库路径>] \
#                       [--required <必需包清单文件>] --out <输出目录>
#
# 铁律:
#   * 只读不改。不安装任何包，不写入库路径下任何文件，不改 .libPaths() 的持久配置。
#   * 不做静默降级。检查不到就如实报“缺”，不用近似、替代或猜测填补。
#
# 产出:
#   <out>/env_manifest.tsv   全部已装包 + 版本（制表符分隔）
#   <out>/env_report.md      人读摘要：R 版本 / 平台 / 库路径 / 必需包核对 / 缺失清单 / 工具链
#
# 交叉平台: 路径一律用 file.path 拼接；不写死平台相关路径。
# ============================================================================

# ---------------------------------------------------------------------------
# 1. 参数解析
# ---------------------------------------------------------------------------

usage <- function() {
  cat(
    "用法: Rscript env_check.R [--lib <库路径>]... [--required <清单文件>] --out <输出目录>\n",
    "  --lib       可多次出现，指定要清点的 R 包库路径；不给则默认清点 .libPaths()\n",
    "  --required  可选，必需包清单文件，每行一个包名（# 开头与空行忽略）\n",
    "  --out       必填，输出目录（不存在则创建）\n",
    sep = ""
  )
}

parse_args <- function(argv) {
  # 支持 --lib 多次出现；其余参数为单值。--key=value 与 --key value 两种写法都接受。
  libs     <- character(0)
  required <- NULL
  out      <- NULL
  i <- 1L
  while (i <= length(argv)) {
    a <- argv[[i]]
    val <- NULL
    if (grepl("^--[^=]+=", a)) {
      key <- sub("^--([^=]+)=.*$", "\\1", a)
      val <- sub("^--[^=]+=", "", a)
    } else if (grepl("^--", a)) {
      key <- sub("^--", "", a)
      if (key %in% c("help", "h")) {
        usage(); quit(status = 0, save = "no")
      }
      if (i == length(argv) || grepl("^--", argv[[i + 1]])) {
        stop(sprintf("参数 --%s 缺少取值", key), call. = FALSE)
      }
      val <- argv[[i + 1]]
      i <- i + 1L
    } else {
      stop(sprintf("无法识别的参数: %s", a), call. = FALSE)
    }
    key <- gsub("-", "_", key)
    if (key == "lib") {
      libs <- c(libs, val)
    } else if (key == "required") {
      required <- val
    } else if (key == "out") {
      out <- val
    } else if (key %in% c("help", "h")) {
      usage(); quit(status = 0, save = "no")
    } else {
      stop(sprintf("未知参数: --%s", key), call. = FALSE)
    }
    i <- i + 1L
  }
  if (is.null(out) || !nzchar(out)) {
    stop("缺少必填参数 --out", call. = FALSE)
  }
  list(libs = libs, required = required, out = out)
}

argv <- commandArgs(trailingOnly = TRUE)
opt  <- tryCatch(parse_args(argv), error = function(e) {
  cat("参数错误:", conditionMessage(e), "\n", file = stderr())
  usage()
  quit(status = 2, save = "no")
})

# ---------------------------------------------------------------------------
# 2. 输出目录准备（只创建 --out 目录，不动库路径）
# ---------------------------------------------------------------------------
out_dir <- opt$out
if (!dir.exists(out_dir)) {
  ok <- dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  if (!ok && !dir.exists(out_dir)) {
    stop(sprintf("无法创建输出目录: %s", out_dir), call. = FALSE)
  }
}
out_dir <- normalizePath(out_dir, winslash = "/", mustWork = TRUE)

# ---------------------------------------------------------------------------
# 3. 库路径确定与校验
# ---------------------------------------------------------------------------
default_libs <- .libPaths()
req_libs     <- if (length(opt$libs) > 0) opt$libs else default_libs

# 用户显式给出的库路径若不存在，如实记录，不静默丢弃
lib_missing <- req_libs[!dir.exists(req_libs)]
lib_valid   <- req_libs[dir.exists(req_libs)]
# 去重并规范化（仅用于扫描，不修改 .libPaths()）
if (length(lib_valid) > 0) {
  lib_valid_norm <- normalizePath(lib_valid, winslash = "/", mustWork = TRUE)
  lib_valid_norm <- lib_valid_norm[!duplicated(tolower(lib_valid_norm))]
} else {
  lib_valid_norm <- character(0)
}

# ---------------------------------------------------------------------------
# 4. 扫描已装包：直接读各库下的 DESCRIPTION（纯读，不改动）
#    返回 data.frame(Package, Version, LibPath, Priority, Status)
# ---------------------------------------------------------------------------
scan_library <- function(lib) {
  # 库目录下的一级子目录即候选包目录
  entries <- list.dirs(lib, full.names = TRUE, recursive = FALSE)
  rows <- list()
  for (pkg_dir in entries) {
    desc <- file.path(pkg_dir, "DESCRIPTION")
    pkg  <- basename(pkg_dir)
    if (!file.exists(desc)) {
      # 不是标准包目录（如 00LOCK / 00new 等）：如实标记，不静默略过
      rows[[length(rows) + 1L]] <- data.frame(
        Package = pkg, Version = NA_character_, LibPath = lib,
        Priority = NA_character_, Status = "缺 DESCRIPTION（非标准包目录）",
        stringsAsFactors = FALSE
      )
      next
    }
    # 只读 DESCRIPTION 的 Version / Priority 两个字段
    lines <- tryCatch(readLines(desc, warn = FALSE), error = function(e) NULL)
    if (is.null(lines)) {
      rows[[length(rows) + 1L]] <- data.frame(
        Package = pkg, Version = NA_character_, LibPath = lib,
        Priority = NA_character_, Status = "DESCRIPTION 读取失败",
        stringsAsFactors = FALSE
      )
      next
    }
    get_field <- function(field) {
      hit <- grep(paste0("^", field, ":"), lines, value = TRUE)
      if (length(hit) == 0) return(NA_character_)
      trimws(sub(paste0("^", field, ":"), "", hit[[1]]))
    }
    ver <- get_field("Version")
    pri <- get_field("Priority")
    status <- if (is.na(ver) || !nzchar(ver)) "缺 Version 字段" else "OK"
    rows[[length(rows) + 1L]] <- data.frame(
      Package = pkg, Version = ver, LibPath = lib,
      Priority = pri, Status = status, stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) {
    return(data.frame(Package = character(0), Version = character(0),
                      LibPath = character(0), Priority = character(0),
                      Status = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

manifest_list <- lapply(lib_valid_norm, scan_library)
manifest <- if (length(manifest_list) == 0) {
  data.frame(Package = character(0), Version = character(0),
             LibPath = character(0), Priority = character(0),
             Status = character(0), stringsAsFactors = FALSE)
} else {
  do.call(rbind, manifest_list)
}
# 排序：库路径 → 包名（不区分大小写）
if (nrow(manifest) > 0) {
  manifest <- manifest[order(tolower(manifest$LibPath), tolower(manifest$Package)), ]
  rownames(manifest) <- NULL
}

# 有效包（有 Version 的）用于查版本
valid_pkgs <- manifest[!is.na(manifest$Version) & nzchar(manifest$Version), , drop = FALSE]
# 同名包可能出现在多个库：R 的解析顺序是 .libPaths 靠前者优先
lookup_pkg <- function(name) {
  hit <- valid_pkgs[tolower(valid_pkgs$Package) == tolower(name), , drop = FALSE]
  if (nrow(hit) == 0) return(NULL)
  hit$order <- match(hit$LibPath, lib_valid_norm)
  hit <- hit[order(hit$order), , drop = FALSE]
  hit[1, , drop = FALSE]
}

# ---------------------------------------------------------------------------
# 5. 必需包核对（若提供 --required）
# ---------------------------------------------------------------------------
required_names <- character(0)
required_file  <- opt$required
required_note  <- "未提供必需包清单（未做核对）"

if (!is.null(required_file)) {
  if (!file.exists(required_file)) {
    # 明确报错退出，不静默降级为“无必需包”
    stop(sprintf("必需包清单文件不存在: %s", required_file), call. = FALSE)
  }
  raw <- readLines(required_file, warn = FALSE)
  raw <- trimws(sub("#.*$", "", raw))   # 去注释与首尾空白
  required_names <- raw[nzchar(raw)]
  required_note  <- sprintf("已读取清单 %s，共 %d 个包名",
                            required_file, length(required_names))
}

req_rows <- list()
if (length(required_names) > 0) {
  for (p in required_names) {
    hit <- lookup_pkg(p)
    if (is.null(hit)) {
      req_rows[[length(req_rows) + 1L]] <- data.frame(
        Required = p, Installed = "否", Version = "-",
        LibPath = "-", stringsAsFactors = FALSE
      )
    } else {
      req_rows[[length(req_rows) + 1L]] <- data.frame(
        Required = p, Installed = "是", Version = hit$Version,
        LibPath = hit$LibPath, stringsAsFactors = FALSE
      )
    }
  }
}
req_table <- if (length(req_rows) == 0) {
  data.frame(Required = character(0), Installed = character(0),
             Version = character(0), LibPath = character(0), stringsAsFactors = FALSE)
} else {
  do.call(rbind, req_rows)
}
# 去重（清单中重复包名只报一次）
if (nrow(req_table) > 0) {
  req_table <- req_table[!duplicated(tolower(req_table$Required)), , drop = FALSE]
  rownames(req_table) <- NULL
}

missing_pkgs  <- req_table$Required[tolower(req_table$Installed) == "否"]
installed_req <- req_table[tolower(req_table$Installed) == "是", , drop = FALSE]

# ---------------------------------------------------------------------------
# 6. 编译工具链检查（make / gcc / g++）—— 决定能否源码编译包
# ---------------------------------------------------------------------------
tool_names <- c("make", "gcc", "g++")
tool_paths <- vapply(tool_names, function(t) {
  p <- Sys.which(t)
  if (is.na(p) || !nzchar(p)) NA_character_ else as.character(p)
}, character(1))
tools <- data.frame(
  Tool = tool_names,
  Found = ifelse(is.na(tool_paths), "否", "是"),
  Path = ifelse(is.na(tool_paths), "-", tool_paths),
  stringsAsFactors = FALSE
)
toolchain_ok <- all(tools$Found == "是")

# Windows 上额外如实探测 Rtools（不存在就报缺，不糊弄）
rtools_note <- NULL
if (.Platform$OS.type == "windows") {
  cand <- c(Sys.getenv("RTOOLS40_HOME"), Sys.getenv("RTOOLS_HOME"),
            "C:/rtools44", "C:/rtools43", "C:/rtools42", "C:/rtools40")
  cand <- cand[nzchar(cand)]
  exist_rt <- cand[dir.exists(cand)]
  rtools_note <- if (length(exist_rt) > 0) {
    sprintf("检测到 Rtools: %s", paste(exist_rt, collapse = "; "))
  } else {
    "未在常见位置检测到 Rtools（Windows 上源码编译还需 Rtools）"
  }
} else {
  # Linux/macOS 需要 C 编译器
  cc <- Sys.which("cc")
  rtools_note <- if (!is.na(cc) && nzchar(cc)) {
    sprintf("检测到 C 编译器 cc: %s", as.character(cc))
  } else {
    "未检测到 cc（部分包源码编译可能失败）"
  }
}

# ---------------------------------------------------------------------------
# 7. 写出 env_manifest.tsv
# ---------------------------------------------------------------------------
manifest_path <- file.path(out_dir, "env_manifest.tsv")
manifest_out <- manifest[, c("Package", "Version", "LibPath", "Priority", "Status")]
manifest_out$Version <- ifelse(is.na(manifest_out$Version), "NA", manifest_out$Version)
manifest_out$Priority <- ifelse(is.na(manifest_out$Priority), "", manifest_out$Priority)
write.table(manifest_out, file = manifest_path, sep = "\t",
            quote = FALSE, row.names = FALSE, na = "")

# ---------------------------------------------------------------------------
# 8. 写出 env_report.md
# ---------------------------------------------------------------------------
md <- character(0)
add <- function(...) md <<- c(md, paste0(...))

add("# 单细胞流水线环境清点报告")
add("")
add("- 生成时间: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
add("- 主机: ", Sys.info()[["nodename"]], "  用户: ", Sys.info()[["user"]])
add("")
add("## 1. R 运行环境")
add("")
add("- R 版本: ", R.version.string)
add("- 平台: ", R.version$platform, " (", .Platform$OS.type, "/", .Platform$r_arch, ")")
add("- 可执行文件: ", file.path(R.home("bin"), "R"))
add("")
add("### 实际 .libPaths()（进程当前生效）")
add("")
for (p in default_libs) add("- ", p)
add("")
add("### 本次清点的库路径")
add("")
if (length(lib_valid_norm) > 0) {
  for (p in lib_valid_norm) {
    n <- sum(tolower(manifest$LibPath) == tolower(p))
    add("- ", p, "  （包目录条目: ", n, "）")
  }
} else {
  add("- （无有效库路径，无法扫描）")
}
if (length(lib_missing) > 0) {
  add("")
  add("以下指定库路径不存在，未能清点（如实列出，未跳过）:")
  add("")
  for (p in lib_missing) add("- 缺: ", p)
}
add("")

add("## 2. 已装包总览")
add("")
add("- 已装包总数（有 Version 的条目）: ", nrow(valid_pkgs))
add("- 库目录条目总数（含异常条目）: ", nrow(manifest))
if (nrow(manifest) > 0 && any(manifest$Status != "OK")) {
  bad <- manifest[manifest$Status != "OK", , drop = FALSE]
  add("- 异常条目: ", nrow(bad), " 个（详见 env_manifest.tsv 的 Status 列）")
  for (i in seq_len(nrow(bad))) {
    add("    - ", bad$Package[i], " @ ", bad$LibPath[i], "  ->  ", bad$Status[i])
  }
}
add("")
add("完整清单见同目录 env_manifest.tsv（列: Package / Version / LibPath / Priority / Status）。")
add("")

add("## 3. 必需包核对")
add("")
add("- 清单状态: ", required_note)
if (length(required_names) > 0) {
  n_missing <- length(missing_pkgs)
  n_ok      <- nrow(installed_req)
  add("- 核对结果: 共 ", nrow(req_table), " 个，已装 ", n_ok, " 个，缺失 ", n_missing, " 个")
  add("")
  add("### 3.1 已装（含版本与来源库路径）")
  add("")
  if (n_ok > 0) {
    for (i in seq_len(nrow(installed_req))) {
      add("- [OK] ", installed_req$Required[i], "  ", installed_req$Version[i],
          "   ", installed_req$LibPath[i])
    }
  } else {
    add("- （无）")
  }
  add("")
  add("### 3.2 缺失（需补装）")
  add("")
  if (n_missing > 0) {
    for (p in missing_pkgs) add("- [缺] ", p)
  } else {
    add("- （无，全部满足）")
  }
  add("")
  add("> 说明: “缺失”仅表示在本次清点的库路径中找不到该包，未做任何近似或替代判断。")
  add("")
} else {
  add("")
  add("未提供 --required 清单文件，本节只报告已装包总览，不做逐项核对。")
  add("")
}

add("## 4. 编译工具链（决定能否源码编译安装）")
add("")
for (i in seq_len(nrow(tools))) {
  add("- ", tools$Tool[i], ": ", tools$Found[i],
      if (tools$Found[i] == "是") paste0("  (", tools$Path[i], ")") else "  (PATH 中未找到)")
}
add("")
add("- 结论: ", if (toolchain_ok) "make / gcc / g++ 齐备，可源码编译包"
                else "工具链不完整，缺 make / gcc / g++ 中的至少一个，源码编译会失败")
if (!is.null(rtools_note)) add("- 附加: ", rtools_note)
add("")
add("## 5. 判定与下一步建议")
add("")
if (length(required_names) > 0) {
  if (length(missing_pkgs) == 0) {
    add("- 必需包全部就位，可继续跑流水线 / 打包。")
  } else {
    add("- 存在缺失包 ", length(missing_pkgs), " 个: ", paste(missing_pkgs, collapse = ", "))
    if (!toolchain_ok) {
      add("- 且编译工具链不完整，缺包无法在本机源码编译，需先补齐 make / gcc / g++（Windows 需 Rtools）。")
    } else {
      add("- 工具链可用，可按清单在上面的库路径补装后重跑本清点。")
    }
  }
} else {
  add("- 本次未做必需包核对；如需核对，请提供每行一个包名的 --required 清单文件后重跑。")
}
add("")
add("---")
add("本报告由 sc-env-check 生成，全程只读：未安装任何包，未修改库路径下任何文件。")
add("")

report_path <- file.path(out_dir, "env_report.md")
writeLines(md, con = report_path, useBytes = TRUE)

# ---------------------------------------------------------------------------
# 9. 控制台摘要（供调用方直接读）
# ---------------------------------------------------------------------------
cat("=== sc-env-check 完成 ===\n")
cat("R 版本      :", R.version.string, "\n")
cat("平台        :", R.version$platform, "\n")
cat("清点库路径  :", length(lib_valid_norm), "个\n")
cat("已装包数量  :", nrow(valid_pkgs), "\n")
if (length(required_names) > 0) {
  cat("必需包核对  : 已装", nrow(installed_req), "/ 缺失", length(missing_pkgs), "\n")
  if (length(missing_pkgs) > 0) cat("缺失        :", paste(missing_pkgs, collapse = ", "), "\n")
}
cat("工具链      :", if (toolchain_ok) "完整" else "不完整（缺 make/gcc/g++）", "\n")
cat("输出        :", manifest_path, "\n")
cat("输出        :", report_path, "\n")
