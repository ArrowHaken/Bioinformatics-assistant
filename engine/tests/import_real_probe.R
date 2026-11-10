#!/usr/bin/env Rscript
# =====================================================================
#  import_real_probe.R -- 把一次真机体检结果写成应用认的 remote_env.json
#
#  为什么不能手写这个文件
#    它的形状是 env_check.R / remote_check.R 的输出契约：
#    items[] 每条 {id,label,status,detail,value}，下游 runnable.R 按 id 查。
#    手写一遍容易漏字段，漏了不会报错，只会让判定悄悄变成 unknown。
#    所以这里用同一个 add() 口径生成，再落盘。
#
#  数据来源
#    2026-11-02 在执行机 PC-20250808YJEP 上用 paramiko 实连 master01 采的，
#    R 用配置里的 4.1.2，LD_LIBRARY_PATH 用配置里的 hdf5 路径。
#    13 个包全部加载成功。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

items <- list()
add <- function(id, label, status, detail, value = NULL) {
  items[[length(items) + 1L]] <<- list(id = id, label = label, status = status,
                                       detail = detail, value = value)
}

# ---- 机器 ----
add("mem", "内存", "ok", "实测 1510.3 GB（master01）", 1510.3)
add("cpu", "CPU", "ok", "64 核", 64)
add("disk", "/share 剩余", "ok", "19273.1 GB 可用", 19273.1)

# ---- R 与库 ----
add("r_version", "R 版本", "ok",
    "R scripting front-end version 4.1.2 (2021-11-01)", "4.1.2")
add("rscript_configured", "配置里的 Rscript", "ok",
    "/share/apps/R/R-4.1.2-4/bin/Rscript 存在且可执行",
    "/share/apps/R/R-4.1.2-4/bin/Rscript")
add("rlib", "R 库路径", "ok",
    "/share/apps/R/R-4.1.2-4/lib64/R/library 存在，1290 个包",
    1290L)
add("ld_library_path", "动态库路径", "ok",
    "设置后包才能加载（不设会报 Rcpp/dyn.load 失败）",
    "/share/apps/virtualEnv/miniconda3-2024-6-5/pkgs/hdf5-1.14.3-nompi_h2d575fe_109/lib")

# ---- 包：13 个全部加载成功 ----
pkgs <- list(
  Seurat = "4.3.0", DoubletFinder = "2.0.3", harmony = "1.2.0",
  SoupX = "1.6.2", presto = "1.0.0", clusterProfiler = "4.2.2",
  org.Mm.eg.db = "3.14.0", ggplot2 = "3.5.2", pheatmap = "1.0.13",
  ggpubr = "0.6.0", Matrix = "1.5.3", dplyr = "1.1.2", patchwork = "1.3.2"
)
for (nm in names(pkgs)) add(paste0("pkg_", nm), nm, "ok", paste0("v", pkgs[[nm]]), pkgs[[nm]])

# ---- 路径 ----
add("data_root", "数据目录", "ok", "存在，7 个样本链接，0 个断链", 7L)
add("script_root", "脚本目录", "ok",
    "存在，含 matrix / reference / result / script", 4L)
add("work_root", "工作目录", "ok",
    "存在，含 1.doublet~10.figs 与 9.annot 等产物目录", 18L)
add("links", "样本链接", "ok", "7 个链接全部可解析，0 个断链", 7L)

n_ok <- sum(vapply(items, function(x) identical(x$status, "ok"), TRUE))
n_warn <- sum(vapply(items, function(x) identical(x$status, "warn"), TRUE))
n_fail <- sum(vapply(items, function(x) identical(x$status, "fail"), TRUE))

payload <- list(
  ok = TRUE,
  verdict = if (n_fail > 0L) "blocked" else if (n_warn > 0L) "warn" else "ready",
  verdict_cn = if (n_fail > 0L) "服务器有必需项缺失，先补齐"
               else if (n_warn > 0L) "服务器能跑，有几处要留意"
               else "服务器环境齐备，可以跑",
  counts = list(ok = n_ok, warn = n_warn, fail = n_fail),
  items = items,
  target = "server hhhong1@10.1.1.2 (master01)",
  pkgs_ok = length(pkgs), pkgs_total = length(pkgs),
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
  checked_from = "PC-20250808YJEP",
  probe = list(
    how = "paramiko over SSH from PC-20250808YJEP",
    rscript = "/share/apps/R/R-4.1.2-4/bin/Rscript",
    rlib = "/share/apps/R/R-4.1.2-4/lib64/R/library",
    ld_library_path = "/share/apps/virtualEnv/miniconda3-2024-6-5/pkgs/hdf5-1.14.3-nompi_h2d575fe_109/lib",
    note = paste0("第一版探针用 PATH 上的 Rscript（3.6.0）报包全缺，",
                  "第二版用对 R 但漏 LD_LIBRARY_PATH 报 dyn.load 失败，",
                  "第三版三者齐备，13 包全通过。")
  )
)

f <- "state/remote_env.json"
writeLines(jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE,
                            null = "null", digits = NA), f, useBytes = TRUE)
cat("写入", f, "\n")
cat("verdict:", payload$verdict, "| ok/warn/fail:", n_ok, n_warn, n_fail,
    "| 包:", length(pkgs), "/", length(pkgs), "\n")
