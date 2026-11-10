#!/usr/bin/env Rscript
# =====================================================================
#  安装R依赖.R -- 一次性把分析要用的 R 包装齐
#
#  为什么要单独一个脚本：
#    这套流程依赖的包不只一个，而且分成「必需」和「可选」两类。
#    装的时候最容易出的问题是：装了半小时，最后发现漏了一个，
#    跑到一半才停。所以这里先把清单列出来，逐个装、逐个报结果，
#    最后给一张「装了什么 / 缺什么」的清单。
#
#  用法：双击这个文件（Windows），或命令行：
#    Rscript 安装R依赖.R
#
#  国内网络慢的话，脚本会自动用清华镜像。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

say <- function(...) { cat(enc2native(paste0(...)), "\n", sep = ""); flush(stdout()) }

# ---- 镜像：清华，国内下 R 包快很多 ----
mirror <- "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"
ok_mirror <- tryCatch({
  con <- url(mirror, open = "r"); close(con); TRUE
}, error = function(e) FALSE)
if (!ok_mirror) {
  say("[提示] 连不上清华镜像，改用官方源（可能较慢）。")
  mirror <- "https://cloud.r-project.org"
}
options(repos = c(CRAN = mirror))
say("R 包来源：", mirror)
say("")

# ---- 清单 ----
# 必需：主流程/出图缺了就跑不动
REQ <- c("Seurat", "Matrix", "dplyr", "ggplot2", "patchwork", "jsonlite", "ggrepel")
# 可选：只影响某一段
OPT <- list(
  list(p = "DoubletFinder",   why = "去双。缺了主流程会在「去双」这一步停"),
  list(p = "harmony",         why = "批次校正（多样本才有用）"),
  list(p = "presto",          why = "快速差异基因（缺了走慢速路径，结果一致）"),
  list(p = "org.Mm.eg.db",    why = "小鼠基因注释（富集用）"),
  list(p = "org.Hs.eg.db",    why = "人基因注释（富集用）"),
  list(p = "clusterProfiler", why = "GO/KEGG 富集。缺了差异富集段出不来"),
  list(p = "SoupX",           why = "环境 RNA 校正（可选）")
)

say("=====================================================")
say("  个性化分析助手 · R 依赖安装")
say("=====================================================")
say("")
say("这一步可能要 20~40 分钟，取决于网速。中途别关窗口。")
say("")

installed_ok <- character(0)
installed_fail <- character(0)

install_one <- function(p, required) {
  if (requireNamespace(p, quietly = TRUE)) {
    v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) "?")
    say(sprintf("  [已有] %-18s v%s", p, v))
    return(TRUE)
  }
  say(sprintf("  [安装] %-18s ...", p))
  r <- tryCatch({
    suppressWarnings(utils::install.packages(p, quiet = TRUE))
    requireNamespace(p, quietly = TRUE)
  }, error = function(e) FALSE)
  if (r) {
    v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) "?")
    say(sprintf("    [成功] v%s", v))
    installed_ok <<- c(installed_ok, p)
    TRUE
  } else {
    say(sprintf("    [失败] %s%s", p, if (required) "  ← 这个是必需的" else "（可选，不影响主流程）"))
    installed_fail <<- c(installed_fail, p)
    FALSE
  }
}

say("---- 必需包 ----")
for (p in REQ) install_one(p, TRUE)

say("")
say("---- 可选包 ----")
for (o in OPT) install_one(o$p, FALSE)

# ---- 总结 ----
say("")
say("=====================================================")
say("  安装结果")
say("=====================================================")
missing_req <- REQ[!vapply(REQ, function(p) requireNamespace(p, quietly = TRUE), logical(1))]
missing_opt <- vapply(OPT, function(o) !requireNamespace(o$p, quietly = TRUE), logical(1))
miss_opt_names <- vapply(OPT[missing_opt], function(o) o$p, character(1))

if (!length(missing_req)) {
  say("必需包：齐全 ✓")
} else {
  say("必需包：还缺 ", paste(missing_req, collapse = ", "))
  say("  → 这几项缺着，主流程跑不起来。检查一下网络，或换个时间再试。")
}
if (!length(miss_opt_names)) {
  say("可选包：齐全 ✓")
} else {
  say("可选包：还缺 ", paste(miss_opt_names, collapse = ", "))
  for (o in OPT) {
    if (!requireNamespace(o$p, quietly = TRUE)) say("  - ", o$p, "：", o$why)
  }
}
say("")
say("装完之后，回到「启动助手-双击这个.bat」重新启动，页面上会看到体检结果。")
say("")
say("按回车关闭本窗口。")
invisible(readLines(file("stdin"), n = 1))
