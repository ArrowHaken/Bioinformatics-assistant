#!/usr/bin/env Rscript
# =====================================================================
#  code_write_check.R -- 助手改应用代码之前先问一次
#
#  为什么需要它
#    页面上的「改脚本」已经在 /api/script-* 那头按同一份配置拦过一次。
#    但助手自己也在这台机器上，可以直接 write_file / edit_file 改源码 ——
#    那条路不经过 HTTP，页面那道门管不到。
#    所以改代码之前必须由助手自己问一次，问的还是同一份
#    config/admin_policy.json，不能另立一套规矩。
#
#  用法（在 app 目录下）
#    Rscript tools/code_write_check.R --speaker gina --uid 784
#    Rscript tools/code_write_check.R --speaker someone --uid 9001
#
#  输出第一行固定是 ALLOW 或 DENY，后面是理由。
#  退出码：ALLOW=0，DENY=1。脚本只读配置、只追加审计，不改任何源码。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

argv <- commandArgs(trailingOnly = TRUE)
opt <- function(k, d = "") {
  i <- match(k, argv)
  if (is.na(i) || i >= length(argv)) return(d)
  trimws(argv[i + 1L])
}

app_dir <- tryCatch({
  f <- sub("^--file=", "", commandArgs(trailingOnly = FALSE)[
    grep("^--file=", commandArgs(trailingOnly = FALSE))])
  normalizePath(file.path(dirname(f[1]), ".."), winslash = "/", mustWork = TRUE)
}, error = function(e) normalizePath(getwd(), winslash = "/"))

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("DENY\n缺少 jsonlite，读不了权限配置；按不能改处理。\n")
  quit(save = "no", status = 1L)
}

policy_file <- file.path(app_dir, "config", "admin_policy.json")
msg <- ""
if (!file.exists(policy_file)) {
  cat("DENY\n没有 config/admin_policy.json，按「修改权限隔离开着」处理。\n")
  quit(save = "no", status = 1L)
}
pol <- tryCatch(jsonlite::fromJSON(paste(readLines(policy_file, warn = FALSE,
                                                 encoding = "UTF-8"), collapse = "\n"),
                                   simplifyVector = FALSE),
                error = function(e) NULL)
if (!is.list(pol)) {
  cat("DENY\n权限配置读不出来；按不能改处理。\n")
  quit(save = "no", status = 1L)
}

chr_vec <- function(x) {
  if (is.null(x)) return(character(0))
  v <- trimws(as.character(unlist(x, use.names = FALSE)))
  v[!is.na(v) & nzchar(v)]
}

speaker <- tolower(opt("--speaker"))
uid <- opt("--uid")
# 开关只认 JSON 里的逻辑真/假；写坏的配置一律当成「开着」（从严）。
write_isolation <- !identical(pol$write_isolation, FALSE)
is_admin <- (nzchar(uid) && uid %in% chr_vec(pol$admin_uids)) ||
  (nzchar(speaker) && speaker %in% tolower(chr_vec(pol$admin_usernames)))

verdict <- if (is_admin || !write_isolation) "ALLOW" else "DENY"
reason <- if (is_admin) {
  "请求者是应用所属机器人的主人（管理员）。"
} else if (!write_isolation) {
  "修改权限隔离已关闭；管理员放开给所有已识别用户。"
} else {
  paste0("修改权限隔离开着：只有管理员可以改应用代码。",
         "要让别人也能改，先由管理员在页面「权限管理」里关掉这一项。")
}

# 留痕：谁问的、问了什么、结论是什么。审计失败不影响结论本身。
tryCatch({
  f <- file.path(app_dir, "state", "admin_audit.jsonl")
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  cat(paste0(jsonlite::toJSON(list(action = "code_write_check",
                                   verdict = verdict, speaker = speaker,
                                   uid = uid, at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
                              auto_unbox = TRUE), "\n"),
      file = f, append = TRUE)
}, error = function(e) NULL)

cat(verdict, "\n", reason, "\n", sep = "")
quit(save = "no", status = if (identical(verdict, "ALLOW")) 0L else 1L)
