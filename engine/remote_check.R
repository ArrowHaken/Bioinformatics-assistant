#!/usr/bin/env Rscript
# =====================================================================
#  remote_check.R -- run the environment check on the configured server
#
#  WHY THIS EXISTS
#    The app may not be running on the machine that can see the analysis
#    server's disk. But it IS given the host, the account and the password
#    in the place config, so it can reach the server over SSH and ask the
#    server about itself.
#
#    The point is that nothing here is hardcoded: change the host in the
#    page and this checks the new host. Previous version imported a
#    one-off result from a script run by hand -- that would break the
#    moment the server changed.
#
#  WHAT IT DOES NOT DO
#    It does not pretend to succeed. If the connection is refused or the
#    password is wrong, it says so and returns no verdict. Writing a
#    plausible-looking but unverified result is worse than writing none.
#
#  USAGE
#    Rscript remote_check.R --host <ip> --user <name> --password <pw> \
#            [--port 22] [--out <json>] [--data <path>] [--script <path>]
#
#  OUTPUT
#    JSON on stdout (and to --out when given) shaped like env_check.json:
#    verdict / verdict_cn / counts / items[], plus target and error.
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

args <- commandArgs(trailingOnly = TRUE)
get_opt <- function(k, d = NULL) {
  i <- match(k, args)
  if (is.na(i) || i == length(args)) return(d)
  args[i + 1]
}

HOST <- get_opt("--host", "")
USER <- get_opt("--user", "")
# 密码优先从环境变量取：命令行参数会出现在 ps 的输出里，
#   这台机器上任何进程都看得到。--password 只作为手工调试的备选。
PWD  <- Sys.getenv("SC_REMOTE_PWD", unset = "")
if (!nzchar(PWD)) PWD <- get_opt("--password", "")
PORT <- get_opt("--port", "22")
OUT  <- get_opt("--out", NULL)
DATA <- get_opt("--data", "")
SCR  <- get_opt("--script", "")

# 用哪个 R、哪个库、哪条动态库路径（2026-11-02 真机实测后补）
#
#  为什么必须显式传：真机上 PATH 上的 `Rscript` 是 R 3.6.0，
#  而分析包全装在 R 4.1.2 的库里。用默认 Rscript 去 library()，
#  十个包会稳定报「there is no package called ...」——
#  人会照着去装一遍，装完还是报缺。
#
#  第二个坑：就算用对了 R 4.1.2，不设 LD_LIBRARY_PATH 仍然全部加载失败，
#  报 dyn.load / "package 'Rcpp' could not be loaded"。实测补上 hdf5 那条
#  动态库路径之后，13 个包全部 OK。
#
#  这两个坑合起来的教训：体检不能只问「包在不在」，
#  必须先声明「用哪个解释器、哪条库路径、哪条动态库路径」，
#  并且把这三样回显出来，否则「包缺」和「找错了 R」在结论上长得一模一样。
RSCRIPT <- get_opt("--rscript", "")
RLIB    <- get_opt("--rlib", "")
LDPATH  <- get_opt("--ld", "")

# 拼一个前缀：什么都没传就退回默认 Rscript（保持老行为，但会回显用的是默认）
r_prefix <- {
  parts <- character(0)
  if (nzchar(RLIB))   parts <- c(parts, paste0("R_LIBS_SITE=", RLIB), paste0("R_LIBS_USER=", RLIB))
  if (nzchar(LDPATH)) parts <- c(parts, paste0("LD_LIBRARY_PATH=", LDPATH, ":$LD_LIBRARY_PATH"))
  paste(parts, collapse = " ")
}
r_bin <- if (nzchar(RSCRIPT)) RSCRIPT else "Rscript"

emit <- function(obj) {
  txt <- jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  if (!is.null(OUT) && nzchar(OUT)) {
    dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
    con <- file(OUT, open = "wb")
    writeBin(charToRaw(enc2utf8(txt)), con)
    close(con)
  }
  cat(txt, "\n", sep = "")
}

fail <- function(reason, detail = "") {
  emit(list(ok = FALSE, error = reason, detail = as.character(detail),
            target = paste0("server ", HOST), items = list(),
            counts = list(ok = 0L, warn = 0L, fail = 1L)))
  quit(save = "no", status = 0L)
}

if (!nzchar(HOST)) fail("no_host", "place config has no server host")
if (!nzchar(USER)) fail("no_user", "place config has no server user")
if (!nzchar(PWD))  fail("no_password", "place config has no stored password")

sshpass <- Sys.which("sshpass")
ssh_bin <- Sys.which("ssh")
if (!nzchar(sshpass)) fail("no_sshpass", "sshpass is not installed on this machine")
if (!nzchar(ssh_bin)) fail("no_ssh", "ssh client is not installed on this machine")

# The remote probe is deliberately one plain command with KEY:VALUE lines:
#   no shell quoting gymnastics, and one round trip instead of ten.
# R 4.1.2 is what the analysis server carries, so the probe keeps to
# `awk`/`test`/`df` and does not assume anything newer.
pkg_list <- c("Seurat", "DoubletFinder", "harmony", "SoupX", "presto",
              "clusterProfiler", "org.Mm.eg.db", "ggplot2", "pheatmap", "ggpubr")
r_expr <- paste0(
  "p<-c(", paste(sprintf('"%s"', pkg_list), collapse = ","), ");",
  "for(x in p){",
  "r<-tryCatch({suppressMessages(suppressWarnings(library(x,character.only=TRUE)));\"OK\"},",
  "error=function(e) paste0(\"FAIL:\",substr(conditionMessage(e),1,70)));",
  "v<-tryCatch(as.character(packageVersion(x)),error=function(e)\"-\");",
  "cat(\"PKG\\t\",x,\"\\t\",v,\"\\t\",r,\"\\n\",sep=\"\")}"
)

remote <- paste(
  'echo "MEM_KB:$(awk \'/MemTotal/{print $2}\' /proc/meminfo)"',
  'echo "DISK_KB:$(df -Pk /share 2>/dev/null | awk \'NR==2{print $4}\')"',
  'echo "RVER:$(' + r_bin + ' --version 2>&1 | head -1)"',
  paste0('echo "RBIN:', if (nzchar(RSCRIPT)) RSCRIPT else "Rscript(PATH 默认)", '"'),
  paste0('echo "RLIB:', if (nzchar(RLIB)) RLIB else "(未指定)", '"'),
  paste0('echo "RLDPATH:', if (nzchar(LDPATH)) LDPATH else "(未指定)", '"'),
  'echo "RLIB_COUNT:$(ls -1 ', if (nzchar(RLIB)) RLIB else "/nonexistent", ' 2>/dev/null | wc -l)"',
  if (nzchar(DATA)) paste0('echo "DATA_EXISTS:$(test -d ', DATA, ' && echo 1 || echo 0)"') else 'echo "DATA_EXISTS:0"',
  if (nzchar(DATA)) paste0('echo "DATA_LINKS:$(ls -1 ', DATA, ' 2>/dev/null | wc -l)"') else 'echo "DATA_LINKS:0"',
  if (nzchar(SCR)) paste0('echo "SDIR_EXISTS:$(test -d ', SCR, ' && echo 1 || echo 0)"') else 'echo "SDIR_EXISTS:0"',
  if (nzchar(SCR)) paste0('echo "SDIR_LIST:$(ls -1 ', SCR, ' 2>/dev/null | tr \'\\n\' \',\')"') else 'echo "SDIR_LIST:"',
  'echo "PKGBEGIN"',
  paste0(r_prefix, if (nzchar(r_prefix)) " " else "", r_bin,
         " --vanilla -e '", r_expr, "' 2>&1 | grep '^PKG'"),
  'echo "PKGEND"',
  sep = "; "
)

# Password goes through the environment, not argv: argv is visible to
# every other process on this machine via ps.
old <- Sys.getenv("SSHPASS", unset = NA)
Sys.setenv(SSHPASS = PWD)
on.exit({
  if (is.na(old)) Sys.unsetenv("SSHPASS") else Sys.setenv(SSHPASS = old)
}, add = TRUE)

o <- tryCatch(
  suppressWarnings(system2(sshpass,
    c("-e", ssh_bin,
      "-o", "StrictHostKeyChecking=no",
      "-o", "UserKnownHostsFile=/dev/null",
      "-o", "ConnectTimeout=8",
      "-o", "BatchMode=no",
      "-p", PORT,
      paste0(USER, "@", HOST),
      shQuote(remote)),
    stdout = TRUE, stderr = TRUE)),
  error = function(e) structure(conditionMessage(e), status = 1L)
)
code <- attr(o, "status"); if (is.null(code)) code <- 0L

txt <- paste(o, collapse = "\n")
if (!identical(as.integer(code), 0L)) {
  low <- tolower(txt)
  reason <- if (grepl("connection refused", low)) "connection_refused"
    else if (grepl("timed out|timeout", low)) "timeout"
    else if (grepl("permission denied|authentication failed", low)) "auth_failed"
    else if (grepl("could not resolve|name or service not known", low)) "dns_failed"
    else "ssh_failed"
  fail(reason, substr(gsub("\\s+", " ", txt), 1, 300))
}

items <- list()
add <- function(id, label, status, detail, value = NULL) {
  items[[length(items) + 1L]] <<- list(id = id, label = label, status = status,
                                       detail = detail, value = value)
}

mem_kb <- NA_real_; disk_kb <- NA_real_; rver <- ""
data_exists <- FALSE; data_links <- 0L; sdir_exists <- FALSE; sdir_list <- character(0)
pkgs <- list(); in_pkg <- FALSE

for (line in strsplit(txt, "\n")[[1]]) {
  line <- sub("\r$", "", line)
  if (identical(line, "PKGBEGIN")) { in_pkg <- TRUE; next }
  if (identical(line, "PKGEND"))   { in_pkg <- FALSE; next }
  if (in_pkg && grepl("^PKG\t", line)) {
    f <- strsplit(line, "\t")[[1]]
    if (length(f) >= 4) {
      pkgs[[f[2]]] <- list(version = f[3], ok = identical(f[4], "OK"), note = f[4])
    }
    next
  }
  if (grepl("^MEM_KB:", line))  mem_kb <- suppressWarnings(as.numeric(sub("^MEM_KB:", "", line)))
  if (grepl("^DISK_KB:", line)) disk_kb <- suppressWarnings(as.numeric(sub("^DISK_KB:", "", line)))
  if (grepl("^RVER:", line))    rver <- trimws(sub("^RVER:", "", line))
  if (grepl("^DATA_EXISTS:", line)) data_exists <- identical(sub("^DATA_EXISTS:", "", line), "1")
  if (grepl("^DATA_LINKS:", line))  data_links <- suppressWarnings(as.integer(sub("^DATA_LINKS:", "", line)))
  if (grepl("^SDIR_EXISTS:", line)) sdir_exists <- identical(sub("^SDIR_EXISTS:", "", line), "1")
  if (grepl("^SDIR_LIST:", line)) {
    v <- sub("^SDIR_LIST:", "", line)
    sdir_list <- if (nzchar(v)) strsplit(v, ",", fixed = TRUE)[[1]] else character(0)
  }
}

# 内存：服务器是大机器，给 GB 更直观；读到就读，读不到不编
if (!is.na(mem_kb)) {
  gb <- round(mem_kb / 1024 / 1024, 1)
  st <- if (gb >= 32) "ok" else if (gb >= 12) "ok" else "warn"
  add("mem", "内存", st,
      sprintf("实测 %.1f GB%s", gb, if (gb < 32) "，偏小时会自动走分块路径（结果等价）" else ""), gb)
} else {
  add("mem", "内存", "warn", "读不到 /proc/meminfo")
}

if (!is.na(disk_kb)) {
  gb <- round(disk_kb / 1024 / 1024, 1)
  add("disk", "/share 剩余", if (gb >= 20) "ok" else "warn", sprintf("%.1f GB 可用", gb), gb)
}

if (nzchar(rver)) add("r_version", "R 版本", "ok", rver, rver)

if (nzchar(DATA)) {
  add("data_root", "数据目录", if (data_exists) "ok" else "fail",
      if (data_exists) sprintf("存在，%d 个样本子目录/链接", data_links) else "目录不存在",
      data_links)
}
if (nzchar(SCR)) {
  add("script_root", "脚本目录", if (sdir_exists) "ok" else "fail",
      if (sdir_exists) paste0("存在，含 ", paste(utils::head(sdir_list, 8), collapse = " / "))
      else "目录不存在",
      length(sdir_list))
}

n_ok <- 0L; n_fail <- 0L
for (nm in names(pkgs)) {
  p <- pkgs[[nm]]
  st <- if (isTRUE(p$ok)) "ok" else "fail"
  if (isTRUE(p$ok)) n_ok <- n_ok + 1L else n_fail <- n_fail + 1L
  add(paste0("pkg_", nm), nm, st,
      if (isTRUE(p$ok)) paste0("v", p$version) else paste0("加载失败：", p$note),
      p$version)
}

counts <- list(
  ok = sum(vapply(items, function(x) identical(x$status, "ok"), logical(1))),
  warn = sum(vapply(items, function(x) identical(x$status, "warn"), logical(1))),
  fail = sum(vapply(items, function(x) identical(x$status, "fail"), logical(1)))
)
verdict <- if (counts$fail > 0L) "blocked" else if (counts$warn > 0L) "warn" else "ready"
verdict_cn <- switch(verdict,
  ready = "服务器环境齐备，可以跑",
  warn = "服务器能跑，有几处要留意",
  blocked = "服务器有必需项缺失，先补齐")

emit(list(
  ok = TRUE,
  verdict = verdict, verdict_cn = verdict_cn, counts = counts, items = items,
  target = paste0("server ", USER, "@", HOST),
  pkgs_ok = n_ok, pkgs_total = length(pkgs),
  checked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
  checked_from = Sys.info()[["nodename"]]
))
quit(save = "no", status = 0L)
