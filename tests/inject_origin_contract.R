#!/usr/bin/env Rscript
# 注入首行契约：按钮名必须随入队一起落盘，并且补发时能重建首行。
#
# 为什么必须有这条（2026-11-23，云准助手交接件指出、本机核实）：
#   服务端 prepare 生成指令包时已经把按钮名算出来了，写进了正文首行，
#   却没有传给 inject_enqueue —— 于是队列条目 origin 字段为空。
#   后果不是"少一个字段"：补发时 inject_with_origin 拿不到按钮名，
#   重建不出 【按钮名】 首行；而正文里那行前缀又因为文本比对不进来
#   不走合并分支，同一条指令在两条路径下首行不一样。
#   实测形态：待发注入 23 条里 22 条 origin 为空。
#
# 用法：
#   Rscript tests/inject_origin_contract.R [app_dir]
#
# 判据：
#   1) 静态：reason="prepared" 的每一处 inject_enqueue 都必须传 origin
#   2) 动态：带 origin 入队，origin 字段落盘、正文首行是 【按钮名】
#   3) 动态：inject_with_origin 幂等，已带同前缀不重复加
#   4) 动态：补发时用存下的 origin 能重建首行（无 origin 就重建不出）

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

args <- commandArgs(trailingOnly = TRUE)
app_src <- normalizePath(if (length(args)) args[[1]] else ".", mustWork = TRUE)
main_file <- file.path(app_src, "engine", "server_main.R")
stopifnot(file.exists(main_file))

fail <- character()
check <- function(label, ok) {
  cat(if (isTRUE(ok)) "PASS:" else "FAIL:", label, "\n")
  if (!isTRUE(ok)) fail <<- c(fail, label)
}

# ---- 1) 静态：每处入队都要么传 origin，要么是注明的例外 --------------
#   为什么从"只查 prepared"扩到全量（2026-11-23）：
#     只查一类，等于给别的路径留了口子。实测就是漏了 run_enqueue 的决策注入，
#     它只在派工被「先定一件事」拦住时才触发，平时走不到，一直没被看见。
#     现在把两个文件里的每一处 inject_enqueue 都扫一遍。
scan_origin <- function(path) {
  lines <- readLines(path, warn = FALSE)
  calls <- grep("inject_enqueue\\(", lines)
  calls <- calls[!grepl("<- function", lines[calls])]
  # 唯一合法例外：/api/inject 通用队列。origin 由调用方在 body 里给
  # （前端拼按钮名），服务端这层没有可推断的按钮名，编一个就是造假。
  whitelist <- grep('text = as\\.character\\(b\\$text %\\|\\|% ""\\)', lines)
  missed <- integer(0)
  for (i in calls) {
    win <- seq.int(i, min(length(lines), i + 20L))
    if (any(grepl("origin\\s*=", lines[win]))) next
    if (length(whitelist) && any(abs(whitelist - i) <= 2L)) next
    missed <- c(missed, i)
  }
  missed
}
missed <- c(scan_origin(main_file), scan_origin(file.path(app_src, "engine", "run_enqueue.R")))
check("every inject_enqueue passes origin or is a documented exception",
      length(missed) == 0L)
if (length(missed))
  cat("  漏传 origin 的调用在第", paste(missed, collapse = ", "), "行\n")

# ---- 桩环境：只加载定义，不跑 CLI --------------------------------
root <- tempfile("inject-origin-")
dir.create(root)
on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
dir.create(file.path(root, "state"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(root, "config"), showWarnings = FALSE, recursive = TRUE)
# serve.R 会按自己的位置找 engine/server_main.R，所以临时根里要有这两个链接。
stopifnot(file.symlink(file.path(app_src, "engine"), file.path(root, "engine")))
stopifnot(file.symlink(file.path(app_src, "serve.R"), file.path(root, "serve.R")))

e <- new.env(parent = globalenv())
e$APP_DIR <- root
e$STATE_DIR <- file.path(root, "state")
e$HAS_SESSIONS <- FALSE
e$DB_READY <- FALSE
e$PORT <- 18999
old <- options(pa.serve.definitions_only = TRUE, pa.app_dir = root)
sys.source(file.path(root, "serve.R"), envir = e)
options(old)
e$APP_DIR <- root
e$STATE_DIR <- file.path(root, "state")

# ---- 2) 带 origin 入队 -------------------------------------------
txt <- "请按原样重跑这一环。"
it <- tryCatch(e$inject_enqueue(text = txt, session = "s-t1", reason = "prepared",
                                origin = "按原样重跑这一环"),
               error = function(err) list(err = conditionMessage(err)))
if (!is.null(it$err)) {
  check("inject_enqueue accepts origin", FALSE)
  cat("  错误：", it$err, "\n")
} else {
  check("origin is persisted on the queue item",
        identical(as.character(it$origin %||% ""), "按原样重跑这一环"))
  first <- strsplit(as.character(it$text %||% ""), "\n", fixed = TRUE)[[1]][1]
  check("first line is the button name", identical(first, "【按原样重跑这一环】"))
}

# ---- 3) 幂等：已带前缀不重复加 -----------------------------------
twice <- e$inject_with_origin(as.character(it$text %||% txt), "按原样重跑这一环")
check("inject_with_origin is idempotent",
      identical(twice, as.character(it$text %||% txt)))

# ---- 4) 补发路径：有 origin 才能重建首行 --------------------------
bare <- "请按原样重跑这一环。"
rebuilt <- tryCatch(e$inject_with_origin(bare, as.character(it$origin %||% "")),
                    error = function(err) bare)
reb_first <- strsplit(as.character(rebuilt), "\n", fixed = TRUE)[[1]][1]
check("resend can rebuild the first line from stored origin",
      identical(reb_first, "【按原样重跑这一环】"))

no_origin <- tryCatch(e$inject_with_origin(bare, ""), error = function(err) bare)
no_first <- strsplit(as.character(no_origin), "\n", fixed = TRUE)[[1]][1]
check("without origin the first line cannot be rebuilt",
      !identical(no_first, "【按原样重跑这一环】"))

# ---- 5) 先无按钮名入队、后带按钮名重试：必须合并成一条并升级标题 ----
#   这是云准助手举的反例（inj-20261122190603-ggxf）在本机的同型：
#   按全文比对就不相等，于是新建一条；旧那条 origin 空、首行没前缀，照旧补发。
want <- "【只读提问】这份结果怎么看？"
body <- "这份结果怎么看？"
a1 <- e$inject_enqueue(text = body, session = "s-t2", reason = "prepared")   # 无 origin
a2 <- e$inject_enqueue(text = body, session = "s-t2", reason = "prepared",
                       origin = "只读提问")                                  # 带 origin
same_id <- identical(as.character(a1$id %||% ""), as.character(a2$id %||% ""))
all_items <- tryCatch(e$inject_read()$items, error = function(err) list())
n_same_body <- sum(vapply(all_items, function(x)
  identical(e$inject_body_of(x$text), body), TRUE))
check("retry with a button name merges into the same item instead of adding one",
      same_id && n_same_body == 1L)
check("merged item keeps the button name", identical(as.character(a2$origin %||% ""), "只读提问"))
check("merged item upgrades its first line",
      identical(strsplit(as.character(a2$text %||% ""), "\n", fixed = TRUE)[[1]][1],
                "【只读提问】"))

if (length(fail)) {
  cat("\nFAILED:", paste(fail, collapse = "; "), "\n")
  quit(save = "no", status = 1L)
}
cat("\ninject origin contract\n")