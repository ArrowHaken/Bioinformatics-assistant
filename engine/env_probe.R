#!/usr/bin/env Rscript
# =====================================================================
#  env_probe.R -- 平台能力探针：只回答「能不能无激活注入」这一件事
#
#  为什么需要单独一个文件
#    页面以前写过「定时已生效」这种话，依据是「我们有个重发队列」。
#    那是把「代码里存在重发逻辑」当成了「平台允许定时发消息」——
#    两件事完全不同，而且后者决定了能不能真的无人值守。
#    判断错的代价很具体：人关掉页面，以为它还会自己推进，实际停在原地。
#
#  三档，只有三档
#    server  平台允许无用户激活的注入（可以定时发）
#    manual  只允许点击时注入（页面要挂「待人工查询」计数，不能承诺自动）
#    unknown 探针没跑成（不写成功，也不写失败）
#
#  为什么 unknown 必须独立存在
#    探针失败时最常见的错误是「默认当成 server」，因为那是我们想要的结果。
#    但「没探到」和「探到了可以」在证据上完全不同：前者什么也没证明。
#    把未知写成已知，页面就会说一句没有依据的话。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

env_probe_path <- function(app_dir) file.path(app_dir, "state", "env_probe.json")

# probe_inputs：探针需要的可核事实，由调用方提供（不在本文件里猜）
#   has_sdk        页面里是否存在平台 SDK（由前端上报）
#   handshake_ok   宿主是否完成握手（由前端上报）
#   can_schedule   平台是否接受无激活注入（由一次真实注入的结果判定）
#   evidence       每一条的来源说明
env_probe_decide <- function(inputs) {
  ev <- as.character(inputs$evidence %||% character(0))
  if (is.null(inputs$has_sdk) || is.na(inputs$has_sdk))
    return(list(level = "unknown", why = "没有拿到 SDK 探测结果",
                evidence = ev))
  if (!isTRUE(inputs$has_sdk))
    return(list(level = "manual", why = "页面里没有平台 SDK：只能点击时注入",
                evidence = c(ev, "has_sdk = false")))
  if (!isTRUE(inputs$handshake_ok))
    return(list(level = "unknown",
                why = "SDK 在，但宿主没完成握手：既不能断言可以自动，也不能断言不行",
                evidence = c(ev, "handshake_ok = false（会话绑定未生效）")))
  if (is.null(inputs$can_schedule) || is.na(inputs$can_schedule))
    return(list(level = "unknown",
                why = "握手成功，但还没有一次「无激活注入」的实测结果",
                evidence = c(ev, "can_schedule = 未测")))
  if (isTRUE(inputs$can_schedule))
    return(list(level = "server", why = "实测无激活注入成功，可以定时发",
                evidence = c(ev, "can_schedule = true")))
  list(level = "manual", why = "实测无激活注入被拒：只能点击时注入",
       evidence = c(ev, "can_schedule = false"))
}

# 页面文案必须与档位一致。文案在这里生成，不在前端各写一份。
env_probe_message <- function(level) {
  switch(as.character(level),
    server = "平台允许无用户激活的注入：关掉页面后队列仍会按退避继续推进。",
    manual = "平台只允许点击时注入：关掉页面后不会自动推进，页面会挂「待人工查询」计数。",
    unknown = "还没能确认平台是否允许无激活注入。不承诺自动推进；要看进度请回到页面点一次查询。",
    "未知档位：按 unknown 处理，不承诺自动推进。")
}

env_probe_write <- function(app_dir, inputs) {
  d <- env_probe_decide(inputs)
  obj <- list(level = d$level, why = d$why, evidence = d$evidence,
              message = env_probe_message(d$level),
              at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
              rule_version = "env-probe-1.0")
  f <- env_probe_path(app_dir)
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, null = "null"),
             f, useBytes = TRUE)
  obj
}

env_probe_read <- function(app_dir) {
  f <- env_probe_path(app_dir)
  if (!file.exists(f)) return(NULL)
  tryCatch(jsonlite::fromJSON(paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                                    collapse = "\n"), simplifyVector = FALSE),
           error = function(e) NULL)
}
