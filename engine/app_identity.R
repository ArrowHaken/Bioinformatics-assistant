# 应用身份名（app id）
#
#   这个值不是显示名，是网关里的路由键。`/_gateway/me?app=<它>` 据此判断
#   「问的是哪一台应用的身份」。同一个名字在全网关只能对应一台实例。
#
# 为什么必须从本机配置读，不能写死在代码里（这条是踩出来的）：
#   2026-11-20 的故障，三个文件把名字写死成了 personal-analysis-assistant。
#   那个名字在网关里是另一台实例的注册名：合法、可达、HTTP 200、结构完整。
#   于是网关照常回话，回的是另一个应用域的访问者。这份身份落到本机记录上，
#   与任何 owner 都对不上，record_access 按 fail-closed 处理，记录接口全 403。
#   页面能开、静态资源全 200、日志无报错，看着像坏了，其实是问错了人。
#   同一份代码要发到多台机器，名字必须各归各的，所以只能读本机。

PA_GATEWAY_ORIGIN_DEFAULT <- "https://artifact.catsco.cc"

pa_identity_env <- function(key) {
  v <- Sys.getenv(key, unset = "")
  if (!is.character(v) || length(v) != 1L || is.na(v)) return("")
  trimws(v)
}

# connector.json 可能在的位置。PA_ARTIFACTS_DIR 优先，用于非标准部署。
pa_identity_artifact_dirs <- function() {
  home <- Sys.getenv("HOME", unset = "")
  cands <- c(
    pa_identity_env("PA_ARTIFACTS_DIR"),
    "/opt/xiaoba-cloud-bot/state/artifacts",
    if (nzchar(home)) file.path(home, ".local", "share", "catsco", "cloud-html-artifact", "artifacts") else "",
    if (nzchar(home)) file.path(home, "AppData", "Local", "catsco", "cloud-html-artifact", "artifacts") else ""
  )
  cands <- cands[nzchar(cands)]
  unique(cands[dir.exists(cands)])
}

# 读所有 connector.json，只取 appId 与 localPort。
#   不读 identityFile / known_hosts 这类东西：它们与本函数无关，也不该出现在日志里。
pa_identity_connectors <- function() {
  dirs <- pa_identity_artifact_dirs()
  out <- list()
  for (d in dirs) {
    files <- list.files(d, pattern = "^connector\\.json$", recursive = TRUE, full.names = TRUE)
    for (f in files) {
      c <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
      if (!is.list(c) || is.null(c$appId)) next
      app <- trimws(as.character(c$appId[[1]]))
      if (!nzchar(app)) next
      port <- suppressWarnings(as.integer(c$localPort[[1]]))
      out[[length(out) + 1L]] <- list(app = app, port = port, file = f)
    }
  }
  out
}

# 当前监听端口。serve.R 里是 PORT；独立维护脚本没有它时读环境变量。
#
# 为什么要找两处：主进程用 Rscript 跑 serve.R，PORT 落在 .GlobalEnv；
# 而请求 worker（engine/server_worker.R）把 serve.R source 进一个私有环境，
# PORT 只存在于那个环境里，.GlobalEnv 中根本没有。旧实现只看 .GlobalEnv，
# 于是 worker 永远解析不出端口，也就解析不出应用身份名；记录接口全部按
# fail-closed 回 403「身份或数据库不可用」——页面能开、静态资源 200、
# 日志无报错，只有新建分析失败。
# 所以这里同时看 .GlobalEnv 和本函数的定义环境（即 serve.R 被 source 进去的那个）。
pa_identity_port <- function(port = NULL) {
  if (!is.null(port) && length(port) == 1L && !is.na(port)) return(as.integer(port))
  for (env in list(.GlobalEnv, environment())) {
    p <- get0("PORT", envir = env, inherits = TRUE)
    if (!is.null(p) && is.numeric(p) && length(p) == 1L && !is.na(p)) return(as.integer(p))
  }
  p <- suppressWarnings(as.integer(pa_identity_env("PA_LOCAL_PORT")))
  if (is.na(p)) NULL else p
}

# 解析本机应用身份名。取不到返回 NA，绝不猜。
#   为什么宁可不猜：
#     猜错的表现就是这次故障 —— 一切都"成功"，只是成功地在问别人的身份。
#     拿不到名字时应当明确退化成「身份不可用」，让记录接口按 fail-closed 拦住，
#     同时在启动日志里留下看得见的一行。宁可拒绝服务，不给错的人开门。
pa_app_id <- local({
  cached <- NULL
  function(port = NULL) {
    if (!is.null(cached)) return(cached)

    forced <- pa_identity_env("PA_APP_ID")
    if (nzchar(forced)) return(cached <<- forced)

    items <- pa_identity_connectors()
    if (!length(items)) return(NA_character_)

    p <- pa_identity_port(port)
    if (!is.null(p)) {
      hit <- Filter(function(x) !is.na(x$port) && identical(x$port, as.integer(p)), items)
      if (length(hit) == 1L) return(cached <<- hit[[1]]$app)
      # 同端口命中多条说明部署本身有歧义，此时任何选择都可能是别人的名字。
      if (length(hit) > 1L) {
        apps <- unique(vapply(hit, function(x) x$app, character(1)))
        if (length(apps) == 1L) return(cached <<- apps[1])
        return(NA_character_)
      }
    }
    # 端口对不上就停在这里。不使用「只有一个就用它」这种兜底：
    #   一台机器上常常同时挂着多个应用，唯一的那条未必是自己。
    NA_character_
  }
})

pa_gateway_origin <- function() {
  v <- pa_identity_env("PA_GATEWAY_ORIGIN")
  if (nzchar(v)) return(sub("/+$", "", v))
  PA_GATEWAY_ORIGIN_DEFAULT
}

# 身份端点地址。拿不到 app id 时返回 NA，调用方据此跳过问询而不是乱问。
pa_gateway_me_url <- function() {
  app <- pa_app_id()
  if (is.na(app) || !nzchar(app)) return(NA_character_)
  paste0(pa_gateway_origin(), "/_gateway/me?app=", utils::URLencode(app, reserved = TRUE))
}

# 启动自检：把「名字对不对」变成开机就能看见的一行。
#   ok      名字拿到了，且网关认这个名
#   unknown 网关不认识这个名字
#   missing 本机没配名字
#   unreachable 网关问不到（网络问题，不阻断启动）
pa_identity_selfcheck <- function(timeout = 3) {
  app <- pa_app_id()
  if (is.na(app) || !nzchar(app)) {
    return(list(ok = FALSE, status = "missing", app_id = NA_character_,
                detail = "没有拿到本机应用身份名：设 PA_APP_ID，或确认 connector.json 的 localPort 等于监听端口"))
  }
  if (!requireNamespace("curl", quietly = TRUE)) {
    return(list(ok = TRUE, status = "unverified", app_id = app, detail = "没有 curl 包，跳过网关校验"))
  }
  got <- tryCatch({
    h <- curl::new_handle(timeout = timeout, connecttimeout = timeout)
    r <- curl::curl_fetch_memory(pa_gateway_me_url(), handle = h)
    list(code = r$status_code, body = rawToChar(r$content))
  }, error = function(e) list(error = conditionMessage(e)))

  if (!is.null(got$error)) {
    return(list(ok = TRUE, status = "unreachable", app_id = app, detail = got$error))
  }
  if (!identical(as.integer(got$code), 200L)) {
    return(list(ok = FALSE, status = "unknown", app_id = app,
                detail = paste0("网关返回 ", got$code, "：", substr(got$body, 1, 120))))
  }
  j <- tryCatch(jsonlite::fromJSON(got$body, simplifyVector = FALSE), error = function(e) NULL)
  echoed <- if (is.list(j) && !is.null(j$app_id)) as.character(j$app_id[[1]]) else ""
  if (nzchar(echoed) && !identical(echoed, app)) {
    return(list(ok = FALSE, status = "mismatch", app_id = app,
                detail = paste0("本机是 ", app, "，网关回的是 ", echoed)))
  }
  list(ok = TRUE, status = "ok", app_id = app, detail = "网关认这个名字")
}
