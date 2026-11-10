#!/usr/bin/env Rscript
# =====================================================================
#  agent_gate.R -- 助手代写数据的那道门
#
#  为什么需要这个文件
#    助手和人的浏览器不是同一个身份来源：
#      · 人从平台进来，带 cookie，网关换出 viewer:xxx；
#      · 助手在本机发请求，没有 cookie，被兜底成 local:default。
#    于是助手写进去的记录，人在页面上看不见。这不是 bug，是两条身份链
#    各走各的。要让他们共享同一批记录，只能由人显式授权一次，
#    把「助手」接到「某个人」的 owner_key 上。
#
#  为什么是令牌而不是让人把 cookie 给助手
#    cookie 是平台长期凭据，交给任何本地进程都等于交出整个账号。
#    这里发的是应用自己的短凭据：只能写记录，不能登录、不能读别的应用，
#    可以随时撤销，且撤销后立即失效。
#
#  为什么只存哈希
#    配置文件会被备份、被打包、被复制。存明文等于每次复制都泄露一次。
#
#  这个文件只管三件事：发牌、验牌、收牌。
#  它不碰 sessions.json，也不碰数据库 —— 那是调用方的事。
# =====================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

AGENT_TOKEN_FILE <- function(app_dir) file.path(app_dir, "config", "agent_tokens.json")
AGENT_AUDIT_FILE <- function(app_dir) file.path(app_dir, "state", "agent_audit.jsonl")
AGENT_TOKEN_PREFIX <- "pa_agent_"

# ---- 小工具 ---------------------------------------------------------

agent_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

agent_sha256 <- function(x) {
  if (requireNamespace("digest", quietly = TRUE))
    return(paste0(digest::digest(x, algo = "sha256", serialize = FALSE)))
  # 没有 digest 也不能退化成明文比对：
  #   退化的后果是「看起来有校验」，实际是把令牌原文当哈希存。
  stop("缺少 digest 包，无法安全处理令牌")
}

agent_rand_hex <- function(n_bytes = 32L) {
  # sample() 用的是 R 的伪随机源，令牌不能用它。
  #   这里只要能拿到系统熵就用 openssl，拿不到就明确失败 ——
  #   静默降级到伪随机会让「猜令牌」从不可行变成可行。
  if (!requireNamespace("openssl", quietly = TRUE))
    stop("缺少 openssl 包，无法生成安全令牌")
  paste0(as.character(openssl::rand_bytes(n_bytes)), collapse = "")
}

# 令牌原文只在签发那一刻出现一次，之后只有哈希。
agent_new_token <- function() paste0(AGENT_TOKEN_PREFIX, agent_rand_hex(24L))

agent_token_id <- function(token) substr(agent_sha256(token), 1L, 16L)

# ---- 读写令牌表 -----------------------------------------------------

agent_tokens_read <- function(app_dir) {
  f <- AGENT_TOKEN_FILE(app_dir)
  if (!file.exists(f)) return(list(tokens = list()))
  obj <- tryCatch(
    jsonlite::fromJSON(paste(readLines(f, warn = FALSE), collapse = "\n"),
                       simplifyVector = FALSE),
    error = function(e) NULL)
  if (is.null(obj) || !is.list(obj)) return(list(tokens = list()))
  if (is.null(obj$tokens) || !is.list(obj$tokens)) obj$tokens <- list()
  obj
}

agent_tokens_write <- function(app_dir, obj) {
  f <- AGENT_TOKEN_FILE(app_dir)
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  txt <- jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null", pretty = TRUE,
                          digits = NA)
  con <- file(f, open = "wb")
  writeBin(charToRaw(enc2utf8(as.character(txt))), con)
  close(con)
  # 只有属主可读。配置文件目录里还有服务器口令，权限统一收紧。
  try(Sys.chmod(f, mode = "0600"), silent = TRUE)
  invisible(TRUE)
}

agent_audit <- function(app_dir, action, detail = list()) {
  f <- AGENT_AUDIT_FILE(app_dir)
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  row <- c(list(at = agent_now(), action = action), detail)
  line <- tryCatch(
    as.character(jsonlite::toJSON(row, auto_unbox = TRUE, null = "null", digits = NA)),
    error = function(e) NULL)
  if (is.null(line)) return(invisible(FALSE))
  con <- file(f, open = "a")
  writeLines(enc2utf8(line), con, useBytes = TRUE)
  close(con)
  invisible(TRUE)
}

# ---- 发牌 -----------------------------------------------------------

# 只接受「已经从平台确认过的人」来发牌。
#   为什么不在本机回环上也允许发：那条路上谁都自称是用户，
#   发出去的令牌就等于把所有记录开放给任何本地进程。
#   owner_key 必须是 viewer: 或 topic: 开头，local:default 一律拒绝。
agent_token_issue <- function(app_dir, owner_key, note = "", ttl_days = 30L) {
  ok <- !is.na(owner_key %||% NA_character_) &&
    grepl("^(viewer|topic):", as.character(owner_key))
  if (!ok) {
    return(list(ok = FALSE, code = "owner_not_authenticated",
      error = paste0("只有已确认身份的用户可以授权助手写入；",
                     "当前身份是 ", as.character(owner_key %||% "未识别"),
                     "。请从平台会话里打开这个应用后再点授权。")))
  }
  ttl_days <- suppressWarnings(as.integer(ttl_days))
  if (is.na(ttl_days) || ttl_days < 1L || ttl_days > 365L) ttl_days <- 30L

  token <- agent_new_token()
  obj <- agent_tokens_read(app_dir)
  rec <- list(
    id = agent_token_id(token),
    hash = agent_sha256(token),
    owner_key = as.character(owner_key),
    note = as.character(note %||% ""),
    created_at = agent_now(),
    expires_at = format(Sys.time() + ttl_days * 86400, "%Y-%m-%dT%H:%M:%S"),
    revoked_at = NULL,
    last_used_at = NULL
  )
  obj$tokens[[length(obj$tokens) + 1L]] <- rec
  agent_tokens_write(app_dir, obj)
  agent_audit(app_dir, "issue", list(token_id = rec$id, owner_key = rec$owner_key,
                                     expires_at = rec$expires_at))
  # Handoff credentials are owner-specific and created under restrictive umask.
  hf <- agent_handoff_file(app_dir, owner_key)
  old_umask <- Sys.umask("0077")
  tryCatch({
    dir.create(dirname(hf), showWarnings = FALSE, recursive = TRUE, mode = "0700")
    ho <- list(token = token, token_id = rec$id, owner_key = rec$owner_key,
               issued_at = rec$created_at, expires_at = rec$expires_at)
    tmp <- tempfile("handoff-", tmpdir = dirname(hf))
    on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
    writeLines(as.character(jsonlite::toJSON(ho, auto_unbox = TRUE, null = "null")), tmp)
    Sys.chmod(tmp, "0600")
    if (!file.rename(tmp, hf)) stop("handoff_write_failed")
  }, error = function(e) {
    agent_token_revoke(app_dir, rec$id, owner_key)
    stop("令牌交接失败，已撤销新令牌")
  }, finally = Sys.umask(old_umask))
  # 原文只在这里返回一次。
  list(ok = TRUE, token = token, id = rec$id, owner_key = rec$owner_key,
       expires_at = rec$expires_at)
}

# 交接文件路径。助手直接读这个文件即可，不需要任何接口。
agent_handoff_file <- function(app_dir, owner_key) {
  if (length(owner_key) != 1L || is.na(owner_key) || !nzchar(owner_key))
    stop("handoff_owner_required")
  file.path(app_dir, "state", "agent_handoffs", paste0(agent_sha256(owner_key), ".json"))
}

# Return only the non-secret path. An agent can reuse, never mint authorization.
agent_handoff_ensure <- function(app_dir, owner) {
  key <- owner$key %||% NA_character_
  if (length(key) != 1L || is.na(key) || !grepl("^(viewer|topic):.+$", key))
    return(list(ok = FALSE, code = "human_identity_required"))
  hf <- agent_handoff_file(app_dir, key)
  ho <- tryCatch(jsonlite::fromJSON(hf), error = function(e) NULL)
  valid <- if (is.list(ho)) agent_token_check(app_dir, ho$token) else list(ok = FALSE)
  if (isTRUE(valid$ok) && identical(valid$owner_key, key) &&
      identical(as.character(ho$owner_key), key) &&
      (agent_human_owner(owner) ||
       (identical(owner$kind, "agent") &&
        identical(as.character(owner$external_id %||% ""), valid$token_id))))
    return(list(ok = TRUE, path = normalizePath(hf, mustWork = TRUE)))
  if (!agent_human_owner(owner))
    return(list(ok = FALSE, code = "human_identity_required"))
  issued <- agent_token_issue(app_dir, key, note = "保存并识别：体检回写授权")
  if (!isTRUE(issued$ok)) return(list(ok = FALSE, code = issued$code))
  list(ok = TRUE, path = normalizePath(hf, mustWork = TRUE))
}

# ---- 验牌 -----------------------------------------------------------

# 返回 list(ok, owner_key, token_id, code)
#   失败时给稳定的 code，让调用方能区分「没带令牌」和「令牌无效」。
#   这两种的处置完全不同：前者退回原来的身份链，后者必须直接拒绝，
#   不能悄悄降级成 local:default —— 那等于无效令牌被当成有效。
agent_token_check <- function(app_dir, token) {
  if (is.null(token) || !nzchar(as.character(token)))
    return(list(ok = FALSE, code = "no_token"))
  token <- trimws(as.character(token))
  if (!startsWith(token, AGENT_TOKEN_PREFIX))
    return(list(ok = FALSE, code = "token_format"))
  want <- agent_sha256(token)
  obj <- agent_tokens_read(app_dir)
  for (rec in obj$tokens) {
    if (!identical(as.character(rec$hash %||% ""), want)) next
    if (nzchar(as.character(rec$revoked_at %||% "")))
      return(list(ok = FALSE, code = "token_revoked"))
    exp <- as.character(rec$expires_at %||% "")
    if (nzchar(exp)) {
      # 过期的令牌不能靠字符串比较判：时区与格式都可能不同。
      #   解析失败按过期处理，宁可让人重新授权，不放行一个看不懂的日期。
      t <- tryCatch(as.POSIXct(exp, format = "%Y-%m-%dT%H:%M:%S", tz = ""),
                    error = function(e) NA)
      if (is.na(t) || t < Sys.time())
        return(list(ok = FALSE, code = "token_expired"))
    }
    return(list(ok = TRUE, owner_key = as.character(rec$owner_key),
                token_id = as.character(rec$id), code = "ok"))
  }
  list(ok = FALSE, code = "token_unknown")
}

# 校验通过后记一次使用时间。写失败不影响本次请求。
agent_token_touch <- function(app_dir, token_id) {
  obj <- agent_tokens_read(app_dir)
  hit <- FALSE
  for (i in seq_along(obj$tokens)) {
    if (!identical(as.character(obj$tokens[[i]]$id %||% ""), as.character(token_id))) next
    obj$tokens[[i]]$last_used_at <- agent_now()
    hit <- TRUE
  }
  if (hit) try(agent_tokens_write(app_dir, obj), silent = TRUE)
  invisible(hit)
}

# ---- 收牌 -----------------------------------------------------------

agent_token_revoke <- function(app_dir, token_id, owner_key = NULL) {
  obj <- agent_tokens_read(app_dir)
  hit <- FALSE
  for (i in seq_along(obj$tokens)) {
    rec <- obj$tokens[[i]]
    if (!identical(as.character(rec$id %||% ""), as.character(token_id))) next
    # 只能撤销自己的。跨用户撤销是一个越权入口。
    if (!is.null(owner_key) && !identical(as.character(rec$owner_key), as.character(owner_key)))
      return(list(ok = FALSE, code = "not_owner", error = "只能撤销自己授权的令牌"))
    if (nzchar(as.character(rec$revoked_at %||% "")))
      return(list(ok = TRUE, id = token_id, already = TRUE))
    obj$tokens[[i]]$revoked_at <- agent_now()
    hit <- TRUE
  }
  if (!hit) return(list(ok = FALSE, code = "token_not_found", error = "没有这个令牌"))
  agent_tokens_write(app_dir, obj)
  for (rec in obj$tokens) {
    if (!identical(as.character(rec$id), as.character(token_id))) next
    hf <- agent_handoff_file(app_dir, rec$owner_key)
    ho <- tryCatch(jsonlite::fromJSON(hf), error = function(e) NULL)
    if (!is.null(ho) && identical(as.character(ho$token_id), as.character(token_id)))
      unlink(hf)
  }
  agent_audit(app_dir, "revoke", list(token_id = as.character(token_id)))
  list(ok = TRUE, id = as.character(token_id))
}

# 给页面看的清单：永不回哈希，只回可辨识的元信息。
agent_token_list <- function(app_dir, owner_key) {
  obj <- agent_tokens_read(app_dir)
  out <- list()
  for (rec in obj$tokens) {
    if (!is.null(owner_key) && !identical(as.character(rec$owner_key), as.character(owner_key)))
      next
    out[[length(out) + 1L]] <- list(
      id = as.character(rec$id %||% ""),
      note = as.character(rec$note %||% ""),
      created_at = as.character(rec$created_at %||% ""),
      expires_at = as.character(rec$expires_at %||% ""),
      revoked_at = as.character(rec$revoked_at %||% ""),
      active = !nzchar(as.character(rec$revoked_at %||% "")),
      last_used_at = as.character(rec$last_used_at %||% ""))
  }
  out
}

# ---- 从请求里取令牌 ------------------------------------------------

# 令牌走请求头，不走 query string。
#   为什么：query 会进访问日志、进浏览器历史、进代理记录。
#   X-Agent-Token 在服务端自己手里，不外泄。
agent_token_from_req <- function(req) {
  v <- req$headers[["x-agent-token"]]
  if (is.null(v) || !length(v)) return(NULL)
  v <- trimws(as.character(v[[1]]))
  if (!nzchar(v)) NULL else v
}

agent_human_owner <- function(owner) {
  key <- owner$key
  is.character(key) && length(key) == 1L && !is.na(key) &&
    owner$kind %in% c("viewer", "topic") && grepl("^(viewer|topic):.+$", key)
}
