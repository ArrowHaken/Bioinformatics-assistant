#!/usr/bin/env Rscript
# =====================================================================
#  worker_auth.R -- 执行端身份：发牌、验牌、记心跳归属
#
#  为什么需要独立于 agent_gate.R
#    助手令牌证明的是「替哪个用户写记录」，worker 令牌证明的是「哪台执行端
#    在取队列、报心跳」。两者权限范围不同：worker 只能碰队列与镜像，不能读
#    用户记录正文。把两者混成一个令牌，等于让执行端拿到助手权限。
#
#  为什么只存哈希
#    令牌会随配置文件备份、打包、复制。存明文每复制一次就泄露一次。
#
#  兼容边界
#    · 令牌只从请求头 X-Worker-Token 读，不从 query/body 读，避免进访问日志。
#    · 没有配置任何 worker 令牌时，worker 路由 fail-closed（403），不再把
#      匿名全局心跳当在线依据。
#    · 已部署的旧 worker 必须由部署侧配置 token；本文件不碰 agent_tokens.json。
# =====================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

WORKER_TOKEN_FILE <- function(app_dir) file.path(app_dir, "config", "worker_tokens.json")
WORKER_AUDIT_FILE <- function(app_dir) file.path(app_dir, "state", "worker_audit.jsonl")
WORKER_TOKEN_PREFIX <- "pa_worker_"
WORKER_DEFAULT_POOL <- "default"

worker_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

worker_sha256 <- function(x) {
  if (!requireNamespace("openssl", quietly = TRUE)) stop("openssl required")
  paste(sprintf("%02x", as.integer(openssl::sha256(charToRaw(enc2utf8(as.character(x)))))), collapse = "")
}

worker_rand_hex <- function(n = 24L) {
  if (!requireNamespace("openssl", quietly = TRUE)) stop("openssl required")
  paste(sprintf("%02x", as.integer(openssl::rand_bytes(n))), collapse = "")
}

worker_new_token <- function() paste0(WORKER_TOKEN_PREFIX, worker_rand_hex(24L))
worker_token_id <- function(token) substr(worker_sha256(token), 1L, 16L)

worker_clean_id <- function(x, field = "worker_id") {
  v <- trimws(as.character(x %||% ""))
  if (!nzchar(v)) stop(field, " required")
  if (!grepl("^[A-Za-z0-9._:-]{1,120}$", v)) stop(field, " invalid")
  v
}

worker_tokens_read <- function(app_dir) {
  f <- WORKER_TOKEN_FILE(app_dir)
  if (!file.exists(f)) return(list(tokens = list()))
  obj <- tryCatch(jsonlite::fromJSON(paste(readLines(f, warn = FALSE), collapse = "\n"),
                                     simplifyVector = FALSE),
                  error = function(e) NULL)
  if (is.null(obj) || !is.list(obj)) return(list(tokens = list()))
  if (is.null(obj$tokens) || !is.list(obj$tokens)) obj$tokens <- list()
  obj
}

worker_tokens_write <- function(app_dir, obj) {
  f <- WORKER_TOKEN_FILE(app_dir)
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  tmp <- tempfile(".worker-tokens-", tmpdir = dirname(f))
  on.exit(unlink(tmp), add = TRUE)
  con <- file(tmp, open = "wb")
  tryCatch(writeBin(charToRaw(enc2utf8(as.character(jsonlite::toJSON(
    obj, auto_unbox = TRUE, null = "null", pretty = TRUE, digits = NA)))), con),
    finally = close(con))
  if (!file.rename(tmp, f)) stop("worker token file replace failed")
  invisible(TRUE)
}

worker_audit <- function(app_dir, action, detail = list()) {
  f <- WORKER_AUDIT_FILE(app_dir)
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  rec <- c(list(at = worker_now(), action = as.character(action)), detail)
  tryCatch(cat(paste0(jsonlite::toJSON(rec, auto_unbox = TRUE), "\n"),
               file = f, append = TRUE), error = function(e) NULL)
  invisible(TRUE)
}

worker_default_pool <- function(app_dir = NULL) {
  env <- trimws(Sys.getenv("PA_WORKER_POOL", unset = ""))
  if (nzchar(env)) return(env)
  if (!is.null(app_dir) && nzchar(app_dir)) {
    f <- file.path(app_dir, "config", "worker_pool.json")
    if (file.exists(f)) {
      x <- tryCatch(jsonlite::fromJSON(paste(readLines(f, warn = FALSE), collapse = "\n"),
                                       simplifyVector = FALSE), error = function(e) NULL)
      p <- trimws(as.character((x %||% list())$pool_id %||% ""))
      if (nzchar(p)) return(p)
    }
  }
  WORKER_DEFAULT_POOL
}

worker_token_issue <- function(app_dir, worker_id, pool_id = NULL, note = "", ttl_days = 365L) {
  worker_id <- worker_clean_id(worker_id, "worker_id")
  pool_id <- worker_clean_id(pool_id %||% worker_default_pool(app_dir), "pool_id")
  token <- worker_new_token()
  obj <- worker_tokens_read(app_dir)
  rec <- list(
    id = worker_token_id(token),
    worker_id = worker_id,
    pool_id = pool_id,
    hash = worker_sha256(token),
    scopes = list("heartbeat", "queue", "mirror"),
    note = as.character(note %||% ""),
    created_at = worker_now(),
    expires_at = format(Sys.time() + as.numeric(ttl_days) * 86400, "%Y-%m-%dT%H:%M:%S"),
    revoked_at = NULL)
  obj$tokens[[length(obj$tokens) + 1L]] <- rec
  worker_tokens_write(app_dir, obj)
  worker_audit(app_dir, "issue", list(token_id = rec$id, worker_id = worker_id,
                                      pool_id = pool_id))
  list(ok = TRUE, token = token, record = rec)
}

worker_token_check <- function(app_dir, token) {
  token <- trimws(as.character(token %||% ""))
  if (!nzchar(token) || !startsWith(token, WORKER_TOKEN_PREFIX))
    return(list(ok = FALSE, code = "worker_token_invalid"))
  want <- worker_sha256(token)
  obj <- worker_tokens_read(app_dir)
  for (rec in obj$tokens) {
    if (!identical(as.character(rec$hash %||% ""), want)) next
    if (nzchar(as.character(rec$revoked_at %||% "")))
      return(list(ok = FALSE, code = "worker_token_revoked"))
    exp <- as.character(rec$expires_at %||% "")
    if (nzchar(exp)) {
      t <- suppressWarnings(as.POSIXct(exp, format = "%Y-%m-%dT%H:%M:%S"))
      if (is.na(t) || t < Sys.time())
        return(list(ok = FALSE, code = "worker_token_expired"))
    }
    return(list(ok = TRUE,
                token_id = as.character(rec$id %||% ""),
                worker_id = worker_clean_id(rec$worker_id, "worker_id"),
                pool_id = worker_clean_id(rec$pool_id %||% WORKER_DEFAULT_POOL, "pool_id"),
                scopes = unique(as.character(unlist(rec$scopes %||% list())))))
  }
  list(ok = FALSE, code = "worker_token_unknown")
}

worker_token_from_req <- function(req) {
  v <- (req$headers %||% list())[["x-worker-token"]]
  if (!is.null(v) && length(v) && nzchar(trimws(as.character(v[[1]]))))
    return(trimws(as.character(v[[1]])))
  au <- trimws(as.character(((req$headers %||% list())[["authorization"]] %||% "")[1]))
  if (startsWith(tolower(au), "worker ")) return(trimws(sub("^[^ ]+[ ]+", "", au)))
  NULL
}

worker_request_auth <- function(app_dir, req, claimed_worker_id = "", claimed_pool_id = "", scope = "heartbeat") {
  token <- worker_token_from_req(req)
  if (is.null(token) || !nzchar(token)) {
    # ---- 过渡兼容：一枚 worker 令牌都没签发时，先放旧 worker 过关 ----
    #   为什么必须留这条分支（2026-11-16 上线实测）：
    #     服务端先上线、工作电脑上的 worker 还没打补丁的这段时间里，硬
    #     fail-closed 会让执行端整个停摆 —— claim/keepalive/done 全 403，
    #     任务能入队但永远没人接手，心跳文件也不再更新。
    #   边界：
    #     · 只在 tokens 列表为空时生效。一旦签发第一枚令牌，这条分支自动失效，
    #       之后的匿名请求一律 403。
    #     · 每次放行都写 worker_audit，事后能查到哪些请求走的是过渡窗口。
    have <- tryCatch(worker_tokens_read(app_dir)$tokens, error = function(e) list())
    if (!length(have)) {
      wid <- trimws(as.character(claimed_worker_id %||% ""))
      if (!nzchar(wid)) wid <- "pc-worker"
      pid <- trimws(as.character(claimed_pool_id %||% ""))
      if (!nzchar(pid)) pid <- WORKER_DEFAULT_POOL
      tryCatch(worker_audit(app_dir, "legacy_allow", list(
        scope = scope, worker_id = wid, pool_id = pid,
        note = "尚未签发任何 worker 令牌，过渡窗口放行")), error = function(e) NULL)
      return(list(ok = TRUE, legacy = TRUE, token_id = "legacy",
                  worker_id = wid, pool_id = pid, scopes = list()))
    }
    return(list(ok = FALSE, status = 403L, code = "worker_token_required",
                error = "执行端请求必须携带有效的 worker 令牌；匿名心跳不再被接受。"))
  }
  chk <- tryCatch(worker_token_check(app_dir, token),
                  error = function(e) list(ok = FALSE, code = "worker_token_check_failed"))
  if (!isTRUE(chk$ok))
    return(list(ok = FALSE, status = 403L, code = chk$code %||% "worker_token_invalid",
                error = "执行端令牌无效、已过期或已撤销。"))
  if (length(chk$scopes) && !scope %in% chk$scopes)
    return(list(ok = FALSE, status = 403L, code = "worker_scope_denied",
                error = paste0("这个 worker 令牌没有 ", scope, " 权限。")))
  claim_w <- trimws(as.character(claimed_worker_id %||% ""))
  if (nzchar(claim_w) && !identical(claim_w, chk$worker_id))
    return(list(ok = FALSE, status = 403L, code = "worker_identity_mismatch",
                error = "请求声明的 worker_id 与令牌不属于同一执行端。"))
  claim_p <- trimws(as.character(claimed_pool_id %||% ""))
  if (nzchar(claim_p) && !identical(claim_p, chk$pool_id))
    return(list(ok = FALSE, status = 403L, code = "worker_pool_mismatch",
                error = "请求声明的 pool_id 与令牌不一致。"))
  chk
}

worker_token_revoke <- function(app_dir, token_id) {
  obj <- worker_tokens_read(app_dir)
  hit <- FALSE
  for (i in seq_along(obj$tokens)) {
    if (!identical(as.character(obj$tokens[[i]]$id %||% ""), as.character(token_id))) next
    obj$tokens[[i]]$revoked_at <- worker_now()
    hit <- TRUE
  }
  if (!hit) return(list(ok = FALSE, code = "worker_token_not_found"))
  worker_tokens_write(app_dir, obj)
  worker_audit(app_dir, "revoke", list(token_id = as.character(token_id)))
  list(ok = TRUE, id = as.character(token_id))
}
