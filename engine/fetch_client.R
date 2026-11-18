# Fetch-only credentials are intentionally separate from agent and worker tokens.
fetch_scalar <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return("")
  x
}
fetch_deny <- function(code, status = 403L) list(ok = FALSE, status = status, code = code)
fetch_client_store <- function(app_dir) file.path(app_dir, "config", "fetch_tokens.json")
fetch_hash <- function(x) paste(format(openssl::sha256(charToRaw(x))), collapse = "")
fetch_random <- function(n = 32L) paste(sprintf("%02x", as.integer(openssl::rand_bytes(n))), collapse = "")
fetch_client_read <- function(app_dir) {
  f <- fetch_client_store(app_dir)
  if (!file.exists(f)) return(list(tokens = list()))
  obj <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  if (!is.list(obj$tokens)) stop("fetch_store_invalid")
  obj
}
fetch_client_mutate <- function(app_dir, fn) {
  f <- fetch_client_store(app_dir)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  lock <- paste0(f, ".lock")
  if (!dir.create(lock, mode = "0700", showWarnings = FALSE)) stop("fetch_store_busy")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  result <- fn(fetch_client_read(app_dir))
  tmp <- tempfile(".fetch-", tmpdir = dirname(f))
  on.exit(unlink(tmp), add = TRUE)
  writeLines(jsonlite::toJSON(result$store, auto_unbox = TRUE, null = "null", digits = NA), tmp, useBytes = TRUE)
  Sys.chmod(tmp, "0600")
  if (!file.rename(tmp, f)) stop("fetch_store_replace_failed")
  result$value
}
fetch_client_web <- function(ctx) {
  identical(ctx$kind, "viewer") && startsWith(fetch_scalar(ctx$key), "viewer:") &&
    nzchar(substring(fetch_scalar(ctx$key), 8L))
}
fetch_client_issue <- function(app_dir, owner_ctx, base_url, device_id = "fetch-device", ttl_days = 7) {
  if (!fetch_client_web(owner_ctx)) return(fetch_deny("web_identity_required"))
  if (!grepl("^https://[A-Za-z0-9][A-Za-z0-9.:-]*(/[A-Za-z0-9._~/-]*)?$", fetch_scalar(base_url)) ||
      grepl("[?#@]", base_url)) return(fetch_deny("invalid_base_url", 400L))
  if (!grepl("^[A-Za-z0-9._-]{1,80}$", fetch_scalar(device_id))) return(fetch_deny("invalid_device_id", 400L))
  if (length(ttl_days) != 1L || !is.numeric(ttl_days) || !is.finite(ttl_days) || ttl_days <= 0 || ttl_days > 7)
    return(fetch_deny("invalid_ttl", 400L))
  token <- paste0("pa_fetch_", fetch_random())
  now <- as.numeric(Sys.time())
  rec <- list(id = fetch_random(12L), hash = fetch_hash(token), owner_key = owner_ctx$key,
    device_id = device_id, scope = "mirror_fetch", created_at = now,
    expires_at = now + ttl_days * 86400, revoked_at = NULL)
  fetch_client_mutate(app_dir, function(obj) {
    obj$tokens <- lapply(obj$tokens, function(x) {
      if (identical(x$owner_key, owner_ctx$key) && is.null(x$revoked_at)) x$revoked_at <- now
      x
    })
    obj$tokens[[length(obj$tokens) + 1L]] <- rec
    list(store = obj, value = list(ok = TRUE, id = rec$id, expires_at = rec$expires_at,
      config = list(version = 1L, base_url = sub("/+$", "", base_url),
        fetch_token = token, device_id = device_id, expires_at = rec$expires_at)))
  })
}
fetch_client_revoke <- function(app_dir, owner_ctx) {
  if (!fetch_client_web(owner_ctx)) return(fetch_deny("web_identity_required"))
  fetch_client_mutate(app_dir, function(obj) {
    obj$tokens <- lapply(obj$tokens, function(x) {
      if (identical(x$owner_key, owner_ctx$key) && is.null(x$revoked_at)) x$revoked_at <- as.numeric(Sys.time())
      x
    })
    list(store = obj, value = list(ok = TRUE))
  })
}
fetch_client_check <- function(app_dir, token) {
  token <- fetch_scalar(token)
  if (!grepl("^pa_fetch_[0-9a-f]{64}$", token)) return(fetch_deny("fetch_token_invalid"))
  obj <- tryCatch(fetch_client_read(app_dir), error = function(e) NULL)
  if (is.null(obj)) return(fetch_deny("fetch_store_unavailable"))
  hits <- Filter(function(x) identical(x$hash, fetch_hash(token)), obj$tokens)
  if (length(hits) != 1L) return(fetch_deny("fetch_token_invalid"))
  rec <- hits[[1]]
  if (!is.null(rec$revoked_at)) return(fetch_deny("fetch_token_revoked"))
  if (!is.numeric(rec$expires_at) || length(rec$expires_at) != 1L ||
      !is.finite(rec$expires_at) || rec$expires_at <= as.numeric(Sys.time())) return(fetch_deny("fetch_token_expired"))
  if (!identical(rec$scope, "mirror_fetch") || !nzchar(fetch_scalar(rec$owner_key))) return(fetch_deny("fetch_scope_denied"))
  list(ok = TRUE, owner_ctx = list(key = rec$owner_key, kind = "fetch", fetch_id = rec$id))
}
fetch_client_request_auth <- function(app_dir, req, path, method, con = NULL, body = list()) {
  h <- req$headers
  if (is.null(h[["x-fetch-token"]])) return(list(present = FALSE))
  deny <- function(code) c(list(present = TRUE), fetch_deny(code))
  allowed <- (identical(method, "GET") && identical(path, "/api/mirror/pending")) ||
    (identical(method, "POST") && path %in% c("/api/mirror/put", "/api/mirror/put-chunk", "/api/mirror/fail"))
  if (!allowed) return(deny("fetch_route_denied"))
  if (!is.null(h[["x-agent-token"]]) || !is.null(h[["x-worker-token"]]) || !is.null(h[["authorization"]]))
    return(deny("fetch_mixed_identity"))
  auth <- fetch_client_check(app_dir, h[["x-fetch-token"]])
  if (!isTRUE(auth$ok)) return(c(list(present = TRUE), auth))
  if (!identical(method, "GET")) {
    sid <- fetch_scalar(body$session_id)
    if (!nzchar(sid)) return(deny("session_required"))
    cap <- tryCatch(record_capability(con, app_dir, sid, auth$owner_ctx), error = function(e) NULL)
    if (!isTRUE(cap$ok) || !identical(cap$owner_key, auth$owner_ctx$key) || !isTRUE(cap$is_owner))
      return(deny("fetch_record_forbidden"))
  }
  c(list(present = TRUE), auth)
}
