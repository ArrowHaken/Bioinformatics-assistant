#!/usr/bin/env Rscript
# Record addressing, capability checks, and fail-closed execution context.
# All record-scoped routes use this file instead of comparing against current.

# 隔离开关的兜底。
#   本文件也会被契约测试单独 source，那时 admin_policy.R 可能没加载。
#   缺它就按「两个隔离都打开」算 —— 从严那一侧，不是放开那一侧。
if (!exists("record_policy", mode = "function"))
  record_policy <- function(app_dir)
    list(record_isolation = TRUE, write_isolation = TRUE, updated_at = NULL)
if (!exists("record_policy_public", mode = "function"))
  record_policy_public <- function(pol = NULL) {
    p <- if (is.null(pol)) list() else pol
    list(record_isolation = !identical(p$record_isolation, FALSE),
         write_isolation = !identical(p$write_isolation, FALSE),
         updated_at = if (is.null(p$updated_at)) NULL else p$updated_at)
  }
if (!exists("admin_code_write_gate", mode = "function"))
  admin_code_write_gate <- function(app_dir, owner_ctx) {
    key <- if (is.null(owner_ctx)) "" else trimws(as.character(owner_ctx$key %||% ""))
    if (!nzchar(key))
      return(list(ok = FALSE, code = "identity_unavailable", admin = FALSE,
                  policy = list(), error = "身份未识别，不能修改应用代码。"))
    list(ok = FALSE, code = "admin_required", admin = FALSE, policy = list(),
         error = "修改权限隔离开着：只有管理员可以修改应用代码。")
  }
if (!exists("record_visibility_query", mode = "function"))
  record_visibility_query <- function(app_dir, owner_ctx) {
    key <- if (is.null(owner_ctx)) "" else trimws(as.character(owner_ctx$key %||% ""))
    list(sql = paste0("SELECT session_id, owner_key, visibility, name, updated_at ",
                      "FROM sessions WHERE owner_key = ? OR visibility = 'shared'"),
         params = list(key))
  }

record_scalar <- function(x, default = "") {
  if (is.null(x) || !length(x)) return(default)
  v <- as.character(x)[1]
  if (is.na(v)) default else v
}

record_trim <- function(x) trimws(record_scalar(x, ""))

record_query_value <- function(q, key) {
  if (is.list(q)) return(record_trim(q[[key]]))
  if (is.character(q) && length(q) == 1L &&
      exists("qget", mode = "function", inherits = TRUE))
    return(record_trim(qget(q, key)))
  ""
}

record_query_session <- function(q) {
  sid <- record_query_value(q, "session")
  if (!nzchar(sid)) sid <- record_query_value(q, "session_id")
  sid
}

# Canonical sid: query/body are equal-or-conflict; neither side wins silently.
# body$id is a target only for /api/sessions/* routes.
record_session_target <- function(q = NULL, body = NULL, path = "",
                                  current_sid = "", allow_current = FALSE,
                                  require = FALSE) {
  if (!is.list(body)) body <- list()
  qsid <- record_query_session(q)
  bsid <- record_trim(body[["session_id"]])
  bid <- record_trim(body[["id"]])
  qid <- record_query_value(q, "id")
  is_sessions <- startsWith(as.character(path %||% ""), "/api/sessions/")
  conflict <- function() list(ok = FALSE, status = 400L, code = "session_conflict",
    query_session = qsid, body_session_id = if (nzchar(bsid)) bsid else bid,
    error = "query/body 指定了不同的分析记录，未执行任何读写。")
  if (is_sessions && nzchar(qsid) && nzchar(qid) && !identical(qsid, qid))
    return(conflict())
  if (is_sessions && nzchar(qsid) && nzchar(bid) && !identical(qsid, bid))
    return(conflict())
  if (is_sessions && nzchar(qid) && nzchar(bid) && !identical(qid, bid))
    return(conflict())
  if (is_sessions && nzchar(qid) && nzchar(bsid) && !identical(qid, bsid))
    return(conflict())
  if (is_sessions && nzchar(bsid) && nzchar(bid) && !identical(bsid, bid))
    return(conflict())
  if (is_sessions) {
    if (!nzchar(bsid) && nzchar(bid)) bsid <- bid
    if (!nzchar(bsid) && nzchar(qid)) bsid <- qid
  }
  if (nzchar(qsid) && nzchar(bsid) && !identical(qsid, bsid)) {
    return(conflict())
  }
  sid <- if (nzchar(qsid)) qsid else bsid
  from_current <- FALSE
  if (!nzchar(sid) && isTRUE(allow_current)) {
    sid <- record_trim(current_sid)
    from_current <- nzchar(sid)
  }
  if (!nzchar(sid) && isTRUE(require)) {
    return(list(ok = FALSE, status = 400L, code = "session_required",
      query_session = qsid, body_session_id = bsid,
      error = "这条记录级请求必须显式指定 session_id。"))
  }
  list(ok = TRUE, sid = sid, query_session = qsid, body_session_id = bsid,
       record_source = if (from_current) "current" else "explicit")
}

# Route-level policy is the inventory used by both the HTTP preflight and the
# static contract test. A record route cannot execute without a capability check.
record_route_policy <- function(p, method = "GET") {
  method <- toupper(as.character(method %||% "GET"))
  record_read <- c(
    "/api/sessions/state", "/api/sessions/archives",
    "/api/status", "/api/flow", "/api/artifacts", "/api/download",
    "/api/artifact/download", "/api/artifact/file", "/api/artifact/preview",
    "/api/artifact/fetch", "/api/artifact/fetch/status",
    "/api/run/logs", "/api/run/logs/download",
    "/api/notebook/download", "/api/notebook/replay/download",
    "/api/brief", "/api/envcheck", "/api/quality", "/api/reviewer",
    "/api/decision", "/api/decisions", "/api/decisions/trace",
    "/api/reference/list", "/api/reference/get", "/api/ledger/out",
    "/api/variants", "/api/exec/view", "/api/judge",
    "/api/orch/next", "/api/convergence",
    "/api/runnable", "/api/dataset/profile", "/api/history",
    "/api/stage-scripts", "/api/script-resolve", "/api/run-dir")
  record_write <- c(
    "/api/sessions/switch", "/api/sessions/update", "/api/sessions/archive",
    "/api/sessions/restore", "/api/place/commit", "/api/config", "/api/run",
    "/api/stop", "/api/review", "/api/ppt/build", "/api/pack/build",
    "/api/artifact/request", "/api/artifacts/import", "/api/artifact/bundle",
    "/api/notebook/cells", "/api/notebook/build",
    "/api/notebook/replay/build", "/api/reference/upload",
    "/api/reference/meta", "/api/reference/choose",
    "/api/reference/scan-request", "/api/reference/scan",
    "/api/reference/litdict", "/api/decisions/ask", "/api/decisions/answer",
    "/api/ledger/ack", "/api/session/share", "/api/task",
    "/api/run-dir", "/api/snapshot", "/api/rollback")
  version_read <- c("/api/deliverable", "/api/version/status")
  task_bound <- c(
    "/api/step/import", "/api/envcheck/import", "/api/review/import",
    "/api/step/recover", "/api/attempts/result", "/api/mirror/put", "/api/mirror/fail",
    "/api/mirror/put-chunk", "/api/artifact/chunk", "/api/review/release",
    "/api/verdict")
  worker_routes <- c(
    "/api/exec/due", "/api/exec", "/api/exec/claim", "/api/exec/keepalive",
    "/api/exec/requeue", "/api/exec/done", "/api/exec/heartbeat",
    "/api/mirror/pending")
  system_routes <- c(
    "/api/fetch-client/config", "/api/fetch-client/revoke",
    "/api/fetch-client/client", "/api/fetch-client/status",
    "/api/me", "/api/whoami", "/api/ping", "/api/db/status",
    "/api/agent/token", "/api/agent/token/revoke",
    "/api/admin/policy",
    "/api/app-handoff/config", "/api/app-handoff/initialize",
    "/api/envprobe", "/api/instructions/prepare", "/api/instruction/prepare",
    "/api/intent/import", "/api/agent/dispatch", "/api/sessions", "/api/sessions/new", "/api/runs",
    "/api/dataset/audit", "/api/scripts/audit", "/api/consistency",
    "/api/supplement", "/api/supplement/get", "/api/engine-pack",
    "/api/where", "/api/env", "/api/fs", "/api/remote-check", "/api/where",
    "/api/flow/deps", "/api/pipeline/suggest", "/api/review/mode",
    "/api/rework/list", "/api/rework/get", "/api/engine-asset",
    "/api/engine/snapshot", "/api/poller/status", "/api/inject",
    "/api/inject/due", "/api/inject/item", "/api/outbox", "/api/queue",
    "/api/library", "/api/scripts", "/api/script-scan", "/api/script-archive",
    "/api/archive-read", "/api/lib-read", "/api/script-read",
    "/api/script-review", "/api/recent-dirs", "/api/instruct",
    "/api/script-put", "/api/script-restore", "/api/script-upload",
    "/api/incoming", "/api/script-change", "/api/script-ask")
  target_field <- if (p %in% c(
    "/api/sessions/state", "/api/sessions/archives", "/api/sessions/switch",
    "/api/sessions/update", "/api/sessions/archive", "/api/sessions/restore"))
    "session_id" else "session"
  if (p %in% c("/api/place", "/api/variants", "/api/dataset/profile",
               "/api/run-dir")) {
    scope <- "record"
    need <- if (identical(method, "GET")) "read" else "write"
    explicit <- !identical(need, "read")
    return(list(scope = scope, need = need, explicit = explicit,
                allow_current = identical(need, "read"), target = target_field))
  }
  if (p %in% record_read)
    return(list(scope = "record", need = "read", explicit = FALSE,
                allow_current = TRUE, target = target_field))
  if (p %in% record_write)
    return(list(scope = "record", need = "write", explicit = TRUE,
                allow_current = FALSE, target = target_field))
  if (p %in% version_read)
    return(list(scope = "version", need = "read", explicit = TRUE,
                allow_current = FALSE, target = "version_id"))
  if (p %in% task_bound)
    return(list(scope = "task_bound", need = NA_character_, explicit = FALSE,
                allow_current = FALSE, target = target_field))
  if (p %in% worker_routes)
    return(list(scope = "worker", need = NA_character_, explicit = FALSE,
                allow_current = FALSE, target = ""))
  if (p %in% system_routes)
    return(list(scope = "system", need = NA_character_, explicit = FALSE,
                allow_current = FALSE, target = ""))
  list(scope = "unclassified", need = NA_character_, explicit = FALSE,
       allow_current = FALSE, target = "")
}

record_artifact_dir <- function(app_dir) file.path(app_dir, "state", "artifacts")
record_artifact_file <- function(app_dir, sid) {
  s <- record_trim(sid)
  if (!nzchar(s)) return("")
  file.path(record_artifact_dir(app_dir), paste0(gsub("[^A-Za-z0-9._-]", "_", s), ".json"))
}

record_artifact_index_read <- function(app_dir, sid) {
  f <- record_artifact_file(app_dir, sid)
  if (!nzchar(f) || !file.exists(f)) return(list(session_id = record_trim(sid), items = list()))
  d <- tryCatch(jsonlite::fromJSON(read_text_any(f), simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(d) || !is.list(d)) return(list(session_id = record_trim(sid), items = list()))
  if (is.null(d$items) || !is.list(d$items)) d$items <- list()
  d$session_id <- record_trim(sid)
  d
}

record_artifact_index_write <- function(app_dir, sid, d) {
  f <- record_artifact_file(app_dir, sid)
  if (!nzchar(f)) stop("session_required")
  d$session_id <- record_trim(sid)
  d$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  write_json_file(f, d)
  invisible(d)
}

record_payload_place <- function(payload) {
  if (is.list(payload)) return(payload$place)
  txt <- record_scalar(payload)
  if (!nzchar(txt)) return(NULL)
  x <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (is.list(x)) x$place else NULL
}

record_capability <- function(con, app_dir, sid, owner_ctx) {
  sid <- record_trim(sid)
  if (!nzchar(sid))
    return(list(ok = FALSE, status = 400L, code = "session_required",
      error = "缺少分析记录号。"))
  if (is.null(con))
    return(list(ok = FALSE, status = 503L, code = "database_unavailable",
      error = "记录数据库不可用，无法校验归属。"))
  rows <- tryCatch(db_rows(con, "SELECT * FROM sessions WHERE session_id = ?", list(sid)),
                   error = function(e) NULL)
  if (is.null(rows))
    return(list(ok = FALSE, status = 503L, code = "record_table_unreadable",
      error = "记录表不可读，无法校验归属。"))
  if (!nrow(rows))
    return(list(ok = FALSE, status = 404L, code = "session_record_missing",
      session_id = sid, error = "没有这条分析记录。"))

  row <- as.list(rows[1, , drop = FALSE])
  rec <- tryCatch(sess_find(app_dir, sid), error = function(e) NULL)
  record_owner <- record_scalar(row$owner_key)
  requester <- record_scalar(owner_ctx$key)
  visibility <- record_scalar(row$visibility, "private")
  is_owner <- nzchar(requester) && nzchar(record_owner) && identical(requester, record_owner)

  # 两个开关在这里落地，别处不再各写一遍。
  #   record_isolation 关掉 = 已识别的人都能读别人的记录（管理员显式选择）。
  #   write_isolation 关掉 = 已识别的人都能改别人的记录，其中包括让助手改代码。
  #   未识别身份仍然 fail-closed：开关不是"放陌生人进来"，是"放开已有账号之间"。
  identified <- nzchar(requester)
  pol <- record_policy(app_dir)
  write_open <- identified && !isTRUE(pol$write_isolation)
  read_open <- identified && !isTRUE(pol$record_isolation)

  can_write <- is_owner || write_open
  # 能写必须能读：只放开修改、不放开可见时，会出现「能改但看不到」的状态，
  #   页面于是能切进一条自己读不了的记录 —— 那不是放开权限，那是数据错位。
  can_read <- is_owner || identical(visibility, "shared") || read_open || can_write

  # The execution context comes from the sessions row. A JSON record cannot
  # supply a missing mode/run_dir and thereby bypass the fail-closed rules.
  mode <- record_trim(row$mode)
  run_dir <- record_trim(row$run_dir)
  place <- if (!is.null(rec) && !is.null(rec$place)) rec$place else record_payload_place(row$payload)
  if (!is.list(place)) place <- NULL
  version_id <- record_trim(row$version_id)

  list(ok = TRUE, session_id = sid, owner_key = record_owner,
       visibility = visibility, mode = mode, run_dir = run_dir, place = place,
       version_id = version_id, is_owner = is_owner,
       can_read = can_read, can_write = can_write, readonly = !can_write,
       record = rec)
}

record_cap_fields <- function(cap) {
  if (is.null(cap) || !isTRUE(cap$ok)) return(list())
  list(session_id = cap$session_id, can_read = isTRUE(cap$can_read),
       can_write = isTRUE(cap$can_write), readonly = isTRUE(cap$readonly))
}

record_refusal <- function(x, error = NULL) {
  body <- list(ok = FALSE, code = x$code %||% "record_forbidden",
               error = error %||% x$error %||% "记录权限校验未通过")
  if (!is.null(x$session_id)) body$session_id <- x$session_id
  if (!is.null(x$can_read)) body$can_read <- isTRUE(x$can_read)
  if (!is.null(x$can_write)) body$can_write <- isTRUE(x$can_write)
  if (!is.null(x$readonly)) body$readonly <- isTRUE(x$readonly)
  if (!is.null(x$query_session)) body$query_session <- x$query_session
  if (!is.null(x$body_session_id)) body$body_session_id <- x$body_session_id
  list(status = as.integer(x$status %||% 403L), body = body)
}

record_context <- function(cap, require_run_dir = TRUE) {
  if (!isTRUE(cap$ok)) return(cap)
  mode <- record_trim(cap$mode)
  run_dir <- record_trim(cap$run_dir)
  if (!nzchar(mode))
    return(list(ok = FALSE, status = 409L, code = "record_mode_missing",
      error = "目标记录没有 mode，不能猜全局运行方式。"))
  if (!mode %in% c("local", "server"))
    return(list(ok = FALSE, status = 409L, code = "record_mode_invalid",
      error = paste0("目标记录的 mode 不可用：", mode)))
  if (isTRUE(require_run_dir) && !nzchar(run_dir))
    return(list(ok = FALSE, status = 409L, code = "record_run_dir_missing",
      error = "目标记录没有 run_dir；不会回退全局目录。"))
  srv <- if (is.list(cap$place)) cap$place$server else NULL
  if (identical(mode, "server") && (!is.list(srv) || !length(srv)))
    return(list(ok = FALSE, status = 409L, code = "record_server_place_missing",
      error = "目标记录是 server 模式，但没有 place$server。"))
  list(ok = TRUE, session_id = cap$session_id, owner_key = cap$owner_key,
       mode = mode, run_dir = run_dir, run_dir_remote = !identical(mode, "local"),
       place = cap$place %||% list(), server_place = srv %||% list(),
       ref_mode = record_trim((srv %||% list())$ref_mode),
       ref_path = record_trim((srv %||% list())$ref_path),
       version_id = cap$version_id, cap = cap)
}

record_place_ready <- function(place, mode, run_dir = "") {
  mode <- record_trim(mode)
  p <- if (is.list(place)) place else list()
  if (identical(mode, "local")) {
    lp <- record_trim(run_dir)
    if (!nzchar(lp)) lp <- record_trim(p$local$path)
    return(nzchar(lp) && dir.exists(lp))
  }
  if (identical(mode, "server")) {
    s <- if (is.list(p$server)) p$server else list()
    return(nzchar(record_trim(s$host)) && nzchar(record_trim(s$user)) &&
             nzchar(record_trim(s$data_root)))
  }
  FALSE
}

record_access <- function(con, app_dir, sid, owner_ctx, need = c("read", "write"),
                          require_ctx = FALSE, require_run_dir = TRUE) {
  need <- match.arg(need)
  cap <- record_capability(con, app_dir, sid, owner_ctx)
  if (!isTRUE(cap$ok)) return(record_refusal(cap))
  allowed <- if (identical(need, "write")) isTRUE(cap$can_write) else isTRUE(cap$can_read)
  if (!allowed) {
    cap$status <- 403L
    cap$code <- if (identical(need, "write")) "not_owner" else "record_forbidden"
    cap$error <- if (identical(need, "write"))
      "只有记录所有者可以写入这条分析记录。" else "无权读取这条分析记录。"
    return(record_refusal(cap))
  }
  ctx <- NULL
  if (isTRUE(require_ctx)) {
    ctx <- record_context(cap, require_run_dir = require_run_dir)
    if (!isTRUE(ctx$ok)) {
      out <- record_refusal(ctx)
      out$body <- c(out$body, record_cap_fields(cap))
      return(out)
    }
  }
  list(ok = TRUE, cap = cap, ctx = ctx)
}

record_request_preflight <- function(app_dir, con, p, method, q, body, owner_ctx,
                                     current_sid = "") {
  policy <- record_route_policy(p, method)
  if (identical(policy$scope, "version")) {
    vid <- record_query_value(q, "version_id")
    if (!nzchar(vid)) {
      return(list(ok = FALSE, status = 400L,
        body = list(ok = FALSE, code = "version_required",
                    error = "必须指定 version_id，未执行任何读写。"),
        policy = policy))
    }
    rows <- if (is.null(con)) NULL else tryCatch(
      db_rows(con, "SELECT session_id FROM sessions WHERE version_id = ?", list(vid)),
      error = function(e) NULL)
    if (is.null(rows)) {
      return(list(ok = FALSE, status = 503L,
        body = list(ok = FALSE, code = "database_unavailable",
                    error = "记录数据库不可用，无法校验版本归属。"),
        policy = policy))
    }
    sids <- unique(trimws(as.character(rows$session_id)))
    sids <- sids[nzchar(sids)]
    if (length(sids) != 1L) {
      return(list(ok = FALSE, status = if (length(sids)) 409L else 404L,
        body = list(ok = FALSE,
          code = if (length(sids)) "version_record_ambiguous" else "version_record_missing",
          version_id = vid,
          error = if (length(sids)) "这个版本对应多条分析记录，不能猜是哪一条。"
                  else "没有分析记录指向这个版本。"),
        policy = policy))
    }
    target <- list(ok = TRUE, sid = sids[[1L]], version_id = vid,
                   query_session = record_query_session(q), body_session_id = "")
    access <- record_access(con, app_dir, target$sid, owner_ctx,
                            need = "read", require_ctx = FALSE)
    if (!isTRUE(access$ok)) {
      return(list(ok = FALSE, status = access$status, body = access$body,
                  policy = policy, target = target, access = access))
    }
    return(list(ok = TRUE, policy = policy, target = target, access = access))
  }
  if (!identical(policy$scope, "record"))
    return(list(ok = TRUE, policy = policy, target = NULL, access = NULL))
  target <- record_session_target(q, body, path = p, current_sid = current_sid,
                                  allow_current = isTRUE(policy$allow_current),
                                  require = isTRUE(policy$explicit))
  if (!isTRUE(target$ok)) {
    refused <- record_refusal(target)
    return(list(ok = FALSE, status = refused$status, body = refused$body,
                policy = policy, target = target))
  }
  access <- record_access(con, app_dir, target$sid, owner_ctx,
                          need = policy$need, require_ctx = FALSE)
  if (!isTRUE(access$ok)) {
    return(list(ok = FALSE, status = access$status, body = access$body,
                policy = policy, target = target, access = access))
  }
  list(ok = TRUE, policy = policy, target = target, access = access)
}

record_preflight_sid <- function(preflight) {
  if (is.null(preflight) || !isTRUE(preflight$ok) || is.null(preflight$target))
    return("")
  record_trim(preflight$target$sid)
}

record_preflight_cap <- function(preflight) {
  if (is.null(preflight) || !isTRUE(preflight$ok) || is.null(preflight$access))
    return(NULL)
  preflight$access$cap
}

record_preflight_ctx <- function(preflight, require_run_dir = TRUE) {
  cap <- record_preflight_cap(preflight)
  if (is.null(cap)) return(NULL)
  record_context(cap, require_run_dir = require_run_dir)
}

record_public_place <- function(place) {
  if (!is.list(place)) return(list())
  if (is.list(place$server)) place$server$password <- NULL
  place
}

record_place_merge <- function(base, incoming) {
  base <- if (is.list(base)) base else if (exists("place_default", mode = "function")) place_default() else list()
  inc <- if (is.list(incoming)) incoming else list()
  inc$session_id <- NULL
  if (is.list(inc$server)) {
    pwd <- record_scalar(inc$server$password)
    if (!nzchar(pwd)) inc$server$password <- NULL
  }
  if (exists("place_deep_merge", mode = "function")) {
    out <- place_deep_merge(base, inc)
  } else {
    out <- base
    for (nm in names(inc)) out[[nm]] <- inc[[nm]]
  }
  if (exists("place_clean_server", mode = "function") && is.list(out$server))
    out$server <- place_clean_server(out$server)
  if (exists("place_as_path", mode = "function") && is.list(out$local))
    out$local$path <- place_as_path(out$local$path, "")
  out$mode <- tolower(record_scalar(out$mode, "local"))
  out
}

record_place_run_dir <- function(place, mode) {
  if (identical(as.character(mode), "local")) return(record_trim((place %||% list())$local$path))
  record_trim((place %||% list())$server$work_root)
}
