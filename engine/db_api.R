#!/usr/bin/env Rscript
# =====================================================================
#  db_api.R -- 把数据库层接到 HTTP 上（由 serve.R source）
#
#  为什么单独一个文件
#    serve.R 已经有 4800 行、几十条路由。把新的数据层逻辑直接塞进去，
#    下一个读代码的人分不清哪些是旧字段、哪些是新表。
#    这里只做三件事：解析 owner、把请求翻成一条 SQL、把结果翻成 JSON。
#    「怎么判」在 verdict.R，「怎么催」在 rework.R，不在这里重复。
# =====================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# db_api.R is also sourced by small contract tests without serve.R. Keep the
# preflight accessors local-compatible; serve.R replaces these with the shared
# implementations from record_access.R when it loads later.
if (!exists("record_scalar", mode = "function"))
  record_scalar <- function(x, default = "") {
    if (is.null(x) || !length(x)) return(default)
    v <- as.character(x)[1]
    if (is.na(v)) default else v
  }
if (!exists("record_trim", mode = "function"))
  record_trim <- function(x) trimws(record_scalar(x, ""))
if (!exists("record_preflight_sid", mode = "function"))
  record_preflight_sid <- function(preflight) {
    if (is.null(preflight) || !isTRUE(preflight$ok) || is.null(preflight$target))
      return("")
    record_trim(preflight$target$sid)
  }
if (!exists("record_preflight_cap", mode = "function"))
  record_preflight_cap <- function(preflight) {
    if (is.null(preflight) || !isTRUE(preflight$ok) || is.null(preflight$access))
      return(NULL)
    preflight$access$cap
  }
if (!exists("record_cap_fields", mode = "function"))
  record_cap_fields <- function(cap) {
    if (is.null(cap) || !isTRUE(cap$ok)) return(list())
    list(session_id = cap$session_id, can_read = isTRUE(cap$can_read),
         can_write = isTRUE(cap$can_write), readonly = isTRUE(cap$readonly))
  }

DB_READY <- FALSE

db_api_boot <- function(app_dir) {
  con <- tryCatch(db_connect(app_dir), error = function(e) NULL)
  if (is.null(con)) return(NULL)
  ok <- tryCatch({db_init(con); TRUE}, error = function(e) FALSE)
  if (!ok) {DBI::dbDisconnect(con); return(NULL)}
  con
}

# ---- owner 解析（B1 / B4）-------------------------------------------

# 每次请求算一次，放进请求上下文，后续所有读写都带它。
#   顺序：网关注入的身份 -> 本机匿名 cookie -> 未识别（fail-closed）。
#   为什么 fail-closed：现在的实现是「身份拿不到就照常读全局那份」，
#   于是某个时刻网关超时，用户会看到别人的五条记录，而且页面上没有任何提示。
db_api_owner <- function(app_dir, req) {
  # ---- 助手令牌优先，且必须是独立分支（2026-11-09）------------------
  #   为什么放在最前面：
  #     助手在本机发请求，永远不会带平台 cookie。它带的是人授权过的令牌。
  #     如果先走 cookie 链，它就一路掉到 local:default，于是写的记录人看不见。
  #   为什么不许「令牌无效就继续往下走」：
  #     那样一个过期令牌会被当成没带令牌，静默降级成本机身份，
  #     人得到的是一句"写入成功"，实际写进了另一个人的空间。
  #     所以带了令牌就必须验通过，验不过直接失败。
  if (exists("agent_token_from_req", mode = "function")) {
    tok <- agent_token_from_req(req)
    if (!is.null(tok)) {
      chk <- tryCatch(agent_token_check(app_dir, tok),
                      error = function(e) list(ok = FALSE, code = "token_check_failed"))
      if (isTRUE(chk$ok)) {
        tryCatch(agent_token_touch(app_dir, chk$token_id), error = function(e) NULL)
        return(list(key = chk$owner_key, kind = "agent",
                    external_id = chk$token_id, via = "agent_token"))
      }
      return(list(key = NA_character_, kind = "agent_token_invalid",
                  external_id = NA_character_, code = chk$code,
                  error = paste0("助手令牌不可用：", chk$code)))
    }
  }

  ident <- NULL
  ck <- req$headers[["cookie"]]
  au <- req$headers[["authorization"]]
  if ((!is.null(ck) && nzchar(as.character(ck))) || (!is.null(au) && nzchar(as.character(au)))) {
    ident <- tryCatch({
      if (!requireNamespace("curl", quietly = TRUE)) stop("no curl")
      h <- curl::new_handle(timeout = 4, connecttimeout = 2)
      hdrs <- list()
      if (!is.null(ck) && nzchar(as.character(ck))) hdrs$cookie <- as.character(ck)
      if (!is.null(au) && nzchar(as.character(au))) hdrs$authorization <- as.character(au)
      if (length(hdrs)) curl::handle_setheaders(h, .list = hdrs)
      r <- curl::curl_fetch_memory(
        "https://artifact.catsco.cc/_gateway/me?app=personal-analysis-assistant", handle = h)
      if (r$status_code != 200L) stop(paste0("gateway ", r$status_code))
      jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE)
    }, error = function(e) list(error = conditionMessage(e)))
  }

  token <- db_api_anon_token(app_dir, req)
  owner <- owner_from_identity(ident, token)
  # 平台账号字段：判「这个人是不是机器人主人」要用它。
  #   只从上面那条网关问询的结果里取，绝不从请求头取 —— 头是客户端输入。
  #   取不到就留空，管理员判定会退回 owner_key / 配置里的用户名，
  #   不会因为缺一个字段就把谁当成管理员。
  owner$viewer_uid <- NA_character_
  owner$viewer_username <- NA_character_
  if (is.list(ident) && isTRUE(ident$authenticated) && is.list(ident$viewer)) {
    v <- ident$viewer
    uid <- v$uid %||% v$user_id %||% NULL
    uname <- v$username %||% NULL
    if (length(uid) == 1L && !is.list(uid) && !is.na(uid) && nzchar(trimws(as.character(uid))))
      owner$viewer_uid <- trimws(as.character(uid))
    if (length(uname) == 1L && !is.list(uname) && !is.na(uname) &&
        nzchar(trimws(as.character(uname))))
      owner$viewer_username <- trimws(as.character(uname))
  }
  # ident comes only from the fixed HTTPS gateway above, never request identity headers.
  approval_file <- file.path(app_dir, "config", "legacy_ownership_approvals.json")
  if (is.list(ident) && isTRUE(ident$authenticated) && file.exists(approval_file)) {
    source(file.path(app_dir, "engine", "legacy_ownership.R"), local = TRUE)
    claim_con <- get0("DB_CON", inherits = TRUE)
    if (!is.null(claim_con)) legacy_claim_approved(claim_con, ident, legacy_approval(app_dir))
  }

  # No anonymous local:default fallback. Host and forwarding headers are client input;
  # serve.R supplies no verified transport boundary, and a loopback proxy is not a user.
  # A local-only anonymous mode requires an explicitly isolated listener with no proxy
  # ingress, not a Host allowlist or a peer-address-only authentication shortcut.

  if (is.na(owner$key)) {
    owner$error <- if (is.list(ident) && !is.null(ident$error)) ident$error else "no_identity"
  }
  owner
}

# 匿名令牌：只在本机有效，写在 cookie 里。
#   不写 cookie 的后果很具体：每刷新一次就是新用户，历史记录碎成一份一份，
#   用户会以为"记录又丢了"。这正是本文件要修的那类现象之一。
db_api_anon_token <- function(app_dir, req) {
  f <- file.path(app_dir, "state", "anon_tokens.json")
  ck <- as.character(req$headers[["cookie"]] %||% "")
  m <- regmatches(ck, regexpr("pa_anon=[A-Za-z0-9]+", ck))
  if (length(m) && nzchar(m)) return(sub("^pa_anon=", "", m))
  NULL
}

owner_ctx_key <- function(owner) if (is.null(owner) || is.na(owner$key %||% NA))
  NA_character_ else owner$key


# ---- 引擎资产白名单（D2）-------------------------------------------
#  只放开「执行端真的需要」的目录：PPT 套件、补充脚本库、白名单、流程说明。
#  不放 engine/ 整体：那等于给了一个读全部源码的接口。
DB_API_ASSET_ROOTS <- c("sc-ppt-kit/templates", "sc-ppt-kit/scripts",
                        "sc-plot-kit/scripts", "sc-pipeline/scripts",
                        "library/C_supplement", "audit", "workflow",
                        # 体检探针本身也要能取：执行端在别人内网，够不到应用磁盘。
                        #   以前没有这条，执行端只能靠人把脚本拷过去，
                        #   于是各机器上的副本慢慢就旧了 —— PC 上留着的两份
                        #   都是 24 KB / 15 KB 的旧版，最新那份 28 KB 只在应用本机。
                        #   只放这一个文件名，不放 engine/ 根：根放开了等于读全部源码。
                        "workpc_check.py")

db_api_engine_asset_path <- function(app_dir, rel) {
  r <- gsub("\\\\", "/", as.character(rel %||% ""))
  r <- sub("^/+", "", r)
  # 任何一段是 .. 就拒：路径穿越只有这一种形态，挡住它就够了。
  if (grepl("(^|/)\\.\\.(/|$)", r)) return(NULL)
  if (grepl("^[A-Za-z]:", r)) return(NULL)
  hit <- vapply(DB_API_ASSET_ROOTS, function(root)
    identical(r, root) || startsWith(r, paste0(root, "/")), TRUE)
  if (!any(hit)) return(NULL)
  file.path(app_dir, "engine", r)
}

db_api_fetch_session <- function(con, job) {
  if (is.null(con) || is.null(job) || !nrow(job)) return("")
  row <- job[1, , drop = FALSE]
  pid <- record_scalar(row$version_id)
  own <- record_scalar(row$owner_key)
  if (!nzchar(pid) || !nzchar(own)) return("")
  rows <- tryCatch(db_rows(con,
    "SELECT session_id FROM sessions WHERE version_id = ? AND owner_key = ?",
    list(pid, own)), error = function(e) NULL)
  if (is.null(rows) || nrow(rows) != 1L) return("")
  record_trim(rows$session_id[[1L]])
}

# ---- 路由 ------------------------------------------------------------

# 返回 NULL 表示「这条不是我处理的」，调用方继续往下走。
db_api_handle <- function(con, app_dir, p, req, q, body_json, send_json, owner,
                          preflight = NULL) {
  if (!DB_READY || is.null(con)) return(NULL)
  now <- db_now()
  pre_sid <- record_preflight_sid(preflight)
  pre_cap <- record_preflight_cap(preflight)
  pre_owner <- if (is.null(pre_cap)) NA_character_ else record_scalar(pre_cap$owner_key)
  pre_mode <- if (is.null(pre_cap)) "" else record_trim(pre_cap$mode)
  pre_vid <- if (is.null(pre_cap)) "" else record_trim(pre_cap$version_id)

  # ---- 我现在的身份与工作区 ------------------------------------------
  if (p == "/api/me") {
    if (is.na(owner$key)) {
      return(list(code = 200, body = list(
        ok = FALSE, identified = FALSE, code = "identity_unavailable",
        error = "身份未识别，不会显示任何已有记录",
        hint = "点「重试识别」，或从平台会话里重新打开这个应用",
        records = list())))
    }
    owner_touch(con, owner)
    n <- as.integer(db_scalar(con, "SELECT COUNT(*) FROM sessions WHERE owner_key = ?",
                              list(owner$key)))
    return(list(code = 200, body = list(
      ok = TRUE, identified = TRUE, owner_key = owner$key, kind = owner$kind,
      record_count = n,
      workspace = file.path("state", "users", gsub("[^A-Za-z0-9_.:@-]", "_", owner$key)))))
  }

  # ---- 数据库状态（自查用）------------------------------------------
  if (p == "/api/db/status") {
    tbl <- function(t) as.integer(db_scalar(con, paste0("SELECT COUNT(*) FROM ", t)))
    return(list(code = 200, body = list(
      ok = TRUE, db = db_path(app_dir),
      schema_version = db_meta_get(con, "schema_version"),
      migrated_at = db_meta_get(con, "migrated_at"),
      counts = list(projects = tbl("projects"), versions = tbl("versions"),
                    runs = tbl("runs"), attempts = tbl("step_attempts"),
                    snapshots = tbl("snapshots"), artifacts = tbl("artifacts"),
                    events = tbl("events"), reviews = tbl("reviews"),
                    tickets = tbl("rework_tickets"), jobs = tbl("poll_jobs"),
                    sessions = tbl("sessions"), decisions = tbl("decisions")))))
  }

  # ---- 交付门（A10）-------------------------------------------------
  if (p == "/api/deliverable") {
    vid <- as.character(qget(q, "version_id") %||% "")
    if (!nzchar(vid)) {
      return(list(code = 400, body = list(
        ok = FALSE, code = "version_required",
        error = "必须指定 version_id。不接受「最新」语义 —— 那正是旧结果盖掉新结果的入口。")))
    }
    d <- db_deliverable(con, vid)
    return(list(code = 200, body = c(list(ok = TRUE), d)))
  }

  # ---- 单版本四轴投影（A8）------------------------------------------
  if (p == "/api/version/status") {
    vid <- as.character(qget(q, "version_id") %||% "")
    if (!nzchar(vid))
      return(list(code = 400, body = list(ok = FALSE, code = "version_required",
                                          error = "必须指定 version_id")))
    return(list(code = 200, body = c(list(ok = TRUE),
                                     tryCatch(db_project_status(con, vid),
                                              error = function(e) list(error = conditionMessage(e))))))
  }

  # ---- 编排：下一环能不能开（F6）------------------------------------
  #   这条回答的是「现在该做什么」，不是「做过什么」。
  #   页面拿它决定按钮能不能点，以及点了之后提示什么。
  if (p == "/api/orch/next") {
    if (is.na(owner$key)) return(list(code = 403, body = list(ok = FALSE, error = "身份未识别")))
    sid <- pre_sid
    vinfo <- tryCatch(db_api_version_of(con, sid), error = function(e) NULL)
    vid <- as.character(vinfo$version_id %||% "")
    if (!nzchar(vid))
      return(list(code = 200, body = list(ok = TRUE, ready = FALSE, stage = NULL,
        blocked_by = "no_version",
        why = "这条分析记录还没有版本，先跑第一步并回写",
        fix = "点第一步「开始这一步」，回写后这里会给出下一环")))
    gp <- tryCatch({
      if (exists("dec_pending", mode = "function")) length(dec_pending(sid)) else 0L
    }, error = function(e) 0L)
    r <- tryCatch(orch_next(con, app_dir, vid,
                            if (nzchar(pre_mode)) pre_mode else "server",
                            gates_pending = gp),
                  error = function(e) list(ready = FALSE, blocked_by = "error",
                                           why = conditionMessage(e), fix = "看服务日志"))
    return(list(code = 200, body = c(list(ok = TRUE, version_id = vid,
                                          gates_pending = gp), r)))
  }

  # ---- 收敛判定（G4）------------------------------------------------
  if (p == "/api/convergence") {
    sid <- pre_sid
    vinfo <- tryCatch(db_api_version_of(con, sid), error = function(e) NULL)
    vid <- as.character(vinfo$version_id %||% "")
    if (!nzchar(vid)) return(list(code = 400, body = list(ok = FALSE, error = "没有对应的版本")))
    r <- tryCatch(orch_convergence(con, vid, as.character(qget(q, "step") %||% NULL)),
                  error = function(e) list(converged = NA, why = conditionMessage(e)))
    return(list(code = 200, body = c(list(ok = TRUE, version_id = vid), r)))
  }

  # ---- 判断留痕（G1）------------------------------------------------
  #   反查：从任一产物/环节，回看影响过它的每次判断。
  if (p == "/api/decisions/trace") {
    sid <- pre_sid
    vinfo <- tryCatch(db_api_version_of(con, sid), error = function(e) NULL)
    vid <- as.character(vinfo$version_id %||% "")
    if (!nzchar(vid)) return(list(code = 400, body = list(ok = FALSE, error = "没有对应的版本")))
    items <- tryCatch(decision_trace(con, vid, as.character(qget(q, "step") %||% NULL)),
                      error = function(e) list())
    return(list(code = 200, body = list(ok = TRUE, version_id = vid, items = items)))
  }

  # ---- pipeline 建议（G2）-------------------------------------------
  #   只生成草稿 + 自检，不改在用文件。合入留给人。
  if (p == "/api/pipeline/suggest" && identical(req$method, "POST")) {
    if (is.na(owner$key)) return(list(code = 403, body = list(ok = FALSE, error = "身份未识别")))
    b <- body_json()
    r <- tryCatch(pipeline_suggest(app_dir,
      action = as.character(b$action %||% ""),
      stage_id = as.character(b$stage %||% ""),
      reason = as.character(b$reason %||% ""),
      drop_stage = as.character(b$drop_stage %||% NULL)),
      error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    db_event(con, "pipeline_suggest", owner_key = owner$key,
             payload = list(action = as.character(b$action %||% ""),
                            stage = as.character(b$stage %||% ""),
                            ok = isTRUE(r$ok)))
    return(list(code = 200, body = r))
  }

  # ---- 数据画像（F1）------------------------------------------------
  #   为什么要一个接口而不是让前端拼：
  #     画像要读四处（位置设置、识别结论、体检结果、参考表），
  #     前端拼这四样等于把「这批数据是什么」的判断搬到浏览器里 ——
  #     那就又变成第二份判据了。
  if (p == "/api/dataset/profile") {
    if (is.na(owner$key)) return(list(code = 403, body = list(ok = FALSE, error = "身份未识别")))
    sid <- pre_sid
    sess <- dp_record(app_dir, sid, con = con)
    place <- sess$place
    env <- sess$identify$result
    ref <- tryCatch({
      rid <- record_trim((sess %||% list())$reference_id)
      if (nzchar(rid) && exists("ref_get", mode = "function")) ref_get(rid) else NULL
    }, error = function(e) NULL)
    profile_input <- sess %||% list()
    profile_input$result <- env
    prof <- tryCatch(dp_build(place = place, identify = profile_input,
                              env = env, ref = ref),
                     error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    if (identical(req$method, "POST")) {
      b <- body_json()
      # 人补的字段优先：画像里留空的项，人在页面上答了就写进去。
      #   为什么不直接采用默认值：物种猜错不会报错，只会拿错参数跑一遍。
      #
      # ---- 与 /api/sessions/update 同一条防污染规则 --------------------
      #   为什么要在这里也做一遍（2026-11-04 第二次加固）
      #     写物种/组织其实有两条路：一条是会话记录的 /api/sessions/update，
      #     另一条就是这里（写数据画像）。上一轮只堵了第一条 ——
      #     等于门锁好了但旁边窗户还开着，同一个错照样能进来。
      #   规则完全一样：值若与这批数据路径里的字样重合，先拦下要求确认，
      #   确认（path_like_ack=true）才放行，并把这个确认记进画像。
      corpusW <- if (exists("dp_path_corpus", mode = "function"))
        dp_path_corpus(place) else ""
      hitW <- character(0)
      if (nzchar(corpusW) && exists("dp_value_in_path", mode = "function")) {
        for (k in c("species", "tissue")) {
          v <- trimws(as.character(b[[k]] %||% ""))
          if (nzchar(v) && dp_value_in_path(v, corpusW))
            hitW <- c(hitW, paste0(if (k == "species") "物种「" else "组织「", v, "」"))
        }
      }
      if (length(hitW) && !isTRUE(b$path_like_ack)) {
        return(list(code = 409, body = list(
          ok = FALSE, code = "value_matches_path",
          error = paste0(paste(hitW, collapse = "、"),
                         " 这几个字也出现在这批数据的数据路径里。"),
          why = "路径名是项目的历史命名，不等于这批数据取的就是那个组织/物种。",
          detail = paste0("261026测试1 就是在这里出过事：有人看到目录名里有 spleen，",
                          "把组织记成了脾脏，实际是小鼠肾脏。"),
          hint = paste0("如果你是根据实验信息填的（不是从目录名抄的），",
                        "带上 path_like_ack=true 再提交一次即可；",
                        "这个确认会被记进数据画像。"),
          matched = as.list(hitW))))
      }
      if (length(hitW) && isTRUE(b$path_like_ack)) {
        prof$path_like_ack_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
        prof$path_like_ack_note <- "填值人与数据路径里的字样重合，已明确确认是按实验信息填的"
      }

      for (k in c("species", "tissue")) {
        v <- as.character(b[[k]] %||% "")
        if (nzchar(v)) { prof[[k]] <- v; prof[[paste0(k, "_source")]] <- "human" }
      }
      if (!is.null(b$samples)) {
        nm <- as.character(unlist(b$samples))
        if (length(nm)) {
          prof$samples <- nm; prof$sample_count <- length(nm)
          prof$samples_source <- "human"
        }
      }
      save_owner <- if (!is.na(pre_owner)) pre_owner else owner$key
      save <- tryCatch(dp_save(app_dir, prof, NULL, owner_key = save_owner),
                       error = function(e) list(error = conditionMessage(e)))
      # 保存后重算 missing：人补过了就不该还挂着
      prof$missing <- Filter(function(m) {
        if (identical(m$key, "species")) is.na(prof$species %||% NA)
        else if (identical(m$key, "samples")) !length(prof$samples %||% character(0))
        else TRUE
      }, prof$missing %||% list())
      prof$ok <- length(prof$missing) == 0L
      db_event(con, "dataset_profile_filled", owner_key = owner$key,
               payload = list(species = prof$species, samples = prof$sample_count,
                              missing = length(prof$missing)))
      return(list(code = 200, body = list(ok = TRUE, session_id = sid,
                                          profile = prof, saved = save)))
    }
    return(list(code = 200, body = list(ok = TRUE, session_id = sid, profile = prof)))
  }

  # ---- 依赖链自检（F4 / G3）-----------------------------------------
  if (p == "/api/flow/deps") {
    r <- tryCatch({
      e <- new.env(parent = globalenv())
      sys.source(file.path(app_dir, "engine", "flow_spec.R"), envir = e)
      e$flow_deps_status("server")
    }, error = function(err) list(ok = FALSE, message = conditionMessage(err), breaks = list()))
    return(list(code = 200, body = c(list(ok = TRUE), r)))
  }

  # ---- 环境能不能跑（F7）--------------------------------------------
  if (p == "/api/runnable") {
    sid <- pre_sid
    env <- dp_record_env(app_dir, sid, con = con)
    r <- tryCatch({
      e <- new.env(parent = globalenv())
      sys.source(file.path(app_dir, "engine", "flow_spec.R"), envir = e)
      for (k in ls(e)) if (is.list(e[[k]]) || is.function(e[[k]])) assign(k, e[[k]], envir = globalenv())
      source(file.path(app_dir, "engine", "runnable.R"))
      runnable_summary(env, if (nzchar(pre_mode)) pre_mode else "server")
    }, error = function(err) list(verdict = "unknown", message = conditionMessage(err),
                                  blocking = list(), unknown = list(), notes = list()))
    return(list(code = 200, body = c(list(ok = TRUE, session_id = sid), r)))
  }

  # ---- 判定面板：页面一次拿全（前端只调这一条）---------------------
  #   为什么做一个聚合端点，而不是让前端各调四条：
  #     四条接口各自有 owner 校验、各自可能返回 403，前端要写四套错误处理；
  #     更麻烦的是"半套数据"——档位拿到了、返工单没拿到，页面就会画出一个
  #     状态自相矛盾的屏。一次拿全，要么全有要么全没有。
  if (p == "/api/judge") {
    if (is.na(owner$key)) return(list(code = 200, body = list(
      ok = FALSE, identified = FALSE, code = "identity_unavailable",
      error = "身份未识别，不显示判定信息",
      hint = "点「重试识别」，或从平台会话里重新打开这个应用")))
    sid <- pre_sid
    vinfo <- tryCatch(db_api_version_of(con, sid), error = function(e) NULL)
    vid <- as.character(vinfo$version_id %||% "")
    mode <- as.character(db_meta_get(con, "review_mode") %||% "warn")
    review_owner <- if (!is.na(pre_owner)) pre_owner else owner$key

    tickets <- db_rows(con,
      # attempt_id 必须带出来：页面的「放行」按钮要用它调 /api/review/release。
      #   不带的话按钮只能拿 ticket_id 去猜是哪一次尝试，猜错了就放行了另一次。
      "SELECT ticket_id, attempt_id, run_id, version_id, step_id, attempt_no, max_auto,
              state, issued_at, due_at, nudges, failed_checks, command_hint, must_do
         FROM rework_tickets WHERE owner_key = ? AND version_id = ?
         ORDER BY issued_at DESC LIMIT 20",
      list(review_owner, vid))
    tk <- if (nrow(tickets)) lapply(seq_len(nrow(tickets)), function(i) {
      r <- as.list(tickets[i, ])
      r$failed_checks <- db_unjson(r$failed_checks, list())
      r$must_do <- db_unjson(r$must_do, list())
      r$instruction <- tryCatch(rework_instruction_text(con, as.character(r$ticket_id)),
                                error = function(e) NULL)
      r
    }) else list()

    steps <- list()
    deliverable <- NULL
    if (nzchar(vid)) {
      st <- tryCatch(db_project_status(con, vid), error = function(e) NULL)
      steps <- st$steps %||% list()
      deliverable <- st$deliverable %||% NULL
    }
    recent <- db_rows(con,
      "SELECT review_id, step_id, verdict, at, checks_json FROM reviews
        WHERE owner_key = ? AND version_id = ? ORDER BY at DESC LIMIT 20",
      list(review_owner, vid))
    reviews <- if (nrow(recent)) lapply(seq_len(nrow(recent)), function(i) {
      r <- as.list(recent[i, ])
      ch <- db_unjson(r$checks_json, list())
      r$checks_json <- NULL
      r$failed <- Filter(function(c) c$status %in% c("fail", "unknown"), ch)
      r$not_implemented <- Filter(function(c) identical(c$status, "not_implemented"), ch)
      r$counts <- list(fail = sum(vapply(ch, function(c) identical(c$status, "fail"), TRUE)),
                       unknown = sum(vapply(ch, function(c) identical(c$status, "unknown"), TRUE)),
                       warn = sum(vapply(ch, function(c) identical(c$status, "warn"), TRUE)),
                       not_impl = length(r$not_implemented))
      r
    }) else list()

    return(list(code = 200, body = list(
      ok = TRUE, identified = TRUE,
      owner_key = review_owner, session_id = sid, version_id = vid %||% NULL,
      mode = mode,
      mode_meaning = switch(mode,
        off = "只记录，不提示",
        warn = "记录 + 顶部红条，但允许继续（默认）",
        block = "拦住下一步，放行必须填理由",
        "未知档位"),
      deliverable = deliverable,
      steps = steps, tickets = tk, reviews = reviews,
      unverified = deliverable$unverified %||% list())))
  }

  # ---- 返工面板（J3：不只写进聊天）-----------------------------------
  if (p == "/api/rework/list") {
    if (is.na(owner$key)) return(list(code = 403, body = list(ok = FALSE, error = "身份未识别")))
    rows <- db_rows(con, "SELECT ticket_id,step_id,attempt_no,max_auto,state,issued_at,due_at,
                            nudges,failed_checks,command_hint
                     FROM rework_tickets WHERE owner_key = ? ORDER BY issued_at DESC LIMIT 50",
                    list(owner$key))
    return(list(code = 200, body = list(ok = TRUE, tickets = rows)))
  }

  if (p == "/api/rework/get") {
    tid <- as.character(qget(q, "ticket_id") %||% "")
    rows <- db_rows(con, "SELECT * FROM rework_tickets WHERE ticket_id = ?", list(tid))
    if (!nrow(rows)) return(list(code = 404, body = list(ok = FALSE, error = "没有这张返工单")))
    t <- as.list(rows[1, ])
    return(list(code = 200, body = list(
      ok = TRUE, ticket = t,
      instruction = rework_instruction_text(con, tid))))
  }

  # ---- 判定档位（E3）：off / warn / block ----------------------------
  #   默认 warn：记录 + 下一步顶部红条，但流程能继续。
  #   为什么默认不是 block：一开始就全拦，人的第一反应是找 force，
  #   而 force 用得越顺手，闭环越像装饰。先让人看见红条，再逐环收紧。
  if (p == "/api/review/mode" && identical(req$method, "GET")) {
    return(list(code = 200, body = list(
      ok = TRUE,
      mode = as.character(db_meta_get(con, "review_mode") %||% "warn"),
      modes = c("off", "warn", "block"),
      meaning = list(off = "只记录，不提示",
                     warn = "记录 + 下一步顶部红条（默认）",
                     block = "拦住下一步，放行必须填理由"))))
  }

  if (p == "/api/review/mode" && identical(req$method, "POST")) {
    b <- body_json()
    want <- as.character(b$mode %||% "")
    if (!want %in% c("off", "warn", "block"))
      return(list(code = 400, body = list(ok = FALSE, error = "mode 只能是 off / warn / block")))
    db_meta_set(con, "review_mode", want)
    db_event(con, "review_mode_changed", owner_key = owner_ctx_key(owner),
             payload = list(mode = want, by = as.character(b$who %||% "unknown")))
    return(list(code = 200, body = list(ok = TRUE, mode = want)))
  }

  # ---- 共享 / 取消共享（B3）------------------------------------------
  #   边界按记录设，不按使用者设：一刀切全私有会把「同一个项目多人看同一份结果」堵死。
  #   共享后对方只读：能看进度、产物、日志，不能跑、不能改配置、不能下载原始数据。
  if (p == "/api/session/share" && identical(req$method, "POST")) {
    if (is.na(owner$key)) return(list(code = 403, body = list(ok = FALSE, error = "身份未识别")))
    b <- body_json()
    sid <- pre_sid
    want <- as.character(b$visibility %||% "private")
    if (!want %in% c("private", "shared"))
      return(list(code = 400, body = list(ok = FALSE, error = "visibility 只能是 private / shared")))
    r <- db_rows(con, "SELECT * FROM sessions WHERE session_id = ?", list(sid))
    if (!nrow(r)) return(list(code = 404, body = list(ok = FALSE, error = "没有这条分析记录")))
    record_owner <- if (!is.na(pre_owner)) pre_owner else as.character(r$owner_key[1])
    if (!identical(as.character(r$owner_key[1]), owner$key) || !identical(record_owner, owner$key))
      return(list(code = 403, body = list(ok = FALSE, code = "not_owner",
        error = "只有记录的所有者能改共享状态")))
    db_tx(con, function() {
      DBI::dbExecute(con, "UPDATE sessions SET visibility=?, updated_at=? WHERE session_id=?",
                     list(want, db_now(), sid))
      db_event(con, "share", owner_key = record_owner, payload = list(
        session_id = sid, visibility = want, at = db_now(),
        by = as.character(b$who %||% owner$key)))
    })
    return(list(code = 200, body = list(ok = TRUE, session_id = sid, visibility = want)))
  }

  # ---- 人工放行（只有人能放行）---------------------------------------
  if (p == "/api/review/release" && identical(req$method, "POST")) {
    b <- body_json()
    att <- as.character(b$attempt_id %||% "")
    who <- as.character(b$who %||% "")
    why <- as.character(b$why %||% "")
    # 身份只从请求上下文取，不从请求体取。
    #   请求体里的 who 是调用方自己写的，谁都能写成 "gina"。
    #   这条路径以前就是这么做的，于是「agent 不能给自己放行」形同虚设。
    r <- tryCatch({
      rework_release(con, att, who, why,
                     verified_actor = owner$key,
                     caller_kind = as.character(b$caller_kind %||% b$actor_kind %||% ""))
      list(ok = TRUE)
    }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    return(list(code = if (isTRUE(r$ok)) 200 else 403, body = r))
  }

  # ---- 判定器手动入口（只给应用内调用）-------------------------------
  if (p == "/api/verdict" && identical(req$method, "POST")) {
    b <- body_json()
    att <- as.character(b$attempt_id %||% "")
    r <- tryCatch(verdict_step(con, app_dir, att, writeback = b$writeback %||% NULL,
                               local_root = as.character(b$local_root %||% "")),
                  error = function(e) list(error = conditionMessage(e)))
    return(list(code = if (is.null(r$error)) 200 else 400, body = r))
  }

  # ---- 产物下载（C1 / C2 / C3）--------------------------------------
  #   为什么不复用 /api/download：那条路的实现是 list.files(runDir) 遍历本机目录。
  #   服务器模式下 runDir 在远端，本地 dir.exists() 恒为假，于是必然 404。
  #   这不是「下载没做」，是「下载只对本地数据有效」。
  #   新端点按三级取数：本地镜像命中 -> 直接流式返回；未命中且可自动取 -> 入队；
  #   大文件 -> 先确认。响应带 Range，断线能续传。
  if (p == "/api/artifact/fetch") {
    if (is.na(owner$key)) return(list(code = 403, body = list(ok = FALSE, error = "身份未识别")))
    sid <- pre_sid
    rel <- as.character(qget(q, "rel") %||% "")
    mode <- as.character(qget(q, "mode") %||% "download")
    if (!nzchar(rel))
      return(list(code = 400, body = list(ok = FALSE, error = "缺 rel")))
    kind <- verdict_kind_of(rel)
    # secret 类默认拒绝：含样本标识的原始表不是「点一下就能下」的东西。
    if (identical(kind, "secret"))
      return(list(code = 403, body = list(ok = FALSE, code = "secret_requires_grant",
        error = "这一类产物含样本标识，需要单独授权后才能取回")))

    lp <- if (exists("mirror_path", mode = "function")) mirror_path(sid, rel) else ""
    if (nzchar(lp) && file.exists(lp)) {
      return(list(code = 200, body = list(ok = TRUE, ready = TRUE, path = lp,
                                          bytes = file.info(lp)$size, kind = kind,
                                          session_id = sid,
                                          md5 = unname(tools::md5sum(lp)))))
    }
    if (is.null(pre_cap) || !isTRUE(pre_cap$can_write)) {
      return(list(code = 403, body = list(ok = FALSE, code = "fetch_requires_owner",
        session_id = sid,
        error = "shared 记录只能读取已经取回的产物；创建取回任务需要记录所有者。")))
    }

    req_id <- db_new_id("fx")
    job_owner <- if (!is.na(pre_owner)) pre_owner else owner$key
    job_vid <- if (nzchar(pre_vid)) pre_vid else as.character(qget(q, "version_id") %||% NA)
    big <- identical(kind, "large") || identical(kind, "ondemand")
    need_confirm <- big && !isTRUE(as.logical(qget(q, "confirm") %||% FALSE))
    DBI::dbExecute(con,
      "INSERT INTO fetch_jobs(request_id,owner_key,version_id,rel,state,created_at,updated_at)
       VALUES(?,?,?,?,?,?,?)",
      list(req_id, job_owner, job_vid,
           rel, if (need_confirm) "pending" else "pending", db_now(), db_now()))
    DBI::dbExecute(con,
      "INSERT INTO poll_jobs(job_id,kind,owner_key,version_id,ticket_id,state,
         next_check_at,created_at,updated_at)
       VALUES(?,?,?,?,?,?,?,?,?)",
      list(paste0("job-", req_id), "fetch", job_owner, job_vid, req_id,
           "pending", db_now(), db_now(), db_now()))
    return(list(code = 202, body = list(
      ok = TRUE, ready = FALSE, request_id = req_id, kind = kind, session_id = sid,
      need_confirm = need_confirm,
      error = if (need_confirm)
        paste0("这个产物属于 ", kind, " 类，体积可能很大。确认取回请带 confirm=1。") else NULL,
      hint = if (need_confirm) "确认后会进入取回队列，页面显示进度；中断可续传。" else
        "已进入取回队列，就绪后再次请求同一 rel 会直接返回本地文件。")))
  }

  # ---- 引擎资产分发（D2）--------------------------------------------
  #   为什么必须单独一条通道：模板 16 MB，base64 之后约 21.3 MB，
  #   塞进一个 JSON 响应里会让执行端等很久、内存峰值翻倍，还可能直接超时。
  #   补充脚本（几 KB）继续走原来的 /api/supplement/get，不重复造一条。
  #   这条只做三件事：报清单、给分片、单片取。拼装与校验在执行端做。
  #
  #   安全：只允许 engine/ 下的白名单目录，且禁止任何 .. 或绝对路径穿越。
  #   为什么不用「engine/ 下随便读」：那等于给了一个读源码的接口，
  #   以后有人把密钥写到 engine 目录里，这条通道就成了后门。
  if (p == "/api/engine-asset") {
    rel <- as.character(qget(q, "path") %||% "")
    if (!nzchar(rel))
      return(list(code = 400, body = list(ok = FALSE,
        error = "缺 path，例如 sc-ppt-kit/templates/template.pptx")))
    fp <- db_api_engine_asset_path(app_dir, rel)
    if (is.null(fp))
      return(list(code = 403, body = list(ok = FALSE, code = "asset_not_allowed",
        error = "这个路径不在可分发的白名单里",
        allowed = DB_API_ASSET_ROOTS)))
    if (!file.exists(fp) || dir.exists(fp))
      return(list(code = 404, body = list(ok = FALSE, error = "引擎里没有这个文件",
                                          path = rel)))

    info <- file.info(fp)
    md5 <- unname(tools::md5sum(fp))
    chunk_size <- 2L * 1024L * 1024L
    total <- as.integer(info$size)
    nchunks <- max(1L, as.integer(ceiling(total / chunk_size)))

    # 不带 chunk 时只回清单：先让执行端知道多大、几片、什么摘要。
    if (is.null(q[["chunk"]])) {
      return(list(code = 200, body = list(
        ok = TRUE, path = rel, bytes = total, md5 = md5,
        chunk_size = chunk_size, chunks = nchunks,
        howto = "逐片 GET /api/engine-asset?path=<同一路径>&chunk=<0..n-1>，取 b64 字段拼接后校验 md5")))
    }

    idx <- suppressWarnings(as.integer(q[["chunk"]]))
    if (is.na(idx) || idx < 0L || idx >= nchunks)
      return(list(code = 400, body = list(ok = FALSE,
        error = paste0("chunk 超出范围，应为 0..", nchunks - 1L))))

    fc <- file(fp, open = "rb")
    on.exit(close(fc), add = TRUE)
    seek(fc, where = idx * chunk_size, origin = "start")
    raw <- readBin(fc, "raw", chunk_size)
    return(list(code = 200, body = list(
      ok = TRUE, path = rel, chunk = idx, chunks = nchunks,
      offset = idx * chunk_size, len = length(raw),
      b64 = jsonlite::base64_enc(raw),
      md5 = md5, bytes = total)))
  }

  # ---- 取回进度（C3）------------------------------------------------
  if (p == "/api/artifact/fetch/status") {
    rid <- as.character(qget(q, "request_id") %||% "")
    sid <- pre_sid
    job_owner <- if (!is.na(pre_owner)) pre_owner else owner$key
    sql <- "SELECT * FROM fetch_jobs WHERE request_id = ? AND owner_key = ?"
    args <- list(rid, job_owner)
    if (nzchar(pre_vid)) {
      sql <- paste0(sql, " AND version_id = ?")
      args <- c(args, list(pre_vid))
    }
    r <- db_rows(con, sql, args)
    if (!nrow(r)) return(list(code = 404, body = list(ok = FALSE, error = "没有这个取回任务")))
    return(list(code = 200, body = c(list(ok = TRUE, session_id = sid,
                                          job = as.list(r[1, ])),
                                     record_cap_fields(pre_cap))))
  }

  # ---- 分片取回（C2：大文件专用）-------------------------------------
  #   16 MB 模板 base64 进 JSON 会让单次请求超时、内存峰值翻倍。
  #   这里给分片清单，执行端逐片 POST，服务端拼接后整体校验 md5。
  if (p == "/api/artifact/chunk") {
    if (identical(req$method, "GET")) {
      rid <- as.character(qget(q, "request_id") %||% "")
      chunk <- 4L * 1024L * 1024L
      r <- db_rows(con, "SELECT * FROM fetch_jobs WHERE request_id = ?", list(rid))
      if (!nrow(r)) return(list(code = 404, body = list(ok = FALSE, error = "没有这个取回任务")))
      if (!is.na(owner$key) && !identical(as.character(r$owner_key[1]), owner$key))
        return(list(code = 403, body = list(ok = FALSE, code = "fetch_owner_mismatch",
          error = "这个取回任务不属于请求者。未返回任何分片信息。")))
      sid <- db_api_fetch_session(con, r)
      if (!nzchar(sid))
        return(list(code = 409, body = list(ok = FALSE, code = "fetch_session_ambiguous",
          error = "无法把这个取回任务唯一绑定到一条分析记录；未返回分片信息。")))
      sid_req <- as.character(qget(q, "session") %||% qget(q, "session_id") %||% "")
      rel_req <- as.character(qget(q, "rel") %||% "")
      if ((nzchar(sid_req) && !identical(sid_req, sid)) ||
          (nzchar(rel_req) && !identical(rel_req, as.character(r$rel[1]))))
        return(list(code = 409, body = list(ok = FALSE, code = "fetch_binding_mismatch",
          request_id = rid, session_id = sid, rel = as.character(r$rel[1]),
          error = "request_id 与 session/rel 不属于同一个已登记取回任务；未返回分片信息。")))
      total <- suppressWarnings(as.numeric(r$expected_bytes[1]))
      if (is.na(total)) return(list(code = 400, body = list(ok = FALSE,
        error = "不知道总大小，无法切片；请先给 expected_bytes")))
      n <- ceiling(total / chunk)
      return(list(code = 200, body = list(ok = TRUE, request_id = rid, session_id = sid,
        chunk_size = chunk, total_bytes = total,
        chunks = lapply(seq_len(n) - 1L, function(i) list(
          chunk_index = i, offset = i * chunk,
          len = min(chunk, total - i * chunk))))))
    }
    b <- body_json()
    rid <- as.character(b$request_id %||% "")
    idx <- suppressWarnings(as.integer(b$chunk_index %||% NA))
    r <- db_rows(con, "SELECT * FROM fetch_jobs WHERE request_id = ?", list(rid))
    if (!nrow(r) || is.na(idx))
      return(list(code = 400, body = list(ok = FALSE, error = "缺 request_id 或 chunk_index")))
    if (!is.na(owner$key) && !identical(as.character(r$owner_key[1]), owner$key))
      return(list(code = 403, body = list(ok = FALSE, code = "fetch_owner_mismatch",
        error = "这个取回任务不属于请求者。未写入任何分片。")))
    sid <- db_api_fetch_session(con, r)
    if (!nzchar(sid))
      return(list(code = 409, body = list(ok = FALSE, code = "fetch_session_ambiguous",
        error = "无法把这个取回任务唯一绑定到一条分析记录；未写入任何分片。")))
    sid_req <- as.character(b$session_id %||% b$session %||% "")
    rel_req <- as.character(b$rel %||% "")
    if ((nzchar(sid_req) && !identical(sid_req, sid)) ||
        (nzchar(rel_req) && !identical(rel_req, as.character(r$rel[1]))))
      return(list(code = 409, body = list(ok = FALSE, code = "fetch_binding_mismatch",
        request_id = rid, session_id = sid, rel = as.character(r$rel[1]),
        error = "request_id 与 session/rel 不属于同一个已登记取回任务；未写入任何分片。")))
    part <- tryCatch(jsonlite::base64_dec(as.character(b$b64 %||% "")), error = function(e) NULL)
    if (is.null(part) || !length(part))
      return(list(code = 400, body = list(ok = FALSE, error = "分片内容为空或不是 base64")))
    lp <- if (exists("mirror_path", mode = "function"))
      mirror_path(sid, as.character(r$rel[1])) else ""
    if (!nzchar(lp))
      return(list(code = 400, body = list(ok = FALSE, error = "取回路径不合法")))
    dir.create(dirname(lp), showWarnings = FALSE, recursive = TRUE)
    # 按偏移写入而不是追加：重传同一片不会把文件写长一截。
    fc <- file(lp, open = "r+b")
    if (!file.exists(lp)) { close(fc); fc <- file(lp, open = "w+b") }
    seek(fc, where = idx * 4L * 1024L * 1024L, origin = "start")
    writeBin(part, fc); close(fc)
    got <- file.info(lp)$size
    DBI::dbExecute(con, "UPDATE fetch_jobs SET received_bytes=?, state='fetching', updated_at=?
                         WHERE request_id=?", list(got, db_now(), rid))
    done <- !is.na(suppressWarnings(as.numeric(r$expected_bytes[1]))) &&
      got >= as.numeric(r$expected_bytes[1])
    if (done) {
      md5 <- unname(tools::md5sum(lp))
      want <- as.character(r$expected_md5[1] %||% "")
      if (nzchar(want) && !identical(md5, want)) {
        unlink(lp)
        DBI::dbExecute(con, "UPDATE fetch_jobs SET state='failed', last_error=?, updated_at=?
                             WHERE request_id=?", list("md5_mismatch", db_now(), rid))
        return(list(code = 409, body = list(ok = FALSE, code = "md5_mismatch",
          error = "拼接后校验和不符，已删除半成品", expected = want, actual = md5)))
      }
      DBI::dbExecute(con, "UPDATE fetch_jobs SET state='verified', local_path=?, updated_at=?
                           WHERE request_id=?", list(lp, db_now(), rid))
    }
    return(list(code = 200, body = list(ok = TRUE, request_id = rid, session_id = sid,
                                        chunk_index = idx, received = got,
                                        complete = done)))
  }

  # ---- 多选打包（C5）------------------------------------------------
  if (p == "/api/artifact/bundle" && identical(req$method, "POST")) {
    b <- body_json()
    rels <- as.character(unlist(b$rels %||% character(0)))
    if (!length(rels)) return(list(code = 400, body = list(ok = FALSE, error = "没勾选任何产物")))
    if (!nzchar(pre_sid) || is.null(pre_cap) || !isTRUE(pre_cap$can_write))
      return(list(code = 403, body = list(ok = FALSE, code = "not_owner",
        error = "只有记录所有者可以打包这条记录的产物。")))
    paths <- vapply(rels, function(r)
      if (exists("mirror_path", mode = "function")) mirror_path(pre_sid, r) else "", "")
    missing <- rels[!nzchar(paths) | !file.exists(paths)]
    if (length(missing))
      return(list(code = 409, body = list(ok = FALSE, code = "not_fetched",
        session_id = pre_sid,
        error = "有产物还没取回到本地，先取回再打包", missing = missing)))
    out <- file.path(tempdir(), paste0("bundle-", db_new_id("b"), ".zip"))
    # 清单与文件一一对应：解压后能回答「这份是谁的、哪一轮、哪个环节」。
    man <- lapply(seq_along(rels), function(i) list(
      rel = rels[i], bytes = file.info(paths[i])$size,
      md5 = unname(tools::md5sum(paths[i])),
      session_id = pre_sid, owner_key = pre_owner,
      version_id = if (nzchar(pre_vid)) pre_vid else as.character(b$version_id %||% NA)))
    manf <- file.path(dirname(out), paste0(tools::file_path_sans_ext(basename(out)), "_manifest.json"))
    writeLines(jsonlite::toJSON(man, auto_unbox = TRUE, pretty = TRUE), manf, useBytes = TRUE)
    utils::zip(out, files = c(paths, manf), flags = "-j -q")
    return(list(code = 200, body = c(list(ok = TRUE, session_id = pre_sid,
                                          zip = out, items = length(rels), manifest = manf),
                                     record_cap_fields(pre_cap))))
  }

  NULL
}

# 本地镜像路径：按 owner 分区（B2）。
#   为什么不沿用 state/mirror/<session>：那是全局单目录，
#   两个人各自的 session id 不冲突但目录是一棵，
#   删库、清缓存、改权限时会一起受影响。
db_api_local_path <- function(app_dir, owner_key, rel) {
  ok <- gsub("[^A-Za-z0-9_.:@-]", "_", as.character(owner_key %||% "anonymous"))
  safe <- gsub("\\.\\.", "_", gsub("\\\\", "/", as.character(rel %||% "")))
  file.path(app_dir, "state", "users", ok, "mirror", safe)
}

# 返回 list(accepted, verdict, ticket_id, failed_checks, attempt_id)
db_api_report_step <- function(con, app_dir, owner, b, runDir, sid) {
  if (is.null(con)) return(NULL)

  step_id <- as.character(b$step %||% "")
  if (!nzchar(step_id)) return(NULL)

  owner_key <- if (is.na(owner$key)) "unattributed" else owner$key
  # 分析记录 -> project / version / run 的映射。缺哪层就建哪层，不猜已有记录。
  sess_row <- db_rows(con, "SELECT * FROM sessions WHERE session_id = ?", list(sid))
  if (nrow(sess_row)) {
    sr <- as.list(sess_row[1, ])
    # 只有「非 NA、非空」的已有 ID 才能复用，其余一律走兜底建号。
    #   原写法是 sr$project_id %||% db_ensure_project(...)：%||% 只认 is.null
    #   与长度 0，而 DBI 把 SQL NULL 读成 NA_character_（长度 1），兜底永远不触发。
    #   于是 NA 被原样带进 step_attempts.version_id，NOT NULL 约束直接失败，
    #   整份回写被判 db_report_error —— 2026-11-12 37li 那次就是这么丢的。
    #   已有的真实 ID 仍然照用，owner_key 隔离语义不变（两条兜底都在 owner_key 下建号）。
    sess_id_of <- function(x) {
      v <- suppressWarnings(as.character(x))
      if (!length(v) || is.na(v[1])) return(NA_character_)
      v <- trimws(v[1])
      if (!nzchar(v)) return(NA_character_)
      v
    }
    pid <- sess_id_of(sr$project_id)
    if (is.na(pid)) pid <- as.character(db_ensure_project(con, owner_key, sr$name %||% sid))
    vid <- sess_id_of(sr$version_id)
    if (is.na(vid)) vid <- as.character(db_ensure_version(con, pid, owner_key))
    # 建出来的版本号必须写回分析记录（2026-11-18 修）。
    #   原来只在内存里算一次，sessions.version_id 一直是 NULL ——
    #   于是每一条回写都会再建一个新版本：doublet 在 V1、merge 在 V2、markers 在 V3……
    #   同一条分析记录的产物散落在不同版本里，闸门按版本查上游就查不到，
    #   回写也会被误判成 stale_write。写回之后同一条记录的所有环节共用同一个版本。
    if (!is.na(pid) && !is.na(vid) &&
        (is.na(sess_id_of(sr$version_id)) || is.na(sess_id_of(sr$project_id))))
      tryCatch(DBI::dbExecute(con,
        "UPDATE sessions SET project_id = COALESCE(project_id, ?),
                             version_id = COALESCE(version_id, ?)
         WHERE session_id = ?", list(pid, vid, sid)), error = function(e) NULL)
    rid <- db_ensure_run(con, vid, owner_key, work_root = as.character(sr$run_dir %||% runDir))
  } else {
    # 没有这条分析记录时，顺手把记录建出来。
    #   为什么必须建：/api/run 的闸门靠 sessions 表把 session 映射到 version，
    #   如果只建 project/version/run 而不建 session，闸门查不到版本，
    #   就会「静默跳过检查」—— 那是一个不出声的绕过口，比拦错更难发现。
    pid <- db_ensure_project(con, owner_key, paste0("记录 ", if (nzchar(sid)) sid else "未命名"))
    vid <- db_ensure_version(con, pid, owner_key)
    rid <- db_ensure_run(con, vid, owner_key, work_root = runDir)
    if (nzchar(sid)) {
      DBI::dbExecute(con,
        "INSERT OR IGNORE INTO sessions(session_id,owner_key,name,mode,run_dir,project_id,
           version_id,visibility,created_at,updated_at,payload)
         VALUES(?,?,?,?,?,?,?,?,?,?,?)",
        list(sid, owner_key, paste0("记录 ", sid), NA_character_,
             runDir %||% NA_character_, pid, vid, "private", db_now(), db_now(),
             db_json(list(auto_created = TRUE,
                          why = "首次回写时补建，保证闸门能查到版本"))))
    }
  }

  req_id <- trimws(as.character(b$req_id %||% ""))
  ticket_in <- trimws(as.character(b$ticket_id %||% ""))

  # 先看这次回写是否对应一个已存在的 attempt（带 req_id 或 ticket_id 的返工回写）。
  #   必须带上 step_id：整条链共用一个请求号，不限定环节就会命中上一环那条已验收记录，
  #   进而被判 stale_write。口径与链条记账 (chain_id, step) 一致。
  att <- db_attempt_by_req(con, req_id, step_id)
  if (is.null(att) && nzchar(ticket_in)) {
    r <- db_rows(con, "SELECT * FROM step_attempts WHERE ticket_id = ? ORDER BY attempt_no DESC LIMIT 1",
                 list(ticket_in))
    if (nrow(r)) att <- as.list(r[1, ])
  }

  # 旧 attempt 晚到：比 attempt_no 与 req_id，小者丢弃并记 stale_write（A7）。
  if (!is.null(att) && identical(as.character(att$status), "verified") &&
      !identical(as.character(att$version_id), vid)) {
    db_event(con, "stale_write", owner_key = owner_key, version_id = att$version_id,
             step_id = step_id, attempt_id = att$attempt_id,
             payload = list(req_id = req_id, why = "版本不一致"))
    return(list(accepted = FALSE, code = "stale_write",
                error = "这份回写属于别的版本，不能写入当前记录"))
  }

  # ---- 同一步骤的再次申报要新开一次尝试（J2 / F8）------------------
  #   原来的行为：同一个 req_id 二次回写直接 UPDATE 那一行，attempt_no 不变。
  #   后果有两个，都很具体：
  #     ① 返工轮次永远算作第 1 次，max_auto=3 的上限失效，催不动；
  #     ② 「重跑后旧 attempt 仍可查」不成立 —— 旧结论被覆盖，事后无法对比
  #        「上一轮错在哪、这一轮改了什么」。
  #   现在的规则：只有「这条回写是同一请求的重试」才沿用原行；
  #   已经判过（rejected / verified / released / escalated）的，一律新开一行。
  if (!is.null(att)) {
    closed <- as.character(att$status) %in% c("rejected", "verified", "released", "escalated")
    if (closed) {
      db_event(con, "reopen_attempt", owner_key = owner_key, version_id = vid,
               run_id = rid, step_id = step_id, attempt_id = att$attempt_id,
               payload = list(prev_status = as.character(att$status),
                              prev_attempt_no = as.integer(att$attempt_no),
                              why = "对上一步的再次申报：新开一次尝试，旧结论保留"))
      att <- NULL
    }
  }

  if (is.null(att)) {
    aid <- db_new_id("att")
    no <- db_next_attempt_no(con, rid, step_id)
    db_tx(con, function() {
      DBI::dbExecute(con,
        "INSERT INTO step_attempts(attempt_id,run_id,version_id,owner_key,step_id,attempt_no,
           req_id,ticket_id,script_path,script_digest,exit_code,status,reported_at,started_at,ended_at,payload)
         VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
        list(aid, rid, vid, owner_key, step_id, no, req_id %||% NA, ticket_in %||% NA,
             as.character(b$script_path %||% b$script %||% NA),
             as.character(b$script_md5 %||% b$script_digest %||% NA),
             suppressWarnings(as.integer(b$exit_code %||% b$code %||% NA)),
             "reported", db_now(),
             as.character(b$started_at %||% NA), as.character(b$ended_at %||% NA),
             db_json(b)))
    })
  } else {
    aid <- as.character(att$attempt_id)
    # 拒绝写入 verified：任何外部请求都不许把状态直接写成 verified（I1）。
    if (identical(as.character(b$status %||% ""), "verified")) {
      db_event(con, "rejected_direct_verified", owner_key = owner_key,
               version_id = att$version_id, step_id = step_id, attempt_id = aid,
               payload = list(who = as.character(b$session_id %||% "")))
      return(list(accepted = FALSE, code = "verified_not_writable",
                  error = "verified 只能由判定器写入，外部回写一律不收"))
    }
    db_tx(con, function() {
      DBI::dbExecute(con,
        "UPDATE step_attempts SET payload=?, reported_at=?, exit_code=?, script_path=?, script_digest=?
          WHERE attempt_id=?",
        list(db_json(b), db_now(),
             suppressWarnings(as.integer(b$exit_code %||% b$code %||% NA)),
             as.character(b$script_path %||% b$script %||% NA),
             as.character(b$script_md5 %||% b$script_digest %||% NA), aid))
    })
  }

  # 台账登记：finish 行带 set_id / 脚本路径 / 脚本指纹 / 退出码。
  #   为什么放在这里：这是唯一一处「外部声称某步跑完」的入口。
  #   台账如果不在这一刻写，就永远不会写 —— 之前就是 0 行，
  #   K1 第 7 项的防篡改守卫保护的是一个空表。
  tryCatch(db_ledger_append(
    con, run_id = rid, set_id = as.character(b$set_id %||% b$skill %||% ""),
    script_path = as.character(b$script_path %||% b$script %||% ""),
    script_md5 = as.character(b$script_md5 %||% b$script_digest %||% ""),
    phase = "finish",
    payload = list(step_id = step_id, attempt_id = aid,
                   exit_code = suppressWarnings(as.integer(b$exit_code %||% b$code %||% NA)),
                   req_id = req_id, ticket_id = ticket_in)),
    error = function(e) NULL)

  # 「done」只表示「我跑完了」。存 reported，然后立刻判。
  db_event(con, "reported", owner_key = owner_key, version_id = vid, run_id = rid,
           step_id = step_id, attempt_id = aid,
           payload = list(req_id = req_id, claimed_status = as.character(b$status %||% "done")))

  vr <- tryCatch(verdict_step(con, app_dir, aid, writeback = b,
                              local_root = as.character(b$local_root %||% "")),
                 error = function(e) list(error = conditionMessage(e)))

  if (!is.null(vr$error)) {
    return(list(accepted = FALSE, code = "verdict_failed", error = vr$error,
                attempt_id = aid, status = "reported"))
  }

  # 通过口径：pass 与 warn 都算通过，fail 与 unknown 不算。
  #   为什么 warn 要算通过：E3 定的默认档就是 warn —— 记录 + 下一步顶部红条，
  #   但流程能继续。如果把 warn 也判成不通过，正常产物会因为「没给环境锁定」
  #   这种非阻断项被退回，人会开始习惯性放行，闭环又退化成提示。
  #   warn 与 fail 的区别仍然留在 reviews.verdict 里，页面红条读它。
  accepted <- vr$verdict %in% c("pass", "warn")
  failed <- Filter(function(c) c$status %in% c("fail", "unknown"), vr$checks)

  # ---- 判定结果是唯一能写 verified 的地方（I1 的核心）---------------
  #   这一步漏掉的后果非常具体：判定通过的那一步状态还停在 reported，
  #   而闸门只认 verified —— 于是每一环都 409，流程被锁成死路，
  #   人只能去按 force，闭环等于没有。
  #   「只有判定器能写」的含义是：只有这一条代码路径写 verified，
  #   HTTP 回写入口、页面按钮、agent 都不能写（入口已拒 status=verified）。
  if (accepted) {
    DBI::dbExecute(con,
      "UPDATE step_attempts SET status='verified', verified_at=? WHERE attempt_id=?",
      list(db_now(), aid))
    db_event(con, "verified", owner_key = owner_key, version_id = vid, run_id = rid,
             step_id = step_id, attempt_id = aid,
             payload = list(verdict = vr$verdict, by = "verdict.R"))
  }

  list(accepted = accepted,
       verdict = vr$verdict,
       # 判定里解出来的 set_id 与来源：护照要用同一份事实，不能自己再猜一次。
       set_id = vr$set_id,
       set_id_provenance = vr$set_id_provenance,
       attempt_id = aid,
       ticket_id = if (is.na(vr$ticket_id %||% NA)) NULL else vr$ticket_id,
       status = if (accepted) "verified" else "rejected",
       failed_checks = lapply(failed, function(c) list(
         layer = c$layer, name = c$name, expected = c$expected,
         actual = c$actual, evidence = c$evidence, detail = c$detail)),
       claims_ignored = vr$claims_ignored)
}

# ---- 闸门：上一步不是 verified 就不许开工 ----------------------------

db_api_gate <- function(con, app_dir, owner, version_id, step_id) {
  if (is.null(con) || is.null(version_id) || !nzchar(version_id)) return(NULL)
  order <- tryCatch(flow_run_order_engine(app_dir), error = function(e) character(0))
  i <- match(step_id, order)
  if (is.na(i) || i <= 1L) return(NULL)
  prev <- order[i - 1L]
  r <- db_rows(con, "SELECT * FROM step_attempts WHERE version_id=? AND step_id=?
                     ORDER BY attempt_no DESC LIMIT 1", list(version_id, prev))
  if (!nrow(r)) {
    return(list(code = 409, body = list(
      ok = FALSE, code = "upstream_not_verified", step = step_id, upstream = prev,
      error = paste0("上一步 ", prev, " 还没有任何回写，不能开工。"),
      hint = "先跑上一步并按契约回写；人工放行只能由人填理由后操作。")))
  }
  r <- as.list(r[1, ])
  st <- as.character(r$status)
  # 迁移来的历史步骤没有可核事实（旧 JSON 里没有 set_id / md5 / 退出码）。
  #   它们的 status 一律是 reported，不是 verified，所以本来就该拦住。
  #   但要把原因说清楚：不是「你做错了」，是「这一轮从来没被核过」。
  legacy <- isTRUE(db_unjson(r$payload, list())$migrated)
  if (!st %in% c("verified", "released")) {
    # 判定档位真的影响闸门（E3）：
    #   off  只记录，不拦；
    #   warn 记红条但放行（默认）—— 先让人看见，再逐环收紧；
    #   block 直接 409。
    #   这一条以前只把档位存进库、没有任何地方读它，
    #   于是「三档」只是三个字符串，不是三种行为。
    mode <- as.character(db_meta_get(con, "review_mode") %||% "warn")
    if (identical(mode, "off")) return(NULL)
    if (identical(mode, "warn")) {
      tk0 <- as.character(r$ticket_id %||% "")
      return(list(code = 409, body = list(
        ok = TRUE, warn_only = TRUE, code = "upstream_not_verified",
        step = step_id, upstream = prev, upstream_status = st,
        ticket_id = if (nzchar(tk0)) tk0 else NULL,
        error = paste0("上一步 ", prev, " 的状态是 ", st,
                       "，不是 verified。当前档位是 warn：允许继续，但这一条会记进交付清单的未核验项。"),
        hint = "要真正拦住，把 /api/review/mode 设为 block。查红条文案用 /api/review/mode。")))
    }
    tk <- as.character(r$ticket_id %||% "")
    return(list(code = 409, body = list(
      ok = FALSE,
      code = if (legacy) "legacy_unverified" else "upstream_not_verified",
      step = step_id, upstream = prev,
      upstream_status = st,
      ticket_id = if (nzchar(tk)) tk else NULL,
      error = if (legacy)
        paste0("上一步 ", prev, " 是历史数据迁移来的，从来没有经过判定，不能当通过。")
      else paste0("上一步 ", prev, " 的状态是 ", st, "，不是 verified。未通过判定不能往下走。"),
      hint = if (legacy)
        paste0("重跑一次 ", prev, " 并按契约回写（带产物清单、退出码、脚本指纹与 set_id），它会进入判定。")
      else "按返工单修正后重跑上一步；人工放行需要填理由并记录到交付清单。")))
  }
  NULL
}

# ---- 当前分析记录对应的 version -------------------------------------

db_api_version_of <- function(con, sid) {
  if (is.null(con) || is.null(sid) || !nzchar(sid)) return(NULL)
  r <- db_rows(con, "SELECT * FROM sessions WHERE session_id = ?", list(sid))
  if (!nrow(r)) return(NULL)
  list(session_id = sid,
       project_id = as.character(r$project_id[1] %||% NA),
       version_id = as.character(r$version_id[1] %||% NA),
       run_dir = as.character(r$run_dir[1] %||% NA))
}

# Both JSON and download use the same DB-authorized record selector.
db_api_run_logs <- function(con, owner, sid, tail = NULL) {
  key <- owner_ctx_key(owner)
  if (!db_run_log_authorized(con, key, sid))
    return(list(code = 403L, body = list(ok = FALSE, code = "run_log_not_owner",
      error = "无权读取这条分析记录的运行日志")))
  rows <- db_run_log_read(con, key, sid, tail)
  list(code = 200L, body = list(ok = TRUE, session_id = sid, read_at = db_now(), entries = rows),
       text = db_run_log_text(rows))
}

# Notebook storage uses a session path; authorize before any filesystem access.
db_api_notebook_authorized <- function(con, owner, sid) {
  if (!is.character(sid) || length(sid) != 1L || is.na(sid) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_-]{0,127}$", sid)) return(FALSE)
  tryCatch(isTRUE(db_run_log_authorized(con, owner_ctx_key(owner), sid)),
           error = function(e) FALSE)
}
