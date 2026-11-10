#!/usr/bin/env Rscript
# =====================================================================
#  intent_bridge.R -- 把「一段话」变成应用里的一条记录
#
#  为什么需要这一层
#    用户不会照着表单填空。他在会话里说的话是这种形状：
#      「帮我把这个数据拷贝到 ~/project/00.test/00.rawdata，
#        参考 scRNA_analysis_GPU.ipynb 改 2、3 cell，
#        按这个组别分组，其他不动，跑到 08 之前，
#        根据 cluster 的 umap 空间位置和 top 基因注释大群，给我审核」
#    这句话里有七八个字段，但要落到应用里，必须变成确定的键值。
#    中间加一层显式 intent，解决三件事：
#      1. 可核对    —— 人能看到「我理解成这些」，而不是只看到「已创建」；
#      2. 可追问    —— 缺的项留空并列出来，不靠猜；
#      3. 可重放    —— 同一句话重复提交不会造出两条记录。
#
#  为什么不在 serve.R 里做
#    serve.R 已经是七千行，再加一套 schema 校验和幂等逻辑，
#    下一个读的人分不清哪些是页面收发、哪些是业务规则。
#    这里只做「校验 + 幂等 + 落地」，HTTP 部分留在 serve.R。
#
#  谁可以调用
#    只有带有效助手令牌的请求，且令牌绑定的 owner_key 就是这个人的。
#    没有令牌的请求一律拒绝 —— 这条路径能建记录，不能对匿名开放。
# =====================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

INTENT_SCHEMA <- "pa.intent.v1"
INTENT_MAX_TEXT <- 20000L      # 一段话再长也不该超过这个；超了是附件不是话
INTENT_MAX_FIELD <- 500L

# ---- 字段清单（唯一出处）-------------------------------------------
#   mode/step 之外的字符串都有长度上限。上限的作用不是防攻击，
#   是防止一整篇文档被塞进一个字段里，之后谁也不知道那是什么。

intent_allowed_keys <- function() {
  c("schema", "project_name", "mode", "server", "paths", "samples",
    "reference", "scope", "annotation", "species", "tissue",
    "missing", "source")
}

# 单个标量的清洗：去首尾空白、限长、拒绝控制字符。
intent_scalar <- function(v, max_len = INTENT_MAX_FIELD) {
  if (is.null(v) || !length(v)) return(NULL)
  if (is.list(v)) return(NULL)
  s <- trimws(as.character(v)[1])
  if (is.na(s) || !nzchar(s)) return(NULL)
  # 控制字符（除换行外）不该出现在任何字段里：它们往往意味着
  #   有人想把多行内容塞进单行字段，之后展示时会错位。
  s <- gsub("[\x01-\x09\x0b-\x1f]", "", s, perl = TRUE)
  if (nchar(s, type = "chars") > max_len) return(NULL)
  s
}

# 路径类字段：只做形状检查，不判存在性 —— 应用到不了那台服务器。
intent_path <- function(v) {
  s <- intent_scalar(v, 1000L)
  if (is.null(s)) return(NULL)
  if (grepl("(^|/)\\.\\.(/|$)", s)) return(NULL)
  s
}

# ---- 规范化 ---------------------------------------------------------

# 返回 list(ok, intent, errors)
#   不抛异常：调用方要把错误原文回给助手，让它能改。
intent_normalize <- function(raw) {
  errors <- list()
  if (!is.list(raw)) return(list(ok = FALSE, intent = NULL,
                                 errors = list("body 必须是 JSON 对象")))

  unknown <- setdiff(names(raw), intent_allowed_keys())
  if (length(unknown))
    errors[[length(errors) + 1L]] <- paste0("不认识的字段：", paste(unknown, collapse = "、"))

  # Reject misspellings and malformed nested data before scalar conversion.
  # runtime keys cover place.R and the actual flow_vars consumers in serve.R.
  sections <- list(
    server = c("label", "host", "user", "port", "exec_pc", "samples", "runtime"),
    paths = c("data_root", "work_root", "rawdata_dir", "script_root", "liny_data"),
    samples = c("count", "group_table", "groups"),
    reference = c("notebook", "modified_cells"),
    scope = c("run_until", "annotation_gate"),
    annotation = c("policy", "merge_adjacent"),
    source = c("topic_id", "message_id", "raw_text"))
  runtime_keys <- c("rscript", "lib_path", "ld_library_path", "python", "queue",
                    "qsub", "hdf5", "node_constraint")
  validate_object <- function(obj, allowed, path) {
    if (is.null(obj)) return(invisible(NULL))
    if (!intent_json_object(obj) && !(is.list(obj) && !length(obj))) {
      errors[[length(errors) + 1L]] <<- paste0(path, " 必须是 JSON 对象")
      return(invisible(NULL))
    }
    if ("password" %in% names(obj)) errors[[length(errors) + 1L]] <<-
      paste0(path, ".password 不接受：口令不进 intent")
    unknown <- setdiff(names(obj), c(allowed, "password"))
    if (length(unknown)) errors[[length(errors) + 1L]] <<-
      paste0(path, " 不接受字段：", paste(unknown, collapse = "、"))
    for (key in intersect(names(obj), allowed)) {
      v <- obj[[key]]
      field <- paste0(path, ".", key)
      if (identical(field, "server.runtime")) {
        validate_object(v, runtime_keys, field)
      } else if (field %in% c("samples.groups", "reference.modified_cells")) {
        vv <- if (is.list(v)) v else as.list(v)
        valid <- is.null(v) || ((is.character(v) || is.numeric(v) || is.list(v)) &&
          is.null(names(v)) && all(vapply(vv, function(x)
            !is.list(x) && length(x) == 1L && !is.na(x), logical(1))))
        if (!valid) errors[[length(errors) + 1L]] <<- paste0(field, " 必须是标量数组")
      } else if (!is.null(v)) {
        valid <- !is.list(v) && length(v) == 1L && !is.na(v)
        if (field %in% c("scope.annotation_gate", "annotation.merge_adjacent"))
          valid <- valid && is.logical(v)
        else valid <- valid && (is.character(v) || is.numeric(v))
        if (valid && field %in% c("server.port", "samples.count")) {
          n <- suppressWarnings(as.numeric(v))
          valid <- is.finite(n) && n == floor(n) && n >= 1 &&
            n <= if (field == "server.port") 65535 else .Machine$integer.max
        }
        if (valid && (identical(path, "paths") ||
            field %in% paste0("server.runtime.", c("rscript", "lib_path", "ld_library_path", "python", "qsub", "hdf5"))))
          valid <- !is.null(intent_path(v))
        else if (valid && is.character(v) && nzchar(trimws(v))) {
          limits <- c("server.label" = 100L, "server.host" = 200L,
            "server.user" = 100L, "server.exec_pc" = 200L, "server.samples" = 1000L,
            "samples.group_table" = 300L, "reference.notebook" = 300L,
            "scope.run_until" = 20L, "annotation.policy" = 200L,
            "source.topic_id" = 200L, "source.message_id" = 200L,
            "source.raw_text" = INTENT_MAX_TEXT)
          max_len <- if (field %in% names(limits)) limits[[field]] else INTENT_MAX_FIELD
          valid <- !is.null(intent_scalar(v, max_len))
        }
        if (!valid) errors[[length(errors) + 1L]] <<- paste0(field, " 的类型或值无效")
      }
    }
  }
  for (key in names(sections)) validate_object(raw[[key]], sections[[key]], key)
  if (length(errors)) return(list(ok = FALSE, intent = NULL, errors = errors))

  if (!is.null(raw$schema) && !identical(raw$schema, INTENT_SCHEMA))
    errors[[length(errors) + 1L]] <- "schema 必须为 pa.intent.v1"
  it <- list(schema = INTENT_SCHEMA)
  it$project_name <- intent_scalar(raw$project_name, 200L)
  if (is.null(it$project_name))
    errors[[length(errors) + 1L]] <- "project_name 必填（这是记录的名字，不能空）"

  mode <- tolower(intent_scalar(raw$mode, 20L) %||% "server")
  if (!mode %in% c("server", "local"))
    errors[[length(errors) + 1L]] <- "mode 只能是 server 或 local"
  it$mode <- mode

  # ---- server ----
  srv <- raw$server
  it$server <- list()
  if (is.list(srv)) {
    it$server$label <- intent_scalar(srv$label, 100L)
    it$server$host  <- intent_scalar(srv$host, 200L)
    it$server$user  <- intent_scalar(srv$user, 100L)
    it$server$exec_pc <- intent_scalar(srv$exec_pc, 200L)
    it$server$samples <- intent_scalar(srv$samples, 1000L)
    if (is.list(srv$runtime)) {
      it$server$runtime <- list()
      for (key in names(srv$runtime)) {
        clean <- if (key %in% c("rscript", "lib_path", "ld_library_path", "python", "qsub", "hdf5")) intent_path else intent_scalar
        it$server$runtime[[key]] <- clean(srv$runtime[[key]])
      }
    }
    port <- suppressWarnings(as.integer(srv$port %||% NA))
    it$server$port <- if (is.na(port) || port < 1L || port > 65535L) 22L else port
    # 口令绝不从这条路进来。intent 会被落盘、被复制、被贴进对话。
    if (!is.null(srv$password)) {
      errors[[length(errors) + 1L]] <-
        "server.password 不接受：口令从本机 config/server_profile.json 读，不进 intent"
    }
  }
  if (identical(mode, "server") && is.null(it$server$host))
    errors[[length(errors) + 1L]] <- "server 模式必须给 server.host"

  # ---- paths ----
  p <- raw$paths
  it$paths <- list()
  if (is.list(p)) {
    it$paths$data_root   <- intent_path(p$data_root)
    it$paths$work_root   <- intent_path(p$work_root)
    it$paths$rawdata_dir <- intent_path(p$rawdata_dir)
    it$paths$script_root <- intent_path(p$script_root)
    it$paths$liny_data <- intent_path(p$liny_data)
  }
  if (is.null(it$paths$work_root))
    errors[[length(errors) + 1L]] <- "paths.work_root 必填（不知道往哪跑就不能建记录）"

  # ---- samples ----
  sm <- raw$samples
  it$samples <- list()
  if (is.list(sm)) {
    n <- suppressWarnings(as.integer(sm$count %||% NA))
    it$samples$count <- if (is.na(n) || n < 1L) NULL else n
    it$samples$group_table <- intent_scalar(sm$group_table, 300L)
    grp <- sm$groups
    if (is.list(grp) || is.character(grp)) {
      g <- vapply(as.list(grp), function(x) intent_scalar(x, 100L) %||% "", "")
      g <- g[nzchar(g)]
      it$samples$groups <- if (length(g)) as.list(g) else NULL
    }
  }

  # ---- reference ----
  rf <- raw$reference
  it$reference <- list()
  if (is.list(rf)) {
    it$reference$notebook <- intent_scalar(rf$notebook, 300L)
    cells <- rf$modified_cells
    if (is.list(cells) || is.character(cells) || is.numeric(cells)) {
      cc <- suppressWarnings(as.integer(unlist(cells, use.names = FALSE)))
      cc <- cc[!is.na(cc) & cc > 0L]
      it$reference$modified_cells <- if (length(cc)) as.list(cc) else NULL
    }
  }

  # ---- scope ----
  sc <- raw$scope
  it$scope <- list()
  if (is.list(sc)) {
    it$scope$run_until <- intent_scalar(sc$run_until, 20L)
    if (!is.null(sc$annotation_gate)) it$scope$annotation_gate <- isTRUE(sc$annotation_gate)
  }

  # ---- annotation ----
  an <- raw$annotation
  it$annotation <- list()
  if (is.list(an)) {
    it$annotation$policy <- intent_scalar(an$policy, 200L)
    if (!is.null(an$merge_adjacent)) it$annotation$merge_adjacent <- isTRUE(an$merge_adjacent)
  }

  # ---- 物种与组织：只接受人给的，不从路径猜 ----
  #   知识库里记过一次：把目录名里的 spleen 当成组织证据，判成了小鼠脾脏，
  #   实际是小鼠肾脏。所以这两个字段没有就留空，进 missing，由助手去问。
  it$species <- intent_scalar(raw$species, 100L)
  it$tissue  <- intent_scalar(raw$tissue, 100L)

  # ---- source：幂等键 ----
  src <- raw$source
  it$source <- list()
  if (is.list(src)) {
    it$source$topic_id <- intent_scalar(src$topic_id, 200L)
    it$source$message_id <- intent_scalar(src$message_id, 200L)
    it$source$raw_text <- intent_scalar(src$raw_text, INTENT_MAX_TEXT)
  }
  if (is.null(it$source$message_id)) {
    errors[[length(errors) + 1L]] <-
      "source.message_id 必填，不能仅用会话号区分不同消息"
  }

  # ---- missing：不由调用方填，这里算 ----
  #   为什么算而不是收：如果允许助手自己声明"不缺"，它就永远不会缺，
  #   追问这一步就没了。缺不缺由 schema 决定。
  need <- character(0)
  if (is.null(it$species)) need <- c(need, "species")
  if (is.null(it$tissue))  need <- c(need, "tissue")
  if (is.null(it$samples$group_table) && is.null(it$samples$groups))
    need <- c(need, "samples.groups")
  if (identical(mode, "server") && is.null(it$paths$data_root))
    need <- c(need, "paths.data_root")
  it$missing <- if (length(need)) as.list(need) else list()

  list(ok = !length(errors), intent = it, errors = errors)
}

# ---- 幂等键 ---------------------------------------------------------

# 同一句话可能被提交多次：助手重试、网络抖动、用户重复发。
#   判据取 topic_id + message_id。两个都没有时上层已经拒绝。
intent_key <- function(intent) {
  src <- intent$source %||% list()
  paste0(as.character(src$topic_id %||% ""), "|",
         as.character(src$message_id %||% ""))
}

# 已落地的 intent 存在哪：
#   state/intents.json  {items:{"<key>": {session_id, created_at, intent}}}
#   为什么单独一个文件而不是塞进 sessions.json：
#     sessions.json 是「有哪些记录」，这份是「哪句话生成了哪条记录」。
#     语义不同，混在一处会让「记录被删了但幂等键还在」变成无解状态。
intent_store_file <- function(app_dir) file.path(app_dir, "state", "intents.json")

intent_store_read <- function(app_dir) {
  f <- intent_store_file(app_dir)
  if (!file.exists(f)) return(list(items = list()))
  intent_json_read(f, list(items = list()), "items")
}

intent_store_write <- function(app_dir, obj) {
  intent_json_write(intent_store_file(app_dir), obj)
  invisible(TRUE)
}

intent_lookup <- function(app_dir, intent, owner_key) {
  key <- paste0(owner_key, "|", intent_key(intent))
  if (!nzchar(key) || identical(key, "|")) return(NULL)
  obj <- intent_store_read(app_dir)
  obj$items[[key]]
}

intent_remember <- function(app_dir, intent, session_id, owner_key) {
  key <- paste0(owner_key, "|", intent_key(intent))
  if (!nzchar(key) || identical(key, "|")) return(invisible(FALSE))
  obj <- intent_store_read(app_dir)
  obj$items[[key]] <- list(
    session_id = as.character(session_id),
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    project_name = as.character(intent$project_name %||% ""),
    missing = intent$missing %||% list())
  intent_store_write(app_dir, obj)
  invisible(TRUE)
}

# ---- 落地 -----------------------------------------------------------

# 把 intent 写成 place 块（保存到 config/server_profile.json 与记录里）。
#   注意：这里不写口令。口令仍由人从 config 里那份读。
intent_to_place <- function(intent) {
  srv <- intent$server %||% list()
  list(
    mode = as.character(intent$mode %||% "server"),
    samples = intent$samples %||% list(),
    local = list(label = "本机", path = as.character(intent$paths$work_root %||% "")),
    server = list(
      label = as.character(srv$label %||% "分析服务器"),
      host = as.character(srv$host %||% ""),
      port = as.integer(srv$port %||% 22L),
      user = as.character(srv$user %||% ""),
      exec_pc = as.character(srv$exec_pc %||% ""),
      samples = as.character(srv$samples %||% ""),
      runtime = srv$runtime %||% list(),
      rawdata_dir = as.character(intent$paths$rawdata_dir %||% ""),
      liny_data = as.character(intent$paths$liny_data %||% ""),
      data_root = as.character(intent$paths$data_root %||% ""),
      script_root = as.character(intent$paths$script_root %||% ""),
      work_root = as.character(intent$paths$work_root %||% ""),
      note = "由会话意图创建"
    )
  )
}

# 把 intent 里能直接用的部分写进记录的 profile，页面据此预填。
#   只写有值的键：写一堆空字符串会让页面显示成「填过但是空的」，
#   比留空更难判断。
intent_to_profile_patch <- function(intent) {
  out <- list()
  if (!is.null(intent$species)) out$species <- intent$species
  if (!is.null(intent$tissue)) out$tissue <- intent$tissue
  out$intent <- intent
  out
}

# Strict reads are local to imports: damaged state is never treated as empty.
intent_json_read <- function(path, absent = list(), member = NULL) {
  if (!file.exists(path)) return(absent)
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (!grepl("^\\s*\\{", text)) stop("导入状态不是 JSON 对象: ", path)
  obj <- jsonlite::fromJSON(text, simplifyVector = FALSE)
  if (!is.list(obj) || (!is.null(member) && !is.list(obj[[member]])))
    stop("导入状态结构损坏: ", path)
  obj
}

intent_atomic_bytes <- function(path, bytes) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(".intent-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  con <- file(tmp, "wb")
  tryCatch(writeBin(bytes, con), finally = close(con))
  if (!file.rename(tmp, path)) stop("导入文件原子替换失败: ", path)
  invisible(TRUE)
}

intent_json_write <- function(path, obj) {
  intent_atomic_bytes(path, charToRaw(enc2utf8(as.character(jsonlite::toJSON(
    obj, auto_unbox = TRUE, null = "null", pretty = TRUE, digits = NA)))))
}

# main() dispatches requests and the poller synchronously. No callbacks or yields
# occur here. Snapshots cover only the three files written by this import; no
# global config/selection mutation, recursive directory backup or broad cleanup.
# This compensates write/commit errors, not abrupt process termination.
intent_import <- function(app_dir, con, owner, it) {
  failure_code <- "intent_index_write_failed"
  sid <- NULL
  snapshots <- list()
  touched <- character()
  made_dirs <- character()
  capture <- function(path) {
    if (dir.exists(path)) stop("导入目标是目录: ", path)
    snapshots[[path]] <<- list(exists = file.exists(path), bytes = if (file.exists(path))
      readBin(path, "raw", n = file.info(path)$size) else raw())
  }
  write_json <- function(path, obj) {
    # Track before calling the writer: a failure can happen after rename.
    touched <<- unique(c(touched, path))
    d <- dirname(path)
    missing <- character()
    while (!dir.exists(d)) {
      missing <- c(missing, d)
      parent <- dirname(d)
      if (identical(parent, d)) stop("无可用的导入父目录")
      d <- parent
    }
    made_dirs <<- unique(c(made_dirs, missing))
    if (identical(intent_json_write(path, obj), FALSE)) stop("导入文件保存失败")
  }
  result <- tryCatch(db_tx(con, function() {
    index <- intent_store_read(app_dir)
    st <- intent_json_read(sess_file(app_dir), list(sessions = list()), "sessions")
    key <- paste0(owner$key, "|", intent_key(it))
    prev <- index$items[[key]]
    hit <- integer()
    if (!is.null(prev)) {
      sid0 <- as.character(prev$session_id %||% "")
      hit <- which(vapply(st$sessions, function(s) identical(s$id, sid0), logical(1)))
    }
    duplicate <- length(hit) > 0L
    if (duplicate) {
      if (length(hit) != 1L) stop("分析记录索引重复")
      s <- st$sessions[[hit]]
      sid <<- s$id
      # Do not collapse SQL read failures into an authorization denial.
      own <- DBI::dbGetQuery(con, "SELECT owner_key FROM sessions WHERE session_id=?", list(sid))
      if (nrow(own) != 1L || !identical(own$owner_key[[1]], owner$key))
        return(list(status = 403L, body = list(ok = FALSE, code = "intent_not_owner",
          error = "不能更新其他用户的分析记录")))
      s$note <- it$project_name
      s$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    } else {
      place <- intent_to_place(it)
      now <- Sys.time()
      # Reject collisions rather than overwriting an existing profile/directory.
      sid <<- sess_new_id(now)
      if (dir.exists(sess_dir(app_dir, sid)) || any(vapply(st$sessions,
          function(s) identical(s$id, sid), logical(1)))) stop("分析记录编号冲突，请重试")
      s <- list(id = sid, name = it$project_name,
        created_at = format(now, "%Y-%m-%dT%H:%M:%S"),
        updated_at = format(now, "%Y-%m-%dT%H:%M:%S"), note = it$project_name,
        mode = sess_chr(place$mode, "server"),
        run_dir = sess_chr(place$server$work_root %||% place$local$path %||% ""),
        place = place, identify = NULL, steps = list(), archives = list())
      st$sessions[[length(st$sessions) + 1L]] <- s
    }
    # Refresh record-local execution binding for creates and idempotent updates.
    # No global config or current-selection writes belong to this path.
    place <- intent_to_place(it)
    s$name <- it$project_name
    s$place <- place
    s$mode <- place$mode
    s$run_dir <- if (identical(place$mode, "local")) place$local$path else place$server$work_root
    profile_path <- sess_profile_f(app_dir, sid)
    failure_code <<- "intent_profile_write_failed"
    prof <- intent_json_read(profile_path)
    patch <- intent_to_profile_patch(it)
    prof[names(patch)] <- patch
    s$species <- prof$species
    s$tissue <- prof$tissue
    if (duplicate) st$sessions[[hit]] <- s
    else st$sessions[[length(st$sessions)]] <- s
    if (!duplicate) {
      prof$created_from <- "agent_intent"
      prof$created_via <- as.character(owner$external_id %||% "")
    }
    for (path in c(sess_file(app_dir), profile_path, intent_store_file(app_dir))) capture(path)
    failure_code <<- "intent_owner_write_failed"
    if (duplicate) {
      n <- DBI::dbExecute(con,
        "UPDATE sessions SET name=?,mode=?,run_dir=?,updated_at=? WHERE session_id=? AND owner_key=?",
        list(s$name, s$mode, s$run_dir, s$updated_at, sid, owner$key))
    } else {
      n <- DBI::dbExecute(con,
        "INSERT INTO sessions(session_id,owner_key,name,mode,run_dir,created_at,updated_at) VALUES(?,?,?,?,?,?,?)",
        list(s$id, owner$key, s$name, s$mode, s$run_dir, s$created_at, s$updated_at))
    }
    if (n != 1L) stop("记录归属写入失败")
    write_json(sess_file(app_dir), st)
    failure_code <<- "intent_profile_write_failed"
    write_json(profile_path, prof)
    failure_code <<- "intent_index_write_failed"
    index$items[[key]] <- list(session_id = sid,
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      project_name = it$project_name, missing = it$missing %||% list())
    write_json(intent_store_file(app_dir), index)
    body <- list(ok = TRUE, duplicate = duplicate, session_id = sid,
                 intent = it, missing = it$missing)
    if (duplicate) body$message <- "同一句话已经建过记录，这次只更新了字段，没有重复创建"
    else body$place <- place
    list(status = 200L, body = body)
  }), error = function(e) {
    restore_errors <- character()
    for (path in rev(touched)) {
      tryCatch({
        snap <- snapshots[[path]]
        if (snap$exists) intent_atomic_bytes(path, snap$bytes)
        else if (file.exists(path) && unlink(path) != 0L) stop("删除失败")
      }, error = function(re) restore_errors <<- c(restore_errors, conditionMessage(re)))
    }
    for (d in made_dirs[order(nchar(made_dirs), decreasing = TRUE)]) {
      if (dir.exists(d) && !length(list.files(d, all.files = TRUE, no.. = TRUE))) unlink(d, recursive = TRUE)
    }
    body <- list(ok = FALSE, code = failure_code,
      error = if (length(restore_errors)) "导入失败且恢复未完成，需要检查存储后再重试"
              else "导入写入失败，本次改动已撤回；请重试")
    if (!is.null(sid)) body$session_id <- sid
    list(status = 503L, body = body)
  })
  result
}

# App execution IDs only: neither GPU notebook cells nor flow_spec stages map here.
INTENT_APP_CHAIN <- c('env', 'decide', 'precheck', 'pipeline1', 'pipeline2',
                      'plots', 'ppt', 'pack')

intent_json_object <- function(x) {
  is.list(x) && !is.null(names(x)) && !anyNA(names(x)) &&
    all(nzchar(names(x))) && !anyDuplicated(names(x))
}

intent_json_unique <- function(x) {
  if (!is.list(x)) return(TRUE)
  if (!is.null(names(x)) && !intent_json_object(x)) return(FALSE)
  all(vapply(x, intent_json_unique, logical(1)))
}

# A bounded single app step is not a bounded downstream all-run plan.
intent_execution_gate <- function(app_dir, sid, requested_step = NULL) {
  allow <- list(ok = TRUE, fields = list())
  deny <- function(code, fields = character(), message) list(ok = FALSE,
    code = code, fields = as.list(fields), error = message)
  scalar_text <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (!is.null(requested_step) && !scalar_text(requested_step))
    return(deny('intent_step_invalid', message = '执行阶段必须是明确的应用阶段 ID。'))
  path <- if (exists('sess_profile_f', mode = 'function')) sess_profile_f(app_dir, sid) else
    file.path(app_dir, 'state', 'sessions', sid, 'profile.json')
  if (!file.exists(path)) return(allow)
  profile <- tryCatch(intent_json_read(path), error = function(e) NULL)
  unreadable <- function() deny('intent_profile_unreadable',
    message = '分析记录或限制结构无法完整读取，未启动执行。')
  if (!intent_json_object(profile) || !intent_json_unique(profile)) return(unreadable())
  it <- profile[['intent']]
  if (is.null(it)) return(allow)
  # Legacy normalized empty sections serialize as []; only the empty form is safe.
  section <- function(x) intent_json_object(x) || (is.list(x) && length(x) == 0L)
  if (!section(it)) return(unreadable())
  for (key in c('scope', 'reference', 'annotation')) {
    if (!is.null(it[[key]]) && !section(it[[key]])) return(unreadable())
  }
  sc <- it[['scope']]; rf <- it[['reference']]; an <- it[['annotation']]
  bound <- sc[['run_until']]; review <- sc[['annotation_gate']]
  if (!is.null(bound) && !scalar_text(bound)) return(unreadable())
  if (!is.null(review) && !(is.logical(review) && length(review) == 1L && !is.na(review)))
    return(unreadable())
  fields <- character()
  if (!is.null(bound)) fields <- c(fields, 'scope.run_until')
  if (isTRUE(review)) fields <- c(fields, 'scope.annotation_gate')
  if (!is.null(rf[['modified_cells']])) fields <- c(fields, 'reference.modified_cells')
  if (!is.null(an[['merge_adjacent']])) fields <- c(fields, 'annotation.merge_adjacent')
  unimplemented <- function(fields) deny('intent_constraints_unimplemented', fields,
    '已保存的分析限制尚未转成可验证执行计划，未启动或委派任务。')
  # Compatibility callers without a step must retain the conservative contract.
  if (is.null(requested_step)) {
    if (length(fields)) return(unimplemented(fields))
    return(allow)
  }
  unsupported <- intersect(fields, c('reference.modified_cells', 'annotation.merge_adjacent'))
  if (length(unsupported)) return(unimplemented(unsupported))
  # Only named scope constraints require app-chain IDs; legacy flow IDs stay executor-owned.
  if ((!is.null(bound) || isTRUE(review)) &&
      !requested_step %in% c(INTENT_APP_CHAIN, 'config', 'all'))
    return(deny('intent_step_invalid', message = '执行阶段必须是明确的应用阶段 ID。'))
  if (!is.null(bound) && !bound %in% INTENT_APP_CHAIN)
    return(unimplemented('scope.run_until'))
  if (identical(requested_step, 'all') && length(fields))
    return(deny('intent_scope_requires_bounded_plan', fields,
      '带范围限制的全部执行需要显式有界计划，未启动或入队。'))
  # No approval endpoint exists here; force and saved approval-looking fields confer no authority.
  if (isTRUE(review) && !requested_step %in% INTENT_APP_CHAIN[1:4])
    return(deny('annotation_review_required', 'scope.annotation_gate',
      '此阶段必须先完成人工注释审核，未启动或入队。'))
  if (!is.null(bound) && (!requested_step %in% INTENT_APP_CHAIN ||
      match(requested_step, INTENT_APP_CHAIN) > match(bound, INTENT_APP_CHAIN)))
    return(deny('intent_scope_outside_plan', 'scope.run_until',
      '请求阶段超出已保存的执行范围，未启动或入队。'))
  allow
}
