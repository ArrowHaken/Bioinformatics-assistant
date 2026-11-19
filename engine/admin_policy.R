#!/usr/bin/env Rscript
# =====================================================================
#  admin_policy.R -- 管理员与两个隔离开关
#
#  为什么需要这个文件
#    这个应用按 owner_key 做了用户隔离：谁的记录谁看得见。
#    但「谁是这个应用的主人」从来没有被表达过 —— 于是没有一处能回答
#    「要不要把隔离打开」。所有者本人（机器人主人）也没有一个开关可用，
#    想让别人协作看记录，只能改数据库。
#
#  三件事，只做这三件
#    1. 认出管理员：机器人主人的平台身份（uid / 用户名 / 已记录 owner_key）。
#    2. 存两个开关：record_isolation（记录可见）、write_isolation（修改权限）。
#    3. 给出判定结果：某个人能不能读某条记录、能不能改、能不能改应用代码。
#    判定本身仍在 record_access.R；这里只提供事实，避免两处各写一套规则。
#
#  为什么默认两个开关都打开
#    关闭隔离意味着所有登录用户能看到彼此的记录、能改彼此的记录。
#    默认关闭会把现有数据一次性变成公开的，而且没有任何提示。
#    所以默认从严；打开只有管理员一次显式动作能做。
#
#  为什么管理员不是「第一个访问的人」
#    那样谁先打开网址谁就是管理员。这里只认部署配置里写明的平台身份
#    （config/admin_policy.json 的 admin_uids / admin_usernames），
#    并在第一次成功操作后把该 owner_key 记下来，
#    免得平台后改用户名就把主人自己锁在门外。
# =====================================================================

# serve.R 里已经有 %||%。这个文件也会被契约测试单独 source，
#   那种场合没有它，缺一个就会因为一次空值判断把整个模块打挂。
if (!exists("%||%", mode = "function"))
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

admin_policy_default <- function() {
  list(record_isolation = TRUE, write_isolation = TRUE,
       admin_uids = character(0), admin_usernames = character(0),
       admin_owner_keys = character(0), updated_at = NULL, updated_by = NULL)
}

admin_policy_file <- function(app_dir) file.path(app_dir, "config", "admin_policy.json")
admin_audit_file <- function(app_dir) file.path(app_dir, "state", "admin_audit.jsonl")

# 读文本：不依赖 serve.R 的 read_text_any，单独加载本文件时也能跑。
admin_read_text <- function(path) {
  if (!file.exists(path)) return("")
  tryCatch(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
           error = function(e) "")
}

admin_chr_vec <- function(x) {
  if (is.null(x) || !length(x)) return(character(0))
  v <- trimws(as.character(unlist(x, use.names = FALSE)))
  v[!is.na(v) & nzchar(v)]
}

admin_env_chr_vec <- function(name) {
  v <- Sys.getenv(name, unset = "")
  if (!nzchar(trimws(v))) return(character(0))
  parts <- unlist(strsplit(v, "[,;[:space:]]+"))
  parts <- trimws(parts)
  parts[nzchar(parts)]
}

admin_policy_read <- function(app_dir) {
  pol <- admin_policy_default()
  f <- admin_policy_file(app_dir)
  if (file.exists(f)) {
    d <- tryCatch(jsonlite::fromJSON(admin_read_text(f), simplifyVector = FALSE),
                  error = function(e) NULL)
    if (is.list(d)) {
      # 只接受 TRUE / FALSE；写坏了就按默认（从严）走，不按字符串猜。
      pick <- function(k, dflt) if (is.logical(d[[k]]) && length(d[[k]]) == 1L &&
                                    !is.na(d[[k]])) isTRUE(d[[k]]) else dflt
      pol$record_isolation <- pick("record_isolation", pol$record_isolation)
      pol$write_isolation <- pick("write_isolation", pol$write_isolation)
      pol$admin_uids <- admin_chr_vec(d$admin_uids)
      pol$admin_usernames <- admin_chr_vec(d$admin_usernames)
      pol$admin_owner_keys <- admin_chr_vec(d$admin_owner_keys)
      pol$updated_at <- if (is.character(d$updated_at) && length(d$updated_at) == 1L)
        d$updated_at else NULL
      pol$updated_by <- if (is.character(d$updated_by) && length(d$updated_by) == 1L)
        d$updated_by else NULL
    }
  }
  # 环境变量只做追加，不覆盖配置：临时加一个 uid 排查，不该改掉部署事实。
  pol$admin_uids <- unique(c(pol$admin_uids, admin_env_chr_vec("PA_ADMIN_VIEWER_UIDS")))
  pol$admin_usernames <- unique(c(pol$admin_usernames,
                                  admin_env_chr_vec("PA_ADMIN_USERNAMES")))
  pol
}

# 两个开关的读入口。叫这个短名字是因为 record_access.R 只关心开关本身，
#   不关心管理员是谁；分开命名免得每处都写一遍 admin_policy_read。
record_policy <- function(app_dir) admin_policy_read(app_dir)

# 回给页面的形状：只有开关和更新时间，不含管理员名单。
#   名单是部署事实，不该随每个页面响应分发出去。
#   加上「上次是谁改的」：gina 2026-11-19 要求任何修改都要登记好是谁做的，
#   只写进 jsonl 不够 —— 人打开这个面板时就该看到上一手是谁。
record_policy_public <- function(pol = NULL) {
  p <- if (is.null(pol)) admin_policy_default() else pol
  list(record_isolation = isTRUE(p$record_isolation),
       write_isolation = isTRUE(p$write_isolation),
       updated_at = p$updated_at %||% NULL,
       updated_by = p$updated_by %||% NULL,
       updated_by_username = p$updated_by_username %||% NULL)
}

admin_policy_write <- function(app_dir, record_isolation, write_isolation, actor = NULL,
                               uid = NULL, username = NULL) {
  pol <- admin_policy_read(app_dir)
  pol$record_isolation <- isTRUE(record_isolation)
  pol$write_isolation <- isTRUE(write_isolation)
  pol$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  pol$updated_by <- if (is.null(actor) || is.na(actor)) "" else as.character(actor)
  # 谁改的，除了 owner_key 还要存平台账号。
  #   owner_key 是应用内伪名，人看不出这是谁；用户名和 uid 才是人能对上号的。
  #   gina 2026-11-19：任何修改都要登记好是谁做的。
  pol$updated_by_username <- if (is.null(username) || is.na(username)) "" else as.character(username)
  pol$updated_by_uid <- if (is.null(uid) || is.na(uid)) "" else as.character(uid)
  # 管理员第一次成功操作后记住自己的 owner_key。
  if (!is.null(actor) && !is.na(actor) && nzchar(as.character(actor)))
    pol$admin_owner_keys <- unique(c(pol$admin_owner_keys, as.character(actor)))

  f <- admin_policy_file(app_dir)
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(f, ".tmp-", Sys.getpid())
  ok <- tryCatch({
    jsonlite::write_json(pol, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null")
    if (file.exists(f)) file.remove(f)
    file.rename(tmp, f)
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) return(list(ok = FALSE, error = "策略文件写入失败"))
  admin_audit(app_dir, list(action = "policy_update",
                            record_isolation = pol$record_isolation,
                            write_isolation = pol$write_isolation,
                            actor = pol$updated_by, uid = uid, username = username,
                            at = pol$updated_at))
  list(ok = TRUE, policy = pol)
}

admin_audit <- function(app_dir, rec) {
  f <- admin_audit_file(app_dir)
  tryCatch({
    dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
    line <- jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null")
    cat(paste0(line, "\n"), file = f, append = TRUE)
    TRUE
  }, error = function(e) FALSE)
}

# 身份字段：owner_from_identity 会把平台 uid / username 一并带进来。
admin_identity <- function(app_dir, owner_ctx) {
  pol <- admin_policy_read(app_dir)
  key <- if (is.null(owner_ctx)) "" else trimws(as.character(owner_ctx$key %||% ""))
  if (length(key) != 1L || is.na(key)) key <- ""
  uid <- trimws(as.character(owner_ctx$viewer_uid %||% ""))
  if (length(uid) != 1L || is.na(uid)) uid <- ""
  uname <- trimws(as.character(owner_ctx$viewer_username %||% ""))
  if (length(uname) != 1L || is.na(uname)) uname <- ""

  is_admin <- FALSE
  reason <- ""
  if (nzchar(key) && key %in% pol$admin_owner_keys) {
    is_admin <- TRUE; reason <- "owner_key"
  } else if (nzchar(uid) && uid %in% pol$admin_uids) {
    is_admin <- TRUE; reason <- "uid"
  } else if (nzchar(uname) &&
             tolower(uname) %in% tolower(pol$admin_usernames)) {
    is_admin <- TRUE; reason <- "username"
  }
  list(is_admin = is_admin, reason = reason, owner_key = key, uid = uid,
       username = uname, policy = pol)
}

# 记录可见性：只给一条 SQL 的口径，调用方各自拼字段。
#   隔离打开时仍是「自己的 + 别人显式共享的」；关闭时所有记录都可见。
record_visibility_query <- function(app_dir, owner_ctx) {
  pol <- record_policy(app_dir)
  # 任一隔离被关掉都要给出全部记录：
  #   记录隔离关 = 能读别人的；修改隔离关 = 能改别人的，而能改就必须能读。
  if (!isTRUE(pol$record_isolation) || !isTRUE(pol$write_isolation))
    return(list(sql = "SELECT session_id, owner_key, visibility, name, updated_at FROM sessions",
                params = NULL))
  # 管理员要看得到迁移来的记录。
  #   迁移记录的 owner_key 里是旧应用发的伪名，伪名按应用发放、跨应用对不上，
  #   本人换了应用之后靠 owner_key 永远匹配不到自己的历史记录 ——
  #   列表里看不到，自然也切不进去。
  #   这里按配置里点名的平台账号认回（uid / username），不是「登录了就全给看」。
  a <- tryCatch(admin_identity(app_dir, owner_ctx), error = function(e) NULL)
  if (isTRUE(a$is_admin))
    return(list(sql = "SELECT session_id, owner_key, visibility, name, updated_at FROM sessions",
                params = NULL))
  key <- if (is.null(owner_ctx)) "" else trimws(as.character(owner_ctx$key %||% ""))
  list(sql = paste0("SELECT session_id, owner_key, visibility, name, updated_at FROM sessions ",
                    "WHERE owner_key = ? OR visibility = 'shared'"),
       params = list(key))
}

# 改应用代码（脚本库里的脚本）算不算改应用本身：
#   隔离打开时只有管理员能改；关闭时任何已识别用户都能改。
#   为什么不并进 write_isolation 的记录判定：改脚本影响的是所有人下一次运行，
#   和「能不能改某一条记录」不是同一种风险，所以走同一条开关但单独一处调用。
admin_code_write_gate <- function(app_dir, owner_ctx) {
  a <- admin_identity(app_dir, owner_ctx)
  pol <- a$policy
  key <- a$owner_key
  if (!nzchar(key))
    return(list(ok = FALSE, code = "identity_unavailable", admin = FALSE, policy = pol,
                error = "身份未识别，不能修改应用代码。"))
  if (isTRUE(a$is_admin))
    return(list(ok = TRUE, code = "ok", admin = TRUE, policy = pol))
  if (!isTRUE(pol$write_isolation))
    return(list(ok = TRUE, code = "open_by_admin", admin = FALSE, policy = pol))
  list(ok = FALSE, code = "admin_required", admin = FALSE, policy = pol,
       error = "修改权限隔离开着：只有管理员可以修改应用代码。")
}
