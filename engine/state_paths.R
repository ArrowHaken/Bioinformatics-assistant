#!/usr/bin/env Rscript
# =====================================================================
#  state_paths.R -- 私有数据放在哪（B2 的单一开关）
#
#  要解决的问题
#    sessions / quality / decisions / mirror 四条路径各自写死在各自文件里，
#    都指向 state/ 根目录。于是「按使用者分区」这件事只能在四个地方各改一次，
#    改漏一处就是两套数据并存 —— 那比不分区更糟。
#    更糟的是读和写可能落在不同的根上：页面读到的是分区里的，
#    写进去的是根目录的，人会看到「刚做的操作没了」。
#
#  做法：一个开关，读写共用
#    state_private_root(app_dir) 是唯一出口。四个模块都问它，不再自己拼路径。
#
#  开关怎么判
#    分区里存在记录索引（users/<owner>/sessions.json）就用分区，
#    否则回退 state/ 根目录。
#    判据是「文件在不在」而不是配置项：配置文件忘了改会静默走错路径，
#    而文件在不在这件事没有第二种解释。
#
#  为什么保留回退
#    分区是一次迁移，不是重写。旧数据复制过去、原件没删，
#    所以回退永远可用 —— 开关坏了也能退回能跑的状态。
#    删旧数据这件事刻意没做成自动的：不可逆的动作要人来按。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# 当前使用者的分区目录名（与 db.R 的 user_dir 同一规则）
state_owner_slug <- function(owner_key) {
  gsub("[^A-Za-z0-9_.:@-]", "_", as.character(owner_key %||% "local:default"))
}

# 分区根：谁在用，就看谁
state_user_root <- function(app_dir, owner_key = NULL) {
  file.path(app_dir, "state", "users", state_owner_slug(owner_key %||% "local:default"))
}

# 有没有分区真的立起来了（判据：分区里有 sessions.json）
state_partition_active <- function(app_dir, owner_key = NULL) {
  f <- file.path(state_user_root(app_dir, owner_key), "sessions.json")
  file.exists(f)
}

# 唯一出口：四个模块都用它
#   返回的根目录下面直接就是 sessions.json / sessions/ / quality/ / decisions/ / mirror/
state_private_root <- function(app_dir, owner_key = NULL) {
  # 1) 该使用者自己的分区
  if (state_partition_active(app_dir, owner_key)) return(state_user_root(app_dir, owner_key))
  # 2) 历史（未归属）分区：对本机单用户来说，这就是他自己的数据
  if (state_partition_active(app_dir, "legacy")) return(state_user_root(app_dir, "legacy"))
  # 3) 回退：老的 state/ 根目录
  file.path(app_dir, "state")
}

# 页面/诊断用：现在到底在用哪个根，以及为什么
state_paths_status <- function(app_dir, owner_key = NULL) {
  own <- state_user_root(app_dir, owner_key)
  leg <- state_user_root(app_dir, "legacy")
  root <- state_private_root(app_dir, owner_key)
  list(
    root = root,
    own_partition = list(path = own, active = state_partition_active(app_dir, owner_key)),
    legacy_partition = list(path = leg, active = state_partition_active(app_dir, "legacy")),
    why = if (identical(root, own)) "使用当前使用者的分区"
          else if (identical(root, leg)) "使用历史（未归属）分区"
          else "回退到 state/ 根目录（分区里没有记录索引）",
    rule = "有 users/<owner>/sessions.json 就用分区，否则回退 state/")
}
