# Run from app/: Rscript tests/admin_policy_contract.R
# Temporary app tree + SQLite only; no production state, queue, mirror, or process.
run_contract <- function() {
  `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
  source('engine/db.R', local = TRUE)
  source('engine/sessions.R', local = TRUE)
  source('engine/admin_policy.R', local = TRUE)
  source('engine/record_access.R', local = TRUE)

  root <- tempfile('admin-policy-')
  dir.create(file.path(root, 'state'), recursive = TRUE)
  dir.create(file.path(root, 'config'), recursive = TRUE)
  con <- db_connect(root)
  on.exit({
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    unlink(root, recursive = TRUE)
  }, add = TRUE)
  db_init(con)

  fail <- character()
  check <- function(label, value) {
    cat(if (isTRUE(value)) 'PASS:' else 'FAIL:', label, '\n')
    if (!isTRUE(value)) fail <<- c(fail, label)
  }

  owner <- 'viewer:policy-owner'
  other <- 'viewer:policy-other'
  run_a <- file.path(root, 'run-a'); dir.create(run_a)
  place <- list(mode = 'local', local = list(path = run_a), server = list())
  a <- sess_create(root, 'A', place, activate = TRUE)
  DBI::dbExecute(con,
    'INSERT INTO sessions(session_id,owner_key,name,mode,run_dir,visibility,created_at,updated_at) VALUES(?,?,?,?,?,?,?,?)',
    list(a$id, owner, a$name, a$mode, a$run_dir, 'private', a$created_at, a$updated_at))
  ctx_owner <- list(key = owner, kind = 'viewer')
  ctx_other <- list(key = other, kind = 'viewer')
  ctx_none <- list(key = NA_character_, kind = 'unidentified')

  # ---- 默认：两个开关都开，且配置缺失时也不会放开 ---------------------
  pol0 <- record_policy(root)
  check('default policy keeps both isolations on',
        isTRUE(pol0$record_isolation) && isTRUE(pol0$write_isolation))
  cap <- record_capability(con, root, a$id, ctx_other)
  check('isolation on: foreign private record is unreadable',
        isTRUE(cap$ok) && !isTRUE(cap$can_read) && !isTRUE(cap$can_write))
  r <- record_access(con, root, a$id, ctx_other, need = 'read')
  check('isolation on: foreign read is 403 record_forbidden',
        identical(r$status, 403L) && identical(r$body$code, 'record_forbidden'))

  # ---- 写坏的配置不能当成关闭 ---------------------------------------
  writeLines(jsonlite::toJSON(list(record_isolation = 'false',
                                   write_isolation = 'no',
                                   admin_owner_keys = list(owner)),
                              auto_unbox = TRUE),
             file.path(root, 'config', 'admin_policy.json'))
  pol_bad <- record_policy(root)
  check('string "false" does not switch isolation off',
        isTRUE(pol_bad$record_isolation) && isTRUE(pol_bad$write_isolation))

  # ---- 关掉记录隔离：能读别人的，但不能改 -----------------------------
  save <- admin_policy_write(root, record_isolation = FALSE, write_isolation = TRUE,
                             actor = owner)
  check('policy write succeeds', isTRUE(save$ok))
  cap <- record_capability(con, root, a$id, ctx_other)
  check('record isolation off: foreign record readable',
        isTRUE(cap$can_read) && !isTRUE(cap$can_write) && isTRUE(cap$readonly))
  r <- record_access(con, root, a$id, ctx_other, need = 'write')
  check('record isolation off: foreign write still 403 not_owner',
        identical(r$status, 403L) && identical(r$body$code, 'not_owner'))

  # ---- 关掉修改隔离：能改别人的 -------------------------------------
  admin_policy_write(root, record_isolation = FALSE, write_isolation = FALSE, actor = owner)
  cap <- record_capability(con, root, a$id, ctx_other)
  check('write isolation off: foreign record writable',
        isTRUE(cap$can_read) && isTRUE(cap$can_write) && !isTRUE(cap$readonly))

  # ---- 未识别身份不受开关影响（fail-closed）-------------------------
  cap <- record_capability(con, root, a$id, ctx_none)
  check('unidentified stays fail-closed even with both isolations off',
        !isTRUE(cap$can_read) && !isTRUE(cap$can_write))
  g <- admin_code_write_gate(root, ctx_none)
  check('unidentified cannot modify app code even with write isolation off',
        !isTRUE(g$ok) && identical(g$code, 'identity_unavailable'))

  # ---- 改应用代码那道门 ---------------------------------------------
  admin_policy_write(root, record_isolation = TRUE, write_isolation = TRUE, actor = owner)
  g <- admin_code_write_gate(root, ctx_other)
  check('isolation on: non-admin cannot modify app code',
        !isTRUE(g$ok) && identical(g$code, 'admin_required'))
  g <- admin_code_write_gate(root, ctx_owner)
  check('isolation on: admin can modify app code',
        isTRUE(g$ok) && isTRUE(g$admin))
  admin_policy_write(root, record_isolation = TRUE, write_isolation = FALSE, actor = owner)
  g <- admin_code_write_gate(root, ctx_other)
  check('write isolation off: non-admin can modify app code',
        isTRUE(g$ok) && !isTRUE(g$admin))

  # ---- 管理员判定：三种依据各认一次 ---------------------------------
  jsonlite::write_json(list(record_isolation = TRUE, write_isolation = TRUE,
                            admin_uids = list('784'),
                            admin_usernames = list('gina'),
                            admin_owner_keys = list(owner)),
                       file.path(root, 'config', 'admin_policy.json'),
                       auto_unbox = TRUE, pretty = TRUE)
  a1 <- admin_identity(root, list(key = owner))
  check('admin recognised by recorded owner_key', isTRUE(a1$is_admin) &&
          identical(a1$reason, 'owner_key'))
  a2 <- admin_identity(root, list(key = 'viewer:someone-else', viewer_uid = '784'))
  check('admin recognised by platform uid', isTRUE(a2$is_admin) &&
          identical(a2$reason, 'uid'))
  a3 <- admin_identity(root, list(key = 'viewer:third', viewer_username = 'Gina'))
  check('admin recognised by username, case-insensitive', isTRUE(a3$is_admin) &&
          identical(a3$reason, 'username'))
  a4 <- admin_identity(root, list(key = 'viewer:random-user'))
  check('a stranger is not admin', !isTRUE(a4$is_admin))

  # 第一次成功操作后把管理员自己的 owner_key 记下来，免得平台改用户名把人锁在门外
  admin_policy_write(root, record_isolation = TRUE, write_isolation = TRUE,
                     actor = 'viewer:new-owner')
  pol_after <- record_policy(root)
  check('admin owner_key is remembered after a successful change',
        'viewer:new-owner' %in% pol_after$admin_owner_keys)

  # ---- 审计留痕 ------------------------------------------------------
  audit <- readLines(file.path(root, 'state', 'admin_audit.jsonl'), warn = FALSE)
  check('policy changes are audited', length(audit) >= 4L &&
          any(grepl('policy_update', audit, fixed = TRUE)))

  # ---- 可见性 SQL 与能力判定同口径 ----------------------------------
  admin_policy_write(root, record_isolation = TRUE, write_isolation = TRUE, actor = owner)
  vq <- record_visibility_query(root, ctx_other)
  rows <- db_rows(con, sub('SELECT session_id, owner_key, visibility, name, updated_at',
                           'SELECT session_id, owner_key, visibility', vq$sql, fixed = TRUE),
                  vq$params)
  check('isolation on: visibility query hides the foreign record', nrow(rows) == 0L)
  admin_policy_write(root, record_isolation = FALSE, write_isolation = TRUE, actor = owner)
  vq <- record_visibility_query(root, ctx_other)
  rows <- db_rows(con, vq$sql, vq$params)
  check('record isolation off: visibility query returns the foreign record', nrow(rows) == 1L)

  # ---- 回给页面时不带管理员名单 -------------------------------------
  pub <- record_policy_public(record_policy(root))
  check('public policy shape carries no admin list',
        setequal(names(pub), c('record_isolation', 'write_isolation', 'updated_at')))

  if (length(fail)) stop(paste(length(fail), 'admin policy failures'), call. = FALSE)
  cat('PASS: admin policy contract\n')
}
run_contract()
