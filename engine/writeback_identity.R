# Stable write-back identity.
#
# A dispatch owns one attempt_id. Both submission modes (qsub/batch and
# login/interactive observe) must carry that same id back to /api/step/import.
# The resolver below never guesses when an explicit id is present: mismatches
# become named errors instead of silently reopening or overwriting an attempt.

wbi_chr <- function(x, default = "") {
  if (is.null(x) || !length(x)) return(default)
  v <- as.character(x[[1]])
  if (!length(v) || is.na(v)) default else trimws(v)
}

wbi_json <- function(x) {
  tryCatch(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA),
           error = function(e) "")
}

wbi_digest <- function(x) {
  if (!requireNamespace("digest", quietly = TRUE)) return("")
  as.character(digest::digest(wbi_json(x), algo = "md5"))
}

wbi_script_digest <- function(script_path = "", supplied = "") {
  supplied <- wbi_chr(supplied)
  if (nzchar(supplied)) return(supplied)
  p <- wbi_chr(script_path)
  if (!nzchar(p) || !file.exists(p) || dir.exists(p)) return("")
  v <- tryCatch(unname(tools::md5sum(p)), error = function(e) "")
  wbi_chr(v)
}

wbi_payload <- function(att) {
  raw <- wbi_chr((att %||% list())$payload)
  if (!nzchar(raw)) return(list())
  x <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE),
                error = function(e) list())
  if (is.list(x)) x else list()
}

wbi_payload_digest <- function(att) {
  p <- wbi_payload(att)
  wbi_chr(p$wbi_payload_digest)
}

wbi_merge_payload <- function(att, body, extra = list()) {
  p <- wbi_payload(att)
  p$report <- body
  p$wbi_payload_digest <- wbi_digest(body)
  if (length(extra)) for (nm in names(extra)) p[[nm]] <- extra[[nm]]
  db_json(p)
}

wbi_attempt_by_id <- function(con, attempt_id) {
  aid <- wbi_chr(attempt_id)
  if (!nzchar(aid) || is.null(con)) return(NULL)
  r <- db_rows(con, "SELECT * FROM step_attempts WHERE attempt_id = ?", list(aid))
  if (!nrow(r)) NULL else as.list(r[1, ])
}

wbi_bind_attempt <- function(con, body, step_id, owner_key,
                             version_id = "", run_id = "") {
  step_id <- wbi_chr(step_id)
  owner_key <- wbi_chr(owner_key)
  version_id <- wbi_chr(version_id)
  run_id <- wbi_chr(run_id)
  aid <- wbi_chr(body$attempt_id)
  if (nzchar(aid)) {
    att <- wbi_attempt_by_id(con, aid)
    if (is.null(att))
      return(list(ok = FALSE, code = "attempt_id_unknown", explicit = TRUE,
                  error = paste0("attempt_id 不存在：", aid, "；不要改用 req_id 猜写。")))
    att_owner <- wbi_chr(att$owner_key)
    if (nzchar(owner_key) && nzchar(att_owner) &&
        !identical(att_owner, owner_key) && !identical(att_owner, "unattributed"))
      return(list(ok = FALSE, code = "attempt_owner_mismatch", explicit = TRUE,
                  attempt = att, error = "attempt_id 属于另一份分析记录。"))
    if (nzchar(step_id) && !identical(wbi_chr(att$step_id), step_id))
      return(list(ok = FALSE, code = "attempt_step_mismatch", explicit = TRUE,
                  attempt = att,
                  error = paste0("attempt_id 属于步骤 ", wbi_chr(att$step_id),
                                 "，本次回写是 ", step_id, "。")))
    # attempt_id is the strongest identity. A stale version/run echoed by an
    # observe/login script must not reject an otherwise correct write-back;
    # retain the attempt's own version/run and record the override.
    overrides <- list()
    if (nzchar(version_id) && !identical(wbi_chr(att$version_id), version_id))
      overrides$version_id <- list(kept = wbi_chr(att$version_id), sent = version_id)
    if (nzchar(run_id) && !identical(wbi_chr(att$run_id), run_id))
      overrides$run_id <- list(kept = wbi_chr(att$run_id), sent = run_id)
    return(list(ok = TRUE, attempt = att, binding = "attempt_id",
                explicit = TRUE, overrides = overrides))
  }

  req_id <- wbi_chr(body$req_id)
  if (!nzchar(req_id)) req_id <- wbi_chr(body$req_id_carried)
  if (nzchar(req_id) && nzchar(step_id)) {
    # Legacy callers do not own a dispatcher-issued id. Bind only inside the
    # record's current version; otherwise an old attempt from V1 could capture
    # a write-back for V2 and keep scattering one record across versions.
    q <- paste0("SELECT * FROM step_attempts WHERE req_id = ? AND step_id = ? ")
    args <- list(req_id, step_id)
    if (nzchar(version_id)) {
      q <- paste0(q, "AND version_id = ? ")
      args <- c(args, list(version_id))
    }
    q <- paste0(q, "ORDER BY attempt_no DESC LIMIT 1")
    r <- db_rows(con, q, args)
    if (nrow(r)) {
      att <- as.list(r[1, ])
      att_owner <- wbi_chr(att$owner_key)
      if (nzchar(owner_key) && nzchar(att_owner) &&
          !identical(att_owner, owner_key) && !identical(att_owner, "unattributed"))
        return(list(ok = FALSE, code = "attempt_owner_mismatch", explicit = FALSE,
                    attempt = att, error = "req_id 命中的 attempt 属于另一份分析记录。"))
      return(list(ok = TRUE, attempt = att, binding = "req_id",
                  explicit = FALSE, legacy = TRUE))
    }
  }

  ticket_id <- wbi_chr(body$ticket_id)
  if (nzchar(ticket_id)) {
    r <- db_rows(con,
      "SELECT * FROM step_attempts WHERE ticket_id = ? ORDER BY attempt_no DESC LIMIT 1",
      list(ticket_id))
    if (nrow(r)) {
      att <- as.list(r[1, ])
      return(list(ok = TRUE, attempt = att, binding = "ticket_id",
                  explicit = FALSE, legacy = TRUE))
    }
  }

  list(ok = TRUE, attempt = NULL, binding = "none", explicit = FALSE)
}

# Create (or reuse) the one attempt that owns this dispatch before any executor
# receives the script. Reusing the same req_id returns the same immutable id.
db_register_dispatch <- function(con, owner_key, session_id, version_id,
                                 step_id, req_id, exec_id = "",
                                 script_path = "", script_digest = "",
                                 run_dir = "", executor_mode = "") {
  owner_key <- wbi_chr(owner_key)
  version_id <- wbi_chr(version_id)
  step_id <- wbi_chr(step_id)
  req_id <- wbi_chr(req_id)
  script_path <- wbi_chr(script_path)
  script_digest <- wbi_script_digest(script_path, script_digest)
  if (is.null(con) || !nzchar(version_id) || !nzchar(step_id))
    return(list(ok = FALSE, code = "dispatch_binding_unavailable",
                error = "缺少数据库、版本号或步骤，未建立稳定 attempt。"))

  run_id <- db_ensure_run(con, version_id, owner_key,
                          work_root = if (nzchar(run_dir)) run_dir else NULL)
  if (nzchar(req_id)) {
    r <- db_rows(con,
      paste0("SELECT * FROM step_attempts WHERE req_id = ? AND step_id = ? ",
             "AND version_id = ? ORDER BY attempt_no DESC LIMIT 1"),
      list(req_id, step_id, version_id))
    if (nrow(r)) {
      att <- as.list(r[1, ])
      closed_status <- c("verified", "rejected", "released", "escalated")
      if (!(wbi_chr(att$status) %in% closed_status))
        return(list(ok = TRUE, attempt_id = wbi_chr(att$attempt_id),
                    run_id = wbi_chr(att$run_id), version_id = wbi_chr(att$version_id),
                    existing = TRUE))
    }
  }

  aid <- db_new_id("att")
  payload <- db_json(list(dispatch = list(
    exec_id = wbi_chr(exec_id), req_id = req_id, session_id = wbi_chr(session_id),
    script_path = script_path, script_digest = script_digest,
    executor_mode = wbi_chr(executor_mode), run_dir = wbi_chr(run_dir),
    at = db_now())))
  inserted <- tryCatch({
    db_tx(con, function() {
      no <- db_next_attempt_no(con, run_id, step_id)
      DBI::dbExecute(con,
        paste0("INSERT INTO step_attempts(attempt_id,run_id,version_id,owner_key,",
               "step_id,attempt_no,req_id,script_path,script_digest,status,",
               "started_at,payload) VALUES(?,?,?,?,?,?,?,?,?,?,?,?)"),
        list(aid, run_id, version_id, owner_key, step_id, no,
             if (nzchar(req_id)) req_id else NA_character_,
             script_path, script_digest, "delegated",
             db_now(), payload))
    })
    TRUE
  }, error = function(e) FALSE)

  if (!inserted) {
    if (nzchar(req_id)) {
      r <- db_rows(con,
        paste0("SELECT * FROM step_attempts WHERE req_id = ? AND step_id = ? ",
               "AND version_id = ? ORDER BY attempt_no DESC LIMIT 1"),
        list(req_id, step_id, version_id))
      if (nrow(r)) {
        att <- as.list(r[1, ])
        closed_status <- c("verified", "rejected", "released", "escalated")
        if (!(wbi_chr(att$status) %in% closed_status))
          return(list(ok = TRUE, attempt_id = wbi_chr(att$attempt_id),
                      run_id = wbi_chr(att$run_id),
                      version_id = wbi_chr(att$version_id), existing = TRUE))
      }
    }
    return(list(ok = FALSE, code = "dispatch_attempt_insert_failed",
                error = "无法登记派工 attempt。"))
  }
  list(ok = TRUE, attempt_id = aid, run_id = run_id, version_id = version_id,
       existing = FALSE)
}

# job_number is a property of one attempt, not a mutable label that a later
# heartbeat may replace. Conflicting numbers are rejected and the first kept.
db_attempt_note_job <- function(con, attempt_id, job_number = "",
                                exec_id = "", executor_mode = "") {
  aid <- wbi_chr(attempt_id)
  if (is.null(con) || !nzchar(aid)) return(list(ok = TRUE, code = "no_attempt"))
  att <- wbi_attempt_by_id(con, aid)
  if (is.null(att)) return(list(ok = FALSE, code = "attempt_id_unknown",
                                error = paste0("attempt_id 不存在：", aid)))
  jn <- wbi_chr(job_number)
  p <- wbi_payload(att)
  kept <- wbi_chr(p$job_number)
  if (nzchar(kept) && nzchar(jn) && !identical(kept, jn))
    return(list(ok = FALSE, code = "job_number_conflict",
                kept = kept, rejected = jn,
                error = paste0("该 attempt 已登记作业号 ", kept,
                               "，拒绝改写为 ", jn, "。")))
  if (nzchar(jn)) p$job_number <- jn
  if (nzchar(exec_id)) p$executor_task_id <- wbi_chr(exec_id)
  if (nzchar(executor_mode)) p$executor_mode <- wbi_chr(executor_mode)
  p$job_number_updated_at <- db_now()
  DBI::dbExecute(con,
    "UPDATE step_attempts SET payload = ? WHERE attempt_id = ?",
    list(db_json(p), aid))
  list(ok = TRUE, code = if (nzchar(kept) && identical(kept, jn)) "same" else "set",
       value = if (nzchar(jn)) jn else kept)
}

db_attempt_note_job_for_exec <- function(con, task, job_number = "",
                                         exec_id = "", executor_mode = "") {
  if (is.null(con) || !is.list(task)) return(list(ok = TRUE, code = "no_attempt"))
  aid <- wbi_chr(task$attempt_id)
  if (!nzchar(aid)) {
    req <- wbi_chr(task$req_id)
    step <- wbi_chr(task$step)
    if (nzchar(req) && nzchar(step)) {
      r <- db_rows(con,
        paste0("SELECT attempt_id FROM step_attempts WHERE req_id = ? AND step_id = ? ",
               "ORDER BY attempt_no DESC LIMIT 1"), list(req, step))
      if (nrow(r)) aid <- wbi_chr(r$attempt_id[[1]])
    }
  }
  if (!nzchar(aid)) return(list(ok = TRUE, code = "no_attempt"))
  db_attempt_note_job(con, aid, job_number = job_number,
                      exec_id = exec_id, executor_mode = executor_mode)
}
