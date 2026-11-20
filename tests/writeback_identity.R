#!/usr/bin/env Rscript
# Regression tests for stable write-back identity.
# Run: Rscript tests/writeback_identity.R [--app <dir>]

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

args <- commandArgs(trailingOnly = TRUE)
APP <- local({
  a <- sub("^--app=", "", args[grepl("^--app=", args)])
  if (length(a)) normalizePath(a) else normalizePath(getwd())
})
if (!dir.exists(file.path(APP, "engine"))) APP <- normalizePath(file.path(APP, ".."))

source(file.path(APP, "engine", "db.R"))
source(file.path(APP, "engine", "writeback_identity.R"))
source(file.path(APP, "engine", "db_api.R"))

PASS <- 0L; FAIL <- 0L
ok <- function(name, cond, detail = "") {
  if (isTRUE(cond)) {
    PASS <<- PASS + 1L; cat(sprintf("  [ok]   %s\n", name))
  } else {
    FAIL <<- FAIL + 1L; cat(sprintf("  [FAIL] %s  %s\n", name, detail))
  }
}

TMP <- tempfile("wbi-test-")
dir.create(TMP, recursive = TRUE, showWarnings = FALSE)
on.exit(try(unlink(TMP, recursive = TRUE, force = TRUE), silent = TRUE), add = TRUE)
dir.create(file.path(TMP, "state"), showWarnings = FALSE, recursive = TRUE)
con <- db_connect(TMP)
db_init(con)

OWNER <- "gina"
SID <- "sess-wbi"
PID <- db_ensure_project(con, OWNER, "writeback test")
VID <- db_ensure_version(con, PID, OWNER)
RID <- db_ensure_run(con, VID, OWNER, work_root = TMP)
DBI::dbExecute(con,
  paste0("INSERT INTO sessions(session_id,owner_key,name,mode,run_dir,project_id,",
         "version_id,visibility,created_at,updated_at,payload) ",
         "VALUES(?,?,?,?,?,?,?,?,?,?,?)"),
  list(SID, OWNER, "writeback test", "server", TMP, PID, VID, "private",
       db_now(), db_now(), db_json(list())))

reg1 <- db_register_dispatch(con, OWNER, SID, VID, "doublet", "req-1",
                             exec_id = "ex-1", script_path = "/srv/run.R",
                             executor_mode = "batch")
ok("dispatch creates an attempt", isTRUE(reg1$ok) && nzchar(reg1$attempt_id))
reg2 <- db_register_dispatch(con, OWNER, SID, VID, "doublet", "req-1",
                             exec_id = "ex-1", script_path = "/srv/run.R",
                             executor_mode = "batch")
ok("same req reuses the same attempt", identical(reg1$attempt_id, reg2$attempt_id))

aid <- reg1$attempt_id
att <- wbi_attempt_by_id(con, aid)
ok("attempt starts delegated", identical(as.character(att$status), "delegated"))
ok("attempt carries run/version", identical(as.character(att$run_id), RID) &&
     identical(as.character(att$version_id), VID))
ok("attempt carries project", identical(wbi_chr(att$project_id), PID))
run_row <- db_rows(con, "SELECT * FROM runs WHERE run_id=?", list(RID))
ok("run carries project", nrow(run_row) == 1L &&
     identical(wbi_chr(run_row$project_id[[1]]), PID))

b_good <- list(attempt_id = aid, step = "doublet", version_id = VID,
               run_id = RID, req_id = "req-1", session_id = SID)
ok("explicit attempt binds", isTRUE(wbi_bind_attempt(con, b_good, "doublet", OWNER,
                                                    version_id = VID, run_id = RID)$ok))
bad_step <- wbi_bind_attempt(con, b_good, "merge", OWNER, version_id = VID, run_id = RID)
ok("wrong step is named", identical(bad_step$code, "attempt_step_mismatch"))
bad_ver <- wbi_bind_attempt(con, b_good, "doublet", OWNER,
                            version_id = "other-V9", run_id = RID)
ok("stale version echoed with a valid attempt_id is overridden",
   isTRUE(bad_ver$ok) && identical(bad_ver$overrides$version_id$kept, VID))
bad_owner <- wbi_bind_attempt(con, b_good, "doublet", "someone-else",
                              version_id = VID, run_id = RID)
ok("wrong owner is named", identical(bad_owner$code, "attempt_owner_mismatch"))

j1 <- db_attempt_note_job(con, aid, "12345", exec_id = "ex-1", executor_mode = "batch")
j2 <- db_attempt_note_job(con, aid, "12345", exec_id = "ex-1", executor_mode = "batch")
j3 <- db_attempt_note_job(con, aid, "99999", exec_id = "ex-1", executor_mode = "batch")
ok("job number first write accepted", isTRUE(j1$ok))
ok("same job number idempotent", isTRUE(j2$ok) && identical(j2$code, "same"))
ok("different job number rejected", !isTRUE(j3$ok) &&
     identical(j3$code, "job_number_conflict"))

# A verified attempt replayed with the same script digest is idempotent and must
# not enter verdict again.
DBI::dbExecute(con,
  "UPDATE step_attempts SET status='verified', script_digest='abc', verified_at=? WHERE attempt_id=?",
  list(db_now(), aid))
r1 <- db_api_report_step(con, TMP, list(key = OWNER),
  list(attempt_id = aid, step = "doublet", session_id = SID, version_id = VID,
       run_id = RID, req_id = "req-1", script_digest = "abc", status = "done",
       job_number = "12345"),
  runDir = TMP, sid = SID)
ok("verified replay is idempotent", isTRUE(r1$accepted) &&
     identical(r1$verdict, "idempotent") && isTRUE(r1$duplicate))
r2 <- db_api_report_step(con, TMP, list(key = OWNER),
  list(attempt_id = aid, step = "doublet", session_id = SID, version_id = VID,
       run_id = RID, req_id = "req-1", script_digest = "different", status = "done",
       job_number = "12345"),
  runDir = TMP, sid = SID)
ok("verified replay with different script is rejected",
   !isTRUE(r2$accepted) && identical(r2$code, "attempt_script_mismatch"))

DBI::dbExecute(con, "UPDATE step_attempts SET status='rejected' WHERE attempt_id=?",
               list(aid))
r3 <- db_api_report_step(con, TMP, list(key = OWNER),
  list(attempt_id = aid, step = "doublet", session_id = SID, version_id = VID,
       run_id = RID, req_id = "req-1", script_digest = "abc", status = "done",
       job_number = "12345"),
  runDir = TMP, sid = SID)
ok("closed attempt is not silently reopened",
   !isTRUE(r3$accepted) && identical(r3$code, "attempt_closed"))

# A closed attempt must never be reused, even when a caller replays the same
# request id. New work gets a new immutable attempt number.
reg_re <- db_register_dispatch(con, OWNER, SID, VID, "doublet", "req-1",
                               exec_id = "ex-re", script_path = "/srv/run.R",
                               executor_mode = "login")
ok("closed request gets a new attempt", isTRUE(reg_re$ok) &&
     !identical(reg_re$attempt_id, aid))

# A genuinely different request also gets its own attempt.
reg3 <- db_register_dispatch(con, OWNER, SID, VID, "doublet", "req-2",
                             exec_id = "ex-2", script_path = "/srv/run.R",
                             executor_mode = "login")
ok("new request gets a new attempt", isTRUE(reg3$ok) &&
     !identical(reg3$attempt_id, aid))

# Dispatch records a digest for scripts the application host can actually read.
script_file <- file.path(TMP, "dispatch-script.R")
writeLines("cat('hello')\n", script_file, useBytes = TRUE)
reg_digest <- db_register_dispatch(con, OWNER, SID, VID, "markers", "req-digest",
                                   exec_id = "ex-digest", script_path = script_file,
                                   executor_mode = "login")
att_digest <- wbi_attempt_by_id(con, reg_digest$attempt_id)
ok("dispatch computes readable script md5",
   isTRUE(reg_digest$ok) &&
     identical(wbi_chr(att_digest$script_digest), unname(tools::md5sum(script_file))))

# A legacy callback is anchored to the record's current version. An old
# attempt from V1 must not capture a write-back after the record moved to V2.
PID2 <- db_ensure_project(con, OWNER, "writeback test v2")
VID2 <- db_ensure_version(con, PID2, OWNER)
DBI::dbExecute(con, "UPDATE sessions SET project_id=?, version_id=? WHERE session_id=?",
               list(PID2, VID2, SID))
legacy <- db_api_report_step(con, TMP, list(key = OWNER),
  list(step = "doublet", session_id = SID, req_id = "req-1", version_id = VID,
       script_digest = "abc", status = "done", job_number = "12345"),
  runDir = TMP, sid = SID)
v2_rows <- db_rows(con,
  "SELECT * FROM step_attempts WHERE req_id='req-1' AND step_id='doublet' AND version_id=?",
  list(VID2))
ok("legacy callback is written into the record's current version",
   nrow(v2_rows) == 1L && identical(wbi_chr(v2_rows$version_id[[1]]), VID2))
ok("legacy callback does not silently reuse the older version",
   is.null(legacy$attempt_id) || !identical(wbi_chr(legacy$attempt_id), aid))

try(DBI::dbDisconnect(con), silent = TRUE)
cat(sprintf("\nPASS=%d FAIL=%d\n", PASS, FAIL))
quit(save = "no", status = if (FAIL == 0L) 0L else 1L)
