#!/usr/bin/env Rscript
# SQLite identity-chain contract. Temporary database only.

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

args <- commandArgs(trailingOnly = TRUE)
APP <- local({
  a <- sub("^--app=", "", args[grepl("^--app=", args)])
  if (length(a)) normalizePath(a) else normalizePath(getwd())
})
if (!dir.exists(file.path(APP, "engine"))) APP <- normalizePath(file.path(APP, ".."))

source(file.path(APP, "engine", "db.R"))

PASS <- 0L; FAIL <- 0L
ok <- function(name, cond, detail = "") {
  if (isTRUE(cond)) { PASS <<- PASS + 1L; cat(sprintf("  [ok]   %s\n", name)) }
  else { FAIL <<- FAIL + 1L; cat(sprintf("  [FAIL] %s  %s\n", name, detail)) }
}
rejected <- function(expr) inherits(try(expr, silent = TRUE), "try-error")

TMP <- tempfile("db-identity-")
dir.create(TMP, recursive = TRUE, showWarnings = FALSE)
on.exit(try(unlink(TMP, recursive = TRUE, force = TRUE), silent = TRUE), add = TRUE)
con <- db_connect(TMP)
db_init(con)

ok("schema version is v2", identical(db_meta_get(con, "schema_version"), "2"))
for (spec in list(c("runs", "project_id"), c("step_attempts", "project_id"),
                  c("snapshots", "project_id"), c("artifacts", "attempt_id"),
                  c("artifacts", "project_id"))) {
  ok(paste("column", spec[[1]], spec[[2]]),
     spec[[2]] %in% db_table_columns(con, spec[[1]]))
}

OWNER <- "alice"
OWNER2 <- "bob"
PID <- db_ensure_project(con, OWNER, "identity test")
VID <- db_ensure_version(con, PID, OWNER)
RID <- db_ensure_run(con, VID, OWNER, work_root = TMP)
run_row <- db_rows(con, "SELECT * FROM runs WHERE run_id=?", list(RID))
ok("run project matches version", nrow(run_row) == 1L &&
     identical(as.character(run_row$project_id[[1]]), PID))

AID <- "att-identity-1"
DBI::dbExecute(con, paste0(
  "INSERT INTO step_attempts(attempt_id,run_id,version_id,project_id,owner_key,",
  "step_id,attempt_no,status) VALUES(?,?,?,?,?,?,?,?)"),
  list(AID, RID, VID, PID, OWNER, "merge", 1L, "verified"))
attempt_row <- db_rows(con, "SELECT * FROM step_attempts WHERE attempt_id=?", list(AID))
ok("attempt project matches run", nrow(attempt_row) == 1L &&
     identical(as.character(attempt_row$project_id[[1]]), PID))

PID2 <- db_ensure_project(con, OWNER2, "other identity test")
VID2 <- db_ensure_version(con, PID2, OWNER2)
ok("cross-owner run is rejected", rejected(DBI::dbExecute(con, paste0(
  "INSERT INTO runs(run_id,project_id,version_id,owner_key,started_at) VALUES(?,?,?,?,?)"),
  list("run-cross-owner", PID2, VID2, OWNER, db_now()))))
ok("cross-version attempt is rejected", rejected(DBI::dbExecute(con, paste0(
  "INSERT INTO step_attempts(attempt_id,run_id,version_id,project_id,owner_key,step_id,attempt_no,status)",
  " VALUES(?,?,?,?,?,?,?,?)"),
  list("att-cross-version", RID, VID2, PID2, OWNER, "merge", 2L, "verified"))))
ok("cross-owner attempt is rejected", rejected(DBI::dbExecute(con, paste0(
  "INSERT INTO step_attempts(attempt_id,run_id,version_id,project_id,owner_key,step_id,attempt_no,status)",
  " VALUES(?,?,?,?,?,?,?,?)"),
  list("att-cross-owner", RID, VID, PID, OWNER2, "merge", 3L, "verified"))))

SNAP <- "snap-identity-1"
DBI::dbExecute(con, paste0(
  "INSERT INTO snapshots(snapshot_id,attempt_id,version_id,project_id,owner_key,created_at,sealed)",
  " VALUES(?,?,?,?,?,?,?)"), list(SNAP, AID, VID, PID, OWNER, db_now(), 1L))
ok("snapshot binds to attempt", DBI::dbGetQuery(con,
  "SELECT COUNT(*) n FROM snapshots WHERE snapshot_id=? AND attempt_id=?", list(SNAP, AID))$n[[1]] == 1L)
ok("cross-version snapshot is rejected", rejected(DBI::dbExecute(con, paste0(
  "INSERT INTO snapshots(snapshot_id,attempt_id,version_id,project_id,owner_key,created_at,sealed)",
  " VALUES(?,?,?,?,?,?,?)"), list("snap-cross", AID, VID2, PID2, OWNER, db_now(), 1L))))

DBI::dbExecute(con, paste0(
  "INSERT INTO artifacts(artifact_id,snapshot_id,attempt_id,version_id,project_id,owner_key,rel,source)",
  " VALUES(?,?,?,?,?,?,?,?)"), list("art-identity-1", SNAP, AID, VID, PID, OWNER, "plots/a.png", "writeback"))
art <- db_rows(con, "SELECT * FROM artifacts WHERE artifact_id=?", list("art-identity-1"))
ok("artifact carries attempt and project", nrow(art) == 1L &&
     identical(as.character(art$attempt_id[[1]]), AID) &&
     identical(as.character(art$project_id[[1]]), PID))
ok("artifact with wrong attempt is rejected", rejected(DBI::dbExecute(con, paste0(
  "INSERT INTO artifacts(artifact_id,snapshot_id,attempt_id,version_id,project_id,owner_key,rel,source)",
  " VALUES(?,?,?,?,?,?,?,?)"),
  list("art-wrong-attempt", SNAP, "att-not-this-one", VID, PID, OWNER, "plots/b.png", "writeback"))))

ok("sealed snapshot cannot be updated", rejected(DBI::dbExecute(con,
  "UPDATE snapshots SET qc_verdict='pass' WHERE snapshot_id=?", list(SNAP))))
ok("sealed snapshot cannot be deleted", rejected(DBI::dbExecute(con,
  "DELETE FROM snapshots WHERE snapshot_id=?", list(SNAP))))
ok("artifact is append-only", rejected(DBI::dbExecute(con,
  "UPDATE artifacts SET rel='plots/changed.png' WHERE artifact_id=?", list("art-identity-1"))))

DBI::dbExecute(con, paste0(
  "INSERT INTO step_attempts(attempt_id,run_id,version_id,project_id,owner_key,",
  "step_id,attempt_no,req_id,status) VALUES(?,?,?,?,?,?,?,?,?)"),
  list("att-req-1", RID, VID, PID, OWNER, "doublet", 4L, "same-request", "reported"))
ok("one active attempt per request", rejected(DBI::dbExecute(con, paste0(
  "INSERT INTO step_attempts(attempt_id,run_id,version_id,project_id,owner_key,",
  "step_id,attempt_no,req_id,status) VALUES(?,?,?,?,?,?,?,?,?)"),
  list("att-req-2", RID, VID, PID, OWNER, "doublet", 5L, "same-request", "reported"))))

db_init(con)
ok("schema migration is idempotent", identical(db_meta_get(con, "schema_version"), "2") &&
     all(c("attempt_id", "project_id") %in% db_table_columns(con, "artifacts")))

# Upgrade a real v1-shaped database: old rows must gain the scope columns
# before the append-only and scope guards are installed.
V1 <- tempfile("db-identity-v1-")
dir.create(V1, recursive = TRUE, showWarnings = FALSE)
old <- db_connect(V1)
for (sql in c(
  "CREATE TABLE schema_meta(key TEXT PRIMARY KEY, value TEXT NOT NULL)",
  "CREATE TABLE projects(project_id TEXT PRIMARY KEY, name TEXT NOT NULL, owner_key TEXT NOT NULL, created_at TEXT NOT NULL, note TEXT, visibility TEXT NOT NULL DEFAULT 'private', UNIQUE(owner_key,name))",
  "CREATE TABLE versions(version_id TEXT PRIMARY KEY, project_id TEXT NOT NULL REFERENCES projects(project_id), parent_version_id TEXT REFERENCES versions(version_id), label TEXT, frozen_at TEXT, spec_digest TEXT NOT NULL, created_at TEXT NOT NULL, created_by TEXT, reason TEXT, payload TEXT)",
  "CREATE TABLE runs(run_id TEXT PRIMARY KEY, version_id TEXT NOT NULL REFERENCES versions(version_id), owner_key TEXT NOT NULL, work_root TEXT, exec_host TEXT, exec_pc TEXT, status TEXT NOT NULL DEFAULT 'open', started_at TEXT, ended_at TEXT, payload TEXT)",
  "CREATE TABLE step_attempts(attempt_id TEXT PRIMARY KEY, run_id TEXT NOT NULL REFERENCES runs(run_id), version_id TEXT NOT NULL, owner_key TEXT NOT NULL, step_id TEXT NOT NULL, attempt_no INTEGER NOT NULL, req_id TEXT, ticket_id TEXT, script_path TEXT, script_digest TEXT, exit_code INTEGER, status TEXT NOT NULL, reported_at TEXT, verified_at TEXT, started_at TEXT, ended_at TEXT, payload TEXT, UNIQUE(run_id,step_id,attempt_no))",
  "CREATE TABLE snapshots(snapshot_id TEXT PRIMARY KEY, attempt_id TEXT REFERENCES step_attempts(attempt_id), version_id TEXT NOT NULL, owner_key TEXT NOT NULL, created_at TEXT NOT NULL, sealed INTEGER NOT NULL DEFAULT 0, manifest_path TEXT, manifest_md5 TEXT, input_refs TEXT, params TEXT, env_lock TEXT, command TEXT, log_tail TEXT, qc_verdict TEXT, payload TEXT)",
  "CREATE TABLE artifacts(artifact_id TEXT PRIMARY KEY, snapshot_id TEXT REFERENCES snapshots(snapshot_id), version_id TEXT NOT NULL, owner_key TEXT NOT NULL, rel TEXT NOT NULL, bytes INTEGER, md5 TEXT, kind TEXT NOT NULL DEFAULT 'preview', mtime TEXT, source TEXT, UNIQUE(snapshot_id,rel))")) {
  DBI::dbExecute(old, sql)
}
DBI::dbExecute(old, "INSERT INTO schema_meta(key,value) VALUES('schema_version','1')")
DBI::dbExecute(old, "INSERT INTO projects(project_id,name,owner_key,created_at) VALUES('p-v1','legacy','legacy','2026-01-01T00:00:00')")
DBI::dbExecute(old, "INSERT INTO versions(version_id,project_id,label,spec_digest,created_at,created_by) VALUES('v-v1','p-v1','legacy','legacy','2026-01-01T00:00:00','legacy')")
DBI::dbExecute(old, "INSERT INTO runs(run_id,version_id,owner_key,started_at) VALUES('r-v1','v-v1','legacy','2026-01-01T00:00:00')")
DBI::dbExecute(old, "INSERT INTO step_attempts(attempt_id,run_id,version_id,owner_key,step_id,attempt_no,status) VALUES('a-v1','r-v1','v-v1','legacy','doublet',1,'verified')")
DBI::dbExecute(old, "INSERT INTO snapshots(snapshot_id,attempt_id,version_id,owner_key,created_at,sealed) VALUES('s-v1','a-v1','v-v1','legacy','2026-01-01T00:00:00',1)")
DBI::dbExecute(old, "INSERT INTO artifacts(artifact_id,snapshot_id,version_id,owner_key,rel,source) VALUES('art-v1','s-v1','v-v1','legacy','plots/legacy.png','legacy')")
db_init(old)
ok("v1 run is backfilled with project", identical(as.character(db_scalar(old,
  "SELECT project_id FROM runs WHERE run_id='r-v1'")), "p-v1"))
ok("v1 attempt is backfilled with project", identical(as.character(db_scalar(old,
  "SELECT project_id FROM step_attempts WHERE attempt_id='a-v1'")), "p-v1"))
ok("v1 snapshot and artifact are backfilled", identical(as.character(db_scalar(old,
  "SELECT project_id FROM snapshots WHERE snapshot_id='s-v1'")), "p-v1") &&
     identical(as.character(db_scalar(old,
  "SELECT attempt_id FROM artifacts WHERE artifact_id='art-v1'")), "a-v1") &&
     identical(as.character(db_scalar(old,
  "SELECT project_id FROM artifacts WHERE artifact_id='art-v1'")), "p-v1"))
db_init(old)
ok("v1 upgrade is idempotent after guards exist",
   identical(db_meta_get(old, "schema_version"), "2"))
DBI::dbDisconnect(old)

try(DBI::dbDisconnect(con), silent = TRUE)
cat(sprintf("\nPASS=%d FAIL=%d\n", PASS, FAIL))
quit(save = "no", status = if (FAIL == 0L) 0L else 1L)
