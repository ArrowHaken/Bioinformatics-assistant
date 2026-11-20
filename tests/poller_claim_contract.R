#!/usr/bin/env Rscript
# Poller lease contract: atomic claim, expiry, finish, drain limit and real
# multi-process contention on one SQLite database.

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

args <- commandArgs(trailingOnly = TRUE)
self <- normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
APP <- normalizePath(file.path(dirname(self), ".."))
source(file.path(APP, "engine", "db.R"))
source(file.path(APP, "engine", "poller.R"))

wait_for <- function(predicate, seconds = 20) {
  end <- proc.time()[["elapsed"]] + seconds
  while (!isTRUE(predicate())) {
    if (proc.time()[["elapsed"]] > end) stop("process barrier timed out")
    Sys.sleep(0.02)
  }
}

if (length(args) && identical(args[1], "--claim-worker")) {
  root <- args[2]; worker <- args[3]; now <- args[4]; tag <- args[5]
  file.create(file.path(root, paste0(tag, ".ready")))
  wait_for(function() file.exists(file.path(root, "go")))
  con <- db_connect(root)
  answer <- tryCatch({
    job <- poller_claim(con, worker, now)
    if (is.null(job)) list(ok = FALSE)
    else list(ok = TRUE, job_id = as.character(job$job_id),
              worker = as.character(job$leased_by))
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
  DBI::dbDisconnect(con)
  result <- file.path(root, paste0(tag, ".result"))
  saveRDS(answer, paste0(result, ".tmp"))
  stopifnot(file.rename(paste0(result, ".tmp"), result))
  quit(status = 0)
}

PASS <- 0L; FAIL <- 0L
ok <- function(name, cond, detail = "") {
  if (isTRUE(cond)) { PASS <<- PASS + 1L; cat(sprintf("  [ok]   %s\n", name)) }
  else { FAIL <<- FAIL + 1L; cat(sprintf("  [FAIL] %s  %s\n", name, detail)) }
}

TMP <- tempfile("poller-claim-")
dir.create(TMP, recursive = TRUE, showWarnings = FALSE)
on.exit(try(unlink(TMP, recursive = TRUE, force = TRUE), silent = TRUE), add = TRUE)
con <- db_connect(TMP)
db_init(con)

NOW <- "2026-11-20T10:00:00"
insert_job <- function(id, kind = "unknown", due = NOW) {
  DBI::dbExecute(con, paste0(
    "INSERT INTO poll_jobs(job_id,kind,owner_key,state,next_check_at,created_at,updated_at) ",
    "VALUES(?,?,?,?,?,?,?)"),
    list(id, kind, "tester", "pending", due, due, due))
}

insert_job("job-one")
first <- poller_claim(con, "worker-a", NOW)
ok("first claim wins", isTRUE(first$job_id == "job-one") && first$leased_by == "worker-a")
row <- db_rows(con, "SELECT * FROM poll_jobs WHERE job_id='job-one'")
ok("claim records lease", identical(as.character(row$state[[1]]), "leased") &&
     identical(as.character(row$lease_until[[1]]), "2026-11-20T10:01:30") &&
     startsWith(as.character(row$leased_by[[1]]), "worker-a:"))
ok("leased job is not claimable by another worker",
   is.null(poller_claim(con, "worker-b", NOW)))

before_expiry <- poller_claim(con, "worker-b", "2026-11-20T10:01:30")
ok("lease boundary is not expired at lease_until", is.null(before_expiry))
after_expiry <- poller_claim(con, "worker-b", "2026-11-20T10:01:31")
ok("expired lease can be claimed again", isTRUE(after_expiry$job_id == "job-one") &&
     after_expiry$leased_by == "worker-b")

poller_finish(con, after_expiry, ok = TRUE)
finished <- db_rows(con, "SELECT * FROM poll_jobs WHERE job_id='job-one'")
ok("finish clears lease", identical(as.character(finished$state[[1]]), "done") &&
     is.na(finished$lease_until[[1]]) && is.na(finished$leased_by[[1]]))
ok("finished job is not claimable", is.null(poller_claim(con, "worker-c",
  "2026-11-20T10:02:00")))

for (i in seq_len(5L)) insert_job(paste0("drain-", i))
ok("drain respects limit", identical(poller_drain(con, TMP, "drain", limit = 3L), 3L))
ok("drain limit zero does no work",
   identical(poller_drain(con, TMP, "drain", limit = 0L), 0L))
ok("remaining jobs stay claimable",
   identical(poller_drain(con, TMP, "drain", limit = 10L), 2L))

insert_job("race-a")
insert_job("race-b")
workers <- paste0("c", seq_len(8L))
for (w in workers) {
  system2(file.path(R.home("bin"), "Rscript"),
          shQuote(c(self, "--claim-worker", TMP, "shared", NOW, w)),
          wait = FALSE, stdout = file.path(TMP, paste0(w, ".log")),
          stderr = file.path(TMP, paste0(w, ".err")))
}
wait_for(function() all(file.exists(file.path(TMP, paste0(workers, ".ready")))))
file.create(file.path(TMP, "go"))
wait_for(function() all(file.exists(file.path(TMP, paste0(workers, ".result")))))
answers <- lapply(file.path(TMP, paste0(workers, ".result")), readRDS)
wins <- vapply(answers, function(x) isTRUE(x$ok), logical(1))
won <- unique(vapply(answers[wins], function(x) as.character(x$job_id), ""))
ok("exactly two concurrent claims win for two jobs", sum(wins) == 2L)
ok("same worker name never returns the same job twice",
   length(won) == 2L && setequal(won, c("race-a", "race-b")))
ok("losers return cleanly", all(vapply(answers[!wins],
  function(x) is.null(x$error), logical(1))))

try(DBI::dbDisconnect(con), silent = TRUE)
cat(sprintf("\nPASS=%d FAIL=%d\n", PASS, FAIL))
quit(save = "no", status = if (FAIL == 0L) 0L else 1L)
