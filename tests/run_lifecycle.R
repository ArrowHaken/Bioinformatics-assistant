#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
self <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
module <- normalizePath(file.path(dirname(self), "..", "engine", "run_lifecycle.R"))
source(module)
wait_for <- function(predicate, seconds = 20) {
  end <- proc.time()[["elapsed"]] + seconds
  while (!isTRUE(predicate())) {
    if (proc.time()[["elapsed"]] > end) stop("Process barrier timed out")
  }
}
if (length(args) && args[1] == "--claim-worker") {
  root <- args[2]; worker <- args[3]
  file.create(file.path(root, paste0(worker, ".ready")))
  wait_for(function() file.exists(file.path(root, "go")))
  answer <- tryCatch({
    x <- rl_claim(root, "race", "a1", worker, paste0("claim-", worker), now = 101)
    list(ok = TRUE, worker = x$worker_id)
  }, run_lifecycle_error = function(e) list(ok = FALSE, code = e$code))
  result <- file.path(root, paste0(worker, ".result"))
  saveRDS(answer, paste0(result, ".tmp"))
  stopifnot(file.rename(paste0(result, ".tmp"), result))
  quit(status = 0)
}
if (length(args) && args[1] == "--restart-reader") {
  x <- rl_read(args[2], "main", now = 120)
  stopifnot(x$status == "succeeded", x$attempt_id == "a2", length(x$evidence) == 1L)
  stopifnot(identical(unname(tools::md5sum(x$evidence[[1]]$path)), x$evidence[[1]]$md5))
  saveRDS(x, file.path(args[2], "restart-read.rds"))
  quit(status = 0)
}

root <- tempfile("lifecycle-tests-")
dir.create(root)
on.exit_cleanup <- function() unlink(root, recursive = TRUE)
# Top-level Rscript has no function on.exit; tryCatch finally owns this temp root.
checks <- 0L
check <- function(x) { stopifnot(isTRUE(x)); checks <<- checks + 1L }
reject <- function(expr, code) {
  got <- tryCatch({ force(expr); "NO_ERROR" }, run_lifecycle_error = function(e) e$code)
  check(identical(got, code))
}
tryCatch({
  r <- file.path(root, "owner-session"); dir.create(r)
  a <- rl_create(r, "main", "a1", "create", list(input = "dataset"), now = 100)
  check(a$status == "queued" && a$seq == 1)
  check(identical(a, rl_create(r, "main", "a1", "create", list(input = "dataset"), now = 200)))
  reject(rl_create(r, "main", "a1", "create", list(input = "other")), "request_conflict")
  b <- rl_claim(r, "main", "a1", "w1", "claim", now = 101)
  check(b$status == "running" && b$seq == 2)
  reject(rl_claim(r, "main", "a1", "w2", "claim2"), "not_queued")
  reject(rl_heartbeat(r, "main", "a1", "w2", "wrong"), "not_holder")
  h <- rl_heartbeat(r, "main", "a1", "w1", "beat", now = 102)
  check(h$heartbeat_at == 102 && h$seq == 3)
  check(identical(h, rl_heartbeat(r, "main", "a1", "w1", "beat", now = 105)))
  reject(rl_finish(r, "main", "a1", "w1", "beat", "failed"), "request_conflict")
  check(rl_read(r, "main", now = 500)$observed_status == "stalled")
  check(rl_read(r, "main", now = 1)$observed_status == "unknown")
  check(rl_read(r, "main", now = 500)$status == "running")
  reject(rl_heartbeat(r, "main", "a1", "w1", "backwards", now = 90), "clock_regression")
  q <- rl_handoff(r, "main", "a1", "w1", "handoff", "a2", now = 103)
  check(q$status == "queued" && q$seq == 4)
  reject(rl_heartbeat(r, "main", "a1", "w1", "oldbeat"), "not_holder")
  reject(rl_finish(r, "main", "a1", "w1", "oldfinish", "failed"), "not_holder")
  rl_claim(r, "main", "a2", "w2", "newclaim", now = 104)
  reject(rl_handoff(r, "main", "a2", "w2", "reuse", "a1"), "attempt_reused")
  reject(rl_heartbeat(r, "main", "a1", "w1", "oldbeat2"), "not_holder")
  reject(rl_finish(r, "main", "a1", "w1", "oldfinish2", "succeeded"), "not_holder")
  reject(rl_finish(r, "main", "a2", "w2", "finish", "succeeded"), "evidence_required")
  output <- file.path(r, "analysis.rds")
  # A real deterministic R calculation produces the evidence, not a simulated delay.
  x <- seq_len(1000)
  fit <- lm(I(x * 3 + 7) ~ x)
  saveRDS(list(coefficients = coef(fit), rows = length(x)), output)
  check(abs(readRDS(output)$coefficients[[2]] - 3) < 1e-9)
  empty <- file.path(r, "empty"); file.create(empty)
  reject(rl_finish(r, "main", "a2", "w2", "empty", "succeeded", empty), "evidence_empty")
  outside <- file.path(root, "outside"); writeLines("not session evidence", outside)
  reject(rl_finish(r, "main", "a2", "w2", "outside", "succeeded", outside), "evidence_outside_root")
  done <- rl_finish(r, "main", "a2", "w2", "finish", "succeeded", output, now = 110)
  check(done$status == "succeeded" && done$seq == 6 && length(done$evidence) == 1)
  check(identical(done, rl_finish(r, "main", "a2", "w2", "finish", "succeeded", output, now = 115)))
  reject(rl_claim(r, "main", "a2", "w2", "reopen"), "not_queued")
  reject(rl_heartbeat(r, "main", "a2", "w2", "terminalbeat"), "not_running")
  reject(rl_finish(r, "main", "a2", "w2", "terminalfinish", "failed"), "not_running")
  for (status in c("failed", "cancelled")) {
    rl_create(r, status, "a", "create", now = 100)
    rl_claim(r, status, "a", "w", "claim", now = 101)
    check(rl_finish(r, status, "a", "w", "finish", status, reason = "explicit", now = 102)$status == status)
    reject(rl_claim(r, status, "a", "w", "reopen"), "not_queued")
  }
  # A different request/run and a different authorized root remain independent.
  check(rl_create(r, "independent", "a1", "create", list(input = "new"), now = 100)$status == "queued")
  r2 <- file.path(root, "owner2-session"); dir.create(r2)
  check(rl_create(r2, "main", "a1", "create", now = 100)$seq == 1)
  check(rl_read(r, "main", now = 120)$status == "succeeded")
  reject(rl_create(r, "../escape", "a1", "bad"), "invalid_id")
  check(is.null(rl_read(r, "missing")))
  lock <- file.path(r, ".lifecycle-main.lock"); dir.create(lock)
  writeLines("unknown owner", file.path(lock, "unknown"))
  reject(rl_heartbeat(r, "main", "a2", "w2", "locked"), "locked")
  check(file.exists(file.path(lock, "unknown")))
  unlink(lock, recursive = TRUE) # This test created it, not the module.
  # Eight independent Rscript processes contend for one queued run.
  rl_create(r, "race", "a1", "create", now = 100)
  workers <- paste0("w", seq_len(8))
  for (w in workers) {
    system2(file.path(R.home("bin"), "Rscript"),
            shQuote(c(self, "--claim-worker", r, w)), wait = FALSE,
            stdout = file.path(r, paste0(w, ".log")), stderr = file.path(r, paste0(w, ".err")))
  }
  wait_for(function() all(file.exists(file.path(r, paste0(workers, ".ready")))))
  file.create(file.path(r, "go"))
  wait_for(function() all(file.exists(file.path(r, paste0(workers, ".result")))))
  answers <- lapply(file.path(r, paste0(workers, ".result")), readRDS)
  wins <- vapply(answers, function(a) a$ok, logical(1))
  check(sum(wins) == 1)
  check(all(vapply(answers[!wins], function(a) a$code %in% c("locked", "not_queued"), logical(1))))
  check(identical(rl_read(r, "race", now = 102)$worker_id, workers[wins]))
  check(rl_read(r, "race", now = 102)$seq == 2)
  winner <- workers[wins]
  beat1 <- rl_heartbeat(r, "race", "a1", winner, "request-one", now = 103)
  beat2 <- rl_heartbeat(r, "race", "a1", winner, "request-two", now = 104)
  check(beat2$seq == beat1$seq + 1 && beat2$heartbeat_at == 104)
  replay <- rl_claim(r, "main", "a1", "w1", "claim", now = 130)
  check(replay$status == "running" && rl_read(r, "main", now = 130)$status == "succeeded")
  check(rl_read(r, "main", now = 130)$seq == done$seq)
  code <- system2(file.path(R.home("bin"), "Rscript"), shQuote(c(self, "--restart-reader", r)))
  check(code == 0 && file.exists(file.path(r, "restart-read.rds")))
  check(identical(readRDS(file.path(r, "restart-read.rds"))$evidence, done$evidence))
  cat(sprintf("PASS: %d checks; 8 Rscript claim contenders, 1 winner; restart readback verified\n", checks))
}, finally = on.exit_cleanup())
