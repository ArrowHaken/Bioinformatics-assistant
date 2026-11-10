#!/usr/bin/env Rscript
source("engine/runs.R")
now <- as.POSIXct("2026-11-09T12:00:00", format = "%Y-%m-%dT%H:%M:%S")
fresh <- "2026-11-09T11:59:55"
old <- "2026-11-09T11:50:00"
checks <- 0L
expect <- function(actual, expected, label) {
  if (!identical(actual, expected)) stop(label, ": expected ", expected, ", got ", actual)
  checks <<- checks + 1L
}
step <- function(id = "a", status = "done", at = fresh, ...) list(id = id, status = status, imported_at = at, ...)
summary <- function(steps = list(), flow = list(), order = c("a", "b")) runs_summarize(list(id = "test"), steps, flow, order, now)
expect(summary(list(step()))$state, "idle", "partial completion is not running")
expect(summary(list(step(), step("b")))$state, "completed", "all expected done")
expect(summary(list(step(status = "failed"), step("b", "running")))$state, "failed", "failure beats fresh running")
expect(summary(list(step(status = "delegated")))$state, "idle", "delegation is not execution")
expect(summary(list(step(status = "running")))$state, "running", "explicit fresh execution")
expect(summary(list(step(status = "running", at = old), step("b")))$state, "stalled", "other fresh writes cannot renew heartbeat")
expect(summary(list(step(status = "running", at = "2026-11-09T10:00:00")))$state, "lost", "lost execution")
expect(summary(list(step(status = "running", at = "2026-11-10T12:00:00")))$state, "unknown", "future heartbeat")
expect(summary(list(step(status = "running", at = "bad")))$state, "unknown", "invalid heartbeat")
expect(summary()$state, "unknown", "no observations")
expect(summary(list(step()), order = character())$state, "unknown", "empty expected set not completed")
expect(summary(flow = list(list(stage = "a", status = "running", updated_at = fresh)))$state, "running", "flow participates")
expect(summary(list(step()), list(list(stage = "a", status = "running", updated_at = fresh)))$state, "idle", "done cannot be revived by flow")
expect(summary(list(step()), list(list(stage = "a", status = "failed", updated_at = fresh)))$state, "failed", "flow failure wins")
expect(summary(flow = list(list(stage = "a", status = "done"), list(stage = "b", status = "completed")))$state, "completed", "flow completion")
expect(summary(list(step(status = "running", subs = list(list(id = "x", status = "failed")))))$state, "failed", "substep failure")
r <- summary(list(step(status = "running", at = old, subs = list(list(id = "x", title = "real sub", status = "running", imported_at = fresh)))))
expect(r$state, "running", "substep heartbeat")
expect(r$current_sub, "real sub（1/1）", "substep label")
expect(summary(list(step(status = "delegated")), list(list(stage = "a", ok = TRUE, imported_at = fresh)))$state, "idle", "review is not execution")
expect(summary(list(step("outside", "running")))$state, "unknown", "out-of-order execution ignored")
expect(summary(list(step()), order = c("a", "a"))$total_steps, 1L, "unique expected stages")
expect(summary(list(step(status = "failed")), list(list(stage = "a", status = "done")))$state, "failed", "fresh flow cannot clear failure")
expect(summary(flow = list(list(stage = "a", status = "delegated", updated_at = fresh)))$state, "idle", "flow delegation")
expect(summary(flow = list(list(stage = "a", subs = list(list(id = "x", status = "running", started_at = fresh)))))$state, "running", "flow explicit sub execution")
expect(summary(list(step(status = "running", at = "bad"), step("b", "running")))$current_stage, "b", "verified stage preferred")
for (bad in list(NULL, NA_character_, character(), c(fresh, fresh), "2026-11-09", paste0(fresh, "garbage"), "2026-02-30T12:00:00", "2026-11-09T25:00:00", "2026-11-09T11:59:55Z", "2026-11-09T11:59:55.123")) {
  expect(is.na(runs_parse_time(bad)), TRUE, "strict timestamp")
}
expect(runs_liveness("2026-11-10T12:00:00", now)$state, "unknown", "future liveness")
for (age in c(0, 90, 91, 1800, 1801)) {
  at <- format(now - age, "%Y-%m-%dT%H:%M:%S")
  expect(runs_liveness(at, now)$state, if (age <= 90) "running" else if (age <= 1800) "stalled" else "lost", "heartbeat boundary")
}
for (st in c("running", "stalled", "lost", "failed", "completed", "idle", "unknown", "unexpected")) {
  text <- runs_state_sentence(list(state = st, age_sec = 120L))
  expect(is.character(text) && length(text) == 1L && nzchar(text), TRUE, "state sentence")
  if (st != "running") expect(grepl("正在跑", text, fixed = TRUE), FALSE, "non-running wording")
}
expected_fields <- c("session_id", "name", "mode", "run_dir", "state", "age_sec", "last_at", "current_stage", "current_title", "current_sub", "done_steps", "failed_steps", "total_steps")
expect(all(expected_fields %in% names(summary())), TRUE, "field compatibility")
cat("PASS runs_summary:", checks, "checks\n")
