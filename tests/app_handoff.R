source("engine/app_handoff.R")
now <- 1900000000000
envelope <- list(protocol = "catsco.app-handoff.v1", type = "handoff", nonce = "nonce1",
  request_id = "request1", topic_id = "topic1", agent_uid = "agent1", action = "initialize",
  parameters = list(session_id = "session1"), expires_at = now + 60000)
identity <- list(authenticated = TRUE, viewer = list(id = "viewer1"), topic_id = "topic1", agent_uid = "agent1")
owner <- list(key = "viewer:viewer1", kind = "viewer")
validate <- function(e = envelope, i = identity, o = owner, a = "agent1", n = now) pah_validate(e, i, o, a, n)
stopifnot(identical(validate(), list(ok = TRUE, session_id = "session1", request_id = "request1")))
reject <- function(...) stopifnot(identical(validate(...), list(ok = FALSE, code = "handoff_rejected")))
for (k in c("nonce", "request_id", "topic_id", "agent_uid")) {
  e <- envelope; e[[k]] <- list("nested"); reject(e)
  e <- envelope; e[[k]] <- ""; reject(e)
}
for (k in c("topic_id", "agent_uid")) { e <- envelope; e[[k]] <- "forged"; reject(e) }
for (value in c(now - 1, now, now + 300001, Inf, NA_real_, 1.5)) { e <- envelope; e$expires_at <- value; reject(e) }
e <- envelope; e$expires_at <- now + 300000; stopifnot(validate(e)$ok)
e <- envelope; e$expires_at <- as.character(now + 1); reject(e)
for (value in list(list(session_id = "session1", command = "whoami"), list(session_id = list(id = "session1")),
    list(session_id = "../state"), list(path = "/tmp/x"), list(session_id = paste(rep("x", 129), collapse = "")),
    list(session_id = "secret;command"), list(session_id = NA_character_), list(session_id = c("a", "b")), list())) {
  e <- envelope; e$parameters <- value; reject(e)
}
for (k in names(envelope)) { e <- envelope; e[[k]] <- NULL; reject(e) }
e <- envelope; e$action <- "run"; reject(e)
e <- envelope; e$type <- "hello"; reject(e)
e <- envelope; e$secret <- "secret"; reject(e)
e <- envelope; names(e)[2] <- "protocol"; reject(e)
for (value in list(FALSE, 1, "true", NULL, NA)) { i <- identity; i$authenticated <- value; reject(i = i) }
for (value in list(NULL, list(id = "wrong"), list(uid = "viewer1"), list(id = c("a", "b")))) { i <- identity; i$viewer <- value; reject(i = i) }
for (value in list(NULL, "wrong", list("topic1"))) { i <- identity; i$topic_id <- value; reject(i = i) }
for (value in list(NULL, "wrong", list("agent1"))) { i <- identity; i$agent_uid <- value; reject(i = i) }
i <- identity; i$agent_uid <- NULL; i$agent <- list(uid = "agent1"); stopifnot(validate(i = i)$ok)
i$agent$uid <- "wrong"; reject(i = i)
i <- identity; i$agent <- list(uid = "wrong"); reject(i = i)
i <- identity; i$agent <- list(uid = "agent1"); stopifnot(validate(i = i)$ok)
for (value in list(NULL, "", "wrong", NA_character_, c("agent1", "agent2"))) reject(a = value)
for (value in list(NULL, list(kind = "topic", key = "viewer:viewer1"), list(kind = "viewer", key = "viewer:other"), list(kind = "agent", key = "viewer:viewer1"))) reject(o = value)
reject(n = NA_real_); reject(n = as.character(now)); reject(n = Inf)
before <- serialize(list(envelope, identity, owner), NULL)
validate()
stopifnot(identical(before, serialize(list(envelope, identity, owner), NULL)))
cat("app_handoff.R: PASS (identity/owner/agent matrix, envelope/schema, expiry, pure validation)\n")
