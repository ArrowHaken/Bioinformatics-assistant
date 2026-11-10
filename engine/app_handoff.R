pah_steps <- function() c("all", "env", "decide", "precheck", "pipeline1", "pipeline2", "plots", "ppt", "pack")

# Transport shape only. Route normalizes business fields after authorizing session.
pah_intent <- function(value, topic_id, request_id) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      nchar(value, type = "chars") > 512L) return(NULL)
  it <- tryCatch(jsonlite::fromJSON(value, simplifyVector = FALSE), error = function(e) NULL)
  obj <- function(x) is.list(x) && !is.null(names(x)) && !anyNA(names(x)) &&
    !anyDuplicated(names(x)) && all(nzchar(names(x)))
  allowed <- list(server = c("label", "host", "user", "port", "exec_pc", "samples", "runtime"),
    paths = c("data_root", "work_root", "rawdata_dir", "script_root", "liny_data"),
    samples = c("count", "group_table", "groups"), reference = c("notebook", "modified_cells"),
    scope = c("run_until", "annotation_gate"), annotation = c("policy", "merge_adjacent"),
    source = c("topic_id", "message_id", "raw_text"))
  keys <- c("schema", "project_name", "mode", names(allowed), "species", "tissue")
  if (!obj(it) || any(!names(it) %in% keys) || !identical(it$schema, "pa.intent.v1")) return(NULL)
  scalar <- function(x) length(x) == 1L && !is.list(x) && !is.null(x) && !is.na(x) &&
    (is.character(x) || is.logical(x) || (is.numeric(x) && is.finite(x)))
  for (k in names(it)) {
    v <- it[[k]]
    if (k %in% names(allowed)) {
      if (!obj(v) || any(!names(v) %in% allowed[[k]])) return(NULL)
      for (n in names(v)) {
        x <- v[[n]]
        if (identical(n, "runtime")) {
          if (!obj(x) || any(!names(x) %in% c("rscript", "lib_path", "ld_library_path",
              "python", "queue", "qsub", "hdf5", "node_constraint")) ||
              !all(vapply(x, scalar, logical(1)))) return(NULL)
        } else if (n %in% c("groups", "modified_cells")) {
          if (!is.list(x) || !is.null(names(x)) || !all(vapply(x, scalar, logical(1)))) return(NULL)
        } else if (!scalar(x)) return(NULL)
      }
    } else if (!scalar(v)) return(NULL)
  }
  # Provenance is bound to authenticated envelope identity, never caller source claims.
  it$source <- list(topic_id = topic_id, message_id = request_id)
  it
}

# Pure validator. Caller MUST supply identity from the fixed HTTPS gateway, never
# body/header claims; owner_ctx from authenticated owner resolution; configured agent.
# Success includes validated action, parameters, parsed intent and defaulted step.
# Failure: list(ok=FALSE, code="handoff_rejected"). No I/O, clock reads, or writes.
# Wire envelope: protocol,type,nonce,request_id,topic_id,agent_uid,action,
# parameters=list(session_id, intent? JSON string) or list(session_id, step?).
# Identity: authenticated=TRUE, viewer=list(id=<string>), topic_id=<string>,
# agent_uid=<string> OR agent=list(uid=<string>); if both supplied both must match.
# Owner: kind="viewer", key=paste0("viewer:", identity$viewer$id).
# Session ownership/existence MUST additionally be authorized by the route before
# returning success; this function validates identity binding, not database access.
pah_validate <- function(envelope, identity, owner_ctx, expected_agent_uid, now_ms) {
  reject <- function() list(ok = FALSE, code = "handoff_rejected")
  field <- function(x, k) if (is.list(x)) x[[k, exact = TRUE]] else NULL
  object <- function(x) is.list(x) && !is.data.frame(x) &&
    !is.null(names(x)) && !anyNA(names(x)) && !anyDuplicated(names(x))
  exact <- function(x, keys) object(x) && length(x) == length(keys) && setequal(names(x), keys)
  text <- function(x) is.character(x) && length(x) == 1L && !is.na(x) &&
    grepl("^[A-Za-z0-9][A-Za-z0-9_-]{0,127}$", x, perl = TRUE)
  epoch <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x) &&
    is.finite(x) && x >= 0 && x <= 9007199254740991 && x == floor(x)
  keys <- c("protocol", "type", "nonce", "request_id", "topic_id", "agent_uid", "action", "parameters", "expires_at")
  if (!exact(envelope, keys) || !identical(field(envelope, "protocol"), "catsco.app-handoff.v1") ||
      !identical(field(envelope, "type"), "handoff") ||
      !is.character(field(envelope, "action")) || length(field(envelope, "action")) != 1L || is.na(field(envelope, "action")) ||
      !field(envelope, "action") %in% c("initialize", "run")) return(reject())
  request_id <- field(envelope, "request_id")
  if (!is.character(request_id) || length(request_id) != 1L || is.na(request_id) ||
      !grepl("^[A-Za-z0-9_-]{8,80}$", request_id, perl = TRUE)) return(reject())
  if (!all(vapply(c("nonce", "topic_id", "agent_uid"),
      function(k) text(field(envelope, k)), logical(1)))) return(reject())
  parameters <- field(envelope, "parameters")
  action <- field(envelope, "action")
  allowed <- if (identical(action, "initialize")) c("session_id", "intent") else c("session_id", "step")
  if (!object(parameters) || any(!names(parameters) %in% allowed) ||
      !text(field(parameters, "session_id"))) return(reject())
  intent <- NULL
  if ("intent" %in% names(parameters)) {
    intent <- pah_intent(parameters$intent, envelope$topic_id, envelope$request_id)
    if (is.null(intent)) return(reject())
  }
  step <- field(parameters, "step")
  if ("step" %in% names(parameters) && (!is.character(step) || length(step) != 1L ||
      is.na(step) || !step %in% pah_steps())) return(reject())
  if (is.null(step)) step <- "all"
  expires <- field(envelope, "expires_at")
  if (!epoch(now_ms) || !epoch(expires) || expires <= now_ms || expires > now_ms + 300000) return(reject())
  if (!object(identity) || !identical(field(identity, "authenticated"), TRUE) ||
      !object(owner_ctx) || !text(expected_agent_uid)) return(reject())
  viewer <- field(identity, "viewer")
  viewer_id <- field(viewer, "id")
  if (!object(viewer) || !text(viewer_id) || !identical(field(owner_ctx, "kind"), "viewer") ||
      !identical(field(owner_ctx, "key"), paste0("viewer:", viewer_id))) return(reject())
  topic <- field(identity, "topic_id")
  if (!text(topic) || !identical(topic, field(envelope, "topic_id"))) return(reject())
  direct_present <- "agent_uid" %in% names(identity)
  nested <- field(identity, "agent")
  nested_present <- is.list(nested) && "uid" %in% names(nested)
  if (!direct_present && !nested_present) return(reject())
  agents <- list()
  if (direct_present) agents <- c(agents, list(field(identity, "agent_uid")))
  if (nested_present) agents <- c(agents, list(field(nested, "uid")))
  if (!all(vapply(agents, function(x) text(x) && identical(x, expected_agent_uid) &&
      identical(x, field(envelope, "agent_uid")), logical(1)))) return(reject())
  list(ok = TRUE, session_id = field(parameters, "session_id"), request_id = field(envelope, "request_id"),
    action = action, parameters = parameters, intent = intent, step = step)
}
