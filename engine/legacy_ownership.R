# Only server-loaded approvals and a response fetched from the fixed gateway may enter here.
legacy_scalar <- function(x) {
  if (length(x) != 1L || is.list(x) || is.na(x)) return("")
  trimws(as.character(x))
}
legacy_approval <- function(app_dir) {
  f <- file.path(app_dir, "config", "legacy_ownership_approvals.json")
  if (!file.exists(f)) return(NULL)
  jsonlite::fromJSON(f, simplifyVector = FALSE)
}
legacy_identity_matches <- function(ident, approval) {
  if (!is.list(ident) || !isTRUE(ident$authenticated) || !is.list(ident$viewer) ||
      !is.list(approval) || !identical(approval$enabled, TRUE)) return(FALSE)
  v <- ident$viewer
  # id is required even when a separately verified uid is configured.
  if (!nzchar(legacy_scalar(v$id))) return(FALSE)
  verified <- approval$verified_identity
  if (is.list(verified) && length(verified)) {
    fields <- names(verified)
    if (!length(fields) || any(!fields %in% c("id", "uid"))) return(FALSE)
    return(all(vapply(fields, function(k) nzchar(legacy_scalar(verified[[k]])) &&
      identical(legacy_scalar(v[[k]]), legacy_scalar(verified[[k]])), TRUE)))
  }
  identical(v$username, approval$username) && identical(approval$username, "gina")
}
legacy_claim_approved <- function(con, ident, approval) {
  if (!legacy_identity_matches(ident, approval)) return(invisible(FALSE))
  sids <- unlist(approval$session_ids, use.names = FALSE)
  allowed <- c("s-20261022105407-dcol", "s-20261026154846-952j")
  if (!nzchar(legacy_scalar(approval$approval_id)) || !length(sids) || anyDuplicated(sids) || any(!sids %in% allowed) ||
      !nzchar(legacy_scalar(approval$authorization))) stop("invalid legacy approval")
  owner <- owner_from_identity(ident)
  if (!identical(owner$kind, "viewer") || !identical(owner$external_id, legacy_scalar(ident$viewer$id)))
    stop("legacy identity key mismatch")
  db_tx(con, function() {
    mark <- paste0("legacy-claim:", approval$approval_id)
    done <- db_meta_get(con, mark)
    if (!is.null(done)) {
      if (!identical(done, owner$key)) stop("approval already bound to another identity")
      return(invisible(FALSE))
    }
    tables <- DBI::dbListTables(con)
    rows <- lapply(setNames(tables, tables), function(t) DBI::dbReadTable(con, t))
    ss <- rows$sessions[rows$sessions$session_id %in% sids,,drop=FALSE]
    if (nrow(ss) != length(sids) || any(ss$owner_key != "legacy")) stop("approved sessions missing or already owned")
    ps <- unique(ss$project_id); vs <- rows$versions$version_id[rows$versions$project_id %in% ps]
    if (anyNA(ps) || any(!nzchar(ps)) || !all(ss$version_id %in% vs)) stop("incomplete session graph")
    other <- rows$sessions[!rows$sessions$session_id %in% sids,,drop=FALSE]
    if (any(other$project_id %in% ps | other$version_id %in% vs)) stop("shared project/version; manual review required")
    rs <- rows$runs$run_id[rows$runs$version_id %in% vs]
    ats <- rows$step_attempts$attempt_id[rows$step_attempts$run_id %in% rs]
    snaps <- rows$snapshots$snapshot_id[rows$snapshots$version_id %in% vs]
    tickets <- rows$rework_tickets$ticket_id[rows$rework_tickets$version_id %in% vs]
    reqs <- unique(na.omit(rows$step_attempts$req_id[rows$step_attempts$attempt_id %in% ats]))
    reqs <- reqs[nzchar(reqs)]
    if (any(rows$step_attempts$req_id[!rows$step_attempts$attempt_id %in% ats] %in% reqs)) stop("shared request identity")
    selected <- list(); counts <- list()
    supported <- c("owners","projects","runs","step_attempts","snapshots","artifacts","events","reviews","rework_tickets","poll_jobs","fetch_jobs","sessions","run_log_observations","decisions","outbox")
    for (t in names(rows)) {
      d <- rows[[t]]
      if (!"owner_key" %in% names(d) || t == "owners") next
      if (!t %in% supported) stop(paste("unreviewed owner table", t))
      hit <- rep(FALSE, nrow(d))
      links <- list(session_id=sids, project_id=ps, version_id=vs, run_id=rs,
                    attempt_id=ats, snapshot_id=snaps, ticket_id=tickets)
      for (k in intersect(names(links), names(d))) hit <- hit | d[[k]] %in% links[[k]]
      if ("payload" %in% names(d)) {
        for (i in seq_len(nrow(d))) {
          p <- tryCatch(jsonlite::fromJSON(d$payload[i], simplifyVector=FALSE), error=function(e) NULL)
          if (is.list(p) && (legacy_scalar(p$legacy_session) %in% sids ||
              legacy_scalar(p$legacy_session_id) %in% sids || legacy_scalar(p$session_id) %in% sids)) hit[i] <- TRUE
        }
      }
      if (t == "outbox") {
        hit <- hit | d$session_ref %in% sids | d$req_id %in% reqs
        for (sid in sids) hit <- hit | grepl(paste0("(?<![A-Za-z0-9-])",sid,"(?![A-Za-z0-9-])"), d$text, perl=TRUE)
        for (sid in other$session_id) {
          if (any(hit & grepl(paste0("(?<![A-Za-z0-9-])",sid,"(?![A-Za-z0-9-])"), d$text, perl=TRUE))) stop("shared outbox session references")
        }
      }
      # Cross-linked rows are not safe to claim even if one link matches.
      links$session_ref <- sids
      for (k in intersect(c("session_id","session_ref","project_id","version_id","run_id","attempt_id","snapshot_id","ticket_id"), names(d))) {
        active <- !is.na(d[[k]]) & nzchar(d[[k]])
        if (k == "session_ref") active <- active & d[[k]] %in% rows$sessions$session_id
        if (any(hit & active & !d[[k]] %in% links[[k]])) stop(paste("cross-linked", t, k))
      }
      # Other owners' authored reviews/decisions/jobs are not legacy property.
      # Structural graph ownership must agree; authored records remain untouched.
      if (t %in% c("sessions","projects") &&
          any(hit & !is.na(d$owner_key) & d$owner_key != "legacy")) stop(paste("conflicting owner", t))
      selected[[t]] <- which(hit & !is.na(d$owner_key) & d$owner_key == "legacy")
    }
    owner_touch(con, owner)
    # Keep historical event authorship immutable. The new event records the ownership transfer.
    for (t in names(selected)) {
      idx <- selected[[t]]
      if (!length(idx) || t == "events") next
      d <- rows[[t]]
      info <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(", t, ")"))
      pk <- info$name[info$pk == 1L]
      if (length(pk) != 1L) stop(paste("unsupported primary key", t))
      triggers <- NULL
      if (t == "run_log_observations") {
        triggers <- DBI::dbGetQuery(con, "SELECT name,sql FROM sqlite_master WHERE type='trigger' AND tbl_name='run_log_observations' AND upper(sql) LIKE '%BEFORE UPDATE%'")
        for (tr in triggers$name) DBI::dbExecute(con, paste0('DROP TRIGGER ', DBI::dbQuoteIdentifier(con,tr)))
      }
      for (i in idx) DBI::dbExecute(con, paste0("UPDATE ",t," SET owner_key=? WHERE ",pk,"=? AND owner_key='legacy'"), list(owner$key,d[[pk]][i]))
      if (!is.null(triggers)) for (sql in triggers$sql) DBI::dbExecute(con, sql)
      counts[[t]] <- length(idx)
    }
    db_event(con, "legacy_ownership_authorized", owner_key=owner$key, payload=list(
      approval_id=approval$approval_id, authorization=approval$authorization,
      session_ids=as.list(sids), previous_owner="legacy", new_owner=owner$key,
      authenticated=TRUE, viewer_id=ident$viewer$id, username=ident$viewer$username,
      counts=counts, historical_events="preserved original authorship"))
    db_meta_set(con, mark, owner$key)
    invisible(TRUE)
  })
}
