# Authenticated initialization updates one owned record; no selection or execution.
pah_apply_initialize <- function(app_dir, con, owner, envelope, out) {
  sid <- out$session_id
  if (is.null(out$intent)) return(list(status = 200L, body = list(ok = TRUE,
    session_id = sid, request_id = out$request_id, initialized = TRUE, supports_run = FALSE)))
  snapshots <- list(); touched <- character()
  capture <- function(path) {
    snapshots[[path]] <<- list(exists = file.exists(path), bytes = if (file.exists(path))
      readBin(path, 'raw', n = file.info(path)$size) else raw())
  }
  write <- function(path, obj) {
    capture(path); touched <<- c(touched, path); intent_json_write(path, obj)
  }
  tryCatch(db_tx(con, function() {
    row <- DBI::dbGetQuery(con, 'SELECT owner_key FROM sessions WHERE session_id=?', list(sid))
    if (nrow(row) != 1L || !identical(row$owner_key[[1]], owner$key))
      return(list(status = 403L, body = list(ok = FALSE, code = 'handoff_record_forbidden')))
    st <- intent_json_read(sess_file(app_dir), list(sessions = list()), 'sessions')
    hit <- which(vapply(st$sessions, function(s) identical(s$id, sid), logical(1)))
    if (length(hit) != 1L) return(list(status = 409L, body = list(ok = FALSE, code = 'handoff_record_missing')))
    s <- st$sessions[[hit]]
    profile_path <- sess_profile_f(app_dir, sid)
    prof <- intent_json_read(profile_path)
    receipt_path <- file.path(dirname(profile_path), 'handoff_receipts.json')
    receipts <- intent_json_read(receipt_path, list(items = list()), 'items')
    key <- digest::digest(paste(owner$key, envelope$topic_id, out$request_id, sep = '|'), algo = 'sha256', serialize = FALSE)
    signature <- digest::digest(jsonlite::toJSON(list(action = envelope$action,
      parameters = envelope$parameters), auto_unbox = TRUE, null = 'null'), algo = 'sha256', serialize = FALSE)
    prior <- receipts$items[[key]]
    if (!is.null(prior)) {
      if (!identical(prior$signature, signature)) return(list(status = 409L,
        body = list(ok = FALSE, code = 'handoff_request_conflict')))
      return(list(status = 200L, body = list(ok = TRUE, session_id = sid,
        request_id = out$request_id, initialized = TRUE, duplicate = TRUE, supports_run = FALSE)))
    }
    # Merge only with this record's state. Global server configuration is never read.
    base <- prof$intent %||% list(schema = 'pa.intent.v1', project_name = s$name,
      mode = s$mode, paths = list(work_root = s$run_dir))
    if (is.null(base$server)) {
      srv <- s$place$server %||% list()
      base$server <- srv[intersect(names(srv), c('label','host','port','user','exec_pc','samples','runtime'))]
      for (k in c('data_root','script_root','liny_data','rawdata_dir')) if (!is.null(srv[[k]]) && nzchar(as.character(srv[[k]]))) base$paths[[k]] <- srv[[k]]
    }
    merge <- function(a, b) {
      for (k in names(b)) a[[k]] <- if (is.list(a[[k]]) && is.list(b[[k]]) &&
        !is.null(names(b[[k]]))) merge(a[[k]], b[[k]]) else b[[k]]
      a
    }
    raw <- merge(base, out$intent)
    raw$source <- list(topic_id = envelope$topic_id, message_id = out$request_id)
    normalized <- intent_normalize(raw)
    if (!isTRUE(normalized$ok)) return(list(status = 400L,
      body = list(ok = FALSE, code = 'handoff_parameters_invalid', errors = normalized$errors)))
    it <- normalized$intent
    place <- merge(s$place %||% list(), intent_to_place(it))
    # Preserve record-only reference/credential fields; incoming intent cannot set them.
    s$place <- place; s$mode <- place$mode
    s$run_dir <- if (identical(s$mode, 'local')) place$local$path else place$server$work_root
    s$updated_at <- format(Sys.time(), '%Y-%m-%dT%H:%M:%S')
    # Imported facts remain unconfirmed; never manufacture a confirmation stamp.
    for (k in c('species','tissue')) if (!is.null(it[[k]])) s[[k]] <- it[[k]]
    if (any(c('species','tissue') %in% names(out$intent))) {
      s$species_tissue_source <- 'agent-intent-unconfirmed'
      s$species_tissue_confirmed_at <- NULL
      s$species_tissue_confirmed_by <- NULL
      prof$species_tissue_source <- 'agent-intent-unconfirmed'
      prof$species_tissue_confirmed_at <- NULL
      prof$species_tissue_confirmed_by <- NULL
    }
    st$sessions[[hit]] <- s
    patch <- intent_to_profile_patch(it); prof[names(patch)] <- patch
    n <- DBI::dbExecute(con, 'UPDATE sessions SET mode=?,run_dir=?,updated_at=? WHERE session_id=? AND owner_key=?',
      list(s$mode, s$run_dir, s$updated_at, sid, owner$key))
    if (n != 1L) stop('record update failed')
    receipts$items[[key]] <- list(signature = signature, session_id = sid, request_id = out$request_id)
    write(sess_file(app_dir), st); write(profile_path, prof); write(receipt_path, receipts)
    list(status = 200L, body = list(ok = TRUE, session_id = sid, request_id = out$request_id,
      initialized = TRUE, duplicate = FALSE, supports_run = FALSE))
  }), error = function(e) {
    restored <- TRUE
    for (path in rev(touched)) tryCatch({
      snap <- snapshots[[path]]
      if (snap$exists) intent_atomic_bytes(path, snap$bytes)
      else if (file.exists(path) && unlink(path) != 0L) stop('restore failed')
    }, error = function(e) restored <<- FALSE)
    list(status = 503L, body = list(ok = FALSE,
      code = if (restored) 'handoff_write_failed' else 'handoff_restore_failed'))
  })
}
