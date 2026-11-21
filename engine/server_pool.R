#!/usr/bin/env Rscript
# Cross-platform request concurrency for the local HTTP server.
#
# The master blocks in socketAccept(). Every inbound connection is one of:
#   PA-WORKER <token>              a worker registering its job socket
#   PA-DELIVERY <token> <job_id>   a worker returning a complete HTTP response
#   PA-POLLER <token>              the persistent poller checking its parent
#   GET/POST ...                   a normal HTTP request
#
# socketSelect() is deliberately absent from this loop. On Windows its timeout
# expiry has a hard ~200 ms floor, and even a ready socket can wait for that
# wake-up. Blocking accept returns as soon as a connection arrives, so worker
# deliveries and browser requests both stay in the millisecond range.

server_worker_target <- function() {
  raw <- Sys.getenv("PA_SERVER_WORKERS", unset = "")
  if (nzchar(raw)) {
    n <- suppressWarnings(as.integer(raw))
    if (length(n) == 1L && !is.na(n)) return(max(0L, min(n, 32L)))
  }
  opt <- getOption("pa.server.workers", NULL)
  if (!is.null(opt)) {
    n <- suppressWarnings(as.integer(opt))
    if (length(n) == 1L && !is.na(n)) return(max(0L, min(n, 32L)))
  }
  cores <- tryCatch(parallel::detectCores(logical = FALSE), error = function(e) 2L)
  if (length(cores) != 1L || is.na(cores) || cores < 2L) cores <- 2L
  max(2L, min(6L, as.integer(cores) - 1L))
}

server_token <- function() {
  if (requireNamespace("openssl", quietly = TRUE)) {
    return(paste(sprintf("%02x", as.integer(openssl::rand_bytes(24L))), collapse = ""))
  }
  paste(sprintf("%02x", sample.int(255L, 24L, replace = TRUE) - 1L), collapse = "")
}

server_env_secs <- function(name, default) {
  raw <- Sys.getenv(name, "")
  if (!nzchar(raw)) return(default)
  value <- suppressWarnings(as.numeric(raw))
  if (length(value) != 1L || is.na(value) || value <= 0) default else value
}

server_trace <- function(where) {
  f <- Sys.getenv("PA_SERVE_TRACE_FILE", "")
  if (!nzchar(f)) return(invisible(NULL))
  cat(format(Sys.time(), "%H:%M:%OS6"), " ", where, "\n", file = f, append = TRUE)
  invisible(NULL)
}

server_worker_spawn <- function(port, token, app_dir, index, script, rscript) {
  log <- file.path(tempdir(), sprintf("pa-server-worker-%s-%d.log",
                                      gsub("[^0-9]", "", as.character(Sys.getpid())), index))
  args <- c("--vanilla", shQuote(script), "--port", as.character(port),
            "--app", shQuote(app_dir))
  # The token is inherited through the child environment. Passing it as an
  # argument would expose it in the process command line to every local user.
  previous <- Sys.getenv("PA_WORKER_TOKEN", unset = NA_character_)
  had_previous <- !is.na(previous)
  Sys.setenv(PA_WORKER_TOKEN = token)
  on.exit({
    if (had_previous) Sys.setenv(PA_WORKER_TOKEN = previous)
    else Sys.unsetenv("PA_WORKER_TOKEN")
  }, add = TRUE)
  ok <- tryCatch({
    system2(rscript, args, wait = FALSE, stdout = log, stderr = log,
            minimized = IS_WIN, invisible = TRUE)
    TRUE
  }, error = function(e) FALSE)
  list(ok = ok, log = log)
}

server_pool_new <- function(app_dir, port) {
  target <- server_worker_target()
  if (target <= 0L) return(NULL)
  script <- file.path(app_dir, "engine", "server_worker.R")
  rscript <- file.path(R.home("bin"), if (IS_WIN) "Rscript.exe" else "Rscript")
  if (!file.exists(script) || !file.exists(rscript)) {
    say("[warn] 请求并发 worker 不可用，API 请求将返回 503")
    return(NULL)
  }

  pool <- new.env(parent = emptyenv())
  pool$target <- as.integer(target)
  pool$port <- as.integer(port)
  pool$token <- server_token()
  pool$conns <- list()
  pool$busy <- list()
  pool$logs <- character(0)
  for (i in seq_len(target)) {
    spawned <- server_worker_spawn(port, pool$token, app_dir, i, script, rscript)
    if (!isTRUE(spawned$ok)) {
      say("[warn] 第 ", i, " 个请求 worker 启动失败")
      break
    }
    pool$logs <- c(pool$logs, spawned$log)
  }
  say("[init] request workers: target=", pool$target,
      " (connecting in background)")
  pool
}

server_pool_register <- function(pool, con) {
  if (is.null(pool)) return(FALSE)
  try(socketTimeout(con, 86400), silent = TRUE)
  pool$conns[[length(pool$conns) + 1L]] <- con
  pool$busy[[length(pool$busy) + 1L]] <- NA_integer_
  TRUE
}

server_pool_idle <- function(pool) {
  if (is.null(pool) || !length(pool$busy)) return(integer(0))
  which(vapply(pool$busy, function(x) length(x) == 1L && is.na(x), logical(1)))
}

server_pool_dispatch <- function(pool, job_id, req) {
  repeat {
    idle <- server_pool_idle(pool)
    if (!length(idle)) return(0L)
    i <- idle[[1L]]
    ok <- tryCatch({
      serialize(list(kind = "request", token = pool$token,
                     job_id = as.integer(job_id), req = req),
                pool$conns[[i]])
      flush(pool$conns[[i]])
      TRUE
    }, error = function(e) FALSE)
    if (ok) {
      pool$busy[[i]] <- as.integer(job_id)
      return(i)
    }
    # A worker may have died while idle. Drop it and try the next one.
    server_pool_drop(pool, i)
  }
}

server_pool_mark_idle <- function(pool, worker_con) {
  if (is.null(pool) || !length(pool$conns)) return(invisible(NULL))
  i <- which(vapply(pool$conns, function(x) identical(x, worker_con), logical(1)))
  if (length(i)) pool$busy[[i[[1L]]]] <- NA_integer_
  invisible(NULL)
}

server_pool_drop <- function(pool, i) {
  if (is.null(pool) || i < 1L || i > length(pool$conns)) return(invisible(NULL))
  try(close(pool$conns[[i]]), silent = TRUE)
  pool$conns[[i]] <- NULL
  pool$busy[[i]] <- NULL
  invisible(NULL)
}

server_pool_close <- function(pool) {
  if (is.null(pool)) return(invisible(NULL))
  for (i in seq_along(pool$conns)) {
    try(serialize(list(kind = "shutdown", token = pool$token), pool$conns[[i]]),
        silent = TRUE)
    try(flush(pool$conns[[i]]), silent = TRUE)
    try(close(pool$conns[[i]]), silent = TRUE)
  }
  invisible(NULL)
}

server_poller_spawn <- function(app_dir, port, token, interval = 30L) {
  if (identical(Sys.getenv("PA_SERVE_POLLER", "1"), "0")) return(NULL)
  if (!isTRUE(get0("DB_READY", ifnotfound = FALSE))) return(NULL)
  script <- file.path(app_dir, "engine", "poller.R")
  rscript <- file.path(R.home("bin"), if (IS_WIN) "Rscript.exe" else "Rscript")
  if (!file.exists(script) || !file.exists(rscript)) return(NULL)
  interval <- suppressWarnings(as.integer(interval))
  if (length(interval) != 1L || is.na(interval) || interval <= 0L) interval <- 30L
  log <- file.path(tempdir(), sprintf("pa-server-poller-%s.log",
                                      gsub("[^0-9]", "", as.character(Sys.getpid()))))
  args <- c("--vanilla", shQuote(script), "--app", shQuote(app_dir),
            "--interval", as.character(as.integer(interval)),
            "--health-port", as.character(as.integer(port)),
            "--health-interval", "1")
  previous <- Sys.getenv("PA_POLLER_TOKEN", unset = NA_character_)
  had_previous <- !is.na(previous)
  Sys.setenv(PA_POLLER_TOKEN = token)
  on.exit({
    if (had_previous) Sys.setenv(PA_POLLER_TOKEN = previous)
    else Sys.unsetenv("PA_POLLER_TOKEN")
  }, add = TRUE)
  ok <- tryCatch({
    system2(rscript, args, wait = FALSE, stdout = log, stderr = log,
            minimized = IS_WIN, invisible = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) say("[warn] 持久调度器启动失败；/api/poller/status 会显示未运行")
  if (ok) say("[init] persistent poller started (interval=", interval, "s)")
  list(ok = ok, log = log)
}

server_write_response <- function(con, bytes) {
  server_trace("write-before")
  ok <- tryCatch({
    if (length(bytes)) writeBin(bytes, con)
    flush(con)
    TRUE
  }, error = function(e) FALSE)
  try(close(con), silent = TRUE)
  server_trace("write-after")
  ok
}

server_master_handle <- function(con, req) {
  server_trace("handler-before")
  ok <- tryCatch({
    serve_request(con, req)
    TRUE
  }, error = function(e) {
    say("[err] ", conditionMessage(e), " <- ",
        paste(utils::head(deparse(conditionCall(e)), 3), collapse = " "))
    try(send_json(con, 500L, list(ok = FALSE, error = enc2utf8(conditionMessage(e)))),
        silent = TRUE)
    FALSE
  })
  try(close(con), silent = TRUE)
  server_trace("handler-after")
  ok
}

server_read_http <- function(con, first) {
  if (!length(first) || !nzchar(first)) return(NULL)
  header_lines <- character(0)
  total <- nchar(first, type = "bytes")
  repeat {
    line <- tryCatch(readLines(con, n = 1L, warn = FALSE), error = function(e) character(0))
    if (!length(line)) break
    if (!nzchar(line[[1]])) break
    total <- total + nchar(line[[1]], type = "bytes")
    if (total > 131072L) break
    header_lines <- c(header_lines, line[[1]])
    if (length(header_lines) > 256L) break
  }
  head <- http_parse_head(paste(c(first, header_lines), collapse = "\r\n"))
  if (is.null(head)) return(NULL)
  clen <- http_content_length(head$headers)
  body_raw <- raw(0)
  while (length(body_raw) < clen) {
    chunk <- tryCatch(readBin(con, "raw", clen - length(body_raw)),
                      error = function(e) raw(0))
    if (!length(chunk)) break
    body_raw <- c(body_raw, chunk)
  }
  list(method = head$method, target = head$target, headers = head$headers,
       body = http_decode_text(body_raw))
}

server_serve_loop <- function(srv, app_dir, port) {
  pool <- server_pool_new(app_dir, port)
  SERVER_WORKER_POOL <<- pool
  on.exit(server_pool_close(pool), add = TRUE)
  control_token <- if (is.null(pool)) server_token() else pool$token
  poll_interval <- as.numeric(get0("POLL_INTERVAL_SECS", ifnotfound = 30L))
  poller <- server_poller_spawn(app_dir, port, control_token, poll_interval)
  say("[init] request loop ready")

  pending <- list()
  jobs <- list()
  next_job <- 0L
  queue_timeout <- server_env_secs("PA_SERVER_QUEUE_MS", 10000) / 1000
  job_timeout <- server_env_secs("PA_SERVER_JOB_MS", 120000) / 1000

  finish_job <- function(job_id, bytes) {
    key <- as.character(job_id)
    job <- jobs[[key]]
    jobs[[key]] <<- NULL
    if (is.null(job)) return(invisible(NULL))
    if (!length(bytes)) {
      try(send_json(job$con, 503L, list(ok = FALSE, code = "worker_empty_response")),
          silent = TRUE)
      try(close(job$con), silent = TRUE)
    } else {
      server_write_response(job$con, bytes)
    }
    invisible(NULL)
  }

  dispatch_pending <- function() {
    while (length(pending)) {
      if (!length(server_pool_idle(pool))) break
      item <- pending[[1L]]
      pending <<- pending[-1L]
      next_job <<- next_job + 1L
      wi <- server_pool_dispatch(pool, next_job, item$req)
      if (wi <= 0L) {
        try(send_json(item$con, 503L, list(ok = FALSE, code = "worker_unavailable")),
            silent = TRUE)
        try(close(item$con), silent = TRUE)
        next
      }
      jobs[[as.character(next_job)]] <<- list(
        con = item$con, at = item$at, worker_con = pool$conns[[wi]])
    }
  }

  reap <- function() {
    now <- as.numeric(Sys.time())
    if (length(pending)) {
      stale <- which(vapply(pending, function(x) now - x$at > queue_timeout,
                            logical(1)))
      for (i in rev(stale)) {
        try(send_json(pending[[i]]$con, 503L,
                      list(ok = FALSE, code = "worker_queue_timeout")), silent = TRUE)
        try(close(pending[[i]]$con), silent = TRUE)
        pending[[i]] <<- NULL
      }
    }
    if (length(jobs)) {
      stale <- names(jobs)[vapply(jobs, function(x) now - x$at > job_timeout,
                                  logical(1))]
      for (key in stale) {
        job <- jobs[[key]]
        jobs[[key]] <<- NULL
        server_pool_mark_idle(pool, job$worker_con)
        try(send_json(job$con, 503L,
                      list(ok = FALSE, code = "worker_job_timeout")), silent = TRUE)
        try(close(job$con), silent = TRUE)
      }
    }
  }

  repeat {
    dispatch_pending()
    reap()
    con <- tryCatch(socketAccept(srv, blocking = TRUE, open = "r+b",
                                 options = "no-delay"),
                    error = function(e) NULL)
    if (is.null(con)) {
      Sys.sleep(0.05)
      next
    }
    try(socketTimeout(con, 30), silent = TRUE)
    server_trace("accepted")

    first <- tryCatch(readLines(con, n = 1L, warn = FALSE),
                      error = function(e) character(0))
    if (!length(first)) {
      try(close(con), silent = TRUE)
      next
    }
    first <- first[[1L]]

    if (startsWith(first, "PA-")) {
      parts <- strsplit(first, " ", fixed = TRUE)[[1L]]
      kind <- parts[[1L]]
      token <- if (length(parts) >= 2L) parts[[2L]] else ""

      if (identical(kind, "PA-WORKER")) {
        if (!is.null(pool) && identical(token, pool$token) &&
            server_pool_register(pool, con)) {
          try(writeBin(charToRaw("PA-WORKER-OK\r\n"), con), silent = TRUE)
          try(flush(con), silent = TRUE)
          say("[init] request worker connected")
          dispatch_pending()
        } else {
          try(writeBin(charToRaw("PA-WORKER-DENIED\r\n"), con), silent = TRUE)
          try(flush(con), silent = TRUE)
          try(close(con), silent = TRUE)
        }
        next
      }

      if (identical(kind, "PA-DELIVERY")) {
        job_id <- if (length(parts) >= 3L) suppressWarnings(as.integer(parts[[3L]])) else NA_integer_
        valid <- !is.null(pool) && identical(token, pool$token) &&
          length(job_id) == 1L && !is.na(job_id)
        job_key <- if (valid) as.character(job_id) else ""
        job <- if (valid) jobs[[job_key]] else NULL
        if (is.null(job)) {
          try(close(con), silent = TRUE)
          next
        }
        result <- tryCatch(unserialize(con), error = function(e) NULL)
        try(close(con), silent = TRUE)
        if (!is.null(result) && is.list(result) &&
            identical(result$kind, "result") &&
            identical(as.integer(result$job_id), job_id)) {
          if (!is.null(job)) server_pool_mark_idle(pool, job$worker_con)
          finish_job(job_id, result$bytes)
          dispatch_pending()
        } else {
          server_pool_mark_idle(pool, job$worker_con)
          finish_job(job_id, NULL)
          dispatch_pending()
        }
        next
      }

      if (identical(kind, "PA-POLLER")) {
        ok <- identical(token, control_token)
        try(writeBin(charToRaw(if (ok) "PA-POLLER-OK\r\n" else
          "PA-POLLER-DENIED\r\n"), con), silent = TRUE)
        try(flush(con), silent = TRUE)
        try(close(con), silent = TRUE)
        if (ok) {
          reap()
          dispatch_pending()
        }
        next
      }

      try(close(con), silent = TRUE)
      next
    }

    req <- server_read_http(con, first)
    server_trace("parsed")
    if (is.null(req)) {
      try(send_json(con, 400L, list(ok = FALSE, code = "bad_request")), silent = TRUE)
      try(close(con), silent = TRUE)
      next
    }
    path <- parse_query(req$target)$path
    test_path <- identical(path, "/__test_slow")
    # Ping is a strict master fast path. Static files use an idle worker when
    # one exists (parallel first paint), but fall back to the master when every
    # worker is busy, so a slow API call can never starve the page shell. The
    # deterministic test seam is always worker work: its sleep must exercise the
    # concurrency path, not block accept.
    static_path <- identical(req$method, "GET") && !test_path &&
      !startsWith(path, "/api/")
    master_path <- identical(path, "/api/ping") ||
      (static_path && !length(server_pool_idle(pool)))
    if (master_path) {
      server_master_handle(con, req)
    } else {
      pending[[length(pending) + 1L]] <- list(
        con = con, req = req, at = as.numeric(Sys.time()))
      dispatch_pending()
    }
  }
}
