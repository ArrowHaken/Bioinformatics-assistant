# Startup probes live outside serve.R so the port-conflict decision is explicit.
# The production unit restarts on non-zero exit; a second healthy instance must
# therefore exit 0 instead of creating a restart storm.

startup_probe_existing <- function(port, host = "127.0.0.1", timeout = 2) {
  con <- NULL
  ok <- tryCatch({
    # Binary mode is required by writeBin(); a text connection ("r+") throws
    # "can only write to a binary connection", which used to turn every healthy
    # instance into a false negative and leave the restart storm in place.
    con <- socketConnection(host, as.integer(port), open = "r+b",
                            blocking = TRUE, timeout = timeout)
    writeBin(charToRaw(paste0(
      "GET /api/ping HTTP/1.1\r\nHost: ", host, ":", as.integer(port),
      "\r\nConnection: close\r\n\r\n")), con)
    flush(con)
    line <- readLines(con, n = 1L, warn = FALSE)
    length(line) == 1L && grepl("^HTTP/1\\.[01] 200", line[[1]])
  }, error = function(e) FALSE)
  if (!is.null(con)) try(close(con), silent = TRUE)
  isTRUE(ok)
}
