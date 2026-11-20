# Static asset delivery for the local HTTP server.
# Kept out of serve.R so compression/caching/304 behaviour has one testable home.

if (!exists("%||%", mode = "function"))
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
if (!exists("STATUS_TEXT")) STATUS_TEXT <- c(`200` = "OK", `304` = "Not Modified")

http_static_etag <- function(path, info = NULL) {
  if (is.null(info)) info <- file.info(path)
  paste0('W/"', format(as.numeric(info$size), scientific = FALSE, trim = TRUE),
         "-", format(as.numeric(info$mtime), scientific = FALSE, digits = 15, trim = TRUE), '"')
}

http_static_compressible <- function(ctype) {
  ctype <- tolower(as.character(ctype %||% ""))
  grepl("^(text/|application/(javascript|json|xml|svg)|image/svg\\+)", ctype)
}

http_static_cache_policy <- function(rel) {
  base <- basename(as.character(rel %||% ""))
  if (!nzchar(base) || identical(tolower(base), "index.html"))
    return("no-cache")
  # app-shell.20261028a.js style names are immutable because the version is in
  # the URL. Other static files may be replaced in place, so keep them short.
  if (grepl("(^|[.-])[0-9]{8}[a-z]?[.]", base, ignore.case = TRUE))
    return("public, max-age=31536000, immutable")
  "public, max-age=300"
}

http_static_headers <- function(code, ctype, len, etag = "", cache = "no-cache",
                                encoding = "", extra = character(0)) {
  status <- if (identical(as.integer(code), 304L)) "Not Modified" else
    unname(STATUS_TEXT[as.character(code)])
  if (!length(status) || is.na(status)) status <- "OK"
  h <- c(paste0("HTTP/1.1 ", as.integer(code), " ", status),
         paste0("Content-Type: ", ctype),
         paste0("Cache-Control: ", cache),
         paste0("Vary: Accept-Encoding"),
         "Connection: close")
  if (nzchar(etag)) h <- c(h, paste0("ETag: ", etag))
  if (nzchar(encoding)) h <- c(h, paste0("Content-Encoding: ", encoding))
  if (!identical(as.integer(code), 304L)) h <- c(h, paste0("Content-Length: ", as.integer(len)))
  if (length(extra)) h <- c(h, extra)
  paste0(paste(h, collapse = "\r\n"), "\r\n\r\n")
}

http_send_static <- function(con, req, f, ctype) {
  info <- file.info(f)
  rel <- basename(f)
  etag <- http_static_etag(f, info)
  cache <- http_static_cache_policy(rel)
  inm <- as.character((req$headers %||% list())[["if-none-match"]] %||% "")
  if (nzchar(inm)) {
    tags <- trimws(strsplit(inm, ",", fixed = TRUE)[[1]])
    if (etag %in% tags || "*" %in% tags) {
      writeBin(charToRaw(http_static_headers(304L, ctype, 0L, etag, cache)), con)
      flush(con)
      return(invisible(NULL))
    }
  }

  accept <- tolower(as.character((req$headers %||% list())[["accept-encoding"]] %||% ""))
  use_gzip <- grepl("gzip", accept, fixed = TRUE) &&
    isTRUE(http_static_compressible(ctype)) && as.numeric(info$size) >= 1024

  if (use_gzip) {
    raw <- readBin(f, "raw", as.integer(info$size))
    gz <- memCompress(raw, type = "gzip")
    hdr <- http_static_headers(200L, ctype, length(gz), etag, cache, "gzip")
    writeBin(c(charToRaw(hdr), gz), con)
    flush(con)
    return(invisible(NULL))
  }

  hdr <- http_static_headers(200L, ctype, as.integer(info$size), etag, cache)
  writeBin(charToRaw(hdr), con)
  fc <- file(f, open = "rb")
  on.exit(try(close(fc), silent = TRUE), add = TRUE)
  repeat {
    chunk <- readBin(fc, "raw", 262144L)
    if (!length(chunk)) break
    writeBin(chunk, con)
  }
  flush(con)
  invisible(NULL)
}
