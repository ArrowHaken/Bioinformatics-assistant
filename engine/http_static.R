# Static asset delivery for the local HTTP server.
# Kept out of serve.R so compression/caching/304 behaviour has one testable home.

if (!exists("%||%", mode = "function"))
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
if (!exists("STATUS_TEXT")) STATUS_TEXT <- c(`200` = "OK", `304` = "Not Modified")

# Compression is deterministic for a given file identity. Cache the gzip body
# so concurrent page loads do not each compress the same 400 KB app.js.
HTTP_STATIC_CACHE <- new.env(parent = emptyenv())

http_static_gzip <- function(path, info, etag) {
  key <- paste0(normalizePath(path, winslash = "/", mustWork = TRUE), "|", etag)
  if (exists(key, envir = HTTP_STATIC_CACHE, inherits = FALSE))
    return(get(key, envir = HTTP_STATIC_CACHE, inherits = FALSE))
  raw <- readBin(path, "raw", as.integer(info$size))
  gz <- http_static_gzip_raw(raw)
  assign(key, gz, envir = HTTP_STATIC_CACHE)
  gz
}

# memCompress(type = "gzip") is a zlib stream (78 9c), NOT a gzip container
# (1f 8b). Sending zlib while declaring "Content-Encoding: gzip" makes every
# browser reject the body: all scripts and styles drop to net::ERR_FAILED and
# the page stays on its boot placeholder. gzfile is the base-R writer that
# emits a real gzip container, so the declared encoding matches the bytes.
http_static_gzip_raw <- function(raw) {
  tmp <- tempfile(fileext = ".gz")
  on.exit(unlink(tmp), add = TRUE)
  con <- gzfile(tmp, "wb")
  writeBin(raw, con)
  close(con)
  readBin(tmp, "raw", as.integer(file.info(tmp)$size))
}

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
    gz <- http_static_gzip(f, info, etag)
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
