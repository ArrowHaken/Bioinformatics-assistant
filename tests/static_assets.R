#!/usr/bin/env Rscript
# Static delivery regression tests.
# Run: Rscript tests/static_assets.R

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))
APP <- normalizePath(getwd())
if (!dir.exists(file.path(APP, "engine"))) APP <- normalizePath(file.path(APP, ".."))
STATUS_TEXT <- c(`200` = "OK", `304` = "Not Modified")
source(file.path(APP, "engine", "http_static.R"))

PASS <- 0L; FAIL <- 0L
ok <- function(name, cond, detail = "") {
  if (isTRUE(cond)) { PASS <<- PASS + 1L; cat(sprintf("  [ok]   %s\n", name)) }
  else { FAIL <<- FAIL + 1L; cat(sprintf("  [FAIL] %s  %s\n", name, detail)) }
}

ok("js is compressible", http_static_compressible("text/javascript; charset=utf-8"))
ok("css is compressible", http_static_compressible("text/css; charset=utf-8"))
ok("html is compressible", http_static_compressible("text/html; charset=utf-8"))
ok("png is not compressed", !http_static_compressible("image/png"))
ok("versioned asset is immutable",
   grepl("immutable", http_static_cache_policy("app-shell.20261028a.js"), fixed = TRUE))
ok("index is no-cache", identical(http_static_cache_policy("index.html"), "no-cache"))
ok("ordinary asset is short cache", grepl("max-age=300", http_static_cache_policy("app.js"), fixed = TRUE))

TMP <- tempfile("static-test-"); dir.create(TMP)
f <- file.path(TMP, "app.js")
writeBin(charToRaw(paste(rep("console.log('x');", 100), collapse = "\n")), f)
i1 <- file.info(f)
e1 <- http_static_etag(f, i1)
h200 <- http_static_headers(200L, "text/javascript; charset=utf-8", 1400L, e1,
                            "public, max-age=300", "gzip")
ok("200 header has ETag", grepl("ETag: ", h200, fixed = TRUE))
ok("200 header has Vary", grepl("Vary: Accept-Encoding", h200, fixed = TRUE))
ok("200 header has gzip", grepl("Content-Encoding: gzip", h200, fixed = TRUE))
ok("200 header has exact length", grepl("Content-Length: 1400", h200, fixed = TRUE))
h304 <- http_static_headers(304L, "text/javascript; charset=utf-8", 0L, e1,
                            "public, max-age=300")
ok("304 has no body length", !grepl("Content-Length:", h304, fixed = TRUE))
ok("304 says Not Modified", grepl("304 Not Modified", h304, fixed = TRUE))

Sys.setFileTime(f, Sys.time() + 2)
i2 <- file.info(f)
ok("ETag changes with file metadata", !identical(e1, http_static_etag(f, i2)))
gz <- memCompress(readBin(f, "raw", file.info(f)$size), type = "gzip")
ok("gzip actually shrinks the JS fixture", length(gz) < file.info(f)$size)

unlink(TMP, recursive = TRUE, force = TRUE)
cat(sprintf("\nPASS=%d FAIL=%d\n", PASS, FAIL))
quit(save = "no", status = if (FAIL == 0L) 0L else 1L)
