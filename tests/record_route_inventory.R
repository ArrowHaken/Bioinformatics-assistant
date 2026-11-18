# Run from app/: Rscript tests/record_route_inventory.R
# Static route inventory only: no server, database, queue, mirror or filesystem mutation.
run_contract <- function() {
  `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
  source('engine/record_access.R', local = TRUE)

  strings <- function(files) {
    txt <- paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = '\n')
    hits <- regmatches(txt, gregexpr('"(/api/[A-Za-z0-9_./-]+)"', txt, perl = TRUE))[[1L]]
    if (!length(hits)) return(character())
    substr(hits, 2L, nchar(hits) - 1L)
  }

  routes <- unique(strings(c('serve.R', 'engine/db_api.R')))
  routes <- sort(unique(sub('[?#].*$', '', routes)))
  fail <- character()
  check <- function(label, value) {
    cat(if (isTRUE(value)) 'PASS:' else 'FAIL:', label, '\n')
    if (!isTRUE(value)) fail <<- c(fail, label)
  }
  check('route inventory is non-trivial', length(routes) >= 120L)

  scopes <- character(0)
  unclassified <- character(0)
  for (route in routes) {
    for (method in c('GET', 'POST')) {
      policy <- record_route_policy(route, method)
      scopes <- c(scopes, as.character(policy$scope))
      if (identical(policy$scope, 'unclassified')) unclassified <- c(unclassified, route)
    }
  }
  counts <- table(scopes)
  cat('ROUTES:', length(routes), paste(names(counts), as.integer(counts), sep = '=', collapse = ' '), '\n')
  check('every /api route literal has a declared policy', !length(unclassified))
  check('all policy scopes are known',
        all(names(counts) %in% c('record', 'version', 'task_bound', 'worker', 'system')))
  check('record, version, task-bound, worker and system scopes are all represented',
        all(c('record', 'version', 'task_bound', 'worker', 'system') %in% names(counts)))
  check('a new route cannot silently default to classified',
        identical(record_route_policy('/api/not_in_inventory', 'GET')$scope, 'unclassified'))

  if (length(fail)) stop(paste(length(fail), 'route inventory failures'), call. = FALSE)
  cat('PASS: record route inventory\n')
}
run_contract()
