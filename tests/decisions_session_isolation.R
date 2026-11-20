# Run from app/: Rscript tests/decisions_session_isolation.R
# Definitions only; all reads/writes below use a fresh temporary state tree.
local({
  e <- new.env(parent = globalenv())
  for (x in parse('engine/server_main.R')) {
    if (is.call(x) && identical(x[[1]], as.name('<-')) &&
        is.call(x[[3]]) && identical(x[[3]][[1]], as.name('function'))) eval(x, e)
  }
  source('engine/sessions.R', local = e)
  root <- tempfile('decisions-session-'); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  e$APP_DIR <- root; e$STATE_DIR <- file.path(root, 'state')
  dir.create(file.path(e$STATE_DIR, 'remote'), recursive = TRUE)
  e$REMOTE_STEP_FILE <- file.path(e$STATE_DIR, 'remote_steps.json')
  e$read_text_any <- function(p, ...) paste(readLines(p, warn = FALSE), collapse = '\n')
  e$write_json_file <- e$sess_write_json
  e$cur_sid <- function() 'A' # Intentionally different from the decision's explicit B.
  source('engine/decisions.R', local = e)
  shared <- function(name, obj) e$write_json_file(file.path(e$STATE_DIR, 'remote', name), obj)
  ledger <- function(steps) e$write_json_file(e$REMOTE_STEP_FILE, list(steps = steps))
  clusters <- function(id) list(clusters = list(list(cluster = id, n = 1, mp_pct = 50)))
  after <- function(id) list(samples = list(list(sample = id, group = 'Mock', n = 1)), cells_total = 1)
  step <- function(sid, id, evidence = NULL, artifacts = NULL) list(
    session_id = sid, id = id, status = 'done', evidence = evidence, artifacts = artifacts)
  failures <- character()
  check <- function(label, value) {
    cat(if (isTRUE(value)) 'PASS:' else 'FAIL:', label, '\n')
    if (!isTRUE(value)) failures <<- c(failures, label)
  }
  text <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = 'null'))
  has <- function(x, marker) grepl(marker, text(x), fixed = TRUE)
  shared('cluster1_evidence.json', clusters('A_ONLY'))
  shared('after_drop_stats.json', after('A_SAMPLE'))
  ledger(list(step('A', 'doublet', artifacts = list(list(rel = '1.doublet/A_ONLY.rds'))),
              step(NULL, 'doublet', artifacts = list(list(rel = '1.doublet/UNOWNED.rds')))))
  b <- e$dec_specs('filter', 'B')
  cat('REPRO B filter specs:', text(b), '\n')
  check('B filter excludes A_ONLY from unowned shared evidence', !has(b, 'A_ONLY'))
  check('B final excludes A_SAMPLE from unowned shared evidence', !has(e$dec_specs('final', 'B'), 'A_SAMPLE'))
  check('B doublet never follows currently selected A', !has(e$dec_specs('doublet', 'B'), 'A_ONLY'))
  check('legacy filter still reads unowned shared evidence', has(e$dec_specs('filter'), 'A_ONLY'))
  check('legacy final still reads unowned shared evidence', has(e$dec_specs('final'), 'A_SAMPLE'))
  check('legacy doublet still reads whole old ledger', has(e$dec_specs('doublet'), 'UNOWNED'))

  shared('cluster1_evidence.json', c(list(session_id = 'A'), clusters('A_OWNED')))
  shared('after_drop_stats.json', c(list(session_id = 'A'), after('A_OWNED_SAMPLE')))
  check('shared evidence explicitly owned by A is rejected for B', !has(e$dec_specs('filter', 'B'), 'A_OWNED'))
  check('shared after-drop explicitly owned by A is rejected for B', !has(e$dec_specs('final', 'B'), 'A_OWNED_SAMPLE'))
  shared('cluster1_evidence.json', c(list(session_id = 'B'), clusters('B_SHARED')))
  shared('after_drop_stats.json', c(list(session_id = 'B'), after('B_SHARED_SAMPLE')))
  check('explicit B shared cluster evidence is accepted', has(e$dec_specs('filter', 'B'), 'B_SHARED'))
  check('explicit B shared after-drop evidence is accepted', has(e$dec_specs('final', 'B'), 'B_SHARED_SAMPLE'))

  ledger(list(step('A', 'markers', list(cluster1_evidence = clusters('A_INLINE'))),
              step('B', 'markers', list(cluster1_evidence = clusters('B_INLINE'))),
              step('B', 'filter', list(after_drop_stats = after('B_INLINE_SAMPLE'))),
              step('A', 'doublet', artifacts = list(list(rel = '1.doublet/A_ONLY.rds'))),
              step('B', 'doublet', artifacts = list(list(rel = '1.doublet/B_ONLY.rds')))))
  check('B record callback clusters take precedence over shared file',
        has(e$dec_specs('filter', 'B'), 'B_INLINE') && !has(e$dec_specs('filter', 'B'), 'B_SHARED'))
  check('B record callback samples take precedence over shared file',
        has(e$dec_specs('final', 'B'), 'B_INLINE_SAMPLE') && !has(e$dec_specs('final', 'B'), 'B_SHARED_SAMPLE'))
  check('B sample enumeration uses only explicit owner B',
        has(e$dec_specs('doublet', 'B'), 'B_ONLY') && !has(e$dec_specs('doublet', 'B'), 'A_ONLY'))
  check('A record callback remains accessible without switching current', has(e$dec_specs('filter', 'A'), 'A_INLINE'))

  # Even a buggy upstream reader returning all rows must not bypass the evidence boundary.
  original_reader <- e$remote_steps_read
  e$remote_steps_read <- function(sid = NULL) original_reader('')
  ledger(list(step('A', 'markers', list(cluster1_evidence = clusters('A_INLINE'))),
              step(NULL, 'markers', list(cluster1_evidence = clusters('UNOWNED_INLINE'))),
              step(NULL, 'doublet', artifacts = list(list(rel = '1.doublet/UNOWNED.rds')))))
  shared('cluster1_evidence.json', clusters('UNOWNED_SHARED'))
  check('evidence reader revalidates explicit callback session_id',
        !has(e$dec_specs('filter', 'B'), 'A_INLINE') && !has(e$dec_specs('filter', 'B'), 'UNOWNED'))
  check('sample reader revalidates explicit callback session_id', !has(e$dec_specs('doublet', 'B'), 'UNOWNED'))
  ledger(list(step('B', 'markers', list(cluster1_evidence = c(list(session_id = 'A'), clusters('CONFLICT'))))))
  check('conflicting nested evidence owner is rejected', !has(e$dec_specs('filter', 'B'), 'CONFLICT'))
  e$remote_steps_read <- original_reader

  # Pending decisions may already contain leaked options. Repair in-place, never duplicate.
  ledger(list(step('B', 'markers', list(cluster1_evidence = clusters('B_REPAIRED')))))
  stale <- list(id = 'existing-pending', key = 'cluster_filter', step = 'filter',
                status = 'asked', asked_at = 'original-time', options = list(list(value = 'A_ONLY')),
                suggested = 'A_ONLY', evidence = clusters('A_ONLY'))
  e$dec_write('B', list(session_id = 'B', items = list(stale)))
  made <- e$dec_ensure('filter', 'B'); repaired <- e$dec_read('B')$items[[1]]
  check('ensure refreshes contaminated pending options and evidence', !has(repaired, 'A_ONLY') && has(repaired, 'B_REPAIRED'))
  check('pending refresh preserves identity and asked time', identical(repaired$id, stale$id) && identical(repaired$asked_at, stale$asked_at))
  check('pending refresh does not send duplicate new decision', length(made) == 0L && length(e$dec_read('B')$items) == 1L)
  e$dec_ensure('filter', 'B')
  check('pending refresh is idempotent', identical(repaired, e$dec_read('B')$items[[1]]))
  stale$status <- 'answered'; stale$answer <- 'A_ONLY'; stale$answered_at <- 'original-answer-time'
  e$dec_write('B', list(session_id = 'B', items = list(stale)))
  historical <- e$dec_read('B')$items[[1]]
  e$dec_ensure('filter', 'B')
  check('ensure never silently overwrites historical human answers', identical(e$dec_read('B')$items[[1]], historical))
  check('unverified scoped historical answer is not applied downstream', is.null(e$dec_answer_of('B', 'cluster_filter')))
  e$dec_write('B', list(session_id = 'B', items = list()))
  fresh <- e$dec_ensure('filter', 'B')[[1]]
  check('new decision carries explicit evidence session ownership', identical(fresh$evidence_session_id, 'B'))
  e$dec_answer('B', fresh$id, 'keep_all')
  check('fresh scoped human answer remains usable', identical(e$dec_answer_of('B', 'cluster_filter')$value, 'keep_all'))
  e$dec_write('', list(session_id = '', items = list(stale)))
  check('legacy historical answers remain compatible', identical(e$dec_answer_of('', 'cluster_filter')$value, 'A_ONLY'))
  obsolete <- list(id = 'obsolete', key = 'dropsample', step = 'final', status = 'asked',
                   options = list(list(value = 'A_ONLY')), suggested = 'A_ONLY')
  e$dec_write('C', list(session_id = 'C', items = list(obsolete)))
  e$dec_ensure('final', 'C')
  check('ensure removes obsolete pending sample question when no owned evidence exists',
        !has(e$dec_read('C'), 'A_ONLY') && !any(vapply(e$dec_read('C')$items,
          function(x) identical(x$key, 'dropsample'), logical(1))))
  if (length(failures)) stop(paste(length(failures), 'decision session isolation failures'), call. = FALSE)
  cat('PASS: decision session isolation contract\n')
})
