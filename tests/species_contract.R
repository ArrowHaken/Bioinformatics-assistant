# Pure contracts only: parse selected definitions, never source the server or science scripts.
source('engine/flow_spec.R')
load_assignments <- function(path, names, env) {
  for (expr in parse(path)) {
    if (is.call(expr) && identical(expr[[1]], as.name('<-')) &&
        is.symbol(expr[[2]]) && as.character(expr[[2]]) %in% names) eval(expr, env)
  }
}
e <- new.env(parent = globalenv())
load_assignments('serve.R', c('SPECIES_ALIASES', 'species_canon', 'flow_record_server',
                            'flow_vars', 'remote_step_instruction', 'flow_subs_of'), e)
e$HAS_SESSIONS <- TRUE; e$HAS_REFERENCE <- FALSE; e$HAS_FLOW <- TRUE
e$APP_DIR <- '/fixture'; e$pjoin <- file.path
e$sess_find <- function(...) list(species = e$input_species, place = list(server = srv))
e$flow_profile <- function() 'server'
e$STEP_TASK <- list(enrich = list(title = '富集', ask = 'fixture'))
e$supplement_for_stage <- function(...) list()
srv <- list(work_root = '/tmp/work path', script_root = '/tmp/scripts path',
            runtime = list(rscript = '/opt/R path/Rscript'))
vars_for <- function(alias) {
  e$input_species <- alias
  V <- e$flow_vars(srv, sid = 'fixture-session')
  V$reference_xlsx <- '/tmp/ref.json'
  V
}
# Real downstream map and normalization, with no libraries loaded or analyses executed.
p <- new.env(parent = baseenv())
load_assignments('engine/sc-plot-kit/scripts/plot_all_v2.R', c('SPECIES_MAP', '.norm_species'), p)
# Evaluate the actual doublet species branch against a recorder, not a Seurat object.
doublet_ast <- parse('engine/library/archive/R1-official/script/soupX_doublefinder.R')
find_species_branch <- function(expr) {
  if (!is.call(expr)) return(NULL)
  if (identical(expr[[1]], as.name('if')) &&
      identical(expr[[2]], quote(species == 'Human'))) return(expr)
  for (child in as.list(expr)[-1L]) {
    hit <- find_species_branch(child)
    if (!is.null(hit)) return(hit)
  }
  NULL
}
branches <- Filter(Negate(is.null), lapply(doublet_ast, find_species_branch))
stopifnot(length(branches) > 0L)
expected_db <- c(Mouse = 'org.Mm.eg.db', Human = 'org.Hs.eg.db', Rat = 'org.Rn.eg.db')
expected_mt <- c(Mouse = '^mt-', Human = '^MT-', Rat = '^Mt-')
for (key in names(e$SPECIES_ALIASES)) {
  canonical <- paste0(toupper(substr(key, 1, 1)), substring(key, 2))
  for (alias in c(e$SPECIES_ALIASES[[key]], canonical, paste0(' ', toupper(key), ' '))) {
    V <- vars_for(alias)
    stopifnot(identical(V$species, canonical))
    for (id in c('enrich', 'figs', 'doublet')) {
      x <- flow_execution(flow_stage(id), V)
      stopifnot(x$ok, identical(x$argv[match('--species', x$argv) + 1L], canonical))
    }
    x <- flow_execution(flow_stage('enrich'), V)
    stopifnot(identical(p$SPECIES_MAP[[p$.norm_species(tail(x$argv, 1L))]]$db,
                        unname(expected_db[canonical])))
    d <- new.env(parent = baseenv()); d$species <- canonical; d$sample_seu <- list()
    d$PercentageFeatureSet <- function(object, pattern) pattern
    eval(branches[[1]], d)
    stopifnot(identical(d$sample_seu$percent.mt, unname(expected_mt[canonical])))
    text <- e$remote_step_instruction('enrich', list(server = srv), 'fixture-session', 'fixture-request')
    stopifnot(grepl(x$command, text, fixed = TRUE),
              grepl(paste0("'--species' '", canonical, "'"), text, fixed = TRUE))
  }
}
# Annotation selects a reference/dictionary, not a species fallback or implicit species flag.
V <- vars_for('rat')
a <- flow_execution(flow_stage('annotate'), V)
stopifnot(a$ok, '--lit-dict' %in% a$argv, !('--reference' %in% a$argv), !('--species' %in% a$argv))
annot <- new.env(parent = baseenv())
annot$get_opt <- function(flag) if (flag == '--lit-dict') '/tmp/ref.json' else NULL
load_assignments('engine/sc-pipeline/scripts/02_annotate.R', c('reference', 'lit_dict'), annot)
stopifnot(is.null(annot$reference), identical(annot$lit_dict, '/tmp/ref.json'))
# Direct resolver callers must provide a single canonical string. No alias or first-value fallback.
bad <- list('human', 'rat', '小鼠', ' Human ', 'mouce', 'mouse/path', NA_character_,
            NULL, character(), c('Human', 'Rat'), c('Human', NA_character_),
            list('Human'), factor('Human'), matrix('Human'), TRUE, 1,
            structure('Human', class = 'fixture_species'))
# Empty and whitespace values intentionally request auto-detection.
for (value in c('', ' ')) {
  V <- vars_for(value)
  x <- flow_execution(flow_stage('enrich'), V)
  stopifnot(x$ok, identical(x$script, '-e'), '--args' %in% x$argv,
            identical(x$argv[match('--target', x$argv) + 1L],
                      file.path('/tmp/work path', '_engine/sc-plot-kit/scripts/plot_all_v2.R')))
}
for (value in bad) {
  V$species <- value
  for (id in c('enrich', 'figs', 'doublet')) {
    x <- tryCatch(flow_execution(flow_stage(id), V), error = identity)
    stopifnot(!inherits(x, 'error'), identical(x$ok, FALSE), 'species' %in% x$missing,
              is.null(x$argv), is.null(x$command), is.null(flow_cmd(id, V)))
  }
}
for (unknown in c('mouce', 'mouse/path')) {
  V <- vars_for(unknown)
  stopifnot(!flow_execution(flow_stage('enrich'), V)$ok)
  text <- e$remote_step_instruction('enrich', list(server = srv), 'fixture-session', 'fixture-request')
  stopifnot(grepl('当前不可提交', text, fixed = TRUE),
            !grepl("'--species'", text, fixed = TRUE))
}
cat('PASS: production aliases -> canonical argv/instructions -> real enrichment/doublet mappings; annotation dictionary; direct bad species fail closed\n')
