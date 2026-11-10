# Remote-only species resolver. Source-safe; no input data or packages loaded until called.
# Deliberately do not infer from paths, reference spreadsheets, or symbol capitalization.
species_from_features <- function(features, source = 'features') {
  ids <- unique(sub('\\.[0-9]+$', '', as.character(features)))
  prefixes <- c(Human = '^ENSG[0-9]+$', Mouse = '^ENSMUSG[0-9]+$', Rat = '^ENSRNOG[0-9]+$')
  counts <- vapply(prefixes, function(p) sum(grepl(p, ids)), integer(1))
  unsupported <- grepl('^ENS[A-Z]*G[0-9]+$', ids) & !grepl('^(ENSG|ENSMUSG|ENSRNOG)[0-9]+$', ids)
  if (any(unsupported)) stop('发现不支持或混合物种的Ensembl基因标识：', source, call. = FALSE)
  present <- names(counts)[counts > 0L]
  if (length(present) != 1L || counts[present] < 3L)
    stop('无法可靠自动识别物种（证据不足或混合物种）：', source,
         '; Human=', counts['Human'], ', Mouse=', counts['Mouse'], ', Rat=', counts['Rat'],
         '。需要数据本身携带明确基因标识；不按目录名或基因符号大小写猜测。', call. = FALSE)
  list(species = present, evidence = paste0(source, ': ', present, ' Ensembl gene IDs=', counts[present]))
}

species_from_10x <- function(datadir, samples) {
  if (!nzchar(datadir) || !length(samples) || any(!nzchar(samples)))
    stop('自动识别缺少实际10x输入路径/样本', call. = FALSE)
  results <- lapply(samples, function(sample) {
    root <- file.path(datadir, sample, 'outs', 'filtered_feature_bc_matrix')
    files <- file.path(root, c('features.tsv.gz', 'features.tsv', 'genes.tsv.gz', 'genes.tsv'))
    files <- files[file.exists(files)]
    if (!length(files)) stop('自动识别找不到实际10x features：', root, call. = FALSE)
    path <- files[1L]
    con <- if (grepl('\\.gz$', path)) gzfile(path, 'rt') else file(path, 'rt')
    on.exit(close(con), add = TRUE)
    tab <- read.delim(con, header = FALSE, sep = '\t', quote = '', comment.char = '',
                      colClasses = 'character', check.names = FALSE)
    # Ignore antibody/guide IDs: species evidence must belong to RNA Gene Expression.
    if (ncol(tab) >= 3L) tab <- tab[tab[[3L]] == 'Gene Expression', , drop = FALSE]
    species_from_features(tab[[1L]], path)
  })
  found <- unique(vapply(results, `[[`, '', 'species'))
  if (length(found) != 1L) stop('样本间物种不一致，停止执行', call. = FALSE)
  list(species = found, evidence = paste(vapply(results, `[[`, '', 'evidence'), collapse = '; '))
}

species_from_seurat <- function(path) {
  if (!file.exists(path)) stop('自动识别找不到Seurat输入：', path, call. = FALSE)
  # Load class definitions for v4/v5 rownames/slots; fail explicitly if unavailable.
  if (!requireNamespace('SeuratObject', quietly = TRUE) && !requireNamespace('Seurat', quietly = TRUE))
    stop('自动识别Seurat输入需要SeuratObject或Seurat包', call. = FALSE)
  obj <- readRDS(path)
  if (!inherits(obj, 'Seurat') || !('assays' %in% methods::slotNames(obj)))
    stop('自动识别输入不是Seurat对象：', path, call. = FALSE)
  assays <- methods::slot(obj, 'assays')
  if (!('RNA' %in% names(assays))) stop('自动识别需要RNA assay，不使用integrated/SCT特征猜测', call. = FALSE)
  assay <- assays[['RNA']]
  features <- rownames(assay)
  # RNA feature metadata can retain original Ensembl IDs after symbol conversion.
  for (slot in intersect(c('meta.features', 'meta.data'), methods::slotNames(assay))) {
    meta <- methods::slot(assay, slot)
    if (nrow(meta) == length(rownames(assay)) &&
        (identical(rownames(meta), rownames(assay)) ||
         identical(rownames(meta), as.character(seq_len(nrow(meta)))))) {
      cols <- intersect(c('gene_id', 'ensembl_id', 'ensembl_gene_id', 'original_gene_id'), names(meta))
      features <- c(features, unlist(meta[cols], use.names = FALSE))
    }
  }
  species_from_features(features, paste0(path, ' [RNA]'))
}

species_exec_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) < 4L || !identical(args[1L], '--target') || !identical(args[3L], '--'))
    stop('用法：species_exec.R --target SCRIPT -- ORIGINAL_ARGS', call. = FALSE)
  target <- args[2L]; argv <- args[-(1:3)]
  raw_i <- which(argv == '--species-raw-input')
  raw_input <- raw_samples <- NULL
  if (length(raw_i)) {
    if (length(raw_i) != 1L || raw_i + 2L > length(argv)) stop('自动识别原始数据参数无效')
    raw_input <- argv[raw_i + 1L]; raw_samples <- argv[raw_i + 2L]
    argv <- argv[-(raw_i:(raw_i + 2L))]
  }
  if (!file.exists(target)) stop('执行脚本不存在：', target, call. = FALSE)
  index <- which(argv == '--species')
  if (length(index) != 1L || index >= length(argv) || !identical(argv[index + 1L], 'auto'))
    stop('自动识别包装器只接受单个 --species auto', call. = FALSE)
  opt <- function(flag) {
    i <- which(argv == flag)
    if (length(i) != 1L || i >= length(argv)) stop('缺少或重复参数：', flag, call. = FALSE)
    argv[i + 1L]
  }
  result <- if (!is.null(raw_input)) {
    species_from_10x(raw_input, strsplit(raw_samples, ',', fixed = TRUE)[[1L]])
  } else if ('--datadir' %in% argv) {
    species_from_10x(opt('--datadir'), strsplit(opt('--sample'), ',', fixed = TRUE)[[1L]])
  } else {
    if (!length(argv) || startsWith(argv[1L], '--')) stop('自动识别缺少Seurat输入', call. = FALSE)
    species_from_seurat(argv[1L])
  }
  message('自动识别物种：', result$species, '; evidence: ', result$evidence)
  argv[index + 1L] <- result$species
  invisible(gc())
  # Same installed R as the wrapper, preserve target argument order and child exit status.
  status <- system2(file.path(R.home('bin'), 'Rscript'),
                    args = vapply(c(target, argv), shQuote, '', type = 'sh'))
  as.integer(status)
}

if (sys.nframe() == 0L) {
  status <- tryCatch(species_exec_main(), error = function(e) {
    message('物种自动识别/执行失败：', conditionMessage(e)); 1L
  })
  quit(status = status)
}
