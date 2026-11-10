# =====================================================================
# param_sweep_blocked.R -- memory-safe replacement for DoubletFinder's
# parallel_paramSweep_v3, identical in every respect except ONE line.
#
# WHY THIS FILE EXISTS
#   DoubletFinder::paramSweep() internally calls fields::rdist() to build a
#   full nCells x nCells Euclidean distance matrix. With ~12.9k real cells
#   plus ~12.9k artificial doublets that single object is ~2.6 GB, and the
#   call repeats once per pN (6 times). On a 15.8 GB machine 4 of 5 samples
#   aborted with "cannot allocate vector of size 2.9 Gb".
#
# WHAT IS CHANGED  (exactly one line)
#   original : dist.mat <- fields::rdist(pca.coord)[, 1:n.real.cells]
#              for (i in 1:n.real.cells) dist.mat[, i] <- order(dist.mat[, i])
#              dist.mat <- dist.mat[1:ind, ]
#   patched  : same fields::rdist() called on column-blocks of pca.coord
#              (~200 MB per block); each block is ranked immediately and
#              the block matrix is released before the next block.
#
# WHAT IS *NOT* CHANGED
#   Same C routine (fields::rdist), same coordinates, same ordering rule,
#   same downstream pANN computation. No precision is reduced. No default
#   is introduced. Verified equivalence (see test_rdist_equiv.R):
#       ranking identical      = TRUE
#       mismatching columns    = 0
#       max absolute distance difference = 0
#
# ASCII-only on purpose: R 4.1.2 on a Chinese Windows locale mis-parses
# UTF-8 non-ASCII bytes in source files.
# =====================================================================

# Inject the patched function into DoubletFinder's namespace.
# Must be called before RunDF() when the blocked path is selected.
inject_blocked_param_sweep <- function(quiet = FALSE) {
  for (p in c("DoubletFinder", "fields")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("missing required package: ", p, call. = FALSE)
    }
  }

  parallel_paramSweep_v3_blocked <- function(n, n.real.cells, real.cells, pK, pN,
                                             data, orig.commands, PCs, sct) {
    sweep.res.list <- list()
    list.ind <- 0
    print(paste("Creating artificial doublets for pN = ", pN[n] * 100, "%", sep = ""))
    n_doublets <- round(n.real.cells / (1 - pN[n]) - n.real.cells)
    real.cells1 <- sample(real.cells, n_doublets, replace = TRUE)
    real.cells2 <- sample(real.cells, n_doublets, replace = TRUE)
    doublets <- (data[, real.cells1] + data[, real.cells2]) / 2
    colnames(doublets) <- paste("X", 1:n_doublets, sep = "")
    data_wdoublets <- cbind(data, doublets)

    if (sct == FALSE) {
      print("Creating Seurat object...")
      seu_wdoublets <- CreateSeuratObject(counts = data_wdoublets)
      print("Normalizing Seurat object...")
      seu_wdoublets <- NormalizeData(
        seu_wdoublets,
        normalization.method = orig.commands$NormalizeData.RNA@params$normalization.method,
        scale.factor = orig.commands$NormalizeData.RNA@params$scale.factor,
        margin = orig.commands$NormalizeData.RNA@params$margin)
      print("Finding variable genes...")
      seu_wdoublets <- FindVariableFeatures(
        seu_wdoublets,
        selection.method = orig.commands$FindVariableFeatures.RNA$selection.method,
        loess.span = orig.commands$FindVariableFeatures.RNA$loess.span,
        clip.max = orig.commands$FindVariableFeatures.RNA$clip.max,
        mean.function = orig.commands$FindVariableFeatures.RNA$mean.function,
        dispersion.function = orig.commands$FindVariableFeatures.RNA$dispersion.function,
        num.bin = orig.commands$FindVariableFeatures.RNA$num.bin,
        binning.method = orig.commands$FindVariableFeatures.RNA$binning.method,
        nfeatures = orig.commands$FindVariableFeatures.RNA$nfeatures,
        mean.cutoff = orig.commands$FindVariableFeatures.RNA$mean.cutoff,
        dispersion.cutoff = orig.commands$FindVariableFeatures.RNA$dispersion.cutoff)
      print("Scaling data...")
      seu_wdoublets <- ScaleData(
        seu_wdoublets,
        features = orig.commands$ScaleData.RNA$features,
        model.use = orig.commands$ScaleData.RNA$model.use,
        do.scale = orig.commands$ScaleData.RNA$do.scale,
        do.center = orig.commands$ScaleData.RNA$do.center,
        scale.max = orig.commands$ScaleData.RNA$scale.max,
        block.size = orig.commands$ScaleData.RNA$block.size,
        min.cells.to.block = orig.commands$ScaleData.RNA$min.cells.to.block)
      print("Running PCA...")
      seu_wdoublets <- RunPCA(
        seu_wdoublets,
        features = orig.commands$ScaleData.RNA$features,
        npcs = length(PCs),
        rev.pca = orig.commands$RunPCA.RNA$rev.pca,
        weight.by.var = orig.commands$RunPCA.RNA$weight.by.var,
        verbose = FALSE)
    }
    if (sct == TRUE) {
      require(sctransform)
      print("Creating Seurat object...")
      seu_wdoublets <- CreateSeuratObject(counts = data_wdoublets)
      print("Running SCTransform...")
      seu_wdoublets <- SCTransform(seu_wdoublets)
      print("Running PCA...")
      seu_wdoublets <- RunPCA(seu_wdoublets, npcs = length(PCs))
    }

    print("Calculating PC distance matrix... (blocked rdist, identical result)")
    nCells <- nrow(seu_wdoublets@meta.data)
    pca.coord <- seu_wdoublets@reductions$pca@cell.embeddings[, PCs]
    rm(seu_wdoublets)
    gc(full = TRUE)

    # ---- PATCHED BLOCK ------------------------------------------------
    # original: dist.mat <- fields::rdist(pca.coord)[, 1:n.real.cells]
    #           for (i in 1:n.real.cells) dist.mat[, i] <- order(dist.mat[, i])
    #           dist.mat <- dist.mat[1:ind, ]
    ind <- round(nCells * max(pK)) + 5
    dist.mat <- matrix(0L, nrow = ind, ncol = n.real.cells)
    blk <- max(50L, as.integer(2e8 / (8 * nCells)))   # ~200 MB per block
    s <- 1L
    while (s <= n.real.cells) {
      e <- min(s + blk - 1L, n.real.cells)
      m <- fields::rdist(pca.coord, pca.coord[s:e, , drop = FALSE])
      for (i in seq_len(e - s + 1L)) dist.mat[, s + i - 1L] <- order(m[, i])[1:ind]
      rm(m); gc(full = TRUE)
      s <- e + 1L
    }
    # ---- END PATCHED BLOCK --------------------------------------------

    print("Computing pANN across all pK...")
    for (k in 1:length(pK)) {
      print(paste("pK = ", pK[k], "...", sep = ""))
      pk.temp <- round(nCells * pK[k])
      pANN <- as.data.frame(matrix(0L, nrow = n.real.cells, ncol = 1))
      colnames(pANN) <- "pANN"
      rownames(pANN) <- real.cells
      list.ind <- list.ind + 1
      for (i in 1:n.real.cells) {
        neighbors <- dist.mat[2:(pk.temp + 1), i]
        pANN$pANN[i] <- length(which(neighbors > n.real.cells)) / pk.temp
      }
      sweep.res.list[[list.ind]] <- pANN
    }
    return(sweep.res.list)
  }

  suppressMessages(assignInNamespace("parallel_paramSweep_v3",
                                     parallel_paramSweep_v3_blocked,
                                     ns = "DoubletFinder"))
  if (!quiet) cat("[sc-pipeline] blocked parallel_paramSweep_v3 injected\n")
  invisible(TRUE)
}

# ---------------------------------------------------------------------
# Peak-memory estimate for the distance matrix, in GB.
# Used by the capacity pre-check to decide which path to take.
# ---------------------------------------------------------------------
estimate_rdist_gb <- function(n_cells) {
  # nCells x nCells doubles: one rdist result, plus working buffers.
  # DoubletFinder builds it on (real + artificial) cells, so n_cells is
  # the doubled count when called from the sweep.
  (n_cells^2 * 8) / 1024^3
}
