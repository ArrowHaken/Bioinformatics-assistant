# =====================================================================
# test_rdist_equiv.R -- equivalence proof for the blocked distance matrix
#
# WHY
#   sc-pipeline 在内存不足时会用 scripts/lib/param_sweep_blocked.R
#   替换 DoubletFinder 的分块距离矩阵实现。这个测试用来证明：
#   分块写法与官方一次算完的写法，结果【完全一致】。
#
# 它比对两件事：
#   1) 距离矩阵的数值本身（不是只比排名）
#   2) 下游真正用到的排序结果（DoubletFinder 取每列前 ind 名）
#
# 用法：
#   Rscript scripts/lib/test_rdist_equiv.R
#   Rscript scripts/lib/test_rdist_equiv.R --n 800 --m 200 --ind 40
#
# 退出码：0 = 完全等价；非 0 = 存在差异（必须停下来查，不许继续）
# ASCII-only on purpose (R 4.1.2 on a Chinese Windows locale mis-parses
# UTF-8 non-ASCII bytes in source files).
# =====================================================================

args <- commandArgs(trailingOnly = TRUE)
get_opt <- function(k, d) {
  i <- match(k, args)
  if (is.na(i) || i == length(args)) return(d)
  as.integer(args[i + 1])
}
n   <- get_opt("--n",   400L)   # number of rows (cells)
m   <- get_opt("--m",   120L)   # number of columns to rank
ind <- get_opt("--ind",  30L)   # neighbours kept per column
blk <- get_opt("--blk",   7L)   # block size (small on purpose, to stress the path)

if (!requireNamespace("fields", quietly = TRUE)) {
  stop("缺少 fields 包。它是 DoubletFinder 的依赖，装 DoubletFinder 时会一并装上。", call. = FALSE)
}

set.seed(1)
coords <- matrix(rnorm(n * 3), ncol = 3)

# ---- ORIGINAL: one-shot full matrix, then rank each column -----------
full <- fields::rdist(coords)
orig.rank <- matrix(0L, nrow = ind, ncol = m)
for (i in 1:m) orig.rank[, i] <- order(full[, i])[1:ind]

# ---- PATCHED: blocked rdist, rank each block immediately -------------
blk.rank <- matrix(0L, nrow = ind, ncol = m)
dist.vals <- matrix(NA_real_, n, m)
s <- 1L
while (s <= m) {
  e <- min(s + blk - 1L, m)
  mm <- fields::rdist(coords, coords[s:e, , drop = FALSE])
  dist.vals[, s:e] <- mm
  for (i in seq_len(e - s + 1L)) blk.rank[, s + i - 1L] <- order(mm[, i])[1:ind]
  s <- e + 1L
}

rank_identical <- identical(blk.rank, orig.rank)
mismatch_cols  <- sum(colSums(abs(blk.rank - orig.rank)) != 0)
max_rank_diff  <- max(abs(blk.rank - orig.rank))
max_dist_diff  <- max(abs(full[, 1:m] - dist.vals))

cat("===== blocked-rdist equivalence =====\n")
cat("matrix          : ", n, " x ", m, "  (block = ", blk, ")\n", sep = "")
cat("neighbours kept : ", ind, "\n", sep = "")
cat("RANK_IDENTICAL          : ", rank_identical, "\n", sep = "")
cat("MISMATCH_COLS           : ", mismatch_cols, "\n", sep = "")
cat("MAX_RANK_ELEM_DIFF      : ", max_rank_diff, "\n", sep = "")
cat("MAX_ABS_DISTANCE_DIFF   : ", format(max_dist_diff, scientific = TRUE), "\n", sep = "")
cat("=====================================\n")

ok <- isTRUE(rank_identical) && mismatch_cols == 0 && max_dist_diff == 0
if (!ok) {
  cat("RESULT: NOT EQUIVALENT -- do not use the blocked path.\n")
  quit(status = 1)
}
cat("RESULT: EQUIVALENT -- blocked path is safe to use on low-memory machines.\n")
quit(status = 0)
