# 差异分析的跳过记录：只记账，不做判定。
#   为什么单独抽出来：这段逻辑以前只活在内存里，`next` 之后什么都没留下，
#   事后无法区分「本来就没有差异」与「跑了但失败被跳过」。
diff_skip_init <- function() {
  data.frame(cell_type = character(0), status = character(0),
             n_group1 = numeric(0), n_group2 = numeric(0),
             reason = character(0), stringsAsFactors = FALSE)
}

# 0 行也能安全 rbind：先补成同一列集合，再拼。
# 写进 TSV 的文本必须是单行：换行会把一条记录拆成两行，
#   读回来就多出一条伪类型（实测出现过 cell_type="fixture-statistics-error"）。
.one_line <- function(x) {
  x <- as.character(x)
  x <- gsub("[\r\n\t]+", " ", x)
  trimws(x)
}

diff_skip_add <- function(df, cell_type, status, reason = "",
                          n1 = NA_real_, n2 = NA_real_) {
  if (is.null(df) || !nrow(df)) df <- diff_skip_init()
  row <- data.frame(cell_type = .one_line(cell_type),
                    status = .one_line(status),
                    n_group1 = as.numeric(n1), n_group2 = as.numeric(n2),
                    reason = .one_line(reason), stringsAsFactors = FALSE)
  rbind(df, row)
}

# 无论有没有跳过都写文件：没有跳过时是只有表头的空表，
#   这本身就是证据（"一个都没跳过"），比不留文件更能说明问题。
diff_skip_write <- function(df, path) {
  if (is.null(df)) df <- diff_skip_init()
  # 兜底再洗一次：调用方可能自己拼了多行文本。
  for (k in intersect(c("cell_type", "status", "reason"), names(df)))
    df[[k]] <- .one_line(df[[k]])
  write.table(df, path, quote = FALSE, sep = "\t", row.names = FALSE)
  invisible(path)
}

# 是否有计算类失败（不是"少于 3 个细胞"这种正常跳过）。
diff_skip_has_error <- function(df) {
  is.data.frame(df) && nrow(df) > 0 && any(df$status == "error")
}
