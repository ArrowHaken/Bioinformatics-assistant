# =====================================================================
#  决策：跑到那一步才生成问题，人答了才算定
# ---------------------------------------------------------------------
#  设计原则（gina 2026-10-28 的原话）：
#    「不会提前有清单的，清单需要处理的过程中你给人发让人去选择
#      才会得到清单」
#
#  所以这里没有"预置清单"：
#    · 不复制模板、不预先写一批 pending 条目；
#    · 只有走到某个阶段（或某个阶段刚跑完、下一步需要取舍）时，
#      才按当时的真实数据生成这一条问题；
#    · 问题生成之后立刻进入待答状态，并且要真的发到会话里让人看见；
#    · 人答了才落成 answer。没人答就一直是 asked。
#
#  与旧 gates.json 的区别：
#    旧的那份是初始化时把九条一次性抄过去，人在服务器模式下根本读不到，
#    于是"要人决定的事"整块消失、也没人被告知。
#    这里反过来了：清单的长度取决于跑到哪、缺什么，不跑就没有清单。
# =====================================================================

DEC_DIR <- local({
  # 同上：走统一开关，写成整块避免顶层 else 的语法坑。
  if (exists("state_private_root", mode = "function")) {
    file.path(state_private_root(dirname(STATE_DIR)), "decisions")
  } else {
    file.path(STATE_DIR, "decisions")
  }
})

dec_path <- function(sid) {
  s <- as.character(sid %||% "")
  if (!nzchar(s)) s <- "_default"
  s <- gsub("[^A-Za-z0-9._-]", "_", s)
  file.path(DEC_DIR, paste0(s, ".json"))
}

dec_read <- function(sid) {
  f <- dec_path(sid)
  if (!file.exists(f)) return(list(session_id = as.character(sid %||% ""), items = list()))
  d <- tryCatch(jsonlite::fromJSON(read_text_any(f), simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(d) || is.null(d$items)) return(list(session_id = as.character(sid %||% ""), items = list()))
  d
}

dec_write <- function(sid, d) {
  d$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  write_json_file(dec_path(sid), d)
  invisible(d)
}

# 已答的取答案；没答的返回 NULL。给后面拼指令用。
dec_answer_of <- function(sid, key) {
  d <- dec_read(sid)
  for (it in d$items) {
    if (identical(as.character(it$key %||% ""), as.character(key)) &&
        identical(as.character(it$status %||% ""), "answered")) {
      # Preserve old answers on disk, but never execute evidence-dependent
      # scoped answers created before ownership was established.
      if (nzchar(dec_sid(sid)) && as.character(key) %in%
          c("cluster_filter", "dropsample", "read_exclusion") &&
          !identical(dec_sid(it$evidence_session_id), dec_sid(sid))) return(NULL)
      return(list(value = it$answer, detail = it$answer_detail))
    }
  }
  NULL
}

dec_pending <- function(sid) {
  d <- dec_read(sid)
  Filter(function(x) identical(as.character(x$status %||% ""), "asked"), d$items)
}

# ---- 证据：能读到什么就摆什么，读不到就明说读不到 -------------------
# A callback's envelope is the ownership boundary. Do not infer ownership from
# the selected session, a run directory, or a shared filename.
dec_sid <- function(sid = "") {
  if (is.null(sid) || length(sid) != 1L || is.na(sid)) return("")
  as.character(sid)
}

dec_owned <- function(x, sid) {
  is.list(x) && identical(dec_sid(x$session_id), dec_sid(sid))
}

dec_steps <- function(sid = "") {
  sid <- dec_sid(sid)
  d <- remote_steps_read(sid)
  steps <- d$steps %||% list()
  if (nzchar(sid)) steps <- Filter(function(x) dec_owned(x, sid), steps)
  steps
}

# Callback contract: evidence = list(cluster1_evidence = list(clusters = ...),
# after_drop_stats = list(samples = ..., cells_total = ...)). The callback must
# carry session_id. Nested session_id, when supplied, must agree with it.
dec_evidence <- function(key, step, sid = "") {
  sid <- dec_sid(sid)
  rows <- dec_steps(sid)
  for (rec in rev(rows)) {
    if (!identical(as.character(rec$id %||% ""), step)) next
    ev <- if (is.list(rec$evidence)) rec$evidence[[key]] else NULL
    if (is.null(ev)) ev <- rec[[key]]
    if (!is.list(ev)) next
    if (nzchar(sid) && !is.null(ev$session_id) && !dec_owned(ev, sid)) next
    return(ev)
  }
  f <- file.path(STATE_DIR, "remote", paste0(key, ".json"))
  if (!file.exists(f)) return(NULL)
  ev <- tryCatch(jsonlite::fromJSON(read_text_any(f), simplifyVector = FALSE),
                 error = function(e) NULL)
  # Legacy callers may read old files; scoped callers require explicit ownership.
  if (!is.list(ev) || (nzchar(sid) && !dec_owned(ev, sid))) return(NULL)
  ev
}

dec_clusters <- function(sid = "") {
  dec_evidence("cluster1_evidence", "markers", sid)
}

# These tables have different cluster numbering and must never substitute.
dec_after_drop <- function(sid = "") {
  ad <- dec_evidence("after_drop_stats", "filter", sid)
  if (is.null(ad)) return(NULL)
  ad$samples <- dec_samples_norm(ad$samples %||% NULL,
                                 map = ad$sample_group %||% NULL)
  ad
}

# 把「样本」统一成记录列表：list(list(sample=, group=, n=), ...)
#   为什么必须归一化：执行端历史上给的是命名映射 {"18m-G489": 11630}，
#   而下游按 s0$sample / s0$group / s0$n 取字段。直接对命名映射取 $sample
#   会抛「$ operator is invalid for atomic vectors」，整条决策流当场断掉 ——
#   页面看不到「要不要剔样本」，人也就没法做这个决定。
#   两种形状都接受，出口只有一种。
#   分组只认明确来源（map 或记录自带）；没有就留 NA，不按样本名猜组。
dec_samples_norm <- function(x, map = NULL) {
  if (is.null(x) || !length(x)) return(list())
  out <- list()
  if (is.list(x) && is.null(names(x))) {
    # 已是记录列表
    for (it in x) {
      if (!is.list(it)) next
      nm <- as.character(it$sample %||% it$name %||% "")
      if (!nzchar(nm)) next
      n <- suppressWarnings(as.numeric(it$n %||% it$n_cells %||% NA))
      g <- as.character(it$group %||% "")
      if (!nzchar(g) && is.list(map)) g <- as.character(map[[nm]] %||% "")
      out[[length(out) + 1L]] <- list(sample = nm,
                                      group = if (nzchar(g)) g else NA_character_,
                                      n = n)
    }
    return(out)
  }
  # 命名映射 / 命名向量：名字是样本，值是细胞数
  nms <- names(x)
  if (is.null(nms)) return(list())
  for (i in seq_along(x)) {
    nm <- as.character(nms[i])
    if (!nzchar(nm)) next
    v <- x[[i]]
    n <- suppressWarnings(as.numeric(if (is.list(v)) (v$n %||% v$n_cells %||% NA) else v))
    g <- ""
    if (is.list(map)) g <- as.character(map[[nm]] %||% "")
    out[[length(out) + 1L]] <- list(sample = nm,
                                    group = if (nzchar(g)) g else NA_character_,
                                    n = n)
  }
  out
}

# 从一份簇表里挑出"该建议剔除"的簇：空簇、极小簇、单组严重偏斜。
#   返回 list(empty=, tiny=, skew=, ids=)
# 从一份簇表里挑出"该建议剔除"的簇：空簇、极小簇、单组严重偏斜。
#   两份簇表的字段不同：一次聚类那份只有百分比（mock_pct/mp_pct），
#   筛群后那份有绝对数（mock/mp）。这里统一成同一套判据，
#   免得同一个判断在两处算出不同答案。
dec_mp_pct <- function(c0) {
  m <- suppressWarnings(as.numeric(c0$mock %||% NA))
  p <- suppressWarnings(as.numeric(c0$mp %||% NA))
  if (!is.na(m) && !is.na(p) && (m + p) > 0) return(100 * p / (m + p))
  pp <- suppressWarnings(as.numeric(c0$mp_pct %||% NA))
  if (!is.na(pp)) return(pp)
  NA_real_
}

# 簇细胞数：以 n_cells 为准，n 只在 n_cells 缺失时兜底。
#   为什么：执行端回写的逐簇依据里字段叫 n_cells，
#   老代码只读 n，于是每一簇都被算成 0 —— 页面会把所有簇显示成
#   「0 个细胞」并推荐一份错名单。两处（判据与展示文案）必须用同一个取值。
dec_cells <- function(c0) {
  v <- suppressWarnings(as.numeric(c0$n_cells %||% NA))
  if (is.na(v)) v <- suppressWarnings(as.numeric(c0$n %||% NA))
  v
}

dec_judge <- function(clusters) {
  empty <- character(0); tiny <- character(0); skew <- character(0)
  for (c0 in (clusters %||% list())) {
    id <- as.character(c0$cluster %||% "")
    n <- dec_cells(c0)
    if (!nzchar(id) || is.na(n)) next
    if (n == 0) { empty <- c(empty, id); next }
    if (n < 300) { tiny <- c(tiny, id); next }
    mp <- dec_mp_pct(c0)
    if (!is.na(mp) && (mp > 95 || mp < 10)) skew <- c(skew, id)
  }
  list(empty = empty, tiny = tiny, skew = skew,
       ids = unique(c(empty, tiny, skew)))
}

dec_samples <- function(sid = "") {
  # 样本名从已经回写过的产物里取（1.doublet/<sample>.rds），没有就返回空。
  steps <- dec_steps(sid)
  out <- character(0)
  for (s in steps) {
    if (!identical(as.character(s$id %||% ""), "doublet")) next
    for (a in (s$artifacts %||% list())) {
      rel <- if (is.list(a)) as.character(a$rel %||% "") else as.character(a)
      if (!grepl("^1\\.doublet/.*\\.rds$", rel)) next
      out <- c(out, sub("^1\\.doublet/", "", sub("\\.rds$", "", rel)))
    }
  }
  unique(out)
}

# ---- 生成：每个阶段到点才产出的问题 --------------------------------
#  返回 list(list(key=, title=, question=, why=, options=list(list(value=,label=)),
#                 suggested=, evidence=list(...)))
#  没有该阶段的问题就返回空列表 —— 空列表是正常结果，不是错误。
dec_specs <- function(stepId, sid = "") {
  out <- list()

  if (identical(stepId, "filter")) {
    # 筛群读的是合并后的对象（29 个簇，编号 0-28）。
    #   所以这一步能选的簇号，只能是这套编号 —— 依据必须来自同一份对象。
    #   之前我错用了筛群之后那份（24 簇）的表，两套编号混用就会删错细胞群：
    #   29 簇体系里 21 号是 399 细胞，24 簇体系里 21 号是 243 细胞、Mock 占九成。
    clu29 <- (dec_clusters(sid) %||% list())$clusters
    j <- if (!is.null(clu29)) dec_judge(clu29) else list(empty = character(0),
                                                         tiny = character(0),
                                                         skew = character(0),
                                                         ids = character(0))
    strong <- unique(c(j$empty, j$tiny))
    soft <- unique(c(strong, j$skew))

    opts <- list(list(value = "keep_all", label = "全部保留，一个都不删"))
    if (length(strong)) {
      opts <- c(opts, list(list(
        value = paste(strong, collapse = ","),
        label = paste0("删空簇与极小簇：", paste(strong, collapse = "、"),
                       "（都少于 300 个细胞，差异分析跑得出来但结论不可靠）"))))
    }
    if (length(soft) > length(strong)) {
      opts <- c(opts, list(list(
        value = paste(soft, collapse = ","),
        label = paste0("再加上单组严重偏斜的：", paste(soft, collapse = "、")))))
    }
    opts <- c(opts, list(list(value = "custom", label = "我自己指定（在下面写簇号，逗号分隔）")))

    ev <- NULL
    if (!is.null(clu29)) {
      rows <- lapply(Filter(function(c0) {
        n <- dec_cells(c0)
        if (is.na(n)) return(FALSE)
        if (n == 0) return(TRUE)
        if (n < 300) return(TRUE)
        mp <- dec_mp_pct(c0)
        !is.na(mp) && (mp > 95 || mp < 10)
      }, clu29), function(c0) {
        n <- dec_cells(c0); if (is.na(n)) n <- 0
        mp <- dec_mp_pct(c0)
        # 标记必须写真实原因。之前一律写"极小簇"，于是 1524 个细胞的 14 号
        #   也被标成"极小簇（1524 细胞）"—— 数字自己反驳自己，
        #   人一看就不敢信这份依据。这里按实际触发的那条判据写。
        flag <- if (n == 0) "空簇：0 个细胞"
        else if (n < 300) paste0("极小簇（", n, " 细胞）")
        else if (!is.na(mp)) paste0("单组偏斜：Mock ", round(100 - mp, 1),
                                    "% / MP ", round(mp, 1), "%")
        else "需人工确认"
        list(cluster = as.character(c0$cluster %||% ""), n = n,
             mock_pct = if (is.na(mp)) 0 else round(100 - mp, 1),
             mp_pct = if (is.na(mp)) 0 else round(mp, 1),
             flags = flag)
      })
      ev <- list(kind = "clusters", total = length(clu29), flagged = rows,
                 note = paste0("依据：合并对象（这一步要读的那份）的真实簇构成，共 ",
                               length(clu29), " 个簇。"))
    }
    out[[length(out) + 1]] <- list(
      key = "cluster_filter",
      title = "这一步要剔掉哪些簇",
      question = "筛群后要重聚类，剔哪些簇会直接改变后面所有结果，所以停在这里问你。",
      # 依据文案必须跟着真实数据走。
      #   改前写死「0-28」和「不到 300 个细胞…基本是噪音」：
      #     ① 下一轮簇数不同，编号区间就不是 0-28，写死等于给错范围；
      #     ② 「基本是噪音」是替人下了科学结论。小簇或单组偏斜也可能
      #        反映真实的生物学差异（稀有细胞类型、处理组特异扩增）。
      #   这里只做两件事：按本次真实簇表回显范围与判据触发情况，
      #   并明确这是供人工复核的提示，系统不自动剔除、也不断言必然噪音。
      why = paste0(
        "依据是这份对象自己的簇号（下表，共 ",
        if (!is.null(clu29)) length(clu29) else 0, " 个簇",
        if (!is.null(clu29) && length(clu29)) paste0("，编号 ",
          paste(range(suppressWarnings(as.numeric(vapply(clu29, function(x)
            as.character(x$cluster %||% ""), ""))), na.rm = TRUE), collapse = "-")) else "",
        "）。下面标出的只是供人工复核的提示：",
        "0 个细胞的簇没有信息量；细胞很少的簇或单组严重偏斜的簇，",
        "统计功效低、差异结果不稳定，但也可能是稀有细胞类型或真实的组间差异。",
        "是否剔除、剔哪些，由你按生物学背景判断；系统不会自动剔除，",
        "也不认为这些簇必然是噪音。"),
      options = opts,
      suggested = if (length(strong)) paste(strong, collapse = ",") else "keep_all",
      evidence = ev)
  }

  if (identical(stepId, "final")) {
    # 终版要按簇做组间差异：某簇某组细胞数不足时怎么办，这必须人来定。
    out[[length(out) + 1]] <- list(
      key = "diff_guard",
      title = "差异分析碰到某组没有细胞的簇，怎么办",
      question = "跑差异时如果某簇里某一组细胞数为 0 或极少，要跳过还是整体停下？",
      why = "上一轮就是这里断的：R 在某个簇上抛 No cells found 后直接退出，整步失败。这个取舍会改变结果口径，不能由脚本替你定。",
      options = list(
        list(value = "skip", label = "跳过这些簇，其余照跑（写进日志）"),
        list(value = "stop", label = "整体停下，先修数据再跑"),
        list(value = "merge", label = "把这些簇合并后再比较")),
      suggested = "skip",
      evidence = list(kind = "note",
                      note = "上一轮实测：日志停在 DIFF_OK cluster 23，之后报 Error: No cells found / Execution halted。"))

    # 剔除整个样本：这件事以前是写在脚本里的一个字符串，没人被问过。
    #   gina 的原话：「我需要删除 G761，但是这个环节最好要问一次我」。
    #   所以它是一个必须由人确认的决策点，而不是脚本的默认行为 ——
    #   剔一个样本会同时改变样本量、分组平衡和所有下游数字。
    ad2 <- dec_after_drop(sid)
    smp <- if (!is.null(ad2)) (ad2$samples %||% list()) else list()
    if (length(smp)) {
      opts <- list(list(value = "none", label = paste0("一个都不删，", length(smp), " 个样本全用")))
      for (s0 in smp) {
        nm <- as.character(s0$sample %||% "")
        if (!nzchar(nm)) next
        grp <- as.character(s0$group %||% "")
        grp_known <- nzchar(grp) && !is.na(grp)
        nn <- suppressWarnings(as.numeric(s0$n %||% NA))
        # 分组没有明确来源时不写「X 组」，只报细胞数：
        #   按样本名猜组会把未知当已知，人看到的是编出来的分组。
        label <- if (grp_known)
          paste0("删掉 ", nm, "（", grp, " 组，", as.character(nn), " 个细胞）")
        else
          paste0("删掉 ", nm, "（", if (is.na(nn)) "细胞数未知" else paste0(as.character(nn), " 个细胞"), "）")
        opts <- c(opts, list(list(value = nm, label = label)))
      }
      # 建议值按"保持两组样本数接近"来给：他现在就是 3 对 4，
      #   而 Mock 组第三个样本是哪一个不由脚本挑，所以只把差额说出来。
      n_mock <- length(Filter(function(x) identical(as.character(x$group %||% ""), "Mock"), smp))
      n_mp <- length(Filter(function(x) identical(as.character(x$group %||% ""), "Microplastics"), smp))
      n_grp_known <- n_mock + n_mp
      # 分组没确认时不能报「Mock 3 个 / MP 4 个」：那是编的。
      q_txt <- if (n_grp_known == length(smp))
        paste0("现在 Mock ", n_mock, " 个、Microplastics ", n_mp,
               " 个样本。要剔掉哪一个吗？")
      else
        paste0("现在共 ", length(smp), " 个样本。要剔掉哪一个吗？",
               "（本次记录里没有已确认的样本分组，所以不列组别）")
      out[[length(out) + 1]] <- list(
        key = "dropsample",
        title = "终版之前要不要剔掉整个样本",
        question = q_txt,
        why = "剔样本会同时改样本量、分组平衡和所有下游数字，而且剔掉之后补不回来。上一轮是脚本里写死剔 18m-G761，没有经过确认，所以这次先问你。",
        options = opts,
        suggested = "none",
        evidence = list(kind = "note",
                        note = paste0("筛群后仍是 ", length(smp), " 个样本，共 ",
                                      as.character(ad2$cells_total %||% ""), " 个细胞。",
                                      "上一轮 final 实际剔掉了 18m-G761，细胞数从 86462 降到 73661。")))
    }
  }

  if (identical(stepId, "doublet")) {
    smp <- dec_samples(sid)
    if (length(smp)) {
      out[[length(out) + 1]] <- list(
        key = "read_exclusion",
        title = "读入前要不要整体剔掉某个样本",
        question = paste0("这 ", length(smp), " 个样本里有需要整份剔除的吗？（例如质量差、分组不全的）"),
        why = "整份剔样本会改变样本量与分组平衡；这一步之后再想补回来就得从头跑。",
        options = c(list(list(value = "none", label = "不剔，全部读入")),
                    lapply(smp, function(s) list(value = s, label = paste0("剔掉 ", s)))),
        suggested = "none",
        evidence = list(kind = "note", note = paste0("当前回写的样本：", paste(smp, collapse = "、"))))
    }
  }

  out
}

# 到点生成：把还没生成过的问题写进记录（status=asked），返回新生成的条目。
#   重复调用不重复生成 —— 同 key 已存在就跳过，人改过的答案不会被冲掉。
dec_ensure <- function(stepId, sid = "") {
  d <- dec_read(sid)
  have <- vapply(d$items, function(x) as.character(x$key %||% ""), "")
  specs <- dec_specs(stepId, sid)
  changed <- FALSE
  # A sample-dependent question can disappear when its evidence is rejected.
  # Remove only obsolete pending questions, never historical answered entries.
  wanted <- vapply(specs, function(x) as.character(x$key), "")
  if (nzchar(dec_sid(sid))) {
    keep <- Filter(function(it) !(identical(as.character(it$step %||% ""), stepId) &&
      identical(as.character(it$status %||% ""), "asked") &&
      as.character(it$key %||% "") %in% c("read_exclusion", "dropsample") &&
      !(as.character(it$key) %in% wanted)), d$items)
    changed <- length(keep) != length(d$items)
    d$items <- keep
    have <- vapply(d$items, function(x) as.character(x$key %||% ""), "")
  }
  made <- list()
  for (sp in specs) {
    if (as.character(sp$key) %in% have) {
      # Refresh pending questions only; keep identity, timestamps and human
      # answers intact. This also removes options leaked by the old reader.
      for (i in which(have == as.character(sp$key))) {
        it <- d$items[[i]]
        if (!identical(as.character(it$status %||% ""), "asked")) next
        for (field in c("title", "question", "why", "options", "suggested", "evidence"))
          it[field] <- list(sp[[field]])
        it$evidence_session_id <- dec_sid(sid)
        if (!identical(it, d$items[[i]])) { d$items[[i]] <- it; changed <- TRUE }
      }
      next
    }
    it <- list(
      id = paste0("dec-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                  paste0(sample(c(letters, 0:9), 4, replace = TRUE), collapse = "")),
      key = as.character(sp$key),
      step = as.character(stepId),
      title = as.character(sp$title),
      question = as.character(sp$question),
      why = as.character(sp$why %||% ""),
      options = sp$options,
      suggested = sp$suggested,
      evidence = sp$evidence,
      evidence_session_id = dec_sid(sid),
      status = "asked",
      asked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      answer = NULL, answer_detail = NULL, answered_at = NULL)
    d$items[[length(d$items) + 1]] <- it
    made[[length(made) + 1]] <- it
  }
  if (length(made) || changed) dec_write(sid, d)
  made
}

dec_answer <- function(sid, id, answer, detail = "") {
  d <- dec_read(sid)
  for (i in seq_along(d$items)) {
    if (!identical(as.character(d$items[[i]]$id %||% ""), as.character(id))) next
    d$items[[i]]$status <- "answered"
    d$items[[i]]$answer <- answer
    d$items[[i]]$answer_detail <- detail
    d$items[[i]]$answered_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    dec_write(sid, d)
    return(d$items[[i]])
  }
  NULL
}

# 把"要人决定什么"写成一段人能直接读懂的话（会真的发进会话）。
dec_ask_text <- function(items, stepTitle = "") {
  if (!length(items)) return("")
  L <- c("【来自 个性化分析助手】需要你定一件事，定完才能往下跑。", "")
  for (i in seq_along(items)) {
    it <- items[[i]]
    L <- c(L, paste0(i, ". ", as.character(it$title %||% "")))
    L <- c(L, paste0("   ", as.character(it$question %||% "")))
    if (nzchar(as.character(it$why %||% ""))) L <- c(L, paste0("   为什么问你：", as.character(it$why)))
    opts <- it$options %||% list()
    if (length(opts)) {
      L <- c(L, "   可选：")
      for (o in opts) {
        L <- c(L, paste0("     · ", as.character(o$label %||% o$value %||% ""),
                         if (identical(as.character(o$value %||% ""), as.character(it$suggested %||% "")))
                           "（建议值）" else ""))
      }
    }
    L <- c(L, "")
  }
  L <- c(L, "回我一句选哪个就行；或者直接打开应用那一栏点选。")
  paste(L, collapse = "\n")
}
