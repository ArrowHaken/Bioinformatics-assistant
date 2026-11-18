# figs 小表守恒核验
# ---------------------------------------------------------------------------
# 为什么不套 min_bytes：QC / 比例 / 跳过这三张表本来就几百字节，
# 套 1024 下限会把完整正确的表判成"写了一半"。
# 但放宽体积不等于不核。这里按表逐条核：
#   · 证据绑定：每张表都要与产物清单里的 md5 逐字相等
#   · 结构与取值：列名、整数非负、比率 0~100 且与分子分母自洽
#   · 键完整唯一：样本×类型、组×类型都必须齐且各一次
#   · 跨表一致：按 sample→group 映射汇总后逐类型相等，不是只比总数
#   · 上游绑定：逐类型总数、未定型数、总细胞数都要与已验收记录相等
#   · 跳过表：按 status 分别核规则，no_sig 合法，error 拒

figs_norm_sample <- function(x) {
  v <- trimws(as.character(x))
  # 保留长度：regmatches 在没匹配时会丢元素，长度一变后面对不齐。
  #   三种写法都要归到一起：18m-G489 / G489 / 裸 489。
  out <- v
  m1 <- grepl("G[0-9]+$", v)
  out[m1] <- sub("^.*(G[0-9]+)$", "\\1", v[m1])
  m2 <- !m1 & grepl("^[0-9]+$", v)
  out[m2] <- paste0("G", v[m2])
  out
}

figs_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
figs_int_ok <- function(n) length(n) > 0 && all(is.finite(n)) && all(n >= 0) && all(n == floor(n))

figs_table <- function(facts, rel) {
  tabs <- facts$tables %||% NULL
  if (is.null(tabs) || !length(tabs)) return(NULL)
  t0 <- tabs[[rel]]
  if (is.null(t0) || !is.list(t0)) return(NULL)
  art <- Filter(function(a) identical(as.character(a$rel %||% ""), rel),
                facts$artifacts %||% list())
  amd5 <- if (length(art)) as.character(art[[1]]$md5 %||% "") else ""
  list(rel = rel, columns = as.character(t0$columns %||% character(0)),
       rows = t0$data %||% NULL, md5 = as.character(t0$md5 %||% ""),
       artifact_md5 = amd5, in_list = length(art) > 0)
}

figs_rows_df <- function(tb) {
  if (is.null(tb) || is.null(tb$rows) || !length(tb$rows)) return(NULL)
  tryCatch({
    cols <- tb$columns
    m <- lapply(tb$rows, function(r) { v <- as.character(unlist(r)); length(v) <- length(cols); v })
    out <- as.data.frame(do.call(rbind, m), stringsAsFactors = FALSE)
    colnames(out) <- cols
    out
  }, error = function(e) NULL)
}

vchk_fig <- function(nm, status, detail, expected, actual, evidence) {
  list(layer = "L1", name = nm, status = status, detail = detail,
       expected = expected, actual = actual, evidence = evidence)
}

# 主审计
#   upstream: total_cells, unassigned_cells, typed_types, type_counts(named), sample_group(named)
figs_tables_audit <- function(facts, upstream = NULL, declared = NULL) {
  out <- list()
  add <- function(x) out[[length(out) + 1L]] <<- x
  up <- upstream %||% list()

  QC   <- "10.figs/0.QC/cell_number_stat.xls"
  SMP  <- "10.figs/3.cell_ratio/cell_number_sample.xls"
  GRP  <- "10.figs/3.cell_ratio/cell_number_group.xls"
  SKIP <- "10.figs/4.diff_enrich/diff/diff_skipped.xls"

  # ---- 0 绑定：四张表都在清单里，md5 逐字相等 ----
  rels <- c(QC, SMP, GRP, SKIP)
  tabs <- lapply(rels, function(r) figs_table(facts, r)); names(tabs) <- rels
  bind_ok <- TRUE
  for (rel in rels) {
    tb <- tabs[[rel]]
    if (is.null(tb) || !isTRUE(tb$in_list)) {
      add(vchk_fig(paste0("figs_table:", rel), "fail",
        "这张表不在本步产物清单里，或执行端没带回它的结构",
        "在清单且结构齐全", "缺", rel)); bind_ok <- FALSE; next
    }
    if (!nzchar(tb$md5) || !nzchar(tb$artifact_md5) || !identical(tb$md5, tb$artifact_md5)) {
      add(vchk_fig(paste0("figs_table:", rel), "fail",
        "表结构的 md5 与产物清单里的 md5 不一致（或缺失），无法确认核的就是这份文件",
        "md5 逐字相等",
        paste0("表 ", if (nzchar(tb$md5)) tb$md5 else "缺",
               " / 清单 ", if (nzchar(tb$artifact_md5)) tb$artifact_md5 else "缺"), rel))
      bind_ok <- FALSE
    }
  }
  if (!bind_ok) {
    add(vchk_fig("figs_tables_bound", "fail",
      "证据绑定未成立，后续守恒不采信", "全部绑定", "未通过", "回写 tables{} 与 artifacts[]"))
    return(out)
  }

  decl <- figs_norm_sample(as.character(declared %||% character(0)))
  decl <- decl[nzchar(decl)]
  expect_types <- unique(c(as.character(up$typed_types %||% character(0)),
                           as.character(up$unassigned_label %||% "unassigned")))
  type_counts <- up$type_counts %||% NULL
  sgroup <- up$sample_group %||% NULL
  # 映射的键与声明都要先规范化再对：一边是 18m-G489、另一边是 G489 时，
  #   直接按名字取会全取成 NA，于是分组校验永远失败（正常结果被误拒）。
  if (!is.null(sgroup) && length(sgroup)) names(sgroup) <- figs_norm_sample(names(sgroup))
  groups_expect <- if (!is.null(sgroup) && length(sgroup))
    unique(as.character(sgroup[decl])) else character(0)
  groups_expect <- groups_expect[!is.na(groups_expect) & nzchar(groups_expect)]

  # ---- 1 QC 表 ----
  q <- figs_rows_df(tabs[[QC]])
  q_ok <- FALSE
  if (is.null(q) || !all(c("sample", "cell_number") %in% colnames(q))) {
    add(vchk_fig("figs_qc_table", "unknown",
      "QC 表没带回逐行数据或列名不全，样本守恒核不了", "列 sample,cell_number + 逐行", "未取到", QC))
  } else {
    smp <- figs_norm_sample(q$sample); n <- figs_num(q$cell_number)
    dup <- anyDuplicated(smp)
    bad_num <- !figs_int_ok(n)
    set_ok <- length(decl) > 0 && setequal(smp, decl) && !dup && length(smp) == length(decl)
    total <- if (bad_num) NA_real_ else sum(n)
    up_total <- suppressWarnings(as.numeric(up$total_cells %||% NA))
    q_ok <- set_ok && !bad_num

    add(vchk_fig("figs_qc_samples", if (set_ok) "pass" else "fail",
      if (set_ok) "样本集合与本分析声明的一致（规范化后比较，且无重复）"
      else if (!length(decl)) "本分析没有声明样本清单，无法核对样本集合"
      else "QC 表样本集合与声明不一致（缺样本、多样本或重复）",
      paste0("集合 = ", paste(sort(decl), collapse = ",")),
      paste0("表内 ", paste(sort(smp), collapse = ","), if (dup) "（有重复）" else ""), QC))

    add(vchk_fig("figs_qc_counts_valid", if (!bad_num) "pass" else "fail",
      if (!bad_num) "每个样本的细胞数都是有限非负整数" else "存在非整数、负数、无穷或空计数",
      "非负整数", if (bad_num) "存在非法值" else "全部合法", QC))

    add(vchk_fig("figs_qc_total",
      if (!is.na(up_total) && !is.na(total) && total == up_total) "pass"
      else if (is.na(up_total) || is.na(total)) "unknown" else "fail",
      if (is.na(up_total)) "拿不到已验收上游的细胞总数，合计无法绑定"
      else if (is.na(total)) "本表计数有非法值，合计算不了"
      else if (total == up_total) "各样本合计与已验收上游一致"
      else "各样本合计与已验收上游不一致",
      paste0("= ", if (is.na(up_total)) "?" else up_total),
      if (is.na(total)) "?" else as.character(total),
      paste0(QC, " 与已验收 filter 计数")))
  }

  sp <- figs_rows_df(tabs[[SMP]]); gp <- figs_rows_df(tabs[[GRP]])
  need <- c("cell_type", "lvl", "cell_number", "rate")
  if (is.null(sp) || is.null(gp) || !all(need %in% colnames(sp)) || !all(need %in% colnames(gp))) {
    add(vchk_fig("figs_ratio_tables", "unknown",
      "比例表没带回逐行数据或列名不全（需要 cell_type/lvl/cell_number/rate），跨表核对做不了",
      paste(need, collapse = ","), "未取到", paste(SMP, GRP)))
  } else {
    sp$cell_number <- figs_num(sp$cell_number); sp$rate <- figs_num(sp$rate)
    gp$cell_number <- figs_num(gp$cell_number); gp$rate <- figs_num(gp$rate)
    # 先校取值，再汇总：aggregate 遇 NA 默认丢行，会悄悄少算
    sp_bad <- !figs_int_ok(sp$cell_number)
    gp_bad <- !figs_int_ok(gp$cell_number)
    add(vchk_fig("figs_ratio_counts_valid", if (!sp_bad && !gp_bad) "pass" else "fail",
      if (!sp_bad && !gp_bad) "两张比例表的计数都是有限非负整数"
      else "存在非整数、负数或空计数（先拒，不进入汇总）",
      "非负整数", if (sp_bad || gp_bad) "存在非法值" else "全部合法",
      paste(SMP, GRP)))

    # 键完整唯一：样本×类型、组×类型
    sp_n <- figs_norm_sample(sp$lvl)
    gp_l <- trimws(as.character(gp$lvl))
    uniq_ok <- !anyDuplicated(paste(sp_n, sp$cell_type, sep = "\u0001")) &&
               !anyDuplicated(paste(gp_l, gp$cell_type, sep = "\u0001"))
    add(vchk_fig("figs_ratio_unique_key", if (uniq_ok) "pass" else "fail",
      if (uniq_ok) "两张比例表的 (样本/组, 细胞类型) 组合都唯一"
      else "存在重复组合，汇总会重复计数",
      "键唯一", if (uniq_ok) "唯一" else "有重复", paste(SMP, GRP)))

    complete_ok <- length(decl) > 0 && length(expect_types) > 0 &&
      length(sp_n) == length(decl) * length(expect_types) &&
      setequal(unique(sp_n), decl) && setequal(unique(as.character(sp$cell_type)), expect_types)
    add(vchk_fig("figs_ratio_sample_cells_complete",
      if (!length(decl) || !length(expect_types)) "unknown"
      else if (complete_ok) "pass" else "fail",
      if (!length(decl) || !length(expect_types)) "缺声明样本或上游类型集合，完整性核不了"
      else if (complete_ok) "样本×类型的所有组合都在，且各出现一次"
      else "样本×类型组合不全（缺某个样本或某个类型，或某组合重复/缺失）",
      paste0(length(decl), " 样本 × ", length(expect_types), " 类型"),
      paste0(length(sp_n), " 行 / ", length(unique(sp_n)), " 样本 / ",
             length(unique(as.character(sp$cell_type))), " 类型"), SMP))

    # 每样本合计 == QC
    qs <- if (!is.null(q) && all(c("sample", "cell_number") %in% colnames(q)))
      data.frame(lvl = figs_norm_sample(q$sample), qc = figs_num(q$cell_number),
                 stringsAsFactors = FALSE) else NULL
    if (is.null(qs) || sp_bad) {
      add(vchk_fig("figs_ratio_vs_qc", "unknown",
        "QC 不可用或比例表计数非法，比例表与样本计数无法互核",
        "逐样本相等", "无法比对", SMP))
    } else {
      agg <- aggregate(cell_number ~ sp_n, data = sp, FUN = sum)
      colnames(agg) <- c("lvl", "sum_n")
      mg <- merge(agg, qs, by = "lvl", all = TRUE)
      ok <- nrow(mg) > 0 && all(!is.na(mg$sum_n) & !is.na(mg$qc) & mg$sum_n == mg$qc)
      add(vchk_fig("figs_ratio_vs_qc", if (ok) "pass" else "fail",
        if (ok) "每个样本的比例表合计等于 QC 里该样本的细胞数"
        else "存在样本：比例表合计与 QC 不等，或样本对不上",
        "逐样本相等", if (ok) paste(nrow(mg), "个样本全部相等") else "存在不等或缺失",
        paste(SMP, "vs", QC)))
    }

    # 逐类型总数对上游（不只比覆盖）
    if (is.null(type_counts) || !length(type_counts) || sp_bad) {
      add(vchk_fig("figs_ratio_type_totals", "unknown",
        "上游没有逐类型计数或比例表计数非法，逐类型总数无法绑定",
        "逐类型相等", "无法比对", paste(SMP, "与已验收注释计数")))
    } else {
      agg2 <- aggregate(cell_number ~ cell_type, data = sp, FUN = sum)
      uu <- names(type_counts)
      mine <- setNames(as.numeric(agg2$cell_number), as.character(agg2$cell_type))
      want <- suppressWarnings(as.numeric(unlist(type_counts)))
      names(want) <- uu
      miss <- setdiff(uu, names(mine)); extra <- setdiff(names(mine), uu)
      bad <- intersect(uu, names(mine))
      bad <- bad[is.na(mine[bad]) | is.na(want[bad]) | mine[bad] != want[bad]]
      ok3 <- !length(miss) && !length(extra) && !length(bad)
      add(vchk_fig("figs_ratio_type_totals", if (ok3) "pass" else "fail",
        if (ok3) "逐类型细胞总数与已验收注释逐个相等"
        else paste0("逐类型总数不等：",
                    if (length(bad)) paste0("数值不符 ", paste(bad, collapse = ",")) else "",
                    if (length(miss)) paste0(" 缺 ", paste(miss, collapse = ",")) else "",
                    if (length(extra)) paste0(" 多 ", paste(extra, collapse = ",")) else ""),
        "逐类型相等",
        paste0("不符 ", length(bad), " 缺 ", length(miss), " 多 ", length(extra)), SMP))
    }

    # 分组：必须是真实两组，且按 sample→group 映射汇总后逐类型相等
    if (is.null(sgroup) || !length(sgroup) || !length(groups_expect) || gp_bad || sp_bad) {
      add(vchk_fig("figs_ratio_groups", "unknown",
        "缺 sample→group 映射或计数非法，分组归属核不了",
        "两组且按映射相等", "无法比对", GRP))
    } else {
      gset <- setequal(unique(gp_l), groups_expect) && length(groups_expect) >= 2
      add(vchk_fig("figs_ratio_groups", if (gset) "pass" else "fail",
        if (gset) paste0("分组表就是真实的两组：", paste(sort(groups_expect), collapse = " / "))
        else "分组表的组标签与已确认映射不一致（可能换了组或多了组）",
        paste(sort(groups_expect), collapse = " / "),
        paste(sort(unique(gp_l)), collapse = " / "), GRP))

      if (gset) {
        sp$grp <- as.character(sgroup[sp_n])
        expect_g <- aggregate(cell_number ~ grp + cell_type, data = sp, FUN = sum)
        colnames(expect_g) <- c("lvl", "cell_type", "n_expect")
        cmp <- merge(expect_g, gp[, c("lvl", "cell_type", "cell_number")],
                     by = c("lvl", "cell_type"), all = TRUE)
        okg <- nrow(cmp) > 0 && all(!is.na(cmp$n_expect) & !is.na(cmp$cell_number) &
                                     cmp$n_expect == cmp$cell_number)
        add(vchk_fig("figs_ratio_group_mapping", if (okg) "pass" else "fail",
          if (okg) "按 sample→group 映射汇总后，组×类型逐个相等（换组会被这里抓住）"
          else "按映射汇总后与分组表不等，或存在只在一侧出现的组合",
          "逐组逐类型相等", if (okg) paste(nrow(cmp), "组×类型全部相等") else "存在不等",
          paste(SMP, "→", GRP)))
      }
    }

    # rate 与 count/分母自洽（允许两位小数舍入）
    rate_chk <- function(df, denom_of, label) {
      d <- suppressWarnings(as.numeric(denom_of[as.character(df$lvl)]))
      bad_rate <- !is.finite(df$rate) | df$rate < 0 | df$rate > 100
      exp_rate <- suppressWarnings(df$cell_number / d * 100)
      off <- !is.finite(exp_rate) | abs(df$rate - exp_rate) > 0.01
      ok <- !any(bad_rate) && !any(off)
      add(vchk_fig(paste0("figs_rate_", label), if (ok) "pass" else "fail",
        if (ok) "比率都在 0~100 且与 计数/分母 相符（两位小数舍入内）"
        else if (any(bad_rate)) "存在越界或非有限的比率"
        else "比率与 计数/分母 不符（数量对但比例错，图会画错）",
        "rate = count/分母*100 ±0.01",
        paste0("越界 ", sum(bad_rate), " 不符 ", sum(off, na.rm = TRUE)), label))
    }
    if (!sp_bad && !is.null(qs)) {
      den_s <- setNames(qs$qc, qs$lvl)
      sp$lvl <- sp_n
      rate_chk(sp, den_s, "sample")
    } else {
      add(vchk_fig("figs_rate_sample", "unknown", "缺分母或计数非法，样本比率核不了",
                   "rate 自洽", "无法比对", SMP))
    }
    if (!gp_bad && length(groups_expect)) {
      den_g <- if (!is.null(qs) && length(sgroup))
        tapply(qs$qc, as.character(sgroup[qs$lvl]), sum) else NULL
      if (is.null(den_g)) {
        add(vchk_fig("figs_rate_group", "unknown", "缺分母，分组比率核不了",
                     "rate 自洽", "无法比对", GRP))
      } else {
        gp$lvl <- gp_l
        rate_chk(gp, den_g, "group")
      }
    } else {
      add(vchk_fig("figs_rate_group", "unknown", "缺分母或计数非法，分组比率核不了",
                   "rate 自洽", "无法比对", GRP))
    }
  }

  # ---- 3 跳过表：按 status 分别核 ----
  sk <- tabs[[SKIP]]
  if (!all(c("cell_type", "status", "n_group1", "n_group2", "reason") %in% sk$columns)) {
    add(vchk_fig("figs_skip_table", "fail",
      "跳过表缺表头列，无法核对跳过规则",
      "cell_type,status,n_group1,n_group2,reason", paste(sk$columns, collapse = ","), SKIP))
  } else if (!length(sk$rows %||% list())) {
    add(vchk_fig("figs_skip_table", "pass",
      "跳过表有表头、没有任何跳过记录（本次没有类型被跳过）", ">= 0 行", "0 行", SKIP))
  } else {
    sdf <- figs_rows_df(sk)
    sdf$n_group1 <- figs_num(sdf$n_group1); sdf$n_group2 <- figs_num(sdf$n_group2)
    stv <- trimws(as.character(sdf$status)); rs <- trimws(as.character(sdf$reason))
    allowed <- c("skipped_lt3", "no_sig")
    bad_stat <- !stv %in% allowed
    bad_reason <- !nzchar(rs)
    bad_cnt <- !figs_int_ok(sdf$n_group1) || !figs_int_ok(sdf$n_group2)

    # lt3 的两组计数必须与分组表该类型真实计数一致（顺序无关）
    lt3_idx <- which(stv == "skipped_lt3")
    lt3_ok <- TRUE; lt3_why <- ""
    if (length(lt3_idx) && !is.null(gp) && all(c("cell_type", "lvl", "cell_number") %in% colnames(gp))) {
      for (i in lt3_idx) {
        ct <- as.character(sdf$cell_type[i])
        sub <- gp[as.character(gp$cell_type) == ct, , drop = FALSE]
        if (nrow(sub) != 2) { lt3_ok <- FALSE; lt3_why <- paste0(ct, " 分组表里不是两组"); next }
        real <- sort(suppressWarnings(as.numeric(sub$cell_number)))
        mine <- sort(c(sdf$n_group1[i], sdf$n_group2[i]))
        if (!identical(real, mine)) {
          lt3_ok <- FALSE
          lt3_why <- paste0(ct, " 申报 ", paste(mine, collapse = "/"),
                            " 实际 ", paste(real, collapse = "/"))
        } else if (min(real) >= 3) {
          lt3_ok <- FALSE; lt3_why <- paste0(ct, " 两组都不小于 3 却写 lt3")
        }
      }
    } else if (length(lt3_idx)) {
      lt3_ok <- FALSE; lt3_why <- "分组表不可用，lt3 计数无法核对"
    }

    ok_skip <- !any(bad_stat) && !any(bad_reason) && !bad_cnt && lt3_ok
    add(vchk_fig("figs_skip_table", if (ok_skip) "pass" else "fail",
      if (ok_skip) paste0("跳过记录合规（", length(stv), " 条；",
                          sum(stv == "skipped_lt3"), " 条 lt3，",
                          sum(stv == "no_sig"), " 条 no_sig）")
      else paste0("跳过记录不合规：",
                  if (any(bad_stat)) paste0("状态非法(", paste(unique(stv[bad_stat]), collapse = ","), ")") else "",
                  if (any(bad_reason)) " 缺原因" else "",
                  if (bad_cnt) " 计数非整数" else "",
                  if (!lt3_ok) paste0(" ", lt3_why) else ""),
      "status ∈ {skipped_lt3,no_sig}；lt3 与分组表两计数相等且 min<3；error 拒",
      paste0(length(stv), " 条"), SKIP))
  }

  out
}