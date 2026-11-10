#!/usr/bin/env Rscript
# =====================================================================
#  dataset_profile.R -- 这批数据是什么，以及由此决定每环用什么参数
#
#  为什么需要这一步
#    现在换一批数据，实际情况是「人在对话里让助手改脚本」。
#    那不是适配，那是每次重新手写一遍。要真做到「换个数据还能跑」，
#    第一步必须先把「这批数据是什么」写成一个可核的文件，再让参数从它推出来。
#
#  三条规矩
#    1. 只填能从可核来源读到的东西。读不到就留空，不猜。
#       —— 猜出来的物种会让一整轮结果错得看不出来（拿小鼠参数跑人类数据）。
#    2. 填不齐就停在闸门问人，不自己补一个默认值。
#    3. 生成结果与模板的差异必须留档。
#       —— 不记差异，事后没人能回答「这次为什么 dims=20 而不是 30」。
#
#  数据从哪来
#    place    会话里填的服务器位置（data_root / script_root / work_root）
#    identify 上次识别结论（含样本数与链接检查）
#    env      体检结果（remote_env.json 里的 samples 段最准）
#    ref      选中的参考表（注释环要看它的物种与组织）
#  这四处都是「已经存在、且是别人核过的」，不新增采集动作。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

DP_RULE_VERSION <- "dataset-profile-1.0"

# ---- 从各处已有的东西里抽画像 ----------------------------------------

# samples: 优先用体检里的（它是真的列过目录），其次识别结论
dp_samples <- function(env = NULL, identify = NULL) {
  s <- env$samples %||% NULL
  if (!is.null(s) && length(s$names)) {
    return(list(count = as.integer(s$count %||% length(s$names)),
                names = as.character(s$names),
                source = "env_check", note = as.character(s$note %||% "")))
  }
  st <- identify$result %||% NULL
  if (!is.null(st) && length(st$samples)) {
    nm <- as.character(st$samples)
    return(list(count = length(nm), names = nm, source = "identify", note = ""))
  }
  list(count = NA_integer_, names = character(0), source = "unknown",
       note = "没有可核的样本清单")
}

# ---- 物种与组织：只认「人明确给过的」，不从路径/文件名推 ----------------
#
#  2026-11-04 加的一道防线。为什么要加
#    261026测试1 这条分析的组织原来被记成 Spleen。核过一遍：
#      · 三个历史备份里 species/tissue 都是空的；
#      · 唯一能把值写进去的入口是 /api/sessions/update（页面上那个「记下」）；
#      · 而数据目录名是 …/SY_wangwenbiao_spleen_mouse_20250521/…。
#    也就是说：有人（包括我自己）看到路径里有 spleen，就把它当成了
#    「这批数据是脾脏」的证据，然后写进了分析记录。
#    路径名是项目的历史命名，跟这批数据取的是什么组织没有必然关系。
#    这个错误的代价很实在：后面每一次提到这批数据，都会带上"脾脏"，
#    注释依据、报告措辞、PPT 标题全都跟着错，而错的地方看起来很正常。
#
#  规则三条
#    ① 只在人能明确给出的地方取值：分析记录里标了 user-confirmed 的优先，
#       其次是参考表自己声明的。其余一律留空。
#    ② 不从 data_root / liny_data / work_root / 文件名里取任何组织或物种词。
#       哪怕路径里明明白白写着 spleen，也不等于这批数据是脾脏。
#    ③ 留空不是缺陷。留空会走"要人答"的那条路；猜一个值则会让整条链
#       带着错前提跑到底，而且没人会回头核。
dp_species <- function(ref = NULL, sess = NULL, corpus = "") {
  # sess 可能是两种形状：分析记录本体，或者 sess_state() 的返回
  #   （{ok, session, profile, sessions, current}）。
  #   这两种都要能取到物种与组织 —— 否则画像会把"人已经确认过的"
  #   当成"没有来源"，于是又变成"不知道"，白费了人填的那一次。
  ss <- sess %||% list()
  if (!is.null(ss$session) && is.list(ss$session)) ss <- ss$session
  # ① 人确认过的，优先级最高
  ssp <- trimws(as.character(ss$species %||% ""))
  sti <- trimws(as.character(ss$tissue %||% ""))
  ssrc <- as.character(ss$species_tissue_source %||% "")
  if (nzchar(ssp) && identical(ssrc, "user-confirmed"))
    return(list(value = ssp, tissue = if (nzchar(sti)) sti else NA_character_,
                source = "confirmed",
                # 确认过的不算"可疑"，但要单独记下"路径里也出现了同样的字"
                #   —— 这是两个不同的事实，不能混成一个字段。
                #   混在一起会出现这种误导：审计里一条"已确认"的值被标成可疑，
                #   看的人以为它有问题，而它恰恰是唯一核对过的那条。
                #   matches_path  = 客观：这个字确实出现在路径里
                #   suspect_from_path = 判断：没确认过，且只出现在路径里
                suspect_from_path = FALSE,
                matches_path = dp_value_in_path(sti, corpus) || dp_value_in_path(ssp, corpus),
                why = paste0("分析记录里由人确认过：", ssp, " / ", if (nzchar(sti)) sti else "未标组织")))

  # ② 参考表自己声明的
  if (!is.null(ref)) {
    rsp <- trimws(as.character(ref$species %||% ""))
    rti <- trimws(as.character(ref$tissue %||% ""))
    if (nzchar(rsp))
      return(list(value = rsp, tissue = if (nzchar(rti)) rti else NA_character_,
                  source = "reference",
                  suspect_from_path = FALSE,
                  matches_path = FALSE,
                  why = paste0("取自参考表自身标注：", rsp, " / ", if (nzchar(rti)) rti else "未标组织")))
  }

  # ③ 都没有：留空。注意这里**不**去读 place 里的任何路径。
  #   见本函数头部的说明：路径名不是证据。
  if (nzchar(ssp)) {
    # ---- 历史数据兜底：没有来源标记的值，一律按"未确认"处理 --------------
    #   为什么必须有这一段（2026-11-04）
    #     sources 标记是这次才加的。之前写进去的值没有标记，
    #     它们当中可能有从路径猜来的 —— 261026测试1 的 Spleen 就是。
    #     如果只认标记、不认历史，那些旧值会被当成"没有来源"而继续用，
    #     等于把同一个错误留在数据里等下一次踩。
    #   这里不删值（删了等于替人做决定），但把它降级成"未确认"，
    #     并标出它是否只出现在路径里 —— 让页面和下游都能看到这条不确定。
    susp <- dp_value_in_path(sti, corpus) || dp_value_in_path(ssp, corpus)
    return(list(value = ssp, tissue = if (nzchar(sti)) sti else NA_character_,
                source = "unverified",
                suspect_from_path = susp,
                matches_path = susp,
                why = paste0(
                  "分析记录里填过但没有确认标记",
                  if (susp) "，而且这个值只出现在数据路径里 —— 很可能当时是从目录名抄来的" else "",
                  "。页面上的「记下」按一次即可确认。")))
  }

  # 完全没有值：顺便把"路径里看着像组织/物种"的词报出来，
  #   让填的人知道哪些是路径里抄来的、不要直接采信。
  list(value = NA_character_, tissue = NA_character_, source = "unknown",
       suspect_from_path = FALSE,
       matches_path = FALSE,
       path_hints = list(tissue = dp_tissue_candidates(corpus, "tissue"),
                         species = dp_tissue_candidates(corpus, "species")),
       why = "没有可核来源：既不是人确认的，参考表也没标。不猜 —— 路径名与文件名不能当证据")
}

# ---- 路径重查：这个值是不是从路径里"看起来合理"的 ----------------------
#
#  2026-11-04。为什么要单独一组函数
#    只把错值改掉解决不了下一次。同类错误的成因是固定的：
#    数据目录名里带着组织/物种字样（…/SY_wangwenbiao_spleen_mouse_20250521/…），
#    看到路径里写着 spleen，就把它当成"这批数据是脾脏"的证据。
#    要防住下一次，就得让"写入的值"和"路径里的字样"之间有一道强制核对，
#    而不是靠人记得别猜。
#
#  三个用途
#    dp_path_corpus()      把一批数据相关的所有路径收成一份语料
#    dp_value_in_path()    值是否出现在这份语料里（只做证据比对，不判断对错）
#    dp_tissue_candidates() 从语料里能"读出来"的组织/物种词 —— 只用于提示，
#                           永不自动采用。列出来是为了让人一眼看到
#                           "这个值可能是从路径里抄来的"。

# 常见的组织与物种词表。只用于"路径里出现了哪些可疑词"的提示。
#   为什么不用大词表：词表越大误报越多，而误报会让人忽略提示。
#   这里只收这批实验常出现的，其余靠值是否出现在路径里判断。
DP_PATH_WORDS <- list(
  tissue = c("spleen", "kidney", "liver", "lung", "heart", "brain", "blood",
             "bone", "muscle", "skin", "colon", "intestine", "stomach",
             "pancreas", "thymus", "ovary", "testis", "placenta", "retina",
             "脾", "肾", "肝", "肺", "心", "脑", "血"),
  species = c("human", "mouse", "mice", "rat", "monkey", "pig", "zebrafish",
              "人", "小鼠", "大鼠", "猴", "猪")
)

# 把一批数据相关的路径收成一份语料（小写）。
#   收 data_root / liny_data / script_root / work_root / 结果目录。
dp_path_corpus <- function(place = NULL, extra = character(0)) {
  srv <- (place %||% list())$server %||% list()
  ps <- c(
    as.character(srv$data_root %||% ""),
    as.character(srv$liny_data %||% ""),
    as.character(srv$script_root %||% ""),
    as.character(srv$work_root %||% ""),
    as.character(extra %||% character(0))
  )
  ps <- ps[nzchar(ps)]
  tolower(paste(ps, collapse = " "))
}

# 值是否作为"独立词"出现在路径语料里。
#
#  2026-11-04 第二版。第一版用正则做词边界，写错了：
#    想把值里的特殊字符转义，结果 []{} 在 R 的正则里本身有含义，
#    拼出来的模式直接报 "Invalid contents of {}"。
#  现在改成「先分词、再等值比较」，完全不碰正则转义：
#    按非字母/数字/汉字切开，切成一个个词，再看有没有完全相同的那个词。
#    这比正则更可靠 —— 组织名和物种名都是普通词，不需要复杂模式。
#  为什么要词边界：组织名都是短词（如 rat），
#    纯子串匹配会把 "generate"、"separate" 也算成命中，那种误报会让人忽略提示。
dp_value_in_path <- function(value, corpus = "") {
  v <- tolower(trimws(as.character(value %||% "")))
  if (!nzchar(v) || !nzchar(corpus)) return(FALSE)
  # 分词：非「字母 / 数字 / 汉字」的一切字符都算分隔符。
  #   英文靠空格与标点自然切开；中文没有空格，一串连续汉字会成为一个词。
  split_tok <- function(x) {
    t <- unlist(strsplit(x, "[^a-z0-9\u4e00-\u9fa5]+"))
    t[nzchar(t)]
  }
  v_tok <- split_tok(v)
  if (!length(v_tok)) return(FALSE)
  c_tok <- split_tok(corpus)
  if (!length(c_tok)) return(FALSE)

  # ---- 英文与中文必须用两条不同的规则 --------------------------------
  #   英文用「整词相等」：
  #     组织名都是短词（rat / pig / bone），子串匹配会把
  #     generate、separate、bone2 也算命中。这种误报一旦多了，
  #     人会很快学会忽略这条提示 —— 那这道检查就白加了。
  #   中文用「允许包含」：
  #     中文没有空格，人填「脾」、路径写的是「脾脏」，
  #     整词相等永远对不上，那是漏拦。漏拦比误报严重：
  #     误报只是多确认一次，漏拦等于这道检查根本不存在。
  #   实测（2026-11-04）：
  #     dp_value_in_path("Spleen", "x/sy_spleen_mouse/y") → TRUE
  #     dp_value_in_path("rat",    "/data/generate/x")     → FALSE
  #     dp_value_in_path("脾",     "/数据/小鼠脾脏/2025")   → TRUE
  has_cjk <- function(x) grepl("[\u4e00-\u9fa5]", x)
  for (a in v_tok) {
    if (a %in% c_tok) return(TRUE)
    if (has_cjk(a) && any(grepl(a, c_tok, fixed = TRUE))) return(TRUE)
  }
  FALSE
}

# 从路径里能"读出来"的组织/物种词。只用于提示，绝不自动采用。
dp_tissue_candidates <- function(corpus = "", kind = c("tissue", "species")) {
  kind <- match.arg(kind)
  words <- DP_PATH_WORDS[[kind]]
  hit <- words[vapply(words, function(w) dp_value_in_path(w, corpus), logical(1))]
  unique(hit)
}

# 矩阵格式与三件套
dp_matrix <- function(env = NULL) {
  s <- env$samples %||% NULL
  n <- suppressWarnings(as.integer(s$matrix_files_each %||% NA))
  if (is.na(n)) return(list(format = NA_character_, files_each = NA_integer_,
                            source = "unknown", why = "体检没列矩阵文件数"))
  # 10x 三件套 = barcodes / features / matrix
  fmt <- if (n >= 3L) "10x_mtx" else "unknown"
  list(format = fmt, files_each = n, source = "env_check",
       why = if (identical(fmt, "10x_mtx")) "每个样本 3 个文件，符合 10x 三件套"
             else paste0("每个样本 ", n, " 个文件，不是标准三件套"))
}

# 运行环境（R 版本、库路径、队列、节点约束）
dp_runtime <- function(env = NULL) {
  rt <- env$runtime %||% NULL
  if (is.null(rt)) return(list(source = "unknown", why = "体检没带 runtime 段"))
  list(rscript = as.character(rt$rscript %||% NA),
       lib_paths = as.character(rt$lib_paths %||% character(0)),
       queue = as.character(rt$queue %||% NA),
       node = as.character(rt$node_constraint %||% NA),
       source = "env_check", why = "取自服务器体检")
}

# 组装画像
#   返回 list(ok, profile, missing) —— missing 是「必须人答、不能猜」的项
dp_build <- function(place = NULL, identify = NULL, env = NULL, ref = NULL) {
  smp <- dp_samples(env, identify)
  srv0 <- (place %||% list())$server %||% list()
  # 路径语料：把数据侧所有路径收成一份，供"这个值是不是从路径抄来的"比对。
  #   为什么在这里生成一次：三个下游函数（dp_species、审计、页面提示）要用同一份，
  #   各自去拼容易拼出不一样的字符串，比对结论就不一致了。
  corpus <- dp_path_corpus(place)
  sp <- dp_species(ref, sess = identify, corpus = corpus)
  mx <- dp_matrix(env)
  rt <- dp_runtime(env)
  srv <- srv0

  missing <- list()
  if (is.na(smp$count) || !length(smp$names))
    missing[[length(missing) + 1L]] <- list(
      key = "samples", why = "样本清单读不到",
      ask = "这批数据有哪些样本？把样本名按顺序给我，或者让执行端列一次目录。")
  if (is.na(sp$value))
    missing[[length(missing) + 1L]] <- list(
      key = "species", why = "物种没确定",
      ask = "这批数据是什么物种（人 / 小鼠 / 大鼠）？这个不能猜 —— 猜错不会报错，只会拿错参数跑一遍。")
  if (is.na(mx$format) || !identical(mx$format, "10x_mtx"))
    missing[[length(missing) + 1L]] <- list(
      key = "matrix_format", why = mx$why,
      ask = "矩阵是什么格式？每个样本有哪些文件？")

  prof <- list(
    rule_version = DP_RULE_VERSION,
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    species = sp$value, tissue = sp$tissue %||% NA_character_,
    species_source = sp$source, species_why = sp$why,
    # 这两个字段是给页面和审计用的：一个说"这个值可能来自路径"，
    # 一个把路径里能读出哪些组织/物种词列出来（只提示，不采用）。
    suspect_from_path = isTRUE(sp$suspect_from_path),
    matches_path = isTRUE(sp$matches_path),
    path_hints = sp$path_hints %||% NULL,
    sample_count = smp$count, samples = smp$names,
    samples_source = smp$source, samples_note = smp$note,
    matrix_format = mx$format, matrix_files_each = mx$files_each,
    matrix_source = mx$source, matrix_why = mx$why,
    runtime = rt,
    data_root = as.character(srv$data_root %||% NA_character_),
    script_root = as.character(srv$script_root %||% NA_character_),
    work_root = as.character(srv$work_root %||% NA_character_),
    reference_id = as.character((ref %||% list())$id %||% NA_character_),
    reference_digest = as.character((ref %||% list())$digest %||% NA_character_),
    missing = missing,
    ok = length(missing) == 0L
  )
  prof
}

# ---- 参数：从画像推出来，并与模板对账 ---------------------------------

#  template 是 pipeline.default.json 那份结构（每字段 list(value, source, ...)）
#  返回 list(params, diffs, blocked)
#    diffs    画像导致的和模板默认值不一样的每一项，含依据
#    blocked  填不齐、必须人答的项（闸门用）
dp_resolve_params <- function(profile, template, gates = list()) {
  params <- template
  diffs <- list()
  blocked <- list()

  setp <- function(path, value, source, why) {
    parts <- strsplit(path, ".", fixed = TRUE)[[1]]
    cur <- params
    for (i in seq_along(parts)) {
      k <- parts[i]
      if (i == length(parts)) {
        old <- cur[[k]]$value
        if (!identical(old, value)) {
          diffs[[length(diffs) + 1L]] <<- list(
            path = path, old = old, new = value, source = source, why = why)
        }
        cur[[k]]$value <- value
        cur[[k]]$source <- source
        if (nzchar(why %||% "")) cur[[k]]$why <- why
      } else {
        cur[[k]] <- cur[[k]] %||% list()
        cur <- cur[[k]]
      }
    }
    params <<- params
    invisible(TRUE)
  }

  # 物种：画像有就写进去，没有就停在闸门
  if (!is.na(profile$species %||% NA) && nzchar(profile$species)) {
    # 模板里写的是 "Mouse" / "Human" / "Rat"，画像里可能来自参考表的中文
    norm <- c(mouse = "Mouse", 小鼠 = "Mouse", human = "Human", 人 = "Human",
              rat = "Rat", 大鼠 = "Rat")
    v <- norm[[tolower(as.character(profile$species))]]
    if (is.null(v)) v <- as.character(profile$species)
    setp("params.species", v, "dataset_profile",
         paste0("来自画像：", profile$species_why %||% ""))
  } else {
    blocked[[length(blocked) + 1L]] <- list(
      key = "species", path = "params.species",
      why = "画像里没有物种，不能拿模板默认值顶上")
  }

  # 样本数：决定了去双要产出几份
  if (!is.na(profile$sample_count %||% NA)) {
    setp("io.sample_count", as.integer(profile$sample_count), "dataset_profile",
         paste0("来自画像：", profile$samples_source, " 核到 ", profile$sample_count, " 个样本"))
  } else {
    blocked[[length(blocked) + 1L]] <- list(
      key = "samples", path = "io.sample_count", why = "样本数读不到")
  }

  # 路径：画像里有就填，没有就等闸门
  for (nm in c(indir = "data_root", outdir = "work_root")) {
    key <- names(nm); p <- nm[[1]]
    val <- profile[[p]] %||% NA_character_
    if (!is.na(val) && nzchar(val)) setp(paste0("io.", key), val, "dataset_profile",
                                          "来自位置设置")
    else blocked[[length(blocked) + 1L]] <- list(
      key = key, path = paste0("io.", key), why = "位置设置里没填")
  }

  # 人已经答过的闸门值优先于模板默认值
  for (g in gates %||% list()) {
    k <- as.character(g$key %||% "")
    if (!nzchar(k)) next
    if (!is.null(g$path) && !is.null(g$value)) setp(as.character(g$path), g$value,
                                                    "gate", "人在闸门里定的")
  }

  # 模板里 source=gate 且值为空的，全部列进 blocked
  walk_gate <- function(o, prefix) {
    for (k in names(o)) {
      v <- o[[k]]
      if (is.list(v) && !is.null(v$source)) {
        if (identical(v$source, "gate") &&
            (is.null(v$value) || length(v$value) == 0L ||
             (length(v$value) == 1L && (is.na(v$value) || !nzchar(as.character(v$value)))))) {
          blocked[[length(blocked) + 1L]] <<- list(
            key = paste0(prefix, k), path = paste0(prefix, k),
            why = "模板把这一项标成「等人定」，现在还空着")
        }
      } else if (is.list(v) && is.null(v$source)) {
        walk_gate(v, paste0(prefix, k, "."))
      }
    }
  }
  walk_gate(template, "")

  # 去重：同一 path 只留一条，优先保留带 key 的那条
  seen <- character(0); uniq <- list()
  for (b in blocked) {
    if (b$path %in% seen) next
    seen <- c(seen, b$path)
    uniq[[length(uniq) + 1L]] <- b
  }

  list(params = params, diffs = diffs, blocked = uniq,
       ok = length(uniq) == 0L)
}

# Read only evidence saved on the requested record, never the latest global probe.
dp_record <- function(app_dir, sid = "", con = NULL, version_id = NULL) {
  owner <- NULL
  if (!is.null(version_id)) {
    rows <- tryCatch(DBI::dbGetQuery(con,
      "SELECT session_id, owner_key FROM sessions WHERE version_id = ?",
      params = list(version_id)), error = function(e) NULL)
    if (is.null(rows) || nrow(rows) != 1L) return(NULL)
    sid <- as.character(rows$session_id[1])
    owner <- as.character(rows$owner_key[1])
  }
  if (length(sid) != 1L || is.na(sid) || !nzchar(sid)) return(NULL)
  if (is.null(owner) && !is.null(con)) {
    rows <- tryCatch(DBI::dbGetQuery(con,
      "SELECT owner_key FROM sessions WHERE session_id = ?", params = list(sid)),
      error = function(e) NULL)
    if (!is.null(rows) && nrow(rows) == 1L) owner <- as.character(rows$owner_key[1])
  }
  root <- if (exists("state_private_root", mode = "function"))
    state_private_root(app_dir, owner) else file.path(app_dir, "state")
  f <- file.path(root, "sessions.json")
  data <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  for (s in data$sessions %||% list())
    if (identical(as.character(s$id %||% ""), sid)) return(s)
  NULL
}

dp_record_env <- function(app_dir, sid = "", con = NULL, version_id = NULL) {
  dp_record(app_dir, sid, con, version_id)$identify$result
}

# ---- 落盘 ------------------------------------------------------------

dp_dir <- function(app_dir, owner_key = "local:default") {
  ok <- gsub("[^A-Za-z0-9_.:@-]", "_", as.character(owner_key %||% "local:default"))
  d <- file.path(app_dir, "state", "users", ok, "dataset")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

dp_save <- function(app_dir, profile, resolved = NULL, owner_key = "local:default") {
  d <- dp_dir(app_dir, owner_key)
  f <- file.path(d, "dataset_profile.json")
  writeLines(jsonlite::toJSON(profile, auto_unbox = TRUE, pretty = TRUE,
                              null = "null", digits = NA), f, useBytes = TRUE)
  out <- list(path = f)
  if (!is.null(resolved)) {
    # 差异必须留档：事后要能回答「这次为什么和默认值不一样」。
    fd <- file.path(d, paste0("params-diff-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".json"))
    writeLines(jsonlite::toJSON(list(
      at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      rule_version = DP_RULE_VERSION,
      diffs = resolved$diffs,
      blocked = resolved$blocked,
      ok = resolved$ok), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA),
      fd, useBytes = TRUE)
    fp <- file.path(d, "params.resolved.json")
    writeLines(jsonlite::toJSON(resolved$params, auto_unbox = TRUE, pretty = TRUE,
                                null = "null", digits = NA), fp, useBytes = TRUE)
    out$diff_path <- fd
    out$params_path <- fp
  }
  out
}
