# =====================================================================
#  注释依据（参考表 / marker 词典）的管理
# ---------------------------------------------------------------------
#  gina 2026-10-29 的原话：
#    「在我们后续的其他处理环节中，参考表应该是需要人为上传的吧？
#      不同的细胞类型或者不同的组织样本，它针对的参考表或者注释依据
#      应该是不一样的。即使这一次的参考表在服务器上已经有，
#      但后期如果要处理其他的细胞数据，这个参考表就应该要人工手动上传。
#      你没有提供这样的入口，这个应用怎么可能复用到其他的细胞处理过程中呢？」
#
#  所以这里不做"内置一份官方表就完了"的事。三件事：
#    ① 上传：人把这次要用的参考表传进应用，应用保存并算指纹。
#    ② 选用：分析记录里记下"这一次用哪一份"，不是全局一份。
#    ③ 下发：执行端按这份的路径去服务器取/落盘，跑完带指纹回来。
#
#  为什么不直接用服务器上那份现成的：
#    那份属于 liny 的另一个项目，是 6 样本那批的注释依据。
#    这一轮数据、组织、分组都可能不同；沿用别人的表等于用错依据，
#    而错依据注释出来的细胞类型是对的格式、错的内容 —— 事后极难发现。
# =====================================================================

REF_DIR <- file.path(STATE_DIR, "references")

ref_index_file <- function() file.path(REF_DIR, "index.json")

ref_index_read <- function() {
  f <- ref_index_file()
  if (!file.exists(f)) return(list(items = list()))
  d <- tryCatch(jsonlite::fromJSON(read_text_any(f), simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(d) || is.null(d$items)) return(list(items = list()))
  d
}

ref_index_write <- function(d) {
  dir.create(REF_DIR, showWarnings = FALSE, recursive = TRUE)
  d$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  write_json_file(ref_index_file(), d)
  invisible(d)
}

# 保存一份上传的参考表。
#   b64    : 文件内容的 base64（前端读成 base64 传上来，避开 multipart 解析）
#   name   : 原始文件名
#   note   : 这份表适用于什么（组织/物种/细胞类型范围），必填
#            为什么必填：以后换数据时，人要能一眼分清"哪份适用于哪批"。
#            没有说明的表，一个月后没人敢用。
ref_save <- function(name, b64, note = "", species = "", tissue = "") {
  nm <- basename(as.character(name %||% ""))
  if (!nzchar(nm)) return(list(ok = FALSE, error = "缺文件名"))
  ext <- tolower(tools::file_ext(nm))
  if (!ext %in% c("xlsx", "xls", "tsv", "csv", "json")) {
    return(list(ok = FALSE, error = paste0("不支持的类型 .", ext,
                                           "（只收 xlsx / xls / tsv / csv / json）")))
  }
  if (!nzchar(trimws(as.character(note %||% "")))) {
    return(list(ok = FALSE, error = "必须写一句这份表适用于什么（组织 / 物种 / 细胞范围），否则以后没人敢用"))
  }

  dir.create(REF_DIR, showWarnings = FALSE, recursive = TRUE)
  raw <- tryCatch(jsonlite::base64_dec(as.character(b64 %||% "")), error = function(e) NULL)
  if (is.null(raw) || !length(raw)) return(list(ok = FALSE, error = "文件内容为空或编码不是 base64"))
  if (length(raw) > 30 * 1024 * 1024) return(list(ok = FALSE, error = "文件超过 30MB，拒绝"))

  id <- paste0("ref-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
               paste0(sample(c(letters, 0:9), 4, replace = TRUE), collapse = ""))
  keep <- paste0(id, ".", ext)
  fp <- file.path(REF_DIR, keep)
  con <- file(fp, open = "wb")
  writeBin(raw, con)
  close(con)

  digest_now <- unname(tools::md5sum(fp))

  # ---- 内容去重（gina 2026-11-19）-------------------------------------
  #   她的原话：「同一个文件要检查里面的内容，完全一致就只显示这一份，
  #   如果不一致就存成新的一份」。
  #
  #   原来是每传一次就新生成一个 ID、新落一份文件、新加一条索引 ——
  #   同一个表传三次就在列表里出现三条，磁盘上三份，看不出哪份算数。
  #
  #   现在：先算内容的 md5（不是文件名），拿它跟已登记的条目比。
  #   完全一致 -> 复用那一条，不落新文件；只把这次填的用途说明与物种组织
  #   补进去（空的不覆盖已有值），并把 original_name 记下传过的文件名。
  #   不一致 -> 照旧落新的一份。
  #   为什么按内容而不是文件名：同一个表换个名字传上来还是同一份，
  #   而两个不同版本的表常常同名。按名字判会两头都错。
  d <- ref_index_read()
  dup <- NULL
  for (x in (d$items %||% list())) {
    if (!identical(tolower(as.character(x$digest %||% "")), tolower(digest_now))) next
    # 服务器来源的条目没有本地副本；内容一致时优先复用真正在本地的那一份。
    if (identical(as.character(x$source %||% ""), "server") &&
        !nzchar(as.character(x$stored %||% "")) && is.null(dup)) {
      dup <- x
      next
    }
    dup <- x
    break
  }

  if (!is.null(dup)) {
    # 复用：删掉刚写的那份临时副本，改成更新原条目。
    unlink(fp)
    idx <- NA_integer_
    for (i in seq_along(d$items)) {
      if (identical(as.character(d$items[[i]]$id %||% ""), as.character(dup$id %||% ""))) {
        idx <- i; break
      }
    }
    if (!is.na(idx)) {
      it <- d$items[[idx]]
      it$note <- as.character(note)
      if (nzchar(as.character(species %||% ""))) it$species <- as.character(species)
      if (nzchar(as.character(tissue %||% ""))) it$tissue <- as.character(tissue)
      # 传过的文件名都留着：同一个内容可能被叫过好几个名字，
      #   以后按名字找它时不至于找不到。
      prev <- as.character(unlist(it$aliases %||% list()))
      prev <- unique(c(prev, as.character(it$name %||% "")))
      if (!(nm %in% prev)) prev <- c(prev, nm)
      it$aliases <- as.list(prev[nzchar(prev)])
      it$reused_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      it$reuse_count <- as.integer(it$reuse_count %||% 0L) + 1L
      d$items[[idx]] <- it
      ref_index_write(d)
      return(list(ok = TRUE, item = it, duplicate = TRUE, reused = TRUE,
                  message = paste0("这份表的内容与已登记的「", as.character(it$name %||% ""),
                                   "」完全一致，已复用那一条，没有多存一份。")))
    }
  }

  item <- list(
    id = id,
    name = nm,
    stored = keep,
    bytes = length(raw),
    # 字段名就叫 digest。用 md5（与守卫同一套），不号称 sha256 ——
    #   把 md5 标成 sha256 是"看起来更安全"的谎，校验逻辑按名字走就会错。
    digest = digest_now,
    note = as.character(note),
    species = as.character(species %||% ""),
    tissue = as.character(tissue %||% ""),
    source = "uploaded",          # uploaded = 人传上来的；server = 服务器上已有的
    server_path = "",             # source=server 时，服务器上的绝对路径
    uploaded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))

  d$items[[length(d$items) + 1]] <- item
  ref_index_write(d)
  list(ok = TRUE, item = item, duplicate = FALSE)
}

# ---- 这份依据适不适用于这批数据 -------------------------------------
# ---- 这份表的来源与适用范围：只陈述，不评判 -------------------------
#
#  2026-11-04 重写。为什么
#    这段原来是"适用性判定"，产出 ok / cross_species / tissue_note / unknown，
#    本意是提醒人"这份表是别的物种/别的组织"。但它带来两个实际危害：
#      ① 它把物种与组织写成了"能不能用"的判断，而这件事本来就该由人定。
#         跨物种注释在研究里是常规做法：把人的细胞移植到小鼠体内做实验，
#         后期取到的细胞既有人源的、也有鼠源的；人和小鼠的多数细胞类型
#         判定基因本身就是一对一同源基因（人 CD3D / 鼠 Cd3d），
#         符号只差大小写，脚本比对时已统一成大写，照样对得上。
#         这里不存在任何安全性问题，也没有需要拦截的伦理或合规风险。
#      ② 更糟的是它的措辞会被下游当成限制。指令里写一句"换物种就要换一份"，
#         执行端就会得出"这张人的表不能用于这批小鼠数据"，于是整条链停住 ——
#         工作做不下去，而原因只是一个措辞。
#    所以现在只做一件事：如实说明表里标了什么、这批数据标了什么，摆在人面前。
#    用不用、怎么用，由人选。选了就严格执行，不二次质疑。
#
#  唯一保留的判定：表的身份信息是否完整（能不能说清它是什么）。
#    缺信息不是"不适用"，只是"这份表没写清自己是什么"。
#    这种情况也照用 —— 只是结果解读时得知道依据的边界在哪。
ref_applicability <- function(item, species = "", tissue = "") {
  it <- item %||% list()
  isp <- trimws(as.character(it$species %||% ""))
  its <- trimws(as.character(it$tissue %||% ""))
  wsp <- trimws(as.character(species %||% ""))
  wts <- trimws(as.character(tissue %||% ""))

  norm <- function(x) tolower(gsub("[^a-z]", "", tolower(trimws(x))))
  same <- function(a, b) {
    if (!nzchar(a) || !nzchar(b)) return(FALSE)
    grepl(norm(a), norm(b), fixed = TRUE) || grepl(norm(b), norm(a), fixed = TRUE)
  }

  base <- list(
    blocking = FALSE,          # 永远不拦。这个字段保留只为兼容旧调用方。
    table_species = isp, table_tissue = its,
    data_species = wsp, data_tissue = wts)

  # 表里什么都没标：不是问题，只是说明依据的边界看不清。
  if (!nzchar(isp) && !nzchar(its))
    return(c(base, list(
      verdict = "unlabeled",
      label = "未标注适用范围",
      why = "这份表没写适用物种与组织。可以直接用；只是结果解读时要知道依据的边界没写清。")))

  # 物种与组织都和这批数据写得一致
  if ((!nzchar(isp) || same(isp, wsp)) && (!nzchar(its) || same(its, wts)))
    return(c(base, list(
      verdict = "ok",
      label = paste(c(isp, its)[nzchar(c(isp, its))], collapse = " / "),
      why = paste0("表标注为「",
                   paste(c(isp, its)[nzchar(c(isp, its))], collapse = " / "),
                   "」，与这批数据一致。"))))

  # 其余情况：如实并列写出两边，不判"能不能用"。
  #   为什么不分"跨物种"和"组织不同"两种 verdict：
  #   分了就会被下游当成两种不同的问题去处理，而它们在本应用里的处理方式
  #   完全相同 —— 都是"照人选的用"。多一个分类只会多一处被误读的地方。
  parts <- character(0)
  if (nzchar(isp) && nzchar(wsp) && !same(isp, wsp))
    parts <- c(parts, paste0("表是「", isp, "」，这批数据是「", wsp, "」"))
  if (nzchar(its) && nzchar(wts) && !same(its, wts))
    parts <- c(parts, paste0("表标注组织「", its, "」，这批数据是「", wts, "」"))

  c(base, list(
    verdict = "noted",
    label = paste(c(isp, its)[nzchar(c(isp, its))], collapse = " / "),
    why = paste0(paste(parts, collapse = "；"),
                 "。照用：跨物种的细胞类型判定基因多为同源基因，",
                 "脚本比对时统一转成大写，符号能对上；移植实验里人源与鼠源细胞共存也是常规情况。",
                 "依据的适用范围由人确定，应用不替人做这个决定。")))
}

ref_list <- function(species = "", tissue = "") {
  d <- ref_index_read()
  lapply(d$items, function(x) {
    ap <- ref_applicability(x, species = species, tissue = tissue)
    list(id = x$id, name = x$name, bytes = x$bytes, digest = x$digest %||% x$sha256,
         note = x$note, species = x$species, tissue = x$tissue,
         source = as.character(x$source %||% "uploaded"),
         server_path = as.character(x$server_path %||% ""),
         uploaded_at = x$uploaded_at,
         # 适用性：ok / mismatch / unlabeled。
         #   给到前端，人在选之前就能看到"这份是哪个物种哪个组织"，
         #   而不是选完跑一遍才发现名字全错。
         applicable = ap$verdict,
         applicable_why = ap$why)
  })
}

ref_path <- function(id) {
  d <- ref_index_read()
  for (x in d$items) {
    if (identical(as.character(x$id %||% ""), as.character(id))) {
      p <- file.path(REF_DIR, as.character(x$stored %||% ""))
      if (file.exists(p)) return(p)
      return("")
    }
  }
  ""
}

# 给某一份候选补上/改正它的适用说明。
#   为什么要人能改：物种和组织不一定写在表里（很多补充材料只在正文说）。
#   猜是错的，空着又判不了，所以留一处"人来写"的入口。
#   只改这几项，不动文件、不动来源、不动路径。
ref_set_meta <- function(id, note = NULL, species = NULL, tissue = NULL) {
  d <- ref_index_read()
  hit <- NA_integer_
  for (i in seq_along(d$items)) {
    if (identical(as.character(d$items[[i]]$id %||% ""), as.character(id))) hit <- i
  }
  if (is.na(hit)) return(list(ok = FALSE, error = "没有这一份"))
  for (nm in c("note", "species", "tissue")) {
    v <- get(nm)
    if (!is.null(v)) d$items[[hit]][[nm]] <- as.character(v)
  }
  d$items[[hit]]$meta_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  ref_index_write(d)
  list(ok = TRUE, item = d$items[[hit]])
}

ref_get <- function(id) {
  d <- ref_index_read()
  for (x in d$items) if (identical(as.character(x$id %||% ""), as.character(id))) return(x)
  NULL
}


# =====================================================================
#  服务器上已有的注释依据表（第三个来源）
# ---------------------------------------------------------------------
#  为什么要有这一类：
#    gina 2026-10-29：「在前期考察环境时，看是否能依据数据本身，
#      对已有的文件库进行罗列。比如服务器里如果已经有 8 到 10 份 reference 表，
#      我针对这套数据可能只需要用其中的一份，那我就省得再手动上传，直接点击就可以了。」
#
#  所以这一类的表和"人工上传"的表在数据上是同一种东西（一张 marker 表），
#  区别只在来源：一份在应用这侧存着、要下发；一份本来就在服务器上、直接用路径。
#  统一登记进同一个索引，用 source 区分，选用逻辑只有一套。
# =====================================================================

# 执行端扫描完服务器后回写候选清单。
#   items: list(list(path=, name=, bytes=, mtime=))
#   为什么按 path 去重：同一个文件被扫到两次不该出现两行；
#   为什么保留旧的人工上传项：扫描只替换 source=server 的那一批，
#   不能把用户自己传的删掉。
# replace=FALSE：按路径合并，不清空别的。
#   为什么改：原来是"每次扫描先删掉所有服务器条目再加回来"。
#   于是扫得窄一次（比如只扫了 reference/），先前扫到的其它目录条目会被
#   一起抹掉 —— 人看到的是"列表越来越短"，还以为是文件没了。
#   扫不到只代表这次没扫到，不代表它不存在。
ref_register_server <- function(items, host = "", replace = FALSE) {
  d <- ref_index_read()
  keep <- Filter(function(x) {
    same_host <- !nzchar(host) || identical(as.character(x$host %||% ""), as.character(host))
    !(identical(as.character(x$source %||% "uploaded"), "server") && same_host && isTRUE(replace))
  }, d$items %||% list())

  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  added <- 0
  for (it in (items %||% list())) {
    p <- as.character(it$path %||% "")
    if (!nzchar(p)) next
    ext <- tolower(tools::file_ext(p))
    if (!ext %in% c("xlsx", "xls", "tsv", "csv", "json")) next
    # id 用路径哈希，稳定：同一个文件每次扫描都是同一个 id，
    #   换成人能读的随机串会导致"选过的那份下次扫描就找不到了"。
    id <- paste0("srv-", substr(paste0("h", digest_of_str(p)), 1, 12))
    # 同一个路径已经有了：更新字段，不要再多出一条同名的。
    #   重复条目会让人在列表里看到两个一模一样的文件，选哪个都心虚。
    dup <- NA_integer_
    for (i in seq_along(keep)) {
      if (identical(as.character(keep[[i]]$id %||% ""), id)) dup <- i
    }
    if (!is.na(dup)) {
      keep[[dup]]$bytes <- as.integer(it$bytes %||% keep[[dup]]$bytes %||% 0)
      keep[[dup]]$scanned_at <- now
      for (fld in c("note", "species", "tissue")) {
        v <- as.character(it[[fld]] %||% "")
        if (nzchar(v)) keep[[dup]][[fld]] <- v
      }
      added <- added + 1
      next
    }
    keep[[length(keep) + 1]] <- list(
      id = id,
      name = basename(p),
      stored = "",
      bytes = as.integer(it$bytes %||% 0),
      digest = "",
      note = as.character(it$note %||% ""),
      species = as.character(it$species %||% ""),
      tissue = as.character(it$tissue %||% ""),
      source = "server",
      server_path = p,
      host = as.character(host %||% ""),
      scanned_at = now,
      uploaded_at = now)
    added <- added + 1
  }
  d$items <- keep
  ref_index_write(d)
  srvN <- sum(vapply(keep, function(x)
    identical(as.character(x$source %||% "uploaded"), "server"), logical(1)))
  list(ok = TRUE, added = added, total = length(keep), server_total = srvN)
}

# 简易稳定哈希（不引外部包）：用于从路径生成固定 id。
digest_of_str <- function(x) {
  # 两个不同乘数各算一遍再拼，避免单模运算下同余路径撞车。
  bytes <- charToRaw(enc2utf8(as.character(x %||% "")))
  h1 <- 0; h2 <- 0
  for (b in as.integer(bytes)) {
    h1 <- (h1 * 31 + b) %% 1000000007
    h2 <- (h2 * 131 + b) %% 1000000009
  }
  sprintf("%08x%08x", as.integer(h1), as.integer(h2))
}

# 某个 id 对应的"该交给执行端的路径"。
#   source=server：就是那个绝对路径，直接用，不下发。
#   source=uploaded：返回空串，由调用方给出"落地后的路径"。
ref_server_path <- function(id) {
  it <- ref_get(id)
  if (is.null(it)) return("")
  if (!identical(as.character(it$source %||% "uploaded"), "server")) return("")
  as.character(it$server_path %||% "")
}
