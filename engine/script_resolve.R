# =====================================================================
#  script_resolve.R -- 这一环该用哪个脚本：服务器 / 库 / 待编写
#
#  要解决的问题
#    「拿到数据先去服务器上找脚本；服务器没有就查自带库；库里也没有就
#     先把脚本补齐」—— 这条链以前只做到第 2 层，而且第 2 层是
#     靠人看红条判断的，不是应用自己给的结论。
#    结果就是：某个环节缺脚本时，人只看到"这一步跑不了"，
#    看不出"是服务器没有、还是库里有但没挂上、还是真缺、缺什么"。
#
#  做法
#    对每一环，按固定顺序给一个结论（不看现状猜，只看事实）：
#      1) server   这一环在 flow_spec 里登记了服务器脚本，且该脚本能在
#                  服务器上定位到（依据最近一次体检回写的脚本清单）
#      2) library  服务器上没有，但应用自带库里有能挂到这一环的条目
#      3) draft    两边都没有：给出"要写什么"的清单（输入、产出、契约），
#                  并落一份骨架草稿到 state/drafts/ 供人改
#    第三层刻意只到"草稿"，不自动生效：改脚本一律转人工（J5）。
#
#  为什么结论要写清"为什么"
#    判错脚本不会报错，只会算出一份看起来正常的结果 —— 那是最难查的一类问题。
#    所以每一环都带依据，人可以逐条核对，而不是只能相信一个结论。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# 服务器上已登记脚本清单（来自体检回写）。
#   取不到就返回空表 —— 空表表示"不知道"，不等于"没有"。
#   这一点必须区分：把不知道当成没有，会让人白写一遍脚本。
script_resolve_server_files <- function(app_dir, sid = "", con = NULL) {
  d <- if (exists("dp_record_env", mode = "function"))
    dp_record_env(app_dir, sid = sid, con = con) else NULL
  if (is.null(d)) return(list(known = FALSE, root = "", files = character(0)))
  sl <- d$script_lib %||% list()
  files <- unlist(lapply(sl$files %||% list(), function(x) {
    if (is.list(x)) as.character(x$file %||% "") else as.character(x)
  }), use.names = FALSE)
  files <- as.character(files[nzchar(files)])
  list(known = length(files) > 0L,
       root = as.character(sl$root %||% ""),
       files = files)
}

# 脚本名归一化：去掉路径与扩展名，只留主名。
#   服务器清单里既有 "02_annotate.R" 也有 "sc-pipeline/scripts/02_annotate.R"，
#   两种写法要能对上。
script_base <- function(x) {
  s <- gsub("\\\\", "/", as.character(x %||% ""))
  s <- basename(s)
  tolower(sub("\\.[A-Za-z0-9]+$", "", s))
}

# 单个环节的结论
script_resolve_one <- function(stage_id, stage, srv, sup_entries) {
  title <- as.character(stage$title %||% stage_id)
  declared <- as.character(stage$source %||% "")

  # 1) 服务器上有没有：只看登记过的脚本名，且只在"知道清单"时才判
  script_rel <- as.character(stage$script %||% "")
  want <- script_base(script_rel)
  on_server <- NA  # NA = 判不了
  if (isTRUE(srv$known) && nzchar(want)) {
    have <- vapply(srv$files, script_base, "")
    on_server <- any(have == want)
  }

  # 2) 应用库里有没有能挂到这一环的条目
  lib <- Filter(function(e) identical(as.character(e$for_stage %||% ""), stage_id), sup_entries)
  lib <- Filter(function(e) !identical(as.character(e$status %||% "ready"), "draft"), lib)

  src <- if (isTRUE(on_server)) "server"
         else if (length(lib)) "library"
         else "draft"

  why <- if (src == "server")
    paste0("服务器上已有 ", basename(script_rel))
  else if (src == "library")
    paste0(if (isTRUE(srv$known)) paste0("服务器上没找到 ", want) else "本记录的服务器脚本清单尚未知",
           "；应用库里有 ", length(lib), " 个条目可挂到这一环")
  else if (!isTRUE(srv$known))
    paste0("还不知道服务器上有哪些脚本（体检清单为空）——先跑一次识别，再判这一环")
  else
    paste0("服务器清单里没有 ", want, "，应用库里也没有登记这一环的脚本")

  list(
    stage = stage_id, title = title,
    declared_source = declared,
    script = script_rel,
    on_server = if (is.na(on_server)) "unknown" else if (isTRUE(on_server)) "yes" else "no",
    library_entries = lapply(lib, function(e) list(
      id = as.character(e$id %||% ""),
      file = as.character(e$file %||% ""),
      title = as.character(e$title %||% ""),
      status = as.character(e$status %||% "ready"))),
    plan = src,
    why = why,
    # 第三层要人写时，写清"写什么"：这是把"缺脚本"从一句抱怨变成一份任务。
    needs = if (src == "draft") list(
      inputs = as.character(unlist(stage$deps %||% character(0))),
      products = as.character(unlist(stage$artifacts %||% character(0))),
      contract = "从统一入口进来、读 pipeline.json、失败即停、产物写到 outdir"
    ) else NULL
  )
}

# 全部环节
script_resolve_all <- function(engine_dir, app_dir, stage_ids, stage_getter, sup_getter,
                               sid = "", con = NULL) {
  srv <- script_resolve_server_files(app_dir, sid = sid, con = con)
  sup <- tryCatch(sup_getter(), error = function(e) list())
  sup <- sup$entries %||% sup
  out <- lapply(stage_ids, function(id) {
    st <- tryCatch(stage_getter(id), error = function(e) NULL)
    if (is.null(st)) return(NULL)
    script_resolve_one(id, st, srv, sup)
  })
  out <- Filter(Negate(is.null), out)
  cnt <- table(factor(vapply(out, function(x) x$plan, ""),
                      levels = c("server", "library", "draft")))
  # 注意 table 在某一档为 0 时不给该键；直接 cnt["server"] 会拿到 NA，
  #   而 NA 不是 NULL，`%||%` 兜不住 —— 页面就会显示 server: NA。
  num <- function(k) { v <- suppressWarnings(as.integer(cnt[k])); if (is.na(v)) 0L else v }
  list(ok = TRUE,
       server_scan = list(known = isTRUE(srv$known), root = srv$root,
                          files = length(srv$files)),
       counts = list(server = num("server"),
                     library = num("library"),
                     draft = num("draft")),
       stages = out)
}

# 缺脚本的环节：落一份骨架草稿，供人改。
#   刻意不自动生效：写脚本属于「改代码」，一律转人工（见 J5）。
#   草稿里写清输入、产出、契约与禁止事项，比给一个空文件有用。
script_draft_skeleton <- function(app_dir, engine_dir, one) {
  dir <- file.path(app_dir, "state", "drafts")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(dir, paste0("script-", one$stage, "-",
                             format(Sys.time(), "%Y%m%d-%H%M%S"), ".R"))
  needs <- one$needs %||% list()
  lines <- c(
    paste0("# ===== 待编写脚本：", one$title, "（阶段 id=", one$stage, "）====="),
    "# 这份是骨架草稿，不会自动生效。改好之后要人工合入，并在页面上确认。",
    paste0("# 为什么会有这一份：", one$why),
    "",
    "# 必须读的设置：pipeline.json（不要写死阈值）",
    paste0("# 输入（上游产物）：", paste(needs$inputs %||% "-", collapse = "、")),
    paste0("# 应写出的产物：", paste(needs$products %||% "-", collapse = "、")),
    paste0("# 契约：", as.character(needs$contract %||% "")),
    "",
    "# 禁止事项：",
    "#   · 不要绕过统一入口（守卫要能认出这次运行）",
    "#   · 不要吞掉错误：出错就停，把原始报错写出去",
    "#   · 不要用别的项目的产物顶替本次输入",
    "",
    "suppressWarnings(options(stringsAsFactors = FALSE))",
    "",
    "args <- commandArgs(trailingOnly = TRUE)",
    "",
    "# TODO 1：读输入",
    "# TODO 2：算",
    "# TODO 3：写产物，并核对产出的文件确实存在、大小不为 0",
    "",
    "stop(\"骨架草稿：还没实现，先不要跑。\")"
  )
  writeLines(lines, f, useBytes = TRUE)
  list(ok = TRUE, draft = f)
}
