#!/usr/bin/env Rscript
# =====================================================================
#  step_plan.R -- 「这一步到底跑哪条命令」的唯一出处
#
#  为什么要单独一个文件：
#    「一键全流程」必须能一步一步地、在轮到它的时候才解析命令。
#    如果在链启动时就把六条命令全定下来，「出图」会因为还没生成
#    processed.rds 而被判死，一键全流程就永远起不来。
#    所以链式运行器需要在每一步开始前，重新问一次「这一步现在跑什么」。
#
#  因此：服务端和链式运行器都 source 这个文件，共用同一套解析逻辑。
#        两边不会各写一份、各写各的。
#
#  本文件不做任何 IO 副作用，只读配置、只算路径。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, scipen = 999, warn = -1))

IS_WIN <- .Platform$OS.type == "windows"
pjoin <- function(...) do.call(file.path, as.list(c(...)))

# ---- 读 JSON（统一走 jsonlite，缺了就明确报错，不自己造轮子）----
need_jsonlite <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("缺少 jsonlite 包（读写配置用）。安装：install.packages('jsonlite')", call. = FALSE)
  invisible(TRUE)
}

read_text_any <- function(path, max_bytes = 200000L) {
  if (!file.exists(path)) return("")
  sz <- file.info(path)$size
  con <- file(path, open = "rb"); on.exit(close(con), add = TRUE)
  if (!is.na(sz) && sz > max_bytes) seek(con, where = sz - max_bytes, origin = "start")
  raw <- readBin(con, "raw", n = max_bytes * 4L)
  if (!length(raw)) return("")
  txt <- rawToChar(raw)
  if (!validUTF8(txt)) txt <- iconv(txt, from = "GBK", to = "UTF-8", sub = "?")
  txt
}

read_json_file <- function(path) {
  if (!file.exists(path)) return(NULL)
  need_jsonlite()
  tryCatch(jsonlite::fromJSON(read_text_any(path, max_bytes = 5e6), simplifyVector = FALSE),
           error = function(e) NULL)
}

# 兼容 {value, source} 包装与裸值
cfg_val <- function(cfg, section, key, default = NULL) {
  node <- cfg[[section]][[key]]
  if (is.null(node)) return(default)
  if (is.list(node) && !is.null(node[["value"]])) return(node[["value"]])
  node
}

read_cfg <- function(runDir) read_json_file(pjoin(runDir, "pipeline.json"))

rscript_bin <- function(cfg = NULL) {
  p <- cfg_val(cfg, "environment", "rscript_path", NULL)
  if (!is.null(p) && length(p) == 1L && nzchar(as.character(p)) && file.exists(as.character(p)))
    return(enc2native(as.character(p)))
  file.path(R.home("bin"), if (IS_WIN) "Rscript.exe" else "Rscript")
}

find_python <- function(cfg = NULL) {
  p <- cfg_val(cfg, "environment", "python_path", NULL)
  if (!is.null(p) && length(p) == 1L && nzchar(as.character(p)) && file.exists(as.character(p)))
    return(as.character(p))
  cand <- if (IS_WIN) c("python.exe", "python3.exe") else c("python3", "python")
  for (c1 in cand) {
    hit <- Sys.which(c1)
    if (nzchar(hit)) return(unname(hit))
  }
  NULL
}

zip_bin <- function() {
  if (IS_WIN) {
    t <- unname(Sys.which("tar")); if (nzchar(t)) return(t)
    return(NULL)
  }
  z <- unname(Sys.which("zip")); if (nzchar(z)) return(z)
  t <- unname(Sys.which("tar")); if (nzchar(t)) return(t)
  NULL
}

# 该拿哪个对象出图：有注释过的就用注释过的
plot_input <- function(outdir) {
  a <- pjoin(outdir, "annot", "annotated.rds")
  if (file.exists(a)) return(a)
  p <- pjoin(outdir, "processed.rds")
  if (file.exists(p)) return(p)
  NULL
}

# ---------------------------------------------------------------------
# 六步：每步一条固定命令
#   返回结构里 make() 要么给 list(cmd,args,cwd)，要么给 list(err=...)。
#   err 表示「现在还不能跑」以及为什么 —— 页面直接把这句显示给使用者。
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# 脚本目录：按「环境 × 需求」判定该取哪一套。
#
#   以前这里直接写死 sc-pipeline/scripts，pick.R 判出来的结果只显示在页面上，
#   跑起来还是那一份固定副本 —— 判定与实际执行脱节，属于展示性功能。
#
#   取不到时退回随应用带的那一份，但要把原因写下来。
#   静默回退和静默使用固定副本是同一个毛病：看结果看不出来用了哪套。
# ---------------------------------------------------------------------
resolve_scripts_dir <- function(engineDir, cfg, runDir = NULL) {
  bundled <- pjoin(engineDir, "sc-pipeline", "scripts")
  res <- list(dir = bundled, set_id = "bundled", why = "",
              fallback = FALSE)

  pickf <- pjoin(engineDir, "pick.R")
  if (!file.exists(pickf)) {
    res$why <- "engine/pick.R 不存在，用随应用带的脚本"
  } else {
    mem <- cfg_val(cfg, "environment", "memory_gb", NULL)
    mem <- suppressWarnings(as.numeric(mem))
    if (length(mem) != 1L || is.na(mem)) mem <- NULL
    r <- tryCatch({
      env <- new.env(parent = globalenv())
      sys.source(pickf, envir = env)
      env$pick_set(engineDir, category = "A", demand = "D1_pipeline", mem_gb = mem)
    }, error = function(e) NULL)

    if (is.null(r)) {
      res$why <- "查表出错，退回随应用带的脚本"
      res$fallback <- TRUE
    } else if (!is.null(r$error)) {
      res$why <- paste0("查表未给出可用套（", r$error, "），退回随应用带的脚本")
      res$fallback <- TRUE
    } else if (!isTRUE(r$usable)) {
      # 不用 %||%：这个 helper 可能被单独 source，别让它依赖外面的定义。
      res$set_id <- if (is.null(r$id)) "unknown" else as.character(r$id)
      rsn <- if (!is.null(r$reason)) as.character(r$reason)
             else if (!is.null(r$status)) as.character(r$status) else ""
      res$why <- sprintf("判定该取 %s，但它现在取不出来（%s），退回随应用带的脚本",
                         res$set_id, rsn)
      res$fallback <- TRUE
    } else {
      # 库里有一类条目是「处理规程」而不是脚本套（本就不含 .R 文件）。
      #   把规程目录当脚本目录用，会让流程指向一个空目录 —— 比走兜底更糟。
      #   所以判定结果必须过一道「里面真有没有流程脚本」的检查。
      cand <- as.character(r$path_abs)
      has_scripts <- length(list.files(cand, pattern = "\\\\.R$",
                                       recursive = FALSE)) > 0L
      res$set_id <- if (is.null(r$id)) "unknown" else as.character(r$id)
      envid <- if (is.null(r$env) || is.null(r$env$id)) "" else as.character(r$env$id)
      if (!has_scripts) {
        res$why <- sprintf("判定指向 %s，但那里没有流程脚本（是规程不是脚本套），退回随应用带的脚本",
                           res$set_id)
        res$fallback <- TRUE
      } else {
        res$dir <- cand
        res$why <- sprintf("按环境判定取用 %s（%s）", res$set_id, envid)
      }
    }
  }

  # 判定结果落盘：换台机器、隔几天回看，都要能回答「上次用的是哪套、为什么」。
  if (!is.null(runDir) && nzchar(runDir)) {
    sf <- pjoin(runDir, "status", "library_pick.json")
    try({
      dir.create(dirname(sf), showWarnings = FALSE, recursive = TRUE)
      write_json_file(sf, list(set_id = res$set_id, dir = res$dir,
                               fallback = res$fallback, why = res$why,
                               decided_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))
    }, silent = TRUE)
  }
  res
}

build_steps <- function(cfg, runDir, engineDir) {
  outdir <- cfg_val(cfg, "io", "outdir", NULL)
  if (is.null(outdir) || !nzchar(as.character(outdir))) outdir <- pjoin(runDir, "results")
  outdir <- as.character(outdir)
  indir <- cfg_val(cfg, "io", "indir", "")
  if (is.null(indir)) indir <- ""
  cfgf   <- pjoin(runDir, "pipeline.json")
  gatesf <- pjoin(runDir, "gates.json")
  # 脚本目录由判定给出；取不到才退回随应用带的那一份（原因写进状态文件）
  .pick  <- resolve_scripts_dir(engineDir, cfg, runDir)
  sp     <- .pick$dir
  envchk <- pjoin(engineDir, "env_check.R")
  plotR  <- pjoin(engineDir, "sc-plot-kit", "scripts", "plot_all_v2.R")
  pptPy  <- pjoin(engineDir, "sc-ppt-kit", "scripts", "make_ppt.py")
  tpl    <- pjoin(engineDir, "sc-ppt-kit", "templates", "template.pptx")

  list(
    list(id = "env", title = "环境体检", needGate = FALSE,
         note = "查 R、分析要用的包、内存、Python 是否齐。只读，不改任何数据。",
         make = function() {
           list(cmd = rscript_bin(cfg),
                args = c(envchk, "--out", pjoin(runDir, "status", "env_check.json")),
                cwd = runDir)
         }),

    list(id = "decide", title = "自动判定", needGate = FALSE,
         note = "自己判断：这台机器上有没有现成脚本、内存够不够、包齐不齐、数据在不在。够就往下走，不够就说清缺什么。",
         make = function() {
           # 这一条不改数据、不改配置，只做判定并写出 status/decision.json。
           #   顺手把「机器上已有的脚本」扫一遍 —— 有现成的就不该再拷一套。
           #   两套并行跑同样的数据，早晚分叉；分叉之后真正麻烦的不是哪个快，
           #   而是同一个问题有两个答案，事后说不清哪个算数。
           list(cmd = rscript_bin(cfg),
                args = c(pjoin(engineDir, "decide_cli.R"), "--dir", runDir,
                         "--engine", engineDir),
                cwd = runDir)
         }),

    list(id = "precheck", title = "容量预检", needGate = TRUE, gatePhase = "A",
         note = "只算内存够不够、数据文件在不在。不跑分析，不写结果。",
         make = function() {
           if (!nzchar(as.character(indir)) || !dir.exists(indir))
             return(list(err = sprintf("数据目录还没填，或者填的路径不存在：%s", indir)))
           list(cmd = rscript_bin(cfg),
                args = c(pjoin(sp, "00_run_all.R"), "--config", cfgf,
                         "--indir", indir, "--outdir", outdir, "--dry-run"),
                cwd = runDir)
         }),

    # 主流程拆成上下两段，中间停下来等人做决定。
    #   为什么必须拆：筛群和剔样本是人定的，依据（簇大小、样本分布、marker）
    #   只有跑到一次聚类才产生。顺序只能是「跑到那里 → 出依据 → 停 → 人定 → 接着跑」。
    #   不拆的话，脚本要么替人瞎猜，要么在没依据的情况下问人 —— 两种都不对。
    list(id = "pipeline1", title = "跑主流程 · 上段", needGate = TRUE, gatePhase = "A",
         note = "去双、质控、合并、批次校正、一次聚类。跑完把人工判断依据摆出来就停住。",
         make = function() {
           if (!nzchar(as.character(indir)) || !dir.exists(indir))
             return(list(err = sprintf("数据目录还没填，或者填的路径不存在：%s", indir)))
           a <- c(pjoin(sp, "00_run_all.R"), "--config", cfgf,
                  "--indir", indir, "--outdir", outdir, "--stage", "cluster1")
           if (file.exists(gatesf)) a <- c(a, "--gates", gatesf)
           list(cmd = rscript_bin(cfg), args = a, cwd = runDir)
         }),

    list(id = "pipeline2", title = "跑主流程 · 下段", needGate = TRUE, gatePhase = "B",
         note = "接着上段的结果，做筛群、二次聚类、剔除样本、文献注释。",
         make = function() {
           if (!nzchar(as.character(indir)) || !dir.exists(indir))
             return(list(err = sprintf("数据目录还没填，或者填的路径不存在：%s", indir)))
           snap <- pjoin(outdir, "status", "stage_cluster1.rds")
           if (!file.exists(snap))
             return(list(err = "上段还没跑完（找不到一次聚类的快照）。先跑「跑主流程 · 上段」。"))
           a <- c(pjoin(sp, "00_run_all.R"), "--config", cfgf,
                  "--indir", indir, "--outdir", outdir, "--stage", "continue")
           if (file.exists(gatesf)) a <- c(a, "--gates", gatesf)
           list(cmd = rscript_bin(cfg), args = a, cwd = runDir)
         }),

    list(id = "plots", title = "出图", needGate = FALSE,
         note = "六套图：质控、聚类、marker、注释、细胞比例、组间差异富集。",
         make = function() {
           rds <- plot_input(outdir)
           if (is.null(rds))
             return(list(err = "还没找到 processed.rds，所以出不了图。先把主流程上下两段跑完。"))
           list(cmd = rscript_bin(cfg),
                args = c(plotR, rds, "--outdir", pjoin(outdir, "plots"),
                         "--config", cfgf,
                         "--stage", "qc,cluster,markgene,annot,ratio,diff,enrich"),
                cwd = runDir)
         }),

    list(id = "ppt", title = "出 PPT", needGate = FALSE,
         note = "把图按客户那套版式组装成可以直接交付的 pptx。",
         make = function() {
           py <- find_python(cfg)
           if (is.null(py))
             return(list(err = "找不到 Python。出 PPT 需要它；可在「全部设置」里填 python_path。"))
           plots <- pjoin(outdir, "plots")
           if (!dir.exists(plots)) return(list(err = "还没出图，所以没东西可装。先点「出图」。"))
           if (!file.exists(tpl)) return(list(err = "缺少 PPT 母版 template.pptx。"))
           list(cmd = py,
                args = c(pptPy, "--result-dir", plots, "--template", tpl,
                         # 交付标题从这次运行的配置取（case.name）。
                         #   不传的话标题只能退回模板里的默认值，
                         #   换数据时交付稿会顶着上一批的项目名。
                         "--config", cfgf,
                         "--out", pjoin(outdir, "analysis_report.pptx")),
                cwd = runDir)
         }),

    list(id = "pack", title = "打包结果", needGate = FALSE,
         note = "把图和 PPT 压成一个 zip，方便拷走或发人。",
         make = function() {
           zipsrc <- c(pjoin(outdir, "plots"), pjoin(outdir, "analysis_report.pptx"),
                       pjoin(outdir, "run_report.md"), pjoin(outdir, "run_manifest.json"))
           zipsrc <- zipsrc[file.exists(zipsrc)]
           if (!length(zipsrc)) return(list(err = "还没有可打包的产物。先跑主流程和出图。"))
           zb <- zip_bin()
           if (is.null(zb)) return(list(err = "这个系统上找不到 zip 或 tar，打不了包。"))
           zipf <- pjoin(outdir, "analysis_results.zip")
           if (file.exists(zipf)) unlink(zipf)
           if (IS_WIN) {
             list(cmd = zb, args = c("-a", "-c", "-f", zipf, "-C", outdir, basename(zipsrc)), cwd = outdir)
           } else if (grepl("zip$", zb)) {
             list(cmd = zb, args = c("-r", "-q", zipf, basename(zipsrc)), cwd = outdir)
           } else {
             list(cmd = zb, args = c("-a", "-c", "-f", zipf, "-C", outdir, basename(zipsrc)), cwd = outdir)
           }
         })
  )
}

step_titles <- function(cfg, runDir, engineDir) {
  lapply(build_steps(cfg, runDir, engineDir),
         function(s) list(id = s$id, title = s$title, note = s$note,
                          needGate = s$needGate,
                          gatePhase = if (is.null(s$gatePhase)) NULL else s$gatePhase))
}

# 解析一步的完整命令；不能跑就返回 error
resolve_step_cli <- function(stepId, runDir, engineDir) {
  if (is.null(runDir) || !dir.exists(runDir)) return(list(error = "运行目录不可用"))
  cfg <- read_cfg(runDir)
  if (is.null(cfg))
    return(list(error = "这个运行目录里还没有 pipeline.json。先点「生成一份默认设置」。"))
  hit <- Filter(function(x) identical(x$id, stepId), build_steps(cfg, runDir, engineDir))
  if (!length(hit)) return(list(error = paste0("未知步骤：", stepId)))
  s <- hit[[1]]
  r <- tryCatch(s$make(), error = function(e) list(err = conditionMessage(e)))
  if (!is.null(r$err)) return(list(error = r$err, id = s$id, title = s$title))
  list(id = s$id, title = s$title,
       cmd = enc2utf8(as.character(r$cmd)),
       args = enc2utf8(vapply(r$args, as.character, character(1))),
       cwd = enc2utf8(as.character(r$cwd)))
}

# 还有哪些闸门没答；没 gates 文件时返回 NULL（交给脚本自己判断）
#   phase 给 "A" / "B" 时只数该段位里没答的。
#   没标 phase 的一律算 A —— 老配置里没有这个字段，
#   算成 A 会让它更早被问到（多问一次），而不是被跳过（漏问）。
pending_gate_names <- function(runDir, phase = NULL) {
  g <- read_json_file(pjoin(runDir, "gates.json"))
  if (is.null(g) || is.null(g$gates)) return(NULL)
  out <- character(0)
  for (one in g$gates) {
    st <- if (is.null(one$status)) "pending" else one$status
    if (!identical(st, "pending")) next
    ph <- if (is.null(one$phase)) "A" else as.character(one$phase)
    if (!is.null(phase) && !identical(ph, phase)) next
    out <- c(out, if (is.null(one$title)) one$id else one$title)
  }
  out
}

# 该步卡在哪些闸门上（用于「为什么还不能跑」的说明）
blocking_gates_for <- function(runDir, stepId) {
  ph <- step_gate_phase(stepId)
  if (is.null(ph)) return(character(0))
  p <- pending_gate_names(runDir, ph)
  if (is.null(p)) character(0) else p
}

# ---------------------------------------------------------------------
# 初始化默认设置
#
# 放在这里而不是各自的入口里：网页入口和命令行入口必须生成一模一样的
# 配置文件。两处各写一份，早晚会不一致 —— 而不一致的配置会让「同样的
# 参数跑出不同结果」这种最难查的问题出现。
# ---------------------------------------------------------------------
detect_memory_gb <- function() {
  if (IS_WIN) {
    tot <- tryCatch(suppressWarnings(system2("wmic", c("OS", "get", "TotalVisibleMemorySize", "/value"),
                                            stdout = TRUE, stderr = FALSE)), error = function(e) character(0))
    hit <- grep("TotalVisibleMemorySize", tot, value = TRUE)
    if (length(hit)) {
      kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", hit[1])))
      if (!is.na(kb)) return(kb / 1024 / 1024)
    }
    return(NA_real_)
  }
  mi <- tryCatch(readLines("/proc/meminfo"), error = function(e) character(0))
  hit <- grep("^MemTotal", mi, value = TRUE)
  if (length(hit)) {
    kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", hit[1])))
    if (!is.na(kb)) return(kb / 1024 / 1024)
  }
  NA_real_
}

# 能自动读出来的本机事实就自动填上，别推给人当闸门。
#   留成 gate 的后果是：明明读一下就知道的事，却卡住不让跑 —— 那是形式主义。
#   来源标成 instance（本机适配），和官方口径区分开。
autofill_environment <- function(obj) {
  if (is.null(obj$environment) || !is.list(obj$environment)) obj$environment <- list()

  rs <- tryCatch(file.path(R.home("bin"), if (IS_WIN) "Rscript.exe" else "Rscript"),
                 error = function(e) NULL)
  if (!is.null(rs) && file.exists(rs))
    obj$environment$rscript_path <- list(value = gsub("\\\\", "/", rs), source = "instance",
                                        why = "本机实际使用的 R")

  libs <- .libPaths()
  if (length(libs))
    obj$environment$r_lib_paths <- list(value = as.list(gsub("\\\\", "/", libs)), source = "instance",
                                       why = "本机 R 包搜索路径")

  mem <- detect_memory_gb()
  if (!is.na(mem))
    obj$environment$memory_gb <- list(value = round(mem, 1), source = "instance",
                                     why = "本机物理内存实测")

  py <- tryCatch(find_python(NULL), error = function(e) NULL)
  if (!is.null(py))
    obj$environment$python_path <- list(value = enc2utf8(gsub("\\\\", "/", py)), source = "instance",
                                       why = "本机 Python，出 PPT 用")

  # samples_file 是可选的：没有分组表也能跑，只是分组相关图会缺
  if (is.null(obj$io$samples_file) ||
      (is.list(obj$io$samples_file) && identical(obj$io$samples_file$source, "gate"))) {
    v <- obj$io$samples_file$value
    if (is.null(v) || (is.character(v) && !nzchar(v)))
      obj$io$samples_file <- list(value = NULL, source = "user",
                                  why = "没提供分组表；分组相关图会缺")
  }
  obj
}

init_config <- function(runDir, engineDir, overwrite = FALSE) {
  tplDir <- file.path(engineDir, "sc-pipeline", "templates")
  written <- character(0)
  for (nm in c("pipeline.json", "gates.json")) {
    target <- file.path(runDir, nm)
    if (file.exists(target) && !overwrite) {
      # 已有就别动：里面可能是人填过的，覆盖掉是不可逆的损失
      written <- c(written, paste0(nm, "（已有，没动）")); next
    }
    src <- file.path(tplDir, sub("\\.json$", ".default.json", nm))
    if (!file.exists(src)) { written <- c(written, paste0(nm, "（模板缺失）")); next }
    txt <- read_text_any(src, max_bytes = 5e6)
    txt <- gsub("__RUNDIR__", gsub("\\\\", "/", runDir), txt)
    obj <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(obj)) {
      if (is.null(obj$io) || !is.list(obj$io)) obj$io <- list()
      if (is.null(obj$io$outdir) || !is.list(obj$io$outdir)) obj$io$outdir <- list(value = NULL, source = "unspecified")
      if (is.null(obj$io$outdir$value) || !nzchar(as.character(obj$io$outdir$value)))
        obj$io$outdir$value <- gsub("\\\\", "/", file.path(runDir, "results"))
      if (is.null(obj$io$indir) || !is.list(obj$io$indir)) obj$io$indir <- list(value = NULL, source = "gate")
      if (is.null(obj$case) || !is.list(obj$case)) obj$case <- list()
      if (is.null(obj$case$created_at)) obj$case$created_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      if (is.null(obj$schema_version)) obj$schema_version <- "1.0"
      obj <- autofill_environment(obj)
      txt <- jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
    }
    con <- file(target, open = "wb"); writeBin(charToRaw(enc2utf8(txt)), con); close(con)
    written <- c(written, nm)
  }
  written
}

# 依序跑这八个阶段（注释以前写「七步」，与下面的实际元素个数不符，已按代码更正）。
#   主流程占两格（pipeline1 / pipeline2），因为它们之间必须停下来等人。
CHAIN_ORDER <- c("env", "decide", "precheck", "pipeline1", "pipeline2", "plots", "ppt", "pack")

# 每一步登记哪些产物（供台账记指纹、供 verify 事后核验）
STEP_OUTPUTS <- list(
  # env / decide 的状态文件写在运行目录下的 status/（引擎自己写的）
  env       = c("status/env_check.json"),
  decide    = c("status/decision.json"),
  # precheck 及之后的产物都写在输出目录（results/）下的 status/ 里 ——
  #   因为那是分析脚本（01_preprocess.R）写的，它只知道 OUTDIR。
  #   这里写成 "status/precheck.json" 会永远找不到文件，
  #   于是每一次都被判定为「产物丢了 → 失败」，依赖它的下一步全部拒绝执行。
  #   踩过这个坑：run-all 明明跑成功了，却报「前一步失败」。
  precheck  = c("results/status/precheck.json"),
  # 上段的产物是「一次聚类 + 判断依据 + 可直接续跑的快照」。
  # 快照本身就是产物：没有它，下段接不上。
  pipeline1 = c("results/status/stage_cluster1.rds",
                "results/status/cluster1_evidence.json",
                "results/status/cluster1.json"),
  pipeline2 = c("results/processed.rds", "results/annot/annotated.rds",
                "results/run_report.md", "results/run_manifest.json"),
  plots     = c("results/plots"),
  ppt       = c("results/analysis_report.pptx"),
  pack      = c("results/analysis_results.zip")
)

# 每一步的前置步骤（写死，不靠临场判断）
STEP_DEPS <- list(
  env = character(0), decide = c("env"), precheck = c("decide"),
  pipeline1 = c("precheck"), pipeline2 = c("pipeline1"),
  plots = c("pipeline2"), ppt = c("plots"), pack = c("plots")
)

# 每一步要求的闸门段位。
#   A = 跑上段之前就得定（数据在哪、注释依据、质控门槛、读入前剔样本）
#   B = 跑到一次聚类、出完依据之后再定（筛群、剔样本、低置信门槛）
#   分两段的理由：B 那几条的依据只有跑到一次聚类才产生。
#   把它们提前问，就是逼人在没有材料的情况下猜 —— 那和自动瞎猜一样糟。
step_gate_phase <- function(stepId) {
  switch(stepId,
         precheck  = "A",
         pipeline1 = "A",
         pipeline2 = "B",
         NULL)
}

# 自动判定这一步的结论会影响「能不能往下走」，所以单独暴露出来给界面用。
#   这里只读判定结果文件，不重复实现判定逻辑 —— 两处实现早晚不一致。
read_decision <- function(runDir) {
  f <- pjoin(runDir, "status", "decision.json")
  if (!file.exists(f)) return(NULL)
  read_json_file(f)
}
step_needs_gate <- function(stepId) !is.null(step_gate_phase(stepId))
