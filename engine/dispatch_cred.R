# =====================================================================
# 派工凭证：set_id 由应用派工侧给出，不由执行端自报
#
# 为什么单独一份（2026-11-13）
#   set_id 是流程代号（如 liny-doublet），它属于「派工时应用决定用哪套 skill」，
#   不属于「执行端跑完能自己发现的事实」。worker 里没有任何 step→skill 映射，
#   让它在回写体里自报一个代号，等于让它猜，猜对了也证明不了任何事。
#
# 所以这里的口径是：
#   · 应用派工时会记录这一步该用哪个 skill（新任务写进 exec 记录）；
#   · 对派工记录里没有该字段的老任务，允许由本函数从 flow_spec 的 stage$skill
#     取值，但必须同时满足一组可核条件，并在结论里写明 provenance。
#   · 条件不满足一律返回 NULL，让合规层继续判 unknown，绝不放行。
#
# 可核条件（缺一不可）：
#   1. 有且只有一个在案的 exec 任务与本次回写的 exec_id 相同；
#   2. 该任务的 session / step / run_dir 与回写体一致；
#   3. 回写体给了实际 script_path，且其 basename 与本阶段允许的脚本一致；
#   4. 回写体给了 32 位 script_md5；
#   5. 回写体给了脚本 mtime，且它不晚于该任务的派工时间（证明运行期间没被换掉）。
#   第 5 条是「脚本未在本次运行期间变化」的替代判据：应用侧看不到集群磁盘，
#   只能让执行端报 mtime，再用派工时间做先后比对。报不出 mtime 就不给凭证。
# =====================================================================

# 本文件可能被独立加载（判定器单独跑），不依赖别处已定义 %||%。
if (!exists("%||%", mode = "function"))
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

DISPATCH_CRED_VERSION <- "dispatch-cred-1.0"

dc_chr <- function(x, default = "") {
  if (is.null(x) || !length(x)) return(default)
  as.character(x)[1]
}


# 规范化脚本路径：只做机械归一，不做任何"看起来像就当成一样"的近似。
#   为什么必须逐字符相等（2026-11-13 收紧）：
#     basename 相同只说明文件名一样，别的目录放一份同名脚本就能冒充。
#   规则（写清楚，避免以后再猜）：
#     · 反斜杠一律当分隔符，统一成正斜杠 —— Windows 上 shell 送出来的路径
#       可能是反斜杠，那是表示法差异，不是另一个文件；
#     · 折叠重复斜杠，去掉 /./，去掉末尾多余的 /；
#     · 不做大小写折叠：集群是 Linux，大小写不同就是不同文件；
#     · 不解符号链接：应用侧看不到集群磁盘，做不到 realpath。
#       因此符号链接别名会被判不相等 —— 保守方向，宁可拒绝。
dc_norm_path <- function(x) {
  p <- dc_chr(x)
  if (!nzchar(p)) return("")
  # 反斜杠转正斜杠必须用 fixed = TRUE：
  #   单个反斜杠在正则里是非法模式（trailing backslash），会直接抛错，
  #   于是整个派工凭证判成 dispatch_cred_error，任何回写都过不了。
  #   2026-11-13 隔离实测撞到过这条。
  p <- gsub("\\", "/", p, fixed = TRUE)
  p <- gsub("/+", "/", p)
  p <- gsub("/\\./", "/", p)
  while (grepl("/\\./", p)) p <- gsub("/\\./", "/", p)
  if (nchar(p) > 1L) p <- sub("/+$", "", p)
  p
}

# 绝对路径判定：Unix 以 / 开头；Windows 盘符形式也认，但集群侧只应出现 Unix 形式。
dc_is_abs <- function(x) {
  p <- dc_chr(x)
  nzchar(p) && (grepl("^/", p) || grepl("^[A-Za-z]:/", p))
}

# 取脚本修改时间（epoch 秒）。回写优先用 epoch；没有就解析 ISO 串。
dc_script_epoch <- function(b) {
  ep <- suppressWarnings(as.numeric(dc_chr(b$script_mtime_epoch)))
  if (!is.na(ep) && ep > 0) return(ep)
  ts <- dc_parse_ts(dc_chr(b$script_mtime))
  if (is.na(ts)) return(NA_real_)
  as.numeric(ts)
}

dc_parse_ts <- function(x) {
  s <- dc_chr(x)
  if (!nzchar(s)) return(as.POSIXct(NA))
  s <- sub("Z$", "", s)
  s <- sub("T", " ", s, fixed = TRUE)
  for (fmt in c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d")) {
    v <- suppressWarnings(as.POSIXct(s, format = fmt, tz = "Asia/Shanghai"))
    if (!is.na(v)) return(v)
  }
  as.POSIXct(NA)
}

# 本部署声明的样本清单：这是「应该有几个样本」的唯一权威。
#   为什么不能拿回写体自己的 samples[] 当基准：那是被判方自报的，
#   少报两个就只验两个，缺件永远发现不了。实测就是这样：
#   只回写 5 份、samples 也只写 5 个，判定全过，accepted=true。
dc_declared_samples <- function(app_dir) {
  f <- file.path(app_dir, "config", "server_profile.json")
  if (!file.exists(f)) return(character(0))
  x <- tryCatch(jsonlite::fromJSON(paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                                         collapse = "\n"), simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(x) || !is.list(x)) return(character(0))
  raw <- as.character((x$server %||% list())$samples %||% "")
  if (!nzchar(raw)) return(character(0))
  s <- trimws(unlist(strsplit(raw, "[,\\s]+")))
  s[nzchar(s)]
}

# 脚本根目录：只用于把 flow_spec 里 {script_root} 展开成可比的绝对路径
dc_script_root <- function(app_dir) {
  dc_flow_vars(app_dir)$script_root
}

# 展开 flow_spec 占位符要用的几个根目录。
#   为什么必须都给（2026-11-13 修）：flow_spec 里脚本路径用的是三种占位符，
#   doublet 用 {script_root}、merge 与 final 用 {work_root}、
#   filter/annotate/figs 用 {engine}。只展开 script_root 的话，
#   merge 的规范路径会留着字面量 "{work_root}/merge_cluster.R"，
#   与回写的绝对路径永远不相等 —— 那一步就再也过不了 L3。
#   取值口径与 serve.R 的 flow_vars 保持一致：work_root 就是本次结果目录，
#   engine 是结果目录下的 _engine。
dc_flow_vars <- function(app_dir) {
  out <- list(work_root = "", script_root = "", engine = "", run_dir = "")
  f <- file.path(app_dir, "config", "server_profile.json")
  if (!file.exists(f)) return(out)
  x <- tryCatch(jsonlite::fromJSON(paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                                         collapse = "\n"), simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(x) || !is.list(x)) return(out)
  srv <- x$server %||% list()
  work <- as.character(srv$work_root %||% "")
  out$work_root <- work
  out$run_dir <- work
  out$script_root <- as.character(srv$script_root %||% "")
  out$engine <- if (nzchar(work)) paste0(sub("/+$", "", work), "/_engine") else ""
  out
}

# 把 flow_spec 里的占位符按本部署的真实根目录展开。
#   展开不了（取不到对应根）就原样留着，让路径比对判不相等 —— 宁可拒绝。
dc_expand_script <- function(scr, vars) {
  s <- as.character(scr %||% "")
  if (!nzchar(s)) return("")
  for (k in names(vars)) {
    v <- as.character(vars[[k]] %||% "")
    s <- gsub(paste0("\\{", k, "\\}"), v, s, fixed = FALSE)
  }
  # 千分位保护：/share/... 这类真实路径里不会有反斜杠，统一成斜杠便于比较
  s
}

# 从 flow_spec 取本阶段的规范 skill 与允许脚本（已展开 {script_root}）
dc_stage_expect <- function(app_dir, step_id, script_root = "", vars = NULL) {
  f <- file.path(app_dir, "engine", "flow_spec.R")
  if (!file.exists(f)) return(list(skill = NA_character_, scripts = character(0)))
  e <- new.env(parent = globalenv())
  tryCatch(sys.source(f, envir = e), error = function(err) NULL)
  stages <- if (exists("SERVER_STAGES", envir = e)) e$SERVER_STAGES else list()
  st <- NULL
  for (s in stages %||% list()) {
    if (identical(as.character(s$id %||% ""), as.character(step_id))) { st <- s; break }
  }
  if (is.null(st)) return(list(skill = NA_character_, scripts = character(0)))
  sk <- as.character(st$review$skill %||% st$skill %||% "")
  scr <- as.character(st$script %||% "")
  # 三种占位符都要展开：{script_root} / {work_root} / {engine}。
  #   只展开一种，另一类环节的规范路径会留着字面量，与回写的绝对路径
  #   永远不相等 —— 那一步就过不了 L3，而且原因看起来像"脚本不对"，
  #   实际是判定器自己没展开。2026-11-13 在这里撞到过。
  if (is.null(vars)) vars <- dc_flow_vars(app_dir)
  if (nzchar(script_root)) vars$script_root <- script_root
  scr <- dc_expand_script(scr, vars)
  registry <- if (exists("flow_doublet_audit_registry", envir=e, inherits=FALSE))
    e$flow_doublet_audit_registry else NULL
  list(skill = if (nzchar(sk)) sk else NA_character_,
       scripts = if (nzchar(scr)) scr else character(0), audit_registry=registry)
}

# 读在案执行任务。判定器被 serve.R 调用时拿不到 exec 列表，自己读同一份文件。
dc_exec_items <- function(app_dir) {
  f <- file.path(app_dir, "state", "exec_tasks.json")
  if (!file.exists(f)) return(list())
  x <- tryCatch(jsonlite::fromJSON(readLines(f, warn = FALSE, encoding = "UTF-8"),
                                   simplifyVector = FALSE),
                error = function(e) NULL)
  if (is.null(x) || !is.list(x)) return(list())
  x$items %||% list()
}

# 严格 shell 分词：只做词法解析，绝不 eval / 执行任何参数。
#   为什么不能用正则去切 "--args --target"：
#     真实 script_args 的第一段就是一段 R 代码，里面含有 "--target" 帮助文字
#     （"Rscript -e CODE --args --target SCRIPT -- ORIGINAL_ARGS"）。
#     用子串匹配会命中代码内部那处，切出来的"目标"是代码片段的一部分，
#     实测把 target 解析成了 "SCRIPT -- ORIGINAL_ARGS...target <- arg"。
#   规则按 Python shlex.split(posix=TRUE) 对齐；之后再按 submit() 还原 $ 与反引号：
#     · 单引号内全部字面，不可转义
#     · 双引号内只有反斜杠和双引号会被 shlex 去掉转义，其余反斜杠保留
#     · 引号外空白分词，反斜杠转义下一个字符
dc_shlex_tokens <- function(x) {
  if (length(x) != 1L || is.na(x)) return(NULL)
  s0 <- as.character(x)
  n <- nchar(s0, type = "chars")
  if (!n) return(character(0))
  ch <- strsplit(s0, "", fixed = TRUE)[[1]]
  out <- character(0); cur <- ""
  in_s <- FALSE; in_d <- FALSE; started <- FALSE
  i <- 1L
  while (i <= n) {
    c1 <- ch[[i]]
    if (in_s) {
      if (c1 == "'") { in_s <- FALSE } else { cur <- paste0(cur, c1) }
      i <- i + 1L; next
    }
    if (in_d) {
      if (c1 == '"') { in_d <- FALSE; i <- i + 1L; next }
      if (c1 == "\\" && i < n) {
        nx <- ch[[i + 1L]]
        if (nx %in% c('"', "\\")) {
          cur <- paste0(cur, nx); i <- i + 2L; next
        }
      }
      cur <- paste0(cur, c1); i <- i + 1L; next
    }
    if (c1 == "'") { in_s <- TRUE; started <- TRUE; i <- i + 1L; next }
    if (c1 == '"') { in_d <- TRUE; started <- TRUE; i <- i + 1L; next }
    if (c1 == "\\") {
      if (i == n) return(NULL)
      cur <- paste0(cur, ch[[i + 1L]]); started <- TRUE; i <- i + 2L; next
    }
    if (c1 %in% c(" ", "\t", "\r", "\n")) {
      if (started) { out <- c(out, cur); cur <- ""; started <- FALSE }
      i <- i + 1L; next
    }
    cur <- paste0(cur, c1); started <- TRUE; i <- i + 1L
  }
  if (in_s || in_d) return(NULL)
  if (started) out <- c(out, cur)
  out
}

# 自动物种识别入口的派工事实：真实目标脚本 + 包装代码摘要。
#   这个入口由 flow_spec 生成，派工记录的 script 字段是 "-e"，
#   script_args 形如：<包装代码> --args --target <真实脚本> -- <原始参数> ...
#   为什么必须从派工记录里自己解析：
#     包装代码与目标脚本都是"应用这次发出去的"，用它当基准才能证明
#     执行端报的就是这次派工的那一份，而不是事后凑一个能过检查的值。
#   解析不出来就返回 NULL，绝不猜。
dc_wrapper_expect <- function(it) {
  if (!identical(dc_chr(it$script), "-e")) return(NULL)
  toks <- dc_shlex_tokens(dc_chr(it$script_args))
  # 第一个 token 是包装代码本身（species_exec.R 源码），
  #   其后才是 --args --target <真实脚本> -- <原始参数>。
  if (length(toks) < 5L) return(NULL)
  if (!identical(toks[[2L]], "--args")) return(NULL)
  if (!identical(toks[[3L]], "--target")) return(NULL)
  if (!identical(toks[[5L]], "--") || !nzchar(toks[[1L]])) return(NULL)
  tgt <- toks[[4L]]
  if (!nzchar(tgt) || !dc_is_abs(tgt)) return(NULL)
  # 与执行端写 .R 文件时同一套还原规则：两边必须逐字一致，否则摘要对不上。
  code <- toks[[1L]]
  # fixed = TRUE：要替换的是字面量 "\\$" 与 "\\`"，
  #   不能用正则（"\$" 在正则里是"行尾锚点"的转义，语义完全不同）。
  code <- gsub("\\$", "$", code, fixed = TRUE)
  code <- gsub("\\`", "`", code, fixed = TRUE)

  tf <- tempfile()
  con <- file(tf, "wb")
  writeBin(charToRaw(enc2utf8(code)), con)
  close(con)
  m <- tolower(unname(tools::md5sum(tf)))
  unlink(tf)
  list(target = tgt, wrapper_md5 = m, n_tokens = length(toks))
}


# 主函数：返回 list(ok, set_id, provenance, why, checks)
dc_resolve_set_id <- function(app_dir, step_id, b, exec_items = NULL,
                              script_root = "") {
  if (is.null(exec_items)) exec_items <- dc_exec_items(app_dir)
  out <- function(ok, set_id = NA_character_, provenance = NA_character_,
                  why = "", checks = list()) {
    list(ok = ok, set_id = set_id, provenance = provenance,
         why = why, checks = checks, rule_version = DISPATCH_CRED_VERSION)
  }
  ck <- list()
  add <- function(name, ok, detail, evidence = NULL) {
    ck[[length(ck) + 1L]] <<- list(name = name, ok = isTRUE(ok),
                                   detail = detail, evidence = evidence)
  }

  exp <- dc_stage_expect(app_dir, step_id, script_root = script_root)
  if (is.na(exp$skill)) {
    add("stage_registered", FALSE, paste0("flow_spec 里没有这个环节：", step_id),
        "engine/flow_spec.R")
    return(out(FALSE, why = "stage_not_registered", checks = ck))
  }
  add("stage_registered", TRUE, paste0("规范 skill = ", exp$skill),
      "engine/flow_spec.R")

  # 1) 唯一在案任务
  eid <- dc_chr(b$exec_id)
  if (!nzchar(eid)) {
    add("exec_unique", FALSE, "回写没有 exec_id，无法与派工记录核对", "回写 exec_id")
    return(out(FALSE, why = "no_exec_id", checks = ck))
  }
  hits <- Filter(function(it) identical(dc_chr(it$id), eid), exec_items %||% list())
  if (length(hits) != 1L) {
    add("exec_unique", FALSE, paste0("在案任务匹配数 = ", length(hits), "（要求恰好 1 条）"),
        paste0("exec_id=", eid))
    return(out(FALSE, why = "exec_not_unique", checks = ck))
  }
  it <- hits[[1]]
  add("exec_unique", TRUE, paste0("唯一在案任务 ", eid), "state/exec_tasks.json")

  # 2) 身份一致性：session / step / run_dir
  bsid <- dc_chr(b$session_id); bstep <- dc_chr(b$step); brd <- dc_chr(b$run_dir)
  ok_sid <- identical(bsid, dc_chr(it$session_id))
  ok_step <- identical(bstep, dc_chr(it$step))
  ok_rd <- !nzchar(brd) || !nzchar(dc_chr(it$run_dir)) || identical(brd, dc_chr(it$run_dir))
  add("identity", ok_sid && ok_step && ok_rd,
      paste0("session=", ok_sid, " step=", ok_step, " run_dir=", ok_rd),
      paste0("exec=", eid, " step=", bstep))
  if (!(ok_sid && ok_step && ok_rd))
    return(out(FALSE, why = "identity_mismatch", checks = ck))

  # 3) 实际脚本路径：规范化后必须与「本阶段完整展开路径」和「这条 exec 的派工记录」
  #    同时严格相等。只比 basename 不足以排除同名不同目录的脚本。
  sp <- dc_chr(b$script_path)
  if (!nzchar(sp)) {
    add("script_path", FALSE, "回写没有 script_path，无法核对跑的是哪一份", "回写 script_path")
    return(out(FALSE, why = "no_script_path", checks = ck))
  }
  got <- dc_norm_path(sp)
  task_script <- dc_norm_path(dc_chr(it$script))
  stage_script <- dc_norm_path(as.character(exp$scripts[1] %||% ""))

  # 自动物种识别包装入口（script 字段是 "-e"）：
  #   这里只做一件事 —— 把派工记录里那个"真实目标脚本"解析出来并核验包装摘要，
  #   然后把它当成 task_script，让下面的完整阶段路径、script_md5、
  #   基线/时间一致性检查照原样继续跑。
  #   不提前 return TRUE：那是绕过基线检查，等于把"路径看起来对"当成"运行期间没被换过"。
  if (identical(dc_chr(it$script), "-e")) {
    wx <- dc_wrapper_expect(it)
    if (is.null(wx)) {
      add("wrapper_parse", FALSE, "派工记录里的包装参数解析不出来", "exec.script_args")
      return(out(FALSE, why = "wrapper_parse_failed", checks = ck))
    }
    add("wrapper_parse", TRUE,
        paste0("包装入口解析成功：目标 ", wx$target, "，令牌数 ", wx$n_tokens),
        "exec.script_args（严格 shell 分词）")
    wm <- tolower(dc_chr(b$script_wrapper_md5))
    ok_wm <- grepl("^[0-9a-f]{32}$", wm) && identical(wm, tolower(wx$wrapper_md5))
    add("wrapper_md5", ok_wm,
        paste0("回写 ", if (nzchar(wm)) wm else "（未给）", " / 派工 ", wx$wrapper_md5),
        "exec.script_args 的包装代码")
    if (!ok_wm) return(out(FALSE, why = "wrapper_md5_mismatch", checks = ck))
    # 用已核验的真实目标替换派工记录里的 "-e"，后续按普通脚本路径检查继续跑。
    task_script <- dc_norm_path(wx$target)
    add("wrapper_target", TRUE, paste0("目标脚本 ", task_script),
        "exec.script_args 的 --target")
  }

  # Versioned doublet scripts are allowed only by the application registry AND
  # this exact queued contract. Legacy tasks keep their original stage path.
  is_audit <- identical((it$params %||% list())$doublet_audit_version, "v1")
  if (is_audit) {
    reg <- exp$audit_registry
    target <- "/share/home/hhhong1/rachel_doublet_audit_v1/scripts/soupX_doublefinder.audit.v1.R"
    map <- function(x) {
      if (!is.list(x) || !length(x) || is.null(names(x)) || anyDuplicated(names(x)) ||
          any(!nzchar(names(x)))) return(NULL)
      if (!all(vapply(x,function(h) is.character(h) && length(h)==1L &&
                     !is.na(h) && grepl("^[0-9a-f]{64}$",h),TRUE))) return(NULL)
      x[sort(names(x))]
    }
    expected <- map(reg)
    queued <- map((it$params %||% list())$doublet_audit_scripts)
    ev <- b$doublet_evidence
    observed <- NULL
    src <- ev$script_sources
    if (is.list(src) && length(src)) {
      paths <- vapply(src,function(x) dc_chr(x$path),"")
      if (all(nzchar(paths)) && !anyDuplicated(paths))
        observed <- map(setNames(lapply(src,function(x) x$sha256),paths))
    }
    expected_paths <- paste0(dirname(target),"/",c("soupX_doublefinder.audit.v1.R",
      "RunDF.audit.v1.R","audit_helpers.R","01_packages.original.R","observe_doublet.R"))
    ok_registry <- identical(step_id,"doublet") && !is.null(expected) &&
      setequal(names(expected),expected_paths) && identical(expected,queued) &&
      identical(expected,observed) && identical(task_script,target) &&
      identical(got,target) && identical(ev$status,"complete") &&
      isTRUE(ev$manifest_files_verified)
    add("audit_script_registry",ok_registry,
        "核对注册的五份脚本完整路径/SHA256、派工合同与实读manifest来源", "flow_spec / exec.params / doublet_evidence")
    if (!isTRUE(ok_registry)) return(out(FALSE,why="audit_registry_mismatch",checks=ck))
    ok_identity <- is.list(ev$identity) && all(vapply(c("session_id","exec_id","req_id","run_dir"),function(k) {
      val <- if(k=="exec_id") it$id else it[[k]]
      nzchar(dc_chr(val)) && identical(dc_chr(val),dc_chr(b[[k]])) &&
        identical(dc_chr(val),dc_chr(ev$identity[[k]]))
    },TRUE))
    add("audit_identity",ok_identity,"审计来源绑定本次session/exec/req/run_dir", "manifest.identity")
    if (!isTRUE(ok_identity)) return(out(FALSE,why="audit_identity_mismatch",checks=ck))
    # No mtime-only fallback for new audit tasks.
    base0 <- it$script_baseline
    if (!is.list(base0) || !nzchar(dc_chr(base0$script_md5 %||% base0$md5)))
      return(out(FALSE,why="audit_baseline_missing",checks=ck))
    stage_script <- target
  }

  # ---- 系统命令环节：只有 pack，且规范与派工都必须恰好是 zip ----
  #   为什么需要这一条：pack 由 flow_spec 声明成系统命令（script = "zip"），
  #   它不是文件路径，硬套"绝对路径"判据会让正常回写永远过不了。
  #   但放开必须窄到不能再窄，否则就成了"任何字符串都能当脚本"：
  #     · 只认 step == "pack"（其它环节一律仍要求绝对路径脚本）
  #     · flow_spec 声明的命令必须是 zip
  #     · 本次派工记录里的命令也必须是 zip
  #     · 回写里的 script_path 必须逐字等于 zip
  #     · script_real_path 必须是绝对路径，且 basename 为 zip
  #       —— 这一项是"实际跑的是哪个程序"的证据，缺了不算绑定
  #     · script_md5 必须是该二进制文件内容的 md5（由执行端在集群上实算后报来）。
  #       判定层看不到集群磁盘，无法自己复算；因此这一项的可信度建立在
  #       "real_path 已给出、md5 由该路径算出"这条链上，二者缺一即拒。
  #       绝不允许用"命令名 + 路径"之类的拼接值当指纹。
  if (!dc_is_abs(got)) {
    is_cmd_step <- identical(as.character(step_id %||% ""), "pack") &&
      identical(got, "zip") &&
      identical(stage_script, "zip") &&
      identical(task_script, "zip")
    add("script_path_command", is_cmd_step,
        if (is_cmd_step) paste0("系统命令环节：", got)
        else paste0("不是绝对路径，也不是允许的系统命令：", got,
                    "（step=", as.character(step_id %||% ""),
                    " 规范=", stage_script, " 派工=", task_script, "）"),
        sp)
    if (!is_cmd_step) return(out(FALSE, why = "script_path_not_absolute", checks = ck))

    real <- dc_norm_path(dc_chr(b$script_real_path))
    ok_real <- dc_is_abs(real) && identical(basename(real), "zip")
    add("script_real_path", ok_real,
        if (ok_real) paste0("解析到可执行文件 ", real)
        else paste0("script_real_path 需为绝对路径且指向 zip，实际：",
                    if (nzchar(real)) real else "（未提供）"),
        real)
    if (!ok_real) return(out(FALSE, why = "no_script_real_path", checks = ck))

    smd5 <- tolower(dc_chr(b$script_md5))
    ok_md5 <- grepl("^[0-9a-f]{32}$", smd5)
    add("script_md5", ok_md5,
        if (ok_md5) paste0("二进制内容 md5=", smd5, "（来自 ", real, "）")
        else "回写没有该二进制文件的 32 位 md5",
        paste0("回写 script_md5；real_path=", real))
    if (!ok_md5) return(out(FALSE, why = "no_script_md5", checks = ck))

    return(out(TRUE, set_id = exp$skill, provenance = "application_dispatch_command",
               why = paste0("系统命令环节 pack：命令 zip 与 flow_spec 及派工记录一致，",
                            "实际二进制 ", real, "（md5 ", smd5, "）"),
               checks = ck))
  }
  add("script_path_absolute", TRUE, got, sp)

  ok_stage <- nzchar(stage_script) && identical(got, stage_script)
  add("script_path_stage", ok_stage,
      paste0("实际 ", got, " / 本阶段 ",
             if (nzchar(stage_script)) stage_script else "（未取到）"),
      "engine/flow_spec.R 的 stage$script 展开后")
  if (!ok_stage) return(out(FALSE, why = "script_path_mismatch_stage", checks = ck))

  ok_task <- nzchar(task_script) && identical(got, task_script)
  add("script_path_dispatch", ok_task,
      paste0("实际 ", got, " / 派工 ",
             if (nzchar(task_script)) task_script else "（该项未记录脚本）"),
      paste0("state/exec_tasks.json 的 ", eid, ".script"))
  if (!ok_task) return(out(FALSE, why = "script_path_mismatch_dispatch", checks = ck))

  # 4) 脚本指纹必须存在
  smd5 <- tolower(dc_chr(b$script_md5))
  ok_md5 <- grepl("^[0-9a-f]{32}$", smd5)
  add("script_md5", ok_md5,
      if (ok_md5) paste0("md5=", smd5) else "回写没有合法 32 位 md5",
      "回写 script_md5")
  if (!ok_md5) return(out(FALSE, why = "no_script_md5", checks = ck))

  # 5) 与派工前基线比对（新任务走这条）
  #    基线由执行端在提交 qsub 之前读取并上报，收尾时再读一次比对。
  #    只有这样才能说明"运行期间没变"；只在完成时报一个 md5 说明不了。
  base <- it$script_baseline %||% NULL
  if (!is.null(base) && is.list(base) && nzchar(dc_chr(base$md5 %||% base$script_md5))) {
    b_path <- dc_norm_path(dc_chr(base$script_path))
    b_md5 <- tolower(dc_chr(base$script_md5 %||% base$md5))
    b_ep <- suppressWarnings(as.numeric(dc_chr(base$script_mtime_epoch)))
    add("baseline_path", identical(got, b_path),
        paste0("收尾 ", got, " / 基线 ", b_path), "exec.script_baseline")
    if (!identical(got, b_path))
      return(out(FALSE, why = "baseline_path_mismatch", checks = ck))
    add("baseline_md5", identical(smd5, b_md5),
        paste0("收尾 ", smd5, " / 基线 ", b_md5), "exec.script_baseline")
    if (!identical(smd5, b_md5))
      return(out(FALSE, why = "script_changed_since_dispatch", checks = ck))
    got_ep <- dc_script_epoch(b)
    ok_ep <- is.finite(got_ep) && is.finite(b_ep) && identical(as.numeric(got_ep), as.numeric(b_ep))
    add("baseline_mtime", ok_ep,
        paste0("收尾 ", ifelse(is.na(got_ep), "未给", format(as.POSIXct(got_ep, origin = "1970-01-01", tz = "Asia/Shanghai"), "%Y-%m-%dT%H:%M:%S")),
               " / 基线 ", ifelse(is.na(b_ep), "未给", format(as.POSIXct(b_ep, origin = "1970-01-01", tz = "Asia/Shanghai"), "%Y-%m-%dT%H:%M:%S"))),
        "exec.script_baseline")
    if (!ok_ep)
      return(out(FALSE, why = "script_mtime_changed_since_dispatch", checks = ck))
    return(out(TRUE, set_id = exp$skill, provenance = "application_dispatch_baseline",
               why = paste0("派工前基线与收尾一致：路径、md5、mtime 三项均相同（",
                            b_md5, "）"),
               checks = ck))
  }

  # 6) 兼容路径：这条 exec 没有派工前基线（早于基线机制上线就已在跑）
  #    只能核到：绝对路径严格相等、脚本 mtime 早于派工时间、产物与退出码齐全。
  #    无法证明"运行期间从未变动" —— 这一点必须在结论里写明，不许含糊。
  got_ep <- dc_script_epoch(b)
  dispatched <- dc_parse_ts(dc_chr(it$claimed_at))
  if (is.na(dispatched)) dispatched <- dc_parse_ts(dc_chr(it$created_at))
  if (is.na(got_ep)) {
    add("script_mtime", FALSE, "回写没有脚本修改时间，兼容路径也判不了",
        "回写 script_mtime")
    return(out(FALSE, why = "no_script_mtime", checks = ck))
  }
  if (is.na(dispatched)) {
    add("script_mtime", FALSE, "派工记录没有可用时间，无法比对", "state/exec_tasks.json")
    return(out(FALSE, why = "no_dispatch_time", checks = ck))
  }
  mt <- as.POSIXct(got_ep, origin = "1970-01-01", tz = "Asia/Shanghai")
  ok_mt <- mt <= dispatched
  add("script_mtime", ok_mt,
      paste0("脚本 mtime ", format(mt, "%Y-%m-%dT%H:%M:%S"),
             " / 派工 ", format(dispatched, "%Y-%m-%dT%H:%M:%S"),
             "（mtime 早于派工即视为运行前就存在的版本）"),
      "回写 script_mtime 与 exec.claimed_at")
  if (!ok_mt) return(out(FALSE, why = "script_changed_during_run", checks = ck))

  return(out(TRUE, set_id = exp$skill, provenance = "legacy_no_dispatch_hash",
             why = paste0("兼容路径 legacy_no_dispatch_hash：这条 exec 在基线机制上线前就已派工，",
                          "没有派工前脚本指纹。已核：绝对路径与派工记录及本阶段定义严格相等、",
                          "脚本 mtime 早于派工时间、产物与退出码另行校验。",
                          "审计局限：无法证明运行期间脚本从未变动"),
             checks = ck))
}
