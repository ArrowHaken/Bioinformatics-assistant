#!/usr/bin/env Rscript
# =====================================================================
#  reviewer.R -- 交付前审计：这一轮到底用了哪个 skill、哪条脚本
#
#  为什么要有它
#    守卫（guard.R）只能拦住「直接 Rscript 调底层脚本」这一种绕过。
#    它拦不住另一种更隐蔽的情况：有人（包括 AI）另写一份脚本，
#    产出看起来正常的图，然后当成交付物交出去。
#    那种情况下没有台账、没有指纹，事后无法判断对错。
#
#    本文件的立场：执行层做不到「绝对禁止 AI 写脚本」，
#    但可以做到「不合规的产物交不出去」——审核不过就不许交付。
#
#  两道检查（对每个阶段）
#    1. 是不是用了对应的 skill   —— 台账 set_id 落在该阶段规定的 skill 内
#    2. 是不是用了对应的脚本     —— 脚本路径 + MD5 与白名单逐一比对
#    另外附带：工作目录一致、退出码 0、产物存在且指纹未变
#
#  用法
#    Rscript reviewer.R --build-whitelist --engine <engine>   # 生成白名单
#    Rscript reviewer.R --engine <engine> --dir <run-dir>     # 审计整轮
#    Rscript reviewer.R --engine <engine> --dir <run-dir> --stage plots
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

# %||% 必须在本文件里自己定义一次。
#   为什么：脚本是 Rscript 单独起的进程，不经过 serve.R / step_plan.R 的 source，
#   那边定义了不等于这边有。缺了它的表现是「跑到第 147 行才报
#   could not find function "%||%"」—— 前面检查都通过了，看起来像审核通过，
#   实际是崩溃。审核工具自己崩掉，是最坏的一种"看起来正常"。
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

args <- commandArgs(trailingOnly = TRUE)
get_opt <- function(k, d = NULL) {
  i <- match(k, args)
  if (is.na(i) || i == length(args)) return(d)
  args[i + 1]
}
has_flag <- function(k) k %in% args

need_jsonlite <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("缺少 jsonlite 包。安装：install.packages('jsonlite')", call. = FALSE)
}

# 阶段 -> 应当使用的 skill
# 键名必须与 step_plan.R 的 CHAIN_ORDER 逐字一致：
#   主流程在链上是两格（pipeline1 / pipeline2），要分开登记，
#   这里写成 pipeline 就会永远匹配不到，质检形同虚设。
#
# 2026-10-29 扩充：原来这张表只有 sc-pipeline 那一套（env/decide/precheck/
#   pipeline1/pipeline2/plots/ppt/pack），而服务器上真正跑的这条链是
#   doublet → merge → markers → filter → final → fullmark → diff →
#   annotate → figs → enrich。两套阶段名对不上，于是：
#     · 本轮任何一个阶段都进不了检查范围（names(STAGE_SKILL) 里没有它们）；
#     · 用 --stage 点名本轮阶段时，查不到规定 skill，判成"未知"直接失败。
#   这就是"这一步没有质检"的真正原因——不是没跑，是质检根本不认识这些阶段名。
#   现在两套并列登记，各自按自己的名字匹配。
# 阶段 -> 应当使用的 skill
#
#  2026-11-02 起：不再手写这张表，从 flow_spec.R 派生。
#    为什么必须改：这张表原来手写两套十八个名字，而 flow_spec 里也有一份。
#    两处各说各的，出现过的真实后果是：
#      · figs 在 flow_spec 里是 sc-plot-kit，这里写 liny-figs —— 审计时
#        按 liny-figs 去核，永远核不出问题，因为压根没跑那个；
#      · 新增 ppt / pack 之后，这里没有对应项，于是「这一步没有质检」。
#    这不是「补两条」能解决的，是规则的唯一权威没统一。
#    现在只读 flow_spec：改流程只需要改一处，审计跟着变。
STAGE_SKILL <- (function() {
  # engine 目录要从「本脚本的位置 + --engine 参数」自己解一遍。
  #   为什么不能直接用 engine_dir：这张表定义在 engine_dir 解析之前
  #   （原来的手写表不需要目录，派生需要）。第一版就是这么写错的 ——
  #   运行期 engine_dir 还不存在，file.path() 拿到 character(0)，
  #   于是阶段数恒为 0，而报错被 tryCatch 吞掉，看起来「派生成功但一条都没有」。
  #   这种静默为空比报错危险得多：审计会以为所有阶段都不受约束。
  ed <- (function() {
    a <- commandArgs(trailingOnly = FALSE)
    hit <- sub("^--file=", "", a[grep("^--file=", a)])
    if (length(hit)) return(normalizePath(dirname(hit[1]), winslash = "/", mustWork = FALSE))
    normalizePath(".", winslash = "/", mustWork = FALSE)
  })()
  ta <- commandArgs(trailingOnly = TRUE)
  i <- match("--engine", ta)
  if (!is.na(i) && i < length(ta)) ed <- normalizePath(ta[i + 1], winslash = "/", mustWork = FALSE)
  f <- file.path(ed, "flow_spec.R")
  if (!file.exists(f)) return(list())
  e <- new.env(parent = globalenv())
  ok <- tryCatch({ sys.source(f, envir = e); TRUE }, error = function(err) FALSE)
  if (!ok) return(list())
  out <- list()
  for (prof in c("SERVER_STAGES", "LOCAL_STAGES")) {
    stages <- e[[prof]] %||% list()
    for (st in stages) {
      id <- as.character(st$id %||% "")
      if (!nzchar(id)) next
      # review$skill 优先：那是判定器实际读的那一处。
      sk <- as.character((st$review %||% list())$skill %||% st$skill %||% "")
      if (nzchar(sk)) out[[id]] <- sk
    }
  }
  out
})()

# 白名单要扫的目录（相对 engine）
SCAN_DIRS <- c("sc-pipeline/scripts", "sc-plot-kit/scripts", "sc-ppt-kit/scripts", "workflow")

md5_of <- function(p) {
  if (!file.exists(p)) return(NA_character_)
  unname(tools::md5sum(p))
}

rel_to <- function(p, base) {
  p <- normalizePath(p, winslash = "/", mustWork = FALSE)
  base <- normalizePath(base, winslash = "/", mustWork = FALSE)
  sub(paste0("^", gsub("([\\W])", "\\\\\\1", base), "/?"), "", p)
}

engine_dir <- get_opt("--engine", ".")
run_dir    <- get_opt("--dir", NULL)
only_stage <- get_opt("--stage", NULL)
need_jsonlite()
engine_dir <- normalizePath(engine_dir, winslash = "/", mustWork = TRUE)

MANIFEST <- file.path(engine_dir, "audit", "whitelist.json")

# ---------------- 自查：打印派生出来的阶段表 ----------------
if (has_flag("--list-stages")) {
  cat("engine:", engine_dir, "\n", sep = "")
  cat("阶段数:", length(STAGE_SKILL), "\n", sep = "")
  if (!length(STAGE_SKILL)) {
    cat("\n[!] 一条都没派生出：flow_spec.R 没找到，或者里面没有 review$skill。\n")
    quit(save = "no", status = 2L)
  }
  for (k in names(STAGE_SKILL)) cat("  ", k, " -> ", STAGE_SKILL[[k]], "\n", sep = "")
  quit(save = "no", status = 0L)
}

# ---------------- 生成白名单 ----------------
if (has_flag("--build-whitelist")) {
  dir.create(dirname(MANIFEST), showWarnings = FALSE, recursive = TRUE)
  files <- character(0)
  for (d in SCAN_DIRS) {
    full <- file.path(engine_dir, d)
    if (!dir.exists(full)) next
    hit <- list.files(full, pattern = "\\.(R|r|py|sh|json)$", recursive = TRUE, full.names = TRUE)
    hit <- hit[!grepl("/audit/|legacy_reference", hit)]
    files <- c(files, hit)
  }
  files <- sort(unique(files))
  entries <- lapply(files, function(f) {
    list(path = rel_to(f, engine_dir), md5 = md5_of(f), size = as.integer(file.info(f)$size))
  })
  out <- list(
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    engine   = engine_dir,
    digests  = "md5",
    files    = entries
  )
  writeLines(jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE), MANIFEST, useBytes = TRUE)
  cat("白名单已生成：", MANIFEST, "\n", sep = "")
  cat("收录文件：", length(entries), " 个\n", sep = "")
  for (e in entries) cat("  ", e$path, "  ", e$md5, "\n", sep = "")
  quit(save = "no", status = 0L)
}

# ---------------- 审计 ----------------
if (is.null(run_dir)) stop("缺 --dir <运行目录>", call. = FALSE)
run_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)

fail <- character(0)
warn <- character(0)
pass <- character(0)
unknown <- character(0)

add_fail <- function(...) fail <<- c(fail, paste0(...))
# unknown：判不了。责任可能不在被执行方，所以不能当成 fail 计入结论，
#   但要单独列出来，让看的人知道「这一项没核」。
add_unknown <- function(...) unknown <<- c(unknown, paste0(...))
add_pass <- function(...) pass <<- c(pass, paste0(...))
add_warn <- function(...) warn <<- c(warn, paste0(...))

if (!file.exists(MANIFEST)) {
  add_fail("白名单不存在：", MANIFEST, "（先跑 --build-whitelist）")
  wl <- list()
} else {
  wl <- jsonlite::fromJSON(MANIFEST, simplifyVector = FALSE)
}
wl_map <- list()
for (e in wl$files) wl_map[[e$path]] <- e$md5

ledger <- file.path(run_dir, "status", "ledger.jsonl")
if (!file.exists(ledger)) {
  # 台账缺失判 unknown，不判 fail。
  #   为什么：没有台账只说明「没有可核的合规记录」，不能说明「用了错的脚本」。
  #   判 fail 的后果是：一个从没建过台账的项目永久卡在 REFUSED，
  #   而它可能什么都没做错。清单 E4 明确要求这一条。
  add_unknown("台账不存在：", ledger, "（没有可核记录，不代表用了错脚本）")
  entries <- list()
} else {
  lines <- readLines(ledger, warn = FALSE, encoding = "UTF-8")
  entries <- lapply(lines[nzchar(lines)], function(l)
    tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE), error = function(e) NULL))
  entries <- Filter(Negate(is.null), entries)
}

stages <- if (is.null(only_stage)) names(STAGE_SKILL) else strsplit(only_stage, ",")[[1]]

cat("\n============================================================\n")
cat("  交付前审计\n")
cat("  运行目录：", run_dir, "\n", sep = "")
cat("  引擎目录：", engine_dir, "\n", sep = "")
cat("  台账条目：", length(entries), "\n", sep = "")
cat("============================================================\n\n")

for (sid in stages) {
  want_skill <- STAGE_SKILL[[sid]] %||% NA
  cat("[", sid, "] 规定 skill：", want_skill, "\n", sep = "")

  st <- Filter(function(e) identical(e$step, sid) && identical(e$event, "start"), entries)
  fn <- Filter(function(e) identical(e$step, sid) && identical(e$event, "finish"), entries)

  if (!length(st)) {
    add_fail(sid, "：台账里没有 start 记录 —— 这一步没经过统一入口")
    cat("  ✗ 没有 start 记录\n\n"); next
  }
  e <- st[[length(st)]]

  # ① skill 检查
  set_id <- e$set_id %||% ""
  if (!nzchar(set_id) || identical(set_id, "unknown")) {
    add_fail(sid, "：台账没记录用的是哪套脚本（set_id 为空）")
    cat("  ✗ 未记录 skill/脚本套\n")
  } else if (!is.na(want_skill) && !grepl(want_skill, set_id, fixed = TRUE)) {
    add_fail(sid, "：用的是 ", set_id, "，规定是 ", want_skill)
    cat("  ✗ skill 不符：", set_id, "\n", sep = "")
  } else {
    add_pass(sid, "：skill=", set_id)
    cat("  ✓ skill = ", set_id, "\n", sep = "")
  }

  # ② 脚本检查：白名单 + 指纹
  hit_scripts <- Filter(function(p) {
    grepl(paste0("^", (STAGE_SKILL[[sid]] %||% ""), "/"), p)
  }, names(wl_map))
  if (!length(hit_scripts)) {
    add_warn(sid, "：白名单里没有该 skill 的脚本条目")
  } else {
    changed <- character(0)
    missing <- character(0)
    for (p in hit_scripts) {
      full <- file.path(engine_dir, p)
      if (!file.exists(full)) { missing <- c(missing, p); next }
      if (!identical(md5_of(full), wl_map[[p]])) changed <- c(changed, p)
    }
    if (length(missing)) add_fail(sid, "：脚本缺失 ", paste(missing, collapse = ", "))
    if (length(changed)) add_fail(sid, "：脚本被改动 ", paste(changed, collapse = ", "))
    if (!length(missing) && !length(changed)) {
      add_pass(sid, "：", length(hit_scripts), " 条脚本指纹一致")
      cat("  ✓ 脚本指纹一致（", length(hit_scripts), " 条）\n", sep = "")
    } else {
      cat("  ✗ 脚本与白名单不符\n")
    }
  }

  # ③ 工作目录一致性
  if (!identical(normalizePath(e$run_dir %||% "", winslash = "/", mustWork = FALSE), run_dir)) {
    add_fail(sid, "：台账记录的运行目录与本次不一致")
    cat("  ✗ 运行目录不一致\n")
  } else {
    cat("  ✓ 运行目录一致\n")
  }

  # ④ 完成状态与产物指纹
  if (!length(fn)) {
    add_fail(sid, "：没有 finish 记录 —— 跑完了吗？")
    cat("  ✗ 没有 finish 记录\n\n"); next
  }
  f <- fn[[length(fn)]]
  if (!identical(as.integer(f$code %||% 1L), 0L)) {
    add_fail(sid, "：退出码 ", f$code)
    cat("  ✗ 退出码 ", f$code, "\n", sep = "")
  } else {
    cat("  ✓ 退出码 0\n")
  }
  fps <- f$fingerprints %||% list()
  if (!length(fps)) {
    add_warn(sid, "：没有产物指纹")
    cat("  ! 无产物指纹\n")
  } else {
    bad <- character(0)
    for (fp in fps) {
      p <- file.path(run_dir, fp$rel)
      if (!file.exists(p)) { bad <- c(bad, paste0(fp$rel, "(缺失)")); next }
      alg <- fp$alg %||% "md5"
      cur <- if (identical(alg, "md5")) md5_of(p) else md5_of(p)
      if (!is.null(fp$hash) && !identical(cur, fp$hash)) bad <- c(bad, paste0(fp$rel, "(已改)"))
    }
    if (length(bad)) {
      add_fail(sid, "：产物不一致 ", paste(bad, collapse = ", "))
      cat("  ✗ 产物与台账不符\n")
    } else {
      add_pass(sid, "：", length(fps), " 个产物指纹一致")
      cat("  ✓ 产物指纹一致（", length(fps), " 个）\n", sep = "")
    }
  }
  cat("\n")
}

cat("============================================================\n")
cat("  通过 ", length(pass), " 项\n", sep = "")
if (length(warn)) { cat("  提醒 ", length(warn), " 项\n", sep = ""); for (w in warn) cat("    ! ", w, "\n", sep = "") }
# unknown 必须单列：它是「这一项没核」，不是「这一项错了」。
#   混进 fail 会导致没人做错事却拿不到通过；混进 pass 更糟 ——
#   等于把没核过的当核过了。清单 E4 / I3 都要求这个区分。
if (length(unknown)) {
  cat("  判不了 ", length(unknown), " 项\n", sep = "")
  for (x in unknown) cat("    ? ", x, "\n", sep = "")
}

# ---- 结论不再由退出码决定交付 ----
#   这一条是 E4 的核心：reviewer 的退出码原来直接决定「能不能交」。
#   那等于让一个审计脚本的返回码压过产物本身对不对。
#   现在它只给结论，交付由 verdict.R + 交付门判定。
if (length(fail)) {
  cat("  不通过 ", length(fail), " 项\n", sep = "")
  for (x in fail) cat("    ✗ ", x, "\n", sep = "")
  cat("\n  本文件结论：FAIL（合规层未通过）。\n")
  cat("  注意：这不等于交付被拒。能不能交由 /api/deliverable 判，\n")
  cat("        那里会读这一层的结论，并连同产物与判定一起看。\n")
  cat("============================================================\n")
  quit(save = "no", status = 0L)
}
if (length(unknown)) {
  cat("\n  本文件结论：UNKNOWN（有一 ", length(unknown), " 项没有可核记录）。\n", sep = "")
  cat("  没有台账不等于用了错脚本；这一条会进交付清单的未核验项。\n")
  cat("============================================================\n")
  quit(save = "no", status = 0L)
}
cat("\n  本文件结论：PASS（对应 skill + 对应脚本）。\n")
cat("============================================================\n")
quit(save = "no", status = 0L)
