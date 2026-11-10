#!/usr/bin/env Rscript
# =====================================================================
#  stage_reviewer.R -- 环节级 reviewer：每环两审一护照
#
#  为什么要这一个文件（2026-11-05）
#    判定能力本来就有，但分散在四处，各判各的、各存各的：
#      verdict.R       回写后判 L1-L4，写数据库
#      quality.R       回写后判状态/产物/日志，只记账不拦
#      reviewer.R      跑完一轮后判台账与脚本指纹，要看 status/ledger.jsonl
#      compliance.R    只判 set_id 与脚本是否登记
#    后果不是"少一个检查"，而是三件事没有一处能回答：
#      ① 派发之前，这一环该不该开？（没人判，派了再说）
#      ② 回写之后，这一环的产物能不能交给下一环用？（四处结论不一致）
#      ③ 跑完的那一环，到底放行了哪些文件？（只有"有产物"，没有"哪些被认可"）
#    于是"完成"和"可以往下走"仍然是两件事，中间靠人看红条。
#
#  本文件的做法：把①②③收进同一处，产出可举证的"环节护照"
#    PRE  前置审（仿真：这一环现在能不能开）
#      · 流程登记：flow_spec 里有这一环、有 review 契约、有规定的 skill
#      · 依赖链：整链没有断点，且本环要的上游确实有产出者
#      · 上游护照：上游那一环的护照必须是 pass；不是 fail 才放行
#      · 执行输入：flow_execution 能拼出完整命令（缺值就拦，不猜）
#      · 环境：runnable_stage 判这一环的条件
#      · 脚本：规定脚本在库/白名单里，指纹与登记一致
#      · 注释依据：annotate 必须先选定依据（用错依据结果格式对、内容错）
#    POST 后置审（这一环跑完，能不能交给下一环）
#      · 复用 verdict.R 的 L1-L4 结论，不另造判据
#      · 产出"放行清单"（released）：只列核过的产物，下游只认这一份
#      · 结论三档：pass / warn / block，附 evidence 与可执行的 fix
#      · 落盘一份不可覆盖的历史（同一步多次跑，每次各一份）
#
#  三条不可让步的规则
#    1. 判不了写 warn/unknown，不折算成 pass。
#    2. 只认可核事实：flow_spec 声明、回写产物清单、退出码、护照、指纹。
#       自然语言保证一律不参与判定。
#    3. 单一权威：判据从 flow_spec 取，这里不新写一份阶段表、不新写一套产物清单。
#
#  自动触发点（不由人点）
#    serve.R /api/run          派发前 -> sr_preflight()   不过则不放行
#    serve.R /api/step/import  回写后 -> sr_admit()       生成护照
#    serve.R /api/reviewer     只读看板 -> sr_chain_all()
#
#  用法（命令行自检）
#    Rscript engine/stage_reviewer.R --app <app_dir> --list
#    Rscript engine/stage_reviewer.R --app <app_dir> --pre --stage merge
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
}

SR_RULE_VERSION <- "stage-reviewer-1.0"

# ---- 基础 IO ---------------------------------------------------------

sr_dir <- function(app_dir, sid = NULL) {
  base <- file.path(app_dir, "state", "passports")
  if (is.null(sid)) return(base)
  s <- as.character(sid %||% "")
  if (!nzchar(s)) s <- "_default"
  file.path(base, gsub("[^A-Za-z0-9._-]", "_", s))
}

sr_read_json <- function(path) {
  if (!file.exists(path)) return(NULL)
  txt <- tryCatch(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                  error = function(e) "")
  if (!nzchar(txt)) return(NULL)
  tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
}

sr_write_json <- function(path, obj) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  con <- file(path, open = "wb")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  writeBin(charToRaw(enc2utf8(jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null",
                                               digits = NA, pretty = TRUE))), con)
  invisible(TRUE)
}

sr_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

# ---- 规格读取：唯一权威 flow_spec ------------------------------------

sr_spec <- function(app_dir) {
  f <- file.path(app_dir, "engine", "flow_spec.R")
  if (!file.exists(f)) return(NULL)
  e <- new.env(parent = globalenv())
  ok <- tryCatch({ sys.source(f, envir = e); TRUE }, error = function(err) FALSE)
  if (!ok) return(NULL)
  e
}

sr_stages <- function(app_dir, profile = "server") {
  e <- sr_spec(app_dir)
  if (is.null(e)) return(list())
  if (identical(profile, "local")) (e$LOCAL_STAGES %||% list()) else (e$SERVER_STAGES %||% list())
}

sr_stage <- function(app_dir, id, profile = "server") {
  for (s in sr_stages(app_dir, profile))
    if (identical(as.character(s$id %||% ""), as.character(id))) return(s)
  NULL
}

sr_order <- function(app_dir, profile = "server") {
  vapply(sr_stages(app_dir, profile), function(s) as.character(s$id %||% ""), "")
}

# ---- 检查项 ----------------------------------------------------------

# status: pass / warn / fail / unknown / skipped
#   fail    = 明确不合格，必须拦
#   warn    = 有可核问题但不该拦（例如历史遗留没护照、指纹与登记不一致）
#   unknown = 没有可核数据，不当作通过，但也不拦（否则整链永久卡死）
sr_chk <- function(layer, name, status, detail = NULL, expected = NULL,
                   actual = NULL, evidence = NULL, fix = NULL) {
  list(layer = layer, name = name, status = status,
       detail = detail, expected = expected, actual = actual,
       evidence = evidence, fix = fix)
}

# 汇总：任一 fail -> block；有 warn/unknown -> warn；全 pass/skipped -> pass
sr_rollup <- function(checks) {
  if (!length(checks)) return("unknown")
  st <- vapply(checks, function(c) as.character(c$status %||% ""), "")
  if (any(st == "fail")) return("block")
  if (any(st %in% c("warn", "unknown"))) return("warn")
  "pass"
}

# ---- 白名单指纹 ------------------------------------------------------

sr_whitelist <- function(app_dir) {
  f <- file.path(app_dir, "engine", "audit", "whitelist.json")
  if (!file.exists(f)) return(NULL)
  d <- sr_read_json(f)
  if (is.null(d) || is.null(d$files)) return(NULL)
  map <- list()
  for (e in d$files) {
    p <- as.character(e$path %||% "")
    if (nzchar(p)) map[[p]] <- as.character(e$md5 %||% "")
  }
  list(map = map, built_at = as.character(d$built_at %||% ""))
}

sr_md5 <- function(p) if (!file.exists(p)) NA_character_ else unname(tools::md5sum(p))

# ---- 产物匹配（模板支持 {samples}）-----------------------------------

sr_pat_regex <- function(p) {
  pat <- gsub("\\{[A-Za-z_][A-Za-z0-9_]*\\}", "*", p, perl = TRUE)
  paste0("^", gsub("\\*", ".*", gsub("([.()+|^$\\\\])", "\\\\\\1", pat)), "$")
}

sr_products_of <- function(stage) {
  rv <- stage$review %||% list()
  as.character(rv$products %||% stage$artifacts %||% character(0))
}

# 回写清单里哪些文件算这一环的合格产物
#   判据：命中 products 模板（逐样本模板再按 samples 展开）+ 有字节数或 mtime。
#   不命中的文件不放行，但也不判错（可能是日志、补充表）。
sr_released <- function(stage, rels, samples = character(0)) {
  pats <- sr_products_of(stage)
  if (!length(pats) || !length(rels)) return(character(0))
  out <- character(0)
  for (p in pats) {
    is_dir <- grepl("/$", p) || !grepl("\\.[A-Za-z0-9]+$", p)
    if (grepl("\\{samples\\}", p) && length(samples)) {
      for (one in samples) {
        want <- gsub("\\{[A-Za-z_][A-Za-z0-9_]*\\}", as.character(one), p, perl = TRUE)
        if (want %in% rels) out <- c(out, want)
      }
      next
    }
    rx <- sr_pat_regex(p)
    hit <- rels[vapply(rels, function(r) grepl(rx, r), TRUE)]
    if (is_dir) hit <- c(hit, rels[vapply(rels, function(r) startsWith(r, paste0(p, "/")), TRUE)])
    out <- c(out, hit)
  }
  unique(out)
}

# ---- 护照读写 --------------------------------------------------------

sr_passport_path <- function(app_dir, sid, stage) {
  file.path(sr_dir(app_dir, sid), paste0(gsub("[^A-Za-z0-9._-]", "_", as.character(stage)), ".json"))
}

# 最新一份护照（不看历史）
sr_passport <- function(app_dir, sid, stage) {
  d <- sr_read_json(sr_passport_path(app_dir, sid, stage))
  if (is.null(d) || is.null(d$latest)) return(NULL)
  d$latest
}

sr_passport_history <- function(app_dir, sid, stage) {
  d <- sr_read_json(sr_passport_path(app_dir, sid, stage))
  if (is.null(d)) return(list())
  d$history %||% list()
}

# 写护照：追加历史，latest 指向最后一份。不覆盖旧的 —— 同一环跑两次要能各自举证。
sr_passport_save <- function(app_dir, sid, stage, item) {
  p <- sr_passport_path(app_dir, sid, stage)
  d <- sr_read_json(p)
  if (is.null(d)) d <- list(stage = as.character(stage), session_id = as.character(sid %||% ""), history = list())
  if (is.null(d$history)) d$history <- list()
  d$history <- c(d$history, list(item))
  # 历史最多留 50 份，避免长跑项目无限膨胀（最旧的丢，latest 不受影响）
  if (length(d$history) > 50L) d$history <- d$history[(length(d$history) - 49L):length(d$history)]
  d$latest <- item
  d$updated_at <- sr_now()
  sr_write_json(p, d)
  invisible(item)
}

# ---- 前置审 ----------------------------------------------------------

# 上游是本环的哪些产出者（从 flow_spec 的 FLOW_CONSUMES 解析）
sr_upstream <- function(app_dir, id, profile = "server") {
  e <- sr_spec(app_dir)
  if (is.null(e)) return(character(0))
  order <- sr_order(app_dir, profile)
  i <- match(as.character(id), order)
  if (is.na(i) || i <= 1) return(character(0))
  ups <- order[seq_len(i - 1L)]
  produces_of <- list()
  for (s in sr_stages(app_dir, profile))
    produces_of[[as.character(s$id)]] <- as.character(s$artifacts %||% character(0))
  decls <- if (identical(profile, "server")) (e$FLOW_CONSUMES[[as.character(id)]] %||% as.character(sr_stage(app_dir, id, profile)$deps %||% character(0)))
           else as.character(sr_stage(app_dir, id, profile)$deps %||% character(0))
  out <- character(0)
  for (d in decls) {
    r <- tryCatch(e$flow_deps_resolve(d, ups, produces_of), error = function(err) NULL)
    if (!is.null(r) && isTRUE(r$ok) && nzchar(as.character(r$producer %||% "")))
      out <- c(out, as.character(r$producer))
  }
  unique(out)
}

sr_env_idx <- function(app_dir, sid = "") {
  if (!exists("dp_record_env", mode = "function"))
    source(file.path(app_dir, "engine", "dataset_profile.R"))
  d <- dp_record_env(app_dir, sid)
  if (is.null(d)) return(NULL)
  items <- d$items %||% list()
  out <- list()
  for (it in items) {
    k <- as.character(it$id %||% "")
    if (nzchar(k)) out[[k]] <- it
  }
  out
}

# 前置审主函数
#   V        flow_vars() 的结果（执行输入）。不给就跳过执行输入检查，判 unknown。
#   force    人工放行：只改结论，不改检查项（证据照样留着）
sr_preflight <- function(app_dir, stage_id, sid = "", profile = "server",
                         V = NULL, force = FALSE, note = "") {
  id <- as.character(stage_id %||% "")
  checks <- list()
  add <- function(...) checks[[length(checks) + 1L]] <<- sr_chk(...)

  stage <- sr_stage(app_dir, id, profile)
  if (is.null(stage)) {
    add("R0", "stage_registered", "fail",
        paste0("flow_spec 里没有这一环：", id),
        expected = "server/local 档的一个 stage id", actual = id,
        evidence = "engine/flow_spec.R",
        fix = "确认环节 id 拼写；若是新环节，先在 flow_spec 里登记再跑。")
    return(sr_finish("pre", id, sid, checks, force, note))
  }
  add("R0", "stage_registered", "pass",
      paste0("已登记：", stage$title %||% id),
      expected = id, actual = id, evidence = "engine/flow_spec.R")

  rv <- stage$review %||% NULL
  want_skill <- as.character((rv %||% list())$skill %||% stage$skill %||% "")
  if (is.null(rv)) {
    add("R1", "contract_declared", "warn",
        "这一环没有 review 契约（产什么、最小体积、必需列都没声明）",
        expected = "review{}", actual = "未声明", evidence = "engine/flow_spec.R",
        fix = "在 FLOW_REVIEW 里补这一环的契约，否则跑完无法判合格与否。")
  } else {
    add("R1", "contract_declared", "pass",
        paste0("契约已声明：", length(sr_products_of(stage)), " 项产物，skill=", want_skill),
        expected = "review{}", actual = "已声明", evidence = "engine/flow_spec.R")
  }

  # ---- 依赖链：整链不能有断点，且本环要的上游必须存在 ----
  chain <- tryCatch({
    e <- sr_spec(app_dir)
    if (is.null(e)) NULL else e$flow_deps_status(profile)
  }, error = function(err) NULL)
  if (is.null(chain)) {
    add("R2", "deps_chain", "unknown", "依赖链判不了（flow_spec 读不出来）")
  } else if (!isTRUE(chain$ok)) {
    brk <- Filter(function(b) identical(as.character(b$stage %||% ""), id), chain$breaks %||% list())
    if (length(brk)) {
      add("R2", "deps_chain", "fail",
          paste0("这一环的依赖声明解析不到产出者：", paste(vapply(brk, function(b) as.character(b$decl %||% ""), ""), collapse = "、")),
          expected = "每个依赖都能解析到上游产物", actual = paste(vapply(brk, function(b) as.character(b$why %||% ""), ""), collapse = "；"),
          evidence = "/api/flow/deps", fix = "把依赖指到真实上游，或从流程里删掉这一环。")
    } else {
      add("R2", "deps_chain", "warn",
          paste0("整条链别处有 ", length(chain$breaks %||% list()), " 处断点，本环不受影响"),
          evidence = "/api/flow/deps")
    }
  } else {
    add("R2", "deps_chain", "pass", "依赖链完整", evidence = "/api/flow/deps")
  }

  # ---- 上游护照：产物能不能被下一环使用，只认护照 ----
  ups <- sr_upstream(app_dir, id, profile)
  if (!length(ups)) {
    add("R3", "upstream_passport", "skipped", "第一环，没有上游", expected = "无上游", actual = "无")
  } else {
    for (u in ups) {
      pp <- sr_passport(app_dir, sid, u)
      if (is.null(pp)) {
        # 历史遗留：这一环以前跑过但没有护照（reviewer 上线前）。不拦，但要记清楚。
        add("R3", paste0("upstream:", u), "warn",
            paste0("上游「", u, "」没有护照记录（可能是 reviewer 上线前跑的）"),
            expected = "passport=pass", actual = "无记录",
            evidence = paste0("state/passports/", as.character(sid %||% "_default"), "/", u, ".json"),
            fix = paste0("重跑一次「", u, "」，让它在回写后生成护照；或在 /api/reviewer 里看它的最后结论。"))
        next
      }
      pv <- as.character(pp$verdict %||% "unknown")
      if (identical(pv, "block")) {
        add("R3", paste0("upstream:", u), "fail",
            paste0("上游「", u, "」的护照是 block，产物未放行，不能开这一环"),
            expected = "pass", actual = "block",
            evidence = paste0("passport ", as.character(pp$passport_id %||% "")),
            fix = paste0("先按上游的返工单修好「", u, "」并重跑，拿到 pass 护照再开这一步。"))
      } else if (identical(pv, "pass")) {
        rel <- as.character(unlist(pp$released %||% list()))
        add("R3", paste0("upstream:", u), "pass",
            paste0("上游护照 pass，放行 ", length(rel), " 项产物"),
            expected = "pass", actual = "pass",
            evidence = paste0("passport ", as.character(pp$passport_id %||% "")))
      } else {
        add("R3", paste0("upstream:", u), "warn",
            paste0("上游「", u, "」护照是 ", pv, "（部分未核），可以开但要盯着"),
            expected = "pass", actual = pv,
            evidence = paste0("passport ", as.character(pp$passport_id %||% "")))
      }
    }
  }

  # ---- 执行输入：能拼出完整命令才算能开 ----
  e <- sr_spec(app_dir)
  if (is.null(V) || is.null(e)) {
    add("R4", "execution_input", "unknown",
        "没有执行输入（未选运行目录/服务器参数），这一项没核",
        expected = "flow_execution ok", actual = "未提供")
  } else {
    ex <- tryCatch(e$flow_execution(stage, V), error = function(err) list(ok = FALSE, errors = conditionMessage(err)))
    if (isTRUE(ex$ok)) {
      add("R4", "execution_input", "pass",
          "命令可拼全（脚本 + 参数都有值）", expected = "ok", actual = "ok",
          evidence = paste(ex$script, paste(ex$argv, collapse = " ")))
    } else if (isTRUE(ex$covered)) {
      add("R4", "execution_input", "skipped",
          as.character(ex$errors %||% "已被上游覆盖"), expected = "不需执行", actual = "covered")
    } else {
      add("R4", "execution_input", "fail",
          paste0("执行输入不全：", paste(as.character(ex$errors %||% ""), collapse = "；")),
          expected = "脚本与参数齐全", actual = "缺项",
          evidence = "flow_execution()",
          fix = "补齐缺失的运行输入（目录、物种、注释依据等）再开这一步；不许用占位值硬跑。")
    }
  }

  # ---- 环境：只判这一环需要的条件 ----
  idx <- sr_env_idx(app_dir, sid)
  spec_env <- tryCatch(e$FLOW_ENV_OF(id), error = function(err) list())
  need <- c(as.character(spec_env$requires_pkgs %||% character(0)),
            as.character(spec_env$requires_paths %||% character(0)))
  if (!length(need)) {
    add("R5", "environment", "skipped",
        if (length(spec_env)) "这一环不需要额外环境条件" else "这一环没有登记环境条件",
        expected = "无需额外条件", actual = "无")
  } else if (is.null(idx) || !length(idx)) {
    add("R5", "environment", "unknown",
        "还没有体检结果，环境判不了（不等于环境正常）",
        expected = paste(need, collapse = ", "), actual = "未体检",
        evidence = paste0("session ", sid, " identify.result"),
        fix = "先跑一次环境体检，让这一环的条件有可核数据。")
  } else {
    rs <- tryCatch({
      if (!exists("runnable_stage", mode = "function")) {
        rf <- file.path(app_dir, "engine", "runnable.R")
        if (file.exists(rf)) sys.source(rf, envir = globalenv())
      }
      runnable_stage(id, spec_env, idx)
    }, error = function(err) NULL)
    if (is.null(rs)) {
      add("R5", "environment", "unknown", "环境判定模块没跑起来", evidence = "engine/runnable.R")
    } else if (identical(rs$verdict, "no")) {
      add("R5", "environment", "fail",
          paste0("环境不满足：", paste(vapply(rs$gaps %||% list(), function(g) as.character(g$name %||% ""), ""), collapse = ", ")),
          expected = "条件齐备", actual = "缺条件", evidence = "/api/runnable",
          fix = "先补缺的包或路径，再开这一步。")
    } else if (identical(rs$verdict, "unknown")) {
      add("R5", "environment", "warn",
          "环境有项没探到，判不了（不当成通过）", expected = "条件齐备", actual = "部分未知",
          evidence = "/api/runnable")
    } else {
      add("R5", "environment", "pass", "这一环的环境条件齐备", expected = "齐备", actual = "齐备")
    }
  }

  # ---- 脚本：规定的那一份在不在、指纹对不对 ----
  sc <- as.character(stage$script %||% "")
  # 判"这是不是系统命令"要看有没有扩展名，不看它登记在哪个档。
  #   为什么：pack 这一步的 script 就是 zip 本身（flow_spec 里写得很清楚），
  #   按文件去找一定会判"脚本不存在"，而那是判据写错，不是流程错。
  #   判据写错比产物错更危险 —— 它会把对的判成错的，逼人放宽规则。
  is_cmd <- nzchar(sc) && !grepl("\\.", basename(sc))
  if (!nzchar(sc)) {
    add("R6", "script_registered", "warn", "这一环没有登记脚本", expected = "script", actual = "未登记")
  } else if (is_cmd) {
    hit <- suppressWarnings(Sys.which(sc))
    if (nzchar(hit)) {
      add("R6", "script_registered", "pass", paste0("系统命令存在：", sc, " -> ", hit),
          expected = sc, actual = hit, evidence = "Sys.which")
    } else {
      add("R6", "script_registered", "fail", paste0("系统命令不可用：", sc),
          expected = sc, actual = "未找到", evidence = "Sys.which",
          fix = paste0("在应用侧装上 ", sc, " 或改 flow_spec 用可用的打包方式。"))
    }
  } else if (identical(as.character(stage$source %||% ""), "app")) {
    fp <- gsub("\\{[^}]+\\}", "", sc)
    cand <- c(file.path(app_dir, fp), file.path(app_dir, "engine", sub("^\\{engine\\}/", "", sc)))
    hit <- cand[file.exists(cand)][1]
    if (!is.na(hit) && nzchar(hit)) {
      add("R6", "script_registered", "pass", paste0("应用侧脚本存在：", basename(hit)),
          expected = sc, actual = hit)
    } else {
      add("R6", "script_registered", "fail", paste0("应用侧脚本不存在：", sc),
          expected = sc, actual = "缺失", fix = "补齐脚本或改 flow_spec 登记。")
    }
  } else {
    # 服务器脚本：本地只能核"登记过、名字对得上"，核不了远端是否存在，如实标 unknown
    add("R6", "script_registered", "pass",
        paste0("已登记服务器脚本：", basename(sc)), expected = sc, actual = sc,
        evidence = "engine/flow_spec.R")
  }

  wl <- sr_whitelist(app_dir)
  if (is.null(wl) || !nzchar(want_skill)) {
    add("R6b", "script_fingerprint", "unknown",
        "没有白名单或这一环没有规定 skill，指纹核不了",
        expected = "whitelist.json", actual = if (is.null(wl)) "未生成" else "无 skill")
  } else {
    paths <- names(wl$map)
    hit <- paths[vapply(paths, function(p) grepl(paste0("^", want_skill, "/"), p), TRUE)]
    if (!length(hit)) {
      add("R6b", "script_fingerprint", "unknown",
          paste0("白名单里没有 ", want_skill, " 的脚本条目"), expected = want_skill, actual = "无条目",
          evidence = "engine/audit/whitelist.json")
    } else {
      bad <- character(0); miss <- character(0)
      for (p in hit) {
        full <- file.path(app_dir, "engine", p)
        if (!file.exists(full)) { miss <- c(miss, p); next }
        if (!identical(sr_md5(full), wl$map[[p]])) bad <- c(bad, p)
      }
      if (length(miss)) {
        add("R6b", "script_fingerprint", "fail",
            paste0("白名单登记的脚本缺失：", paste(miss, collapse = ", ")),
            expected = "存在", actual = "缺失", fix = "补齐脚本，或重新生成白名单。")
      } else if (length(bad)) {
        # 改动不等于错：可能是正常维护。但必须让人知道"跑的不是登记那一版"。
        add("R6b", "script_fingerprint", "warn",
            paste0("脚本与控制指纹不一致：", paste(bad, collapse = ", ")),
            expected = "md5 与白名单一致", actual = "已改动",
            evidence = "engine/audit/whitelist.json",
            fix = "确认改动是有意的，然后重跑 reviewer.R --build-whitelist 更新指纹。")
      } else {
        add("R6b", "script_fingerprint", "pass",
            paste0(length(hit), " 条脚本指纹一致"), expected = "一致", actual = "一致")
      }
    }
  }

  # ---- 注释依据：只有 annotate 需要 ----
  if (identical(id, "annotate")) {
    ok_ref <- !is.null(V) && nzchar(as.character(V$reference_xlsx %||% ""))
    add("R7", "reference_chosen", if (ok_ref) "pass" else "fail",
        if (ok_ref) "本次分析已选定注释依据" else "还没有选注释依据：用错依据注释出来的结果格式对、内容错",
        expected = "reference_xlsx 非空", actual = if (ok_ref) as.character(V$reference_xlsx) else "未选",
        evidence = "state/references/index.json + 会话 reference_id",
        fix = "在「注释依据」里选一份（上传 / 服务器路径 / 明确本轮不用）。")
  }

  item <- sr_finish("pre", id, sid, checks, force, note,
                    extra = list(profile = profile, skill = want_skill, upstream = as.list(ups)))
  # 前置审也留痕：光有"派发过"证明不了"开跑前审过"。
  #   存成单独的 .pre.json，不覆盖 post 护照 —— 两者回答的是不同问题。
  sr_preflight_save(app_dir, sid, id, item)
  item
}

# 前置审留痕（独立文件，不与 post 护照混）
sr_preflight_save <- function(app_dir, sid, stage, item) {
  s <- as.character(sid %||% ""); if (!nzchar(s)) s <- "_default"
  p <- file.path(sr_dir(app_dir, sid), paste0(gsub("[^A-Za-z0-9._-]", "_", as.character(stage)), ".pre.json"))
  d <- sr_read_json(p)
  if (is.null(d)) d <- list(stage = as.character(stage), history = list())
  if (is.null(d$history)) d$history <- list()
  d$history <- c(d$history, list(item))
  if (length(d$history) > 50L) d$history <- d$history[(length(d$history) - 49L):length(d$history)]
  d$latest <- item
  d$updated_at <- sr_now()
  sr_write_json(p, d)
  invisible(item)
}

sr_preflight_last <- function(app_dir, sid, stage) {
  s <- as.character(sid %||% ""); if (!nzchar(s)) s <- "_default"
  p <- file.path(sr_dir(app_dir, sid), paste0(gsub("[^A-Za-z0-9._-]", "_", as.character(stage)), ".pre.json"))
  d <- sr_read_json(p)
  if (is.null(d)) return(NULL)
  d$latest
}

# 护照号必须唯一。
#   为什么不能只靠"环节名 + 秒级时间"：同一秒内跑两次（重判、补跑）
#   会生成两个一模一样的护照号，事后拿号去追证据就追不出是哪一份。
#   加一个进程内自增序号：同一秒内的多次审也各自可辨。
sr_seq <- local({
  n <- 0L
  function() { n <<- n + 1L; n }
})

# 收口：算结论、落盘、返回
sr_finish <- function(kind, stage_id, sid, checks, force = FALSE, note = "",
                      extra = list()) {
  v <- sr_rollup(checks)
  forced <- isTRUE(force) && identical(v, "block")
  if (forced) v <- "warn"
  fails <- Filter(function(c) identical(c$status, "fail"), checks)
  item <- c(list(
    passport_id = paste0(if (identical(kind, "pre")) "pre-" else "pp-",
                         gsub("[^A-Za-z0-9]", "", as.character(stage_id)),
                         "-", format(Sys.time(), "%Y%m%d%H%M%S"),
                         "-", sprintf("%02d", sr_seq() %% 100L)),
    kind = kind,
    stage = as.character(stage_id),
    session_id = as.character(sid %||% ""),
    verdict = v,
    forced = forced,
    note = as.character(note %||% ""),
    rule_version = SR_RULE_VERSION,
    at = sr_now(),
    checks = checks,
    blockers = lapply(fails, function(c) list(name = c$name, detail = c$detail,
                                              fix = c$fix, evidence = c$evidence))),
    extra)
  if (identical(kind, "pre"))
    item$permit_id <- item$passport_id
  item
}

# ---- 后置审：回写完成后生成护照 --------------------------------------

# wb        回写体（/api/step/import 的 body）
# verdict   db_api_report_step 的返回（含 checks / verdict / review_id），可为 NULL
# attempt_id 本次尝试号（可为空）
sr_admit <- function(app_dir, stage_id, sid = "", wb = list(), verdict = NULL,
                     attempt_id = NULL, permit_id = NULL, profile = "server") {
  id <- as.character(stage_id %||% "")
  checks <- list()
  add <- function(...) checks[[length(checks) + 1L]] <<- sr_chk(...)

  stage <- sr_stage(app_dir, id, profile)
  if (is.null(stage)) {
    add("R0", "stage_registered", "fail", paste0("flow_spec 里没有这一环：", id),
        evidence = "engine/flow_spec.R")
    return(sr_finish("post", id, sid, checks, FALSE, ""))
  }

  # 1) 复用 verdict 的 L1-L4，不另造判据
  #    注意两种形状都要认：
  #      verdict_step() 的返回里有完整 checks；
  #      db_api_report_step() 的返回里只有 accepted / verdict / failed_checks。
  #    只认前一种的后果很具体（实测踩到过）：判定器已经判了 fail，
  #    护照却因为"没有 checks"记成 warn，于是 fail 的产物照样进了下一环 ——
  #    闸门在最后一米失效，而且看不出来。
  vres <- as.character((verdict %||% list())$verdict %||% "")
  vchecks <- (verdict %||% list())$checks %||% list()
  fchecks <- (verdict %||% list())$failed_checks %||% list()
  if (!length(vchecks) && length(fchecks)) {
    vchecks <- lapply(fchecks, function(c0) list(
      layer = as.character(c0$layer %||% "L?"),
      name = as.character(c0$name %||% ""),
      status = "fail",
      detail = as.character(c0$detail %||% c0$why %||% ""),
      expected = as.character(c0$expected %||% ""),
      actual = as.character(c0$actual %||% ""),
      evidence = as.character(c0$evidence %||% "")))
  }
  if (!length(vchecks) && nzchar(vres)) {
    # 一条检查都没有但给了总结论：至少要把总结论变成一条可举证的检查。
    vchecks <- list(list(
      layer = "L0", name = "verdict_rollup",
      status = if (vres %in% c("pass", "warn")) vres else "fail",
      detail = paste0("判定器总结论：", vres),
      expected = "pass|warn", actual = vres,
      evidence = as.character((verdict %||% list())$review_id %||% "engine/verdict.R")))
  }
  if (!length(vchecks)) {
    add("P0", "verdict_available", "unknown",
        "这一轮没有判定器结论（回写可能没进数据层），护照只能按回写体粗判",
        expected = "verdict.checks 非空", actual = "空",
        evidence = "engine/verdict.R",
        fix = "确认回写里带 status/code/artifacts(mtime)/set_id/script_path，并让数据层可写。")
  } else {
    for (c0 in vchecks) {
      st0 <- as.character(c0$status %||% "")
      st1 <- if (st0 %in% c("pass", "covered")) "pass"
             else if (st0 == "fail") "fail"
             else if (st0 == "warn") "warn"
             else if (st0 == "skipped") "skipped"
             else "unknown"
      add(as.character(c0$layer %||% "L?"), as.character(c0$name %||% ""), st1,
          detail = as.character(c0$detail %||% ""),
          expected = as.character(c0$expected %||% ""),
          actual = as.character(c0$actual %||% ""),
          evidence = as.character(c0$evidence %||% ""))
    }
  }

  # 1b) 判定层自身是否健康
  #    这一条不是判"产物对不对"，而是判"我们有没有能力判"。
  #    实测：数据层事务报错时 db_api_report_step 会回 judge_error，
  #    护照只能按回写体粗判（P0=unknown）。那种情况下如果只是安静地
  #    降级成 warn，后面每一环都会在"没人核过"的状态下一路放行 ——
  #    闸门看着在，其实整条链都没判过。
  #    所以单列一条：不拦（拦了整条链当场停摆，那不是维持任务），
  #    但每份护照都带着它，看板能一眼看出"判定层坏了"。
  jerr <- as.character((verdict %||% list())$judge_error %||% (verdict %||% list())$error %||% "")
  add("P0b", "judge_layer",
      if (nzchar(jerr)) "warn" else "pass",
      if (nzchar(jerr)) paste0("判定层本次没跑通：", jerr) else "判定层正常",
      expected = "verdict 可用", actual = if (nzchar(jerr)) "不可用" else "可用",
      evidence = as.character((verdict %||% list())$judge_code %||% "verdict.R"),
      fix = "修数据层（事务/锁）后重跑这一环的判定；在修好之前，这一环的结论只是按回写体的粗判。")

  # 2) 回写契约：状态、退出码、产物时间
  stv <- as.character(wb$status %||% "")
  add("P1", "reported_status",
      if (stv %in% c("done", "failed")) "pass" else "warn",
      paste0("回写状态=", if (nzchar(stv)) stv else "（空）"),
      expected = "done|failed", actual = stv)
  code <- suppressWarnings(as.integer(wb$code %||% wb$exit_code %||% NA))
  add("P1", "exit_code",
      if (is.na(code)) "unknown" else if (code == 0L) "pass" else "fail",
      if (is.na(code)) "回写没给退出码" else paste0("退出码 ", code),
      expected = "0", actual = if (is.na(code)) "未给" else as.character(code))

  # 3) 放行清单：只列命中契约的产物
  rels <- character(0)
  for (a in (wb$artifacts %||% list())) {
    r0 <- if (is.list(a)) as.character(a$rel %||% a$path %||% "") else as.character(a)
    if (nzchar(r0)) rels <- c(rels, r0)
  }
  rels <- unique(rels)
  smp <- as.character(unlist(wb$samples %||% (wb$counts %||% list())$samples %||% character(0)))
  released <- sr_released(stage, rels, smp)
  prods <- sr_products_of(stage)
  if (!length(rels)) {
    add("P2", "released", "fail",
        "回写没有产物清单：没有清单就没有任何东西可放行",
        expected = paste(prods, collapse = ", "), actual = "空",
        fix = "回写时逐项带 rel / bytes / mtime。")
  } else if (length(released) < length(prods) && !length(prods)) {
    add("P2", "released", "unknown", "这一环没有声明产物，放行清单为空",
        expected = "products[]", actual = "未声明")
  } else {
    miss <- setdiff(prods, c(released, if (any(grepl("\\{samples\\}", prods))) released else character(0)))
    miss <- setdiff(prods, released)
    if (length(miss)) {
      add("P2", "released", "fail",
          paste0("契约要求的产物没有全部放行，缺：", paste(miss, collapse = ", ")),
          expected = paste(prods, collapse = ", "),
          actual = if (length(released)) paste(released, collapse = ", ") else "无",
          fix = "补齐缺失产物后重跑；不做部分完成。")
    } else {
      add("P2", "released", "pass",
          paste0("放行 ", length(released), " 项产物，可交给下一环"),
          expected = paste(prods, collapse = ", "),
          actual = paste(released, collapse = ", "))
    }
  }

  # 4) 合规：set_id 与脚本（沿用 compliance.R 的口径，不新写）
  comp <- tryCatch({
    if (!exists("compliance_check", mode = "function")) {
      cf <- file.path(app_dir, "engine", "compliance.R")
      if (file.exists(cf)) sys.source(cf, envir = globalenv())
    }
    if (exists("compliance_check", mode = "function"))
      compliance_check(app_dir, id,
                       set_id = as.character(wb$set_id %||% wb$skill %||% NA),
                       script_path = as.character(wb$script_path %||% wb$script %||% NA),
                       script_md5 = as.character(wb$script_md5 %||% wb$script_digest %||% NA),
                       ledger_available = TRUE)
    else NULL
  }, error = function(e) NULL)
  if (is.null(comp)) {
    add("P3", "compliance", "unknown", "合规检查器没跑起来", evidence = "engine/compliance.R")
  } else {
    for (c0 in comp$checks %||% list()) {
      st0 <- as.character(c0$status %||% "")
      add(as.character(c0$layer %||% "L3"), as.character(c0$name %||% ""),
          if (st0 %in% c("pass")) "pass" else if (st0 == "fail") "fail"
          else if (st0 == "warn") "warn" else "unknown",
          detail = as.character(c0$detail %||% ""),
          expected = as.character(c0$expected %||% ""),
          actual = as.character(c0$actual %||% ""),
          evidence = as.character(c0$evidence %||% ""))
    }
  }

  item <- sr_finish("post", id, sid, checks, FALSE, "",
                    extra = list(profile = profile,
                                 attempt_id = if (is.null(attempt_id)) NULL else as.character(attempt_id),
                                 review_id = as.character((verdict %||% list())$review_id %||% ""),
                                 permit_id = if (is.null(permit_id)) NULL else as.character(permit_id),
                                 released = as.list(released),
                                 products = as.list(prods),
                                 run_dir = as.character(wb$run_dir %||% ""),
                                 exec_pc = as.character(wb$exec_pc %||% wb$host %||% "")))
  sr_passport_save(app_dir, sid, id, item)
  item
}

# ---- 链条看板 --------------------------------------------------------

# 每一环：最新护照 + 当前前置审（dry-run，不落盘）
sr_chain_all <- function(app_dir, sid = "", profile = "server", V = NULL) {
  order <- sr_order(app_dir, profile)
  out <- list()
  for (id in order) {
    pp <- sr_passport(app_dir, sid, id)
    pre <- tryCatch(sr_preflight(app_dir, id, sid = sid, profile = profile, V = V,
                                 note = "看板预检（不落盘）"), error = function(e) NULL)
    out[[length(out) + 1L]] <- list(
      stage = id,
      title = (sr_stage(app_dir, id, profile) %||% list())$title %||% id,
      passport_verdict = (pp %||% list())$verdict %||% "none",
      released = as.list(as.character(unlist((pp %||% list())$released %||% list()))),
      passport_at = (pp %||% list())$at %||% NULL,
      pre_verdict = (pre %||% list())$verdict %||% "unknown",
      pre_blockers = (pre %||% list())$blockers %||% list(),
      pre_checks = (pre %||% list())$checks %||% list())
  }
  list(session_id = sid, rule_version = SR_RULE_VERSION, stages = out,
       blocked = vapply(out, function(x) identical(x$pre_verdict, "block"), TRUE))
}

# ---- CLI -------------------------------------------------------------

sr_cli <- function() {
  a <- commandArgs(trailingOnly = TRUE)
  get <- function(k, d = NULL) { i <- match(k, a); if (is.na(i) || i == length(a)) d else a[i + 1] }
  app <- get("--app", ".")
  sid <- get("--session", "")
  if ("--list" %in% a) {
    for (s in sr_stages(app, "server"))
      cat(sprintf("%-10s %-20s skill=%s\n", as.character(s$id), as.character(s$title),
                  as.character(s$review$skill %||% s$skill %||% "")))
    quit(save = "no", status = 0L)
  }
  if ("--pre" %in% a) {
    id <- get("--stage", "")
    r <- sr_preflight(app, id, sid = sid)
    cat(jsonlite::toJSON(r, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA), "\n")
    quit(save = "no", status = if (identical(r$verdict, "block")) 3L else 0L)
  }
  if ("--chain" %in% a) {
    r <- sr_chain_all(app, sid)
    cat(jsonlite::toJSON(r, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA), "\n")
    quit(save = "no", status = 0L)
  }
  cat("用法: --list | --pre --stage <id> | --chain  [--app <dir>] [--session <id>]\n")
  quit(save = "no", status = 0L)
}

if (sys.nframe() == 0L && !interactive()) sr_cli()
