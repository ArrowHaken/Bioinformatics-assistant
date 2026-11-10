# =====================================================================
#  contract.R -- 单一事实源的守门人
#
#  为什么需要这个文件
#    "这张流程有哪些阶段"这一件事，在应用里被写成了五份：
#      ① engine/flow_spec.R        SERVER_STAGES        ← 真正的权威
#      ② engine/step_plan.R        CHAIN_ORDER          ← 老的八个名字
#      ③ engine/reviewer.R         STAGE_SKILL          ← 两套并列十八个
#      ④ engine/library/C_supplement/index.json  for_stage
#      ⑤ serve.R                   STEP_TASK            ← 又一份中文标题与说明
#    外加服务器上还有一份独立副本 _engine/。
#
#    每加一个环节，要改五处；漏一处不会报错，只会让某一处"不认识"它。
#    于是表现出来的就是"这一步永远没有质检""阶段名不对""引擎里没有这条
#    执行路径"—— 全是同一件事的不同说法。补丁之所以补不完，是因为
#    根本没有一个地方能回答"现在到底有几份说法，差在哪"。
#
#  这个模块只做一件事
#    以 flow_spec 为唯一权威，把其余几份逐个对账，逐条指出
#    「哪个文件、缺哪个阶段 / 多哪个阶段、该改哪一段」。
#    它不替任何人自动改文件 —— 自动改会让"权威"变成"最后写入者"，
#    那是另一种失控。
#
#  判定原则
#    读不到 = unknown，不是 pass。
#    对不上 = fail，并给出可执行的那一句修改说明。
#    源清单本身范围就不同的（例如 C_supplement 只登记补充脚本），
#    按它的语义判：它引用的阶段必须存在，但不要求它覆盖全部阶段。
# =====================================================================

`%||%` <- if (exists("%||%", mode = "function")) `%||%` else
  function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---- 从 R 文本里抽「键 = "值"」形式的条目 -----------------------------
#   为什么不 source：reviewer.R / step_plan.R source 时会执行主程序，
#   那是拉一个会真跑审核的进程，不是读一张表。
contract_keys_in_block <- function(path, open_pat, close_pat = "^\\s*\\)\\s*$") {
  if (!file.exists(path))
    return(list(ok = FALSE, keys = character(0), why = paste0("文件不存在：", path)))
  txt <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
                  error = function(e) NULL)
  if (is.null(txt)) return(list(ok = FALSE, keys = character(0), why = "读不出来"))
  i0 <- grep(open_pat, txt)
  if (!length(i0)) return(list(ok = FALSE, keys = character(0), why = "找不到定义段"))
  i0 <- i0[1]
  rest <- txt[(i0 + 1):length(txt)]
  i1 <- which(grepl(close_pat, rest))
  blk <- if (length(i1)) rest[seq_len(i1[1] - 1)] else rest
  m <- regmatches(blk, regexec("^\\s*([A-Za-z0-9_.]+)\\s*=\\s*[\"\']", blk))
  keys <- vapply(m, function(x) if (length(x) >= 2) x[2] else NA_character_, "")
  keys <- keys[!is.na(keys)]
  # 去掉常见的外层容器名（例如 SERVER_STAGES <- list( 里的下一层 list(）
  keys <- keys[!grepl("^(list|stage|subs)$", keys)]
  list(ok = TRUE, keys = unique(keys), why = "")
}

# ---- 从 c(...) 里抽阶段名 --------------------------------------------
contract_chr_vector <- function(path, name) {
  if (!file.exists(path))
    return(list(ok = FALSE, keys = character(0), why = paste0("文件不存在：", path)))
  txt <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
                  error = function(e) NULL)
  if (is.null(txt)) return(list(ok = FALSE, keys = character(0), why = "读不出来"))
  i0 <- grep(paste0("^\\s*", name, "\\s*<-\\s*c\\("), txt)
  if (!length(i0)) return(list(ok = FALSE, keys = character(0), why = "找不到定义"))
  i0 <- i0[1]
  line <- txt[i0]
  if (!grepl("\\)", line)) {
    j <- i0 + 1
    while (j <= length(txt) && !grepl("\\)", txt[j])) { line <- paste(line, txt[j]); j <- j + 1 }
    if (j <= length(txt)) line <- paste(line, txt[j])
  }
  inner <- sub("^[^(]*\\(", "", line)
  inner <- sub("\\).*$", "", inner)
  keys <- regmatches(inner, gregexpr("\"([^\"]+)\"", inner))[[1]]
  keys <- gsub("^\"|\"$", "", keys)
  list(ok = TRUE, keys = unique(keys), why = "")
}

# ---- 权威名单 --------------------------------------------------------
contract_authority <- function(profile = "server") {
  if (!exists("FLOW_STAGES", mode = "function"))
    return(list(ok = FALSE, ids = character(0), why = "flow_spec.R 没加载"))
  st <- tryCatch(FLOW_STAGES(profile), error = function(e) NULL)
  if (is.null(st)) return(list(ok = FALSE, ids = character(0), why = "取不到阶段表"))
  ids <- vapply(st, function(s) as.character(s$id %||% ""), "")
  ids <- ids[nzchar(ids)]
  list(ok = length(ids) > 0, ids = ids,
       why = if (!length(ids)) "阶段表是空的" else "")
}

# ---- 读 reviewer.R 的实际阶段表 --------------------------------------
#   为什么要单独一个函数（2026-11-03，审核第 5 条）
#     2026-11-02 起 STAGE_SKILL 不再是手写的 list，而是从 flow_spec 派生的
#     函数式定义：STAGE_SKILL <- (function() { ... })()
#     对账器还在用「找 STAGE_SKILL <- list(」的正则去抽，于是一定抽不到，
#     每次打开页面都报一条「读不出来：找不到定义段」。
#     这条是误报 —— 实跑 reviewer.R --list-stages 能正常列出 18 个阶段。
#     更糟的是它配套的 fix 文案：让你"在 STAGE_SKILL 里补上手写项"。
#     照做等于把刚统一掉的唯一权威又拆回两份，回到
#     「figs 写 liny-figs、实际跑的是 sc-plot-kit」那个老坑。
#
#   判定顺序
#     ① 还是 list 形式 → 按原样抽（兼容老写法，不动）
#     ② 是派生形式     → 它和权威同源，不存在"对不上"的可能；
#                        直接采用权威名单，标 derived，不去假装核出差异
#     ③ 都不是         → unknown，并说清为什么
#
#   为什么不干脆起子进程实跑 --list-stages：那是每次刷新页面都拉一个 R 进程，
#   为一条本可以静态判定的事实付 200ms 以上的代价。派生式与 flow_spec 同源，
#   这是可以静态确认的（看它读的就是 flow_spec），不需要实跑。
contract_reviewer_stage_skill <- function(path) {
  if (!file.exists(path))
    return(list(ok = FALSE, keys = character(0), derived = FALSE,
                why = paste0("文件不存在：", path)))
  txt <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
                  error = function(e) NULL)
  if (is.null(txt)) return(list(ok = FALSE, keys = character(0), derived = FALSE, why = "读不出来"))

  # ① 老写法：STAGE_SKILL <- list(
  as_list <- contract_keys_in_block(path, "^STAGE_SKILL\\s*<-\\s*list\\(")
  if (isTRUE(as_list$ok))
    return(list(ok = TRUE, keys = as_list$keys, derived = FALSE, why = ""))

  # ② 派生写法：STAGE_SKILL <- (function() ... flow_spec ...)()
  i0 <- grep("^\\s*STAGE_SKILL\\s*<-\\s*\\(function\\s*\\(", txt)
  if (!length(i0)) i0 <- grep("^\\s*STAGE_SKILL\\s*<-\\s*function\\s*\\(", txt)
  if (!length(i0))
    return(list(ok = FALSE, keys = character(0), derived = FALSE,
                why = "既不是 list 形式，也不是从 flow_spec 派生 —— 需要人来定它该长什么样"))
  # 派生块读到第一个顶格赋值为止，够判断它读的是什么。
  end <- length(txt)
  later <- which(seq_along(txt) > i0[1] & grepl("^[A-Za-z_.][A-Za-z0-9_.]*\\s*<-", txt))
  if (length(later)) end <- later[1] - 1L
  blk <- paste(txt[i0[1]:end], collapse = "\n")
  if (!grepl("flow_spec\\.R", blk, fixed = FALSE))
    return(list(ok = FALSE, keys = character(0), derived = FALSE,
                why = "是派生写法，但读的不是 flow_spec.R —— 那就还是第二份事实，要核"))
  # 它自己就是从权威读出来的，所以"覆盖"这件事天然成立。
  list(ok = TRUE, keys = character(0), derived = TRUE,
       why = "派生自 flow_spec.R，与权威同源；不再另存一份名单，故无可比对的差异")
}

# ---- 对账 ------------------------------------------------------------
#   sources: 命名的副本清单，每项 list(label=, kind=, path=, ...)
#   返回每条差异：where（哪个文件哪一段）、missing（缺）、extra（多）、fix（怎么改）
contract_check <- function(engine_dir, profile = "server",
                          step_task_keys = NULL,
                          server_snapshot = NULL) {
  auth <- contract_authority(profile)
  out <- list(ok = TRUE, profile = profile,
              authority = if (auth$ok) as.list(auth$ids) else list(),
              sources = list(), issues = list())

  if (!auth$ok) {
    out$ok <- FALSE
    out$issues[[length(out$issues) + 1]] <- list(
      level = "fail", where = "engine/flow_spec.R · SERVER_STAGES",
      what = paste0("权威名单读不出来：", auth$why),
      fix = "先修 flow_spec.R；其余几份都以它为准，它坏了就无从对账。")
    return(out)
  }
  A <- auth$ids

  # ① step_plan.R CHAIN_ORDER —— 老流程的八个名字
  sp <- contract_chr_vector(file.path(engine_dir, "step_plan.R"), "CHAIN_ORDER")
  # ② reviewer.R STAGE_SKILL —— 可能是 list，也可能是从 flow_spec 派生
  rv <- contract_reviewer_stage_skill(file.path(engine_dir, "reviewer.R"))
  # ③ serve.R STEP_TASK（运行时对象，直接读内存里的更准）
  sk <- if (!is.null(step_task_keys)) list(ok = TRUE, keys = as.character(step_task_keys), why = "")
        else list(ok = FALSE, keys = character(0), why = "未提供")

  # legacy：这份名单属于另一个入口（老的 sc_workflow.R 那条链），
  #   本应用根本不走它。它的缺失不算不一致 —— 但必须写清楚它属于谁、
  #   什么时候才需要动它。否则只有两种坏结局：
  #   ① 把它当权威，于是本流程每一步都"不存在"；
  #   ② 干脆改掉它，于是另一个入口悄悄坏掉。
  add <- function(label, kind, res, note = "", legacy = FALSE) {
    src <- list(label = label, kind = kind, ok = isTRUE(res$ok),
                count = length(res$keys), keys = as.list(res$keys),
                why = res$why %||% "", legacy = isTRUE(legacy))
    out$sources[[length(out$sources) + 1]] <<- src
    if (!isTRUE(res$ok)) {
      out$issues[[length(out$issues) + 1]] <<- list(
        level = "unknown", where = label, what = paste0("读不出来：", res$why),
        fix = note)
      out$ok <<- FALSE
      return(invisible(NULL))
    }
    if (identical(kind, "covers")) {
      # 派生式：它就是从权威读出来的，不存在"少覆盖"这回事。
      #   不跳过的话 res$keys 是空的，会被判成"少了全部阶段" ——
      #   把一条误报升级成一条更大的误报，比原来更糟。
      if (isTRUE(res$derived)) {
        out$sources[[length(out$sources)]]$note <- res$why %||% ""
        return(invisible(NULL))
      }
      miss <- setdiff(A, res$keys)
      if (length(miss)) {
        out$issues[[length(out$issues) + 1]] <<- list(
          level = if (isTRUE(legacy)) "legacy" else "fail",
          where = label,
          what = paste0(if (isTRUE(legacy)) "属于另一个入口，不覆盖本流程：" else "少了这些阶段：",
                        paste(miss, collapse = "、")),
          # stages：这条差异具体卡住哪几步。
          #   以前对账结论只画页面那个红框，不参与放行 ——
          #   于是"1 处不一致"可以挂一个月，12 个阶段照跑不误。
          #   现在每条 fail 写清它卡哪几步，闸门只拦这几步，
          #   不做全局硬闸门（那会把现有流程全卡死）。
          stages = as.list(miss),
          fix = note)
        if (!isTRUE(legacy)) out$ok <<- FALSE
      }
    } else if (identical(kind, "subset")) {
      extra <- setdiff(res$keys, A)
      if (length(extra)) {
        out$issues[[length(out$issues) + 1]] <<- list(
          level = "fail", where = label,
          what = paste0("引用了不存在的阶段：", paste(extra, collapse = "、")),
          stages = as.list(extra),
          fix = note)
        out$ok <<- FALSE
      }
    }
    invisible(NULL)
  }

  # CHAIN_ORDER 是"这条链跑哪些步"，按要求必须覆盖全部阶段。
  # 老的入口链：env/decide/precheck/pipeline1/pipeline2/plots/ppt/pack。
  #   它服务的是 sc_workflow.R 那套（analysis 项目的 env→pack），
  #   本应用这条 liny 链不走它。标成 legacy 而不是 fail，是因为
  #   "它不认识本流程"是事实、也是正确的 —— 错的是拿它当权威。
  add("engine/step_plan.R · CHAIN_ORDER", "covers", sp,
      "这份名单属于老入口 sc_workflow.R 那条链，本应用不用它；只有当你切回那个入口跑 env→pack 时才需要动，平时别改 —— 改了会把那个入口弄坏。",
      legacy = TRUE)
  # STAGE_SKILL 是"每个阶段该用哪套 skill"，必须覆盖本流程的每一步。
  #   2026-11-03：它已改成从 flow_spec 派生，改这里不再是唯一正确动作 ——
  #   如果它还是手写 list，就补名字；如果它已经是派生，那这条永远不会有差异，
  #   真出现差异说明派生被改坏了，要修的是派生逻辑而不是这张表。
  add("engine/reviewer.R · STAGE_SKILL", "covers", rv,
      "先看它是不是从 flow_spec 派生：是派生的就不用补名字（补了会变回第二份事实）；是手写 list 的，才补上这些阶段对应的 skill 名（与 flow_spec 的 skill 字段逐字一致）。")
  # STEP_TASK 是"给助手的中文交代"，少一项就等于那一步没有交代。
  add("serve.R · STEP_TASK", "covers", sk,
      "在 STEP_TASK 里补上这一项的 title / ask / writes，否则下发的指令里那一步没有可执行说明。")

  # C_supplement 只登记"补充脚本"，语义是"引用必须存在"，不要求覆盖全部。
  ij <- file.path(engine_dir, "library", "C_supplement", "index.json")
  if (file.exists(ij)) {
    ent <- tryCatch({
      raw_txt <- paste(readLines(ij, warn = FALSE, encoding = "UTF-8"), collapse = "")
      jsonlite::fromJSON(raw_txt, simplifyVector = FALSE)
    }, error = function(e) NULL)
    if (is.null(ent)) {
      out$issues[[length(out$issues) + 1]] <- list(
        level = "unknown", where = "engine/library/C_supplement/index.json",
        what = "JSON 解析失败", fix = "修好这个文件的 JSON 语法。")
      out$ok <- FALSE
    } else {
      fs <- vapply(ent$entries %||% list(), function(e) as.character(e$for_stage %||% ""), "")
      fs <- fs[nzchar(fs)]
      out$sources[[length(out$sources) + 1]] <- list(
        label = "engine/library/C_supplement/index.json · for_stage",
        kind = "subset", ok = TRUE, count = length(fs), keys = as.list(unique(fs)), why = "")
      extra <- setdiff(unique(fs), A)
      if (length(extra)) {
        out$issues[[length(out$issues) + 1]] <- list(
          level = "fail", where = "engine/library/C_supplement/index.json · for_stage",
          what = paste0("挂在没有的阶段上：", paste(extra, collapse = "、")),
          fix = "把这些条目的 for_stage 改成 flow_spec 里真实存在的阶段 id，或删掉该条。")
        out$ok <- FALSE
      }
    }
  }

  # 服务器副本：给了快照才比对，没给就明说"没核"，不能默认通过。
  if (!is.null(server_snapshot)) {
    sids <- as.character(server_snapshot$chain_order %||% character(0))
    src <- list(label = "服务器 _engine/step_plan.R · CHAIN_ORDER", kind = "covers",
                ok = isTRUE(server_snapshot$readable), count = length(sids),
                keys = as.list(sids), why = as.character(server_snapshot$why %||% ""))
    out$sources[[length(out$sources) + 1]] <- src
    if (!isTRUE(server_snapshot$readable)) {
      out$issues[[length(out$issues) + 1]] <- list(
        level = "unknown", where = src$label,
        what = paste0("没取到：", src$why),
        fix = "让执行端上报一次服务器引擎快照；取不到就不能说服务器那边一致。")
      out$ok <- FALSE
    } else {
      miss <- setdiff(A, sids)
      if (length(miss)) {
        # 服务器上那份是老入口的引擎。它不认识新阶段，是必然的。
        #   真正的错是"让它的判断参与放行"—— 那件事已经在应用侧撤掉了。
        #   这里只如实记一条，提示"别指望它认识"，不再判为不一致。
        out$issues[[length(out$issues) + 1]] <- list(
          level = "legacy", where = src$label,
          what = paste0("服务器引擎是老入口版本，不认识：", paste(miss, collapse = "、")),
          fix = "不用改它；本应用的放行判断已改由应用侧质检负责。只有当某一步必须由服务器入口启动时，才需要把引擎同步过去。")
      }
    }
  } else {
    # 没上报过就明说"没核"，不能默认通过 —— 这一条正是上一次误判的温床：
    #   应用以为服务器那份=自己这份，实际人家是十月的另一份。
    out$sources[[length(out$sources) + 1]] <- list(
      label = "服务器 _engine · 快照", kind = "snapshot", ok = FALSE,
      count = 0, keys = list(), why = "还没上报过")
    out$issues[[length(out$issues) + 1]] <- list(
      level = "unknown", where = "服务器 _engine · 快照",
      what = "服务器上那份引擎是什么版本，目前没有记录",
      fix = "执行端跑完会带一次快照回来（POST /api/engine/snapshot）；在那之前，服务器那份与应用的差异属于未知，不能当作一致。")
    # 读不到 = unknown，不是 pass。这条原则写在文件开头，这里必须照办：
    #   否则"没核过"会被当成"核过且一致"，那正是上一次误判的温床。
    #   注意：它只影响结论 ok，不卡任何一步 —— 闸门只拦 level=fail。
    out$ok <- FALSE
  }

  out
}
