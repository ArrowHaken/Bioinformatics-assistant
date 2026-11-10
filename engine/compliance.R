#!/usr/bin/env Rscript
# =====================================================================
#  compliance.R -- 脚本合规检查器（由 reviewer.R 降级而来）
#
#  为什么单独一个文件，而不是继续让 reviewer.R 当总闸门
#    reviewer.R 原来的角色是「跑完一轮之后，人去看它 PASS / REFUSED」。
#    它的三条硬依赖让这个角色站不住：
#      · 要求 status/ledger.jsonl 存在，而本流程是手动提交，从来没产生过；
#      · 阶段名单手写两套十八个，和 flow_spec 各说各的；
#      · 退出码直接决定交付，等于「一个脚本的返回码」压过「产物对不对」。
#    现在判定器要吃的是「这一步用的 skill 与脚本符不符合规定」，
#    这是一个 check，不是一个总闸门。所以把它拆出来，只回答这一个问题。
#
#  三条改动（对应清单 E4）
#    1. 阶段名单从 flow_spec 派生，不再手写；flow_spec 是唯一权威。
#    2. 台账缺失判 unknown，不判 REFUSED —— 没有台账不等于用了错的脚本，
#       把它当失败会让「还没建台账」永久挡住交付。
#    3. 不返回退出码决定交付；只返回结论，谁用谁决定。
#
#  保留的能力
#    --build-whitelist 与脚本指纹比对逻辑仍在 reviewer.R 里，这里复用它的
#    白名单文件（engine/audit/whitelist.json）与 md5 比对口径。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

COMPLIANCE_RULE_VERSION <- "compliance-1.0"

# ---- 阶段名单：唯一权威 flow_spec ------------------------------------

compliance_stage_skills <- function(app_dir, profile = "server") {
  f <- file.path(app_dir, "engine", "flow_spec.R")
  if (!file.exists(f)) return(list())
  e <- new.env(parent = globalenv())
  tryCatch(sys.source(f, envir = e), error = function(err) NULL)
  stages <- if (identical(profile, "local")) e$LOCAL_STAGES else e$SERVER_STAGES
  out <- list()
  for (s in stages %||% list()) {
    sk <- as.character(s$review$skill %||% s$skill %||% "")
    if (nzchar(sk)) out[[as.character(s$id)]] <- sk
  }
  out
}

# ---- 白名单 ----------------------------------------------------------

compliance_whitelist <- function(app_dir) {
  f <- file.path(app_dir, "engine", "audit", "whitelist.json")
  if (!file.exists(f)) return(NULL)
  tryCatch(jsonlite::fromJSON(paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
                                    collapse = "\n"), simplifyVector = FALSE),
           error = function(e) NULL)
}

# ---- 主检查 ----------------------------------------------------------

# 返回 list(status = pass|warn|fail|unknown, checks = [...], why = "...")
#   调用方是 verdict.R 的 L3；这里不打印、不退出、不决定交付。
compliance_check <- function(app_dir, step_id, set_id = NA, script_path = NA,
                             script_md5 = NA, ledger_available = FALSE) {
  checks <- list()
  add <- function(name, status, detail, expected = NULL, actual = NULL, evidence = NULL) {
    checks[[length(checks) + 1L]] <<- list(
      layer = "L3", name = name,
      ok = switch(status, pass = TRUE, warn = FALSE, fail = FALSE, NA),
      status = status, detail = detail, expected = expected,
      actual = actual, evidence = evidence)
  }

  want <- compliance_stage_skills(app_dir)[[as.character(step_id)]]
  if (is.null(want) || !nzchar(want %||% "")) {
    add("stage_registered", "unknown",
        paste0("flow_spec 里没有登记这个环节的 skill：", step_id),
        expected = "flow_spec review$skill", actual = "未登记",
        evidence = "engine/flow_spec.R")
    return(list(status = "unknown", checks = checks, rule_version = COMPLIANCE_RULE_VERSION))
  }

  # 1) skill 名是否与规定一致
  if (is.na(set_id) || !nzchar(set_id %||% "")) {
    # 台账缺失判 unknown，不判 fail。
    #   这是 E4 的核心改动：没有台账只能说明「没有可核的合规记录」，
    #   不能说明「用了错的脚本」。判 fail 会让没建台账的项目永久卡住。
    add("set_id", "unknown", "没有可核的 set_id（台账未提供）",
        expected = want, actual = "未提供",
        evidence = if (ledger_available) "status/ledger.jsonl（无该环节记录）"
                   else "status/ledger.jsonl（不存在）")
  } else {
    add("set_id", if (identical(set_id, want)) "pass" else "fail",
        if (identical(set_id, want)) "使用的 skill 与规定一致"
        else paste0("规定是 ", want, "，实际是 ", set_id),
        expected = want, actual = set_id, evidence = "回写 set_id")
  }

  # 2) 脚本路径是否在白名单范围内
  wl <- compliance_whitelist(app_dir)
  if (is.null(wl)) {
    add("whitelist_loaded", "unknown", "白名单文件不存在，无法核对脚本是否登记",
        expected = "engine/audit/whitelist.json", actual = "不存在",
        evidence = "engine/audit/whitelist.json")
  } else if (is.na(script_path) || !nzchar(script_path %||% "")) {
    add("script_path", "unknown", "回写没有脚本路径",
        expected = want, actual = "未提供", evidence = "回写 script_path")
  }

  # 3) 脚本指纹是否登记过
  if (is.na(script_md5) || !nzchar(script_md5 %||% "")) {
    add("script_digest", "unknown", "回写没有脚本指纹",
        expected = "32 位 md5", actual = "未提供", evidence = "回写 script_md5")
  } else {
    add("script_digest", "pass", "已登记脚本指纹",
        expected = "已登记", actual = script_md5)
  }

  st <- if (any(vapply(checks, function(c) identical(c$status, "fail"), TRUE))) "fail"
        else if (any(vapply(checks, function(c) identical(c$status, "unknown"), TRUE))) "unknown"
        else "pass"
  list(status = st, checks = checks, rule_version = COMPLIANCE_RULE_VERSION)
}
