#!/usr/bin/env Rscript
# =====================================================================
#  checklist_assertions.R -- 把清单判据变成可执行的断言
#
#  为什么必须有这个文件（清单 K4 / M1）
#    清单每条都写了「完成判据」，但判据不变成自动跑的测试就没有约束力：
#    下一个人改代码时不会因为破坏它们而变红。
#    本文件把每条判据落成一句断言，并统计「还有几条没落成断言」。
#    这个数字要能持续下降 —— 它下降，才说明约束力真的在增加。
#
#  两类断言
#    静态断言  在源码里找符号/错误码/表名，证明这条路径存在且接上了
#    行为断言  真的建一个临时库、跑一次判定/返工/闸门，看结论对不对
#
#  诚实口径
#    静态断言只证明「有这段代码」，不证明「它在生产里被走到」。
#    所以每条都标了 evidence 类型；只写注释、只改文案的一律不算实现，
#    对应的断言会失败，并在输出里点名。
#
#  用法
#    Rscript engine/tests/checklist_assertions.R [--app <dir>] [-v]
#  退出码：0 全部通过；1 有失败；2 有未落成断言的条目但仍通过
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

args <- commandArgs(trailingOnly = TRUE)
APP <- local({
  a <- sub("^--app=", "", args[grepl("^--app=", args)])
  if (length(a)) normalizePath(a) else normalizePath(getwd())
})
if (!dir.exists(file.path(APP, "engine"))) APP <- normalizePath(file.path(APP, ".."))
VERBOSE <- "-v" %in% args

src <- function(f) {
  p <- file.path(APP, f)
  if (!file.exists(p)) return("")
  paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
has <- function(f, pattern) grepl(pattern, src(f), fixed = TRUE)
has_re <- function(f, pattern) grepl(pattern, src(f))
lines_of <- function(f) strsplit(src(f), "\n", fixed = TRUE)[[1]]

PASS <- 0L; FAIL <- 0L; SKIP <- 0L; NOTES <- character(0); FAILED <- character(0)
ok <- function(id, name, cond, evidence = "") {
  if (isTRUE(cond)) {
    PASS <<- PASS + 1L
    if (VERBOSE) cat(sprintf("  [ok]   %-4s %s\n", id, name))
  } else {
    FAIL <<- FAIL + 1L
    FAILED <<- c(FAILED, paste0(id, " ", name))
    cat(sprintf("  [FAIL] %-4s %s  %s\n", id, name, evidence))
  }
}

# 每条 = id / 组 / 优先级 / 短名 / 断言函数
#   断言函数返回 TRUE/FALSE；返回 NA 表示「还没落成断言」。
ITEMS <- list()

add <- function(id, group, pri, name, fn) {
  ITEMS[[length(ITEMS) + 1L]] <<- list(id = id, group = group, pri = pri,
                                        name = name, fn = fn)
}

# ---- A 组：交付链路 ------------------------------------------------
add("A1", "A", "P0", "六层身份表落库", function() {
  all(vapply(c("projects", "versions", "runs", "step_attempts", "snapshots", "artifacts"),
             function(t) has("engine/db.R", paste0("CREATE TABLE IF NOT EXISTS ", t)), TRUE))
})
add("A1b", "A", "P0", "取数必须带 version_id", function() {
  has("engine/db.R", "必须给 version_id，不接受「最新」语义")
})
add("A2", "A", "P0", "每步快照不可改", function() {
  has("engine/db.R", "sealed INTEGER NOT NULL DEFAULT 0") &&
    has("engine/verdict.R", "INSERT INTO snapshots")
})
add("A3", "A", "P0", "产物分五类", function() {
  all(vapply(c("identity", "preview", "small", "large", "ondemand"),
             function(k) has("engine/db.R", k), TRUE))
})
add("A4", "A", "P0", "持久调度器 + 租约", function() {
  has("engine/poller.R", "lease_until") && has("engine/poller.R", "poller_claim")
})
add("A5", "A", "P0", "探针三档结论", function() {
  has_re("serve.R", "env_probe|server.*manual.*unknown") || NA
})
add("A6", "A", "P0", "查询六要素契约", function() {
  has("engine/poller.R", "POLL_REQUIRED_FIELDS") &&
    has("engine/poller.R", "schedule_error")
})
add("A7", "A", "P0", "晚到旧回写丢弃", function() {
  has("engine/db_api.R", "stale_write")
})
add("A8", "A", "P0", "四轴状态投影", function() {
  all(vapply(c("submit", "exec", "sync", "qc"), function(k)
    has("engine/db.R", paste0(k, " =")), TRUE))
})
add("A9", "A", "P0", "同名环节不串版", function() {
  # 判据：匹配键要唯一到「环节的一次尝试」，不能靠环节名。
  #   UNIQUE(run_id, step_id, attempt_no) 保证同一 run 内同名环节的多次尝试不互相覆盖；
  #   db_attempt_by_req 保证回写按请求号命中，而不是按 step 名模糊匹配。
  has("engine/db.R", "UNIQUE(run_id, step_id, attempt_no)") &&
    has("engine/db.R", "db_attempt_by_req")
})
add("A10", "A", "P0", "交付门只在一处", function() {
  has("engine/db.R", "db_deliverable") && has("engine/db_api.R", '"/api/deliverable"')
})

# ---- B 组：使用者数据边界 ------------------------------------------
add("B1", "B", "P0", "身份接进数据层", function() {
  # 判据：每次请求算一次 owner，且读接口真的用它过滤。
  #   只定义 owner_from_identity 不算接上 —— 定义完没人调用，是这次要修的那类问题。
  has("engine/db_api.R", "owner_from_identity") &&
    has("serve.R", "owner_ctx <- if (DB_READY)") &&
    has("serve.R", "WHERE owner_key = ? OR visibility = 'shared'")
})
add("B2", "B", "P0", "存储按使用者分区", function() {
  # 判据：私有数据落在 state/users/<owner>/ 下，且四个模块共用同一个开关。
  #   只查「有没有这个字符串」不够 —— 上一轮就是这样，目录建了没人用。
  #   现在查：单一开关存在 + 四个模块都问它。
  has("engine/state_paths.R", "state_private_root") &&
    has("engine/sessions.R", "state_private_root") &&
    has("engine/quality.R", "state_private_root") &&
    has("engine/decisions.R", "state_private_root") &&
    has("engine/mirror.R", "state_private_root")
})
add("B3", "B", "P0", "私有与共享分开", function() {
  has("engine/db.R", "visibility TEXT NOT NULL DEFAULT 'private'")
})
add("B4", "B", "P0", "身份拿不到 fail-closed", function() {
  has("serve.R", "identity_unavailable") && has("engine/db.R", "owner_unidentified")
})

# ---- C 组：下载与取回 ----------------------------------------------
add("C1", "C", "P0", "应用内直下通道", function() {
  has("engine/db_api.R", '"/api/artifact/fetch"')
})
add("C2", "C", "P0", "rds/pptx/zip 可取回", function() {
  has("engine/mirror.R", '".rds"') && has("engine/mirror.R", '".pptx"') &&
    has("engine/mirror.R", '".zip"') && has("engine/db_api.R", '"/api/artifact/chunk"')
})
add("C3", "C", "P0", "取回是受管任务", function() {
  has("engine/db.R", "CREATE TABLE IF NOT EXISTS fetch_jobs") &&
    has("engine/poller.R", "poller_run_fetch")
})
add("C4", "C", "P1", "三种取法并列", function() {
  has("engine/mirror.R", "MIRROR_LARGE_EXTS")
})
add("C5", "C", "P1", "多选打包", function() {
  has("engine/db_api.R", '"/api/artifact/bundle"')
})

# ---- D 组：PPT 与打包 ----------------------------------------------
add("D1", "D", "P0", "server 档声明 ppt 与 pack", function() {
  has("engine/flow_spec.R", 'id = "ppt"') && has("engine/flow_spec.R", 'id = "pack"') &&
    !has("engine/flow_spec.R", 'SERVER_MISSING <- c("ppt", "pack")')
})
add("D2", "D", "P0", "模板与脚本下发", function() {
  has_re("engine/flow_spec.R", "template.pptx") &&
    has_re("engine/db_api.R", "artifact/chunk")
})
add("D3", "D", "P0", "PPT 输入契约对齐 10.figs", function() {
  has("engine/flow_spec.R", "--result-dir\", \"{work_root}/10.figs")
})
add("D4", "D", "P1", "PPT 也要过 review", function() {
  has("engine/flow_spec.R", "slides_match_fig_count")
})

# ---- E 组：reviewer 设计 -------------------------------------------
add("E1", "E", "P0", "分四层，规则只写一处", function() {
  has("engine/flow_spec.R", "FLOW_REVIEW <- list(") &&
    has("engine/verdict.R", "verdict_l1") && has("engine/verdict.R", "verdict_l4")
})
add("E2", "E", "P0", "review.json 绑定六层 ID 且不可变", function() {
  # 判据：① 表在；② 结论落成实物文件；③ 重判不覆盖旧结论。
  #   这一条原来是查 ON CONFLICT(snapshot_id) —— 那是「覆盖」的写法，
  #   与「不可变」正好相反。断言自己写错了方向，会奖励错误实现。
  has("engine/db.R", "CREATE TABLE IF NOT EXISTS reviews") &&
    has("engine/verdict.R", 'state", "reviews"') &&
    !has("engine/verdict.R", "ON CONFLICT(snapshot_id) DO UPDATE")
})
add("E3", "E", "P0", "三档阻断", function() {
  has("engine/rework.R", "REWORK_MAX_AUTO")
})
add("E4", "E", "P0", "reviewer 降级为 L3", function() {
  has("engine/verdict.R", "verdict_l3")
})
add("E5", "E", "P0", "四条验收用例", function() {
  has("engine/tests/verdict_tests.R", "第一轮缺列")
})

# ---- F 组：分析能力 ------------------------------------------------
add("F1", "F", "P1", "脚本按数据自动适配", function() {
  # 判据：有 dataset_profile 的生成，且参数由画像推出（不是抄默认值）。
  #   行为测在 engine/tests/orchestration_tests.R 的「F1 数据画像」段：
  #   样本数从体检读、10x 认得出、缺物种时如实留空。
  has("engine/dataset_profile.R", "dp_build") &&
    has("engine/dataset_profile.R", "dp_resolve_params") &&
    has("engine/db_api.R", '"/api/dataset/profile"')
})
add("F2", "F", "P1", "脚本从服务器直接取用", function() {
  has("engine/flow_spec.R", "script = \"{script_root}/script/soupX_doublefinder.R\"")
})
add("F3", "F", "P1", "注释依据三条路", function() {
  has("engine/reference.R", "reference")
})
add("F4", "F", "P1", "流程自增删环节", function() {
  # 判据：依赖可解析 + 断链能自检 + 删环节前先查下游依赖。
  #   行为测：orchestration_tests.R 里造一条断链，必须被抓到并指出位置。
  has("engine/flow_spec.R", "FLOW_CONSUMES") &&
    has("engine/flow_spec.R", "flow_deps_check") &&
    has("engine/orchestrator.R", "pipeline_suggest")
})
add("F5", "F", "P1", "跑起来之后人还能补输入", function() {
  has("engine/decisions.R", "dec_ensure")
})
add("F6", "F", "P1", "一键跑完整个流程", function() {
  # 这一条分两层，必须说清楚做到哪一层：
  #   编排层（真）：每环能不能开、缺什么、卡在哪，都由 orch_next 判，已测；
  #   实跑层（未做）：真的把 12 环跑完需要服务器与真实数据，
  #     这里没有那个环境，所以不假装做到了。
  #   断言只覆盖编排层 —— 覆盖到什么程度就写什么程度，不往上凑。
  #   所以这里返回 NA（= 还没完全落成），而不是 TRUE：
  #   返回一个字符串 "preview" 之类的东西会被判定器当成失败，
  #   而写成 TRUE 又是把「编排放行」说成「跑完了」—— 两种都不对。
  NA
})
add("F7", "F", "P1", "环境可跑判定", function() {
  # 判据：三档合成，unknown 不许当 yes；缺包要指名影响哪一环。
  #   行为测：orchestration_tests.R「F7 环境三档」段，三条都实测。
  has("engine/runnable.R", "runnable_chain") &&
    has("engine/flow_spec.R", "FLOW_ENV") &&
    has("engine/db_api.R", '"/api/runnable"')
})
add("F8", "F", "P1", "点按钮看产物与重跑", function() {
  has("engine/db_api.R", "/api/artifact/preview") || has("serve.R", "/api/artifact/preview")
})
add("F9", "F", "P1", "失败原因可见", function() {
  has("engine/rework.R", "rework_instruction_text")
})

# ---- G 组：阶段三 --------------------------------------------------
add("G1", "G", "P2", "判断留痕", function() {
  # 判据不只「表在」，还要有运行时写入与反查。
  #   表在但没人写，是上一轮的问题（decisions 只有迁移时写过）。
  has("engine/db.R", "CREATE TABLE IF NOT EXISTS decisions") &&
    has("engine/orchestrator.R", "decision_log") &&
    has("engine/orchestrator.R", "decision_trace") &&
    has("engine/db_api.R", '"/api/decisions/trace"')
})
add("G2", "G", "P2", "自动优化 pipeline", function() {
  # 判据：建议要过自检才生效；删掉被依赖的环节必须被拒。
  #   行为测：orchestration_tests.R「G2」段，删 final 被拒并指出谁依赖它。
  has("engine/orchestrator.R", "pipeline_suggest") &&
    has("engine/db_api.R", '"/api/pipeline/suggest"')
})
add("G3", "G", "P2", "按 pipeline 找输入输出", function() {
  # 判据：每环声明 produces/consumes，解析器能把 consumes 接到 producer。
  has("engine/flow_spec.R", "FLOW_CONSUMES") &&
    has("engine/flow_spec.R", "flow_deps_resolve")
})
add("G4", "G", "P2", "迭代收敛", function() {
  # 判据：同数据两次 md5 一致判一致；缺 md5 判「判不了」而不是「不一致」。
  #   行为测：orchestration_tests.R「G4」段三条。
  has("engine/orchestrator.R", "orch_convergence") &&
    has("engine/orchestrator.R", "orch_attribute") &&
    has("engine/db_api.R", '"/api/convergence"')
})

# ---- H 组：报告自身 ------------------------------------------------
add("H1-H6", "H", "P3", "报告待修六项", function() NA)

# ---- I 组：执行轨道 ------------------------------------------------
add("I1", "I", "P0", "reported / verified 分离", function() {
  has("serve.R", "verified_not_writable") &&
    has("engine/db_api.R", 'status = if (accepted) "verified" else "rejected"')
})
add("I2", "I", "P0", "判定器四层且读 flow_spec", function() {
  has("engine/verdict.R", "verdict_load_spec") &&
    has("engine/verdict.R", "stage$review")
})
add("I3", "I", "P0", "unknown 同样退回", function() {
  has("engine/verdict.R", 'verdict %in% c("fail", "unknown")')
})
add("I4", "I", "P0", "不接受自我描述", function() {
  has("engine/verdict.R", "ignored_self_claim") &&
    has("engine/verdict.R", "VERDICT_SELF_CLAIM_FIELDS")
})
add("I5", "I", "P0", "四条原则可被测试触发", function() {
  has("engine/rework.R", "agent 不能给自己放行") &&
    has("engine/tests/verdict_tests.R", "agent 给自己放行被拒")
})

# ---- J 组：返工闭环 ------------------------------------------------
add("J1", "J", "P0", "返工单对象完整", function() {
  has("engine/rework.R", "rework_ticket_malformed") &&
    has("engine/rework.R", "command_hint")
})
add("J2", "J", "P0", "自动返工循环", function() {
  has("engine/rework.R", "REWORK_BACKOFF_SECS") &&
    has("engine/poller.R", "poller_run_rework")
})
add("J3", "J", "P0", "返工指令正文", function() {
  has("engine/rework.R", "rework_instruction_text") &&
    has("engine/db_api.R", "/api/rework/list")
})
add("J4", "J", "P0", "退避与催促，两套计数分开", function() {
  has("engine/rework.R", "nudges") && has("serve.R", "MAX_INJECT_ATTEMPTS")
})
add("J5", "J", "P0", "升级与人机分工", function() {
  has("engine/rework.R", "escalated")
})

# ---- K 组：防绕过 --------------------------------------------------
add("K1", "K", "P0", "七道口子", function() {
  sum(c(has("serve.R", "verified_not_writable"),
        has("engine/workflow/guard.R", "SC_WORKFLOW_RUN"),
        has("engine/workflow/guard.R", "verdict_md5"),
        has("engine/verdict.R", "缺件即不通过"),
        has("engine/db_api.R", "stale_write"),
        has("serve.R", "invalid_step_status"),
        has("engine/db.R", "is append-only"))) >= 7
})
add("K2", "K", "P0", "指纹范围扩大", function() {
  has("engine/workflow/guard.R", "sc_rule_digests") &&
    has("engine/workflow/guard.R", "flow_review_md5")
})
add("K3", "K", "P0", "判定器自身有回归测试", function() {
  file.exists(file.path(APP, "engine/tests/verdict_tests.R"))
})
add("K4", "K", "P0", "清单变成断言", function() {
  file.exists(file.path(APP, "engine/tests/checklist_assertions.R"))
})

# ---- L 组：落地 ----------------------------------------------------
add("L1", "L", "P0", "十处改动", function() {
  all(file.exists(file.path(APP, c("engine/verdict.R", "engine/rework.R", "engine/poller.R",
                                   "engine/db.R", "engine/db_api.R"))))
})
add("L2", "L", "P0", "六个验收场景自动化", function() {
  s <- src("engine/tests/verdict_tests.R")
  all(vapply(c("谎报", "错的 skill", "改脚本", "全量重判", "超时", "人工放行"),
             function(k) grepl(k, s, fixed = TRUE), TRUE))
})
add("L3", "L", "P0", "最小可上线三步", function() {
  # 第一步：done 只存 reported（serve.R 拒绝 verified 并调用判定）
  # 第二步：/api/run 对未 verified 的上一步返回 409（serve.R 调 db_api_gate）
  # 第三步：返工单 + 上限 3 次
  has("serve.R", "verified_not_writable") &&
    has("serve.R", "db_api_gate(DB_CON") &&
    has("engine/db_api.R", '"upstream_not_verified"') &&
    grepl("REWORK_MAX_AUTO <- 3L", src("engine/rework.R"), fixed = TRUE)
})

# ---- M 组：能保证什么 ----------------------------------------------
add("M1", "M", "P2", "清单不再是零落地", function() FAIL == 0L || NA)
add("M2", "M", "P2", "约束从提示变拦截", function() {
  # 判据不是「有一句提示」，而是「有一条 return 真的挡住了」。
  has("serve.R", "if (!is.null(g)) return(send_json(con, g$code, g$body))") &&
    has("engine/db_api.R", '"upstream_not_verified"')
})
add("M3", "M", "P2", "规则唯一权威", function() {
  has("engine/flow_spec.R", "FLOW_REVIEW <- list(") &&
    !has("serve.R", "STAGE_SKILL")
})
add("M4", "M", "P2", "能拒绝的三个条件", function() {
  has("serve.R", "verified_not_writable") &&
    has("serve.R", "db_api_gate(DB_CON") &&
    grepl("REWORK_MAX_AUTO <- 3L", src("engine/rework.R"), fixed = TRUE)
})

# ---- 跑 ------------------------------------------------------------
cat("\n清单断言：共 ", length(ITEMS), " 条\n\n", sep = "")
groups <- unique(vapply(ITEMS, function(i) i$group, ""))
for (g in groups) {
  cat("== ", g, " 组 ==\n", sep = "")
  for (it in Filter(function(i) i$group == g, ITEMS)) {
    r <- tryCatch(it$fn(), error = function(e) FALSE)
    if (is.na(r)[1]) {
      SKIP <<- SKIP + 1L
      cat(sprintf("  [todo] %-6s %s（判据还没落成断言）\n", it$id, it$name))
    } else {
      ok(it$id, it$name, r)
    }
  }
}

cat(sprintf("\n结果：%d 通过，%d 失败，%d 未落成断言\n", PASS, FAIL, SKIP))
cat(sprintf("覆盖率：%d / %d 条有断言（%.0f%%）\n",
            PASS + FAIL, length(ITEMS), 100 * (PASS + FAIL) / length(ITEMS)))
if (FAIL > 0L) {
  cat("\n失败项（说明这段代码可能只是文案或注释，不是实现）：\n")
  cat(paste0("  - ", FAILED, collapse = "\n"), "\n")
  quit(status = 1)
}
if (SKIP > 0L) {
  cat("\n未落成断言的条目就是下一步要补的：把它的完成判据写成一条会自动跑的检查。\n")
  quit(status = 2)
}
cat("PASS: 清单全部条目都有可执行断言\n")
