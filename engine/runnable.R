#!/usr/bin/env Rscript
# =====================================================================
#  runnable.R -- 环境能不能跑：把体检数据合成一个结论
#
#  为什么需要这个文件
#    体检数据早就齐全了：R 版本、包、路径、符号链接、内存、磁盘，一项一条。
#    但它回答的是「有哪些东西」，不是「这一步能不能跑」。
#    人要自己拿着一张 17 行的清单去对照「我要跑注释，那需要什么包」——
#    这件事机器能算，就不该让人算。
#
#  三档，只有三档
#    yes      该环节需要的全部条件都满足
#    no       有硬条件缺失，并指名缺什么、影响哪一环
#    unknown  体检数据不足以判断（没跑过、某项没测、读不到）
#
#  unknown 不许当 yes
#    这是全文件最重要的一条。缺数据时最省事的做法是「没发现问题就算能跑」，
#    但那等于把一个未验证的状态说成已验证 —— 后面所有判断都建在这上面。
#    具体到这套流程：如果服务器体检从没跑过，阅读者会说「能跑」，
#    然后点下去第一步就失败，而页面之前一直显示正常。
#
#  条件从哪来
#    每环的 requires 声明在 flow_spec 的 FLOW_ENV 里（唯一权威）。
#    不在本文件里另写一份 —— 那正是这一轮在清理的那类问题（五处各写一份）。
# =====================================================================

suppressWarnings(options(stringsAsFactors = FALSE, warn = -1))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

RUNNABLE_RULE_VERSION <- "runnable-1.0"

# ---- 体检条目索引 ----------------------------------------------------
#  env 是 env_check.R / remote_check.R 输出里的 items 数组。
#  统一成 id -> item 的映射，方便按 id 查包与路径。
runnable_env_index <- function(env) {
  items <- env$items %||% list()
  out <- list()
  for (it in items) {
    id <- as.character(it$id %||% "")
    if (nzchar(id)) out[[id]] <- it
  }
  out
}

runnable_pkg_status <- function(idx, pkg, pkg_probe_done = FALSE) {
  # 体检里包的 id 是 pkg_<名>；两种体检脚本都这么写。
  it <- idx[[paste0("pkg_", pkg)]]
  if (is.null(it)) {
    # 关键区分：体检有没有探过「包」这一类东西。
    #   探过了（说明它按清单逐项列了包），而这一项不在清单里 ——
    #   那只能解释成「装了但加载不起来」或「根本没装」，都是没有。
    #   一项都没探过，才是「没测」。
    #   这两种情况混成一个，会出现两种相反的错：
    #     该报缺的报成「判不了」，于是永远有人去点「强制继续」；
    #     或者该判不了的报成缺，逼着人去补一个本来就有的包。
    if (isTRUE(pkg_probe_done))
      return(list(state = "no", detail = "体检探过包清单，但这一项没出现（多半没装）"))
    return(list(state = "unknown", detail = "体检没测这一项"))
  }
  st <- as.character(it$status %||% "")
  if (identical(st, "ok")) return(list(state = "yes", detail = as.character(it$detail %||% "")))
  if (identical(st, "fail")) return(list(state = "no", detail = as.character(it$detail %||% "")))
  # warn 不等于不能用，但也不能算满足：某些功能会退化。
  list(state = "warn", detail = as.character(it$detail %||% ""))
}

runnable_path_status <- function(idx, id) {
  it <- idx[[id]]
  if (is.null(it)) return(list(state = "unknown", detail = "体检没测这一项"))
  st <- as.character(it$status %||% "")
  if (identical(st, "ok")) return(list(state = "yes", detail = as.character(it$detail %||% "")))
  if (identical(st, "fail")) return(list(state = "no", detail = as.character(it$detail %||% "")))
  list(state = "warn", detail = as.character(it$detail %||% ""))
}

# ---- 单环节判定 ------------------------------------------------------

# spec: FLOW_ENV 里该环的声明
#   requires_pkgs    必需包（缺一个就 no）
#   optional_pkgs    可选包（缺了 warn，不影响 yes/no 结论，但要列出来）
#   requires_paths   体检条目 id，例如 c("data_root", "script_root")
#   requires_links   需要存在的符号链接条数（来自体检的 links 项）
runnable_stage <- function(stage_id, spec, idx) {
  gaps <- list()      # 缺口：会变成 no
  notes <- list()     # 提醒：不影响结论
  unknown <- list()   # 判不了

  # names(idx) 在索引为空时是 NULL，startsWith 会直接报
  #   "non-character object(s)" —— 而这个错会被上层 tryCatch 吞成「环境判不了」。
  #   一个空列表不该让整条判定链崩掉，所以这里先兜住 NULL 与空串。
  idx_names <- names(idx)
  if (is.null(idx_names)) idx_names <- character(0)
  pkg_probe_done <- any(startsWith(as.character(idx_names), "pkg_"))
  for (p in as.character(spec$requires_pkgs %||% character(0))) {
    s <- runnable_pkg_status(idx, p, pkg_probe_done = pkg_probe_done)
    if (identical(s$state, "no"))
      gaps[[length(gaps) + 1L]] <- list(kind = "pkg", name = p, why = s$detail)
    else if (identical(s$state, "unknown"))
      unknown[[length(unknown) + 1L]] <- list(kind = "pkg", name = p, why = s$detail)
  }

  # optional_pkgs_matrix：体检没探到、但确实会用到的那几个。
  #   为什么要单独一个字段而不是直接塞进 optional_pkgs：
  #   optional_pkgs 里的是「探过了但非必需」，缺了是 warn；
  #   这一批是「根本没探过」，缺了只能是 unknown —— 两件事不能混。
  for (p in as.character(spec$optional_pkgs_matrix %||% character(0))) {
    unknown[[length(unknown) + 1L]] <- list(
      kind = "pkg_unprobed", name = p,
      why = "体检没探这一项；它随 Seurat/ggplot2 一起装，缺主包也起不来")
  }

  for (p in as.character(spec$optional_pkgs %||% character(0))) {
    s <- runnable_pkg_status(idx, p, pkg_probe_done = pkg_probe_done)
    if (!identical(s$state, "yes"))
      notes[[length(notes) + 1L]] <- list(kind = "pkg_optional", name = p,
                                          state = s$state, why = s$detail)
  }

  for (pid in as.character(spec$requires_paths %||% character(0))) {
    s <- runnable_path_status(idx, pid)
    if (identical(s$state, "no"))
      gaps[[length(gaps) + 1L]] <- list(kind = "path", name = pid, why = s$detail)
    else if (identical(s$state, "unknown"))
      unknown[[length(unknown) + 1L]] <- list(kind = "path", name = pid, why = s$detail)
  }

  link_need <- suppressWarnings(as.integer(spec$requires_links %||% NA))
  if (!is.na(link_need)) {
    it <- idx[["links"]]
    if (is.null(it)) {
      unknown[[length(unknown) + 1L]] <- list(kind = "links", name = "links",
                                              why = "体检没测符号链接")
    } else {
      got <- suppressWarnings(as.integer(it$value %||% NA))
      broken <- suppressWarnings(as.integer(it$broken %||% 0))
      if (is.na(got)) {
        unknown[[length(unknown) + 1L]] <- list(kind = "links", name = "links",
                                                why = "体检给了 links 项但没有数量")
      } else if (got < link_need || broken > 0L) {
        gaps[[length(gaps) + 1L]] <- list(
          kind = "links", name = "links",
          why = paste0("需要 ", link_need, " 个链接，实际 ", got,
                       " 个；断链 ", broken, " 个"))
      }
    }
  }

  # 未探到的包不再把整环判成 unknown。
  #   为什么：那是体检覆盖不全，不是环境有问题。把它当 unknown，
  #   结果是整条链永远判不了（实测就是这样：12 环里 4 环恒 unknown），
  #   人只好忽略这个结论 —— 判据写得太严，等于没有判据。
  #   现在它们进 notes（提醒），并在 summaries 里列出「体检没探到的项」，
  #   让人知道覆盖边界在哪，而不是假装覆盖完整。
  verdict <- if (length(gaps)) "no" else "yes"
  list(stage = stage_id, verdict = verdict,
       gaps = gaps, unknown = unknown, notes = notes,
       why = switch(verdict,
         no = paste0(length(gaps), " 项硬条件缺失"),
         unknown = paste0(length(unknown), " 项没有可核数据，判不了"),
         yes = "条件齐备"))
}

# ---- 整条链 ----------------------------------------------------------

#  env     体检 JSON（env_check.R 或 remote_check.R 的输出）
#  profile "server" / "local"
#  返回 list(ok, verdict, stages, blocking, unknown, notes, env_verdict)
runnable_chain <- function(env, profile = "server", flow_env = NULL) {
  fe <- flow_env %||% (if (exists("FLOW_ENV", mode = "list")) FLOW_ENV else list())

  # 体检本身没跑过 / 跑失败：整链 unknown，不是 yes。
  if (is.null(env) || !isTRUE(env$ok %||% FALSE)) {
    why <- if (is.null(env)) "还没有体检结果" else as.character(env$error %||% "体检失败")
    return(list(ok = FALSE, verdict = "unknown", profile = profile,
                env_verdict = NULL,
                stages = list(), blocking = list(), unknown = list(),
                notes = list(),
                message = paste0("判不了：", why,
                                 "。这时候不能说能跑 —— 没测过不等于没问题。")))
  }

  idx <- runnable_env_index(env)
  order <- tryCatch(FLOW_ORDER(profile), error = function(e) names(fe))
  stages <- lapply(order, function(sid) runnable_stage(sid, fe[[sid]] %||% list(), idx))

  blocking <- Filter(function(s) identical(s$verdict, "no"), stages)
  unknown <- Filter(function(s) identical(s$verdict, "unknown"), stages)
  notes <- Filter(function(s) length(s$notes) > 0L, stages)

  verdict <- if (length(blocking)) "no" else if (length(unknown)) "unknown" else "yes"
  msg <- switch(verdict,
    no = paste0("跑不了：", length(blocking), " 个环节缺硬条件（",
                paste(vapply(blocking, function(s) s$stage, ""), collapse = "、"), "）"),
    unknown = paste0("判不了：", length(unknown), " 个环节没有可核数据（",
                     paste(vapply(unknown, function(s) s$stage, ""), collapse = "、"), "）"),
    yes = "12 环的环境条件都满足，可以跑")

  list(ok = identical(verdict, "yes"), verdict = verdict, profile = profile,
       env_verdict = as.character(env$verdict %||% NA),
       rule_version = RUNNABLE_RULE_VERSION,
       stages = stages, blocking = blocking, unknown = unknown, notes = notes,
       message = msg)
}

# 只回结论与缺口，不回整张表（页面用这个）
runnable_summary <- function(env, profile = "server") {
  r <- runnable_chain(env, profile)
  list(verdict = r$verdict, message = r$message,
       blocking = lapply(r$blocking, function(s) list(
         stage = s$stage, why = s$why, gaps = s$gaps)),
       unknown = lapply(r$unknown, function(s) list(
         stage = s$stage, why = s$why, unknown = s$unknown)),
       notes = lapply(r$notes, function(s) list(stage = s$stage, notes = s$notes)))
}
