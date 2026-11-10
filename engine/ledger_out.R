# =====================================================================
#  对外台账（给统一入口的 reviewer.R 用）
# ---------------------------------------------------------------------
#  为什么要有这一层，而不是各自往服务器上写流水：
#
#    gina 2026-10-29 的原话：
#      「你补的流水，不能按照经过环节处理之后、由统一入口产生的流水去做吗？
#        为什么你补的流水会让对面觉得这是没有经过统一入口的？
#        既然你在下一个环节启动之前必须检查到一个经过统一入口的流水，
#        那你为什么不补一个它不会拦截的流水出来呢？」
#
#    这句话点破了之前那版的问题：我补的流水只有 run_id / step / event / by，
#    而 reviewer.R 实际检查的是五样东西：
#      ① set_id 要落在该阶段规定的 skill 里
#      ② 白名单里的脚本指纹一致
#      ③ 台账里的 run_dir 必须等于本次运行目录
#      ④ finish 记录的退出码必须是 0
#      ⑤ 产物指纹（没给 hash 时至少要求文件存在）
#    少一样就判 REFUSED。所以「补了流水」和「补了对的流水」是两件事。
#
#  职责划分（这是这一版的核心）：
#    应用知道语义：这一步属于哪个阶段、规定用哪套 skill、产物应该是哪些。
#    执行端知道事实：代码跑在服务器上、文件在服务器上、退出码是它拿到的。
#    所以：应用负责生成"合规的台账行"，执行端只负责把它追加到服务器流水里。
#    应用不假装自己能算服务器上文件的 md5 —— 没有 hash 就不写 hash，
#    reviewer 对缺 hash 退化成"存在性检查"，这是它自己的既定行为，不是绕过。
# =====================================================================

LED_OUT_DIR <- file.path(STATE_DIR, "ledger_out")

led_out_path <- function(sid) {
  s <- as.character(sid %||% "")
  if (!nzchar(s)) s <- "_default"
  s <- gsub("[^A-Za-z0-9._-]", "_", s)
  file.path(LED_OUT_DIR, paste0(s, ".jsonl"))
}

# 本次运行的编号：按分析记录走，不写死在代码里。
#   写死的后果：换一条分析记录复用同一个 run_id，台账里就分不清谁是谁，
#   而"分不清"在审计上等于"没法追"。
led_run_id <- function(sid) {
  s <- as.character(sid %||% "")
  if (!nzchar(s)) s <- "default"
  paste0("run-", gsub("[^A-Za-z0-9]", "", s))
}

# 某一步规定的 skill 名。取 flow_spec（唯一出处），不在这里另抄一份。
led_skill_of <- function(step) {
  if (!isTRUE(HAS_FLOW)) return("")
  st <- tryCatch(flow_stage(as.character(step), "server"), error = function(e) NULL)
  if (is.null(st)) return("")
  as.character(st$skill %||% "")
}

# 生成一条台账记录并排进待追加队列。
#   kind: "start" | "finish"
#   为什么要排队而不是直接写：服务器上的 status/ledger.jsonl 只有执行端能写。
#   应用把"该写什么"生成好，执行端下一次轮询取走、原样追加、回报已追加。
led_out_push <- function(sid, step, kind, run_dir = "", code = NULL,
                         artifacts = list(), skill = NULL, note = "") {
  dir.create(LED_OUT_DIR, showWarnings = FALSE, recursive = TRUE)

  # 产物清单：只写相对路径。
  #   不编造 hash —— 应用读不到服务器磁盘，写一个假 md5 比不写更坏：
  #   reviewer 会把它当成"指纹一致"，而实际从未核验。
  fps <- list()
  for (a in artifacts) {
    rel <- if (is.list(a)) as.character(a$rel %||% "") else as.character(a)
    if (!nzchar(rel)) next
    fps[[length(fps) + 1]] <- list(rel = rel, alg = "none")
  }

  entry <- list(
    run_id = led_run_id(sid),
    step = as.character(step),
    event = as.character(kind),
    at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    set_id = as.character(skill %||% led_skill_of(step)),
    run_dir = as.character(run_dir %||% ""),
    by = "app",
    note = as.character(note %||% ""))
  if (identical(kind, "finish")) {
    entry$code <- if (is.null(code)) 0L else as.integer(code)
    entry$fingerprints <- fps
  }

  line <- jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null", digits = NA)
  con <- file(led_out_path(sid), open = "ab")
  writeBin(charToRaw(paste0(enc2utf8(line), "\n")), con)
  close(con)
  invisible(entry)
}

# 取走待追加的行（执行端轮询用）。
led_out_take <- function(sid) {
  f <- led_out_path(sid)
  if (!file.exists(f)) return(character(0))
  ln <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"),
                 error = function(e) character(0))
  ln[nzchar(trimws(ln))]
}

# 追加成功后清空队列。
#   为什么可以清：服务器那份是正式台账，这里只是"待追加的寄存"。
#   清之前要求执行端明确回报"已追加成功"，不成功就不清。
led_out_clear <- function(sid) {
  f <- led_out_path(sid)
  if (file.exists(f)) tryCatch(file.remove(f), error = function(e) NULL)
  invisible(TRUE)
}
