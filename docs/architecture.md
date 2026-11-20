# 个性化分析助手：架构与回写契约

本文描述的是仓库当前的真实结构，以及 2026-11 这次修复后必须遵守的边界。目标不是把
服务实现一次性重写，而是把已经出问题的边界拆成可独立测试、可独立验证的模块。

## 1. 当前代码地图

| 位置 | 职责 |
| --- | --- |
| `serve.R` | 极小的启动入口，只解析 APP_DIR 并 `sys.source()` 服务主体。 |
| `engine/server_main.R` | HTTP 路由、静态资源路由、执行队列与部分编排。仍是大文件，边界正在继续拆出。 |
| `engine/db.R` | SQLite schema、事务、owner/project/version/run/attempt 基础查询。 |
| `engine/db_api.R` | 把 HTTP 回写翻译为数据层动作；判定与返工的入口。 |
| `engine/writeback_identity.R` | 派工 attempt 登记、回写身份解析、幂等、作业号冲突。 |
| `engine/http_static.R` | 静态资源 gzip、ETag、304、缓存策略。 |
| `engine/startup_guard.R` | 端口冲突时探测已有健康实例，避免 systemd 重启风暴。 |
| `engine/run_enqueue.R` | `/api/run`、agent dispatch、链式派工的统一入队路径。 |
| `engine/poller.R` | 持久轮询/取回/返工作业。 |
| `engine/flow_spec.R` | 每个阶段的脚本、参数、命令与 qsub 标记。 |
| `engine/attempts.R` | 会话侧的不可变 attempt 绑定（按 session + req_id）。 |
| `public/app.js` | 前端主逻辑。首屏请求已做去重与并行化。 |
| `public/index.html` | 首屏资源清单。 |

`engine/server_main.R` 与 `public/app.js` 仍然是约 588 KB / 434 KB 的大文件。这是
历史事实，不应再用“只有一个 R 脚本”来描述；真正的问题是启动、路由、静态服务和编排
曾经集中在同一个文件里。`serve.R` 现在只负责启动，避免 `Rscript` 逐表达式解析大文件；
后续改动应继续把边界清楚、可测试的逻辑拆到 `engine/`，而不是为了拆文件而重写。

## 2. 控制面与执行面

- **控制面（本应用）**：记录、版本、run、attempt、闸门、判定、返工、产物索引。
- **执行面（PC / 远程服务器 / 集群）**：真正运行 R/Python 脚本、产生日志与产物。
- 控制面不能假设执行面仍在同一登录会话中。执行面断线、重连、换聊天都必须能靠持久
  编号恢复。

## 3. Run / Attempt 状态机

```text
dispatch
  -> step_attempts.status = delegated
  -> executor (batch=qsub | login=interactive observe)
  -> POST /api/step/import
  -> status = reported
  -> verdict_step
       -> pass/warn  => verified
       -> fail/unknown => rejected + rework ticket
```

- 一次派工只创建一个 `attempt_id`。`run_id`、`version_id`、`req_id`、`session_id`
  在派工时就固定。
- `job_number`（qsub 返回的数字号）只是执行端句柄，不是 attempt 身份。
- 同一个 `attempt_id` 的重复回写必须是幂等的；不能新建 attempt，不能重复判定。
- 已关闭的 attempt（`verified/rejected/released/escalated`）不能靠回写偷偷重开。
  需要重跑时，控制面必须派发新的 `attempt_id`。
- 旧调用方没有 `attempt_id` 时，才允许按 `req_id + step` 或 `ticket_id` 兼容绑定；
  这条路径会留下 `legacy` 标记，并且只在记录当前版本内绑定。旧版本的 attempt
  不能接住新版本的回写；已关闭的兼容 attempt 重开时也落在当前版本。

## 4. 回写契约

执行端向 `POST /api/step/import` 提交时，必须原样带回：

```json
{
  "attempt_id": "att-...",
  "run_id": "run-...",
  "version_id": "p...-V1",
  "session_id": "...",
  "req_id": "req-...",
  "step": "doublet",
  "job_number": "60739",
  "executor_mode": "batch|login",
  "status": "done",
  "exit_code": 0,
  "script_path": "/abs/path/script.R",
  "script_digest": "md5",
  "set_id": "sc-pipeline",
  "artifacts": []
}
```

绑定优先级：

1. `attempt_id`（权威）
2. `req_id + step_id`（兼容旧调用）
3. `ticket_id`（返工回写）

明确错误码，不再一律叫 `stale_write`：

- `attempt_id_unknown`
- `attempt_owner_mismatch`
- `attempt_step_mismatch`
- `attempt_closed`
- `attempt_script_mismatch`
- `job_number_conflict`

`attempt_id` 是最强绑定。执行端如果因为 observe/login 旧模板带回了过期
`version_id` 或 `run_id`，应用会采用 attempt 自己的版本与 run，并记录一次
`overrides`，不会因此拒绝整份回写。只有 owner 或 step 明确对不上时才拒绝。

派工时若脚本在应用主机上可读，attempt 会直接记录该文件的 MD5；执行端回写时
再带同一摘要。这样“脚本编号/版本对不上”不再只靠客户端自报，已验证 attempt 的
重复回写可以按脚本指纹判定是否真的同一份代码。

## 5. 两种执行方式

`flow_spec.R` 中只有部分阶段标记 `qsub = TRUE`，其余走 login 直接命令。两种方式的
共同要求：

- 派工时拿到同一个 `attempt_id`。
- qsub 的 `job_number` 写入该 attempt，不允许后续被另一个号覆盖。
- login/observe 脚本不得自行生成“脚本编号/版本号”作为回写身份。
- 轮询任务 `poll_jobs` 复用同一个 `attempt_id`，不再另建一个 attempt。

## 6. 首屏与静态资源

- 文本资源（JS/CSS/HTML/JSON/SVG）在客户端支持时用 gzip。
- 响应带 `ETag`；`If-None-Match` 命中返回 `304`。
- 版本化资源（如 `app-shell.20261028a.js`）可长期缓存；`index.html` 使用 `no-cache`。
- `Vary: Accept-Encoding` 防止缓存串味。
- 首屏的 `/api/place` 与 `/api/whoami` 并行；`/api/envcheck`、`/api/decision`、
  `/api/library` 并行；`renderIdentify()` 复用刚加载的 session state，避免重复请求。

## 7. 启动与单实例

`serve.R --port <n>` 先由极小入口 `sys.source(engine/server_main.R)` 加载实现。旧版把
一万行实现直接交给 `Rscript` 解析，生产冷启动实测会烧掉约 34 秒 CPU；入口拆分后这条
解析路径不再存在。

还有一个独立的冷启动成本：`serve_request()` 目前仍是七千多行的单个闭包。R 默认在函数
第一次调用时做 JIT 编译。实测同一实例里第一次 `/api/ping` 要 18.1 秒，第二次只要
15 毫秒；`/api/ping` 本身不查数据库也不问网关，因此慢点就在巨型闭包的首次编译。入口
现在显式 `compiler::enableJIT(0L)`，实测首个 `/api/ping` 降到 15 毫秒。这个开关是
针对现状的止损，不是最终架构；等路由拆成若干小 handler 后应重新评估是否恢复 JIT。

服务启动失败时：

1. 先请求 `http://127.0.0.1:<n>/api/env`。
2. 若返回 200，说明已有健康实例：记录日志并以状态 0 退出，避免 systemd 反复重启。
3. 若端口被未知进程占用：以状态 1 退出，并明确提示换端口或排查占用者。

探测连接必须用二进制 socket（`open = "r+b"`）。`writeBin()` 不能写文本连接；早期版本
因此把每个健康实例都探测成失败，第二进程仍然以 1 退出，重启风暴没有被真正挡住。

## 8. 不在本次范围

- 不修改生产 systemd unit。
- 不一次重写 `engine/server_main.R` 的全部路由；本次只拆启动、静态资源、回写身份与迁移边界。
- 不改变记录隔离、owner 校验、判定器“只有它能写 verified”的规则。
- 不把 `observe_doublet.R` 等服务器端脚本复制进仓库；它们的身份契约由回写字段约束。
