# 身份链、持久调度与历史索引修复报告

日期：2026-11-20  
基线：`defac35`（与 `origin/main` 一致）  
范围：SQLite schema v2、回写身份链、轮询租约、Windows 主循环、`sessions.json` 恢复、
契约测试与文档。本次未修改生产 systemd unit，也未提交 Git commit。

## 1. 结论

本次把“身份”从只校验 `project -> version` 扩展为完整链路：

```text
project -> version -> run -> step_attempt -> snapshot -> artifact
```

新写入在每一层携带并校验 `project_id`；artifact 还直接携带 `attempt_id`。数据库增加
作用域触发器、封存 snapshot 保护、只追加表和“同一请求最多一个活跃 attempt”约束。
轮询改为单语句原子领取，并修复了同一 worker 回查错行与 `SQLITE_BUSY` 的窗口。服务主
循环可在 Windows 上按秒唤醒，页面关闭后仍继续推进调度。`sessions.json` 主索引损坏时
可回退到 `.last-good`，但两份都坏时仍失败关闭。

## 2. 发现的问题

1. **身份链缺层**：`runs`、`step_attempts`、`snapshots` 没有 `project_id`；
   `artifacts` 只有 `snapshot_id` 和 `version_id`，不能直接证明它属于哪个 attempt。
2. **作用域校验过窄**：原先只挡 `attempt.version_id != run.version_id`，不能挡同一
   version 下跨 owner 的 run/attempt 拼接，也不能挡 snapshot/artifact 指向别层对象。
3. **历史对象可改**：`artifacts`、`reviews` 可被更新或删除；封存 snapshot 也没有
   不可变保护。只追加规则只覆盖 `events`、`ledger`。
4. **同一请求并发派两份**：缺少数据库层的活跃请求唯一约束，仅靠先查后写会在并发时
   留下两个 `delegated/reported` attempt。
5. **轮询领取非原子**：原实现是 `SELECT` 后 `UPDATE`，两个进程之间存在同时选中同一
   job 的窗口；同一 worker 名在同一秒回查也可能拿到另一条刚领取的 job。
6. **Windows 主循环阻塞**：`socketAccept(timeout=)` 在 Windows 不可用，原逻辑一次
   accept 会等到有连接；页面关闭后调度不推进。
7. **历史索引恢复缺口**：写入已保留 `.last-good`，读取却只在主文件损坏时报错，没有
   使用完好的备份。
8. **迁移路径未补作用域列**：`migrate_history()` 按旧列插入 run/attempt/snapshot/
   artifact，会让新迁移出的历史对象缺少 `project_id`/`attempt_id`。

## 3. 改动

### 3.1 数据库 schema v2

- `DB_SCHEMA_VERSION` 从 `"1"` 升到 `"2"`，迁移保持增量、幂等、不删历史行。
- 新增列：
  - `runs.project_id`
  - `step_attempts.project_id`
  - `snapshots.project_id`
  - `artifacts.attempt_id`
  - `artifacts.project_id`
- 新增索引：
  - `runs(project_id, version_id)`
  - `step_attempts(project_id, step_id, status)`
  - `artifacts(attempt_id, rel)`
  - 活跃请求唯一索引：`(version_id, step_id, req_id)`，状态限定为
    `delegated/reported`。
- 旧库若已有重复活跃请求，不删除、不改写历史；唯一索引创建失败时退回普通查询索引，
  保证启动可用，并在新写入上继续暴露冲突。
- v1 回填只在 schema 版本不是 v2 时执行。这样后续启动不会因为只追加/封存触发器而
  再次 `UPDATE` 历史行。

### 3.2 作用域与不可变约束

- `run` 必须匹配 `version -> project -> owner`。
- `attempt` 必须匹配 `run -> version -> project -> owner`。
- `snapshot` 有 attempt 时必须匹配 attempt/version/project/owner。
- `artifact` 必须匹配 snapshot/attempt/version/project/owner；`snapshot_id` 不得为空。
- 封存 snapshot 的 `UPDATE/DELETE` 被拒绝。
- `artifacts`、`reviews` 与 `events`、`ledger` 一样只允许插入。

### 3.3 写入路径

以下入口都改为显式写入 `project_id`：

- `db_ensure_run()`：从 version 解析 project 并写入 run。
- `db_register_dispatch()`：派工 attempt 同时写 project。
- `db_api_report_step()`、`run_enqueue()`：新建 attempt 时写 project，并把
  `attempt_no` 的计算移入事务，缩小并发编号窗口。
- `verdict_step()`：snapshot 写 project；artifact 写 attempt 与 project。
- `migrate_history()`：旧 run/attempt/snapshot/artifact 迁移时补齐可解析的
  `project_id`；旧 artifact 没有真实 attempt 时保留 `attempt_id = NULL`。

### 3.4 轮询、主循环与 Windows

- `poller_claim()` 改为单条带子查询的 `UPDATE`，用 `lease_until` 表达租约。
- 每次领取生成唯一 `claim_token`，写入 `leased_by`；重新查询只按该 token 定位，
  对外仍返回调用者传入的 worker 名。
- 对 `SQLITE_BUSY`/locked 做有限退避重试。
- 新增 `poller_drain(con, app_dir, worker, limit)`；`limit <= 0` 直接返回 0。
- `server_main.R` 新增 `server_accept_with_timeout()`：`socketSelect()` 等 1 秒，
  有连接才 `socketAccept()`；Windows 与 Unix 共用。
- 主循环每秒唤醒；无论是否有 HTTP 请求，`DB_READY` 时都执行一次最多 10 条的
  `poller_sweep()`，并记录 `poller_last_run`、`poller_last_count`、
  `last_sweep_at`。心跳最多每 30 秒写一次，有作业推进时立即写；页面关闭后调度
  继续推进。

### 3.5 `sessions.json`

- 主索引可读时仍为唯一权威来源。
- 主索引缺失或解析失败且 `.last-good` 可读时，使用备份并发出警告。
- 主索引与备份都不可读时失败关闭，不返回空列表、不清空、不覆盖。
- 保留原来的临时文件 + 校验 + `rename` 提交流程。

## 4. 兼容与回滚

- v1 -> v2 是加法迁移；旧行保留，新增列先补列再回填，随后才安装守卫。
- 新增触发器在旧库升级完成后创建；重复 `db_init()` 幂等。
- 新增列不改变旧查询；回滚到旧代码时多余列通常可忽略。
- 若回滚后旧代码仍尝试 `UPDATE/DELETE artifacts`、`reviews` 或封存 snapshot，会
  被新触发器拒绝；完整回滚需要同时移除这些触发器，不能只回滚代码。
- 活跃请求唯一索引在旧库存在重复时不会强删数据，而是退回普通索引；清理重复后可
  再次启动收紧。
- 历史对象若从未有过 attempt 指针，不会伪造；完整身份链约束只对新写入保证。

## 5. 验证

本机使用 `C:\Program Files\R\R-4.6.2\bin\Rscript.exe`。启动时的
`LC_* C.UTF-8 failed` 是环境区域设置警告，不影响测试结果。

| 测试 | 结果 |
| --- | --- |
| `tests/writeback_identity.R` | PASS=21 FAIL=0 |
| `tests/static_assets.R` | PASS=15 FAIL=0 |
| `tests/db_identity_contract.R` | PASS=24 FAIL=0；含真实 v1 库升级与二次初始化 |
| `tests/poller_claim_contract.R` | PASS=13 FAIL=0；8 个独立 Rscript 竞争 2 条 job，恰好 2 个赢家且不重复 |
| `tests/sessions_last_good.R` | PASS=6 FAIL=0 |
| `tests/pending_persistence.R` | 通过 |
| `tests/run_lifecycle.R` | 48 checks，通过 |
| `tests/db_identity_contract.R` 的 v1 迁移夹具 | run/attempt/snapshot/artifact 均回填成功 |
| `migrate_history()` 临时旧 JSON 夹具 | 真实迁移成功；作用域列齐全，重复迁移不重复插入 |
| Windows `socketSelect + socketAccept` 探针 | 空闲 1 秒超时正确，连接到达后可 accept |
| 12 个改动/新增 R 文件 `parse()` | 全部成功 |
| `git diff --check` | 无空白错误 |

以下测试未通过，但已定位为本次变更之外的问题：

- `tests/startup_entrypoint.R`：7 通过、2 失败。`engine/dataset_profile.R:349` 含
  R 4.6.2 在当前 locale 下拒绝的非 ASCII 变量名；路由因此未加载。
- `tests/runs_lifecycle.R`：`queued: unknown`。夹具用反斜杠写
  `binding$app_dir`，`runs_local_lifecycle()` 用斜杠规范化路径比较，属于 Windows
  测试夹具问题。
- `tests/session_isolation_contract.R`、`tests/record_context_contract.R`：在夹具
  调用 `e$serve_request` 时失败。测试仍只解析 `serve.R` 的函数定义，而实现已移到
  `engine/server_main.R`；属于旧测试入口未同步。

## 6. 仍未覆盖

- 未启动完整 Windows 服务做页面级端到端回归；socket 唤醒只做了独立探针验证。
- 未在真实生产库上执行迁移演练；迁移测试使用临时 v1 形状数据库与临时旧 JSON 夹具。
- 历史 artifact 如果没有真实 attempt 来源，仍保留 `attempt_id = NULL`；不会推测。
- 旧的重复活跃请求不会被自动合并或删除，只会在后续新写入时由唯一索引/错误暴露。
- 本次未提交、未建分支、未改生产 systemd 配置。
