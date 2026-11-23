#!/usr/bin/env python3
"""执行端 pool 过滤契约。

为什么必须有（2026-11-23）：
  下发是全量的，执行端又固定取第一条。队头只要压着一条别的 pool 的任务，
  它每轮去领、每轮被拒，后面真正属于它的任务永远轮不到 ——
  一条任务就能把这个 pool 堵死。gina 问「多个账号在一台电脑想接任务集群」时，
  这是第一个会撞上的墙。

判据：
  1) 别的 pool 排在队头时，本 pool 的任务仍被选中
  2) 历史条目没有 pool_id 时按兼容放行，不被误杀
  3) 在跑的不算排队
  4) 全是别的 pool 时返回空，不会去领然后被拒
"""
import importlib.util
import pathlib
import sys

HERE = pathlib.Path(__file__).resolve().parent
WORKER = HERE.parent / "tools" / "pa_exec_worker.py"
if not WORKER.exists():
    print("FAIL: worker not found:", WORKER)
    sys.exit(1)

spec = importlib.util.spec_from_file_location("pa_exec_worker", WORKER)
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)

if not hasattr(mod, "select_queued"):
    print("FAIL: select_queued missing (pool filtering not implemented)")
    sys.exit(1)

fail = []


def check(label, ok):
    print(("PASS:" if ok else "FAIL:"), label)
    if not ok:
        fail.append(label)


# 1) 别的 pool 排在队头
items = [
    {"id": "t-other", "kind": "queued", "pool_id": "pool-b"},
    {"id": "t-mine", "kind": "queued", "pool_id": "pool-a"},
]
got = [x["id"] for x in mod.select_queued(items, "pool-a")]
check("skips another pool sitting at the head", got == ["t-mine"])

# 2) 历史条目没有 pool_id，按兼容放行
items2 = [
    {"id": "t-legacy", "kind": "queued"},
    {"id": "t-mine", "kind": "queued", "pool_id": "pool-a"},
]
got2 = [x["id"] for x in mod.select_queued(items2, "pool-a")]
check("legacy item without pool_id is allowed", got2 == ["t-legacy", "t-mine"])

# 3) 在跑的不算排队
items3 = [{"id": "t-run", "kind": "running", "pool_id": "pool-a"}]
check("running item is not queued", mod.select_queued(items3, "pool-a") == [])

# 4) 全是别的 pool 时返回空
items4 = [{"id": "t-b", "kind": "queued", "pool_id": "pool-b"}]
check("returns nothing when only other pools are present",
      mod.select_queued(items4, "pool-a") == [])

if fail:
    print("\nFAILED:", "; ".join(fail))
    sys.exit(1)
print("\nexec pool filter contract")