#!/usr/bin/env python3
"""执行端并行槽契约。

为什么必须有（2026-11-23，gina 问「一台电脑上有多个任务需要运行」）：
  主循环原来是同步的，领一条就盯到它结束，第二个任务只能干等。
  改造后一台执行端可以开 N 个槽并行盯梢，且同一条任务不会被两个槽同时领到
  （领取在服务端是锁内完成的原子动作，见 exec_lock_contract.R）。

判据：
  1) --slots 默认 1，不改变既有部署行为
  2) slot_worker 跑一轮会退出（--once 语义保留）
  3) stop_evt 已置位时不再取队列
  4) loop 真的按 slots 起线程，且心跳留在主线程
"""
import importlib.util
import pathlib
import re
import sys
import threading
import types

HERE = pathlib.Path(__file__).resolve().parent
WORKER = HERE.parent / "tools" / "pa_exec_worker.py"
if not WORKER.exists():
    print("FAIL: worker not found:", WORKER)
    sys.exit(1)

SRC = WORKER.read_text(encoding="utf-8")

spec = importlib.util.spec_from_file_location("pa_exec_worker", WORKER)
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)

fail = []


def check(label, ok):
    print(("PASS:" if ok else "FAIL:"), label)
    if not ok:
        fail.append(label)


class FakeClient:
    def __init__(self):
        self.due_calls = 0
        self.lock = threading.Lock()

    def due(self):
        with self.lock:
            self.due_calls += 1
        return {"ok": True, "items": [], "running": 0}

    def heartbeat(self, note=""):
        return {"ok": True}


# 1) --slots 默认 1
m = re.search(r'"--slots"[^)]{0,200}?default\s*=\s*(\d+)', SRC, re.S)
check("--slots defaults to 1", bool(m) and m.group(1) == "1")

if not hasattr(mod, "slot_worker"):
    check("slot_worker exists", False)
else:
    check("slot_worker exists", True)

    # 2) 跑一轮就退出
    args = types.SimpleNamespace(pool_id="pool-a", dry_run=True, once=True,
                                 interval=0.01, worker_id="w")
    mod.CLIENT = FakeClient()
    mod.slot_worker(args, 1, threading.Event())
    check("slot_worker runs exactly one round when --once",
          mod.CLIENT.due_calls == 1)

    # 3) stop_evt 已置位立即退出
    evt = threading.Event()
    evt.set()
    mod.CLIENT = FakeClient()
    mod.slot_worker(args, 2, evt)
    check("slot_worker stops when the flag is set",
          mod.CLIENT.due_calls == 0)

# 4) loop 按 slots 起线程，心跳留在主线程
#   注意：实现里用的是 getattr(args, "slots", 1) 而不是 args.slots，
#   所以这里按实际写法匹配，不要写成 "args.slots"（那样会误报失败）。
check("loop spawns threads per slot",
      "threading.Thread" in SRC
      and re.search(r'getattr\(args,\s*"slots"', SRC) is not None)
loop_src = SRC[SRC.index("def loop(args):"):]
check("heartbeat stays in the main loop, not per slot",
      'CLIENT.heartbeat("alive")' in loop_src
      and loop_src.count('CLIENT.heartbeat("alive")') == 1)

if fail:
    print("\nFAILED:", "; ".join(fail))
    sys.exit(1)
print("\nexec slots contract")