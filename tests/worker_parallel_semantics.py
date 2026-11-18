#!/usr/bin/env python3
"""Fake-cluster semantic checks for the Stage 4 worker patches.

The test applies the delivered patch to a temporary copy of the locked 3624-line
worker source. It never imports the source in place, never starts SSH/SGE, and
never touches production state.
"""

from __future__ import annotations

import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import types


APP = Path(__file__).resolve().parent.parent
SPEC = Path("/srv/catsco-agent/work/pa-parallel-spec")
SOURCE = Path(
    "/srv/catsco-agent/data/attachments/catscompany/cc_group_grp_7155/"
    "20261115_012626_716_8423c99e_worker_final3_20261115.py"
)
PARALLEL_PATCH = SPEC / "worker-parallel.patch"
AUTH_PATCH = SPEC / "worker-auth.patch"

failures: list[str] = []


def check(name: str, ok: bool, detail: str = "") -> None:
    print(("PASS:" if ok else "FAIL:") + name + ((" " + detail) if detail else ""))
    if not ok:
        failures.append(name)


def patched_copy(with_auth: bool = False) -> tuple[tempfile.TemporaryDirectory, Path]:
    td = tempfile.TemporaryDirectory(prefix="worker-semantics-")
    dst = Path(td.name) / "worker_final3_20261115.py"
    shutil.copyfile(SOURCE, dst)
    patches = [PARALLEL_PATCH] + ([AUTH_PATCH] if with_auth else [])
    for patch in patches:
        subprocess.run(
            ["patch", "-p1", "--batch", dst.name],
            cwd=td.name,
            stdin=patch.open("rb"),
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
        )
    return td, dst


def load_worker(path: Path, name: str):
    paramiko = types.ModuleType("paramiko")
    paramiko.SSHClient = type("SSHClient", (), {})
    sys.modules["paramiko"] = paramiko
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(mod)
    return mod


class Conn:
    def close(self):
        return None


def base_item(**extra):
    item = {
        "id": "ex-test",
        "step": "merge",
        "session_id": "s1",
        "req_id": "r1",
        "run_dir": "/tmp/run-s1",
        "created_at": "2026-11-16T00:00:00",
        "script": "/share/merge_cluster.R",
    }
    item.update(extra)
    return item


def reset_spool(w):
    p = Path(w.STATE_DIR) / "submit_keepalives.json"
    try:
        p.unlink()
    except FileNotFoundError:
        pass


def run_main_tests(w):
    # 1. selftest stays on its independent terminal path.
    calls = []
    old_self, old_submit = w.handle_selftest, w.submit_only
    try:
        w.handle_selftest = lambda item: calls.append(("self", item)) or "selftest"
        w.submit_only = lambda item: calls.append(("submit", item)) or "submit"
        out = w.handle({"id": "self-1", "step": "selftest"})
    finally:
        w.handle_selftest, w.submit_only = old_self, old_submit
    check("selftest is routed independently", out == "selftest" and
          calls == [("self", {"id": "self-1", "step": "selftest"})])

    # 2. submit_only happy path returns immediately, without wait/collect/done.
    reset_spool(w)
    seen = []
    old = {k: getattr(w, k) for k in (
        "api", "ssh", "run", "write_params", "ensure_engine_ready",
        "baseline_script", "prepare_guard", "submit", "collect_step",
        "file_count", "wait_job", "qstat", "qacct_exit", "log")}
    try:
        def api(path, body=None, timeout=20):
            seen.append((path, body))
            if path == "/api/exec/claim":
                return {"ok": True}
            if path == "/api/exec/keepalive":
                return {"ok": True}
            if path == "/api/exec/done":
                raise AssertionError("done is not a submit-only action")
            raise AssertionError(path)
        w.api = api
        w.ssh = lambda: Conn()
        w.run = lambda c, cmd, timeout=120: ("OK\n" if "find " not in cmd else "0\n", "")
        w.write_params = lambda c, root, item: None
        w.ensure_engine_ready = lambda c, root, step, item: {"ok": True}
        w.baseline_script = lambda c, root, step, item: {
            "script_path": "/share/merge_cluster.R", "script_md5": "a" * 32,
            "script_mtime_epoch": 1}
        w.prepare_guard = lambda c, root, step: None
        w.submit = lambda c, step, root, script="", args="": ("merge-test", "12345")
        w.collect_step = lambda *a, **k: (_ for _ in ()).throw(AssertionError("collect in submit"))
        w.file_count = lambda *a, **k: (_ for _ in ()).throw(AssertionError("file_count in submit"))
        w.wait_job = lambda *a, **k: (_ for _ in ()).throw(AssertionError("wait_job in submit"))
        w.qstat = lambda *a, **k: (_ for _ in ()).throw(AssertionError("qstat in submit"))
        w.qacct_exit = lambda *a, **k: (_ for _ in ()).throw(AssertionError("qacct in submit"))
        w.log = lambda msg: None
        out = w.submit_only(base_item())
    finally:
        for k, v in old.items():
            setattr(w, k, v)
    keep = [b for p, b in seen if p == "/api/exec/keepalive"]
    check("submit_only returns running immediately", out.get("status") == "running" and
          out.get("job_number") == "12345")
    check("submit_only keepalive carries job and numeric job_number",
          len(keep) == 1 and keep[0].get("job") == "merge-test" and
          keep[0].get("job_number") == "12345")
    check("submit_only does not call done/wait/collect", all(p != "/api/exec/done" for p, _ in seen))

    # 3. A lost keepalive after qsub is spooled, not turned into failed/done.
    reset_spool(w)
    seen = []
    old = {k: getattr(w, k) for k in (
        "api", "ssh", "run", "write_params", "ensure_engine_ready",
        "baseline_script", "prepare_guard", "submit", "collect_step",
        "file_count", "qstat", "qacct_exit", "log")}
    try:
        def api(path, body=None, timeout=20):
            seen.append((path, body))
            if path == "/api/exec/claim":
                return {"ok": True}
            if path == "/api/exec/keepalive":
                raise RuntimeError("network down after qsub")
            if path == "/api/exec/done":
                raise AssertionError("done must not be written")
            raise AssertionError(path)
        w.api = api
        w.ssh = lambda: Conn()
        w.run = lambda c, cmd, timeout=120: ("OK\n" if "find " not in cmd else "0\n", "")
        w.write_params = lambda c, root, item: None
        w.ensure_engine_ready = lambda c, root, step, item: {"ok": True}
        w.baseline_script = lambda c, root, step, item: None
        w.prepare_guard = lambda c, root, step: None
        w.submit = lambda c, step, root, script="", args="": ("merge-test", "12345")
        w.collect_step = lambda *a, **k: (_ for _ in ()).throw(AssertionError("collect"))
        w.file_count = lambda *a, **k: 0
        w.qstat = lambda *a, **k: "r"
        w.qacct_exit = lambda *a, **k: ("absent", None, None)
        w.log = lambda msg: None
        out = w.submit_only(base_item())
    finally:
        for k, v in old.items():
            setattr(w, k, v)
    spool = Path(w.STATE_DIR) / "submit_keepalives.json"
    saved = json.loads(spool.read_text()) if spool.exists() else {}
    check("lost keepalive leaves task running and does not call done",
          out.get("status") == "running" and all(p != "/api/exec/done" for p, _ in seen))
    check("lost keepalive persists numeric receipt locally",
          saved.get("ex-test", {}).get("job_number") == "12345")

    # 4. The next watch pass resends the spooled keepalive before qstat.
    seen = []
    old = {k: getattr(w, k) for k in (
        "api", "qstat", "qacct_exit", "task_root", "file_count", "run", "log")}
    try:
        def api(path, body=None, timeout=20):
            seen.append((path, body))
            if path == "/api/exec/keepalive":
                return {"ok": True}
            raise AssertionError(path)
        w.api = api
        w.qstat = lambda c, jn: "r" if jn == "12345" else (_ for _ in ()).throw(AssertionError(jn))
        w.qacct_exit = lambda *a, **k: (_ for _ in ()).throw(AssertionError("qacct"))
        w.task_root = lambda item: ("/tmp/run-s1", "")
        w.file_count = lambda *a, **k: 0
        w.run = lambda *a, **k: ("", "")
        w.log = lambda msg: None
        out = w.watch_running(object(), {
            "id": "ex-test", "step": "merge", "session_id": "s1", "req_id": "r1",
            "job_id": "merge-test", "run_dir": "/tmp/run-s1"})
    finally:
        for k, v in old.items():
            setattr(w, k, v)
    check("watch retries the spooled keepalive and keeps the live task",
          out is None and any(p == "/api/exec/keepalive" and b.get("job_number") == "12345"
                              for p, b in seen) and not spool.exists())

    # 5. Reuse while the numeric job is running: requeue, never collect/done.
    reset_spool(w)
    seen = []
    old = {k: getattr(w, k) for k in (
        "api", "ssh", "run", "write_params", "ensure_engine_ready",
        "baseline_script", "prepare_guard", "submit", "collect_step",
        "file_count", "wait_job", "qstat", "qacct_exit", "log")}
    try:
        def api(path, body=None, timeout=20):
            seen.append((path, body))
            if path in ("/api/exec/claim", "/api/exec/keepalive", "/api/exec/requeue"):
                return {"ok": True}
            if path == "/api/exec/done":
                raise AssertionError("done in reuse-running")
            raise AssertionError(path)
        w.api = api
        w.ssh = lambda: Conn()
        w.run = lambda c, cmd, timeout=120: ("OK\n" if "find " not in cmd else "1\n", "")
        w.write_params = lambda *a, **k: (_ for _ in ()).throw(AssertionError("params"))
        w.ensure_engine_ready = lambda *a, **k: (_ for _ in ()).throw(AssertionError("engine"))
        w.baseline_script = lambda *a, **k: (_ for _ in ()).throw(AssertionError("baseline"))
        w.prepare_guard = lambda *a, **k: None
        w.submit = lambda *a, **k: (_ for _ in ()).throw(AssertionError("submit"))
        w.collect_step = lambda *a, **k: (_ for _ in ()).throw(AssertionError("collect"))
        w.file_count = lambda *a, **k: 0
        w.wait_job = lambda *a, **k: (_ for _ in ()).throw(AssertionError("wait_job"))
        w.qstat = lambda c, jn: "r" if jn == "222" else (_ for _ in ()).throw(AssertionError(jn))
        w.qacct_exit = lambda *a, **k: (_ for _ in ()).throw(AssertionError("qacct"))
        w.log = lambda msg: None
        out = w.submit_only(base_item(job_id="same-name", job_number="222"))
    finally:
        for k, v in old.items():
            setattr(w, k, v)
    check("reuse-running uses numeric job_number, keepalive, and requeue",
          out.get("status") == "running" and
          [p for p, _ in seen].count("/api/exec/requeue") == 1 and
          all(p != "/api/exec/done" for p, _ in seen))

    # 6. Reuse after the numeric job finished: collect/import/done.
    reset_spool(w)
    seen = []
    old = {k: getattr(w, k) for k in (
        "api", "api_soft", "ssh", "run", "collect_step", "qstat", "qacct_exit",
        "_wb_extra", "log")}
    try:
        def api(path, body=None, timeout=20):
            seen.append((path, body))
            if path == "/api/exec/claim":
                return {"ok": True}
            if path == "/api/exec/done":
                return {"ok": True}
            raise AssertionError(path)
        def api_soft(path, body=None, timeout=20):
            seen.append((path, body))
            if path == "/api/step/import":
                return True, {"accepted": True, "chain": {"status": "done"}}
            raise AssertionError(path)
        w.api, w.api_soft = api, api_soft
        w.ssh = lambda: Conn()
        w.run = lambda c, cmd, timeout=120: ("OK\n" if "find " not in cmd else "1\n", "")
        w.collect_step = lambda *a, **k: [{"rel": "3.merge/a.rds"}]
        w.qstat = lambda c, jn: ""
        w.qacct_exit = lambda c, jn: ("ok", 0, 0) if jn == "222" else (_ for _ in ()).throw(AssertionError(jn))
        w._wb_extra = lambda *a, **k: {"job": "same-name", "jobnum": "222"}
        w.log = lambda msg: None
        out = w.submit_only(base_item(job_id="same-name", job_number="222"))
    finally:
        for k, v in old.items():
            setattr(w, k, v)
    done = [b for p, b in seen if p == "/api/exec/done"]
    check("reuse-done collects, imports done, and closes exec by numeric identity",
          out.get("status") == "done" and done == [{"id": "ex-test", "ok": True}])

    # 7. Reuse with fresh artifacts but no numeric job fails closed.
    reset_spool(w)
    seen = []
    old = {k: getattr(w, k) for k in ("api", "api_soft", "ssh", "run", "collect_step", "log")}
    try:
        def api(path, body=None, timeout=20):
            seen.append((path, body))
            if path == "/api/exec/claim":
                return {"ok": True}
            if path == "/api/exec/done":
                return {"ok": True}
            raise AssertionError(path)
        def api_soft(path, body=None, timeout=20):
            seen.append((path, body))
            return True, {"accepted": False}
        w.api, w.api_soft = api, api_soft
        w.ssh = lambda: Conn()
        w.run = lambda c, cmd, timeout=120: ("OK\n" if "find " not in cmd else "1\n", "")
        w.collect_step = lambda *a, **k: (_ for _ in ()).throw(AssertionError("collect"))
        w.log = lambda msg: None
        out = w.submit_only(base_item(job_id="same-name", job_number=""))
    finally:
        for k, v in old.items():
            setattr(w, k, v)
    step_fail = [b for p, b in seen if p == "/api/step/import"]
    check("reuse without job_number fails closed with no success path",
          out.get("status") == "failed" and "reuse_job_number_missing" in out.get("error", "") and
          step_fail and step_fail[0].get("status") == "failed" and
          any(p == "/api/exec/done" and b.get("ok") is False for p, b in seen))


def run_failure_matrix(w):
    cases = []

    def case(name, setup):
        cases.append((name, setup))

    case("root", lambda c: c.update(run_dir=""))
    case("ssh", lambda c: c.update(_ssh_error=True))
    case("params", lambda c: c.update(params_rel="drop_params.json"))
    case("engine", lambda c: c.update(_engine_bad=True))
    case("baseline", lambda c: c.update(_baseline_bad=True))
    case("submit", lambda c: c.update(_submit_bad=True))

    for name, setup in cases:
        reset_spool(w)
        seen = []
        old = {k: getattr(w, k) for k in (
            "api", "api_soft", "ssh", "run", "write_params", "ensure_engine_ready",
            "baseline_script", "prepare_guard", "submit", "collect_step", "log")}
        try:
            def api(path, body=None, timeout=20):
                seen.append((path, body))
                if path == "/api/exec/claim":
                    return {"ok": True}
                if path == "/api/exec/done":
                    return {"ok": True}
                raise AssertionError(path)
            def api_soft(path, body=None, timeout=20):
                seen.append((path, body))
                return True, {"accepted": False}
            w.api, w.api_soft = api, api_soft
            def ssh():
                if item.get("_ssh_error"):
                    raise RuntimeError("ssh unavailable")
                return Conn()
            w.ssh = ssh
            w.run = lambda c, cmd, timeout=120: ("OK\n" if "find " not in cmd else "0\n", "")
            w.write_params = lambda *a, **k: None
            w.ensure_engine_ready = lambda *a, **k: (
                {"ok": False, "error": "engine fixture"} if item.get("_engine_bad")
                else {"ok": True})
            def baseline(*a, **k):
                if item.get("_baseline_bad"):
                    raise RuntimeError("baseline failed")
                return None
            w.baseline_script = baseline
            w.prepare_guard = lambda *a, **k: None
            def submit(*a, **k):
                if item.get("_submit_bad"):
                    raise RuntimeError("qsub failed")
                return "merge-test", "12345"
            w.submit = submit
            w.collect_step = lambda *a, **k: (_ for _ in ()).throw(AssertionError("collect"))
            w.log = lambda msg: None
            item = base_item()
            setup(item)
            out = w.submit_only(item)
        finally:
            for k, v in old.items():
                setattr(w, k, v)
        imports = [b for p, b in seen if p == "/api/step/import"]
        dones = [b for p, b in seen if p == "/api/exec/done"]
        check("pre-submit " + name + " failure writes failed/failed",
              out.get("status") == "failed" and len(imports) == 1 and
              imports[0].get("status") == "failed" and len(dones) == 1 and
              dones[0].get("ok") is False,
              str(out.get("error", ""))[:120])


def run_watch_main_tests(w):
    # 8. Custom script tasks are watched to a terminal result, not ignored.
    seen = []
    old = {k: getattr(w, k) for k in (
        "api", "api_soft", "qstat", "qacct_exit", "task_root", "file_count",
        "collect_step", "_wb_extra", "run", "log")}
    try:
        def api(path, body=None, timeout=20):
            seen.append((path, body))
            if path == "/api/exec/done":
                return {"ok": True}
            raise AssertionError(path)
        def api_soft(path, body=None, timeout=20):
            seen.append((path, body))
            return True, {"accepted": True, "chain": {"status": "done"}}
        w.api, w.api_soft = api, api_soft
        w.qstat = lambda c, jn: ""
        w.qacct_exit = lambda c, jn: ("ok", 0, 0)
        w.task_root = lambda item: ("/tmp/run-s1", "")
        w.file_count = lambda *a, **k: (_ for _ in ()).throw(AssertionError("file_count custom"))
        w.collect_step = lambda *a, **k: (_ for _ in ()).throw(AssertionError("collect custom"))
        w._wb_extra = lambda *a, **k: {}
        w.run = lambda *a, **k: ("", "")
        w.log = lambda msg: None
        w.watch_running(object(), {
            "id": "custom-1", "step": "custom", "session_id": "s1", "req_id": "r1",
            "job_id": "custom-name", "job_number": "777", "run_dir": "/tmp/run-s1"})
    finally:
        for k, v in old.items():
            setattr(w, k, v)
    check("custom script task reaches import/done on successful qacct",
          [p for p, _ in seen] == ["/api/step/import", "/api/exec/done"])

    # 9. A same-name job cannot substitute for the task's numeric job.
    seen = []
    old = {k: getattr(w, k) for k in ("api", "qstat", "qacct_exit", "task_root", "log")}
    try:
        w.api = lambda path, body=None, timeout=20: {"ok": True}
        def qstat(c, jn):
            seen.append(("qstat", jn))
            return "r" if jn == "888" else ""
        w.qstat = qstat
        w.qacct_exit = lambda c, jn: (seen.append(("qacct", jn)) or ("ok", 0, 0))
        w.task_root = lambda item: ("/tmp/run-s1", "")
        w.log = lambda msg: None
        w.watch_running(object(), {
            "id": "collision", "step": "merge", "session_id": "s1", "req_id": "r1",
            "job_id": "same-name", "job_number": "888", "run_dir": "/tmp/run-s1"})
    finally:
        for k, v in old.items():
            setattr(w, k, v)
    check("same-name collision still queries only numeric job_number",
          seen == [("qstat", "888")] and "same-name" not in [str(x) for x in seen])

    # 10. Main submits queued tasks, then still watches running in the same pass.
    order = []
    old = {k: getattr(w, k) for k in ("locked", "api", "mirror_tick", "handle", "watch_running", "ssh", "unlock", "log")}
    try:
        w.locked = lambda: False
        def api(path, body=None, timeout=20):
            if path == "/api/exec/heartbeat":
                return {"ok": True}
            if path == "/api/exec/due":
                return {"items": [
                    {"id": "q1", "step": "merge", "kind": "queued"},
                    {"id": "q2", "step": "merge", "kind": "queued"},
                    {"id": "r1", "step": "merge", "kind": "running"},
                ]}
            raise AssertionError(path)
        w.api = api
        w.mirror_tick = lambda: 0
        w.handle = lambda it: order.append(("submit", it.get("id")))
        w.watch_running = lambda c, it: order.append(("watch", it.get("id")))
        w.ssh = lambda: Conn()
        w.unlock = lambda: order.append(("unlock", ""))
        w.log = lambda msg: None
        rc = w.main()
    finally:
        for k, v in old.items():
            setattr(w, k, v)
    check("main submits queued then watches running in the same pass",
          rc == 0 and order == [
              ("submit", "q1"), ("submit", "q2"), ("watch", "r1"), ("unlock", "")])


def run_pack_sge_identity_tests(w):
    # 11. pack evidence must query accounting by numeric job_number only.
    calls = []
    old = {k: getattr(w, k) for k in ("run", "log")}
    try:
        w.run = lambda c, cmd, timeout=120: (calls.append(cmd) or ("", ""))
        w.log = lambda msg: None
        out = w._wb_pack_sge(
            object(), "/tmp/run-s1", "11.report", "same-name", [], "")
    finally:
        for k, v in old.items():
            setattr(w, k, v)
    check("pack_sge without numeric job_number never queries qacct",
          out == {} and calls == [], str(calls))

    calls = []
    old = {k: getattr(w, k) for k in ("run", "log")}
    try:
        def run(c, cmd, timeout=120):
            calls.append(cmd)
            if "qacct -j" in cmd:
                return ("same-name|12345|node02|0|0|"
                        "2026-11-16 00:00:00|2026-11-16 00:00:10\n", "")
            if "date -d" in cmd:
                return ("1700000000\n", "")
            if "stat -c" in cmd:
                return ("321 1700000005\n", "")
            if "md5sum" in cmd:
                return ("a" * 32 + "\n", "")
            return ("", "")
        w.run = run
        w.log = lambda msg: None
        out = w._wb_pack_sge(
            object(), "/tmp/run-s1", "11.report", "same-name", [], "12345")
    finally:
        for k, v in old.items():
            setattr(w, k, v)
    qacct = [cmd for cmd in calls if "qacct -j" in cmd]
    check("pack_sge queries qacct with numeric job_number only",
          len(qacct) == 1 and "qacct -j 12345" in qacct[0] and
          "same-name" not in qacct[0] and
          out.get("sge_job", {}).get("jobnumber") == "12345",
          str(qacct))


def static_checks(path: Path):
    text = path.read_text(encoding="utf-8-sig")
    check("no account-wide qstat -u identity lookup", "qstat -u " not in text)
    check("no jobname-based identity in qacct call", "jobname" not in
          "\n".join(line for line in text.splitlines() if "qacct -j" in line))
    check("qacct never falls back from job_number to job name",
          'str(jobnum or "").strip() or str(job or "")' not in text)
    check("watch_running uses job_number", "still = qstat(c, jobnum)" in text and
          "st, ec, fa = qacct_exit(c, jobnum)" in text)
    check("handle remains a selftest/parallel split", "if step == \"selftest\":" in text and
          "return submit_only(item)" in text)


def auth_checks():
    td, path = patched_copy(with_auth=True)
    try:
        w = load_worker(path, "worker_auth_semantics")
        seen = []
        old_req, old_url = w.urllib.request.Request, w.urllib.request.urlopen
        try:
            class Resp:
                def __enter__(self):
                    return self
                def __exit__(self, *a):
                    return False
                def read(self):
                    return b'{"ok":true}'
            def request(url, data=None, headers=None):
                seen.append(("request", data, headers or {}))
                return object()
            def urlopen(req, timeout=20):
                return Resp()
            w.urllib.request.Request = request
            w.urllib.request.urlopen = urlopen
            w.WORKER_TOKEN = "pa_worker_fixture"
            w.WORKER_ID = "w-a"
            w.WORKER_POOL = "pool-a"
            w.api("/api/exec/heartbeat", {"by": "pc-worker", "version": "v"})
            w.api("/api/place", None)
        finally:
            w.urllib.request.Request, w.urllib.request.urlopen = old_req, old_url
        body = json.loads(seen[0][1].decode()) if seen else {}
        headers = seen[0][2] if seen else {}
        check("worker auth sends token and identity only on worker paths",
              headers.get("X-Worker-Token") == "pa_worker_fixture" and
              body.get("worker_id") == "w-a" and body.get("pool_id") == "pool-a" and
              len(seen) == 2 and seen[1][2] == {})
    finally:
        td.cleanup()


def main() -> int:
    if not SOURCE.exists():
        raise SystemExit(f"missing locked worker source: {SOURCE}")
    for p in (PARALLEL_PATCH, AUTH_PATCH):
        if not p.exists():
            raise SystemExit(f"missing patch: {p}")
    td, path = patched_copy(with_auth=False)
    try:
        w = load_worker(path, "worker_parallel_semantics")
        run_main_tests(w)
        run_failure_matrix(w)
        run_watch_main_tests(w)
        run_pack_sge_identity_tests(w)
        static_checks(path)
    finally:
        td.cleanup()
    auth_checks()
    if failures:
        print(f"FAIL: worker semantics ({len(failures)} failures)")
        return 1
    print("PASS: worker parallel/auth semantics")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
