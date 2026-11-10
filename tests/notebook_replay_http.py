"""Real copied serve HTTP regression. Run from app with:
PYTHONPATH=/tmp/pa-jupyter-packages python3 tests/notebook_replay_http.py
Only /tmp fixtures and this test are written; no real tokens or production port.
Fixture and JSON evidence are retained for independent inspection.
"""
import ast
import base64
import hashlib
import json
import os
from pathlib import Path
import shutil
import socket
import subprocess
import tempfile
import time
import urllib.error
import urllib.request

APP = Path(__file__).resolve().parents[1]
ROOT = Path(tempfile.mkdtemp(prefix="pa-replay-http-", dir="/tmp"))
FIXTURE = ROOT / "app"
FIXTURE.mkdir()
for directory in ("engine", "public"):
    shutil.copytree(APP / directory, FIXTURE / directory)
shutil.copy2(APP / "serve.R", FIXTURE / "serve.R")
CHECKS = []


def check(name, ok, **evidence):
    CHECKS.append(dict(name=name, passed=bool(ok), **evidence))
    print(("PASS " if ok else "FAIL ") + name, flush=True)


def rscript(code):
    subprocess.run(["Rscript", "--vanilla", "-e", code], cwd=FIXTURE,
                   check=True, capture_output=True, text=True)


seed = r'''
source('engine/db.R'); source('engine/agent_gate.R'); source('engine/sessions.R')
c <- db_connect('.'); db_init(c)
for (id in c('ownerA','ownerB')) {
  rd <- normalizePath(paste0('../run-',id),mustWork=TRUE)
  DBI::dbExecute(c,"INSERT INTO sessions(session_id,owner_key,name,mode,run_dir,created_at,updated_at) VALUES (?,?,?,?,?,datetime('now'),datetime('now'))",params=list(id,paste0('viewer:',id),id,'local',rd))
}
sess_write('.',list(current='ownerA',sessions=lapply(c('ownerA','ownerB'),function(id) list(id=id,name=id,mode='local',run_dir=normalizePath(paste0('../run-',id))))))
a <- agent_token_issue('.','viewer:ownerA'); b <- agent_token_issue('.','viewer:ownerB')
writeLines(as.character(jsonlite::toJSON(list(ownerA=a$token,ownerB=b$token),auto_unbox=TRUE)),'fixture-credentials.json')
stopifnot(identical(sess_find('.','ownerA')$mode,'local'))
stopifnot(identical(DBI::dbGetQuery(c,"SELECT owner_key FROM sessions WHERE session_id='ownerA'")$owner_key,'viewer:ownerA'))
stopifnot(identical(DBI::dbGetQuery(c,"SELECT owner_key FROM sessions WHERE session_id='ownerB'")$owner_key,'viewer:ownerB'))
DBI::dbDisconnect(c)
'''
SOURCES = {
    "scripts/main.py": b"from pathlib import Path\nfrom helper import transform\nraw = Path('input.bin').read_bytes()\nPath('out.bin').write_bytes(transform(raw))\nprint('HTTP_REPLAY_OK')\n",
    "scripts/helper.py": b"def transform(raw):\r\n    return bytes((x ^ 42) for x in raw)\r\n",
    "input.bin": bytes([0, 1, 2, 127, 128, 255]),
}
BASELINE = bytes(x ^ 42 for x in SOURCES["input.bin"])
MANIFEST = {
    "files": [{"path": p, "role": "input" if p == "input.bin" else "script",
               **({"embed_authorized": True} if p == "input.bin" else {})} for p in SOURCES],
    "steps": [{"command": "python3", "argv": ["scripts/main.py"], "cwd": "."}],
    "expected_outputs": ["out.bin"],
}


def write_json(path, obj):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(obj))


def reset(sid="ownerA", attempt="attempt-1"):
    rd = ROOT / ("run-" + sid)
    write_json(rd / "status/notebook_session.json", {
        "session_id": sid, "app_dir": str(FIXTURE), "run_dir": str(rd), "binding_id": "binding-" + sid})
    write_json(rd / "status/notebook_manifest.json", {
        "binding_id": "binding-" + sid,
        "phases": [{"step": "01", "attempt_id": attempt, "state": "succeeded"}]})
    write_json(rd / ("status/notebook_attempts/" + attempt + "/status/attempt.json"), {
        "attempt_id": attempt, "binding_id": "binding-" + sid, "execution_succeeded": True})


for sid in ("ownerA", "ownerB"):
    rd = ROOT / ("run-" + sid)
    rd.mkdir()
    for relative, content in SOURCES.items():
        dest = rd / relative
        dest.parent.mkdir(parents=True, exist_ok=True)
        dest.write_bytes(content)
    (rd / "out.bin").write_bytes(BASELINE)
    reset(sid)
rscript(seed)
credentials = json.loads((FIXTURE / "fixture-credentials.json").read_text())
(FIXTURE / "fixture-credentials.json").chmod(0o600)
with socket.socket() as sock:
    sock.bind(("127.0.0.1", 0))
    PORT = sock.getsockname()[1]
assert PORT != 19980
BASE = "http://127.0.0.1:" + str(PORT)


def request(path, owner="ownerA", body=None):
    headers = {"X-Agent-Token": credentials.get(owner, owner)}
    if body is not None:
        headers["Content-Type"] = "application/json"
    req = urllib.request.Request(BASE + path, headers=headers,
                                 data=None if body is None else json.dumps(body).encode())
    try:
        response = urllib.request.urlopen(req, timeout=10)
    except urllib.error.HTTPError as exc:
        response = exc
    with response:
        return response.status, response.read(), dict(response.headers), response.reason


def build(owner="ownerA", sid="ownerA"):
    return request("/api/notebook/replay/build", owner, {"session_id": sid, "manifest": MANIFEST})


def download(owner="ownerA", sid="ownerA"):
    return request("/api/notebook/replay/download?session=" + sid, owner)


def status_check(name, result, expected, code=None):
    status, content, _, reason = result
    parsed = json.loads(content) if content.startswith(b"{") else {}
    check(name, status == expected and (code is None or parsed.get("code") == code),
          status=status, expected=expected, code=parsed.get("code"), reason=reason)


log = (ROOT / "serve.log").open("w")
process = subprocess.Popen(["Rscript", "serve.R", "--port", str(PORT), "--no-open"],
                           cwd=FIXTURE, stdout=log, stderr=log)
try:
    for _ in range(600):
        try:
            if request("/api/ping")[0] == 200:
                break
        except OSError:
            pass
        if process.poll() is not None:
            raise RuntimeError("Copied serve exited: " + (ROOT / "serve.log").read_text())
        time.sleep(.1)
    else:
        raise RuntimeError("Copied serve startup timeout")
    status_check("ownerA build", build(), 200)
    status_check("ownerB build explicit own noncurrent session", build("ownerB", "ownerB"), 200)
    for owner in ("ownerB", "pa_agent_invalid_fixture"):
        status_check(owner + " build denied", build(owner), 403)
        status_check(owner + " download denied", download(owner), 403)
    # DB ownership must precede session/run binding validation, including when metadata is bad.
    bp = ROOT / "run-ownerA/status/notebook_session.json"
    binding = json.loads(bp.read_text())
    bp.write_text('{"session_id":"foreign"}')
    status_check("wrong owner before invalid filesystem binding", build("ownerB"), 403, "notebook_not_owner")
    status_check("foreign binding build failclosed", build(), 409, "replay_binding_or_manifest_invalid")
    status_check("foreign binding download failclosed", download(), 409, "replay_binding_or_manifest_invalid")
    write_json(bp, binding)
    downloaded = download()
    status, content, headers, reason = downloaded
    check("ownerA download exact raw export bytes", status == 200 and content ==
          (FIXTURE / "state/notebooks/ownerA/declared-replay.ipynb").read_bytes(), bytes=len(content))
    check("download valid HTTP content type", headers.get("Content-Type") == "application/x-ipynb+json",
          headers=headers, reason=reason)
    (ROOT / "downloaded.ipynb").write_bytes(content)
    status_check("ownerB download explicit own noncurrent session", download("ownerB", "ownerB"), 200)
    status_check("wrong owner noncurrent download authorization first", download("ownerA", "ownerB"), 403, "notebook_not_owner")
    status_check("invalid token noncurrent download authorization first", download("pa_agent_invalid_fixture", "ownerB"), 403, "agent_token_invalid")
    nb = json.loads(content)
    bootstrap = next("".join(c["source"]) for c in nb["cells"]
                     if c["cell_type"] == "code" and "_REPLAY = json.loads(" in "".join(c["source"]))
    tree = ast.parse(bootstrap)
    assignment = next(n for n in tree.body if isinstance(n, ast.Assign)
                      and any(isinstance(t, ast.Name) and t.id == "_REPLAY" for t in n.targets))
    payload = json.loads(ast.literal_eval(assignment.value.args[0]))
    for f in payload["files"]:
        expected = SOURCES[f["path"]]
        check("exact embedded source " + f["path"], base64.b64decode(f["base64"]) == expected
              and f["sha256"] == hashlib.sha256(expected).hexdigest()
              and (f["readable_source"] is None or f["readable_source"].encode() == expected))
    for relative in ("input.bin", "out.bin", "scripts/helper.py"):
        path = ROOT / "run-ownerA" / relative
        original = path.read_bytes()
        stamp = path.stat()
        path.write_bytes(bytes([original[0] ^ 1]) + original[1:])
        os.utime(path, ns=(stamp.st_atime_ns, stamp.st_mtime_ns))
        status_check("same-size same-mtime changed " + relative, download(), 409,
                     "replay_binding_or_manifest_invalid")
        path.write_bytes(original)
        status_check("restored " + relative, download(), 200)
    reset(attempt="attempt-2")
    status_check("new successful attempt denies old download", download(), 409, "replay_binding_or_manifest_invalid")
    status_check("new attempt rebuild", build(), 200)
    status_check("new attempt download", download(), 200)
    reset()
    status_check("old attempt cannot reuse newer cached export", download(), 409, "replay_binding_or_manifest_invalid")
    status_check("original attempt rebuild", build(), 200)
    # The downloaded bytes alone enter the clean kernel. Original project is made unavailable.
    import nbformat
    from nbclient import NotebookClient
    from jupyter_client import KernelManager
    notebook = nbformat.reads(content.decode(), as_version=4)
    nbformat.validate(notebook)
    isolated = ROOT / "clean-kernel"
    isolated.mkdir()
    replay = ROOT / "restored"
    notebook.cells[1].source = "REPLAY_ROOT = " + repr(str(replay)) + "\n"
    nbformat.write(notebook, isolated / "only-input.ipynb")
    (ROOT / "run-ownerA").rename(ROOT / "unavailable-run-ownerA")
    km = KernelManager(kernel_name="python3")
    km.kernel_spec.argv = ["/usr/bin/python3", "-m", "ipykernel_launcher", "-f", "{connection_file}"]
    client = NotebookClient(notebook, timeout=60, kernel_name="python3",
                            resources={"metadata": {"path": str(isolated)}}, km=km)
    client.execute()
    nbformat.write(notebook, isolated / "executed.ipynb")
    check("downloaded notebook clean Jupyter output bytes", (replay / "out.bin").read_bytes() == BASELINE)
    for relative, expected in SOURCES.items():
        check("clean replay exact source " + relative, (replay / relative).read_bytes() == expected)
    check("full replay certification remains false", notebook.metadata.pa.explicit_replay.full_analysis_replay_verified is False)
except Exception as exc:
    check("test completed without unexpected exception", False, error=repr(exc))
    raise
finally:
    process.terminate()
    try:
        process.wait(timeout=5)
    except subprocess.TimeoutExpired:
        process.kill()
        process.wait()
    log.close()
    report = dict(root=str(ROOT), port=PORT, copied_real_server=True,
                  production_touched=False, checks=CHECKS,
                  passed=bool(CHECKS) and all(c["passed"] for c in CHECKS))
    (ROOT / "results.json").write_text(json.dumps(report, indent=2))
    print("Evidence: " + str(ROOT / "results.json"), flush=True)
assert all(c["passed"] for c in CHECKS), "HTTP contract failures; inspect " + str(ROOT / "results.json")
