"""Real Chromium replay controls against the copied HTTP/SQLite fixture.
Run: PYTHONPATH=/tmp/pa-jupyter-packages python3 tests/notebook_replay_browser.py
No API/fetch mocks, production service, user device, or biological data needed.
Only this test and /tmp evidence are written. Fixture agent headers test agent
scope, not real-user authorization. The HTTP fixture is reused up to server start.
"""
import ast
import base64
import hashlib
import json
import os
from pathlib import Path
import subprocess
import time

from playwright.sync_api import sync_playwright

HTTP_TEST = Path(__file__).with_name("notebook_replay_http.py")
# Execute the fixture's actual setup, not its HTTP checks or destructive replay.
module = ast.parse(HTTP_TEST.read_text())
setup = []
for node in module.body:
    if isinstance(node, ast.Assign) and any(isinstance(t, ast.Name) and t.id == "log" for t in node.targets):
        break
    setup.append(node)
else:
    raise RuntimeError("HTTP fixture server-start boundary changed")
fixture = {"__file__": str(HTTP_TEST), "__name__": "replay_browser_fixture"}
exec(compile(ast.Module(body=setup, type_ignores=[]), str(HTTP_TEST), "exec"), fixture)
ROOT, FIXTURE, BASE, PORT = [fixture[k] for k in ("ROOT", "FIXTURE", "BASE", "PORT")]
SOURCES, BASELINE, MANIFEST = [fixture[k] for k in ("SOURCES", "BASELINE", "MANIFEST")]
check, CHECKS, credentials = [fixture[k] for k in ("check", "CHECKS", "credentials")]
manifest_file = ROOT / "authorized-manifest.json"
manifest_file.write_text(json.dumps(MANIFEST))
requests, responses, errors, screenshots = [], [], [], []
downloads = {}
runtime = {}
log = (ROOT / "browser-serve.log").open("w")
process = subprocess.Popen(["Rscript", "serve.R", "--port", str(PORT), "--no-open"],
                           cwd=FIXTURE, stdout=log, stderr=log)


def verify_notebook(path, sid):
    content = path.read_bytes()
    check(sid + " browser download exact server export", content ==
          (FIXTURE / "state/notebooks" / sid / "declared-replay.ipynb").read_bytes())
    nb = json.loads(content)
    bootstrap = next("".join(c["source"]) for c in nb["cells"]
                     if c["cell_type"] == "code" and "_REPLAY = json.loads(" in "".join(c["source"]))
    tree = ast.parse(bootstrap)
    assignment = next(n for n in tree.body if isinstance(n, ast.Assign)
                      and any(isinstance(t, ast.Name) and t.id == "_REPLAY" for t in n.targets))
    payload = json.loads(ast.literal_eval(assignment.value.args[0]))
    check(sid + " exact embedded inventory", [f["path"] for f in payload["files"]] == list(SOURCES))
    for f in payload["files"]:
        original = SOURCES[f["path"]]
        expected_role = "input" if f["path"] == "input.bin" else "script"
        check(sid + " source fields " + f["path"],
              base64.b64decode(f["base64"], validate=True) == original
              and f["sha256"] == hashlib.sha256(original).hexdigest()
              and f["bytes"] == len(original) and f["role"] == expected_role
              and f["readable_source"] == (None if expected_role == "input" else original.decode()),
              sha256=f["sha256"], bytes=f["bytes"], role=f["role"])
    check(sid + " normalized declared steps exact", payload["steps"] == [
        dict(step, title="Step " + str(i)) for i, step in enumerate(MANIFEST["steps"], 1)])
    check(sid + " expected output baseline exact", payload["expected_outputs"] == [
        {"path": "out.bin", "bytes": len(BASELINE), "sha256": hashlib.sha256(BASELINE).hexdigest()}])
    check(sid + " not full scientific certification", nb["metadata"]["pa"]["explicit_replay"]["full_analysis_replay_verified"] is False)
    return nb


try:
    for _ in range(600):
        try:
            if fixture["request"]("/api/ping")[0] == 200:
                break
        except OSError:
            pass
        if process.poll() is not None:
            raise RuntimeError("Copied serve exited: " + (ROOT / "browser-serve.log").read_text())
        time.sleep(.1)
    else:
        raise RuntimeError("Copied serve startup timeout")
    with sync_playwright() as pw:
        executable = os.environ.get("PA_CHROMIUM_EXECUTABLE")
        options = {"headless": True, "args": ["--no-sandbox"]}
        if executable:
            options["executable_path"] = executable
        browser = pw.chromium.launch(**options)
        runtime.update(browser_version=browser.version, executable=executable or pw.chromium.executable_path)
        for sid in ("ownerA", "ownerB"):
            context = browser.new_context(extra_http_headers={"X-Agent-Token": credentials[sid]}, accept_downloads=True)
            page = context.new_page()
            page.on("pageerror", lambda error: errors.append(str(error)))
            def observe_request(req):
                if "/api/notebook/replay/" in req.url or "/api/sessions/switch" in req.url:
                    requests.append({"owner": sid, "method": req.method, "url": req.url, "body": req.post_data_json if req.post_data else None})
            def observe_response(response):
                if "/api/notebook/replay/" in response.url or "/api/sessions/switch" in response.url:
                    responses.append({"owner": sid, "url": response.url, "status": response.status})
            page.on("request", observe_request)
            page.on("response", observe_response)
            page.goto(BASE, wait_until="domcontentloaded")
            page.wait_for_load_state("networkidle", timeout=30000)
            check(sid + " initial networkidle reached", True)
            page.wait_for_function("typeof S !== 'undefined' && !sessionChanging && typeof switchSession === 'function'")
            # Use the real session transition and real /api/sessions/switch.
            with page.expect_response(lambda r: "/api/sessions/switch" in r.url) as switched:
                page.evaluate("sid => switchSession(sid)", sid)
            check(sid + " real switch endpoint accepted", switched.value.status == 200)
            page.wait_for_function("sid => S.sessionId === sid && !sessionChanging", arg=sid)
            check(sid + " selected actual own session", page.evaluate("S.sessionId") == sid)
            for selector in ("#replayManifest", "#btnBuildReplay", "#btnDownloadReplay"):
                check(sid + " control visible " + selector, page.locator(selector).is_visible())
            page.locator("#replayManifest").set_input_files(str(manifest_file))
            check(sid + " JSON file selected", page.locator("#replayManifest").evaluate("el => el.files[0].name") == manifest_file.name)
            with page.expect_response(lambda r: "/api/notebook/replay/build" in r.url, timeout=60000) as built:
                page.locator("#btnBuildReplay").click()
            check(sid + " browser build accepted", built.value.status == 200)
            page.wait_for_function("document.querySelector('#notebookStatus').textContent.includes('已打包授权清单')")
            with page.expect_download(timeout=60000) as pending:
                page.locator("#btnDownloadReplay").click()
            downloaded = pending.value
            check(sid + " browser download filename", downloaded.suggested_filename == "analysis-" + sid + "-declared-replay.ipynb")
            destination = ROOT / (sid + "-browser-download.ipynb")
            downloaded.save_as(str(destination))
            downloads[sid] = destination
            verify_notebook(destination, sid)
            page.wait_for_function("document.querySelector('#notebookStatus').textContent.includes('已交给浏览器下载')")
            page.screenshot(path=str(ROOT / (sid + "-browser.png")), full_page=True)
            screenshots.append(str(ROOT / (sid + "-browser.png")))
            context.close()
        browser.close()
    for sid in ("ownerA", "ownerB"):
        build_requests = [r for r in requests if r["owner"] == sid and r["url"].endswith("/api/notebook/replay/build")]
        download_requests = [r for r in requests if r["owner"] == sid and "/api/notebook/replay/download?" in r["url"]]
        check(sid + " explicit build session and complete manifest", len(build_requests) == 1 and
              build_requests[0]["method"] == "POST" and build_requests[0]["body"] == {"session_id": sid, "manifest": MANIFEST})
        check(sid + " explicit download own session endpoint", len(download_requests) == 1 and
              download_requests[0]["method"] == "GET" and download_requests[0]["url"] == BASE + "/api/notebook/replay/download?session=" + sid)
    check("no uncaught browser errors", not errors, errors=errors)
    import nbformat
    from nbclient import NotebookClient
    from jupyter_client import KernelManager
    notebook = nbformat.read(downloads["ownerA"], as_version=4)
    nbformat.validate(notebook)
    isolated = ROOT / "browser-clean-kernel"
    isolated.mkdir()
    replay = ROOT / "browser-restored"
    notebook.cells[1].source = "REPLAY_ROOT = " + repr(str(replay)) + "\n"
    nbformat.write(notebook, isolated / "only-input.ipynb")
    for sid in ("ownerA", "ownerB"):
        (ROOT / ("run-" + sid)).rename(ROOT / ("unavailable-run-" + sid))
    km = KernelManager(kernel_name="python3")
    km.kernel_spec.argv = ["/usr/bin/python3", "-m", "ipykernel_launcher", "-f", "{connection_file}"]
    NotebookClient(notebook, timeout=60, kernel_name="python3", resources={"metadata": {"path": str(isolated)}}, km=km).execute()
    nbformat.write(notebook, isolated / "executed.ipynb")
    check("browser download executes in clean real Jupyter", (replay / "out.bin").read_bytes() == BASELINE)
    for relative, original in SOURCES.items():
        check("clean kernel original bytes " + relative, (replay / relative).read_bytes() == original)
    check("executed full certification remains false", notebook.metadata.pa.explicit_replay.full_analysis_replay_verified is False)
except Exception as exc:
    check("completed without unexpected exception", False, error=repr(exc))
    raise
finally:
    process.terminate()
    try:
        process.wait(timeout=5)
    except subprocess.TimeoutExpired:
        process.kill()
        process.wait()
    log.close()
    report = dict(root=str(ROOT), port=PORT, checks=CHECKS, passed=bool(CHECKS) and all(c["passed"] for c in CHECKS),
                  runtime=runtime, requests=requests, responses=responses, page_errors=errors, screenshots=screenshots,
                  copied_real_server=True, real_sqlite_owner_fixture=True, api_or_fetch_mocked=False,
                  fixture_agent_header_only=True, real_user_authorization_tested=False,
                  biological_data_required=False, biological_analysis_validated=False,
                  production_touched=False, user_device_touched=False, published=False,
                  late_response_race_tested=False, fixture_source=str(HTTP_TEST))
    (ROOT / "browser-results.json").write_text(json.dumps(report, indent=2, ensure_ascii=False))
    print("Evidence: " + str(ROOT / "browser-results.json"), flush=True)
assert all(c["passed"] for c in CHECKS), "Inspect " + str(ROOT / "browser-results.json")
