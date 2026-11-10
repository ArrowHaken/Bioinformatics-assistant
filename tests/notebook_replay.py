"""Run: PYTHONPATH=/tmp/pa-jupyter-packages python3 tests/notebook_replay.py

Real kernel replay; only generated notebooks enter the fresh execution directories.
No dependency installation, original source access, shell commands, or rpy2.
"""
import ast
import base64
import copy
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile

import nbformat
from nbclient import NotebookClient
from nbclient.exceptions import CellExecutionError
from jupyter_client import KernelManager

APP = Path(__file__).resolve().parents[1]
ROOT = Path(tempfile.mkdtemp(prefix="pa-explicit-replay-proof-"))
subprocess.run(["Rscript", "--vanilla", "tests/notebook_replay.R", str(ROOT)], cwd=APP, check=True)
assert not (ROOT / "source-project").exists()
# Make the renamed source genuinely unavailable even to accidental relative lookups.
source = ROOT / "source-project-unavailable"
source.rename(ROOT / "unavailable-do-not-use")
os.environ["PA_REPLAY_TEST_SENTINEL"] = "must-not-enter-analysis-subprocess"


def payload(nb):
    cell = next(c for c in nb.cells if c.cell_type == "code" and "_REPLAY = json.loads(" in c.source)
    tree = ast.parse(cell.source)
    assignment = next(n for n in tree.body if isinstance(n, ast.Assign) and any(isinstance(t, ast.Name) and t.id == "_REPLAY" for t in n.targets))
    return json.loads(ast.literal_eval(assignment.value.args[0]))


def replace_payload(nb, data):
    cell = next(c for c in nb.cells if c.cell_type == "code" and "_REPLAY = json.loads(" in c.source)
    lines = cell.source.splitlines(keepends=True)
    i = next(i for i, line in enumerate(lines) if line.startswith("_REPLAY = json.loads("))
    lines[i] = "_REPLAY = json.loads(" + repr(json.dumps(data)) + ")\n"
    cell.source = "".join(lines)


checks = []


def run(name, notebook="replay.ipynb", mutate=None, failure=None, requested=True):
    isolated = ROOT / (name + "-kernel")
    isolated.mkdir()
    nb = nbformat.read(ROOT / notebook, as_version=4)
    nbformat.validate(nb)
    replay = ROOT / (name + "-restored")
    if requested:
        nb.cells[1].source = "REPLAY_ROOT = " + repr(str(replay)) + "\n"
    if mutate:
        mutate(nb, replay)
    # Copy only the notebook into the independent kernel working directory.
    nbformat.write(nb, isolated / "only-input.ipynb")
    km = KernelManager(kernel_name="python3")
    km.kernel_spec.argv = ["/usr/bin/python3", "-m", "ipykernel_launcher", "-f", "{connection_file}"]
    client = NotebookClient(nb, timeout=60, kernel_name="python3", resources={"metadata": {"path": str(isolated)}}, km=km)
    try:
        client.execute()
    except CellExecutionError as exc:
        if not failure or failure not in str(exc):
            raise
        checks.append({"case": name, "passed": True, "expected_error": failure})
        nbformat.write(nb, isolated / "failed.executed.ipynb")
        return
    if failure:
        raise AssertionError("Expected failure: " + failure)
    nbformat.write(nb, isolated / "replay.executed.ipynb")
    text = "".join(str(o.get("text", "")) for c in nb.cells if c.cell_type == "code" for o in c.get("outputs", []))
    if not requested:
        root_line = next(line for line in text.splitlines() if line.startswith("Declared files restored to "))
        replay = Path(root_line.removeprefix("Declared files restored to "))
    data = payload(nb)
    for file in data["files"]:
        actual = (replay / file["path"]).read_bytes()
        assert actual == base64.b64decode(file["base64"])
        assert hashlib.sha256(actual).hexdigest() == file["sha256"]
        if file["readable_source"] is not None:
            assert actual == file["readable_source"].encode("utf-8")
    for output in data["expected_outputs"]:
        actual = (replay / output["path"]).read_bytes()
        assert actual == (ROOT / "baselines" / Path(output["path"]).name).read_bytes()
    assert "R_INTERMEDIATE_OK" in text and "PY_FINAL_OK" in text
    assert all(c.execution_count is not None for c in nb.cells if c.cell_type == "code")
    assert nb.metadata.pa.explicit_replay.full_analysis_replay_verified is False
    assert nb.metadata.pa.explicit_replay.fixture_replay_verified is False
    checks.append({"case": name, "passed": True, "replay_root": str(replay), "externally_compared_outputs": [o["path"] for o in data["expected_outputs"]]})


run("fresh-configured-root")
run("default-temporary-root", requested=False)
run("binary-input", notebook="binary.ipynb")
run("nested-cwd", notebook="nested-cwd.ipynb")
run("failed-step", notebook="failure.ipynb", failure="intentional fixture failure")
run("missing-output", notebook="missing-output.ipynb", failure="Expected output missing")
run("wrong-output", notebook="wrong-output.ipynb", failure="Output baseline mismatch")
run("symlink-output", notebook="symlink-output.ipynb", failure="Symlink replay path rejected")


def modify_payload(transform):
    def mutate(nb, replay):
        data = payload(nb)
        transform(data)
        replace_payload(nb, data)
    return mutate


run("corrupt-bytes", mutate=modify_payload(lambda p: p["files"][0].update(sha256="0" * 64)), failure="Embedded bytes/hash mismatch")
run("readable-source-tamper", mutate=modify_payload(lambda p: p["files"][0].update(readable_source="not original")), failure="Readable source differs")
run("payload-path-escape", mutate=modify_payload(lambda p: p["files"][0].update(path="../escape.R")), failure="Unsafe relative replay path")
run("undeclared-module", mutate=modify_payload(lambda p: p.update(files=[f for f in p["files"] if f["path"] != "scripts/helper.py"])), failure="ModuleNotFoundError")


def nonempty(nb, replay):
    replay.mkdir()
    (replay / "existing").write_text("do not overwrite")


run("nonempty-root", mutate=nonempty, failure="REPLAY_ROOT must be empty")


def symlink(nb, replay):
    actual = ROOT / "symlink-real"
    actual.mkdir()
    replay.symlink_to(actual, target_is_directory=True)


run("symlink-root", mutate=symlink, failure="Symlink replay root rejected")
report = {"passed": True, "fixture_replay_verified": True, "verification_scope": "External real R+Python fixture execution and byte comparison only; not biological validation", "full_analysis_replay_verified": False, "checks": checks, "root": str(ROOT)}
(ROOT / "verification.json").write_text(json.dumps(report, indent=2))
print("PASS independent real Jupyter replay:", ROOT, "cases=", len(checks))
