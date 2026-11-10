"""Local-only fixtures: no SSH, real R, scheduler submission, or production paths."""
import json
import os
from pathlib import Path
import socket
import subprocess
import sys
import tempfile
import unittest


SCRIPT = Path(__file__).resolve().parents[1] / "engine" / "workpc_check.py"
PACKAGES = ['Seurat', 'DoubletFinder', 'harmony', 'SoupX', 'presto',
            'clusterProfiler', 'org.Mm.eg.db', 'ggplot2', 'pheatmap', 'ggpubr']


class LocalCheckTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="workpc-local-")
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name) / "fixture ' ; dollar$"
        self.root.mkdir()
        self.data = self.root / "matrix"
        sample = self.data / "sample one"
        sample.mkdir(parents=True)
        for name in ('barcodes.tsv', 'features.tsv', 'matrix.mtx'):
            (sample / name).write_text('fixture\n')
        self.scripts = self.root / "scripts"
        self.scripts.mkdir()
        (self.scripts / "custom ' ; $.R").write_text('Read10X()\n')
        self.work = self.root / 'results'
        self.rscript = self.root / "fake ' Rscript"
        self.libs = "library ' ; $HOME"
        self.ldlib = "loader ' ; $HOME"
        self.rscript.write_text(
            '#!' + sys.executable + '\n'
            'import os, sys\n'
            'if "--version" in sys.argv:\n'
            ' print("Rscript fixture 4.4.0")\n'
            'else:\n'
            ' assert os.environ["R_LIBS_USER"] == ' + repr(self.libs) + '\n'
            ' assert os.environ["LD_LIBRARY_PATH"].startswith(' + repr(self.ldlib + ':') + ')\n'
            ' for p in ' + repr(PACKAGES) + ': print("PKG\\t"+p+"\\t1.0\\tOK")\n')
        self.rscript.chmod(0o700)
        self.env = dict(os.environ)
        self.env.pop('SC_REMOTE_PWD', None)
        # -S makes this a stdlib-only invocation even if paramiko is installed.
        self.base = [sys.executable, '-S', str(SCRIPT), '--host', socket.gethostname(),
                     '--data-root', str(self.data), '--script-dir', str(self.scripts),
                     '--work-root', str(self.work), '--rscript', str(self.rscript),
                     '--libs', self.libs, '--ldlib', self.ldlib]

    def run_check(self, *args):
        return subprocess.run(self.base + list(args), env=self.env, text=True,
                              capture_output=True, timeout=30)

    def test_local_stdlib_only_and_quoted_paths(self):
        output = self.root / 'check.json'
        p = self.run_check('--transport', 'local', '--out', str(output))
        self.assertEqual(p.returncode, 0, p.stderr + p.stdout)
        result = json.loads(p.stdout)
        self.assertEqual(result, json.loads(output.read_text()))
        self.assertTrue(result['ok'])
        self.assertEqual(result['pkgs_ok'], 10)
        self.assertEqual(result['checked_from'], socket.gethostname())
        self.assertIn('local', result['checked_via'])
        self.assertNotIn('SSH', result['checked_via'])
        items = {i['id']: i for i in result['items']}
        self.assertEqual(items['r_version']['value'], 'Rscript fixture 4.4.0')
        self.assertEqual(items['data_root']['status'], 'ok')
        self.assertEqual(items['script_root']['status'], 'ok')
        self.assertEqual(items['work_root']['status'], 'ok')
        self.assertEqual(result['script_lib']['files'][0]['role'], '按内容判定：Seurat 读入')
        self.assertEqual(list(self.work.iterdir()), [])

    def test_host_mismatch_blocks_before_any_probe(self):
        p = self.run_check('--transport', 'local', '--host', 'not-this-server.invalid')
        self.assertNotEqual(p.returncode, 0)
        result = json.loads(p.stdout)
        self.assertEqual(result['error_code'], 'local_host_mismatch')
        self.assertFalse(result['ok'])
        self.assertEqual(result['checked_from'], socket.gethostname())
        self.assertFalse(self.work.exists())

    def test_default_still_ssh_requires_credentials(self):
        p = self.run_check('--user', 'fixture')
        self.assertEqual(p.returncode, 4, p.stderr + p.stdout)
        result = json.loads(p.stdout)
        self.assertEqual(result['error_code'], 'credentials_missing')
        self.assertIn('SSH', result['checked_via'])
        self.assertFalse(self.work.exists())

    def test_notebook_contains_actual_results_and_rerunnable_check(self):
        notebook = self.root / 'check.ipynb'
        p = self.run_check('--transport', 'local', '--notebook-out', str(notebook))
        self.assertEqual(p.returncode, 0, p.stderr + p.stdout)
        result = json.loads(p.stdout)
        nb = json.loads(notebook.read_text())
        self.assertEqual(nb['nbformat'], 4)
        self.assertIsInstance(nb['nbformat_minor'], int)
        self.assertEqual(nb['metadata']['kernelspec']['language'], 'python')
        code = next(c for c in nb['cells'] if c['cell_type'] == 'code')
        self.assertEqual(code['outputs'][0]['data']['application/json'], result)
        self.assertIn('仅体检', ''.join(nb['cells'][0]['source']))
        # Execute exported Python code as ordinary Python, without Jupyter.
        rerun = subprocess.run([sys.executable, '-S', '-c', ''.join(code['source'])],
                               env=self.env, text=True, capture_output=True, timeout=30)
        self.assertEqual(rerun.returncode, 0, rerun.stderr)
        self.assertEqual(json.loads(rerun.stdout)['pkgs_ok'], 10)

    def test_notebook_rerun_preserves_relative_path_context(self):
        notebook = self.root / 'relative.ipynb'
        args = list(self.base)
        for flag in ('--data-root', '--script-dir', '--work-root', '--rscript'):
            pos = args.index(flag) + 1
            args[pos] = './' + os.path.relpath(args[pos], self.root)
        p = subprocess.run(args + ['--transport', 'local', '--notebook-out', str(notebook)],
                           cwd=self.root, env=self.env, text=True, capture_output=True, timeout=30)
        self.assertEqual(p.returncode, 0, p.stderr + p.stdout)
        self.assertEqual(json.loads(p.stdout)['pkgs_ok'], 10)
        nb = json.loads(notebook.read_text())
        code = next(c for c in nb['cells'] if c['cell_type'] == 'code')
        rerun = subprocess.run([sys.executable, '-S', '-c', ''.join(code['source'])],
                               cwd=self.tmp.name, env=self.env, text=True,
                               capture_output=True, timeout=30)
        self.assertEqual(rerun.returncode, 0, rerun.stderr)
        self.assertEqual(json.loads(rerun.stdout)['pkgs_ok'], 10)

    def test_missing_rscript_is_failure_not_shell_error_as_version(self):
        p = self.run_check('--transport', 'local', '--rscript', str(self.root / 'missing R'))
        self.assertEqual(p.returncode, 0, p.stderr + p.stdout)
        result = json.loads(p.stdout)
        self.assertEqual(result['verdict'], 'blocked')
        item = next(i for i in result['items'] if i['id'] == 'r_version')
        self.assertEqual(item['status'], 'fail')


if __name__ == '__main__':
    unittest.main()
