'use strict';
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const html = fs.readFileSync(path.join(__dirname, '../public/index.html'), 'utf8');
const source = fs.readFileSync(path.join(__dirname, '../engine/notebook_replay.R'), 'utf8');
const decode = s => s.replace(/&quot;/g, '"').replace(/&#39;/g, "'").replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&amp;/g, '&');
const help = html.match(/<details\b[^>]*id="notebookReplayHelp"[^>]*>([\s\S]*?)<\/details>/);
assert.ok(help, 'static native details help exists');
assert.match(help[1], /<summary>[^<]+<\/summary>/);
assert.doesNotMatch(help[1], /<(?:script|style|button|input|a)\b/i, 'help has no behavior, extra controls or download links');
const example = help[1].match(/<pre id="replayManifestExample">([\s\S]*?)<\/pre>/);
assert.ok(example, 'copyable escaped JSON example exists');
assert.match(example[1], /&quot;/);
const manifest = JSON.parse(decode(example[1]));
assert.deepEqual(Object.keys(manifest).sort(), ['expected_outputs', 'files', 'steps']);
assert.deepEqual(manifest.files, [
  {path: 'main.py', role: 'script'},
  {path: 'input.csv', role: 'input', embed_authorized: true},
  {path: 'settings.json', role: 'config'}
]);
const relative = p => typeof p === 'string' && p.length > 0 && !/[\\:\x00-\x1f]/.test(p) && !/^[\/~]/.test(p) && !p.split('/').some(x => ['', '.', '..'].includes(x));
for (const file of manifest.files) {
  assert.ok(relative(file.path));
  assert.ok(['script', 'config', 'input'].includes(file.role));
  if (file.role === 'input') assert.equal(file.embed_authorized, true);
}
assert.deepEqual(manifest.steps, [{title: '运行分析', command: 'python3', argv: ['main.py'], cwd: '.'}]);
for (const step of manifest.steps) {
  assert.deepEqual(Object.keys(step).sort(), ['argv', 'command', 'cwd', 'title']);
  assert.ok(['Rscript', 'python', 'python3'].includes(step.command));
  assert.ok(step.cwd === '.' || relative(step.cwd));
  assert.ok(Array.isArray(step.argv) && step.argv.length > 0);
  assert.ok(manifest.files.some(f => f.path === step.argv[0] && f.role === 'script'));
}
assert.deepEqual(manifest.expected_outputs, ['out.csv']);
assert.ok(manifest.expected_outputs.every(relative));
assert.match(source, /c\("files", "steps", "expected_outputs"\)/);
assert.match(source, /c\("path", "role", "embed_authorized"\)/);
assert.match(source, /c\("title", "command", "argv", "cwd"\)/);
assert.match(source, /max_file_bytes = 10 \* 1024\^2, max_total_bytes = 50 \* 1024\^2/);
for (const text of ['执行快照不等于独立重放', '直接、间接', 'embed_authorized: true', '同次成功执行', 'SHA-256', '不是沙箱', '不要运行不信任代码', 'run_dir', '路径穿越', 'symlink', '敏感凭据', 'UTF-8', '10,485,760 bytes', '52,428,800 bytes', 'Python 3 内核', 'R/Python', '示例文件名不代表实际目录，需按实际分析填写', '只用于本次手动打包', '不会保存为自动清单', '仅本地执行可用', 'run_dir/replay-manifest.json', '所有 phase 成功且绑定真实 session', '才尝试自动打包', '不自动发现文件']) {
  assert.ok(help[1].includes(text), 'scope/safety guidance: ' + text);
}
const controls = [
  ['btnAgentAccess', 'button', '允许助手写入'],
  ['btnRefreshRuns', 'button', '刷新并行分析'],
  ['btnBuildNotebook', 'button', '生成执行快照'],
  ['btnDownloadNotebook', 'button', '下载执行快照'],
  ['btnBuildReplay', 'button', '打包清单 notebook'],
  ['btnDownloadReplay', 'button', '下载清单 notebook']
];
for (const [id, tag, text] of controls) {
  assert.equal((html.match(new RegExp('id="' + id + '"', 'g')) || []).length, 1);
  assert.match(html, new RegExp('<' + tag + '\\b[^>]*id="' + id + '"[^>]*type="button"[^>]*>' + text + '</' + tag + '>'));
}
assert.match(html, /<label class="btn" for="replayManifest">选择重放清单<\/label>/);
assert.match(html, /<input id="replayManifest" type="file" accept="application\/json,\.json"/);
assert.match(html, /<span id="notebookStatus" class="small" role="status" aria-live="polite"><\/span>/);
const toolbar = html.match(/<div class="ac-toolbar"[^>]*>([\s\S]*?)<\/div>/)[1];
assert.deepEqual([...toolbar.matchAll(/\bid="([^"]+)"/g)].map(m => m[1]), ['btnAgentAccess', 'btnRefreshRuns', 'btnBuildNotebook', 'btnDownloadNotebook', 'replayManifest', 'btnBuildReplay', 'btnDownloadReplay', 'notebookStatus']);
assert.doesNotMatch(help[1], /https?:\/\/|full_analysis_replay_verified\s*[:=]\s*true/i);
console.log('PASS replay help: escaped manifest, source schema, scope, safety and unchanged toolbar controls');
