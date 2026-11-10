'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const vm = require('node:vm');
const fs = require('node:fs');
const source = fs.readFileSync(require('node:path').join(__dirname, '../public/analysis-controls.js'), 'utf8');
const html = fs.readFileSync(require('node:path').join(__dirname, '../public/index.html'), 'utf8');
const flush = () => new Promise(resolve => setImmediate(resolve));
function deferred() { let resolve; const promise = new Promise(r => { resolve = r; }); return {promise, resolve}; }
// Dependency-free DOM event harness. No HTTP calls, storage or real state mutations.
function harness() {
  class Element {
    constructor(tag) { this.tagName = tag; this.children = []; this.events = {}; this.style = {}; this.attrs = {}; this.textContent = ''; this.value = ''; this.hidden = false; this.disabled = false; this.open = false; }
    appendChild(child) { this.children.push(child); return child; }
    replaceChildren(...children) { this.children = children; this.textContent = ''; }
    setAttribute(key, value) { this.attrs[key] = value; }
    addEventListener(event, fn) { (this.events[event] ||= []).push(fn); }
    async emit(event) { for (const fn of this.events[event] || []) await fn({preventDefault(){}}); }
    click() { if (this.tagName === 'a') downloads.push(this); return this.disabled ? Promise.resolve() : this.emit('click'); }
    remove() {} focus() {} showModal() { this.open = true; } close() { this.open = false; this.emit('close'); }
  }
  const elements = new Map();
  for (const match of html.matchAll(/id="([^"]+)"/g)) { assert.ok(!elements.has(match[1]), 'duplicate id'); elements.set(match[1], new Element(match[1])); }
  const calls = [], timers = new Map(), downloads = [], copied = [], revoked = [];
  let nextTimer = 0, backend = async url => ({ok:true, items:[], tokens:[]});
  const document = new Element('document'); document.hidden = false; document.body = new Element('body');
  document.getElementById = id => { assert.ok(elements.has(id), id); return elements.get(id); };
  document.createElement = tag => new Element(tag);
  const window = new Element('window'); window.confirm = () => true;
  const URLWithBlob = class extends URL {};
  URLWithBlob.createObjectURL = () => 'blob:fake'; URLWithBlob.revokeObjectURL = url => revoked.push(url);
  const context = vm.createContext({document, window, URL: URLWithBlob, location:{href:'https://local/app/', origin:'https://local'}, navigator:{clipboard:{writeText:async text => copied.push(text)}}, AbortController,
    setTimeout(fn, ms) { const id = ++nextTimer; timers.set(id, {fn, ms}); return id; }, clearTimeout(id) { timers.delete(id); },
    fetch: async (url, opts) => { calls.push({url, opts}); const result = await backend(url, opts); return {ok:result.httpOk !== false, status:result.status || 200, json:async()=>result, blob:async()=>({blob:true})}; }
  });
  vm.runInContext("const S={sessionId:null}; let sessionChanging=false, generation=0; const recordContext=()=>generation; const recordCurrent=g=>g===generation; const apiUrl=p=>'/app/'+p.replace(/^\\//,''); const selected=[]; async function switchSession(id){selected.push(id); generation++; S.sessionId=id;}", context);
  vm.runInContext(source, context);
  return {context, document, window, calls, timers, downloads, copied, revoked, el:id=>elements.get(id), setBackend:fn=>{backend=fn}, run:code=>vm.runInContext(code, context)};
}
async function open(h) { await h.el('btnAgentAccess').click(); await flush(); }
async function issue(h) { h.setBackend(async (url, opts) => opts.method === 'POST' ? {ok:true, token:'secret-value', id:'t1'} : {ok:true,tokens:[]}); await h.el('agentIssueToken').click(); }
test('classic script follows bundle and global controls remain outside hidden main', () => {
  assert.ok(html.indexOf('analysis-controls.js') > html.indexOf('app.20261109log2.js'));
  assert.ok(html.indexOf('id="btnAgentAccess"') < html.indexOf('id="main"'));
  assert.doesNotMatch(source, /window\.S|localStorage|sessionStorage|console\.|innerHTML|AppShell|send_text/);
});
test('initial empty-session run request is same-origin and has no implicit session', async () => {
  const h = harness(); await flush(); assert.equal(h.calls.length, 1);
  assert.equal(h.calls[0].url, 'https://local/app/api/runs'); assert.equal(h.calls[0].opts.credentials, 'same-origin');
  assert.equal(h.el('runsBar').style.display, ''); assert.equal(h.el('runsBar').children[0].textContent, '暂无可见的分析记录。');
});
test('issue requires human click; hides secret, manual show/copy, close clears it', async () => {
  const h = harness(); await open(h); assert.equal(h.calls.filter(x=>x.opts.method==='POST').length,0);
  await issue(h); assert.equal(h.el('agentTokenSecret').value, ''); assert.equal(h.el('agentTokenSecret').hidden, true);
  await h.el('agentShowToken').click(); assert.equal(h.el('agentTokenSecret').value, 'secret-value');
  await h.el('agentCopyToken').click(); assert.deepEqual(h.copied, ['secret-value']);
  await h.el('agentAccessClose').click(); assert.equal(h.el('agentTokenSecret').value,'');
  await h.el('agentShowToken').click(); assert.equal(h.el('agentTokenSecret').value,'');
  const post = h.calls.find(x=>x.opts.method==='POST'); assert.equal(post.url, 'https://local/app/api/agent/token');
  assert.deepEqual(JSON.parse(post.opts.body), {note:'',ttl_days:30});
});
test('close during delayed issue discards secret and reopening cannot receive it', async () => {
  const h=harness(), d=deferred(); await open(h);
  h.setBackend(async (url, opts)=>opts.method==='POST'?d.promise:{ok:true,tokens:[]});
  const pending=h.el('agentIssueToken').click(); await h.el('agentAccessClose').click(); await open(h);
  d.resolve({ok:true,token:'stale-secret',id:'t1'}); await pending;
  assert.equal(h.el('agentSecretActions').hidden,true); await h.el('agentShowToken').click(); assert.equal(h.el('agentTokenSecret').value,'');
});
test('revoke explicitly sends id only and clears newly issued secret', async () => {
  const h=harness(); await open(h); await issue(h);
  h.setBackend(async (url, opts)=>({ok:true,tokens:[{id:'t1',note:'<img src=x onerror=alert(1)>',expires_at:'later'}]}));
  await h.el('agentRefreshTokens').click(); const row=h.el('agentTokenList').children[0];
  assert.equal(row.children[0].textContent,'<img src=x onerror=alert(1)> · t1');
  await row.children[2].click(); const req=h.calls.find(x=>x.url.endsWith('/revoke'));
  assert.deepEqual(JSON.parse(req.opts.body),{id:'t1'}); assert.equal(h.el('agentSecretActions').hidden,true);
});
test('runs uses server sentence, readonly and escaped text, switching calls existing function',async()=>{
  const h=harness(); await flush(); h.setBackend(async()=>({ok:true,items:[{session_id:'B',name:'<script>x</script>',sentence:'服务端原话',readonly:true}]}));
  await h.el('btnRefreshRuns').click(); const button=h.el('runsBar').children[0];
  assert.equal(button.children[0].textContent,'<script>x</script>'); assert.equal(button.children[1].textContent,'服务端原话'); assert.equal(button.children[2].textContent,'共享 · 只读');
  await button.click(); assert.equal(h.run('selected[0]'),'B');
});
test('run polling never overlaps and pauses hidden',async()=>{
  const h=harness(); await flush(); const d=deferred(); h.setBackend(()=>d.promise);
  const pending=h.el('btnRefreshRuns').click(); const count=h.calls.length;
  await h.el('btnRefreshRuns').emit('click'); assert.equal(h.calls.length,count);
  h.document.hidden=true; await h.document.emit('visibilitychange'); d.resolve({ok:true,items:[]}); await pending;
  assert.equal([...h.timers.values()].filter(t=>t.ms===5000).length,0);
  h.document.hidden=false; h.setBackend(async()=>({ok:true,items:[]})); await h.document.emit('visibilitychange'); await flush(); assert.equal(h.calls.length,count+1);
});
test('notebook binds explicit record and downloads blob under subpath',async()=>{
  const h=harness(); await flush(); h.run("S.sessionId='A'");
  h.setBackend(async()=>({ok:true,session_id:'A',note:'路径检查通过'})); await h.el('btnBuildNotebook').click();
  const req=h.calls.find(x=>x.url.includes('/build')); assert.deepEqual(JSON.parse(req.opts.body),{session_id:'A'}); assert.equal(new URL(req.url).search,'');
  await h.el('btnDownloadNotebook').click(); assert.ok(h.calls.some(x=>x.url==='https://local/app/api/notebook/download?session=A'));
  assert.equal(h.downloads.length,1); assert.equal(h.downloads[0].download,'analysis-A.ipynb');
});
test('notebook late download including A-B-A is ignored',async()=>{
  const h=harness(); await flush(); h.run("S.sessionId='A'"); const d=deferred(); h.setBackend(()=>d.promise);
  const pending=h.el('btnDownloadNotebook').click(); h.run("generation++; S.sessionId='B'; generation++; S.sessionId='A'"); d.resolve({ok:true}); await pending;
  assert.equal(h.downloads.length,0); assert.match(h.el('notebookStatus').textContent,/已忽略/);
});
test('notebook errors and identity errors are visible without arbitrary response secrets',async()=>{
  const h=harness(); await flush(); h.run("S.sessionId='A'"); h.setBackend(async()=>({httpOk:false,status:403,ok:false,code:'notebook_not_owner',error:'accidental-secret'}));
  await h.el('btnBuildNotebook').click(); assert.match(h.el('notebookStatus').textContent,/notebook_not_owner/); assert.doesNotMatch(h.el('notebookStatus').textContent,/accidental-secret/);
  await open(h); assert.match(h.el('agentAccessStatus').textContent,/notebook_not_owner/);
});
test('no notebook request without session, cancellation clears credential',async()=>{
  const h=harness(); await flush(); const before=h.calls.length; await h.el('btnBuildNotebook').click(); assert.equal(h.calls.length,before);
  await open(h); await issue(h); await h.el('agentAccessDialog').emit('cancel'); assert.equal(h.el('agentTokenSecret').value,''); assert.equal(h.el('agentSecretActions').hidden,true);
});
test('declared replay builds only explicit current SID and downloads own blob', async () => {
  const h=harness(); await flush(); h.run("S.sessionId='own'");
  h.el('replayManifest').files=[{size:32,text:async()=>'{"files":[],"steps":[],"expected_outputs":[]}'}];
  h.setBackend(async()=>({ok:true,session_id:'own'}));
  await h.el('btnBuildReplay').click(); await flush();
  const post=h.calls.find(c=>c.url.endsWith('/replay/build'));
  assert.equal(JSON.parse(post.opts.body).session_id,'own');
  assert.deepEqual(JSON.parse(post.opts.body).manifest,{files:[],steps:[],expected_outputs:[]});
  await h.el('btnDownloadReplay').click(); await flush();
  assert.equal(h.downloads.length,1);
  assert.match(h.calls.at(-1).url,/replay\/download\?session=own$/);
});
test('declared replay file read and download are discarded after record A-B-A', async () => {
  const h=harness(), d=deferred(); await flush(); h.run("S.sessionId='A'");
  h.el('replayManifest').files=[{size:32,text:()=>d.promise}];
  const buildPending=h.el('btnBuildReplay').click(); h.run("generation+=2;S.sessionId='A'");
  d.resolve('{}'); await buildPending; await flush();
  assert.equal(h.calls.filter(c=>c.opts.method==='POST').length,0);
  const q=deferred(); h.setBackend(()=>q.promise);
  const downloadPending=h.el('btnDownloadReplay').click(); h.run("generation+=2;S.sessionId='A'");
  q.resolve({ok:true}); await downloadPending; await flush(); assert.equal(h.downloads.length,0);
});
test('declared replay rejects oversized manifest and suppresses repeated request', async () => {
  const h=harness(); await flush(); h.run("S.sessionId='A'");
  h.el('replayManifest').files=[{size:1048577,text:async()=>'{}'}];
  await h.el('btnBuildReplay').click(); await flush();
  assert.equal(h.calls.filter(c=>c.opts.method==='POST').length,0);
  const d=deferred(); h.setBackend(()=>d.promise);
  const downloadPending=h.el('btnDownloadReplay').click(); await h.el('btnDownloadReplay').click();
  assert.equal(h.calls.filter(c=>c.url.includes('/replay/download')).length,1);
  d.resolve({httpOk:false,ok:false,code:'stale',secret:'not-visible'}); await downloadPending; await flush();
  assert.equal(h.downloads.length,0); assert.doesNotMatch(h.el('notebookStatus').textContent,/not-visible/);
});
