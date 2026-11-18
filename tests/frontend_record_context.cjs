'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const path = require('node:path');

const flush = () => new Promise(resolve => setImmediate(resolve));

function lightweightElements() {
  const elements = new Map();
  class Element {
    constructor(tag) {
      this.tagName = tag;
      this.children = [];
      this.events = {};
      this.style = {};
      this.dataset = {};
      this.attrs = {};
      this.className = '';
      this.textContent = '';
      this.hidden = false;
      this.disabled = false;
      this.open = false;
    }
    appendChild(child) { this.children.push(child); return child; }
    replaceChildren(...children) { this.children = children; }
    setAttribute(key, value) { this.attrs[key] = value; }
    addEventListener(event, fn) { (this.events[event] ||= []).push(fn); }
    async emit(event) { for (const fn of this.events[event] || []) await fn({preventDefault(){}}); }
    click() { return this.disabled ? Promise.resolve() : this.emit('click'); }
    remove() {}
    focus() {}
    contains(node) { return node === this || this.children.includes(node); }
    classList = { add() {}, remove() {}, toggle() {} };
    querySelectorAll() { return []; }
    querySelector() { return null; }
    getAttribute() { return ''; }
  }
  const element = id => {
    if (!elements.has(id)) elements.set(id, new Element(id));
    return elements.get(id);
  };
  return { elements, Element, element };
}

function analysisHarness(rows) {
  const dom = lightweightElements();
  const calls = [], selected = [];
  const source = fs.readFileSync(path.join(__dirname, '../public/analysis-controls.js'), 'utf8');
  const document = new dom.Element('document');
  document.hidden = false;
  document.activeElement = null;
  document.body = new dom.Element('body');
  document.getElementById = dom.element;
  document.createElement = tag => new dom.Element(tag);
  document.querySelectorAll = () => [];
  document.addEventListener = () => {};
  const window = new dom.Element('window');
  window.addEventListener = () => {};
  const context = vm.createContext({
    document, window, URL, AbortController,
    location: { href: 'https://local/app/', origin: 'https://local' },
    S: { sessionId: 'A' },
    sessionChanging: false,
    curTab: 'hist',
    recordContext: () => 'gen-A',
    recordCurrent: value => value === 'gen-A',
    apiUrl: p => '/app/' + String(p).replace(/^\/+/, ''),
    switchTab: () => {},
    switchSession: async id => { selected.push(id); },
    openReadOnlySession: async id => { selected.push('readonly:' + id); },
    renderSessions: () => {},
    clearTimeout,
    setTimeout: () => 0,
    fetch: async (url, opts) => {
      calls.push({url, opts});
      return { ok: true, status: 200, json: async () => ({ok: true, items: rows}) };
    }
  });
  vm.runInContext(source, context);
  return { context, dom, document, calls, selected };
}

test('dashboard shows running and queued, and shared rows are read-only', async () => {
  const h = analysisHarness([
    {session_id: 'RUN', name: '运行中', lifecycle: 'running', is_actively_running: true, readonly: false},
    {session_id: 'QUEUE', name: '排队中', lifecycle: 'queued', readonly: false},
    {session_id: 'S', name: '别人共享', lifecycle: 'running', readonly: true}
  ]);
  await flush();
  const dashboard = h.dom.element('parallelDashboard');
  const runs = h.dom.element('runsBar');
  assert.equal(dashboard.hidden, false);
  assert.equal(runs.children.length, 3);
  const shared = runs.children.find(row => row.dataset.runSid === 'S');
  assert.ok(shared);
  assert.match(shared.children.map(child => child.textContent).join(' '), /只读查看/);
  assert.match(shared.children.map(child => child.textContent).join(' '), /状态待核实/);
  assert.match(runs.children.find(row => row.dataset.runSid === 'RUN').children.map(child => child.textContent).join(' '), /运行中/);
  assert.match(runs.children.find(row => row.dataset.runSid === 'QUEUE').children.map(child => child.textContent).join(' '), /排队中/);
  await shared.click();
  await flush();
  assert.deepEqual(h.selected, ['readonly:S']);
  const queued = runs.children.find(row => row.dataset.runSid === 'QUEUE');
  await queued.click();
  await flush();
  assert.deepEqual(h.selected, ['readonly:S', 'QUEUE']);
});

test('a single queued record is visible and labelled queued', async () => {
  const h = analysisHarness([
    {session_id: 'ONLY', name: '唯一排队', lifecycle: 'queued', readonly: false}
  ]);
  await flush();
  const runs = h.dom.element('runsBar');
  assert.equal(h.dom.element('parallelDashboard').hidden, false);
  assert.equal(runs.children.length, 1);
  assert.match(runs.children[0].children.map(child => child.textContent).join(' '), /排队中/);
});

function appHarness() {
  const dom = lightweightElements();
  const calls = [];
  let backend = async () => ({ok: true});
  const source = fs.readFileSync(path.join(__dirname, '../public/app.js'), 'utf8');
  const context = vm.createContext({
    console, URL, Date, JSON, Promise, AbortController,
    location: {href: 'http://local/app/', pathname: '/app/'},
    navigator: {},
    window: {addEventListener() {}},
    document: {
      currentScript: null,
      addEventListener() {},
      querySelector: dom.element,
      querySelectorAll: () => [],
      getElementById: id => dom.element('#' + id),
      createElement: () => new dom.Element('new')
    },
    prompt: () => '',
    setTimeout: fn => { fn(); return 0; },
    clearTimeout: () => {},
    setInterval: () => 0,
    clearInterval: () => {},
    fetch: async (url, opts) => {
      calls.push({url, opts});
      const body = await backend(url, opts);
      return {ok: true, status: 200, text: async () => JSON.stringify(body)};
    }
  });
  vm.runInContext(source.replace(/boot\(\);\s*$/, ''), context);
  const run = code => vm.runInContext(code, context);
  run("S.sessionId='A'; S.runDir='/A'; toast=()=>{}; renderAll=()=>{}; renderSessionsBar=()=>{}; renderSessions=()=>{}; setStageVisible=()=>{}; closeDlg=()=>{}; resetRecordState=()=>{}; stopPolling=()=>{}; stopWaitWriteback=()=>{}; loadPlace=async()=>{}; loadRun=async()=>{};");
  return {context, calls, run, setBackend: fn => { backend = fn; }};
}

test('record requests bind one canonical sid for query and body', async () => {
  const h = appHarness();
  await h.run("getJSON('/api/flow'); postJSON('/api/run', {step:'pipeline1'}); getJSON('/api/flow?session=B'); postJSON('/api/run', {session_id:'B', step:'pipeline1'})");
  await flush();
  assert.match(h.calls[0].url, /\/api\/flow\?session=A$/);
  assert.equal(new URL(h.calls[1].url, 'http://local').searchParams.get('session'), 'A');
  assert.equal(JSON.parse(h.calls[1].opts.body).session_id, 'A');
  assert.match(h.calls[2].url, /\/api\/flow\?session=B$/);
  assert.equal(new URL(h.calls[3].url, 'http://local').searchParams.get('session'), 'B');
  assert.equal(JSON.parse(h.calls[3].opts.body).session_id, 'B');
});

test('shared read-only view loads an explicit state id without switching current', async () => {
  const h = appHarness();
  h.setBackend(async url => {
    if (url.includes('/api/sessions/state')) return {
      ok: true, current: 'A',
      session: {id: 'S', mode: 'server', run_dir: '/S', can_write: false, readonly: true},
      sessions: []
    };
    if (url.includes('/api/sessions/switch')) throw new Error('switch must not be called');
    return {ok: true};
  });
  await h.run("openReadOnlySession('S')");
  await flush();
  const stateCalls = h.calls.filter(call => call.url.includes('/api/sessions/state'));
  assert.equal(stateCalls.length, 1);
  assert.match(stateCalls[0].url, /\/api\/sessions\/state\?id=S$/);
  assert.equal(new URL(stateCalls[0].url, 'http://local').searchParams.has('session'), false);
  assert.equal(h.calls.some(call => call.url.includes('/api/sessions/switch')), false);
  assert.equal(h.run('S.sessionId'), 'S');
  assert.equal(h.run('S.readonly'), true);
});
