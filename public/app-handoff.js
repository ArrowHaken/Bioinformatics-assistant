/* catsco.app-handoff.v1. Load after app.js; explicit backend-authorized run; no direct command execution. */
(function () {
  'use strict';
  const protocol = 'catsco.app-handoff.v1';
  const timeoutMs = 10000;
  const steps = ['all', 'env', 'decide', 'precheck', 'pipeline1', 'pipeline2', 'plots', 'ppt', 'pack'];
  let supportsRun = false;
  const base = ['protocol', 'type', 'nonce', 'request_id', 'topic_id', 'agent_uid'];
  let origin = null, pendingHello = null, generation = 0, active = null;
  const successes = new Map();
  const inFlight = new Set();
  let switching = false, switchHello = null;
  const object = v => !!v && typeof v === 'object' && !Array.isArray(v);
  const exact = (v, keys) => object(v) && Object.keys(v).length === keys.length && keys.every(k => Object.prototype.hasOwnProperty.call(v, k));
  const token = v => typeof v === 'string' && /^[A-Za-z0-9][A-Za-z0-9_-]{0,127}$/.test(v);
  const common = d => d.protocol === protocol && ['nonce', 'topic_id', 'agent_uid'].every(k => token(d[k])) &&
    typeof d.request_id === 'string' && /^[A-Za-z0-9_-]{8,80}$/.test(d.request_id);
  function validIntent(value) {
    if (typeof value !== 'string' || [...value].length > 512) return false;
    let it;
    try { it = JSON.parse(value); } catch (_) { return false; }
    const nested = {server:['label','host','user','port','exec_pc','samples','runtime'], paths:['data_root','work_root','rawdata_dir','script_root','liny_data'],
      samples:['count','group_table','groups'], reference:['notebook','modified_cells'], scope:['run_until','annotation_gate'],
      annotation:['policy','merge_adjacent'], source:['topic_id','message_id','raw_text']};
    const allowed = ['schema','project_name','mode','species','tissue',...Object.keys(nested)];
    const scalar = v => typeof v === 'string' || typeof v === 'boolean' || (typeof v === 'number' && Number.isFinite(v));
    if (!object(it) || it.schema !== 'pa.intent.v1' || Object.keys(it).some(k => !allowed.includes(k))) return false;
    return Object.entries(it).every(([k,v]) => nested[k] ? object(v) && Object.entries(v).every(([n,x]) =>
      nested[k].includes(n) && (n === 'runtime' ? object(x) && Object.entries(x).every(([r,v]) =>
        ['rscript','lib_path','ld_library_path','python','queue','qsub','hdf5','node_constraint'].includes(r) && scalar(v)) :
        ['groups','modified_cells'].includes(n) ? Array.isArray(x) && x.every(scalar) : scalar(x))) : scalar(v));
  }
  function validParameters(d) {
    if (!object(d.parameters) || !token(d.parameters.session_id)) return false;
    const allowed = d.action === 'initialize' ? ['session_id','intent'] : ['session_id','step'];
    if (Object.keys(d.parameters).some(k => !allowed.includes(k))) return false;
    if (d.action === 'initialize') return !Object.hasOwn(d.parameters,'intent') || validIntent(d.parameters.intent);
    return d.action === 'run' && supportsRun && (!Object.hasOwn(d.parameters,'step') || steps.includes(d.parameters.step));
  }
  function endpoint(path) {
    const url = new URL(typeof apiUrl === 'function' ? apiUrl(path) : path, window.location.href);
    if (url.origin !== window.location.origin || url.username || url.password || url.search || url.hash || !url.pathname.endsWith(path)) throw new Error('endpoint');
    return url.href;
  }
  async function request(path, body) {
    const controller = new AbortController();
    let timer;
    try {
      return await Promise.race([
        (async () => {
          const response = await fetch(endpoint(path), {
            method: body === undefined ? 'GET' : 'POST', credentials: 'same-origin', cache: 'no-store',
            redirect: 'error', signal: controller.signal,
            ...(body === undefined ? {} : { headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) })
          });
          if (!response.ok || response.redirected) throw new Error('response');
          if (response.url && new URL(response.url, window.location.href).origin !== window.location.origin) throw new Error('response');
          return await response.json();
        })(),
        new Promise((_, reject) => { timer = setTimeout(() => { controller.abort(); reject(new Error('timeout')); }, timeoutMs); })
      ]);
    } finally { controller.abort(); clearTimeout(timer); }
  }
  function send(d, type, extra) {
    window.parent.postMessage({ protocol, type, nonce: d.nonce, request_id: d.request_id,
      topic_id: d.topic_id, agent_uid: d.agent_uid, ...extra }, origin);
  }
  function hello(d) {
    if (!exact(d, base) || d.type !== 'hello' || !common(d)) return;
    // A ready accepts a generation. Never accept it during a noncancellable switch.
    if (switching) { switchHello = { ...d }; return; }
    generation += 1;
    active = { ...d, generation };
    send(d, 'ready', { schema: { session_id: { type: 'string', required: true, maxLength: 128 },
      intent: { type: 'string', maxLength: 512 }, step: { type: 'enum', values: steps } }, supports_run: supportsRun });
  }
  function current(a) { return active === a && generation === a.generation; }
  function selectedSession() {
    return typeof S !== 'undefined' && S ? S.sessionId : undefined;
  }
  async function handoff(d) {
    const a = active;
    if (!a || !exact(d, base.concat(['action', 'parameters', 'expires_at'])) || d.type !== 'handoff' || !common(d)) return;
    if (!['nonce', 'request_id', 'topic_id', 'agent_uid'].every(k => d[k] === a[k])) return;
    const now = Date.now();
    if (!['initialize', 'run'].includes(d.action) || !Number.isSafeInteger(d.expires_at) || d.expires_at <= now || d.expires_at > now + 300000 ||
        !validParameters(d)) {
      send(d, 'ack', { status: 'failed' }); return;
    }
    const envelope = { ...d, parameters: { ...d.parameters } };
    const key = JSON.stringify(base.map(k => d[k]).concat([d.action, d.parameters.session_id,
      d.parameters.intent ?? null, d.action === 'run' ? (d.parameters.step ?? 'all') : null]));
    const requestKey = JSON.stringify(base.map(k => d[k]));
    if (successes.has(requestKey) && successes.get(requestKey) !== key) {
      send(d, 'ack', { status: 'failed' }); return;
    }
    if (inFlight.has(requestKey)) return;
    inFlight.add(requestKey);
    let ownsSwitch = false;
    try {
      const result = await request('/api/app-handoff/' + d.action, envelope);
      if (!current(a)) return;
      if (!result || result.ok !== true || result.session_id !== envelope.parameters.session_id || result.request_id !== d.request_id) throw new Error('denied');
      if (d.action === 'run' && !['running', 'completed'].includes(result.status)) throw new Error('run_status');
      if (typeof switchSession !== 'function') throw new Error('switch_missing');
      if (Date.now() >= envelope.expires_at) throw new Error('expired');
      // Reauthorize even a cached success; only the UI switch is idempotent.
      if (selectedSession() !== result.session_id || d.action === 'run' || Object.hasOwn(d.parameters, 'intent')) {
        switching = true;
        ownsSwitch = true;
        await switchSession(result.session_id);
      }
      if (selectedSession() !== result.session_id || Date.now() >= envelope.expires_at) throw new Error('switch_failed');
      successes.set(requestKey, key);
      send(d, 'ack', { status: d.action === 'run' ? result.status : 'initialized' });
    } catch (_) {
      if (current(a)) send(d, 'ack', { status: 'failed' });
    } finally {
      inFlight.delete(requestKey);
      if (ownsSwitch) {
        switching = false;
        const buffered = switchHello;
        switchHello = null;
        if (buffered) hello(buffered);
      }
    }
  }
  window.addEventListener('message', event => {
    if (window.parent === window || event.source !== window.parent || !object(event.data)) return;
    if (!origin) {
      // Keep only a shape-checked hello: never retain parameters before configuration.
      if (!pendingHello && exact(event.data, base) && event.data.type === 'hello' && common(event.data)) {
        pendingHello = { origin: event.origin, data: { ...event.data } };
      }
      return;
    }
    if (event.origin !== origin) return;
    if (event.data.type === 'hello') hello(event.data);
    else if (event.data.type === 'handoff') void handoff(event.data);
  });
  void request('/api/app-handoff/config').then(config => {
    if (!config || config.ok !== true || typeof config.platform_origin !== 'string') return;
    const url = new URL(config.platform_origin);
    if (url.protocol !== 'https:' || url.origin !== config.platform_origin || url.username || url.password) return;
    supportsRun = config.supports_run === true;
    origin = url.origin;
    const buffered = pendingHello;
    pendingHello = null;
    if (buffered && buffered.origin === origin) hello(buffered.data);
  }).catch(() => { pendingHello = null; });
})();
