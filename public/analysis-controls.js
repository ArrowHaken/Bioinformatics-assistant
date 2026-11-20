/* Owner-scoped controls intentionally bypass api(): it implicitly binds the current session. */
(() => {
  'use strict';
  const byId = id => document.getElementById(id);
  const runs = byId('runsBar');
  const dashboard = byId('parallelDashboard');
  const notebookNote = byId('nbStatus');
  let runBusy = false, runTimer = null, runController = null, notebookBusy = false;
  window.runSummaries = new Map();
  const text = (node, value) => { if (node) node.textContent = String(value == null ? '' : value); };
  const element = (tag, value, cls) => {
    const node = document.createElement(tag);
    if (value != null) text(node, value);
    if (cls) node.className = cls;
    return node;
  };
  function endpoint(path) {
    const url = new URL(apiUrl(path), location.href);
    if (url.origin !== location.origin) throw new Error('拒绝非同源请求');
    return url.href;
  }
  async function request(path, body, options = {}) {
    const controller = options.controller || new AbortController();
    const timeout = setTimeout(() => controller.abort(), 30000);
    try {
      const response = await fetch(endpoint(path), {
        method: body === undefined ? 'GET' : 'POST', credentials: 'same-origin',
        cache: 'no-store', signal: controller.signal,
        ...(body === undefined ? {} : {headers: {'Content-Type': 'application/json'}, body: JSON.stringify(body)})
      });
      if (options.blob && response.ok) return await response.blob();
      let data;
      try { data = await response.json(); } catch (_) { throw new Error('服务返回了无法识别的响应'); }
      if (!response.ok || !data || data.ok !== true) {
        // Never echo an arbitrary response body: it may contain credentials.
        throw new Error('请求未完成' + (data && data.code ? '（' + String(data.code) + '）' : '（HTTP ' + response.status + '）'));
      }
      return data;
    } catch (error) {
      if (error.name === 'AbortError') throw new Error('请求超时或已取消，请重试');
      throw error;
    } finally { clearTimeout(timeout); }
  }
  const duration = value => Number.isFinite(value) && value >= 0 ? (value < 60 ? Math.round(value) + '秒' : Math.floor(value / 60) + '分' + Math.round(value % 60) + '秒') : '未知';
  function rowButton(item) {
    const button = element('button', null, 'ac-run'); button.type = 'button';
    button.dataset.runSid = String(item.session_id);
    button.appendChild(element('strong', item.name || item.session_id));
    if (item.readonly === true) button.appendChild(element('span', '只读查看'));
    const lifecycle = String(item.lifecycle || '');
    const lifecycleText = lifecycle === 'queued' ? '排队中'
      : (lifecycle === 'running' && item.is_actively_running === true ? '运行中' : '状态待核实');
    button.appendChild(element('span', lifecycleText, 'ac-lifecycle'));
    button.appendChild(element('span', '环节 ' + (item.stage_index == null ? '未知' : item.stage_index) + '/' + (item.stage_total == null ? '未知' : item.stage_total)));
    button.appendChild(element('span', '耗时 ' + duration(item.elapsed_seconds)));
    button.appendChild(element('span', '预计剩余 ' + duration(item.eta_seconds)));
    const meter = element('progress'); meter.max = 1;
    if (Number.isFinite(item.progress_fraction)) meter.value = Math.max(0, Math.min(1, item.progress_fraction));
    meter.setAttribute('aria-label', Number.isFinite(item.progress_fraction) ? '阶段进度 ' + Math.round(item.progress_fraction * 100) + '%' : '进度未知');
    button.appendChild(meter);
    if (String(item.session_id) === String(S.sessionId)) button.setAttribute('aria-current', 'true');
    button.addEventListener('click', async () => {
      if (sessionChanging || !item.session_id) return;
      button.disabled = true;
      try {
        if (item.readonly === true) {
          if (typeof openReadOnlySession === 'function') await openReadOnlySession(String(item.session_id));
          switchTab('cur');
        } else {
          await switchSession(String(item.session_id));
          if (String(S.sessionId) === String(item.session_id)) switchTab('cur');
        }
      }
      finally { button.disabled = false; text(notebookNote, ''); refreshRuns(); }
    });
    return button;
  }
  async function refreshRuns() {
    if (runBusy || document.hidden) return;
    clearTimeout(runTimer); runBusy = true; runController = new AbortController();
    try {
      const data = await request('/api/runs', undefined, {controller: runController});
      if (document.hidden) return;
      const items = Array.isArray(data.items) ? data.items : [];
      window.runSummaries = new Map(items.map(item => [String(item.session_id), item]));
      const active = items.filter(item => item.is_actively_running === true
        || ['queued', 'running'].includes(String(item.lifecycle || '')));
      const wasHidden = dashboard.hidden;
      dashboard.hidden = active.length === 0;
      if (dashboard.hidden || wasHidden) dashboard.open = false;
      text(byId('parallelSummary'), '并行分析 · ' + active.length + ' 条运行中');
      const focusedSid = runs.contains(document.activeElement) ? document.activeElement.dataset.runSid : null;
      runs.replaceChildren(...active.map(rowButton));
      if (focusedSid && !dashboard.hidden) {
        const restored = Array.from(runs.children).find(button => button.dataset.runSid === focusedSid);
        if (restored) restored.focus({preventScroll: true});
      }
      if (typeof renderSessions === 'function' && curTab === 'hist') renderSessions();
    } catch (_) {
      dashboard.hidden = true; dashboard.open = false;
      window.runSummaries = new Map();
      if (typeof renderSessions === 'function' && curTab === 'hist') renderSessions();
    } finally {
      runBusy = false; runController = null;
      if (!document.hidden) runTimer = setTimeout(refreshRuns, 5000);
    }
  }
  document.addEventListener('visibilitychange', () => {
    clearTimeout(runTimer);
    if (document.hidden) { if (runController) runController.abort(); }
    else refreshRuns();
  });
  /* ---- 产物面板的两个页签 -----------------------------------------
     查看各环节产物 / 打包 ipynb。
     两个面板互斥：一次只显示一个，高亮跟着走。
     为什么不再用「再点一次收起」：收起后两个都不亮，人会以为坏了；
     而且产物清单本来就是这一页的默认内容，没有"什么都不显示"的状态。
     原「执行快照」页签已移除（gina 2026-11-20）：它与打包 ipynb 生成的是
     同一份文件，留两个入口只会让人以为有两种东西。 */
  const panels = { art: 'panelArt', nb: 'panelNb' };
  const openPanel = which => {
    Object.keys(panels).forEach(k => {
      const el = byId(panels[k]);
      if (el) el.hidden = (k !== which);
    });
    document.querySelectorAll('#artToolbar .artpt').forEach(b => {
      b.classList.toggle('on', b.dataset.artpanel === which);
    });
  };
  document.querySelectorAll('#artToolbar .artpt').forEach(b => {
    b.addEventListener('click', () => {
      const which = b.dataset.artpanel;
      if (!which || !byId(panels[which])) return;
      openPanel(which);
    });
  });

  /* 打包 ipynb 的状态独立记一份。
     为什么不能跟产物取回混用：取回有没有完成，与 ipynb 生成过没有是两件事；
     混成一个就会出现"没做却说做好了"。 */
  const built = { nb: false };
  const statusNode = { nb: byId('nbStatus') };
  const nbButtons = [byId('btnBuildNb'), byId('btnDownloadNb')];

  async function notebookAction(kind, build) {
    if (notebookBusy) return;
    const note = statusNode[kind];
    if (sessionChanging || !S.sessionId) { text(note, '请先选择一条分析记录。'); return; }
    // 下载必须先有产物：没有就明说「请先生成」，不让人对着没反应的按钮反复点。
    if (!build && !built[kind]) {
      const msg = '请先生成 ipynb';
      text(note, msg);
      if (typeof toast === 'function') toast(msg, 'bad');
      return;
    }
    const sid = String(S.sessionId), generation = recordContext();
    const current = () => recordCurrent(generation) && !sessionChanging && String(S.sessionId) === sid;
    const group = nbButtons;
    notebookBusy = true; group.forEach(button => { if (button) button.disabled = true; });
    text(note, (build ? '正在生成' : '正在下载') + '记录 ' + sid + ' 的 ipynb…');
    try {
      if (build) {
        const data = await request('/api/notebook/build', {session_id: sid});
        if (!current()) return;
        if (String(data.session_id) !== sid) throw new Error('服务返回了不同记录，已忽略');
        built[kind] = true;
        text(note, '记录 ' + sid + ' 已生成。' + (data.note || '') + ' 可以下载了。');
      } else {
        const blob = await request('/api/notebook/download?session=' + encodeURIComponent(sid), undefined, {blob: true});
        if (!current()) return;
        const url = URL.createObjectURL(blob), anchor = element('a');
        anchor.href = url;
        anchor.download = 'analysis-' + sid.replace(/[^a-zA-Z0-9_-]/g, '_') + '.ipynb';
        document.body.appendChild(anchor);
        try { anchor.click(); } finally { anchor.remove(); setTimeout(() => URL.revokeObjectURL(url), 1000); }
        text(note, '记录 ' + sid + ' 的 ipynb 已交给浏览器下载。');
      }
    } catch (error) {
      if (current()) text(note, 'ipynb：' + error.message);
    } finally {
      notebookBusy = false; group.forEach(button => { if (button) button.disabled = false; });
      if (!current()) text(note, '记录已切换，已忽略旧记录的响应。');
    }
  }
  if (byId('btnBuildNb')) byId('btnBuildNb').addEventListener('click', () => notebookAction('nb', true));
  if (byId('btnDownloadNb')) byId('btnDownloadNb').addEventListener('click', () => notebookAction('nb', false));

  window.addEventListener('pagehide', () => { clearTimeout(runTimer); if (runController) runController.abort(); });
  refreshRuns();
})();
