/* Owner-scoped controls intentionally bypass api(): it implicitly binds the current session. */
(() => {
  'use strict';
  const byId = id => document.getElementById(id);
  const runs = byId('runsBar');
  const dashboard = byId('parallelDashboard');
  const notebookNote = byId('notebookStatus');
  let runBusy = false, runTimer = null, runController = null, notebookBusy = false;
  window.runSummaries = new Map();
  const text = (node, value) => { node.textContent = String(value == null ? '' : value); };
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
      try { await switchSession(String(item.session_id)); if (String(S.sessionId) === String(item.session_id)) switchTab('cur'); }
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
      const active = items.filter(item => item.is_actively_running === true && item.lifecycle === 'running');
      const wasHidden = dashboard.hidden;
      dashboard.hidden = active.length < 2;
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
  const notebookButtons = [byId('btnBuildNotebook'), byId('btnDownloadNotebook')];
  async function notebookAction(build) {
    if (notebookBusy) return;
    if (sessionChanging || !S.sessionId) { text(notebookNote, '请先选择一条分析记录。'); return; }
    const sid = String(S.sessionId), generation = recordContext();
    const current = () => recordCurrent(generation) && !sessionChanging && String(S.sessionId) === sid;
    notebookBusy = true; notebookButtons.forEach(button => { button.disabled = true; });
    text(notebookNote, (build ? '正在生成' : '正在下载') + '记录 ' + sid + ' 的 notebook…');
    try {
      if (build) {
        const data = await request('/api/notebook/build', {session_id: sid});
        if (!current()) return;
        if (String(data.session_id) !== sid) throw new Error('服务返回了不同记录，已忽略');
        text(notebookNote, '记录 ' + sid + ' 已生成。' + (data.note || '') + ' 可点击下载 notebook。');
      } else {
        const blob = await request('/api/notebook/download?session=' + encodeURIComponent(sid), undefined, {blob: true});
        if (!current()) return;
        const url = URL.createObjectURL(blob), anchor = element('a');
        anchor.href = url; anchor.download = 'analysis-' + sid.replace(/[^a-zA-Z0-9_-]/g, '_') + '.ipynb';
        document.body.appendChild(anchor);
        try { anchor.click(); } finally { anchor.remove(); setTimeout(() => URL.revokeObjectURL(url), 1000); }
        text(notebookNote, '记录 ' + sid + ' 的 notebook 已交给浏览器下载。');
      }
    } catch (error) { if (current()) text(notebookNote, 'Notebook：' + error.message); }
    finally {
      notebookBusy = false; notebookButtons.forEach(button => { button.disabled = false; });
      if (!current()) text(notebookNote, '记录已切换，已忽略旧记录的 notebook 响应。');
    }
  }
  byId('btnBuildNotebook').addEventListener('click', () => notebookAction(true));
  byId('btnDownloadNotebook').addEventListener('click', () => notebookAction(false));
  async function declaredReplay(build) {
    if (notebookBusy || sessionChanging || !S.sessionId) return;
    const sid = String(S.sessionId), generation = recordContext();
    const current = () => recordCurrent(generation) && !sessionChanging && String(S.sessionId) === sid;
    notebookBusy = true;
    text(notebookNote, '正在校验记录 ' + sid + ' 的显式重放清单…');
    try {
      if (build) {
        const file = byId('replayManifest').files[0];
        if (!file || file.size > 1024 * 1024) throw new Error('请选择不超过 1MB 的 JSON 清单');
        const manifest = JSON.parse(await file.text());
        if (!current()) return;
        const data = await request('/api/notebook/replay/build', {session_id: sid, manifest});
        if (!current()) return;
        if (String(data.session_id) !== sid) throw new Error('记录不匹配');
        text(notebookNote, '已打包授权清单。仅包含声明文件；完整科学分析尚未验证。');
      } else {
        const blob = await request('/api/notebook/replay/download?session=' + encodeURIComponent(sid), undefined, {blob: true});
        if (!current()) return;
        const url = URL.createObjectURL(blob), anchor = element('a');
        anchor.href = url; anchor.download = 'analysis-' + sid.replace(/[^a-zA-Z0-9_-]/g, '_') + '-declared-replay.ipynb';
        document.body.appendChild(anchor);
        try { anchor.click(); } finally { anchor.remove(); setTimeout(() => URL.revokeObjectURL(url), 1000); }
        text(notebookNote, '授权清单 notebook 已交给浏览器下载，运行后逐项核对产物。');
      }
    } catch (_) { if (current()) text(notebookNote, '清单打包或下载未通过。请核对记录绑定、文件授权和本次执行结果。'); }
    finally { notebookBusy = false; if (!current()) text(notebookNote, '记录已切换，已忽略旧记录的清单响应。'); }
  }
  byId('btnBuildReplay').addEventListener('click', () => declaredReplay(true));
  byId('btnDownloadReplay').addEventListener('click', () => declaredReplay(false));
  window.addEventListener('pagehide', () => { clearTimeout(runTimer); if (runController) runController.abort(); });
  refreshRuns();
})();
