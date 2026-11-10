/* =====================================================================
   个性化分析助手 · 页面逻辑
   后端是本机的一个小服务（serve.R），同源同进程起在本机回环地址上。
   所以：不需要浏览器目录权限、不受 iframe 限制、能真的调 R / Python。
   所有重活都在服务端：列目录、跑命令、收产物。
   ===================================================================== */
'use strict';
const $ = (s, r) => (r || document).querySelector(s);
const $$ = (s, r) => Array.prototype.slice.call((r || document).querySelectorAll(s));
const esc = s => String(s === null || s === undefined ? '' : s)
  .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
  .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
const arr = x => Array.isArray(x) ? x : (x === null || x === undefined ? [] : [x]);
const nowTime = () => new Date().toLocaleTimeString('zh-CN', { hour12: false });

function fmtSize(n) {
  if (n === null || n === undefined || n === '') return '';
  n = Number(n);
  if (isNaN(n)) return '';
  if (n < 1024) return n + ' B';
  if (n < 1048576) return (n / 1024).toFixed(0) + ' KB';
  if (n < 1073741824) return (n / 1048576).toFixed(1) + ' MB';
  return (n / 1073741824).toFixed(2) + ' GB';
}
function toast(msg, kind) {
  let host = $('#toast');
  const el = document.createElement('div');
  el.className = 'toastmsg' + (kind ? ' ' + kind : '');
  el.textContent = msg;
  host.appendChild(el);
  setTimeout(() => el.remove(), kind === 'bad' ? 8000 : 4000);
}

const S = {
  env: null, runDir: null, runDirRemote: false, pipeline: null, gates: null, steps: [], warns: [],
  gateState: {}, gateOpen: {}, dirtyParams: {},
  artifacts: [], envCheck: null, running: false, blocked: false, step: null, code: null,
  browseDir: null, browseParent: null, pollTimer: null,
  history: null, recentDirs: [], scripts: null, openScript: null,
  scriptDraft: null, scriptTab: 'list', incoming: [], queue: [], activeNav: null,
  scriptCheck: null, scriptBackups: null,
  // 「保存并识别」顺带自动注入会话的结果。
  //   常驻显示，不靠 toast —— toast 一闪就没了，而"这次到底有没有交给助手"
  //   是要一直摆在那让人核对的信息。
  autoInject: null, autoInjectBusy: false, autoInjectAt: 0,
  // 本次「保存并识别」的请求号（服务端生成）。
  //   它随注入文本一起交给助手，助手回写时原样带回，
  //   页面据此判断"手上的结果是这一次跑出来的"还是上一轮的旧数据。
  commitReq: null,
  // 体检结果的新鲜度：stale=true 表示有结果、但不属于本次请求。
  envStale: false, staleEnv: null,
  // 「保存并识别」之后，识别结论处在哪一步。
  //   为什么要这个状态：服务器模式下识别不由本机执行 ——
  //   本机连不到内网，这一趟必须由助手代跑，结论要等它写回才有。
  //   没有这个状态，页面就会在助手还没动的时候，把"本机连不上服务器"
  //   当成识别结果摆出来（读起来就是"识别不了"），而其实什么都还没开始。
  //   null=还没点过  waiting=已交给助手、等写回  ready=结论已到  timeout=还没回来
  identifyPhase: null,
  // 服务器模式下"这一步交给助手"的结果。
  //   为什么要常驻：toast 一闪就没了，而"到底有没有送进会话"是要能核对的；
  //   送失败时还得把指令原文摆出来，让人可以自己复制发一次。
  delegated: null,
  // 助手回写的每步执行状态（服务器模式）。step id → {status, code, log, imported_at}
  remoteSteps: {},
  // 流程进度（阶段 + 环节 + 耗时 + 质检结论），来自 /api/flow。
  flow: null,
  // 处理过程里每个阶段被手动展开(0)/收起(1)过没有；没表态才按"跑着就展开"算
  flowOpen: {},
  flowTimer: null,
  waitTimer: null, waitTries: 0, waitSince: 0, staleEnvAt: '',
  // ---- 多份分析记录 -------------------------------------------------
  //   这一页不再只有「这一次」。每次分析是一份记录，页面停在其中一份上。
  //   sessionId：现在看的是哪一份（刷新后靠它回到原处）。
  //   collapsed：识别结论里哪几个小节被收起了，按记录存在服务端。
  //   sessions：历史分析列表。
  //   为什么要落服务端而不是 localStorage：换一台电脑、换个浏览器打开，
  //   看到的东西必须一致；localStorage 只在这台浏览器上有。
  sessionId: null, sessions: [], collapsed: {},
  // ---- 我是在哪个会话里被打开的 -------------------------------------
  //   应用挂公网子路径后，自己看不出宿主会话。而注入指令要落到对的那个会话，
  //   助手也得知道回写到哪儿 —— 所以启动时向本机后端问一次（后端去问网关）。
  //   topicId 形如 cc_group_grp_5813；拿不到就是 null，此时不猜、如实说没拿到。
  identity: null, topicId: null,
};

/* ---------------- 与服务端说话 ---------------- */
/* API 基址：从这个页面自己的位置推出来，不写死。
   为什么必须这样：
     同一个页面可能挂在两种位置 ——
       本机直连：   http://127.0.0.1:18980/            （根路径）
       云端应用：   https://…/personal-analysis/      （子路径）
     如果写死 fetch('/api/env')，在云端会请求到 /api/env ——
     那是站点根下的路径，不属于这个应用，返回 404，
     页面就显示「连不上本地服务」。而服务其实是好的。

   优先用「本脚本自己的 src」推，因为那最直接反映应用装在哪；
   取不到时退回 location.pathname。 */
const API_BASE = (() => {
  try {
    const me = document.currentScript && document.currentScript.src;
    if (me) return new URL(me, location.href).pathname.replace(/[^/]*$/, '');
  } catch (e) { /* 退回下面 */ }
  let p = location.pathname || '/';
  if (!p.endsWith('/')) p = p.slice(0, p.lastIndexOf('/') + 1);
  return p;
})();
const apiUrl = p => API_BASE + String(p == null ? '' : p).replace(/^\/+/, '');
async function api(path, opts) {
  const url = apiUrl(path);
  let res;
  try {
    res = await fetch(url, opts);
  } catch (e) {
    // 把实际请求的地址带出来。没有这一句，下次再出问题只能看到
    // 「Failed to fetch」，看不出是路径错了还是服务没起。
    throw new Error('请求失败（' + url + '）：' + (e && e.message ? e.message : e));
  }
  const text = await res.text();
  let body = null;
  try { body = JSON.parse(text); } catch { /* 非 JSON */ }
  if (!res.ok) {
    // 把响应体一起带出来：业务层拒绝（比如"上一步没过质检"）说的是有用的话，
    //   只丢一条 message 出去，调用方就分不清是"被流程拦下"还是"服务坏了"。
    const err = new Error((body && body.error) || text.slice(0, 200) || ('HTTP ' + res.status + ' @ ' + url));
    err.status = res.status;
    err.body = body;
    throw err;
  }
  return body;
}
const getJSON = p => api(p);
const postJSON = (p, obj) => api(p, {
  method: 'POST', headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify(obj || {}),
});

/* ---------------- 词表 ---------------- */
const SRC_LABEL = {
  official: { t: '照官方', c: 'src-official', why: '官方脚本里就是这么写的' },
  instance: { t: '本机适配', c: 'src-instance', why: '为在这台机器做的适配' },
  user: { t: '你定的', c: 'src-user', why: '你亲自指定的' },
  gate: { t: '等你定', c: 'src-gate', why: '要你确认' },
  unspecified: { t: '未标明', c: 'src-unspecified', why: '' },
};
const srcTag = s => {
  const i = SRC_LABEL[s] || SRC_LABEL.unspecified;
  return '<span class="srctag ' + i.c + '" title="' + esc(i.why) + '">' + esc(i.t) + '</span>';
};

const PARAM_LABEL = {
  species: ['物种', '小鼠 / 人 / 大鼠'],
  nfeatures: ['高变基因数', '挑多少基因做降维'],
  dims: ['降维维度', '用前多少个主成分'],
  pcs_doublet: ['去双用的维度', '去双用的主成分数'],
  pN: ['人工双细胞比例', '生成假细胞的比例，官方 0.25'],
  qc_min_features: ['质控下限', '每细胞最少基因数'],
  qc_max_features: ['质控上限', '超过视为异常'],
  qc_mt: ['线粒体上限', '高于视为将死细胞'],
  res1: ['第一次聚类分辨率', '值越大分群越细'],
  res2: ['第二次聚类分辨率', '筛群之后再分一次'],
  seed: ['随机种子', '固定后结果可复现'],
  min_gene_per_type_for_test: ['判定类型的最少基因数', ''],
  rscript_path: ['R 运行程序', '留空就用系统里的'],
  r_lib_paths: ['R 包目录', '多个用逗号分隔'],
  python_path: ['Python 运行程序', '出 PPT 要用'],
  expected_seurat_major: ['Seurat 大版本', '官方用 v4'],
  memory_gb: ['本机内存', 'GB'],
  indir: ['数据目录', '每个样本一个子目录'],
  outdir: ['输出目录', '图、PPT、zip 都放这里'],
  samples_file: ['样本分组表', '两列：样本名 + 分组名'],
  reference_table: ['文献 marker 表', '判定细胞类型的依据'],
  lit_dict: ['marker 词典', '与文献表二选一'],
};
const plabel = k => PARAM_LABEL[k] || [k, ''];

const STEP_ICON = {
  env: '①', decide: '②', precheck: '③', pipeline1: '④',
  pipeline2: '⑤', plots: '⑥', ppt: '⑦', pack: '⑧',
};

/* =====================================================================
   启动
   ===================================================================== */
async function boot() {
  try {
    S.env = await getJSON('/api/env');
  } catch (e) {
    $('#bootErr').style.display = 'block';
    $('#bootErrMsg').textContent = String(e.message || e);
    $('#subline').textContent = '本地服务没连上';
    return;
  }
  bindEvents();
  // 不再有「欢迎页 / 主界面」两态。
  //   以前没选目录就先给一页引导，选完才进主界面；现在第一步本身就
  //   是「选机器 + 选路径」，所以直接进主界面，由第一块负责这件事。
  $('#main').style.display = 'block';
  await loadPlace();
  // 先认清"我在哪个会话里"。拿不到也不阻塞页面，只是不显示会话号。
  await loadIdentity();
  // 先恢复「我在哪一份分析里」。
  //   这一条是需求的根：刷新之后页面必须停在原处，而不是回到空白。
  //   所以顺序是：拿到服务端的记录 -> 决定看哪一份 -> 再按它设置各项状态。
  await loadSessionsState();
  setStageVisible(S.identified);
  renderPlace();
  renderSessionsBar();
  renderSidenav();
  renderJargon();
  await renderIdentify();
  // 有识别结果就把闸门与运行区也画出来，让整页回到上次的样子。
  //   以前这里无条件清空，于是刷新后「需要你决定的事」和「跑分析」全消失。
  if (S.runDir) { try { await loadRun(); } catch (e) {} }
}

/* 位置没确认时，隐藏「需要你决定的事」「跑分析」。
   这两块都要求先知道数据在哪，否则摆出来只是噪音。 */
function setStageVisible(on) {
  ['secGates', 'secRun'].forEach(id => {
    const el = document.getElementById(id);
    if (el) el.style.display = on ? 'block' : 'none';
  });
}

function bindEvents() {
  const bh = $('#btnHistory'); if (bh) bh.addEventListener('click', () => { renderHistory(); openDlg('dlgHistory'); });
  const bn = $('#btnNewSession'); if (bn) bn.addEventListener('click', () => newSession());
  const bs2 = $('#btnSessions'); if (bs2) bs2.addEventListener('click', () => { renderSessions(); openDlg('dlgSessions'); });
  const ba = $('#btnArtifacts'); if (ba) ba.addEventListener('click', () => openDlg('dlgArtifacts'));
  const bv = $('#btnAdv'); if (bv) bv.addEventListener('click', () => { renderSettings(); openDlg('dlgAdv'); });
  const bj = $('#btnJargon'); if (bj) bj.addEventListener('click', () => { renderJargon(); openDlg('dlgJargon'); });
  const br = $('#btnRefreshArt'); if (br) br.addEventListener('click', refreshArtifacts);
  const ra = $('#btnRunAll'); if (ra) ra.addEventListener('click', () => runStep('all'));
  const bss = $('#btnStop'); if (bss) bss.addEventListener('click', stopRun);
  const du = $('#dlgUp'); if (du) du.addEventListener('click', () => { if (S.browseParent) browseTo(S.browseParent); });
  const duse = $('#dlgUse'); if (duse) duse.addEventListener('click', () => { if (S.browseDir) chooseDir(S.browseDir); });
}

/* =====================================================================
   多份分析记录
   ---------------------------------------------------------------------
   一次分析 = 一份记录。页面停在其中一份上，刷新不动它。
   三个入口各管一件事，不要互相兼职：
     新建分析        开一份空白记录（同名复制设置，不复制结论）
     历史分析        列出所有记录，点一份就把整页切过去
     存档与步骤回滚   对当前这一份做存档 / 退回 / 标记从某步重跑
   ===================================================================== */

/* 读回「我在哪一份分析里」。这是刷新后能回到原处的唯一依据。 */
async function loadSessionsState() {
  let d = null;
  try { d = await getJSON('/api/sessions/state'); } catch (e) { d = null; }
  if (!d || !d.ok) return;
  S.sessions = arr(d.sessions);
  S.sessionId = d.current || (d.session && d.session.id) || null;
  const s = d.session || null;
  S.collapsed = (s && s.collapsed) || {};

  // 记录里存的识别结果 = 这一份当时识别出来的东西。
  //   为什么不用 /api/envcheck：那个回答的是"最近一次识别是什么"，
  //   在多份记录并存之后就是错的 —— 打开一份旧记录会看到新记录的数字。
  const ident = s && s.identify;
  if (ident && ident.result) {
    S.envCheck = ident.result;
    S.identified = true;
    // 「这一份是不是服务器模式」以记录自己记下的为准（ident.mode），
    //   不看 placeDraft —— 那要等下面几行才灌进来，顺序反了就会判成本机，
    //   于是「自动判定」被画成"还没跑"，而服务器模式下它本来就随识别一起回来。
    S.needsRemote = (ident.mode === 'server')
      || ((s && s.mode) === 'server');
    S.commitReq = ident.req_id || null;
    S.identifyPhase = 'ready';
    S.envStale = false;
  } else {
    // 这一份还没识别过：不要拿别的记录的结论填进来。
    S.envCheck = null;
    S.identified = false;
    S.identifyPhase = null;
    S.commitReq = null;
  }
  if (s && s.place) {
    S.placeDraft = placeDraftFromServer(s.place);
  }
  if (s && s.run_dir) {
    S.runDir = s.run_dir;
    S.runDirRemote = (s.mode === 'server');
  }
  // 有未回的识别请求：把等待恢复起来，让页面继续自己等。
  if (d.pending_req && !(ident && ident.req_id === d.pending_req)) {
    S.identifyPhase = 'waiting';
    startWaitWriteback();
  }
}

/* 当前这份记录的一行摘要。刷新后它还在，用户一眼知道自己在哪。 */
function renderSessionsBar() {
  const host = $('#sessbar'); if (!host) return;
  const cur = S.sessions.find(s => s.id === S.sessionId) || null;
  if (!cur) {
    host.innerHTML = '<span class="sessempty">还没有分析记录。</span>';
    return;
  }
  let badge = '';
  if (cur.has_identify) {
    const v = String(cur.verdict || '');
    const cls = v === 'ready' ? 'ok' : (v === 'blocked' ? 'bad' : 'warn');
    const txt = cur.verdict_cn || (v === 'ready' ? '可以跑' : (v === 'blocked' ? '跑不了' : '有提醒'));
    badge = '<span class="idbadge ' + cls + '">' + esc(txt) + '</span>';
  } else {
    badge = '<span class="idbadge warn">还没识别</span>';
  }
  host.innerHTML = '<span class="sesscur">当前分析：<b>' + esc(cur.name || cur.id) + '</b></span>'
    + badge
    + (cur.run_dir ? '<span class="sesspath">' + esc(cur.run_dir) + '</span>' : '')
    + '<span class="small">' + esc(String(cur.created_at || '').replace('T', ' '))
    + (cur.n_archives ? '　存档 ' + cur.n_archives + ' 份' : '') + '</span>'
    + '<span class="spacer"></span>'
    + '<button class="btn" data-sess-list="1">切换</button>';
}

/* 新建分析：设置照旧（同一台机器、一样的目录），结论清空。
   为什么要「照旧」：换的通常是数据或判断，不是那台服务器在哪。 */
async function newSession() {
  let name = '';
  try { name = (prompt('这次分析叫什么？', '') || '').trim(); } catch (e) { name = ''; }
  try {
    const r = await postJSON('/api/sessions/new', { name: name });
    if (!r || !r.ok) { toast('新建失败：' + ((r && r.error) || '未知原因'), 'bad'); return; }
    // 先把界面上属于「上一次」的东西清掉，再去读新记录。
    //   不清的话，新记录的第一次渲染会短暂显示上一份的结论。
    S.envCheck = null; S.staleEnv = null; S.envStale = false;
    S.identifyPhase = null; S.commitReq = null; S.autoInject = null;
    S.gates = null; S.pipeline = null; S.steps = [];
    S.collapsed = {};
    stopWaitWriteback();
    await loadSessionsState();
    setStageVisible(S.identified);
    renderAll();
    renderSessionsBar();
    toast('已新建：' + ((r.session && r.session.name) || '') + '。填好设置后点「保存并识别」。', 'good');
  } catch (e) { toast('新建失败：' + e.message, 'bad'); }
}

/* 历史分析：每份记录一行，点一行就把整页切过去。 */
function renderSessions() {
  const host = $('#sessionsHost'), note = $('#sessNote');
  if (!host) return;
  const list = arr(S.sessions);
  if (note) note.textContent = list.length ? ('共 ' + list.length + ' 份') : '';
  if (!list.length) {
    host.innerHTML = '<div class="small">还没有分析记录。</div>';
    return;
  }
  let h = '<table class="libtbl"><thead><tr>'
    + '<th>分析</th><th>创建</th><th>跑在哪</th><th>识别结论</th><th>存档</th><th>操作</th>'
    + '</tr></thead><tbody>';
  list.forEach(s => {
    const cur = s.id === S.sessionId;
    const v = String(s.verdict || '');
    const vtxt = s.has_identify
      ? '<span class="histtag" style="background:var(--done-soft);color:var(--done)">' + esc(s.verdict_cn || v || '已识别') + '</span>'
      : '<span class="histtag">还没识别</span>';
    h += '<tr' + (cur ? ' class="sessrow-cur"' : '') + '>'
      + '<td><b>' + esc(s.name || s.id) + '</b>'
      + (cur ? ' <span class="small">（当前）</span>' : '')
      + (s.note ? '<div class="note">' + esc(s.note) + '</div>' : '') + '</td>'
      + '<td class="pth">' + esc(String(s.created_at || '').replace('T', ' ')) + '</td>'
      + '<td class="pth">' + esc(s.mode === 'server' ? ('服务器 ' + (s.run_dir || '')) : (s.run_dir || '本机')) + '</td>'
      + '<td>' + vtxt + '</td>'
      + '<td>' + (s.n_archives || 0) + ' 份</td>'
      + '<td>' + (cur ? '<span class="small">正在看</span>'
                      : '<button class="btn btn-sm" data-sess-switch="'
                        + esc(s.id) + '">切到这一份</button>') + '</td>'
      + '</tr>';
  });
  h += '</tbody></table>';
  host.innerHTML = h;
}

async function switchSession(id) {
  try {
    const r = await postJSON('/api/sessions/switch', { id: id });
    if (!r || !r.ok) { toast('切不过去：' + ((r && r.error) || '未知原因'), 'bad'); return; }
    S.envCheck = null; S.staleEnv = null; S.envStale = false;
    S.identifyPhase = null; S.commitReq = null; S.autoInject = null;
    S.gates = null; S.pipeline = null; S.steps = [];
    stopWaitWriteback();
    await loadPlace();
    await loadSessionsState();
    setStageVisible(S.identified);
    renderAll();
    renderSessionsBar();
    closeDlg('dlgSessions');
    toast('已切到：' + ((r.session && r.session.name) || id), 'good');
  } catch (e) { toast('切换失败：' + e.message, 'bad'); }
}

/* ---------------- 弹窗 ---------------- */
function openDlg(id) { const el = document.getElementById(id); if (el) el.style.display = 'flex'; }
function closeDlg(id) { const el = document.getElementById(id); if (el) el.style.display = 'none'; }

/* 「再次交给助手处理」：已完成步骤的入口。
   ---------------------------------------------------------------------
   为什么不能直接复用"交给助手去跑"：
     这一步已经有结果了。用户点它不是首次执行，通常是为了
     重跑、只补某一部分、或者纠正上次跑偏的地方。
     直接甩出同一条旧指令，等于把上次的问题原样再发一遍。
   所以先把已完成的事实摆出来，再让他写清这次要什么。 */
let retryStepId = null;

function openRetryDlg(id) {
  const sg = stageOf(id);
  const step = (S.steps || []).find(x => x.id === id) || {};
  const rs = remoteStep(id);
  retryStepId = id;

  $('#retryTitle').textContent = '再次交给助手：' + (step.title || (sg && sg.title) || id);
  $('#retryNote').textContent = sg && sg.status === 'done' ? '这一步已完成' : '';

  // 已完成的事实先摊开：不看这些就决定重跑什么，等于凭印象。
  const bits = [];
  if (sg && sg.ended_at) bits.push('完成于 ' + String(sg.ended_at).replace('T', ' '));
  const dur = sg ? fmtDur(sg.started_at, sg.ended_at) : '';
  if (dur) bits.push('用了 ' + dur);
  if (sg && sg.source_host) bits.push('跑在 ' + sg.source_host);
  if (rs && rs.exec_pc) bits.push('经 ' + rs.exec_pc);
  if (sg && arr(sg.artifacts).length) bits.push('产物 ' + arr(sg.artifacts).length + ' 项');

  let dh = '';
  if (bits.length) {
    dh += '<div class="rz ok" style="margin:0"><span class="mk">✓</span><span>'
      + esc(bits.join('　·　')) + '</span></div>';
  }
  const arts = sg ? arr(sg.artifacts) : [];
  if (arts.length) {
    dh += '<div style="margin-top:8px" class="sdarts">'
      + arts.slice(0, 8).map(a => '<span class="a">' + esc(a) + '</span>').join('')
      + (arts.length > 8 ? '<span class="a">…共 ' + arts.length + ' 项</span>' : '')
      + '</div>';
  }
  if (rs && rs.log) {
    dh += '<pre class="sdlog" style="margin-top:8px">' + esc(String(rs.log).slice(0, 600)) + '</pre>';
  }
  $('#retryDone').innerHTML = dh;

  $('#retryReq').value = '';
  $('#retryHint').textContent = '';
  openDlg('dlgRetry');
  setTimeout(function () {
    const el = $('#retryReq');
    if (el) el.focus();
  }, 50);
}

document.addEventListener('click', function (e) {
  if (!e.target || e.target.id !== 'btnRetryGo') return;
  const id = retryStepId;
  if (!id) return;
  const note = ($('#retryReq') && $('#retryReq').value || '').trim();
  closeDlg('dlgRetry');
  retryStepId = null;
  // 直接把要求带进这一次执行，不再走一遍"先关框再重问"。
  runStep(id, note);
});

document.addEventListener('click', function (e) {
  const c = e.target.closest('[data-dlgclose]');
  if (c) { closeDlg(c.getAttribute('data-dlgclose')); return; }
  // 点遮罩关闭（只在点到弹窗背景本身时，避免误关）
  if (e.target.classList && e.target.classList.contains('modal')) {
    closeDlg(e.target.id);
  }
});

/* =====================================================================
   目录选择
   ===================================================================== */
async function openDirDialog() {
  closeDlg('dlgDir');
  openDlg('dlgDir');
  const start = (S.placeDraft && S.placeDraft.local_path)
    || (S.env && S.env.home) || null;
  await browseTo(start);
}
async function browseTo(dir) {
  try {
    const r = await getJSON('/api/fs' + (dir ? ('?dir=' + encodeURIComponent(dir)) : ''));
    S.browseDir = r.dir; S.browseParent = r.parent;
    $('#crumb').textContent = r.dir;
    const rows = [];
    if (r.parent) rows.push('<div class="lrow" data-go="' + esc(r.parent) + '"><span class="ic">↑</span><span>上一层</span></div>');
    arr(r.drives).forEach(d => {
      rows.push('<div class="lrow" data-go="' + esc(d) + '"><span class="ic">▣</span><span>' + esc(d) + '</span></div>');
    });
    arr(r.dirs).forEach(d => {
      rows.push('<div class="lrow" data-go="' + esc(joinPath(r.dir, d.name)) + '"><span class="ic">▸</span><span>' + esc(d.name) + '</span></div>');
    });
    arr(r.files).slice(0, 80).forEach(f => {
      rows.push('<div class="lrow file"><span class="ic">·</span><span>' + esc(f.name) + '</span><span class="sz">' + fmtSize(f.size) + '</span></div>');
    });
    const host = $('#listing');
    host.innerHTML = rows.join('') || '<div class="emptyart">空文件夹，可以直接当运行目录。</div>';
    $$('[data-go]', host).forEach(el => el.addEventListener('click', () => browseTo(el.getAttribute('data-go'))));
    $('#dlgHint').textContent = '当前：' + r.dir;
  } catch (e) {
    $('#listing').innerHTML = '<div class="emptyart">读不了这个目录：' + esc(e.message) + '</div>';
  }
}
function joinPath(dir, name) {
  const sep = dir.indexOf('\\') >= 0 ? '\\' : '/';
  return String(dir).replace(/[\\/]+$/, '') + sep + name;
}
/* 在弹窗里选中一个目录。
   注意：这里**不再**直接把它设成运行目录，只写进草稿。
   真正的落地发生在「保存选择」——那一步才会去建配置、跑识别。
   以前是选完就立刻生效，于是「选了目录」和「开始干活」两件事
   被合成了一件，用户看不出自己到哪一步了。 */
async function chooseDir(dir) {
  closeDlg('dlgDir');
  S.placeDraft = S.placeDraft || placeDraftFromServer();
  S.placeDraft.mode = 'local';
  S.placeDraft.local_path = dir;
  renderPlace();
  toast('已选中：' + dir + '。确认后点「保存选择」。', 'good');
}

/* =====================================================================
   载入
   ===================================================================== */
async function loadRun() {
  const r = await getJSON('/api/run-dir');
  if (!r.runDir) return;   // 还没确认位置：由 boot() 决定显示什么，不在这里切页面
  S.runDir = r.runDir;
  S.pipeline = r.pipeline || null;
  S.gates = r.gates || null;
  S.steps = r.steps || [];
  S.dirtyParams = {}; S.dirtySteps = {}; S.gateState = {}; S.gateOpen = {};
  arr(S.gates && S.gates.gates).forEach((g, i) => {
    if (!g.id) g.id = 'g' + i;
    const st = (g.status === 'answered') ? 'answered' : (g.status === 'auto' ? 'auto' : 'pending');
    S.gateState[g.id] = {
      status: st,
      answer: g.answer === undefined ? null : g.answer,
      answer_detail: g.answer_detail === undefined ? null : g.answer_detail,
    };
    S.gateOpen[g.id] = (st === 'pending');
  });
  const ss = r.stepState || {};
  S.running = (ss.status === 'running');
  S.step = ss.step || null;
  S.code = (ss.code === undefined ? null : ss.code);

  // 这里以前会去设 #needDir 的 display。那个节点在改版时已经删掉了，
  //   于是取到 null、赋 style 直接抛 TypeError。
  //   它又被 commitPlace 的 try/catch 接住，表现成「点了保存、弹一句失败、
  //   页面原地不动」—— 看起来像后端没成功，其实后端早就成功了。
  //   教训：删 DOM 时要一并清掉引用，不能只删 HTML。
  const mainEl = $('#main');
  if (mainEl) mainEl.style.display = 'block';
  renderAll();
  // 服务器模式必须常驻轮询。
  //   原来只有「本机在跑」才开轮询，服务器模式下本机永远不在跑，
  //   于是页面打开后只取一次状态，之后再也不刷新 ——
  //   助手在服务器上推进、回写，页面一动不动，人只能靠问才知道进展。
  if (S.running || S.needsRemote) startPolling(); else await refreshStatusOnce();
}

function renderAll() {
  renderHead();
  renderPlace();
  // 先拉脚本清单（只取数据），再渲染识别结论 ——
  //   脚本明细并进识别结果的第三段，不再单独占一个区块。
  renderStageScripts().then(() => {
    renderIdentify();
  });
  renderTaskLog();
  renderGates();
  renderRun();
  renderSettings();
  renderHistory();
  renderSidenav();
  refreshArtifacts();
  // 处理过程要单独取一次：它是「跑到哪个环节、花了多久」，
  //   和上面那些渲染各管一件事，不塞进其中任何一个里。
  loadFlow();
}
/* 闸门按段位分两组。
   A = 跑主流程之前就得定（数据在哪、注释依据、质控门槛、读入前剔样本）
   B = 一次聚类之后才定（筛群、剔样本、低置信门槛）
   为什么要分：B 那几条的依据（簇大小、样本分布、marker）只有跑到一次聚类才产生。
   混在一起显示，人会以为「现在就得答」，而这时他手上没有任何材料。 */
function gatePhase(g) { return g && g.phase ? String(g.phase) : 'A'; }
function pendingGates(phase) {
  return arr(S.gates && S.gates.gates).filter(g => {
    if ((S.gateState[g.id] || {}).status !== 'pending') return false;
    if (!phase) return true;
    return gatePhase(g) === phase;
  });
}
/* 某一步卡在哪些闸门上（页面用它决定按钮能不能点、提示什么） */
function blockingTitles(stepId) {
  const st = (S.steps || []).find(s => s.id === stepId);
  if (!st || !st.needGate) return [];
  const ph = st.gatePhase || 'A';
  return pendingGates(ph).map(g => g.title || g.id);
}
/* 顶栏状态条。
   只留「现在能不能跑」和「还差几件」这两类信息。
   以前会显示「还没设置文件」「已答复 N」「未保存的改动 N 处」——
   前两个是过程噪音（配置在保存选择时自动生成、答复进度在闸门区看得到），
   第三个暴露的是实现细节而不是用户要关心的事，都删掉了。 */
function renderHead() {
  const sub = $('#subline');
  if (sub) {
    sub.textContent = S.runDir ? ('工作路径：' + S.runDir)
                               : (S.place && S.place.ready ? '已选好位置' : '先选一台机器和一个路径');
  }
  const pA = pendingGates('A').length, pB = pendingGates('B').length;
  let h = '';
  if (!S.runDir) h += '<span class="pill plain">还没开始</span>';
  else if (pA > 0) h += '<span class="pill wait"><span class="n">' + pA + '</span> 件要先定</span>';
  else if (S.gates) h += '<span class="pill ok">可以开跑</span>';
  if (pB > 0) h += '<span class="pill plain">还有 ' + pB + ' 件等跑完上段再定</span>';
  const t = $('#tally');
  if (t) t.innerHTML = h;
}

/* ---------------- 环境体检 ---------------- */
/* =====================================================================
   需要你决定的事
   ---------------------------------------------------------------------
   用户的原话：「我现在看有非常非常多的选择需要选，但是我其实不知道这些
   东西具体对应哪些环节，然后为什么一定要在前置进行选择。」
   所以每张卡片要回答三件事，而且只用三行：
     1. 这是什么（标题）
     2. 为什么现在就得定（一句话，不是一段）
     3. 选哪个（选项，说明它到底在选什么）
   依据数据默认折叠 —— 它是支撑材料，不是主内容。
   ===================================================================== */

/* 闸门标题改成人话。
   配置里的 title/question 是给脚本看的，直接用会有「gates.json」
   「nFeature」这种词，用户的原话是「这个 json 我不理解是什么意思」。 */
const GATE_UI = {
  data_indir: {
    t: '原始数据在哪个目录',
    why: '选过位置就自动填。',
    opts: { auto: ['用第一步选好的路径', ''], keep: ['先放着，等数据拷过来再说', '暂不推进主流程'] },
  },
  reference_source: {
    t: '判细胞类型用什么依据',
    why: '注释必须有外部依据。',
    hint: '三选一。',
    opts: {
      reference_table: ['用一份文献参考表', '论文补充材料里的 marker 表'],
      lit_dict: ['用一份 marker 词典', 'JSON，可由文献检索生成'],
      skip: ['这次不做注释', '注释图与富集图会缺'],
    },
  },
  qc_threshold: {
    t: '质控下限取多少',
    why: '低于它会被丢掉。官方有 300／400／500 三个数。',
    opts: {
      '300': ['300（照官方脚本里写的）', '官方交付最低 401，实际没用这个值'],
      '400': ['400（对齐官方交付数据实测）', '与官方交付最低 401 吻合'],
      '500': ['500（官方注释里提到的）', '官方脚本里这行被注释掉，没执行'],
    },
  },
  read_exclusion: {
    t: '读入前要不要剔样本',
    why: '实验设计决定，脚本不替你判断。',
    opts: { none: ['不剔除', ''], exclude: ['要剔除', '勾选样本'] },
  },
  cluster_filter: {
    t: '一次聚类后剔哪些簇',
    why: '只对本次有效，照抄会删错。',
    hint: '下面三选一。',
    opts: {
      none: ['都不剔', '全部进入后续'],
      by_name: ['按细胞类型名保留', '填类型名，其余剔除；需先有注释'],
      by_cluster: ['按簇号保留', '填簇号；换一次运行就全变'],
    },
  },
  post_cluster_exclusion: {
    t: '聚类后要不要剔样本',
    why: '这是实验设计决定，脚本不替你判断。',
    opts: { none: ['不剔除', ''], exclude: ['要剔除', '勾选具体哪个样本'] },
  },
  lowconfidence: {
    t: '多小的簇算低置信',
    why: '细胞太少，统计不可靠。',
    opts: {
      flag_only: ['只标记，不定阈值', '标低置信，不命名'],
      threshold: ['按细胞数定一个门槛', '低于门槛的簇标为低置信'],
    },
  },
};
const gateUI = g => GATE_UI[g.id] || {};
const gateTitle = g => (gateUI(g).t || g.title || g.id);
const gateWhy = g => (gateUI(g).why || g.question || '');

/* 选项文案：优先用我们改写过的，没写的才回落到配置里的 */
function gateOptionLabel(g, o) {
  const m = (gateUI(g).opts || {})[o.value];
  return m ? m[0] : (o.label || o.value);
}
function gateOptionDesc(g, o) {
  const m = (gateUI(g).opts || {})[o.value];
  if (m) return m[1];
  return o.desc || '';
}

function findGate(id) { return arr(S.gates && S.gates.gates).filter(g => g.id === id)[0]; }

function evTable(ev, gid) {
  if (!ev) return '<div class="small">这一项没有附带依据数据。</div>';
  const cap = ev.caption ? '<div class="evcap">' + esc(ev.caption) + '</div>' : '';
  if (ev.type === 'kv') {
    const items = arr(ev.items);
    if (!items.length) return '<div class="small">暂无数据</div>';
    return '<div class="evbox">' + cap + '<dl class="kvlist">' + items.map(it =>
      '<dt>' + esc(it.k || '') + '</dt><dd>' + esc(it.v || '') + '</dd>').join('') + '</dl></div>';
  }
  const cols = arr(ev.columns), rows = arr(ev.rows);
  if (!cols.length && !rows.length) {
    return '<div class="small">暂无数据</div>';
  }
  const LIMIT = 8;
  const key = 'ev_' + gid;
  const showAll = !!S[key];
  const list = showAll ? rows : rows.slice(0, LIMIT);
  let numCol = -1;
  cols.forEach((c, i) => { if (String(c).indexOf('细胞数') >= 0 || String(c).indexOf('占比') >= 0) numCol = i; });
  const th = cols.map(c => '<th>' + esc(c) + '</th>').join('');
  const tr = list.map(r => '<tr>' + cols.map((c, i) => {
    const cell = r[i] === undefined ? '' : r[i];
    return '<td' + (numCol === i ? ' class="num"' : '') + '>' + esc(cell) + '</td>';
  }).join('') + '</tr>').join('');
  const more = rows.length > LIMIT
    ? '<div class="morewrap"><button class="btn ghost" data-evmore="' + esc(gid) + '">'
    + (showAll ? '收起来' : '展开全部 ' + rows.length + ' 行') + '</button></div>'
    : '';
  return '<div class="evbox">' + cap + '<table class="ev"><thead><tr>' + th + '</tr></thead><tbody>' + tr + '</tbody></table>' + more + '</div>';
}

function detailSpec(g, val) {
  const id = g.id;
  if (id === 'cluster_filter') {
    if (val === 'by_name') return { type: 'text', label: '要保留的细胞类型名', ph: '例如：PT-S1, PT-S2, LOH, DCT', hint: '用逗号分隔' };
    if (val === 'by_cluster') return { type: 'text', label: '要保留的簇号', ph: '例如：0,1,2,13', warn: '簇号只对本次有效，换一次就全变。' };
  }
  if ((id === 'read_exclusion' || id === 'post_cluster_exclusion') && val === 'exclude') {
    return { type: 'samples', label: '勾选要剔除的样本' };
  }
  if (val && val !== 'none' && val !== 'auto' && val !== 'keep' && g.needs_detail === true && !/^\d+$/.test(String(val))) {
    return { type: 'text', label: '具体内容', ph: '填清楚' };
  }
  return { type: 'none' };
}

function optionsHTML(g) {
  const st = S.gateState[g.id] || {};
  const opts = arr(g.options);
  if (!opts.length) return '';
  return '<div class="opts">' + opts.map(o => {
    const sel = String(st.answer) === String(o.value);
    const sug = String(g.suggested) === String(o.value);
    const risk = /by_cluster|force_native/.test(String(o.value));
    const desc = gateOptionDesc(g, o);
    return '<label class="opt' + (sel ? ' sel' : '') + '" data-gopt="' + esc(g.id) + '" data-val="' + esc(o.value) + '">'
      + '<span class="dot"></span><span class="optmain"><span class="optlab">' + esc(gateOptionLabel(g, o))
      + (sug ? '<span class="badge-sug">建议</span>' : '')
      + (risk ? '<span class="badge-sug" style="background:#fdf3ec;color:#9d3c0d">有风险</span>' : '')
      + '</span>' + (desc ? '<span class="optdesc">' + esc(desc) + '</span>' : '') + '</span></label>';
  }).join('') + '</div>';
}

function detailHTML(g) {
  const st = S.gateState[g.id] || {};
  const spec = detailSpec(g, st.answer);
  if (spec.type === 'none') return '';
  const v = st.answer_detail;
  let inner = '';
  if (spec.type === 'text') {
    const val = v ? (Array.isArray(v) ? v.join(', ') : String(v)) : '';
    inner = '<div class="blocklabel">' + esc(spec.label) + '</div>'
      + '<input type="text" data-gdetail="' + esc(g.id) + '" value="' + esc(val) + '" placeholder="' + esc(spec.ph || '') + '">'
      + (spec.hint ? '<div class="hintline">' + esc(spec.hint) + '</div>' : '')
      + (spec.warn ? '<div class="warnbox">' + esc(spec.warn) + '</div>' : '');
  } else if (spec.type === 'samples') {
    const all = arr(g.sample_options);
    let chosen = arr(v).map(String);
    if (!Array.isArray(v) && typeof v === 'string' && v) chosen = v.split(/[,，]/).map(s => s.trim());
    inner = '<div class="blocklabel">' + esc(spec.label) + '</div>';
    if (!all.length) {
      inner += '<input type="text" data-gdetail="' + esc(g.id) + '" value="' + esc(chosen.join(', ')) + '" placeholder="填样本名，逗号分隔">'
        + '<div class="hintline">聚类后自动列出样本名。</div>';
    } else {
      inner += '<div class="sampgrid">' + all.map(s => {
        const on = chosen.indexOf(String(s)) >= 0;
        return '<label class="sampchk' + (on ? ' on' : '') + '"><input type="checkbox" data-gsamp="' + esc(g.id)
          + '" value="' + esc(s) + '"' + (on ? ' checked' : '') + '>' + esc(s) + '</label>';
      }).join('') + '</div>';
    }
  }
  return '<div class="detail">' + inner + '</div>';
}

/* 一张待决定卡片 */
function gateCard(g, idx) {
  const st = S.gateState[g.id] || { status: 'pending' };
  const hint = gateUI(g).hint || '';
  const head = '<div class="ghead"><div class="gnum">' + (idx || '·') + '</div>'
    + '<div class="gtit"><h3>' + esc(gateTitle(g)) + '</h3>'
    + (gateWhy(g) ? '<div class="gwhy">' + esc(gateWhy(g)) + '</div>' : '') + '</div></div>';

  return '<div class="gcard">' + head + '<div class="gbody">'
    + (hint ? '<div class="blocklabel">' + esc(hint) + '</div>' : '')
    + optionsHTML(g) + detailHTML(g)
    + (g.note ? '<div class="notebox">' + esc(g.note) + '</div>' : '')
    + (g.evidence ? '<details class="stgfold" style="margin-top:12px"><summary>依据数据'
        + '<span class="cnt">' + (String(g.id).indexOf('cluster') >= 0 ? '簇大小 / 样本分布 / marker' : '供判断用')
        + '</span></summary><div style="padding:12px 16px">' + evTable(g.evidence, g.id) + '</div></details>' : '')
    + '</div>'
    + '<div class="gfoot"><button class="btn primary" data-gconfirm="' + esc(g.id) + '">确定</button>'
    + '<button class="btn" data-greco="' + esc(g.id) + '">用建议值</button></div></div>';
}

/* 已定下来的一条：压成一行，需要改再展开 */
function gateDone(g) {
  const st = S.gateState[g.id] || {};
  if (st.status === 'pending') return '';
  const found = arr(g.options).filter(o => String(o.value) === String(st.answer));
  let lab = found.length ? gateOptionLabel(g, found[0]) : String(st.answer === null ? '—' : st.answer);
  if (st.answer_detail) {
    const d = Array.isArray(st.answer_detail) ? st.answer_detail.join(', ') : String(st.answer_detail);
    if (d) lab += '（' + d + '）';
  }
  const auto = st.status === 'auto';
  return '<div class="gcard done"><div style="display:flex;gap:10px;align-items:center;padding:11px 18px;flex-wrap:wrap">'
    + '<span class="tag ' + (auto ? 'auto' : 'done') + '">' + (auto ? '自动' : '已定') + '</span>'
    + '<b>' + esc(gateTitle(g)) + '</b>'
    + '<span class="small" style="flex:1;min-width:0">' + esc(lab) + '</span>'
    + '<button class="btn ghost" data-gopen="' + esc(g.id) + '">改</button>'
    + '</div></div>';
}

function renderGates() {
  const host = $('#gatesHost'), note = $('#gatesNote');
  if (!S.gates) {
    // 没有可决定的事就不该占一屏。原来这里挂一句"还没生成配置"，
    //   等于把"还没内容"写成了内容 —— 直接收起整块。
    host.innerHTML = '';
    note.textContent = '';
    const sec = document.getElementById('secGates');
    if (sec) sec.style.display = 'none';
    return;
  }
  const secG = document.getElementById('secGates');
  if (secG) secG.style.display = 'block';
  const all = arr(S.gates.gates);

  // 分三组，按「什么时候该答」分，而不是按配置里的顺序。
  //   用户看不出来为什么这些要前置，分组标题替他把这件事说了。
  const A = [], B = [], done = [];
  all.forEach(g => {
    const st = (S.gateState[g.id] || {}).status;
    if (st === 'pending') (gatePhase(g) === 'B' ? B : A).push(g);
    else done.push(g);
  });

  let html = '';

  if (A.length) {
    html += '<div class="sech">跑之前要定<span class="aside">' + A.length + ' 件</span></div>';
    A.forEach((g, i) => { html += gateCard(g, i + 1); });
  }
  if (B.length) {
    html += '<div class="sech" style="margin-top:22px">跑完上段再定<span class="aside">' + B.length + ' 件</span></div>'
      + '<div class="small" style="margin-bottom:10px">依据要等上段跑完才有。</div>';
    B.forEach(g => { html += gateCard(g, 0); });
  }
  if (!A.length && !B.length) {
    html += '<div class="pl-card"><div class="rz ok"><span class="mk">✓</span>'
      + '<span>都定好了，可以跑。</span></div></div>';
  }

  if (done.length) {
    html += '<details class="stgfold" style="margin-top:22px"><summary>已经定下来的'
      + '<span class="cnt">' + done.length + ' 件</span></summary>'
      + '<div style="padding:10px 12px">' + done.map(gateDone).join('') + '</div></details>';
  }

  host.innerHTML = html;

  const bits = [];
  if (A.length) bits.push(A.length + ' 件跑之前要定');
  if (B.length) bits.push(B.length + ' 件跑完上段再定');
  note.textContent = bits.length ? bits.join('，') : '都定好了';
  renderHead();
}

/* ---------------- 运行区 ---------------- */
// 服务器模式下，某一步在服务器上到哪了。
//   数据来源只有一个：助手跑完回写的 state/remote_steps.json。
//   本机看不到服务器上的进程，所以这里不回退去猜 —— 没回写就是没回写。
function remoteStep(id) {
  return (S.remoteSteps && S.remoteSteps[id]) ? S.remoteSteps[id] : null;
}
function stepBadge(id) {
  // 退回标记优先：被作废的步骤要先显示「待重跑」，
  //   否则用户刚退回、卡片还写着「已完成」，会以为退回没生效。
  const hs = (S.history && S.history.steps) ? S.history.steps[id] : null;
  if (hs && hs.state === 'stale') return { cls: 'failed', txt: '待重跑（已退回）' };
  if (S.blocked && S.step === id) return { cls: 'failed', txt: '没跑：还有事没定' };
  if (S.running && S.step === id) return { cls: 'running', txt: '正在跑…' };
  if (S.running && S.step === 'all') return { cls: 'running', txt: '在队列里' };
  if (S.step === id && S.code === 0) return { cls: 'done', txt: '已完成' };
  if (S.step === id && S.code !== 0 && S.code !== null) return { cls: 'failed', txt: '失败（退出码 ' + S.code + '）' };
  if (S.step === 'all' && S.code === 0) return { cls: 'done', txt: '全流程已完成' };
  return { cls: '', txt: '' };
}
/* 每个步骤下面挂它自己的环节与耗时。
   ---------------------------------------------------------------------
   为什么并到卡片里：以前「处理过程」是页面下方另一块独立清单，
   同一件事有两个说法 —— 上面卡片写"已完成"，下面清单里同一阶段还挂着"进行中"，
   看的人不知道信哪个。并进来之后，一个步骤只有一处说法。
   返回 HTML 片段；阶段对不上就返回空串，不凭空造环节。 */
function stageOf(id) {
  return arr(S.flow && S.flow.stages).find(x => x.id === id) || null;
}

function renderStageInline(sg) {
  if (!sg) return '';
  let h = '';

  // 环节条：一环一格，状态与耗时都在格子里。
  const subs = arr(sg.subs);
  if (subs.length) {
    h += '<div class="stsubs">';
    subs.forEach(sb => {
      const sm = FLOW_ST_CN[sb.status] || [sb.status || '', ''];
      const sd = fmtDur(sb.started_at, sb.ended_at);
      h += '<span class="stsub ' + sm[1] + '" title="'
        + esc((sb.note || '') + (sd ? '｜' + sd : '')) + '">'
        + '<span class="mk">' + ({ ok: '✓', run: '●', bad: '×', warn: '!', '': '○' }[sm[1]] || '○') + '</span>'
        + esc(sb.title) + (sd ? ' ' + esc(sd) : '') + '</span>';
    });
    h += '</div>';
  }

  // 耗时与节点：只用工整的时间戳算，缺一个就写"计时中"。
  const dur = fmtDur(sg.started_at, sg.ended_at);
  if (dur) {
    h += '<div class="stdur">' + esc(dur)
      + (sg.ended_at ? '' : '（<span class="gap">计时中</span>）') + '</div>';
  }
  return h;
}

/* 这一步已经产出的文件：不论跑完没跑完都摆出来。
   为什么必须这样：
     产物原来只在「任务完成详情」里出现，而那块只在 done 时才渲染。
     这次的结果是「去双已经跑完、七个 rds 都在」，页面却什么都不显示 ——
     人只能以为没产出。产物是这件事的唯一凭据，不该藏在完成态后面。 */
function renderStageArts(sg) {
  const arts = sg ? arr(sg.artifacts) : [];
  if (!arts.length) return '';
  const list = arts.slice(0, 10);
  return '<div class="starts">'
    + '<span class="startsk">产物 ' + arts.length + ' 项</span>'
    + list.map(a => {
        const rel = String(a);
        const nm = rel.split('/').pop();
        return '<span class="start" title="' + esc(rel) + '">' + esc(nm) + '</span>';
      }).join('')
    + (arts.length > list.length ? '<span class="start">…</span>' : '')
    + '</div>';
}

/* 任务完成详情：跑完之后才出现，收起时只占一行。
   为什么要有它：跑完了只显示"完成"两个字，人想核对
   "到底跑的是什么、什么时候、机器是哪台、产物在哪、质检过没过"
   就只能去翻日志。这些都在手里，摊开给人看就是了。 */
function renderStageDone(sg, rs, b) {
  if (!sg) return '';
  const finished = (sg.status === 'done') || (rs && rs.status === 'done');
  if (!finished) return '';

  const dur = fmtDur(sg.started_at, sg.ended_at);
  const rv = sg.review;
  const arts = arr(sg.artifacts);
  const rows = [];

  rows.push('<div class="sdrow"><span class="k">完成时间</span><span class="v">'
    + esc(String(sg.ended_at || (rs && rs.imported_at) || '').replace('T', ' ') || '没记录')
    + (dur ? '（用了 ' + esc(dur) + '）' : '') + '</span></div>');

  if (sg.started_at) {
    rows.push('<div class="sdrow"><span class="k">开始时间</span><span class="v">'
      + esc(String(sg.started_at).replace('T', ' ')) + '</span></div>');
  }
  if (sg.source_host || (rs && rs.source_host)) {
    rows.push('<div class="sdrow"><span class="k">跑在哪</span><span class="v">'
      + esc(sg.source_host || rs.source_host)
      + ((rs && rs.exec_pc) ? ' ／ 经 ' + esc(rs.exec_pc) : '') + '</span></div>');
  }
  if (sg.skill) {
    rows.push('<div class="sdrow"><span class="k">对应脚本</span><span class="v"><code>'
      + esc(sg.skill) + '</code></span></div>');
  }
  if (rv) {
    rows.push('<div class="sdrow"><span class="k">质检</span><span class="v">'
      + (rv.ok ? '<b style="color:#166534">通过</b>' : '<b style="color:#b4342a">没过</b>')
      + (rv.imported_at ? '　' + esc(String(rv.imported_at).replace('T', ' ')) : '')
      + '</span></div>');
  }
  if (arts.length) {
    rows.push('<div class="sdrow"><span class="k">应有产物</span><span class="v"><span class="sdarts">'
      + arts.slice(0, 12).map(a => '<span class="a">' + esc(a) + '</span>').join('')
      + (arts.length > 12 ? '<span class="a">…共 ' + arts.length + ' 项</span>' : '')
      + '</span></span></div>');
  }
  const log = sg.log || (rs && rs.log) || '';
  if (log) {
    rows.push('<div class="sdrow"><span class="k">执行记录</span><span class="v">'
      + '<pre class="sdlog">' + esc(String(log).slice(0, 1200)) + '</pre></span></div>');
  }
  if (b && b.txt) {
    rows.push('<div class="sdrow"><span class="k">状态说明</span><span class="v">'
      + esc(b.txt) + '</span></div>');
  }

  return '<details class="stdone"><summary>任务完成详情</summary>'
    + '<div class="sdbody">' + rows.join('') + '</div></details>';
}

function renderRun() {
  const pA = pendingGates('A').length, pB = pendingGates('B').length;
  // 服务器模式：这一步不在本机跑，但按钮不能是死的。
  //   点了就把这步变成一条任务交给助手，状态显示"交给谁了、在等什么"。
  const remote = !!S.needsRemote;
  // 「跑分析」里只列真正在跑分析的那几步。
  //   环境体检 / 自动判定 / 容量预检 这三步不属于这里：它们是「保存并识别」
  //   那一趟顺带做完的事（用户的原话是「为什么都在跑分析这个环节里」）。
  const PRE = ['env', 'decide', 'precheck'];
  const runSteps = S.steps.filter(s => PRE.indexOf(s.id) < 0);

  $('#stepsHost').innerHTML = runSteps.map((s, i) => {
    const b = stepBadge(s.id);
    const sg = stageOf(s.id);
    const blockers = blockingTitles(s.id);
    const blockedNow = blockers.length > 0;
    const rs = remoteStep(s.id);
    const dl = (S.delegated && S.delegated.step === s.id) ? S.delegated : null;

    // 卡片底色跟着阶段真实状态走，不只跟本机运行状态走。
    //   服务器模式本机从来不会 running，只看本机状态的话，跑完的卡片也不会变色。
    let cls = b.cls;
    if (!cls && sg) {
      if (sg.status === 'done') cls = 'done';
      else if (sg.status === 'running' || sg.status === 'delegated') cls = 'running';
      else if (sg.status === 'failed') cls = 'failed';
    }

    let stateTxt = b.txt;
    if (sg) {
      const sm = FLOW_ST_CN[sg.status] || [sg.status || '', ''];
      if (!stateTxt) stateTxt = sm[0];
    }
    if (remote) {
      if (rs && rs.status === 'running') stateTxt = '助手在服务器上跑';
      else if (rs && rs.status === 'delegated') stateTxt = '已交给助手，等它动手';
      else if (rs && rs.status === 'done') stateTxt = '已在服务器上跑成'
        + (rs.imported_at ? '（' + String(rs.imported_at).replace('T', ' ') + '）' : '');
      else if (rs && rs.status === 'failed') stateTxt = '服务器上没跑通'
        + (rs.code === null || rs.code === undefined ? '' : '（退出码 ' + rs.code + '）');
      if (dl && dl.ok === false) stateTxt = '没送进会话';
      if (dl && dl.ok === true) stateTxt = '已送进会话，助手会接着做';
    }

    // 已完成过就换个说法：不是"再开一次"，而是"带着新要求再来一次"。
    //   这两件事对人是不同的：前者像首次执行，后者才说明这一步已有结果，
    //   点它是为了重跑或补做，所以要给个地方写清楚"这次要怎样"。
    const finished = (sg && sg.status === 'done') || (rs && rs.status === 'done');
    let btnLabel;
    if (blockedNow) btnLabel = '先定 ' + blockers.length + ' 件';
    else if (finished) btnLabel = '再次交给助手处理';
    else if (remote) btnLabel = '交给助手去跑';
    else btnLabel = (S.running && S.step === s.id) ? '运行中' : '开始这一步';

    const tip = blockedNow
      ? '还差：' + blockers.slice(0, 3).join('；') + (blockers.length > 3 ? ' 等' : '')
      : '';

    const btnAttr = finished
      ? ' data-req="' + esc(s.id) + '"'
      : ' data-run="' + esc(s.id) + '"';

    return '<div class="stcard ' + cls + '"' + (tip ? ' title="' + esc(tip) + '"' : '') + '>'
      + '<h4><span class="stnum">' + (STEP_ICON[s.id] || (i + 1)) + '</span>' + esc(s.title)
      + (s.needGate ? '<span class="stgate">' + (blockedNow ? '还差 ' + blockers.length + ' 件' : '已可跑') + '</span>' : '') + '</h4>'
      + '<p>' + esc(s.note || '') + '</p>'
      + '<div class="stmeta">' + esc(stateTxt) + '</div>'
      + renderStageInline(sg)
      + renderStageArts(sg)
      + '<button class="btn' + (s.id === 'pipeline1' && !finished ? ' primary' : '') + '"' + btnAttr
      + (blockedNow || (S.running && !remote && !finished) ? ' disabled' : '') + '>'
      + esc(btnLabel) + '</button>'
      + renderStageDone(sg, rs, b)
      + '</div>';
  }).join('');

  const rsHost = $('#runState');
  if (remote) {
    const dl = S.delegated;
    if (dl && dl.ok === false) { rsHost.className = 'runstate failed'; rsHost.textContent = ''; }
    else if (dl && dl.ok === true) { rsHost.className = 'runstate running'; rsHost.textContent = '已交给助手：' + dl.title; }
    else { rsHost.className = 'runstate'; rsHost.textContent = '服务器模式 · 等你的下一步'; }
  }
  else if (S.running) { rsHost.className = 'runstate running'; rsHost.textContent = '正在跑' + (S.step === 'all' ? '（全流程）' : '：' + S.step); }
  else if (S.blocked) { rsHost.className = 'runstate failed'; rsHost.textContent = '被拦住了：还有事没定'; }
  else if (S.code === 0 && S.step) { rsHost.className = 'runstate done'; rsHost.textContent = '上一步成功'; }
  else if (S.code !== null && S.code !== undefined && S.code !== 0 && S.step) { rsHost.className = 'runstate failed'; rsHost.textContent = '上一步没跑通（退出码 ' + S.code + '）'; }
  else { rsHost.className = 'runstate'; rsHost.textContent = S.runDir ? '空闲' : '还没定运行目录'; }

  // 没有运行目录就不算就绪：连跑到哪、结果写哪都还没定。
  const ready = !!S.runDir;
  $('#btnRunAll').disabled = (S.running && !remote) || pA > 0 || pB > 0 || !ready;
  $('#btnStop').style.display = (S.running && !remote) ? 'inline-block' : 'none';

  $('#runHint').textContent = !ready ? '未设运行目录'
    : (remote ? ''
      : (pA > 0 ? ('待定 ' + pA) : (pB > 0 ? ('待定 ' + pB) : '')));
  renderDelegated();
  renderRunCtl();
  renderFlow();
}

/* =====================================================================
   处理过程：环节清单 + 进度 + 耗时
   ---------------------------------------------------------------------
   为什么要这一块：
     以前页面上只有「这一步跑没跑成」一格状态。用户看不到中途跑到哪、
     每个环节花了多久 —— 而"卡在合并那一步已经四十分钟"和"什么都没开始"
     在旧界面上长得一样。
   数据来源：
     阶段与环节清单来自服务端 /api/flow（它读 engine/flow_spec.R，唯一出处），
     进度来自执行端的环节级回写。
   显示原则：
     只显示真的发生过的：环节没回写就写「未回写」，不替它画成完成。
     耗时只用工整的两个时间戳算；只有一个时间戳就写「计时中」。
   ===================================================================== */
const FLOW_ST_CN = {
  todo: ['待跑', ''], running: ['进行中', 'run'], done: ['完成', 'ok'],
  failed: ['失败', 'bad'], unknown: ['未回写', 'warn'], delegated: ['已交出', 'run'],
  stopped: ['已停', 'warn'],
};
const REVIEW_CN = {
  yes: ['在服务器上跑', 'ok'], no: ['不在目标服务器上', 'bad'], unknown: ['没有机器信息', 'warn'],
};

function fmtDur(a, b) {
  if (!a) return '';
  const t1 = Date.parse(String(a).replace(' ', 'T'));
  const t2 = b ? Date.parse(String(b).replace(' ', 'T')) : Date.now();
  if (isNaN(t1) || isNaN(t2)) return '';
  let ms = t2 - t1;
  if (ms < 0) ms = 0;
  const sec = Math.round(ms / 1000);
  if (sec < 60) return sec + ' 秒';
  const min = Math.floor(sec / 60);
  if (min < 60) return min + ' 分 ' + (sec % 60) + ' 秒';
  const hr = Math.floor(min / 60);
  return hr + ' 时 ' + (min % 60) + ' 分';
}
function fmtClock(x) {
  if (!x) return '';
  const d = String(x).replace('T', ' ');
  return d.length >= 16 ? d.slice(11, 16) : d;
}

/* 流程层面的提醒：只留"每个步骤自己说不出来"的那几条。
   ---------------------------------------------------------------------
   为什么只留这几条：
     环节与耗时已经并进每个步骤卡片了（见 renderStageInline）。
     同一份清单再在页面下方完整画一遍，就会出现"上面说跑完、下面说在跑"
     这种自相矛盾的画面 —— 那正是这次要消掉的东西。
     这里只保留三类卡片放不下的信息：
       · 停滞提示（哪一步多久没动静了）
       · 这套脚本压根没有的阶段
       · 每一步都要过的那三关（全局规则）
   */
function renderFlow() {
  const host = $('#flowHost'); if (!host) return;
  const f = S.flow;
  if (!f || !arr(f.stages).length) { host.innerHTML = ''; return; }

  const st = f.selfcheck || {};
  let h = '<div class="flowwrap">';

  // 自检不过要先说：流程定义和引擎对不上时，后面所有进度都不可信。
  if (st && st.ok === false) {
    h += '<div class="rz fail" style="margin:8px 0"><span class="mk">×</span><span><b>流程定义自检没过</b>'
      + ' <span class="dt">' + esc(arr(st.problems).slice(0, 3).join('；')) + '</span></span></div>';
  }

  // 停滞提示：回答「助手说它在跑、然后就没消息了」。
  const stall = flowStall(f);
  if (stall) {
    h += '<div class="rz warn flowstall"><span class="mk">!</span><span>'
      + '<b>已经 ' + esc(stall.dur) + ' 没有回写了</b>'
      + ' <span class="dt">「' + esc(stall.title) + '」最后一次动静是 '
      + esc(fmtClock(stall.at)) + '。执行端不会自己回来报信，'
      + '可让它报一次，或自己看服务器。</span></span>'
      + '<div class="btnrow" style="margin-top:6px">'
      + '<button class="btn" data-flowpoke="' + esc(stall.stage) + '">让它现在报一次</button>'
      + '<button class="btn" data-flowcheck="1">查一次结果</button></div></div>';
  }

  // 流程里本来就没有的阶段，如实说"没有"，不摆空按钮。
  const miss = arr(f.missing);
  if (miss.length) {
    h += '<div class="small" style="margin-top:8px">这套脚本没有：'
      + esc(miss.join('、')) + '（服务器上没对应脚本）</div>';
  }

  const gates = arr(f.gates);
  if (gates.length) {
    h += '<div class="flowgates"><span class="small">每一步都要过这三关：</span>'
      + gates.map(g => '<span class="fgtag" title="' + esc(g.how || '') + '">'
        + esc(g.title) + '</span>').join('') + '</div>';
  }
  h += '</div>';

  // 全空就不占地方：没有提醒时这一块应该是空的，而不是一个空壳卡片。
  host.innerHTML = h === '<div class="flowwrap"></div>' ? '' : h;
}

/* 判断是否停滞：只看已经发生的事实。
   判据：某一阶段状态是 running，且它自身与它所有环节里最新的时间戳
        距现在超过阈值。阈值给 6 分钟 —— 短于它，正常跑一步也会被误报。
   没有 running 的阶段就不算停滞：那不是"卡住"，是"还没开始"。 */
const FLOW_STALL_MIN = 6;
function flowStall(f) {
  let best = null;
  arr(f && f.stages).forEach(sg => {
    if (sg.status !== 'running' && sg.status !== 'delegated') return;
    const cands = [sg.started_at, sg.imported_at || ''];
    arr(sg.subs).forEach(sb => { cands.push(sb.started_at, sb.ended_at); });
    let latest = 0;
    cands.forEach(x => {
      if (!x) return;
      const t = Date.parse(String(x).replace(' ', 'T'));
      if (!isNaN(t) && t > latest) latest = t;
    });
    if (!latest) return;
    const mins = (Date.now() - latest) / 60000;
    if (mins < FLOW_STALL_MIN) return;
    if (!best || mins > best.mins) {
      const mm = Math.floor(mins);
      best = {
        stage: sg.id, title: sg.title, at: new Date(latest).toISOString(), mins,
        dur: mm < 60 ? (mm + ' 分钟') : (Math.floor(mm / 60) + ' 小时 ' + (mm % 60) + ' 分钟'),
      };
    }
  });
  return best;
}

/* 「让它现在报一次」：把一条短指令送进会话，要求它立刻回写当前进度。
   为什么要这一条：执行端跑起来之后不会自己回来报信，「我等它通知我」
   在真实链路里不成立。人总得有个办法去问一声 —— 这就是那个办法。
   送出去的仍然是一条短指令（带 /api/brief 地址），不占通道长度。 */
async function pokeFlow(stageId) {
  const brief = '/api/brief?step=' + encodeURIComponent(stageId)
    + (S.sessionId ? '&session=' + encodeURIComponent(S.sessionId) : '')
    + (S.commitReq ? '&req=' + encodeURIComponent(S.commitReq) : '');
  const txt = '【来自 个性化分析助手】\n'
    + '要办的事：「' + (stepTitleCN(stageId) || stageId) + '」现在跑到哪了，请立刻回写一次。\n'
    + '1. 到服务器上看这一阶段的实际进程与最后写出的文件，不要凭记忆回答。\n'
    + '2. 回写进度：POST /api/step/import，body 带 step=' + stageId
    + '、sub（当前环节）、status（running/done/failed）、started_at、log。\n'
    + '3. 如果已经停了或失败了，把原因原样写回，不要重跑、不要换参数。\n'
    + '完整流程说明在 ' + brief + ' （GET 取 text 字段）。';
  const r = await injectToSession(txt, function () {});
  if (r && r.ok) toast('已让它报一次，回写后这一栏会自己变。', 'good');
  else toast('没送进会话：' + String((r && (r.why || r.code)) || '通道没连上').replace(/[。．.]+$/, ''), 'bad');
}

function stepTitleCN(id) {
  const hit = arr(S.flow && S.flow.stages).find(x => x.id === id);
  return hit ? hit.title : '';
}

async function loadFlow() {
  try { S.flow = await getJSON('/api/flow'); } catch (e) { S.flow = null; }
  renderFlow();
}

// 服务器模式下"这一步交给谁了"要说清楚，而不是只弹一条会消失的提示。
//   失败时把指令原文摆出来，人可以自己复制发一次 —— 功能不押在一条握手上。
/* 常驻推进条：不藏在弹窗、不等停滞才出现。
   为什么要有：
     原来「让它报一次」「查一次结果」只在检测到停滞时才画出来，
     没停滞就一个入口都没有 —— 而"我想知道现在到底怎么样"恰恰是最常见的需求。
     人只能去问助手，那就等于应用没起作用。
   这里固定给三个动作，任何时刻都在：
     查一次结果 / 让它报一次 / 复制进度询问原文 */
function renderRunCtl() {
  const host = $('#runCtl'); if (!host) return;
  if (!S.needsRemote) { host.innerHTML = ''; return; }

  // 当前跑到哪一步、最后动过是什么时候
  const f = S.flow;
  const stages = arr(f && f.stages);
  let cur = null;
  stages.forEach(sg => {
    if (sg.status === 'running') cur = sg;
  });
  if (!cur) {
    // 没有 running：找最后完成的那一步，说清下一步是什么
    let lastDone = null;
    stages.forEach(sg => {
      if (sg.status === 'done') lastDone = sg;
    });
    if (lastDone) cur = lastDone;
  }
  const nextStage = (function () {
    let hitNext = false;
    for (let i = 0; i < stages.length; i += 1) {
      if (hitNext && stages[i].status !== 'done') return stages[i];
      if (cur && stages[i].id === cur.id) hitNext = true;
    }
    return null;
  })();

  const artCount = arr(S.artifacts).length;
  const text = cur
    ? ((cur.status === 'running' ? '在跑：' : '最近完成：') + cur.title
       + (nextStage && cur.status !== 'running' ? '　下一步：' + nextStage.title : ''))
    : '还没开始';

  host.innerHTML = '<div class="runctl">'
    + '<span class="rctext">' + esc(text)
    + (artCount ? '　产物 ' + artCount + ' 项' : '') + '</span>'
    + '<span class="rcsp"></span>'
    + '<button class="btn btn-sm" data-runcheck="1">查一次结果</button>'
    + '<button class="btn btn-sm" data-runpoke="' + esc(cur ? cur.id : '') + '">让它报一次</button>'
    + '<span class="small" id="runCtlMsg"></span>'
    + '</div>';
}

function renderDelegated() {
  const host = $('#delegatedHost'); if (!host) return;
  const dl = S.delegated;
  if (!dl) { host.innerHTML = ''; return; }
  const cls = dl.ok === true ? 'ok' : (dl.ok === false ? 'fail' : 'warn');
  const mk = dl.ok === true ? '✓' : (dl.ok === false ? '×' : '·');
  host.innerHTML = '<div class="rz ' + cls + '"><span class="mk">' + mk + '</span><span><b>'
    + esc(dl.title) + '</b> <span class="dt">'
    + esc(dl.ok === true
      ? '已送进会话，它会接着做。'
      : (dl.ok === false
        ? ('没送进：' + String(dl.why || '通道没连上').replace(/[。．.]+$/, '')
           + '。原文在下面，可自己发一次。')
        : '正在送…'))
    + '</span></span></div>'
    + (dl.ok === false && dl.text
      ? '<details style="margin-top:6px"><summary class="small">指令原文（可复制）</summary>'
        + '<pre class="log" style="max-height:220px">' + esc(dl.text) + '</pre></details>'
      : '')
    // 全文一直在应用里可取：通道短不等于指令短。
    //   人想核对"到底交给它什么"，点开就是那一条完整指令。
    + (dl.text
      ? '<details style="margin-top:6px"><summary class="small">它拿到的完整指令（' 
        + esc(String(dl.text).length) + ' 字）</summary>'
        + '<pre class="log" style="max-height:260px">' + esc(dl.text) + '</pre></details>'
      : '');
}
async function initConfig() {
  try {
    const r = await postJSON('/api/config', { init: true });
    toast('已生成：' + arr(r.written).join('、'), 'good');
    await loadRun();
  } catch (e) { toast('生成失败：' + e.message, 'bad'); }
}
// 把已经定下的事拼成一句话交给助手。
//   为什么必须带上：这些决定（筛群门槛、剔哪个样本、注释依据）是人在应用里
//   一个个点出来的，助手在服务器上看不到页面。不带过去，它只能自己猜一个值 ——
//   那正是"同一份数据两种跑法"的来源。
function decidedSummary() {
  const gs = arr(S.gates && S.gates.gates);
  const out = [];
  gs.forEach(g => {
    const st = S.gateState[g.id] || {};
    if (st.status !== 'answered' && st.status !== 'auto') return;
    let v = st.answer;
    if (v === null || v === undefined || v === '') return;
    if (Array.isArray(v)) v = v.join('、');
    let d = st.answer_detail;
    if (Array.isArray(d)) d = d.join('、');
    out.push((g.title || g.id) + ' = ' + v + (d && String(d).trim() ? '（' + d + '）' : '')
      + (st.status === 'auto' ? '［系统建议值］' : ''));
  });
  return out.join('；');
}

async function runStep(id, reqNote) {
  if (S.running && !S.needsRemote) return;
  S.running = true; S.step = id; S.code = null; S.lastLogLen = 0;
  renderRun();
  // 注意：这里不先 startPolling()。
  //   以前是先轮询再发请求，第一次轮询跑在请求之前，读到的还是"没开始"的
  //   旧状态 —— wasRunning=true、running=false，于是立刻弹一句"没跑通"。
  //   服务器模式下更明显：那一步本来就是"交出去、等回写"，页面不该自己判失败。
  try {
    const body = { step: id, gate_note: decidedSummary() };
    // 再次交给助手时用户写的具体要求，单独一个字段往后传。
    //   不和 gate_note 混：一个是流程闸门的答案，一个是这次追加的要求，
    //   混在一起事后分不清哪句是当初定的。
    if (reqNote && String(reqNote).trim()) body.req_note = String(reqNote).trim();
    const r = await postJSON('/api/run', body);
    // 服务器模式：这一步不由本机执行，服务端把它变成一条任务交回来。
    //   这里必须真的发出去 —— 只显示"请到助手那边跑"等于按钮是死的，
    //   用户点了只会看到一条报错。发出去之后状态才从"空闲"变成"已交给助手"。
    if (r && r.delegated) {
      S.delegated = { step: id, title: r.title || id, at: Date.now(), ok: null, why: '',
                      text: r.instruction || '' };
      renderRun();
      // 进会话的只用短的那一段：长指令会超过通道载荷上限，被平台判无效。
      //   短的那段带着 /api/brief 的地址，助手按地址取全文，一条不落地照做。
      const sendTxt = r.session_text || r.instruction || '';
      const res = await injectToSession(sendTxt, function () {});
      S.delegated.ok = !!res.ok;
      S.delegated.why = res.ok ? '' : (res.why || res.code || '');
      if (res.ok) {
        toast('已交给助手：' + S.delegated.title + '。跑完会写回。', 'good');
      } else {
        toast('指令没能送进会话：' + String(res.why || res.code || '通道没连上').replace(/[。．.]+$/, '')
          + (res.queued ? '。已入队，每小时自动重试，回写后停止。' : '。原文在下面，可自己发。'), 'bad');
      }
      S.running = false;
      renderRun();
      // 交接出去之后继续盯着：助手回写的那一刻，这一栏要自己变。
      //   这里只是"等结果"，不是"在跑" —— 所以 pollStatus 对服务器模式走另一条判据。
      startPolling();
      return;
    }
    toast('已开始：' + id, 'good');
    if (r && r.steps) S.queue = r.steps;
    startPolling();
  } catch (e) {
    S.running = false;
    // 被质检闸门拦下不是"跑失败"：那一步根本没开始跑。
    //   分开来说，人才能判断该补质检还是该修脚本。
    if (e && e.status === 409 && e.body && e.body.blocked_review) {
      S.delegated = null;
      renderRun();
      toast(e.body.error, 'bad');
      loadFlow();
      return;
    }
    S.code = -1;
    renderRun();
    toast(e.message, 'bad');
    stopPolling();
  }
}
async function stopRun() {
  try {
    const r = await postJSON('/api/stop', {});
    toast(r.msg || '已发出停止指令', '');
  } catch (e) { toast('停不下来：' + e.message, 'bad'); }
}

/* ---------------- 轮询状态 ---------------- */
function startPolling() {
  if (S.pollTimer) return;
  pollStatus();
  // 服务器模式的进度来自回写，变化不快，5 秒一次够用；
  //   本机在跑时仍是 2 秒，看日志要跟手。
  const every = S.needsRemote ? 5000 : 2000;
  S.pollTimer = setInterval(pollStatus, every);
}
function stopPolling() {
  if (S.pollTimer) { clearInterval(S.pollTimer); S.pollTimer = null; }
}
async function pollStatus() {
  try {
    const st = await getJSON('/api/status');
    // 服务器模式：本机看不到服务器上的进程，"在跑/没跑通"都不能由本机判。
    //   判据只有一条 —— 助手有没有把这一步的结果写回来。
    //   以前这里照搬本机那套（状态不再是 running 就当作"跑完了/没跑通"），
    //   于是刚点完按钮就弹一句"没跑通"，而那一步其实只是交出去了、还没人动手。
    if (S.needsRemote) {
      const m = {};
      arr(st.steps).forEach(s => { if (s && s.id) m[s.id] = s; });
      S.remoteSteps = m;
      S.running = (st.status === 'running');
      S.blocked = false;
      S.step = st.step || null;
      if (st.log) {
        $('#logBox').textContent = st.log;
        $('#logHint').textContent = st.log.split('\n').length + ' 行';
      }
      renderRun();
      loadFlow();
      return;
    }
    const running = st.status === 'running';
    const wasRunning = S.running;
    S.running = running;
    S.blocked = (st.status === 'blocked');
    S.step = st.step || null;
    S.code = (st.code === undefined ? null : st.code);
    if (st.log) {
      const box = $('#logBox');
      const atBottom = box.scrollTop + box.clientHeight >= box.scrollHeight - 30;
      box.textContent = st.log;
      if (atBottom) box.scrollTop = box.scrollHeight;
      $('#logHint').textContent = st.log.split('\n').length + ' 行';
    }
    renderRun();
    if (wasRunning && !running) {
      stopPolling();
      if (S.code === 0) toast('跑完了。', 'good');
      else if (S.blocked) toast('停下了：还有事没定，看日志。', 'bad');
      else toast('没跑通，看下面的日志。', 'bad');
      await loadRun();
    }
  } catch (e) {
    stopPolling();
    S.running = false; renderRun();
  }
}
async function refreshStatusOnce() {
  try {
    const st = await getJSON('/api/status');
    if (st.log) {
      $('#logBox').textContent = st.log;
      $('#logHint').textContent = st.log.split('\n').length + ' 行';
    }
    // 服务器模式：每步到哪了只能靠助手回写，所以顺手把这份状态收下来。
    if (Array.isArray(st.steps)) {
      const m = {};
      st.steps.forEach(s => { if (s && s.id) m[s.id] = s; });
      S.remoteSteps = m;
      renderRun();
    }
  } catch { /* 忽略 */ }
}

/* ---------------- 设置表 ---------------- */
function renderSettings() {
  const host = $('#settingsHost'), note = $('#settingsNote');
  if (!S.pipeline) {
    host.innerHTML = '<div style="color:#7c857f">这个运行目录里还没有 pipeline.json。'
      + '<div class="btnrow" style="margin-top:12px"><button class="btn primary" id="btnInit2">生成一份默认设置</button></div></div>';
    note.textContent = '';
    const b = $('#btnInit2'); if (b) b.addEventListener('click', initConfig);
    return;
  }
  const groups = [['params', '分析参数'], ['io', '文件位置'], ['environment', '本机环境']];
  let total = 0, gate = 0, html = '';
  groups.forEach(([sec, title]) => {
    const obj = S.pipeline[sec] || {};
    const keys = Object.keys(obj);
    if (!keys.length) return;
    const rows = keys.map(k => {
      const node = obj[k];
      const val = (node && typeof node === 'object' && 'value' in node) ? node.value : node;
      const src = (node && typeof node === 'object' && node.source) ? node.source : 'unspecified';
      const locked = !!(node && typeof node === 'object' && node.locked);
      const why = (node && typeof node === 'object' && node.why) ? node.why : '';
      total++; if (src === 'gate') gate++;
      const L = plabel(k);
      const changed = !!S.dirtyParams[sec + '.' + k];
      const vstr = (val === null || val === undefined) ? '' : (Array.isArray(val) ? val.join(', ') : String(val));
      return '<tr class="' + (changed ? 'changed' : '') + '">'
        + '<td class="pname">' + esc(L[0]) + (L[1] ? '<small>' + esc(L[1]) + '</small>' : '')
        + (why ? '<small style="color:#a1620a">' + esc(why) + '</small>' : '') + '</td>'
        + '<td class="pval"><input type="text" data-pinput="' + esc(sec + '.' + k) + '" value="' + esc(vstr)
        + '"' + (locked ? ' disabled' : '') + ' placeholder="待填"></td>'
        + '<td>' + srcTag(src) + '</td>'
        + '<td><button class="lockbtn' + (locked ? ' on' : '') + '" data-plock="' + esc(sec + '.' + k) + '">'
        + (locked ? '已锁定' : '可改') + '</button></td>'
        + '<td>' + (src === 'gate' ? '<button class="goto" data-goto="' + esc(k) + '">去答复</button>' : '') + '</td>'
        + '</tr>';
    }).join('');
    html += '<div class="blocklabel" style="margin-top:' + (html ? '20px' : '0') + '">' + esc(title) + '</div>'
      + '<table class="pt"><thead><tr><th>项目</th><th>当前值</th><th>谁定的</th><th>能不能改</th><th></th></tr></thead><tbody>'
      + rows + '</tbody></table>';
  });
  host.innerHTML = html || '<div style="color:#7c857f">没有可显示的设置。</div>';
  note.textContent = total + ' 项' + (gate ? ('，其中 ' + gate + ' 项等你定') : '');
}

/* ---------------- 保存 ---------------- */
function buildPipelineOut() {
  const p = JSON.parse(JSON.stringify(S.pipeline || {}));
  Object.keys(S.dirtyParams).forEach(key => {
    const parts = key.split('.'), sec = parts[0], k = parts.slice(1).join('.');
    if (!p[sec]) p[sec] = {};
    if (!p[sec][k] || typeof p[sec][k] !== 'object') p[sec][k] = { value: null };
    p[sec][k].source = 'user';
  });
  if (!p.case) p.case = {};
  p.case.updated_at = new Date().toISOString().replace(/\.\d{3}Z$/, 'Z');
  return p;
}
function buildGatesOut() {
  const g = JSON.parse(JSON.stringify(S.gates || { gates: [] }));
  g.updated_at = new Date().toISOString().replace(/\.\d{3}Z$/, 'Z');
  g.gates = arr(g.gates).map(one => {
    const st = S.gateState[one.id] || {};
    if (st.status === 'answered' && st.answer !== null && st.answer !== undefined) {
      one.status = 'answered'; one.answer = st.answer; one.answer_detail = st.answer_detail;
      one.answered_at = one.answered_at || new Date().toISOString().replace(/\.\d{3}Z$/, 'Z');
      one.answered_by = 'user';
    } else if (st.status === 'auto') {
      one.status = 'auto';
      if (one.answer === undefined || one.answer === null) one.answer = st.answer;
    } else { one.status = 'pending'; }
    return one;
  });
  return g;
}
async function saveAll() {
  if (!S.runDir) { toast('先选运行目录。', 'bad'); return; }
  const payload = {};
  if (S.pipeline) payload['pipeline.json'] = buildPipelineOut();
  if (S.gates) payload['gates.json'] = buildGatesOut();
  if (!Object.keys(payload).length) { toast('还没有可保存的内容。', 'bad'); return; }
  try {
    const r = await postJSON('/api/config', payload);
    // 服务器模式：配置要落在服务器的结果目录里，本机写不了。
    //   服务端把这件活变成一条指令交回来，这里必须真的发出去 ——
    //   不然「保存我的设置」在服务器模式下就是一句空话。
    if (r && r.delegated) {
      S.delegated = { step: 'config', title: '生成配置文件', at: Date.now(), ok: null, why: '', text: r.instruction || '' };
      renderRun();
      const res = await injectToSession(r.instruction || '', function () {});
      S.delegated.ok = !!res.ok;
      S.delegated.why = res.ok ? '' : (res.why || res.code || '');
      renderRun();
      if (res.ok) toast('已交给助手。', 'good');
      else toast('设置没能送进会话：' + (res.why || res.code || '通道没连上')
        + (res.queued ? '。已入队，每小时自动重试。' : '。原文已摆在下面。'), 'bad');
      return;
    }
    S.dirtyParams = {};
    if (payload['gates.json']) {
      S.gates = payload['gates.json'];
      arr(S.gates.gates).forEach(g => {
        const st = (g.status === 'answered') ? 'answered' : (g.status === 'auto' ? 'auto' : 'pending');
        S.gateState[g.id] = {
          status: st,
          answer: g.answer === undefined ? null : g.answer,
          answer_detail: g.answer_detail === undefined ? null : g.answer_detail,
        };
      });
    }
    renderAll();
    toast('已保存。', 'good');
  } catch (e) { toast('保存失败：' + e.message, 'bad'); }
}

/* ---------------- 产物 ---------------- */
async function refreshArtifacts() {
  if (!S.runDir) return;
  try {
    const r = await getJSON('/api/artifacts');
    S.artifacts = r.items || [];
    const note = $('#artNote');
    const host = $('#artHost');
    // 服务器模式：清单只能来自执行端的回写，应用自己 ls 不到服务器磁盘。
    //   所以"空"有两种完全不同的意思，必须分开说 ——
    //   混成一句「还没有产物」，人就会以为真的白跑了。
    const remote = !!r.remote;
    const listed = r.listed !== false;

    if (note) {
      note.textContent = (r.outdir ? ('来自 ' + r.outdir) : '')
        + (remote && r.listed_at ? ('　列出时间 ' + String(r.listed_at).replace('T', ' ')) : '');
    }

    if (!S.artifacts.length) {
      if (remote && !listed) {
        host.innerHTML = '<div class="emptyart">还没让执行端列过产物清单。'
          + '服务器上的文件在应用这台机器上看不到，需要执行端列一次并回写。</div>';
      } else if (remote) {
        host.innerHTML = '<div class="emptyart">执行端列过了，这个目录下确实没有可下载的产物。</div>';
      } else {
        host.innerHTML = '<div class="emptyart">还没有产物。</div>';
      }
      return;
    }

    // 按目录分组：几十个文件平铺成一列，找一张图要滚很久。
    const groups = {};
    S.artifacts.forEach(a => {
      const dir = String(a.rel || '').indexOf('/') >= 0 ? String(a.rel).split('/')[0] : '（根目录）';
      (groups[dir] = groups[dir] || []).push(a);
    });
    const keys = Object.keys(groups).sort();
    let h = '<div class="artsum">' + S.artifacts.length + ' 个产物，' + keys.length + ' 个目录'
      + (remote ? '　<b>在服务器上，不在本机</b>' : '') + '</div>';
    h += '<div class="artlist">';
    keys.forEach(dir => {
      const list = groups[dir];
      h += '<details class="artdir"><summary>'
        + '<span class="artdirn">' + esc(dir) + '</span>'
        + '<span class="cnt">' + list.length + ' 个</span></summary>';
      h += list.map(a =>
        '<div class="artrow"><span class="artkind ' + esc(a.kind) + '">' + esc(a.kind) + '</span>'
        + '<span class="artname" title="' + esc(a.rel) + '">' + esc(String(a.rel).split('/').pop()) + '</span>'
        + '<span class="artsize">' + fmtSize(a.size) + '</span>'
        // 服务器上的文件下载不了：应用到不了内网。这一点要写出来，
        //   不摆一个点了就 404 的按钮。
        + (remote
            ? '<span class="artsize" title="文件在服务器上，应用取不到">需从服务器取</span>'
            : '<a class="btn" href="' + apiUrl('api/download') + '?rel=' + encodeURIComponent(a.rel) + '">下载</a>')
        + '</div>').join('');
      h += '</details>';
    });
    h += '</div>';
    host.innerHTML = h;
  } catch (e) {
    $('#artHost').innerHTML = '<div class="emptyart">读产物列表失败：' + esc(e.message) + '</div>';
  }
}

/* =====================================================================
   事件委托
   ===================================================================== */
document.addEventListener('click', e => {
  const t = e.target;

  const opt = t.closest('[data-gopt]');
  if (opt) {
    const gid = opt.getAttribute('data-gopt'), val = opt.getAttribute('data-val');
    const st = S.gateState[gid];
    if (st) {
      const old = st.answer;
      st.answer = val;
      if (String(old) !== String(val)) {
        const sp = detailSpec(findGate(gid), val);
        if (sp.type === 'none') st.answer_detail = null;
        if (sp.type === 'samples') st.answer_detail = [];
      }
    }
    renderGates();
    return;
  }
  const evb = t.closest('[data-evmore]');
  if (evb) { const k = 'ev_' + evb.getAttribute('data-evmore'); S[k] = !S[k]; renderGates(); return; }

  const go = t.closest('[data-gopen]');
  if (go) { const id = go.getAttribute('data-gopen'); S.gateState[id].status = 'pending'; S.gateOpen[id] = true; renderGates(); return; }

  const rc = t.closest('[data-greco]');
  if (rc) {
    const id = rc.getAttribute('data-greco'); const g = findGate(id);
    if (g && g.suggested !== undefined && g.suggested !== null) {
      const st = S.gateState[id];
      st.answer = g.suggested;
      const sp = detailSpec(g, g.suggested);
      if (sp.type === 'text') st.answer_detail = null;
      if (sp.type === 'samples') st.answer_detail = [];
      renderGates();
    }
    return;
  }
  const cf = t.closest('[data-gconfirm]');
  if (cf) {
    const id = cf.getAttribute('data-gconfirm'); const g = findGate(id); const st = S.gateState[id];
    if (!st.answer && st.answer !== 0) { toast('先选一项。', 'bad'); return; }
    const sp = detailSpec(g, st.answer);
    if (sp.type === 'text') {
      const v = st.answer_detail;
      const vs = Array.isArray(v) ? v.join(', ') : (v === null || v === undefined ? '' : String(v));
      if (!vs.trim()) { toast('还要填内容。', 'bad'); return; }
      const parts = vs.split(/[,，]/).map(x => x.trim()).filter(Boolean);
      st.answer_detail = parts.length === 1 ? parts[0] : parts;
    }
    if (sp.type === 'samples' && !arr(st.answer_detail).length) { toast('请至少勾选一个样本。', 'bad'); return; }
    st.status = 'answered';
    S.gateOpen[id] = false;
    syncGateToSettings(g, st);
    renderGates(); renderRun(); renderSettings();
    toast('记下了，记得保存。', 'good');
    return;
  }
  const lk = t.closest('[data-plock]');
  if (lk) {
    const key = lk.getAttribute('data-plock'); const parts = key.split('.');
    const sec = parts[0], k = parts.slice(1).join('.');
    const node = S.pipeline[sec][k];
    if (node && typeof node === 'object') node.locked = !node.locked;
    renderSettings();
    return;
  }
  const gt = t.closest('[data-goto]');
  if (gt) {
    const want = gt.getAttribute('data-goto');
    const hit = arr(S.gates && S.gates.gates).filter(g =>
      g.id === want || (g.id || '').indexOf(want) >= 0 || (want || '').indexOf(g.id) >= 0)[0];
    if (hit) {
      S.gateOpen[hit.id] = true; renderGates();
      const el = document.querySelector('[data-gconfirm="' + hit.id + '"]');
      if (el) el.scrollIntoView({ behavior: 'smooth', block: 'center' });
      toast('已经跳到那一项了。', '');
    } else { toast('跑过一次聚类才出现。', 'bad'); }
    return;
  }
  const rn = t.closest('[data-run]');
  if (rn) { runStep(rn.getAttribute('data-run')); return; }
  // 已完成过的步骤：不是"再开一次"，而是"带着新要求再来一次"。
  //   所以先弹一个框问清这次要什么，而不是直接甩出同一条旧指令。
  const rq = t.closest('[data-req]');
  if (rq) { openRetryDlg(rq.getAttribute('data-req')); return; }
});

document.addEventListener('input', e => {
  const t = e.target;
  if (t.matches('[data-pinput]')) {
    const key = t.getAttribute('data-pinput');
    const parts = key.split('.'), sec = parts[0], k = parts.slice(1).join('.');
    const raw = t.value;
    let parsed;
    if (raw === '') parsed = null;
    else if (/^-?\d+(\.\d+)?$/.test(raw.trim())) parsed = Number(raw.trim());
    else if (raw.indexOf(',') >= 0) parsed = raw.split(/[,，]/).map(x => x.trim()).filter(Boolean);
    else parsed = raw;
    if (!S.pipeline[sec]) S.pipeline[sec] = {};
    const node = S.pipeline[sec][k];
    if (node && typeof node === 'object') { node.value = parsed; node.source = 'user'; }
    else S.pipeline[sec][k] = { value: parsed, source: 'user' };
    S.dirtyParams[key] = true;
    const tr = t.closest('tr'); if (tr) tr.classList.add('changed');
    renderHead();
    return;
  }
  if (t.matches('[data-gdetail]')) {
    S.gateState[t.getAttribute('data-gdetail')].answer_detail = t.value;
  }
});

document.addEventListener('change', e => {
  const t = e.target;
  if (t.matches('[data-gsamp]')) {
    const gid = t.getAttribute('data-gsamp'), v = t.value;
    let cur = arr(S.gateState[gid].answer_detail).map(String);
    if (t.checked) { if (cur.indexOf(v) < 0) cur.push(v); } else { cur = cur.filter(x => x !== v); }
    S.gateState[gid].answer_detail = cur;
    const lab = t.closest('.sampchk'); if (lab) lab.className = 'sampchk' + (t.checked ? ' on' : '');
  }
});

/* 把闸门答复同步进设置项，让脚本直接就能读到 */
function syncGateToSettings(g, st) {
  if (!S.pipeline) return;
  const d = st.answer_detail;
  const byId = {
    data_indir(v, dd) {
      const path = (dd === null || dd === undefined) ? '' : (Array.isArray(dd) ? dd[0] : String(dd).trim());
      if (v === 'keep' || !path) return;
      S.pipeline.io = S.pipeline.io || {};
      S.pipeline.io.indir = { value: path, source: 'user', why: '由本人确认（助手第 1 项）' };
      S.dirtyParams['io.indir'] = true;
    },
    reference_source(v, dd) {
      const path = (dd === null || dd === undefined) ? '' : (Array.isArray(dd) ? dd[0] : String(dd).trim());
      S.pipeline.io = S.pipeline.io || {};
      if (v === 'skip') {
        S.pipeline.io.reference_table = { value: null, source: 'user', why: '本次跳过注释' };
        S.pipeline.io.lit_dict = { value: null, source: 'user', why: '本次跳过注释' };
      } else if (v === 'lit_dict' && path) {
        S.pipeline.io.lit_dict = { value: path, source: 'user', why: '由本人确认' };
        S.pipeline.io.reference_table = { value: null, source: 'user', why: '改用 marker 词典' };
      } else if (v === 'reference_table' && path) {
        S.pipeline.io.reference_table = { value: path, source: 'user', why: '由本人确认' };
        S.pipeline.io.lit_dict = { value: null, source: 'user', why: '改用文献参考表' };
      } else return;
      S.dirtyParams['io.reference_table'] = true;
      S.dirtyParams['io.lit_dict'] = true;
    },
    qc_threshold(v) {
      if (v === 'keep') return;
      S.pipeline.params = S.pipeline.params || {};
      S.pipeline.params.qc_min_features = { value: Number(v), source: 'user', why: '由本人确认（助手第 1 项）' };
      S.dirtyParams['params.qc_min_features'] = true;
    },
    cluster_filter(v, dd) {
      S.pipeline.steps = S.pipeline.steps || {};
      const node = S.pipeline.steps.cluster_filter || {};
      node.source = 'user';
      if (v === 'none') { node.mode = 'none'; node.by_name_keep = []; node.by_cluster_keep = []; }
      if (v === 'by_name') {
        node.mode = 'by_name';
        node.by_name_keep = Array.isArray(dd) ? dd : (dd ? String(dd).split(/[,，]/).map(s => s.trim()) : []);
      }
      if (v === 'by_cluster') { node.mode = 'by_cluster'; node.reason = '由本人确认'; }
      node.reason = node.reason || '由本人确认';
      S.pipeline.steps.cluster_filter = node;
      S.dirtySteps['cluster_filter'] = true;
    },
    read_exclusion(v, dd) {
      S.pipeline.steps = S.pipeline.steps || {};
      const node = S.pipeline.steps.read_exclusion || {};
      node.source = 'user';
      node.mode = (v === 'none') ? 'none' : 'exclude';
      node.samples = (v === 'none') ? [] : arr(dd).map(String);
      S.pipeline.steps.read_exclusion = node;
      S.dirtySteps['read_exclusion'] = true;
    },
    post_cluster_exclusion(v, dd) {
      S.pipeline.steps = S.pipeline.steps || {};
      const node = S.pipeline.steps.post_cluster_exclusion || {};
      node.source = 'user';
      node.mode = (v === 'none') ? 'none' : 'exclude';
      node.samples = (v === 'none') ? [] : arr(dd).map(String);
      S.pipeline.steps.post_cluster_exclusion = node;
      S.dirtySteps['post_cluster_exclusion'] = true;
    },
    lowconfidence(v) {
      S.pipeline.steps = S.pipeline.steps || {};
      const node = S.pipeline.steps.lowconfidence || {};
      node.source = 'user';
      if (v === 'flag_only') { node.mode = 'flag_only'; node.min_cells = node.min_cells || 200; }
      else { node.mode = 'threshold'; node.min_cells = Number(v); }
      S.pipeline.steps.lowconfidence = node;
      S.dirtySteps['lowconfidence'] = true;
    },
    memory(v) {
      S.pipeline.steps = S.pipeline.steps || {};
      const node = S.pipeline.steps.memory || {};
      node.source = 'user'; node.mode = v;
      S.pipeline.steps.memory = node;
      S.dirtySteps['memory'] = true;
    },
    seurat_version(v) {
      S.pipeline.params = S.pipeline.params || {};
      S.pipeline.params.expected_seurat_major = { value: (v === 'v5_allow' ? 5 : 4), source: 'user' };
      S.dirtyParams['params.expected_seurat_major'] = true;
    },
  };
  const fn = byId[g.id];
  if (fn) { try { fn(st.answer, d); } catch { /* 忽略 */ } }
}

/* 术语卡：默认收起，想看了点一下才展开。
   用户的原话是「这些专有名词，我要专门做一个卡片，放在最下方，
   也是做一个按钮，当用户想看的时候就看就行了」——所以它不进主流程，
   不占视线，只在需要时打开。 */
const GLOSS_LIST = [
  ['入口锁', 'guard',
   '脚本开头的一把锁。这个脚本如果不是从规定的唯一入口启动的，它就直接拒绝运行，并说明原因。'
   + '目的只有一个：保证每次运行都被记录在案。被绕过偷偷跑掉的脚本，事后查不出用了什么参数。'],
  ['指纹', 'md5',
   '一串由文件内容算出来的字符。内容改一个字，它就完全不同。它不保密，只回答一个问题：'
   + '这个文件还是不是我登记时的那个。'],
  ['台账', 'ledger',
   '一本只加不改的记录本。每次运行都追加一条：什么时候跑的、用了哪些参数、退出码多少、'
   + '产物指纹是什么。事后可以核对到底用了哪一套脚本。'],
  ['白名单', 'whitelist',
   '一份登记表，写着这条流程允许用哪些脚本、各自指纹是多少。交付前会拿实际用过的脚本去比对，'
   + '对不上就不许交付。'],
  ['闸门', 'gate',
   '必须由人来定的事，比如质控下限取多少、剔除哪些簇。没答完，脚本会直接拒绝启动，'
   + '并告诉你还差哪几件。'],
  ['快照', 'snapshot',
   '把当时的设置（参数和你的答复）存一份，随时可以退回去。'],
  ['待重跑', 'stale',
   '这一步的结果做过，但你退回了更早的一步，它现在不算数了，需要重跑。'],
];

function renderJargon() {
  const host = $('#jargonHost'); if (!host) return;
  // 名词解释不再是页面里的一节，改成右上角按钮弹出的面板：
  //   它是查阅用的，不该占正文位置，也不该让目录多出一项。
  let h = '<dl class="kvlist">';
  GLOSS_LIST.forEach(function (it) {
    h += '<dt>' + esc(it[0]) + '</dt><dd>' + esc(it[2]) + '</dd>';
  });
  h += '</dl>';
  host.innerHTML = h;
  const n = $('#jargonNote'); if (n) n.textContent = GLOSS_LIST.length + ' 个词';
}

/* boot() 的调用在文件末尾 */

/* =====================================================================
   第一步 · 运行位置
   ---------------------------------------------------------------------
   这一块取代了旧的「欢迎页 + 顶栏选目录 + 数据在哪台机器上」三处。
   为什么合并：那三处在做同一件事（决定数据在哪），却分散在三个地方，
   用户要在页面上来回找。现在只有一处，而且是第一件事。

   本机 / 服务器 两种模式，字段全部可编辑（预填但不写死）：
     预填是因为这些值通常就那一套，填一次省事；
     可改是因为不同人可能连不同的机器、用不同的账号。
   ===================================================================== */
S.place = null;      // /api/place 的返回
S.placeDraft = null; // 界面上正在编辑、还没保存的值

async function loadPlace() {
  try { S.place = await getJSON('/api/place'); }
  catch (e) { S.place = { ok: false, error: String(e.message || e) }; }
  return S.place;
}

/* 我在哪个会话里 —— 由本机后端代问网关（浏览器直连网关会被 CSP 拦下）。
   失败不抛：这不是页面能用的前提，只是"知道该把指令送到哪儿"的加分项。
   拿不到会话号时如实显示"没拿到"，不编一个出来。 */
async function loadIdentity() {
  try {
    const r = await getJSON('/api/whoami');
    S.identity = r || null;
    S.topicId = (r && r.topic_id) || null;
  } catch (e) {
    S.identity = { ok: false, error: String(e.message || e) };
    S.topicId = null;
  }
  renderIdentity();
  return S.identity;
}

function renderIdentity() {
  const el = document.getElementById('whoamiHint');
  if (!el) return;
  const r = S.identity;
  if (!r) { el.textContent = ''; return; }
  if (S.topicId) {
    el.textContent = '会话 ' + S.topicId;
    el.style.color = '';
    return;
  }
  // 说清是哪一种"没有"：没登录 / 凭据没传过来 / 后端问不到网关。
  if (r.ok && r.identity && r.identity.authenticated === false) {
    el.textContent = r.saw_cookie || r.saw_auth ? '未登录会话' : '无凭据';
  } else {
    el.textContent = '会话未知';
  }
  el.style.color = '#8a8f8b';
}

/* 把服务端读到的值灌进草稿。每次「重新载入」都重置草稿，
   避免用户改了没保存、界面却像已经生效了。
   可选传一份 place（来自分析记录）：切记录时要把那一份记下的机器与路径
   恢复成当前设置 —— 否则页面显示的是这次分析的目录、实际动手用的是上一次的。 */
function placeDraftFromServer(place) {
  const p = place || S.place || {};
  const srv = p.server || {};
  const rt = srv.runtime || {};
  const loc = p.local || {};
  return {
    mode: p.mode || 'local',
    // 兼容两种返回：local.path（新接口）和顶层 local_path（备用）。
    //   接口字段名一旦对不上，界面会显示成空路径，而用户刚填过的内容
    //   看起来像是被清掉了 —— 这类「看起来没保存成功」的问题最难查。
    local_path: loc.path || p.local_path || '',
    host: srv.host || '',
    port: srv.port || 22,
    user: srv.user || '',
    // 执行电脑：哪台工作电脑能连上这台服务器。
    //   存它的理由见 buildInstructionText 里那段说明：指令里写清名字，
    //   执行端就不必逐台试连。第一次跑成功后应用自动填上，也能手改。
    exec_pc: srv.exec_pc || '',
    password: '',          // 口令永远不从服务端取回：服务端根本不会回传它
    has_password: !!srv.has_password,
    data_root: srv.data_root || '',
    script_root: srv.script_root || '',
    work_root: srv.work_root || '',
    rscript: rt.rscript || '',
    queue: rt.queue || '',
    node_constraint: rt.node_constraint || '',
    label: srv.label || '',
  };
}

function renderPlace() {
  const host = $('#placeHost'); if (!host) return;
  if (!S.placeDraft) S.placeDraft = placeDraftFromServer();
  const d = S.placeDraft;
  // 「已识别」只看本次打开之后有没有点过保存，不看配置文件里存的值。
  //   否则刷新后按钮旁会写「已识别过」，可页面明明是刚打开、什么都还没跑。
  const ready = !!S.identified;
  const note = $('#placeNote');

  let h = '<div class="pl-card">';

  // ---- 两台机器二选一 ----
  h += '<div class="pl-modes">'
    + '<label class="pl-mode' + (d.mode === 'local' ? ' sel' : '') + '" data-plmode="local">'
    + '<span class="mk"></span><span><b>本机</b><span>数据在这台电脑上</span></span></label>'
    + '<label class="pl-mode' + (d.mode === 'server' ? ' sel' : '') + '" data-plmode="server">'
    + '<span class="mk"></span><span><b>服务器</b><span>数据在远端服务器上</span></span></label>'
    + '</div>';

  if (d.mode === 'local') {
    // 「本机」= 跑这个应用的这台机器，不是使用者自己的电脑。
    //   以前这里只放一个「选择文件夹」按钮，人会以为浏览框里是自己的 Windows 盘 ——
    //   实际看到的是应用所在机器的目录，两边根本不是一台机器。
    //   与其让人自己撞明白，不如直接说清，并给出正确做法（用整包在自己电脑上跑）。
    h += '<div class="rz warn" style="margin-bottom:12px"><span class="mk">!</span><span>'
      + '<b>「本机」= 跑这个应用的机器</b> '
      + '<span class="dt">要跑在自己电脑上，用整包在本机起服务。</span>'
      + '</span></div>';
    h += '<div class="pl-field"><label>工作路径</label>'
      + '<input type="text" data-plf="local_path" value="' + esc(d.local_path)
      + '" placeholder="例如 /data/run1"></div>';
    if (d.local_path) {
      h += '<div class="small" style="margin-left:132px">'
        + (S.place && S.place.local_ok === false
            ? '<span style="color:#a8631a">路径读不到。</span>'
            : '产物都写在这个目录。')
        + '</div>';
    }
  } else {
    h += '<div class="pl-field"><label>IP / 主机名</label>'
      + '<input type="text" class="mono" data-plf="host" value="' + esc(d.host) + '" placeholder="例如 10.1.1.2"></div>';
    h += '<div class="pl-field"><label>端口</label>'
      + '<input type="text" class="mono" data-plf="port" value="' + esc(d.port) + '" placeholder="22"></div>';
    h += '<div class="pl-field"><label>账号</label>'
      + '<input type="text" class="mono" data-plf="user" value="' + esc(d.user) + '" placeholder="你的登录账号"></div>';
    h += '<div class="pl-field"><label>密码</label>'
      + '<input type="password" data-plf="password" value="" placeholder="'
      + esc(d.has_password ? '已保存（留空则不修改）' : '填登录密码') + '"></div>';
    h += '<div class="pl-field"><label>数据目录</label>'
      + '<input type="text" data-plf="data_root" value="' + esc(d.data_root)
      + '" placeholder="服务器上放 matrix 的目录"></div>';
    // 脚本目录与数据目录是两个位置。
    //   交付过来的脚本通常和数据放在同一个父目录下（这里就是
    //   .../6sample_data_script/ 里面既有 matrix 也有各步骤脚本），
    //   把它们塞进一个字段，就一定会漏掉其中一个。
    h += '<div class="pl-field"><label>脚本目录</label>'
      + '<input type="text" data-plf="script_root" value="' + esc(d.script_root)
      + '" placeholder="服务器上放分析脚本的目录"></div>';
    h += '<div class="pl-field"><label>结果目录</label>'
      + '<input type="text" data-plf="work_root" value="' + esc(d.work_root)
      + '" placeholder="服务器上写结果的目录"></div>';
    // 执行电脑：内网服务器通常只有一台工作电脑连得上。
    //   以前这一项不存在，指令里只能写「在能连上这台服务器的电脑上执行」，
    //   执行端看不到会话选中的是哪一台，只能逐台试 —— 试到连不上的那台，
    //   就得把连接超时一直等完。第一次识别成功后这里会自动填上。
    h += '<div class="pl-field"><label>执行电脑</label>'
      + '<input type="text" data-plf="exec_pc" value="' + esc(d.exec_pc)
      + '" placeholder="能连上服务器的电脑，识别后自动填"></div>';
    h += '<div class="small" style="margin-left:132px">留空就要挨台试，会慢。</div>';
    h += '<details class="stgfold" style="margin-top:14px"><summary>进阶（一般不用改）'
      + '<span class="cnt">R 程序 / 队列 / 节点</span></summary>'
      + '<div style="padding:10px 16px 16px">'
      + '<div class="pl-field"><label>R 程序</label>'
      + '<input type="text" class="mono" data-plf="rscript" value="' + esc(d.rscript) + '" placeholder="/share/apps/R/.../Rscript"></div>'
      + '<div class="pl-field"><label>队列</label>'
      + '<input type="text" class="mono" data-plf="queue" value="' + esc(d.queue) + '" placeholder="all.q"></div>'
      + '<div class="pl-field"><label>节点</label>'
      + '<input type="text" class="mono" data-plf="node_constraint" value="' + esc(d.node_constraint)
      + '" placeholder="hostname=master01"></div>'
      + '</div></details>';
  }

  // ---- 保存并识别 ----
  //   按钮永远叫「保存并识别」，不随状态改名。
  //   之前信息齐全后它会变成「重新识别」，于是这一页上找不到"保存"这个动作 ——
  //   用户的原话是「没有保存识别按钮啊」。动作名不该因为状态而消失。
  h += '<div class="pl-actions">'
    + '<button class="btn primary big" data-plcommit="1">保存并识别</button>'
    + '<span class="small">' + (d.mode === 'server'
        ? '保存并交给助手去识别'
        : '保存并自动识别')
    + '</span>'
    + (ready ? '<span class="small pl-done">改完需重新识别</span>' : '')
    + '<span id="plMsg" class="small"></span>'
    + '</div>';

  // 「这次识别交给助手没有、结果回来没有」不再画在这里。
  //   它和识别结论是同一件事的两面，拆两处显示会各说一半：
  //   按钮下面说"已交给助手"，识别结论里却说"识别不了"。
  //   现在统一放在「识别结论」那一栏里，一处说话。

  h += '</div>';

  // ---- 没识别成功的项：直接列在这里，而不是让人去找 ----
  //   这些都是「原本该自动填上、这次没填上」的东西。
  //   它们不影响已经保存的设置，但会让人误以为环境已经全认出来了。
  // 服务器模式下，后端会带两条「本机连不上服务器 / 体检还没做」的提示。
  //   那不是识别失败，是这条链路的正常形态（识别本来就由助手代跑）。
  //   把它们列进「没能识别」，人会以为出错 —— 所以这里单独说清，不计入失败项。
  // 服务器模式下后端只会带一条「体检还没做、已交给助手」，
  //   它不是识别失败，也不该被列进「没能识别」。这里把它滤掉，只留真问题。
  //   （以前这里还多画一张卡解释"应用在云端所以读不到服务器"——
  //    那段话把链路讲反了：体检本来就不是应用自己去做的事，已删。）
  const isServerMode = (d.mode === 'server');
  const realWarns = isServerMode
    ? arr(S.warns).filter(function (x) {
        const t = String(x);
        return t.indexOf('环境体检还没做') < 0;
      })
    : arr(S.warns);
  const ws = realWarns;
  if (ws.length) {
    h += '<div class="pl-card" style="margin-top:12px">'
      + '<div class="rz warn" style="margin:0 0 8px"><span class="mk">!</span><span>'
      + '<b>这 ' + ws.length + ' 项没识别出来</b> '
      + '<span class="dt">设置已保存，这几项没自动填上。</span></span></div>'
      + '<div class="rzlist">'
      + ws.map(function (w, i) {
          return '<div class="rz"><span class="mk">' + (i + 1) + '</span><span>'
            + esc(w) + '</span></div>';
        }).join('')
      + '</div></div>';
  }

  host.innerHTML = h;

  note.textContent = ready ? '已确认' : '';
}

/* 弹窗选文件夹：复用同一个目录浏览弹窗，选完写回工作路径 */
function openPathPicker() {
  S.pickTarget = 'place';
  openDlg('dlgDir');
  const start = (S.placeDraft && S.placeDraft.local_path)
    || (S.env && S.env.home) || null;
  browseTo(start);
}

async function commitPlace() {
  const d = S.placeDraft || {};
  const msg = $('#plMsg');
  if (msg) msg.textContent = '正在识别环境与脚本库…';
  // 上一轮自动注入的结论不留给这一轮 —— 否则「已交给助手」会一直挂着，
  //   让人以为这次也发出去了。
  S.autoInject = null;
  // 上一轮的等待也要停掉：这一轮有新的请求号，旧的那次再写回来也不算数。
  stopWaitWriteback();
  S.identifyPhase = null;

  // 先把字段存下来，再提交。
  //   分两步是为了：即使识别那一步失败，用户填的东西也已经落地，
  //   不用重填一遍。
  const payload = {
    mode: d.mode,
    // 这次识别属于哪一份分析记录。
    //   带上它，回写的结果才能挂到"这一份"名下；
    //   不带的话，多份记录并存时结果会挂到当前那一份，可能不是用户在看的这份。
    session_id: S.sessionId || undefined,
    local_path: d.mode === 'local' ? d.local_path : undefined,
    server: d.mode === 'server' ? {
      host: d.host, port: d.port, user: d.user,
      exec_pc: d.exec_pc,
      password: d.password,          // 空串 = 不改口令，服务端按这个规则处理
      data_root: d.data_root, script_root: d.script_root, work_root: d.work_root,
      label: d.label,
      runtime: { rscript: d.rscript, queue: d.queue, node_constraint: d.node_constraint },
    } : undefined,
  };
  try {
    // 动手之前先按这一份现在的样子存一个存档。
    //   为什么放在这里：点「保存并识别」往往就是"我要改点什么"的时刻。
    //   先留一份当时的状态，改坏了能整份退回来 —— 而且这一步是自动的，
    //   不指望人记得手动点。
    await archiveNow('before-identify', '自动存档');
    // 一次请求就够：/api/place/commit 自己会先把设置落盘，再往下走。
    //   以前这里是「先 POST /api/place，再 POST /api/place/commit」两次往返 ——
    //   第一次的返回值没人用，纯粹白等一个来回（用户的原话是"为啥这么慢"）。
    //   分成两步的初衷是「保存」和「开始干活」两个动作分开，但那是**界面**上的
    //   两件事，不该落成两次网络往返：commit 内部本来就不做写入以外的事。
    const r = await postJSON('/api/place/commit', payload);
    if (!r.ok) throw new Error(r.error || '识别失败');
    S.place = r.place || S.place;
    S.placeDraft = placeDraftFromServer();
    // 本次会话已经识别过：按钮文案与「已识别」提示以它为准，
    // 不写进任何配置文件 —— 刷新之后要重新回到未识别状态。
    S.identified = true;
    // 本次请求号：后面注入的文本要带上它，助手回写时带回同一个号。
    S.commitReq = r.request_id || null;
    // 服务端可能顺手开了一份新记录（第一次点保存并识别时）——
    //   立刻接管它，否则这一次识别的结果就没有归属，刷新必丢。
    if (r.session_id && S.sessionId !== r.session_id) {
      S.sessionId = r.session_id;
    }
    await loadSessionsState();
    renderSessionsBar();
    // 服务器模式：识别不在本机跑，所以这一轮拿不到数字。
    //   前端据此显示"等助手写回"，而不是把空结果画成"检查完了"。
    S.needsRemote = !!r.needs_remote;
    // 备份任务号：这次识别排给了谁，事后能查到。
    S.queuedCheck = r.queued_check || '';
    S.warns = arr(r.warnings).map(function (x) { return String(x); });
    // 结论什么时候出现，取决于「谁去跑识别」——
    //   本机模式：本机当场跑完，结论立刻可出。
    //   服务器模式：识别由助手在能连上服务器的电脑上做，结论要等它写回才有。
    //   这时不能拿"本机读不到服务器"当识别结果 —— 用户看到的就是"识别不了"，
    //   而其实什么都还没开始。
    const needDelegate = (d.mode === 'server') && !!r.needs_remote;
    S.identifyPhase = needDelegate ? 'waiting' : 'ready';
    if (needDelegate) {
      toast('已保存并交给助手。', 'good');
    } else if (S.warns.length) {
      toast('已保存，' + S.warns.length + ' 项没识别出来', 'bad');
    } else {
      toast('已保存，识别完成。', 'good');
    }
    await loadRun();
    setStageVisible(true);
    renderAll();

    // 服务器模式下把「这次识别」交给助手，并开始等它写回 —— 和按钮同一步发生。
    //   本机模式不需要这一步：那边当场就跑完了。
    if (needDelegate) { autoInjectIdentify(); startWaitWriteback(); }
  } catch (e) {
    if (msg) msg.textContent = '';
    toast('保存失败：' + e.message, 'bad');
  }
}

/* =====================================================================
   把「这次识别」交给助手（由「保存并识别」触发）
   ---------------------------------------------------------------------
   这一步不再有单独的手动入口：用户点一次「保存并识别」，
    设置落盘、本机识别跑完、剩下的交给助手 —— 一个动作做完。
   失败的形态要说清：
     · 通道不通（独立窗口 / 没 SDK）不算保存失败，设置已经存下了。
       这时把原文摆出来让人自己复制发送，功能不押在一条握手上。
     · 落盘失败也不影响识别结果，识别结论照样显示。
   ===================================================================== */
async function autoInjectIdentify() {
  // 上一轮如果卡住了，这里会永远 return，界面也就永远没反应。
  //   为什么会卡：以前 injectToSession 等平台回执没有超时，
  //   await 永不返回，finally 不执行，S.autoInjectBusy 就一直是 true。
  //   现在注入本身有 12 秒上限，这里再加一道保险：超过 30 秒还挂着，
  //   一律视为上一轮已失效，放行这一轮，不让界面被一次卡死拖住。
  if (S.autoInjectBusy) {
    const stuck = Date.now() - (S.autoInjectAt || 0);
    if (stuck < 30000) return;
    S.autoInjectBusy = false;
  }
  const d = S.placeDraft || placeDraftFromServer();
  const text = buildInstructionText(
    '在这台服务器上跑一次环境体检和脚本库识别：'
    + '看内存和磁盘够不够、R 包齐不齐、数据目录与脚本目录在不在，'
    + '再把服务器上已有的分析脚本列出来。'
    + '结果写回应用，让「识别结论」这一栏能显示出服务器那侧的真实情况。');

  S.autoInjectBusy = true;
  S.autoInjectAt = Date.now();
  S.autoInject = null;
  renderPlace();
  try {
    const r = await injectToSession(text, function () {});
    // relayed：通道没通，但指令已经落进本机队列 —— 助手能取走执行。
    //   这两种情况在页面上必须分开写：一个是"没发出去"，一个是"换条路交出去了"。
    S.autoInject = r.ok
      ? { ok: true, text: text }
      : { ok: false, code: r.code || 'failed', why: r.why || '', text: text,
          relayed: !!r.relayed, queueId: r.id || '' };
    // 落一份记录：应用本机留痕，便于事后核对"这次到底发出去没有"。
    //   失败也记，记的是失败原因。发送本身不依赖它，写不进去不影响结果。
    //   注入结果一并记上。为什么要记：注入这一步发生在浏览器里，
    //   服务端看不见平台回了什么。不记的话，事后只能看到"指令生成了"，
    //   分不清是平台没收、还是通道没连上、还是回执根本没来 ——
    //   三种情况的处理方式完全不同。记下来，下一次就有据可查。
    try {
      await postJSON('/api/instruct', {
        text: '（保存并识别时自动提交）' + text,
        inject: S.autoInject
          ? { ok: !!S.autoInject.ok,
              code: S.autoInject.code || (S.autoInject.ok ? 'accepted' : ''),
              why: S.autoInject.why || '' }
          : { ok: false, code: 'not_attempted', why: '没有走到注入这一步' },
      });
    } catch (e) {}
  } catch (e) {
    S.autoInject = { ok: false, code: 'failed', why: String(e.message || e), text: text };
  } finally {
    S.autoInjectBusy = false;
    renderPlace();
  }
}

/* =====================================================================
   识别结果：环境体检 + 自动判定
   ---------------------------------------------------------------------
   这两块不是「需要用户做的决定」，是系统自己跑出来的结论。
   所以它们以结果形式贴在第一步下面，不列进待办清单、不要求确认。
   ===================================================================== */
function verdictBadge(v) {
  if (v === 'ready' || v === 'ok') return ['可以跑', 'ok'];
  if (v === 'blocked' || v === 'fail') return ['跑不了', 'fail'];
  return ['有提醒', 'warn'];
}

/* =====================================================================
   等助手把识别结果写回来
   ---------------------------------------------------------------------
   服务器模式下识别不在本机跑，页面能做的只有两件事：把请求交出去、等结果。
   所以这里不猜、不拿旧数据显示，只是隔几秒问一次「这一次请求的结果到了吗」。
   判据是请求号：只有和本次请求号对得上的结果，才算"这一次跑出来的"。
   到了才画结论；没到就一直显示"识别中"，并说明在等什么。
   ===================================================================== */
function stopWaitWriteback() {
  if (S.waitTimer) { clearInterval(S.waitTimer); S.waitTimer = null; }
}

async function fetchEnvcheckSafe() {
  try { return (await getJSON('/api/envcheck')) || {}; } catch (e) { return {}; }
}

/* 「这次」的判据必须带时间，不能只看请求号。
   为什么：请求号对得上只说明"是这一条请求"，不说明"是刚写回来的"。
   同一条请求隔两小时再点查询，号仍然相同，于是页面照样报"结果到了" ——
   而那次回写其实早已发生，之后没有任何新写入。用户看到的就是假报。
   现在：结果时间必须晚于本次等待起点，否则一律当作旧结果如实说清。 */
function ecResultTime(ec) {
  const r = (ec && ec.result) || {};
  const t = r.imported_at || r.generated_at || '';
  const ms = t ? Date.parse(String(t).replace(' ', 'T')) : 0;
  return { ms: Number.isFinite(ms) ? ms : 0, text: t };
}

function ecIsFresh(ec) {
  if (!ec || !ec.result || ec.stale) return false;
  if (!S.waitSince) return true;
  const t = ecResultTime(ec);
  return !!t.ms && t.ms >= S.waitSince;
}

function ecAtText(ec) {
  const t = ecResultTime(ec);
  if (!t.text) return '';
  const d = String(t.text).replace('T', ' ');
  return d.length >= 16 ? (d.slice(5, 10) + ' ' + d.slice(11, 16)) : d;
}

function startWaitWriteback() {
  stopWaitWriteback();
  S.waitTries = 0;
  S.waitSince = Date.now();
  // 等到什么时候放弃。
  //   40 分钟（480 × 5 秒）：这不是"体检要跑 40 分钟"——
  //   一条 SSH 探针十几秒就完了。留的是**等待**：助手可能正在忙别的、
  //   或者这条指令正排在队列里。以前是 5 分钟，结果体检其实跑成了、
  //   结果也写回来了，页面却早已放弃、显示成超时，用户看到的就是
  //   「页面没有任何变化」。放弃得太早，等于把成功读成失败。
  const MAX_TRIES = 480;
  const tick = async function () {
    S.waitTries += 1;
    const ec = await fetchEnvcheckSafe();
    if (ecIsFresh(ec)) {
      stopWaitWriteback();
      S.identifyPhase = 'ready';
      // 结果到了也顺便把记录状态读回来：这一次的结论要挂在这一份记录上，
      //   刷新之后才还在。
      try { await loadSessionsState(); } catch (e) {}
      renderAll();
      toast('识别结果已写回。', 'good');
      return;
    }
    // 已经有结果但早于本次等待起点：不当成"到了"，只把旧结果的时间记住，
    //   页面显示"最新一份是几点"，人一眼能看出没有新写入。
    if (ec && ec.result && !ec.stale) S.staleEnvAt = ecAtText(ec);
    if (S.waitTries >= MAX_TRIES) {
      stopWaitWriteback();
      S.identifyPhase = 'timeout';
      renderAll();
    }
  };
  S.waitTimer = setInterval(tick, 5000);
  tick();
}

/* 手动查一次：不想干等的时候点一下 */
async function recheckIdentify() {
  const note = $('#idReMsg');
  if (note) note.textContent = '正在查…';
  const ec = await fetchEnvcheckSafe();
  if (ecIsFresh(ec)) {
    stopWaitWriteback();
    S.identifyPhase = 'ready';
    renderAll();
    toast('本次结果已写回。', 'good');
    return;
  }
  // 没到就是没到。若手上有一份更早的，把它的时间说出来 ——
  //   "没有新结果"和"上次那份是 15:55 的"是两件事，后者才解释得清为什么没动。
  const at = ecAtText(ec);
  const msg = at
    ? ('还没回写。手上这份是 ' + at + ' 的旧结果。')
    : '还没回写，也没有任何结果。';
  if (note) note.textContent = msg;
  if (S.identifyPhase === 'timeout' || !S.identifyPhase) {
    S.staleEnvAt = at || '';
    renderAll();
  }
  // 重新开始等：人点了查询，说明还在意这一趟。
  if (S.identifyPhase === 'timeout') S.identifyPhase = 'waiting';
}

/* 等结果这段时间里，识别结论这一栏长什么样。
   要点：只说"在等什么"，不下任何结论 —— 成功还是失败由助手跑出来的算。 */
function identifyWaitingHTML() {
  const pd = S.placeDraft || {};
  const where = (pd.mode === 'server')
    ? ('服务器 ' + (pd.host || '（未填地址）'))
    : '这台电脑（本机）';
  const to = S.identifyPhase === 'timeout';
  const ai = S.autoInject;

  let h = '<div class="pl-card">';
  h += '<div class="idhead">'
    + '<h3 class="idtitle">识别结论</h3>'
    + '<span class="idbadge warn">' + (to ? '还没回来' : '识别中') + '</span>'
    + '<span class="idwhere">' + esc(where) + '</span>'
    + '</div>';
  const oldAt = S.staleEnvAt || '';
  h += '<div class="idsum">' + esc(to
      ? ('还没收到回写 —— 这一栏不下结论。')
      : ('正在等助手到 ' + where + ' 上跑一次环境体检和脚本库识别。'))
    + '</div>';
  // 手上已有旧结果时把时间point出来：不说这句，人只会看到"识别中"不动，
  //   以为是卡住了，其实是这一次还没回写。
  if (oldAt) {
    h += '<div class="idsum" style="color:#8a5a00">手上这份结果是 '
      + esc(oldAt) + ' 的，不是这一次的。</div>';
  }

  // 交给助手这一步的实况：成了就报成，没交出去就把原文摆出来。
  let lv = 'warn', mk = '!', tx = '正在把这次识别交给助手…';
  if (S.autoInjectBusy) { lv = 'warn'; mk = '!'; tx = '正在把这次识别交给助手…'; }
  else if (ai && ai.ok) { lv = 'ok'; mk = '✓'; tx = '已交给助手'; }
  else if (ai && ai.relayed) {
    // 通道断着，但指令已经在本机队列里，助手能取走 —— 这不是失败。
    lv = 'warn'; mk = '!';
    tx = '平台会话通道没连上，已把这段放进助手队列'
      + (ai.queueId ? '（' + ai.queueId + '）' : '')
      + '，我会直接取走执行。';
  }
  else if (ai) {
    const soft = (ai.code === 'standalone' || ai.code === 'no_sdk');
    lv = soft ? 'warn' : 'fail'; mk = soft ? '!' : '×';
    tx = '这段没自动交给助手：' + (ai.why || ai.code || '');
  }
  h += '<div class="rz ' + lv + '" style="margin-top:10px"><span class="mk">' + mk + '</span><span>'
    + '<b>' + esc(tx) + '</b>'
    + (lv === 'ok' ? ' <span class="dt">在执行机上跑。</span>' : '')
    + '</span></div>';

  if (ai && !ai.ok && !ai.relayed) {
    // 通道不通不是"识别失败"，设置已经存下了。把原文摆出来让人自己发。
    h += '<div style="margin-top:8px">'
      + '<div class="small" style="margin-bottom:4px">或复制这段发给助手：</div>'
      + '<textarea readonly data-idraw="1" rows="7" '
      + 'class="idraw">'
      + esc(ai.text || '') + '</textarea>'
      + '<div class="btnrow" style="margin-top:6px">'
      + '<button class="btn" data-idcopy="1">复制这段</button>'
      + '<span class="small" id="idCopyMsg"></span></div></div>';
  }

  h += '<div class="btnrow" style="margin-top:12px">'
    + '<button class="btn" data-idrecheck="1">现在查一次结果</button>'
    + (to ? '<button class="btn" data-idreinject="1">再交给助手一次</button>' : '')
    + '<span class="small" id="idReMsg"></span></div>';

  if (to) {
    h += '<div class="rz warn" style="margin-top:8px"><span class="mk">!</span><span>'
      + '<b>等了 5 分钟还没到</b> <span class="dt">可能还在排队。'
      + '可点「现在查一次」或「再交给一次」。</span></span></div>';
  }

  h += '</div>';
  return h;
}

/* 「保存并识别」这一趟里，头三步各自走到哪了。
   为什么要这一条：
    环境体检 / 自动判定 / 容量预检 本来和识别是同一趟事（用户的原话是
    「为什么都在跑分析这个环节里」）。它们既然属于识别，进展就该在识别结论里
    说得清，而不是靠「跑分析」里三个按钮让人去点 —— 那三个按钮在服务器模式下
    点了只会得到一句"要去助手那边跑"。
   判据只用已经拿到的东西，不猜：
    体检：有 items 就是跑过了；带回 error 就是没跑成
    判定：本机模式下有 decision.json 才有；服务器模式的判定随识别一起回来
    预检：要等主流程上段之后才有依据，这里如实说"还没到" */
function identifyStepStrip(env, dec, envFailed) {
  const it = (id) => arr(env && env.items).find(x => x.id === id);
  const hasEnv = !!env && arr(env.items).length > 0;

  const steps = [];
  // ① 环境体检
  if (!hasEnv && !envFailed) {
    steps.push(['env', '环境体检', 'wait', '等识别结果回来']);
  } else if (envFailed) {
    steps.push(['env', '环境体检', 'fail', String(env.error || '这次没跑成')]);
  } else {
    const c = (env && env.counts) || {};
    steps.push(['env', '环境体检', (c.fail ? 'fail' : (c.warn ? 'warn' : 'done')),
      '通过 ' + (c.ok || 0) + ' 提醒 ' + (c.warn || 0) + ' 缺失 ' + (c.fail || 0)]);
  }
  // ② 自动判定
  if (dec && dec.verdict_cn) {
    const cls = (dec.path === 'blocked') ? 'fail' : ((dec.path === 'warn' || dec.verdict === 'warn') ? 'warn' : 'done');
    steps.push(['decide', '自动判定', cls, String(dec.verdict_cn)]);
  } else if (hasEnv) {
    // 服务器模式下判定由识别那一趟一并给出；这里如实说明它还没回来，
    //   而不是把它画成"不需要"。
    steps.push(['decide', '自动判定', 'wait',
      S.needsRemote ? '随识别结果一并回来' : '还没跑']);
  } else {
    steps.push(['decide', '自动判定', 'wait', '等环境体检先出结果']);
  }
  // ③ 容量预检
  steps.push(['precheck', '容量预检', 'wait', '上段跑完才有依据']);

  const mk = { done: '✓', warn: '!', fail: '×', wait: '○' };
  return '<div class="stepstrip">' + steps.map(s =>
    '<span class="stp ' + s[2] + '"><span class="mk">' + mk[s[2]] + '</span>'
    + '<b>' + esc(s[1]) + '</b> <span class="dt">' + esc(s[3]) + '</span></span>').join('')
    + '</div>';
}

/* =====================================================================
   识别结论
   ---------------------------------------------------------------------
   保存并识别之后，页面不能只多出几个脚本条目 —— 那等于什么都没说：
   看的人不知道该不该往下走、这些脚本从哪来、还缺什么。
   所以这里按「一次判断」的顺序写四段：
     ① 这台机器能不能跑（环境）
     ② 该用哪一套脚本（判断 + 依据）
     ③ 从这台机器识别到什么脚本（数量、哪些要留意 → 明细在下面）
     ④ 应用自带的工具（出图 / PPT / 报告提取 / 参考依据，不依赖服务器）
   ③ 只给结论与指引，明细交给下面的脚本区，避免两处重复。

   每一段的标题可以点开收起。
     为什么要它：①②一定要看，③④是明细 —— 明细长了就把结论挤到屏幕外，
     想核对的人得一路滚。收起状态按分析记录保存在服务端，
     不放在浏览器里：刷新、换一台电脑打开，看到的还是你上次收成什么样。
   ===================================================================== */

/* 小节收纳状态。
   存两份：S.collapsed 是当前生效的，写回服务端由 persistCollapsed 做。
   键名用固定短名（env / pick / sup / tools），不要用下标 ——
   下标会随着「这次有没有这一段」而变，收起的就变成另一段了。

   实现用 <details>/<summary> 而不是自己拿 JS 开关 div：
   原生元素自带键盘可达、找到关键字高亮、打印时按需展开，
   而且标题与内容的结构关系是写在标签里的，不会因为重渲染错位。 */
const IDENT_SECS = ['env', 'pick', 'sup', 'tools'];

/* 四段默认收起 —— 用户的原话是「能折叠的都默认折叠，除非这个流程正在跑」。
   但「默认收起」有一个必须让路的例外：这一段正对应着正在跑的东西。
   正在等结果的人最需要看到的就是那一段的实况，把它收起来等于让人去猜。
   所以：
     · 用户手动点开过 → 尊重用户（c[key] === 0 表示显式展开）
     · 用户手动收起过 → 尊重用户（c[key] === 1）
     · 都没表态过     → 正在跑的那一段展开，其余收起 */
const IDENT_SEC_STAGE = { env: 'env', pick: 'decide', sup: 'figs', tools: '' };

function identSecOpen(key) {
  const c = S.collapsed || {};
  if (c[key] === 0) return true;      // 人明确点开过
  if (c[key] === 1 || c[key] === true) return false;  // 人明确收起过
  // 没表态：看这一段是不是正在跑
  return identSecRunning(key);
}

/* 这一段是不是「正在跑」。
   判据只用已有的东西，不猜：
     · 本机模式：S.running 且当前步骤落在这一段管的阶段上
     · 服务器模式：对应阶段的回写状态是 running / delegated
   拿不到任何依据时返回 false —— 默认收起是基线，不能因为不知道就全展开。 */
function identSecRunning(key) {
  // 环境体检那一趟本来就是「等助手写回」，等的时候要看得见
  // 识别这一趟正在等助手写回时，前两段是要看的
  if (S.identifyPhase === 'waiting' && (key === 'env' || key === 'pick')) return true;
  const stage = IDENT_SEC_STAGE[key];
  if (!stage) return false;
  if (S.running && S.step === stage) return true;
  const rs = (S.remoteSteps || {})[stage];
  if (rs && (rs.status === 'running' || rs.status === 'delegated')) return true;
  return false;
}

/* 一段的标题（可点开收起）。asideHTML 是标题右侧那行灰字说明。 */
function identSecHead(key, titleHTML, asideHTML) {
  const open = identSecOpen(key);
  return '<details class="secblk secfold" data-ident-sec="' + esc(key) + '"'
    + (open ? ' open' : '') + '>'
    + '<summary class="sech"><span class="fmk" aria-hidden="true"></span>'
    + titleHTML + (asideHTML || '') + '</summary>';
}

/* 收起状态落盘。失败不提示、不阻断阅读 —— 它只是阅读习惯，
   写不进去下一次点开还是默认展开，不影响任何结论。 */
let collapsedSaveTimer = null;
function persistCollapsed() {
  if (!S.sessionId) return;
  if (collapsedSaveTimer) clearTimeout(collapsedSaveTimer);
  collapsedSaveTimer = setTimeout(function () {
    postJSON('/api/sessions/update', { id: S.sessionId, collapsed: S.collapsed || {} })
      .catch(function () {});
  }, 400);
}

/* 把已经渲染好的四个小节就地变成可折叠的。
   为什么用 DOM 转换、而不是在拼 HTML 时就把 <details> 写进去：
   这四段的正文是分好几处拼出来的（每段里还有各自的分支），
   在拼串阶段插标签要改五六处、每处都可能把 </div> 配错；
   而「哪个 div 是小节」在 DOM 里是一眼可辨的（.secblk 且直接子节点里有 .sech），
   转换一次就够，正文一个字都不用动 —— 否则这次改动会碰到整块渲染逻辑，
   而那块逻辑里刚修好的东西（失败态、覆盖率判断）不该被顺手碰一遍。

   用 <details>/<summary> 而不是自己拿 JS 开关 div：
   原生元素自带键盘可达、Ctrl+F 能找到收起的内容、打印时按需展开。 */
function bindIdentFolds(root, keys) {
  if (!root) return;
  const list = arr(keys);
  // 四段都在最外层那张卡里，所以先定位卡，再取它的直接子 .secblk ——
  //   用 :scope 锁住层级，免得哪天某段内部再出现一个 .secblk，
  //   把里面的也当成一段收起来。
  const card = root.querySelector('.pl-card') || root;
  const blks = $$(':scope > .secblk', card);
  blks.forEach(function (blk, i) {
    const head = blk.querySelector(':scope > .sech');
    const key = list[i];
    if (!head || !key) return;
    const d = document.createElement('details');
    d.className = 'secblk secfold';
    d.setAttribute('data-ident-sec', key);
    if (identSecOpen(key)) d.open = true;
    const sum = document.createElement('summary');
    sum.className = head.className;
    sum.innerHTML = '<span class="fmk" aria-hidden="true"></span>' + head.innerHTML;
    d.appendChild(sum);
    Array.prototype.slice.call(blk.childNodes).forEach(function (n) {
      if (n === head) return;
      d.appendChild(n);
    });
    blk.parentNode.replaceChild(d, blk);
    d.addEventListener('toggle', function () {
      S.collapsed = S.collapsed || {};
      // 展开记 0、收起记 1 —— 都得记住。
      //   以前展开是 delete，于是「人点开过」和「从没表态」分不出来，
      //   默认一改成收起，人点开的那一段下次刷新又被收回去。
      S.collapsed[key] = d.open ? 0 : 1;
      persistCollapsed();
    });
  });
}

async function renderIdentify() {
  const host = $('#identifyHost'); if (!host) return;
  // 判据是「这一次点过保存并识别」，不是「本机记没记住运行目录」。
  //   服务器模式下运行目录在服务器上，本机 state 里是空的；
  //   按 S.runDir 判会让整块识别结论直接消失 —— 点了保存却什么都没出现。
  if (!S.identified) { host.innerHTML = ''; return; }
  // 服务器模式下，「保存并识别」只是把活交给助手，识别结论要等它写回才有。
  //   这段时间只显示"在等什么"。绝不在这里下结论：本机读不到服务器的内存和包，
  //   拿这份空缺当"识别不了"是错的 —— 那只是"还没开始"。
  if (S.identifyPhase === 'waiting' || S.identifyPhase === 'timeout') {
    host.innerHTML = identifyWaitingHTML();
    return;
  }

  let env = null, dec = null, lib = null;
  let envStale = false, staleEnv = null, staleNote = '';
  // 先看「这一份记录自己识别出了什么」。
  //   多份记录并存之后，这个才是正确的问题 —— /api/envcheck 回答的是
  //   「最近一次识别是什么」，切到旧记录时会显示新记录的数字。
  //   记录里有结论就用它；没有才回退去问 /api/envcheck（老数据/本机模式）。
  let fromSession = false;
  if (S.sessionId) {
    try {
      const ss = await getJSON('/api/sessions/state?id=' + encodeURIComponent(S.sessionId));
      const ident = ss && ss.session && ss.session.identify;
      if (ident && ident.result) {
        env = ident.result;
        S.commitReq = ident.req_id || S.commitReq;
        fromSession = true;
      }
    } catch (e) {}
  }
  // 一律问一次：服务器模式下，/api/envcheck 读的是执行端回写的那份结果。
  //   只认「属于最近一次保存并识别」的结果。
  //   不属于的话，它只是上一轮跑出来的旧数据 —— 拿来当结论显示，
  //   会让人以为刚点的那次已经生效了，而助手可能根本还没动。
  if (!fromSession) {
    try {
      const ec = await getJSON('/api/envcheck');
      envStale = !!ec.stale;
      if (envStale) {
        staleEnv = ec.result || null;
        env = null;
      } else {
        env = ec.result || null;
      }
    } catch (e) {}
  }
  if (!S.needsRemote) {
    try { dec = (await getJSON('/api/decision')).result || null; } catch (e) {}
  }
  try { lib = await getJSON('/api/library'); } catch (e) {}
  S.envCheck = env; S.decision = dec;
  S.envStale = envStale; S.staleEnv = staleEnv;
  // 本机模式下没拿到体检/判定结果就没什么可说的；
  //   但服务器模式本来就不该有这两个结果 —— 不能因此把整块清空。
  if (!S.needsRemote && !env && !dec) { host.innerHTML = ''; return; }

  // 脚本统计：环节数、脚本数、需要留意的个数
  //   只把「能独立跑的脚本」计入需要留意；lib/ 下的子模块本来就靠别人
  //   source 调用，不需要启动锁，把它们算进去会虚报一堆无关的数字。
  const stages = arr(S.stages);
  let nScript = 0, nFlag = 0;
  stages.forEach(st => arr(st.scripts).forEach(s => {
    nScript++;
    const isModule = String(s.role || '').indexOf('子模块') >= 0 || String(s.path || '').indexOf('/lib/') >= 0;
    if (!s.guarded && !isModule) nFlag++;
  }));

  // 应用自带的工具：库表里 B 类（分析绘图）——这些不放在服务器上，
  //   应用自己带着，服务器缺什么都不影响它们。
  //   注意：entries 里没有 category 字段（只有 id 前缀），按 'B-' 前缀取。
  //   之前按 e.category==='B' 过滤，结果一条都取不到，第 ④ 段永远是空的。
  const tools = arr(lib && lib.entries).filter(e => String(e.id || '').indexOf('B-') === 0);

  const blocked = !!(dec && dec.path === 'blocked');
  const warnOnly = !blocked && env && env.verdict === 'warn';
  // 助手写回来了，但带回的是"这次没跑成"（连不上、SSH 失败、拿不到数据……）。
  //   这种情况必须显示成失败，并原样带上它给的原因。
  //   不判这一条的话，空的体检项会被当成"0 项不通过"画成"全部通过"——
  //   把一次失败读成成功，比不显示更糟。
  const envFailed = !!env && (env.ok === false || !!env.error || !arr(env.items).length);

  // 说清是"哪台机器"：用户选的是服务器时，写「服务器 10.1.1.2」而不是含糊的
  //   「这台机器」—— 那句在服务器模式下读起来完全不知道指谁。
  const pd = S.placeDraft || {};
  const where = (pd.mode === 'server')
    ? ('服务器 ' + (pd.host || '（未填地址）'))
    : '这台电脑（本机）';

  let h = '<div class="pl-card">';

  // ---- 总判断 ----
  h += '<div class="idhead">'
    + '<h3 class="idtitle">识别结论</h3>'
    + '<span class="idbadge ' + ((S.needsRemote && !env) ? 'warn'
        : ((blocked || envFailed) ? 'bad' : (warnOnly ? 'warn' : 'ok'))) + '">'
    + ((S.needsRemote && !env) ? '待检查'
        : ((blocked || envFailed) ? '没跑成' : (warnOnly ? '能跑，有提醒' : '可以跑'))) + '</span>'
    + '<span class="idwhere">' + esc(where) + '</span>'
    + '</div>';
  // 本次识别里的头三步走到哪了。
  //   这三步是「保存并识别」那一趟顺带做完的（体检 -> 自动判定 -> 容量预检），
  //   所以它们的状态显示在这里，而不是摆成「跑分析」里的三个按钮 ——
  //   摆成按钮，人点了只会得到一句"服务器模式要去助手那边跑"。
  h += identifyStepStrip(env, dec, envFailed);
  h += '<div class="idsum">' + esc((S.needsRemote && !env)
      ? '服务器还没检查过，等结果回来。'
      : (envFailed
        ? ('这次没跑成：' + String(env.error || '回写里没有体检项')
           + '。下面第 ① 段是它给的原因。')
        : (blocked
        ? ('现在跑不了：' + (arr(dec.gaps)[0] || '有必需项缺失'))
        : (warnOnly
          ? (esc(where) + ' 可以跑，有几处要留意。')
          : (esc(where) + ' 可以跑完整流程。')))))
    + '</div>';

  // ---- ① 环境评估 ----
  h += '<div class="secblk"><div class="sech">' + esc(where) + ' 适不适合跑</div>';
  if (S.needsRemote && !env) {
    // 服务器模式下「本机连不上」不再是一个要解释的现象：
    //   这件事本来就不由应用做。所以这里只说一件事：在等谁、等什么。
    h += '<div class="rz warn"><span class="mk">!</span><span>'
      + '<b>这次识别已交给助手</b> '
      + '<span class="dt">结果写回来这里会换成真实数字。</span></span></div>';
  } else if (envFailed) {
    h += '<div class="rz fail"><span class="mk">×</span><span>'
      + '<b>这次没跑成</b> <span class="dt">'
      + esc(String(env.error || env.checked_via || '')
            || '回写里没有体检项。')
      + '</span></span></div>';
  } else if (env) {
    const c = env.counts || {};
    h += '<div class="idmeta">检查 ' + arr(env.items).length + ' 项：通过 ' + (c.ok || 0)
      + '　提醒 ' + (c.warn || 0) + '　缺失 ' + (c.fail || 0)
      + (env.generated_at ? '　（' + esc(String(env.generated_at).replace('T', ' ')) + '）' : '') + '</div>';
    const bad = arr(env.items).filter(it => it.status !== 'ok');
    if (bad.length) {
      h += '<div class="rzlist">' + bad.map(it =>
        '<div class="rz ' + (it.status === 'fail' ? 'fail' : 'warn') + '">'
        + '<span class="mk">' + (it.status === 'fail' ? '×' : '!') + '</span>'
        + '<span><b>' + esc(it.label) + '</b> <span class="dt">' + esc(it.detail) + '</span></span></div>').join('')
        + '</div>';
    } else {
      // 全部通过时也别只说一句"全部通过"：
      //   看的人真正想知道的是内存多大、磁盘剩多少、数据在不在、包齐不齐。
      //   把这些实际值列出来，结论才可信。
      const keyIds = ['mem','disk','r_version','data_root','script_root'];
      const keys = arr(env.items).filter(it => keyIds.indexOf(it.id) >= 0);
      if (keys.length) {
        h += '<div class="rzlist">' + keys.map(it =>
          '<div class="rz ok"><span class="mk">✓</span><span><b>' + esc(it.label)
          + '</b> <span class="dt">' + esc(it.detail) + '</span></span></div>').join('') + '</div>';
      }
      const npkg = arr(env.items).filter(it => String(it.id).indexOf('pkg_') === 0).length;
      h += '<div class="rz ok"><span class="mk">✓</span><span><b>R 包</b> '
        + '<span class="dt">' + npkg + ' 个必需包全部可加载</span></span></div>';
    }
  } else {
    h += '<div class="idmeta">这次没拿到体检结果。</div>';
  }
  h += '</div>';

  // ---- ② 该用哪一套脚本 ----
  h += '<div class="secblk"><div class="sech">这次该用哪一套脚本</div>';
  if (S.needsRemote && !env) {
    h += '<div class="rz warn"><span class="mk">!</span><span>'
      + '<b>等识别结果回来</b> '
      + '<span class="dt">要扫一次才知道，这里不猜。</span></span></div>';
  } else if (env && env.script_lib && arr(env.script_lib.files).length) {
    // 服务器上扫到了脚本库：直接说用哪套、覆盖哪几步，不再留"不知道"。
    const lib = env.script_lib;
    const covers = arr(lib.covers);
    h += '<div class="idpick">用服务器上这一套：' + esc(lib.root || '') + '</div>';
    // 判断依据要说出来，而不是只给结论。
    //   光写"用这一套"，看的人不知道自己该不该信；把判据摆出来才叫判断。
    const pk = arr(env.items).filter(it => String(it.id).indexOf('pkg_') === 0);
    const pkOk = pk.filter(it => it.status === 'ok').length;
    h += '<div class="idmeta">判断依据：这台机器上已经有 ' + arr(lib.files).length + ' 个现成脚本，'
      + '覆盖 ' + covers.length + ' 个环节；必需 R 包 ' + pkOk + '/' + pk.length + ' 个可加载。'
      + '有现成的就用现成的，不再下发一套。</div>';
    h += '<div class="idmeta">共 ' + arr(lib.files).length + ' 个脚本，'
      + '覆盖 ' + covers.length + ' 个环节：' + esc(covers.join('、')) + '</div>';
    // 缺哪块活没人干 —— 这是她反复要的那一问（"还有哪些脚本、要不要改"）。
    //   已经覆盖的环节不重复说，只说缺口，并指出缺口由谁补。
    const NEED = ['装包检查', '去双细胞', '合并聚类', 'marker 鉴定', '细胞比例', '差异与富集', '出图', '出 PPT', '打包'];
    const miss = NEED.filter(x => !covers.some(c => String(c).indexOf(x.split(' ')[0]) >= 0 || x.indexOf(String(c)) >= 0));
    if (miss.length) {
      h += '<div class="rzlist" style="margin-top:8px">'
        + '<div class="rz warn"><span class="mk">!</span><span><b>这套脚本没覆盖的活</b> '
        + '<span class="dt">' + esc(miss.join('、'))
        + '　由应用自带脚本补位，见第 ③ 段。</span></span></div></div>';
    } else {
      h += '<div class="rz ok"><span class="mk">✓</span><span><b>没有缺口</b> '
        + '<span class="dt">整条流程都能用服务器这套跑完。</span></span></div>';
    }
    if (arr(lib.files).length) {
      h += '<div class="rzlist" style="margin-top:8px">'
        + arr(lib.files).map(f => '<div class="rz"><span class="mk">·</span><span>'
          + esc(typeof f === 'string' ? f : (f.file + '　' + (f.role || '')))
          + '</span></div>').join('') + '</div>';
    }
  } else if (dec) {
    const pick = arr(dec.checks).find(c => c.id === 'library_pick');
    const exist = arr(dec.checks).find(c => c.id === 'existing_scripts');
    h += '<div class="idpick">' + esc((pick && pick.detail) || dec.verdict_cn || '') + '</div>';
    if (exist) {
      h += '<div class="idmeta">本机现成脚本：' + esc(exist.detail || '') + '</div>';
    }
    const others = arr(dec.checks).filter(c => c.status !== 'ok' && c.id !== 'library_pick');
    if (others.length) {
      h += '<div class="rzlist" style="margin-top:8px">' + others.map(c =>
        '<div class="rz ' + (c.status === 'fail' ? 'fail' : 'warn') + '">'
        + '<span class="mk">' + (c.status === 'fail' ? '×' : '!') + '</span>'
        + '<span><b>' + esc(c.label) + '</b> <span class="dt">' + esc(c.detail) + '</span></span></div>').join('')
        + '</div>';
    }
    const gaps = arr(dec.gaps);
    if (gaps.length) {
      h += '<div class="warnbox" style="margin-top:10px"><b>要你先处理的</b><ul style="margin:6px 0 0">'
        + gaps.map(g => '<li>' + esc(g) + '</li>').join('') + '</ul></div>';
    }
  } else {
    h += '<div class="idmeta">'
      + (S.placeDraft && S.placeDraft.mode === 'server'
        ? '要扫一次才知道用哪套。'
        : '没拿到判定结果。')
      + '</div>';
  }
  h += '</div>';

  // ---- ③ 应用自带的补充脚本 ----
  // 这一段的用途是「补位」，不是把应用带的脚本全铺一遍。
  //   服务器那套脚本（② 段）已经能做的环节，这里就不必再列 ——
  //   列了只会让人觉得"怎么到处都是脚本"，却看不出到底还缺什么。
  //   只列它做不到的（出图、出 PPT 这类），人一眼就知道还差哪块。
  const srvCovers = arr(env && env.script_lib && env.script_lib.covers);
  const stageCovered = st => {
    if (!srvCovers.length) return false;
    const words = arr(st.covers).filter(x => String(x || '').length);
    if (!words.length) return false;
    const blob = srvCovers.join(' ').toLowerCase();
    return words.some(w => blob.indexOf(String(w).toLowerCase()) >= 0);
  };
  const supStages = stages.filter(st => !st.infra && !stageCovered(st));
  const supCount = supStages.reduce((n, st) => n + (st.count || 0), 0);
  let supFlag = 0;
  supStages.forEach(st => arr(st.scripts).forEach(s => {
    const isModule = String(s.role || '').indexOf('子模块') >= 0 || String(s.path || '').indexOf('/lib/') >= 0;
    if (!s.guarded && !isModule) supFlag++;
  }));

  h += '<div class="secblk"><div class="sech">应用自带的补充脚本'
    + '<span class="aside">补服务器做不到的 · ' + (srvCovers.length ? supStages.length : stages.length)
    + ' 个环节 · ' + (srvCovers.length ? supCount : nScript) + ' 个脚本</span></div>';
  if (!stages.length) {
    h += '<div class="idmeta">没有读到脚本清单。</div>';
  } else if (!srvCovers.length && env && !envFailed) {
    // 有服务器那套的结果、却没带回覆盖清单（老版本回写的就是这样）。
    //   这时候不能凭猜过滤 —— 把应用自带的都列出来，并说清为什么没过滤。
    h += '<div class="idmeta">回写没带覆盖清单，先全列在这里。'
      + (nFlag ? nFlag + ' 个没带启动锁。' : '')
      + '</div>';
    h += scriptListHTML(stages);
  } else if (!srvCovers.length) {
    // 服务器那套还没跑出来，无从判断哪些是补位 —— 如实说，不乱列。
    h += '<div class="idmeta">服务器脚本还没识别出来，先不列。</div>';
  } else if (!supStages.length) {
    h += '<div class="rz ok"><span class="mk">✓</span><span>'
      + '<b>服务器那套脚本已经覆盖整个流程</b> '
      + '<span class="dt">服务器上 ' + arr(env.script_lib.files).length
      + ' 个脚本已覆盖，没有要补的。</span></span></div>';
  } else {
    h += '<div class="idmeta">服务器已能跑 ' + esc(srvCovers.join('、'))
      + '；下面补它做不到的：'
      + (supFlag ? supFlag + ' 个没带启动锁。' : '')
      + '</div>';
    h += scriptListHTML(supStages);
  }
  h += '</div>';

  // ---- ④ 应用自带的工具 ----
  h += '<div class="secblk"><div class="sech">应用自带的工具'
    + '<span class="aside">不依赖服务器</span></div>';
  if (tools.length) {
    h += '<div class="idmeta">出图和出 PPT 用这些，不依赖服务器。</div>';
    h += '<div class="rzlist">' + tools.map(t => {
      const ok = !!t.usable;
      return '<div class="rz ' + (ok ? 'ok' : 'warn') + '">'
        + '<span class="mk">' + (ok ? '✓' : '!') + '</span>'
        + '<span><b>' + esc(t.title || t.id) + '</b>'
        + ' <span class="dt">' + esc(t.notes || '') + '</span>'
        + (ok ? '' : ' <span class="dt">（现在取不出来：' + esc(t.reason || arr(t.requires_why).join('；') || '状态不可用') + '）</span>')
        + '</span></div>';
    }).join('') + '</div>';
  } else {
    h += '<div class="idmeta">没读到自带工具清单。</div>';
  }
  h += '</div>';

  h += '</div>';
  host.innerHTML = h;
  // 四个小节渲染完再包成可折叠：正文一个字不改，只换外壳。
  bindIdentFolds(host, IDENT_SECS);
}

/* =====================================================================
   这次新增的四件事：
     1. 右侧导航（点一下跳到对应环节）
     2. 历史记录 + 快照 + 回退
     3. 脚本工作台（看 / 换 / 传 / 审 / 改）
     4. 交给助手的任务队列
   同时把「守卫 / 指纹 / 台账」这类词改成点开就有解释的说法。
   ===================================================================== */

/* ---------------- 术语：鼠标停上去就有解释 ---------------- */
const GLOSS = {
  guard: '脚本开头的一把锁。不是从唯一入口启动的，它就拒绝运行 —— 保证每次运行都被记录在案。',
  md5: '一串由文件内容算出来的字符。内容改一个字，它就完全不同。用来判断文件有没有被改过。',
  ledger: '一本只加不改的记录本。每次运行都追加一条：时间、参数、退出码、产物指纹。',
  whitelist: '一份登记表，写着这条流程允许用哪些脚本、各自指纹是多少。交付前拿实际用的去比对。',
  gate: '必须由人决定的问题。没答完，脚本直接拒绝启动，并告诉你还差哪几件。',
  snapshot: '把当时的设置（参数和答复）存一份，随时可以退回去。',
  stale: '这一步的结果做过，但因为你退回了更早的一步，它现在不算数了，需要重跑。',
};
const GLOSS_WORD = {
  guard: '入口锁', md5: '指纹', ledger: '台账', whitelist: '白名单',
  gate: '闸门', snapshot: '快照', stale: '待重跑',
};
function gterm(key) {
  return '<span class="term" title="' + esc(GLOSS[key] || '') + '">?</span>';
}
function gword(key) {
  return esc(GLOSS_WORD[key] || key) + gterm(key);
}

/* =====================================================================
   一 · 欢迎页：最近用过的运行目录，随时切回来
   ===================================================================== */
function navSections() {
  return $$('#main section.sec[data-nav]').filter(sec => sec.style.display !== 'none');
}

function renderSidenav() {
  const nav = $('#sidenav'); if (!nav) return;
  const secs = navSections();
  if (!secs.length) { nav.className = 'sidenav'; return; }
  let h = '', lastGroup = null;
  secs.forEach(sec => {
    const g = sec.getAttribute('data-navgroup') || '';
    if (g && g !== lastGroup) { h += '<div class="snavgroup">' + esc(g) + '</div>'; lastGroup = g; }
    let cls = 'snavitem';
    if (sec.querySelector('.stcard.failed') || sec.querySelector('.chk.fail')) cls += ' s-err';
    else if (sec.querySelector('.stcard.running')) cls += ' s-warn';
    else if (sec.querySelector('.stcard.done')) cls += ' s-done';
    // title / aria-label 都写上。
    //   窄屏时导航收成只剩圆点，文字被 display:none 藏掉，
    //   那时按钮既看不到名字、读屏软件也读不出它是什么。
    const nm = sec.getAttribute('data-nav');
    h += '<button class="' + cls + '" data-navjump="' + esc(sec.id) + '"'
      + ' title="' + esc(nm) + '" aria-label="' + esc(nm) + '">'
      + '<span class="dot"></span><span class="tx">' + esc(nm) + '</span></button>';
  });
  h += '<div class="snavsep"></div><button class="snavitem" data-navjump="__top"'
    + ' title="回到顶部" aria-label="回到顶部">'
    + '<span class="dot"></span><span class="tx">回到顶部</span></button>';
  nav.innerHTML = h;
  nav.className = 'sidenav on';
  markActiveNav();
}

function jumpTo(id) {
  if (id === '__top') { window.scrollTo({ top: 0, behavior: 'smooth' }); return; }
  const el = document.getElementById(id);
  if (!el) return;
  const y = el.getBoundingClientRect().top + window.pageYOffset - 74;
  window.scrollTo({ top: y, behavior: 'smooth' });
}

function markActiveNav() {
  const nav = $('#sidenav'); if (!nav) return;
  const secs = navSections();
  if (!secs.length) return;
  let best = secs[0].id, bestD = Infinity;
  const mid = window.innerHeight * 0.35;
  secs.forEach(sec => {
    const d = Math.abs(sec.getBoundingClientRect().top - mid);
    if (d < bestD) { bestD = d; best = sec.id; }
  });
  $$('.snavitem', nav).forEach(b => {
    b.classList.toggle('active', b.getAttribute('data-navjump') === best);
  });
}
window.addEventListener('scroll', function () { if (S.runDir) markActiveNav(); }, { passive: true });

/* =====================================================================
   三 · 历史记录与回退
   ===================================================================== */
const STEP_CN = {
  env: '环境体检', decide: '自动判定', precheck: '容量预检', pipeline1: '主流程上段',
  pipeline2: '主流程下段', plots: '出图', ppt: '出 PPT', pack: '打包结果',
};
const stepCN = id => STEP_CN[id] || id;

const STATE_CN = {
  done: ['已完成', 'done'], failed: ['失败', 'failed'], running: ['正在跑', ''],
  blocked: ['被拦下', 'failed'], not_run: ['还没跑', ''], stale: ['待重跑', 'stale'],
};
function stateMeta(s) { return STATE_CN[s] || [s || '', '']; }

async function renderHistory() {
  const host = $('#historyHost'), note = $('#histNote');
  if (!host) return;
  // 这一栏有两层东西，别混：
  //   上面一半 = 这一次分析的存档（本轮新增，按记录存，切记录跟着变）
  //   下面一半 = 流程自己的台账快照与历次运行（一直在的东西，没动）
  // 为什么要分开：存档管的是"这次分析我保存过的几个状态"，台账管的是
  //   "每一步跑没跑成"。合成一个列表会让人分不清退回去的是设置还是运行。
  let arcs = [];
  if (S.sessionId) {
    try {
      const r = await getJSON('/api/sessions/archives?id=' + encodeURIComponent(S.sessionId));
      arcs = arr(r && r.archives);
    } catch (e) { arcs = []; }
  }

  let hh = '<div class="blocklabel">存档</div>';
  if (!arcs.length) {
    hh += '<div class="small">还没有存档。</div>';
  } else {
    hh += '<table class="libtbl"><thead><tr><th>时间</th><th>说明</th><th>含什么</th><th>操作</th></tr></thead><tbody>';
    arcs.slice(0, 20).forEach(a => {
      hh += '<tr><td class="pth">' + esc(String(a.created_at || '').replace('T', ' ')) + '</td>'
        + '<td>' + esc(a.label || a.id) + (a.note ? '<div class="note">' + esc(a.note) + '</div>' : '') + '</td>'
        + '<td>步骤 ' + (a.n_steps || 0) + ' 步</td>'
        + '<td><button class="btn btn-sm" data-arc-restore="'
        + esc(a.id) + '">退回到这里</button></td></tr>';
    });
    hh += '</tbody></table>';
  }
  hh += '<div class="btnrow" style="margin-top:10px">'
    + '<button class="btn" data-arc-snap="1">存一个存档</button></div>';
  hh += '<div style="margin-top:16px;padding-top:14px;border-top:1px solid var(--line-soft)">'
    + '<div class="blocklabel">流程台账</div></div>';

  if (!S.runDir) {
    hh += '<div class="small">还没有运行目录。</div>';
    host.innerHTML = hh;
    return;
  }
  let d;
  try { d = await getJSON('/api/history'); }
  catch (e) { host.innerHTML = hh + '<div class="small">读台账失败：' + esc(e.message) + '</div>'; return; }
  if (!d.ok) { host.innerHTML = hh + '<div class="small">' + esc(d.error || '台账不可用') + '</div>'; return; }
  S.history = d;

  const steps = d.steps || {}, order = arr(d.order);
  const snapshots = arr(d.snapshots), runs = arr(d.runs);
  note.textContent = runs.length ? ('台账 ' + runs.length + ' 次运行') : '还没有运行记录';

  let h = '';
  h += '<div class="blocklabel">走到哪一步</div>';
  h += '<div style="display:flex;flex-wrap:wrap;gap:6px;align-items:center;margin-bottom:6px">';
  order.forEach((id, i) => {
    const st = steps[id] || {};
    const meta = stateMeta(st.state);
    if (i) h += '<span class="chainer">→</span>';
    h += '<button class="chainitem ' + meta[1] + '" data-hist-rerun="' + esc(id) + '" '
      + 'title="' + esc((st.invalidated ? '已作废，需重跑｜' : '') + '上次：' + (st.time || '没跑过')) + '">'
      + '<span class="n">' + (i + 1) + '</span>' + esc(stepCN(id))
      + '<span style="opacity:.75">' + esc(meta[0]) + '</span></button>';
  });
  h += '</div>';

  if (d.mark && d.mark.from_step) {
    h += '<div class="warnbox">已标记：从「' + esc(stepCN(d.mark.from_step)) + '」往后需重跑'
      + (arr(d.mark.will_rerun).length ? '（' + esc(arr(d.mark.will_rerun).map(stepCN).join('、')) + '）' : '')
      + '。产物不删。</div>';
  }

  h += '<div class="blocklabel" style="margin-top:18px">' + gword('snapshot') + '</div>';
  if (!snapshots.length) {
    h += '<div class="small">还没有快照。</div>';
  } else {
    h += '<table class="libtbl"><thead><tr><th>时间</th><th>说明</th><th>含什么</th><th>操作</th></tr></thead><tbody>';
    snapshots.slice(0, 20).forEach(s => {
      h += '<tr><td class="pth">' + esc(s.created_at || '') + '</td>'
        + '<td>' + esc(s.label || s.id) + (s.note ? '<div class="note">' + esc(s.note) + '</div>' : '') + '</td>'
        + '<td>' + (s.has_pipeline ? '参数' : '—') + (s.has_gates ? ' + 答复' : '') + '</td>'
        + '<td><button class="btn btn-sm" data-hist-restore="'
        + esc(s.id) + '">退回到这里</button></td></tr>';
    });
    h += '</tbody></table>';
  }
  h += '<div class="btnrow" style="margin-top:10px">'
    + '<button class="btn" data-hist-snap="1">存一个快照</button>'
    + '<button class="btn" data-hist-refresh="1">刷新</button></div>';

  h += '<div class="blocklabel" style="margin-top:20px">历次运行</div>';
  if (!runs.length) {
    h += '<div class="small">还没有记录。</div>';
  } else {
    runs.slice(0, 12).forEach(r => {
      const when = String(r.started || '').replace('T', ' ');
      const verdict = r.ok
        ? '<span class="histtag" style="background:var(--done-soft);color:var(--done)">成功</span>'
        : (r.has_blocked
          ? '<span class="histtag" style="background:var(--wait-soft);color:var(--wait)">被拦下</span>'
          : '<span class="histtag" style="background:var(--err-soft);color:var(--err)">有失败</span>');
      h += '<div class="histrow"><div class="histwhen">' + esc(when) + '</div><div class="histmain">'
        + '<div>' + verdict + ' <span class="small">' + esc(String(r.run_id || '')) + '</span></div>'
        + '<div class="histsteps">' + arr(r.steps).map(s => {
            const st = steps[s] || {};
            const meta = stateMeta(st.state);
            const cls = st.state === 'done' ? 'done' : (st.state === 'failed' ? 'failed' : (st.state === 'stale' ? 'stale' : ''));
            return '<span class="hstep ' + cls + '">' + esc(stepCN(s)) + ' · ' + esc(meta[0]) + '</span>';
          }).join('') + '</div>'
        + (arr(r.set_ids).length ? '<div class="small" style="margin-top:4px">用到的脚本套：'
            + esc(arr(r.set_ids).join('、')) + '</div>' : '')
        + '</div></div>';
    });
  }

  host.innerHTML = hh + h;

  // 历史读完再刷一次导航与步骤卡片：
  //   renderAll 里这两个是先于历史渲染的，拿不到「待重跑」状态。
  //   不回头刷一次，用户退回后要手动刷新页面才看得到红点。
  if (S.runDir) { renderRun(); renderSidenav(); }
}

async function histSnapshot() {
  try {
    const r = await postJSON('/api/snapshot', { label: 'manual' });
    toast(r.ok ? '已存快照：' + r.id : ('存不了：' + (r.error || '')), r.ok ? 'good' : 'bad');
    renderHistory();
  } catch (e) { toast('存快照失败：' + e.message, 'bad'); }
}

/* 这次分析的存档：整份记下来（识别结论、填的位置、步骤状态）。
   与流程台账的快照不是一回事 —— 那份记的是 pipeline.json / gates.json。 */
async function archiveNow(label, note) {
  if (!S.sessionId) { toast('还没有分析记录。', 'bad'); return null; }
  try {
    const r = await postJSON('/api/sessions/archive', {
      id: S.sessionId,
      label: label || 'manual',
      note: note || '',
    });
    if (!r || !r.ok) { toast('存档失败：' + ((r && r.error) || '未知原因'), 'bad'); return null; }
    if (!label || label === 'manual') toast('已存档：' + r.archive.id, 'good');
    // 存完立刻把列表刷出来。
    //   不刷的话，用户点完按钮看不到任何变化（列表里还是"还没有存档"），
    //   会以为没存上、再点一次，于是重复存。
    renderHistory();
    renderSessionsBar();
    return r.archive;
  } catch (e) { toast('存档失败：' + e.message, 'bad'); return null; }
}

async function archiveRestore(id) {
  if (!confirm('退回到 ' + id + '？\n当前状态会先存一份，产物不删。')) return;
  try {
    const r = await postJSON('/api/sessions/restore', { id: S.sessionId, archive: id });
    if (!r || !r.ok) { toast('退不了：' + ((r && r.error) || '未知原因'), 'bad'); return; }
    await loadSessionsState();
    setStageVisible(S.identified);
    renderAll();
    renderSessionsBar();
    renderHistory();
    toast('已退回，当前状态另存为 ' + (r.kept || ''), 'good');
  } catch (e) { toast('退不回去：' + e.message, 'bad'); }
}

async function histRestore(id) {
  if (!confirm('退回快照 ' + id + '？\n当前状态会先存一份，产物不删。')) return;
  try {
    const r = await postJSON('/api/rollback', { mode: 'restore', snapshot: id });
    if (r.ok) { toast('已退回。', 'good'); await loadRun(); }
    else toast('退不了：' + (r.error || ''), 'bad');
  } catch (e) { toast('回退失败：' + e.message, 'bad'); }
}

async function histMarkRerun(step) {
  const idx = arr(S.history && S.history.order).indexOf(step);
  const after = idx >= 0 ? arr(S.history.order).slice(idx).map(stepCN).join(' → ') : stepCN(step);
  if (!confirm('标记从「' + stepCN(step) + '」往后重跑？\n' + after + '\n产物不删。')) return;
  try {
    const r = await postJSON('/api/rollback', { mode: 'rerun', from_step: step });
    if (r.ok) {
      toast('已标记：从「' + stepCN(step) + '」往后重跑。', 'good');
      renderHistory(); renderRun();
    } else toast('标记失败：' + (r.error || ''), 'bad');
  } catch (e) { toast('标记失败：' + e.message, 'bad'); }
}

/* =====================================================================
   脚本区：按环节折叠，每个脚本四个动作
   ---------------------------------------------------------------------
   为什么改成折叠 + 行内四按钮：
   之前把清单、解释、换一份、改一处全部平铺在一块，还带一大段说明文字。
   用户的实际反馈是「这个脚本工作台是干啥用的啊」——问题不在功能，
   在于它一上来就把所有东西摆出来，没有入口的层次。
   现在：默认只看到环节名和数量，点开才看脚本；
   每个脚本旁边就是能对它做的事，不用先读一段说明才知道哪个按钮管什么。
   ===================================================================== */
const CHECK_CN = {
  in_inventory: '在流程的脚本清单里',
  guard: '带有启动锁（不能被绕过单独运行）',
  ascii: '源码是纯英文（旧版 R + 中文 Windows 上才解析得了）',
  reads_config: '参数从配置文件读，没有写死在代码里',
  hardcoded: '没有明显的写死阈值',
  stop_on_fail: '出错就停，不会带着错往下算',
  writes_outdir: '结果写到输出目录',
  declares_stage: '头部写清了属于哪一段',
  parses: '代码语法正确，能正常解析',
};
const CHECK_MK = { pass: ['✓', 'pass'], warn: ['!', 'warn'], fail: ['✗', 'fail'], skip: ['–', 'skip'] };

function checksHTML(r) {
  let h = '';
  if (r.verdict === 'pass') {
    h += '<div class="pickbox ok"><span class="ic">✓</span><span class="tx"><b>可以用</b></span></div>';
  } else if (r.verdict === 'warn') {
    h += '<div class="pickbox warn"><span class="ic">!</span><span class="tx"><b>大体能用，有几点要注意</b></span></div>';
  } else {
    h += '<div class="pickbox no"><span class="ic">✗</span><span class="tx"><b>现在还不能用</b></span></div>';
  }
  h += '<div class="chklist">';
  arr(r.checks).forEach(c => {
    const mk = CHECK_MK[c.status] || ['?', 'skip'];
    h += '<div class="chk ' + mk[1] + '"><span class="mk">' + mk[0] + '</span><span>'
      + esc(CHECK_CN[c.id] || c.id)
      + (c.evidence ? ' <span class="ev">' + esc(c.evidence) + '</span>' : '')
      + '</span></div>';
  });
  h += '</div>';
  return h;
}

/* 一句话说清这个脚本能不能直接用。
   不再用「要看一眼」这种含糊说法 —— 用户原话是「我也看不懂什么叫要看一眼」。 */
function scriptVerdict(s) {
  if (s.guarded) return '<span class="flagline ok">可以用</span>';
  return '<span class="flagline no">建议先检查</span>';
}

async function renderStageScripts() {
  // 只负责取数据。渲染在 renderIdentify() 的第三段里做 ——
  //   用户明确指出：脚本明细应当属于识别结果，不该单独吊在下面。
  const host = $('#scriptsHost');
  let d;
  try { d = await getJSON('/api/stage-scripts'); }
  catch (e) { S.stages = []; if (host) host.innerHTML = ''; return; }
  S.stages = (d && d.ok) ? arr(d.stages) : [];
  if (host) host.innerHTML = '';   // 旧容器不再使用
}

/* 脚本明细的 HTML（供识别结论第三段内联使用）
   list：要渲染的环节数组。不传就用全部环节。
     过滤放在调用处做 —— 这一段现在的语义是「补充脚本」，
     该显示哪几个环节是调用处按服务器脚本库的覆盖情况算出来的。 */
function scriptListHTML(list) {
  let h = '';
  arr(list === undefined ? S.stages : list).forEach(function (st) {
    h += '<details class="stgfold"><summary>'
      + '<span class="stgname">' + esc(st.name) + '</span>'
      + '<span class="skilltag" title="这一环节由这个 skill 负责">skill · ' + esc(st.skill) + '</span>'
      + '<span class="cnt">' + st.count + ' 个脚本</span>'
      + '</summary>';
    if (st.desc) h += '<div class="stgdesc">' + esc(st.desc) + '</div>';
    if (!st.count) {
      h += '<div class="stgempty">这一环节没找到脚本。</div>';
    } else {
      arr(st.scripts).forEach(function (sp) {
        h += '<div class="scriptrow">'
          + '<div class="sn"><b>' + esc(sp.name) + '</b>'
          + (sp.role ? '<span class="srole">' + esc(sp.role) + '</span>' : '')
          + '<div class="p">' + esc(sp.path) + '</div></div>'
          + scriptVerdict(sp)
          + '<div class="acts">'
          + '<button class="btn" data-sw-act="open" data-rel="' + esc(sp.path) + '">打开</button>'
          + '<button class="btn" data-sw-act="check" data-rel="' + esc(sp.path) + '">检查</button>'
          + '<button class="btn" data-sw-act="change" data-rel="' + esc(sp.path) + '">修改</button>'
          + '<button class="btn" data-sw-act="upload" data-rel="' + esc(sp.path) + '">上传</button>'
          + '</div></div>';
      });
    }
    h += '</details>';
  });
  return h;
}

/* ---------------- 动作一：打开（看内容） ---------------- */
async function actOpen(rel) {
  S.curScript = rel;
  const t = $('#scriptTitle'); if (t) t.textContent = rel.split('/').pop();
  const b = $('#scriptBody');
  if (b) b.innerHTML = '<div class="small">正在读…</div>';
  openDlg('dlgScript');
  try {
    const d = await getJSON('/api/script-read?rel=' + encodeURIComponent(rel));
    if (!d.ok) { if (b) b.innerHTML = '<div class="small">读不到：' + esc(d.error || '') + '</div>'; return; }
    S.curScriptInfo = d;
    let h = '<div class="small" style="margin-bottom:10px">' + esc(rel)
      + '　' + fmtSize(d.size) + '　修改时间 ' + esc(d.mtime || '') + '</div>'
      + '<pre class="scriptcode">' + esc(d.content || '') + '</pre>';
    const bks = arr(d.backups);
    if (bks.length) {
      h += '<div class="blocklabel" style="margin-top:16px">历史版本</div>'
        + '<table class="libtbl"><thead><tr><th>时间</th><th>指纹</th><th>操作</th></tr></thead><tbody>'
        + bks.slice(0, 6).map(x => '<tr><td class="pth">' + esc(x.at) + '</td>'
          + '<td class="pth">' + esc(String(x.md5 || '').slice(0, 10)) + '</td>'
          + '<td>' + (x.legacy
              ? '<span class="small">早期存档，无法还原</span>'
              : '<button class="btn" data-sw-restore="' + esc(x.id) + '">还原</button>')
          + '</td></tr>').join('') + '</tbody></table>';
    }
    if (b) b.innerHTML = h;
  } catch (e) { if (b) b.innerHTML = '<div class="small">读不到：' + esc(e.message) + '</div>'; }
}

/* ---------------- 动作二：检查 ----------------
   机械检查在前端能立刻拿到；要读懂代码的判断交给助手。 */
async function actCheck(rel) {
  S.curScript = rel;
  const t = $('#checkTitle'); if (t) t.textContent = '检查 · ' + rel.split('/').pop();
  const b = $('#checkBody');
  if (b) b.innerHTML = '<div class="small">正在检查…</div>';
  openDlg('dlgCheck');
  try {
    const r = await getJSON('/api/script-review?rel=' + encodeURIComponent(rel));
    if (!r.ok) { if (b) b.innerHTML = '<div class="small">检查失败：' + esc(r.error || '') + '</div>'; return; }
    let h = checksHTML(r);
    h += '<div style="margin-top:14px;padding-top:14px;border-top:1px solid var(--line-soft)">'
      + '<div class="small" style="margin-bottom:8px">是否能判的已在上面。'
      + '方法层面要读代码，交给助手：</div>'
      + '<div class="btnrow"><button class="btn primary" data-sw-ask="1">交给助手审阅</button>'
      + '<span id="askMsg" class="small"></span></div></div>';
    if (b) b.innerHTML = h;
  } catch (e) { if (b) b.innerHTML = '<div class="small">检查失败：' + esc(e.message) + '</div>'; }
}

/* ---------------- 动作三：修改（换一份 或 提改动要求） ---------------- */
function actChange(rel) {
  S.curScript = rel;
  const t = $('#changeTitle'); if (t) t.textContent = '修改 · ' + rel.split('/').pop();
  const b = $('#changeBody');
  if (b) b.innerHTML =
      '<div class="small" style="margin-bottom:12px">' + esc(rel) + '</div>'
    + '<div class="blocklabel">一、传新版本替换</div>'
    + '<input type="file" id="chgFile" class="filein">'
    + '<div class="btnrow"><button class="btn" data-sw-replace="1">用这份替换</button>'
    + '<span class="small">替换前自动备份。</span></div>'
    + '<div style="height:18px"></div>'
    + '<div class="blocklabel">二、写要求交给助手改</div>'
    + '<textarea class="idea" id="chgIdea" placeholder="例如：质控下限改成从配置读"></textarea>'
    + '<div class="btnrow" style="margin-top:8px">'
    + '<button class="btn primary" data-sw-change="1">把这条改动要求交给助手</button>'
    + '<span id="chgMsg" class="small"></span></div>';
  openDlg('dlgChange');
}

/* ---------------- 动作四：上传（传完自动送审） ---------------- */
function actUpload(rel) {
  S.curScript = rel;
  const t = $('#uploadTitle'); if (t) t.textContent = '上传 · ' + rel.split('/').pop();
  const b = $('#uploadBody');
  if (b) b.innerHTML =
      '<div class="small" style="margin-bottom:12px">'
      + '上传会替换 ' + esc(rel) + '，并自动送审。</div>'
    + '<input type="file" id="upFile" class="filein">'
    + '<div class="btnrow"><button class="btn primary" data-sw-uploadexec="1">上传并送审</button>'
    + '<span id="upMsg" class="small"></span></div>'
    + '<div id="upResult" style="margin-top:14px"></div>';
  openDlg('dlgUpload');
}

/* 执行「上传并送审」：先替换，再立刻跑一次检查并把结论直接显示出来。
   用户不需要自己去点「检查」——上传这个动作本身就包含审核。 */
async function doUploadAndReview() {
  const inp = $('#upFile');
  const f = inp && inp.files && inp.files[0];
  const msg = $('#upMsg'), out = $('#upResult');
  if (!f) { toast('先选一个文件。', 'bad'); return; }
  if (f.size > 2 * 1024 * 1024) { toast('文件太大（上限 2 MB）。', 'bad'); return; }
  if (msg) msg.textContent = '正在上传…';
  try {
    const content = await f.text();
    const up = await postJSON('/api/script-upload', { name: f.name, content: content });
    if (!up.ok) { if (msg) msg.textContent = '上传失败：' + (up.error || ''); return; }
    if (msg) msg.textContent = '正在替换并检查…';
    const put = await postJSON('/api/incoming', { action: 'adopt', id: up.id, rel: S.curScript, mode: 'replace' });
    if (!put.ok) { if (msg) msg.textContent = '替换失败：' + (put.error || ''); return; }
    if (msg) msg.textContent = '已替换，检查结论如下';
    const rv = await getJSON('/api/script-review?rel=' + encodeURIComponent(S.curScript));
    if (out) out.innerHTML = rv.ok ? checksHTML(rv)
      : '<div class="small">检查失败：' + esc(rv.error || '') + '</div>';
    renderStageScripts();
  } catch (e) { if (msg) msg.textContent = '失败：' + e.message; }
}

/* 用文件直接替换（修改弹窗的方式一） */
async function doReplaceWithFile() {
  const inp = $('#chgFile');
  const f = inp && inp.files && inp.files[0];
  if (!f) { toast('先选一个文件。', 'bad'); return; }
  if (f.size > 2 * 1024 * 1024) { toast('文件太大（上限 2 MB）。', 'bad'); return; }
  if (!confirm('用这份替换 ' + S.curScript + '？\n\n现在这份会自动备份，之后可以还原。')) return;
  try {
    const content = await f.text();
    const up = await postJSON('/api/script-upload', { name: f.name, content: content });
    if (!up.ok) { toast('上传失败：' + (up.error || ''), 'bad'); return; }
    const put = await postJSON('/api/incoming', { action: 'adopt', id: up.id, rel: S.curScript, mode: 'replace' });
    if (!put.ok) { toast('替换失败：' + (put.error || ''), 'bad'); return; }
    toast('已替换。旧版本已备份。', 'good');
    closeDlg('dlgChange');
    renderStageScripts();
    actCheck(S.curScript);
  } catch (e) { toast('替换失败：' + e.message, 'bad'); }
}

async function submitChange() {
  const ta = $('#chgIdea'); const idea = ta ? ta.value.trim() : '';
  const msg = $('#chgMsg');
  if (!idea) { toast('先写下想改什么。', 'bad'); return; }
  try {
    const r = await postJSON('/api/script-change', { rel: S.curScript, idea: idea });
    if (r.ok) {
      if (msg) msg.textContent = '已交给助手。';
      toast('已交给助手。', 'good');
      closeDlg('dlgChange');
      renderTaskLog();
    } else { if (msg) msg.textContent = '提交失败：' + (r.error || ''); }
  } catch (e) { if (msg) msg.textContent = '提交失败：' + e.message; }
}

async function askReview() {
  const msg = document.getElementById('askMsg');
  if (msg) msg.textContent = '正在提交…';
  try {
    const r = await postJSON('/api/script-ask', { rel: S.curScript, question: S.lastIdea || '' });
    if (r.ok) {
      if (msg) msg.textContent = '已交给助手，结论见任务记录。';
      toast('已交给助手审阅。', 'good');
      renderTaskLog();
    } else if (msg) msg.textContent = '提交失败：' + (r.error || '');
  } catch (e) { if (msg) msg.textContent = '提交失败：' + e.message; }
}

async function restoreBackup(id) {
  if (!confirm('还原成这一版？\n\n当前这份会先备份。')) return;
  try {
    const r = await postJSON('/api/script-restore', { backup: id });
    if (r.ok) { toast('已还原。', 'good'); closeDlg('dlgScript'); renderStageScripts(); }
    else toast('还原失败：' + (r.error || ''), 'bad');
  } catch (e) { toast('还原失败：' + e.message, 'bad'); }
}

/* =====================================================================
   交给助手做的事：以任务记录的形式挂在脚本区下面
   ---------------------------------------------------------------------
   之前它自己占一个区块，叫「交给助手的任务」——用户的原话是
   「这个又是什么东西啊」。它其实是脚本动作的副产品，放在动作旁边
   才讲得通；没任务时整块不出现。
   ===================================================================== */
const TASK_CN = {
  script_review: '脚本审阅', script_change: '脚本改动',
  review: '环节质检', stage: '跑某一环节',
};
const TASK_ST_CN = { pending: '等助手处理', running: '处理中', done: '已完成', failed: '失败' };

/* 把一段文本注入会话。
   这个应用以「隧道发布」挂到公网，网关会往页面注入 SDK
   （/_catsco/runtime/annotations-v1.js，挂成 window.CatsCoAnnotations）。
   它的语义就是：这段话作为一条消息出现在对话里，助手看得到。

   读源码得来的三条硬事实，写在这里免得下次又猜：
     1. 宿主连接是异步的，instance() 一开始返回 null，要等。
     2. 只有应用被嵌在会话里打开时才有宿主；独立窗口、直接开网址都没有。
     3. proposeCompose 的返回值只说明"有没有交出去"，
        真正成败要看 onComposeAck 的回执。 */
function sessionChannel() {
  const api = window.CatsCoAnnotations;
  if (!api || typeof api.instance !== 'function') return { state: 'no_sdk', api: null };

  // 读 SDK 源码得来的判据：它只在页面被嵌进另一个窗口时才挂握手监听
  //   （源码里那句 window.parent !== window 就是这道门槛）。
  //   所以"不在会话框里"必须先判出来 —— 否则页面会一直显示"正在连接"，
  //   让人干等一个永远不会来的握手。
  let inFrame = false;
  try { inFrame = window.parent !== window; } catch (e) { inFrame = true; }

  let inst = null;
  try { inst = api.instance(); } catch (e) { inst = null; }
  if (inst) return { state: 'connected', api: api, inst: inst, inFrame: inFrame };
  if (!inFrame) return { state: 'standalone', api: api, inst: null, inFrame: false };
  return { state: 'waiting', api: api, inst: null, inFrame: true };
}

/* 第二条路：平台任务通道（window.catscoArtifactHost）。
   为什么要有两条：compose 那条要宿主先发 happened 握手，而握手不来是
   平台侧的事，本机改不动。任务通道是平台给「页面点一下 → 变成一次真实
   会话」的正规入口，它自己会向宿主报到（bridge.ready），连上与否能直接问。
   两条都试，谁通走谁；两条都不通，就把指令存进本机队列，绝不白点。 */
/* 任务通道到底连上没有 —— 只认宿主主动发来的握手。
   ------------------------------------------------------------------
   为什么不能再用 isConnected()：
     bridge 的 handleMessage 只要"消息来自父页 + origin 在白名单"就会把
     connectedParentOrigin 置位，不管那条消息是什么类型。而网关发布的页面
     本来就会和父页来回发 annotations 的消息，于是 isConnected() 恒为 true。
   结果就是：任务请求交给了一个根本不收它的宿主，15 秒后超时
   （Artifact task request timed out），本该可用的注入通道还被它抢先绕开。
   所以这里改成自己听握手：只有真收到 catsco.artifact.host.connect.v1，
   才认为任务通道可用；听不到就老老实实走注入。 */
let taskHostHandshakeAt = 0;
try {
  window.addEventListener('message', function (ev) {
    let isParent = false;
    try { isParent = (ev.source === window.parent) && (window.parent !== window); } catch (e) { isParent = false; }
    if (!isParent) return;
    const d = ev.data;
    if (d && typeof d === 'object' && d.type === 'catsco.artifact.host.connect.v1') {
      taskHostHandshakeAt = Date.now();
    }
  });
} catch (e) {}

function hostBridge() {
  const h = window.catscoArtifactHost;
  if (!h || typeof h.requestTask !== 'function') return null;
  // 这里刻意不用 h.isConnected()：在这套发布方式下它是假阳性。
  return { api: h, connected: taskHostHandshakeAt > 0 };
}

// 任务通道送一次。用户激活只有 2 秒有效期，所以这个函数要贴着点击调用。
function submitAsTask(api, text) {
  // 任务通道的载荷有长度上限（manifest 里 input_schema.text.maxLength = 1000）。
  //   以前是「先切 950 再接一句队列说明」，接上的那句把总长顶到 1000 以上，
  //   平台判 invalid_payload —— 于是整条被拒，两条通道全废，指令只能落队列。
  //   现在先算好说明文字，剩下的额度再留给正文；总长硬顶在 950 以内。
  const LIMIT = 950;
  const full = String(text || '');
  const prep = full.length > LIMIT
    ? saveOutbox(full).then(function (o) {
        const note = '\n（全文已存队列 id=' + ((o && o.id) || '未知') + '，可从 /api/outbox 取）';
        const body = full.slice(0, Math.max(0, LIMIT - note.length));
        return body + note;
      })
    : Promise.resolve(full);

  return prep.then(function (payloadText) {
    let payload;
    try {
      payload = { text: payloadText };
      const mode = (S.placeDraft && S.placeDraft.mode) || 'server';
      if (mode === 'server') payload.mode = 'server';
    } catch (e) { payload = { text: payloadText }; }
    return api.requestTask('assistant.connect.v1', payload).then(function (t) {
      return { ok: true, via: 'task', code: 'accepted', task: t || null };
    });
  }).catch(function (e) {
    const code = (e && e.code) || 'task_failed';
    const why = code === 'user_activation_required'
      ? '任务通道要新鲜点击，再点一次。'
      : (code === 'host_not_connected'
        ? '任务通道还没连上。'
        : ('平台任务通道没收下：' + ((e && e.message) || '未知原因')));
    return { ok: false, via: 'task', code: code, why: why };
  });
}

/* 两条各有脾气的通道，谁通走谁：
     ① 平台任务通道（window.catscoArtifactHost.requestTask）
        —— 平台给「页面点一下 → 变成一次真实会话」的正规入口。
        它不等宿主先开口，页面报到之后可以自己问「连上没有」；
        代价是用户激活只有 2 秒，必须贴着这次点击调用。
     ② compose 通道（window.CatsCoAnnotations）
        —— 老路，要等宿主先发握手。宿主不发，本机改不动；
        而且回执可能永远不来，所以必须自带超时。
   两条都不通时不留死路：指令存进本机待发送队列，页面照常可用，
   人可以一键复制，也可以让我下次直接从队列里取。 */
function saveOutbox(text) {
  // 带上会话号：队列里的指令也是"从某个会话点出来的"，
  //   取出来重发时要能回到同一个会话，否则又变成不知道送去哪儿。
  return postJSON('/api/outbox', { text: String(text || ''), session: S.topicId || '' })
    .catch(function (e) { return { ok: false, error: String(e && e.message || e) }; });
}

function composeSend(inst, text, finish) {
  let ackTimer = null;
  try {
    inst.onComposeAck(function (ack) {
      if (ackTimer) { clearTimeout(ackTimer); ackTimer = null; }
      finish(ack && ack.accepted
        ? { ok: true, via: 'compose' }
        : { ok: false, via: 'compose', code: (ack && ack.reason) || 'unknown',
            why: '平台没收下：' + ((ack && ack.reason) || '未知原因') + '。' });
    });
  } catch (e) {}
  let sent = false;
  try { sent = inst.proposeCompose({ text: text }); } catch (e) { sent = false; }
  if (!sent) {
    if (ackTimer) { clearTimeout(ackTimer); ackTimer = null; }
    finish({ ok: false, via: 'compose', code: 'send_failed', why: '交给平台时没成功。' });
    return;
  }
  ackTimer = setTimeout(function () {
    ackTimer = null;
    finish({ ok: false, via: 'compose', code: 'ack_timeout',
      why: '12 秒没回执，通道可能已断。' });
  }, 12000);
}

/* 把一段文本注入会话。
   ---------------------------------------------------------------------
   现在这件事交给应用外壳（app-shell.js）统一做，这里只做一层薄适配：
     通道判定、失败入队、每小时复查补发、回写销项，全在外壳里。
   为什么收上去：以前这里自己判"连上没有"、自己决定失败后怎么办，
   判据和外壳不一致（任务通道 isConnected() 恒为真那次，就把本来能用的
   注入通道整个绕过去了）；而且送失败之后只剩"请自己复制"。
   调用点不用改：签名与返回结构保持原样。 */
function injectToSession(text, onState) {
  const shell = window.AppShell;
  if (!shell || typeof shell.inject !== 'function') {
    // 外壳没加载成功时不假装能发：如实报，并交给本机队列兜底。
    return saveOutbox(text).then(function (o) {
      return { ok: false, code: 'shell_missing', queued: !!(o && o.ok),
        why: '注入外壳没加载，指令已存本机队列。' };
    });
  }
  if (onState) onState('sending');
  return shell.inject(text, {
    session: S.topicId || '',
    reqId: S.commitReq || '',
    step: (S.delegated && S.delegated.step) || '',
    mode: (S.placeDraft && S.placeDraft.mode) || 'server',
    intent: 'assistant.connect.v1'
  }).then(function (r) {
    if (onState) onState(r && r.ok ? 'connected' : 'waiting');
    // ok=true 只代表"交给宿主了"，真正做成要看回写 —— 外壳会盯着这一条。
    if (r && r.ok) return { ok: true, via: r.via || 'shell', code: r.code || 'accepted' };
    return {
      ok: false,
      via: (r && r.via) || 'shell',
      code: (r && r.code) || 'not_connected',
      queued: !!(r && r.queued),
      id: (r && r.queueId) || '',
      why: (r && r.why) || '两条通道都没连上。'
    };
  });
}

/* =====================================================================
   把「要做什么」和上面填的服务器信息拼成一段文本
   ---------------------------------------------------------------------
   这段文本会整段原样作为一条消息进对话，所以只放助手真需要的东西：
   要做的事 + 目标机器 + 几个目录。界面上那些说明文案一律不带。
   两条硬规矩：
     · 口令绝不写进文本。它由应用在本机保管，这里只留一句"从本机配置读"。
     · 平台对载荷有长度上限，超了会被拒；这里做一次上限保护。
   ===================================================================== */
function buildInstructionText(ask) {
  const d = S.placeDraft || placeDraftFromServer();
  const MAX = 2000;
  const L = [];

  L.push('【来自 个性化分析助手】');
  // 会话号写在最前面：这条指令是从哪个会话点出来的，助手照它把结果送回去。
  //   以前没有这一行，助手只能落在"应用归属 bot 的默认会话"里 ——
  //   用户在自己那个会话里点了按钮，却看不到回音，看着就是没反应。
  if (S.topicId) {
    L.push('本次对话会话号：' + S.topicId
      + '（就是你现在收到这条消息的这个会话，结果请送回这里）');
  }
  L.push('要办的事：' + String(ask == null ? '' : ask).trim());

  if ((d.mode || 'local') === 'server') {
    L.push('');
    L.push('目标服务器：' + (d.host || '（主机未填）') + ':' + (d.port || 22));
    L.push('登录账号：' + (d.user || '（账号未填）'));
    // 执行电脑写在最前面第二行，因为它决定了"这条指令在哪台机器上跑"。
    //   以前这里只有一句「在能连上这台服务器的电脑上执行」，执行端看不到
    //   会话选中的是哪一台，只能挨台试 —— 试到连不上的那台就得把连接超时
    //   等完（实测白等 23 秒）。第一次识别成功后应用会自动把名字填上。
    if (d.exec_pc) {
      L.push('执行电脑：' + d.exec_pc + '（就在这台电脑上执行，不必再试别的机器）');
    } else {
      L.push('执行电脑：未记录。请在能连上这台服务器的电脑上执行；'
        + '跑通一次后应用会把它的名字记下来，下次直接指名。');
    }
    if (d.data_root) L.push('数据目录：' + d.data_root);
    if (d.script_root) L.push('脚本目录：' + d.script_root);
    if (d.work_root) L.push('结果目录：' + d.work_root);
    if (d.rscript) L.push('R 程序：' + d.rscript);
    if (d.queue) L.push('队列：' + d.queue);
    if (d.node_constraint) L.push('节点约束：' + d.node_constraint);
    L.push('');
    // 这一节是给执行端的做法说明，不是客套话。
    //   要解决的是「每次都得先探一遍路」这件事：应用自带的探针脚本已经把
    //   取哪些项、怎么判、输出什么形状都定好了，照着跑就是一次 SSH 取全。
    //   所以这里明确写清：用哪个脚本、哪些参数、不要先做别的探测。
    L.push('怎么做（照这个来，不要自己另写探路的步骤）：');
    L.push('1. 登录口令在本机应用的 config/server_profile.json 里，'
      + '从本机读取，不要写进对话。');
    L.push('2. 应用自带探针 engine/workpc_check.py（与 step_plan 同一目录）。'
      + '在能连上这台服务器的电脑上跑它一次，一条 SSH 取全下列各项：'
      + '内存 / CPU / /share 剩余 / R 版本 / 10 个必需 R 包 / 数据目录逐个符号链接'
      + ' / 脚本目录与每个脚本的作用 / 结果目录可写 / 队列与节点约束。');
    L.push('3. 参数按上面那几个目录与 R 程序填；脚本目录填上面「脚本目录」那一项。'
      + '探针在这一层没看到脚本时会自己下钻到 script 子目录，'
      + '所以你照给的路径填即可，不需要先去看目录结构。');
    L.push('4. 不要先做任何探路：不要试哪台机器能连、不要在任何机器上找应用路径、'
      + '不要 ls 目录看结构。直接按第 2 步跑探针；连不上就把这条当作结果写回，'
      + '不要换机器重试。');
    L.push('5. 把脚本输出的 JSON 回写应用：POST /api/envcheck/import，'
      + 'body 里放 result（那份 JSON）、host、session_id（本次分析记录号）。');
    if (S.commitReq) {
      // 请求号：助手池完写回结果时，原样带回这个号。
      //   没有它，应用分不清「写回来的是这次要的结果」还是上一轮的旧数据，
      //   只能一律显示 —— 那就等于刚点完按钮就冒出结论。
      L.push('');
      L.push('本次请求号：' + S.commitReq
        + '（写回体检结果时请在 body 里带上 req_id，值就是这一串）');
    }
    if (S.sessionId) {
      L.push('本次分析记录号：' + S.sessionId
        + '（写回时请带上 session_id，结果会挂在这一份分析下）');
    }
  } else {
    L.push('');
    L.push('运行位置：本机');
    if (d.local_path) L.push('工作路径：' + d.local_path);
  }

  let text = L.join('\n');
  if (text.length > MAX) text = text.slice(0, MAX) + '\n…（文本过长已截断）';
  return text;
}

async function renderTaskLog() {
  const host = $('#taskLogHost'); if (!host) return;
  let d;
  try { d = await getJSON('/api/queue'); } catch (e) { host.innerHTML = ''; return; }
  const ts = arr(d.tasks).filter(t => ['script_review', 'script_change'].indexOf(t.kind) >= 0);
  if (!ts.length) { host.innerHTML = ''; return; }
  let h = '<div class="pl-card"><h3>交给助手的事</h3>'
    + '<table class="libtbl"><thead><tr><th>时间</th><th>类型</th><th>对象</th><th>状态</th></tr></thead><tbody>';
  ts.slice(0, 10).forEach(t => {
    h += '<tr><td class="pth">' + esc(String(t.created_at || '').replace('T', ' ')) + '</td>'
      + '<td>' + esc(TASK_CN[t.kind] || t.kind || '') + '</td>'
      + '<td class="pth">' + esc(t.rel || '')
      + (t.idea ? '<div class="note">' + esc(t.idea) + '</div>' : '') + '</td>'
      + '<td>' + esc(TASK_ST_CN[t.status] || t.status || '') + '</td></tr>';
  });
  h += '</tbody></table></div>';
  host.innerHTML = h;
}

/* =====================================================================
   环节质检：改动前后的脚本对照（原有 reviewer 通道保留）
   ===================================================================== */
document.addEventListener('click', async function (ev) {
  const rb = ev.target.closest('button[data-review]');
  if (!rb) return;
  const stage = rb.getAttribute('data-review');
  rb.disabled = true;
  try {
    const r = await postJSON('/api/review', { stage: stage });
    const box = $('#checkBody');
    if (box) {
      box.innerHTML = '<div class="pickbox ' + (r && r.ok ? 'ok' : 'no') + '"><span class="ic">'
        + (r && r.ok ? '✓' : '×') + '</span><span class="tx"><b>'
        + (r && r.ok ? '质检通过' : '质检未通过') + '</b></span></div>'
        + '<pre class="scriptcode">' + esc((r && r.output) || (r && r.error) || '无输出') + '</pre>';
    }
    toast((r && r.ok ? '质检通过：' : '质检未通过：') + stage, r && r.ok ? 'good' : 'bad');
  } catch (e) {
    toast('质检失败：' + e.message, 'bad');
  } finally { rb.disabled = false; }
});

/* 交给助手执行某一环节 */
document.addEventListener('click', async function (ev) {
  const b = ev.target.closest('button[data-stage]');
  if (!b) return;
  const stage = b.getAttribute('data-stage'), skill = b.getAttribute('data-skill');
  b.disabled = true; b.textContent = '已提交';
  try {
    const r = await postJSON('/api/task', { stage: stage, skill: skill });
    if (r && r.ok) {
      toast('任务已生成：' + stage + '。助手会按流程逐步执行。', 'good');
      renderTaskLog();
    } else toast('生成失败：' + ((r && r.error) || '未知原因'), 'bad');
  } catch (e) { toast('生成失败：' + e.message, 'bad'); }
});

/* =====================================================================
   事件委托
   ---------------------------------------------------------------------
   所有按钮都用 data-* 标记、在这里统一接。
   为什么不用每个按钮各自 addEventListener：这一页会频繁重渲染，
   每次重渲染都要重新绑一遍，漏绑一个就变成「点了没反应」——
   而且这种 bug 在代码里看不出来，只能点出来。
   ===================================================================== */
document.addEventListener('click', async function (ev) {
  const t = ev.target;

  const nav = t.closest('[data-navjump]');
  if (nav) { jumpTo(nav.getAttribute('data-navjump')); return; }

  /* ---- 第一步：位置 ---- */
  const pm = t.closest('[data-plmode]');
  if (pm) {
    S.placeDraft = S.placeDraft || placeDraftFromServer();
    S.placeDraft.mode = pm.getAttribute('data-plmode');
    renderPlace();
    return;
  }
  if (t.closest('[data-plbrowse]')) { openPathPicker(); return; }
  if (t.closest('[data-plcommit]')) { commitPlace(); return; }
  /* ---- 识别结论：等助手那段时间的三个动作 ---- */
  if (t.closest('[data-idrecheck]')) { recheckIdentify(); return; }
  if (t.closest('[data-idreinject]')) {
    S.identifyPhase = 'waiting';
    renderAll();
    autoInjectIdentify();
    startWaitWriteback();
    return;
  }
  const idc = t.closest('[data-idcopy]');
  if (idc) {
    const box = document.querySelector('[data-idraw]');
    const note = $('#idCopyMsg');
    const txt = box ? box.value : '';
    const done = function (x) { if (note) note.textContent = x; };
    if (!txt) { done('没有可复制的内容。'); return; }
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(txt).then(function () { done('已复制，粘到对话里就行。'); },
        function () { if (box) box.select(); done('复制失败，上面的文字已选中，按 Ctrl+C。'); });
    } else if (box) { box.select(); done('上面的文字已选中，按 Ctrl+C。'); }
    return;
  }

  const tgl = t.closest('[data-jargon-toggle]');
  if (tgl) { renderJargon(); openDlg('dlgJargon'); return; }

  /* ---- 脚本四个动作 ---- */
  const sa = t.closest('[data-sw-act]');
  if (sa) {
    const act = sa.getAttribute('data-sw-act'), rel = sa.getAttribute('data-rel');
    if (act === 'open') actOpen(rel);
    else if (act === 'check') actCheck(rel);
    else if (act === 'change') actChange(rel);
    else if (act === 'upload') actUpload(rel);
    return;
  }
  if (t.closest('[data-sw-ask]')) { askReview(); return; }
  if (t.closest('[data-sw-change]')) { submitChange(); return; }
  if (t.closest('[data-sw-replace]')) { doReplaceWithFile(); return; }
  if (t.closest('[data-sw-uploadexec]')) { doUploadAndReview(); return; }
  const bk = t.closest('[data-sw-restore]');
  if (bk) { restoreBackup(bk.getAttribute('data-sw-restore')); return; }

  /* ---- 分析记录：切换 / 列表 ---- */
  if (t.closest('[data-sess-list]')) { renderSessions(); openDlg('dlgSessions'); return; }
  const ssw = t.closest('[data-sess-switch]');
  if (ssw) { switchSession(ssw.getAttribute('data-sess-switch')); return; }
  /* ---- 存档 / 退回 ---- */
  if (t.closest('[data-arc-snap]')) { archiveNow(); return; }
  const ars = t.closest('[data-arc-restore]');
  if (ars) { archiveRestore(ars.getAttribute('data-arc-restore')); return; }

  /* ---- 处理过程：催一次 / 查一次 ---- */
  const pk = t.closest('[data-flowpoke]');
  if (pk) { pokeFlow(pk.getAttribute('data-flowpoke')); return; }
  if (t.closest('[data-flowcheck]')) { recheckIdentify(); loadFlow(); return; }
  // 常驻推进条上的两个动作：与停滞提示里那两个同源，只是任何时刻都能点。
  if (t.closest('[data-runcheck]')) {
    const m = $('#runCtlMsg');
    if (m) m.textContent = '查询中…';
    Promise.all([recheckIdentify(), loadFlow(), refreshArtifacts()]).then(function () {
      const el = $('#runCtlMsg');
      if (el) el.textContent = '已查 ' + fmtClock(new Date().toISOString());
    });
    return;
  }
  const rp = t.closest('[data-runpoke]');
  if (rp) { pokeFlow(rp.getAttribute('data-runpoke')); return; }

  /* ---- 历史 ---- */
  if (t.closest('[data-hist-snap]')) { histSnapshot(); return; }
  if (t.closest('[data-hist-refresh]')) { renderHistory(); return; }
  const hs = t.closest('[data-hist-restore]');
  if (hs) { histRestore(hs.getAttribute('data-hist-restore')); return; }
  const hr = t.closest('[data-hist-rerun]');
  if (hr) { histMarkRerun(hr.getAttribute('data-hist-rerun')); return; }
});

/* 第一步的输入框：边打边记进草稿。
   不直接提交 —— 提交只发生在「保存选择」，这样用户改一半走开也不会写脏配置。 */
document.addEventListener('input', function (e) {
  const t = e.target;
  if (t.matches('[data-plf]')) {
    S.placeDraft = S.placeDraft || placeDraftFromServer();
    S.placeDraft[t.getAttribute('data-plf')] = t.value;
  }
});

/* 一切就绪再启动，见上文说明 */
boot();

