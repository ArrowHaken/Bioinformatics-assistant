/* =====================================================================
   应用外壳（App Shell）—— 一处管住"把指令送进会话"这件事
   ---------------------------------------------------------------------
   为什么需要一个外壳层：
     这件事以前散在业务代码里：先试任务通道、再等注入通道、都不通就把
     原文摆出来让人自己复制。结果有三个具体毛病 ——
       1. 每条链路各自判断"连上没有"，判据还不一样，一处假阳性就把另一条
          能用的通道整个绕过去（之前任务通道 isConnected() 恒为真，就是它）。
       2. 送失败之后没有后续。人得自己复制粘贴，而指令里带着请求号、
          产物清单、命令行，手抄必错。
       3. 送成功了也不等于做完了。宿主收了消息只说明"发出去了"，
          助手有没有真去做、有没有回写，没人盯。
   这个外壳把三件事一次解决：
     注入（两条通道，判据只在这里）· 入队（失败不丢）· 复查（每小时，
     没回写就再发一次，直到回写或到重试上限）。
   业务代码只调 AppShell.inject(text, meta)，不再自己碰通道。
   ===================================================================== */
(function () {
  'use strict';

  var RETRY_MS = 3600 * 1000;   // 每小时复查一次（与后端 INJECT_RETRY_SECS 对齐）
  var MAX_ATTEMPTS = 6;         // 与后端 MAX_INJECT_ATTEMPTS 对齐
  var SEND_WAIT_MS = 8000;      // 单次投递最多等这么久

  var started = false;
  var timer = null;
  var inflight = null;          // 单飞：同一时刻只跑一次投递，避免重复发
  var lastStatus = { pending: 0, due: 0, items: [] };

  /* ---------------- 通道判定：只认真握手 ---------------- */

  // 注入通道（compose）。instance() 由网关 bootstrap 建好，
  // 只有真的嵌在会话里、宿主发过 connect 才会有实例。
  function composeInstance() {
    try {
      var api = window.CatsCoAnnotations;
      if (!api || typeof api.instance !== 'function') return null;
      return api.instance() || null;
    } catch (e) { return null; }
  }

  // 任务通道（cloud-html-artifact 的 bridge）。只认宿主主动握手。
  //   判据必须是自己听到 connect 事件，不能用 isConnected()：
  //   那个实现只要父页说过话就置位，在这套发布方式下恒为真。
  var taskHandshakeAt = 0;
  // 只记前 8 条来自父窗口的消息：origin + type。
  //   用途是回答"宿主到底在不在说话"。一条都没有 = 父页从没跟应用通信；
  //   有消息但 origin 不在白名单 = 消息被 SDK 丢了。两种处置完全不同。
  var parentMsgOrigins = [];
  try {
    window.addEventListener('message', function (ev) {
      try {
        if (ev.source === window.parent && window.parent !== window && parentMsgOrigins.length < 8) {
          parentMsgOrigins.push(String(ev.origin) + ':' + String((ev.data && ev.data.type) || '?'));
        }
      } catch (e) {}
    });
  } catch (e) {}
  try {
    window.addEventListener('message', function (ev) {
      var isParent = false;
      try { isParent = (ev.source === window.parent) && (window.parent !== window); }
      catch (e) { isParent = false; }
      if (!isParent) return;
      var d = ev.data;
      if (d && typeof d === 'object' && d.type === 'catsco.artifact.host.connect.v1') {
        taskHandshakeAt = Date.now();
      }
    });
  } catch (e) {}

  function taskApi() {
    var h = window.catscoArtifactHost;
    if (!h || typeof h.requestTask !== 'function') return null;
    if (!taskHandshakeAt) return null;
    return h;
  }

  function inFrame() {
    try { return window.parent !== window; } catch (e) { return true; }
  }

  /* ---------------- 投递 ---------------- */

  function sendViaCompose(inst, text) {
    return new Promise(function (resolve) {
      var settled = false;
      var ackTimer = null;
      function finish(r) {
        if (settled) return;
        settled = true;
        if (ackTimer) { clearTimeout(ackTimer); ackTimer = null; }
        resolve(r);
      }
      try {
        inst.onComposeAck(function (ack) {
          finish(ack && ack.accepted
            ? { ok: true, via: 'compose', code: 'accepted' }
            : { ok: false, via: 'compose',
                code: (ack && ack.reason) || 'unknown',
                why: '平台没收下：' + ((ack && ack.reason) || '未知原因') });
        });
      } catch (e) {}
      var sent = false;
      try { sent = inst.proposeCompose({ text: text }); } catch (e) { sent = false; }
      if (!sent) {
        finish({ ok: false, via: 'compose', code: 'send_failed', why: '交给平台时没成功。' });
        return;
      }
      // 回执也可能永远不来（通道半断），所以自带超时，不无限等。
      ackTimer = setTimeout(function () {
        finish({ ok: false, via: 'compose', code: 'ack_timeout', why: '12 秒没回执。' });
      }, 12000);
    });
  }

  function sendViaTask(api, text, meta) {
    var payload = { text: String(text || '') };
    if (meta && meta.mode === 'server') payload.mode = 'server';
    var intent = (meta && meta.intent) || 'assistant.connect.v1';
    return api.requestTask(intent, payload).then(function (t) {
      return { ok: true, via: 'task', code: 'accepted', task: t || null };
    }).catch(function (e) {
      var code = (e && e.code) || 'task_failed';
      return { ok: false, via: 'task', code: code, why: (e && e.message) || '任务通道没收下。' };
    });
  }

  // 一条通道失败之后，第二条还值不值得试。
  //   值：平台没收下、交出去没回执、任务没连上 —— 这些是"这条路堵了"，不是"内容有问题"。
  //   不值：正文是空的（换条路也一样）。
  function worthSecondTry(r) {
    if (!r || r.ok) return false;
    return ['send_failed', 'ack_timeout', 'unknown', 'not_connected',
      'task_failed', 'host_not_connected', 'request_timeout'].indexOf(r.code) >= 0;
  }

  /* 一次投递尝试：两条通道都试，谁通走谁。
     ------------------------------------------------------------------
     原来这里只认第一条能用的通道，一旦它"看起来在"就死等它的回执：
       · compose 实例在、但宿主其实已经断了 → 干等 12 秒，第二条通道一次没试；
       · 两条都不在时，不管真实原因是什么，why 一律写"宿主的注入通道没有建立"，
         于是"页面压根没拿到 SDK""独立标签页""宿主没发握手"三种完全不同的
         情况，在面板上长得一模一样，人只能反复点同一个按钮。
     现在：先试 compose（不需要用户激活，定时复查也能用），
     它失败且属于"这条路堵了"就立刻转任务通道（要新鲜点击，手动点是有的）；
     两条都不成时，如实按真实原因给码和下一步。 */
  function attemptSend(text, meta) {
    var inst = composeInstance();
    var api = taskApi();

    if (inst) {
      return sendViaCompose(inst, text).then(function (r) {
        if (r.ok || !worthSecondTry(r)) return r;
        if (!api) {
          // 注入通道交不出去、任务通道又没连上 —— 这时只说"交给平台时没成功"
          //   等于没说：两条路都断了这件事必须讲出来，人才知道该去修通道。
          return { ok: false, via: 'compose', code: r.code || 'send_failed',
            why: '注入通道交不出去（' + (r.why || r.code || '未知原因')
              + '），任务通道也没连上。' };
        }
        return sendViaTask(api, text, meta).then(function (r2) {
          return r2.ok ? r2 : (r2.why ? r2 : r);
        });
      });
    }
    if (api) return sendViaTask(api, text, meta);

    // 两个都不在：可能是宿主还没握手完，给一小段时间等它。
    return new Promise(function (resolve) {
      var t0 = Date.now();
      (function poll() {
        var i2 = composeInstance();
        if (i2) { sendViaCompose(i2, text).then(resolve); return; }
        var a2 = taskApi();
        if (a2) { sendViaTask(a2, text, meta).then(resolve); return; }
        if (Date.now() - t0 >= SEND_WAIT_MS) {
          resolve(noChannelWhy());
          return;
        }
        setTimeout(poll, 250);
      })();
    });
  }

  // 两条都不通时，到底是哪一种不通 —— 这三种的处理方式完全不同，不能混着说。
  function noChannelWhy() {
    var hasSdk = !!(window.CatsCoAnnotations && typeof window.CatsCoAnnotations.instance === 'function');
    var hasBridge = !!(window.catscoArtifactHost && typeof window.catscoArtifactHost.requestTask === 'function');
    if (!inFrame()) {
      return { ok: false, code: 'standalone',
        why: '这是独立标签页，没有会话通道。请从会话里打开这个应用再点。' };
    }
    if (!hasSdk && !hasBridge) {
      return { ok: false, code: 'no_sdk',
        why: '页面没拿到平台的会话脚本（网关没注入）。请关掉这个应用面板重开一次。' };
    }
    return { ok: false, code: 'not_connected',
      why: '页面在会话里，但宿主没发握手：注入通道和任务通道都没建立。'
        + '关掉这个应用面板重开一次；还不行就是平台那侧没连上，需要平台侧处理。' };
  }

  /* ---------------- 后端队列 ---------------- */

  // 基路径必须在脚本执行时就定下来。
  //   踩过的坑：写成"调用时读 document.currentScript"，而真正的调用都发生在
  //   页面交互之后 —— 那时 currentScript 已是 null，拼出来的地址会退化成
  //   站点根路径（/api/inject），观感就是"接口 404、队列永远空"。
  var BASE = (function () {
    try {
      var me = document.currentScript && document.currentScript.src;
      if (me) return new URL(me, location.href).pathname.replace(/[^/]*$/, '');
    } catch (e) {}
    var path = location.pathname || '/';
    if (path.charAt(path.length - 1) !== '/') path = path.slice(0, path.lastIndexOf('/') + 1);
    return path;
  })();

  function apiUrl(p) {
    return BASE + String(p == null ? '' : p).replace(/^\/+/, '');
  }

  function postJSON(p, body) {
    return fetch(apiUrl(p), {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify(body || {})
    }).then(function (r) { return r.json(); });
  }

  function getJSON(p) {
    return fetch(apiUrl(p)).then(function (r) { return r.json(); });
  }

  function queueIt(text, meta) {
    return postJSON('api/inject', {
      action: 'queue',
      text: String(text || ''),
      session: (meta && meta.session) || '',
      req_id: (meta && meta.reqId) || '',
      step: (meta && meta.step) || '',
      reason: (meta && meta.reason) || ''
    }).then(function (r) {
      return { ok: true, id: (r && r.item && r.item.id) || '', retrySecs: (r && r.retry_secs) || 3600 };
    }).catch(function () {
      return { ok: false, id: '', retrySecs: 3600 };
    });
  }

  function reportAttempt(id, ok, code, why) {
    if (!id) return Promise.resolve(null);
    return postJSON('api/inject', { action: 'attempt', id: id, ok: !!ok, code: code || '', why: why || '' })
      .catch(function () { return null; });
  }

  /* ---------------- 通道自检与留痕 ---------------- */

  /* 把"页面这一侧到底看到了什么"报给服务端。
     为什么必须在页面里做：注入失败发生在浏览器内，服务端完全看不见。
     光看"通道没连上"三个字，分不清是没拿到 SDK、独立标签页、还是宿主没握手；
     而父页地址（referrer）只有页面自己知道，它正是判断
     "宿主域在不在白名单"的唯一依据。没有它，只能一直猜。 */
  function channelReport() {
    var inst = composeInstance();
    // 父窗口真实地址：平台的 iframe 常把 Referer 清空（referrerpolicy），
    //   单看 referrer 会永远是空。Chrome 的 location.ancestorOrigins 能直接读出
    //   祖先链上的 origin —— 它是判断"宿主域在不在 SDK 白名单"的唯一可靠来源。
    var ancestors = [];
    try {
      var ao = window.location.ancestorOrigins;
      if (ao) for (var i = 0; i < ao.length; i++) ancestors.push(String(ao[i]));
    } catch (e) {}
    // URL 里有没有平台发下来的绑定票据，是判断"面板到底有没有把这次打开
    //   绑到一个会话上"的直接证据：有 aob_/open_ref 说明平台认了这次打开；
    //   什么都没有，说明它只是把页面塞进 iframe，没打算跟应用对话。
    //   只记参数名和票据前缀，不记完整值。
    var qkeys = [], hasOpenRef = false;
    try {
      var sp = new URLSearchParams(location.search);
      sp.forEach(function (v, k) {
        qkeys.push(k);
        if (/aob_|open_ref|openRef/.test(k + '=' + v)) hasOpenRef = true;
      });
    } catch (e) {}
    return {
      referrer: String(document.referrer || ''),
      ancestors: ancestors.join(' '),
      url_path: String(location.pathname || ''),
      url_query_keys: qkeys.join(','),
      url_has_open_ref: hasOpenRef,
      // 本页在浏览器里的真实 origin。这一项是判"iframe 是不是被隔离"的铁证：
      //   正常跨域框里它是 https://artifact.catsco.cc；
      //   若面板给 iframe 加了 sandbox 却没有 allow-same-origin，它会变成 "null"，
      //   而宿主按精确 origin 发来的握手消息会被浏览器直接丢弃 ——
      //   表现就是"面板说已绑定，应用一条消息都收不到"。
      doc_origin: (function () { try { return String(window.origin || ''); } catch (e) { return 'ERR'; } })(),
      is_secure: !!window.isSecureContext,
      parent_msgs: parentMsgOrigins.join(' '),
      parent_origin: (function () {
        try {
          var r = document.referrer || '';
          return r ? new URL(r).origin : '';
        } catch (e) { return ''; }
      })(),
      in_frame: inFrame(),
      has_sdk: !!(window.CatsCoAnnotations && typeof window.CatsCoAnnotations.instance === 'function'),
      has_instance: !!inst,
      has_bridge: !!(window.catscoArtifactHost && typeof window.catscoArtifactHost.requestTask === 'function'),
      task_handshake: !!taskHandshakeAt,
      ua: String(navigator.userAgent || '').slice(0, 160)
    };
  }

  function reportDiag(code, why) {
    var body = channelReport();
    body.action = 'diag';
    body.code = code || '';
    body.why = why || '';
    return postJSON('api/inject', body).catch(function () { return null; });
  }

  /* ---------------- 对外主入口 ---------------- */

  /**
   * 把一段文本送进当前会话。
   * meta: { session, reqId, step, mode, intent, silent }
   * 返回 { ok, via, code, why, queued, queueId }
   *   ok=true  表示已交给宿主（真正做成还要看回写）
   *   ok=false 且 queued=true 表示已入队，外壳会按小时复查补发
   */
  function inject(text, meta) {
    meta = meta || {};
    var body = String(text || '');
    if (!body.trim()) {
      return Promise.resolve({ ok: false, code: 'empty_text', why: '正文是空的。' });
    }
    // 单飞：并发调用合并成同一次投递，避免同一条指令发两遍。
    if (inflight) return inflight;

    inflight = attemptSend(body, meta).then(function (r) {
      if (r.ok) {
        // 发出去也要入队：真正的完成判据是"有没有回写"。
        //   不入队的话，宿主收了消息但助手没动，就再没人提醒。
        return queueIt(body, meta).then(function (q) {
          return reportAttempt(q.id, true, r.code, '').then(function () {
            refreshStatus();
            return { ok: true, via: r.via, code: r.code, queued: true, queueId: q.id };
          });
        });
      }
      // 失败必须留痕，否则下一次还是只能看同一句"没连上"。
      reportDiag(r.code || 'not_connected', r.why || '');
      return queueIt(body, Object.assign({}, meta, { reason: r.code || 'not_connected' }))
        .then(function (q) {
          refreshStatus();
          // 通道不通不等于这件事没交出去。
          //   以前这里只回 ok:false，页面就画一个红色失败，人只能反复点同一个按钮。
          //   实际指令已经落进本机队列了，助手能取走执行 —— 那是同一件事的另一条路。
          //   所以把"已入信箱"单独标出来，让状态与事实一致。
          return {
            ok: false, via: r.via || '', code: r.code || 'not_connected',
            why: r.why || '', queued: !!q.ok, queueId: q.id,
            relayed: !!q.ok
          };
        });
    }).then(function (out) {
      inflight = null;
      return out;
    }, function (e) {
      inflight = null;
      return { ok: false, code: 'shell_error', why: String((e && e.message) || e) };
    });

    return inflight;
  }

  /* ---------------- 每小时复查 + 补发 ---------------- */

  function retryDue() {
    if (inflight) return Promise.resolve(null);
    return getJSON('api/inject/due').then(function (r) {
      var items = (r && r.items) || [];
      if (!items.length) return null;
      // 只补发最早的一条，避免一次往会话里灌一堆。
      var it = items[0];
      if (Number(it.attempts || 0) >= MAX_ATTEMPTS) {
        return reportAttempt(it.id, false, 'max_attempts', '已达重试上限，需要人工处理');
      }
      return attemptSend(it.text, {
        session: it.session, reqId: it.req_id, step: it.step
      }).then(function (res) {
        return reportAttempt(it.id, res.ok, res.code, res.why).then(function () {
          refreshStatus();
          return res;
        });
      });
    }).catch(function () { return null; });
  }

  function refreshStatus() {
    return getJSON('api/inject').then(function (r) {
      lastStatus = r || lastStatus;
      renderStatus();
      return lastStatus;
    }).catch(function () { return lastStatus; });
  }

  function renderStatus() {
    var el = document.getElementById('injectHint');
    if (!el) return;
    var items = ((lastStatus || {}).items || []);
    var pend = 0, dead = 0;
    items.forEach(function (it) {
      if (it.satisfied || it.dropped) return;
      if (it.exhausted) { dead += 1; return; }
      pend += 1;
    });
    var n = pend + dead;
    if (!n) { el.style.display = 'none'; el.textContent = ''; return; }
    el.style.display = '';
    el.className = 'btn btn-sm qbtn' + (dead ? ' qbtn-bad' : '');
    el.innerHTML = '指令队列 <span class="qdot">' + n + '</span>';
  }

  var panelTimer = null;
  var panelScroll = 0;
  var showDropped = false;

  function stopPanelTimer() {
    if (panelTimer) { clearInterval(panelTimer); panelTimer = null; }
  }

  function statusOf(it) {
    if (it.dropped) return { k: 'drop', t: '已删' };
    if (it.satisfied) return { k: 'done', t: '已回写' };
    if (it.claimed) return { k: 'run', t: '处理中' };
    if (it.exhausted) return { k: 'dead', t: '已停' };
    if (it.delivered) return { k: 'sent', t: '待回写' };
    return { k: 'wait', t: '待发送' };
  }

  function showPanel(keepScroll) {
    var old = document.getElementById('injectPanel');
    if (old) {
      var box0 = old.querySelector('.qp-list');
      if (box0) panelScroll = box0.scrollTop;
      old.parentNode.removeChild(old);
      if (!keepScroll) { stopPanelTimer(); return; }
    }

    getJSON('api/inject').then(function (r) {
      var all = (r && r.items) || [];
      var nowSession = currentSession();
      var maxAtt = Number(r.max_attempts || MAX_ATTEMPTS) || MAX_ATTEMPTS;

      var box = document.createElement('div');
      box.id = 'injectPanel';
      box.className = 'qp';
      box.innerHTML = '<div class="qp-h">'
        + '<b>指令队列</b>'
        + '<div class="qp-h-r">'
        + '<button class="btn btn-sm" data-inj-diag="1">自检</button>'
        + '<button class="btn btn-sm" id="injectPanelClose">关闭</button>'
        + '</div></div>'
        + '<div class="qp-tabs" id="qpTabs"></div>'
        + '<div class="qp-list"></div>'
        + '<div class="qp-f"></div>';
      document.body.appendChild(box);

      function draw() {
        var tabs = box.querySelector('#qpTabs');
        var list = box.querySelector('.qp-list');
        var foot = box.querySelector('.qp-f');
        var keep = list.scrollTop;

        var c = { wait: 0, sent: 0, run: 0, dead: 0, done: 0, drop: 0 };
        all.forEach(function (it) { c[statusOf(it).k] += 1; });

        tabs.innerHTML = '<span class="qp-tag qp-wait">待发送 ' + c.wait + '</span>'
          + '<span class="qp-tag qp-sent">待回写 ' + c.sent + '</span>'
          + (c.run ? '<span class="qp-tag qp-run">处理中 ' + c.run + '</span>' : '')
          + (c.dead ? '<span class="qp-tag qp-dead">已停 ' + c.dead + '</span>' : '')
          + (c.done ? '<span class="qp-tag qp-done">已回写 ' + c.done + '</span>' : '')
          + '<label class="qp-toggle"><input type="checkbox" id="qpShowDrop"'
          + (showDropped ? ' checked' : '') + '> 显示已删 ' + c.drop + '</label>';

        var rows = all.filter(function (it) {
          return showDropped ? true : !it.dropped;
        }).sort(function (x, y) {
          return rankIt(x) - rankIt(y)
            || String(y.created_at || '').localeCompare(String(x.created_at || ''));
        });

        if (!rows.length) {
          list.innerHTML = '<div class="qp-empty">没有待处理的指令</div>';
        } else {
          list.innerHTML = rows.map(function (it) {
            var st = statusOf(it);
            var title = it.step ? stageLabel(it.step) : (it.head || '未命名指令');
            var other = it.session && nowSession && it.session !== nowSession;
            var meta = [];
            meta.push(fmtTs(it.created_at));
            if (it.attempts) meta.push('试 ' + it.attempts + '/' + maxAtt);
            if (other) meta.push('旧会话');
            var ops = '';
            if (st.k !== 'done' && st.k !== 'drop') {
              ops += '<button class="qp-op" data-inj-retry="' + escHtml(it.id) + '">'
                + (st.k === 'dead' ? '重试' : '发送') + '</button>';
              ops += '<button class="qp-op" data-inj-copy="' + escHtml(it.id) + '">复制</button>';
            }
            ops += '<button class="qp-op qp-del" data-inj-drop="' + escHtml(it.id) + '">删除</button>';
            return '<div class="qp-item qp-' + st.k + '">'
              + '<div class="qp-r1"><span class="qp-title" title="' + escHtml(it.head || '') + '">'
              + escHtml(title) + '</span><span class="qp-st">' + st.t + '</span></div>'
              + '<div class="qp-r2">' + escHtml(meta.join(' · '))
              + (it.req_id ? ' · ' + escHtml(it.req_id) : '') + '</div>'
              + (st.k === 'wait' && it.last_why
                  ? '<div class="qp-r3">' + escHtml(it.last_why) + '</div>' : '')
              + '<div class="qp-ops">' + ops + '</div>'
              + '</div>';
          }).join('');
        }

        foot.textContent = '每 ' + Math.round(Number(r.retry_secs || RETRY_MS / 1000) / 60)
          + ' 分钟自动复查 · 上限 ' + maxAtt + ' 次';
        list.scrollTop = keep;

        var cb = box.querySelector('#qpShowDrop');
        if (cb) cb.onchange = function () { showDropped = cb.checked; draw(); };
        bindOps(list);
      }

      function bindOps(list) {
        Array.prototype.forEach.call(list.querySelectorAll('[data-inj-retry]'), function (b) {
          b.onclick = function () {
            b.disabled = true;
            b.textContent = '发送中';
            postJSON('api/inject', { action: 'retry', id: b.getAttribute('data-inj-retry') })
              .then(function () { return retryOne(b.getAttribute('data-inj-retry')); })
              .then(function (res) {
                b.textContent = res && res.ok ? '已发送' : humanize(res);
                return refreshStatus();
              }).then(function () { showPanel(true); })
              .catch(function () { b.disabled = false; b.textContent = '发送'; });
          };
        });
        Array.prototype.forEach.call(list.querySelectorAll('[data-inj-copy]'), function (b) {
          b.onclick = function () {
            b.disabled = true;
            getJSON('api/inject/item?id=' + encodeURIComponent(b.getAttribute('data-inj-copy')))
              .then(function (rr) {
                var txt = (rr && rr.item && rr.item.text) || '';
                if (!txt) { b.disabled = false; b.textContent = '取不到'; return; }
                if (navigator.clipboard && navigator.clipboard.writeText) {
                  return navigator.clipboard.writeText(txt).then(function () {
                    b.textContent = '已复制'; b.disabled = false;
                  }, function () { showRaw(txt); b.textContent = '复制'; b.disabled = false; });
                }
                showRaw(txt); b.disabled = false; b.textContent = '复制';
              }).catch(function () { b.disabled = false; b.textContent = '取不到'; });
          };
        });
        Array.prototype.forEach.call(list.querySelectorAll('[data-inj-drop]'), function (b) {
          b.onclick = function () {
            var id = b.getAttribute('data-inj-drop');
            if (!confirm('删除这一条？删除后不再出现在列表里。')) return;
            b.disabled = true;
            postJSON('api/inject', { action: 'delete', id: id, reason: '人工删除' })
              .then(function () { return refreshStatus(); })
              .then(function () { showPanel(true); })
              .catch(function () { b.disabled = false; b.textContent = '删除'; });
          };
        });
      }

      draw();
      stopPanelTimer();
      panelTimer = setInterval(function () {
        if (!document.getElementById('injectPanel')) { stopPanelTimer(); return; }
        refreshStatus().then(function () {
          if (document.getElementById('injectPanel')) showPanel(true);
        });
      }, 6000);

      box.querySelector('#injectPanelClose').onclick = function () {
        stopPanelTimer();
        box.parentNode.removeChild(box);
      };
      var dg = box.querySelector('[data-inj-diag]');
      if (dg) {
        dg.onclick = function () {
          var rep = channelReport();
          dg.textContent = '自检中';
          reportDiag('selfcheck', '人工触发通道自检').then(function () {
            dg.textContent = '自检';
            alert('在框里 ' + (rep.in_frame ? '是' : '否')
              + '\nSDK ' + (rep.has_sdk ? '有' : '无')
              + '\n注入通道 ' + (rep.has_instance ? '已连' : '未连')
              + '\n任务通道 ' + (rep.task_handshake ? '已连' : '未连')
              + '\n父页 ' + (rep.parent_origin || '未知'));
          });
        };
      }
    }).catch(function () {
      alert('读不到指令队列：本地服务没响应。');
    });
  }

   // 排序用：还需要动手的 0，停下的 1，已回写的 2，作废的 3。
  function rankIt(it) {
    if (it.dropped) return 3;
    if (it.satisfied) return 2;
    if (it.exhausted) return 1;
    return 0;
  }

  // 时间统一成"10-25 23:16"：秒对人没意义，却把一行挤满。
  function fmtTs(s) {
    var v = String(s == null ? '' : s).replace('T', ' ');
    var m = v.match(/^(\d{4})-(\d{2})-(\d{2}) (\d{2}:\d{2})/);
    return m ? (m[2] + '-' + m[3] + ' ' + m[4]) : v;
  }

  // 阶段键 → 人话。指令正文里的 step 是 doublet/merge 这类内部键，
  //   直接摆在面板上没人看得懂。
  var STAGE_NAME = {
    all: '全流程', doublet: '逐样本去双与质控', merge: '合并与批次校正',
    markers: '一次聚类 marker', annotate: '细胞注释', prop: '细胞比例',
    diff: '组间差异与富集', review: '环节质检'
  };
  function stageLabel(k) {
    var key = String(k == null ? '' : k);
    return STAGE_NAME[key] || key;
  }

  // 当前会话号：应用把"被哪个会话打开"写在页头 whoamiHint 里。
  //   面板用它判断这条指令会不会送进人现在看的这个会话 ——
  //   送进已结束的旧会话，重发多少次也没人看见。
  function currentSession() {
    var el = document.getElementById('whoamiHint');
    if (!el) return '';
    var t = String(el.textContent || '').trim();
    if (!t) return '';
    var m = t.match(/grp_[0-9A-Za-z_-]+/);
    return m ? ('cc_group_' + m[0]) : '';
  }

  function escHtml(s) {
    return String(s == null ? '' : s).replace(/[&<>"']/g, function (c) {
      return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c];
    });
  }

  /* 手动补发一条：先取全文，再走同一条投递链，最后如实记结果。 */
  function retryOne(id) {
    return getJSON('api/inject/item?id=' + encodeURIComponent(id)).then(function (r) {
      var it = (r && r.item) || null;
      if (!it || !it.text) return { ok: false, code: 'empty_item' };
      return attemptSend(it.text, { session: it.session, reqId: it.req_id, step: it.step })
        .then(function (res) {
          return reportAttempt(id, res.ok, res.code, res.why).then(function () { return res; });
        });
    }).catch(function () { return { ok: false, code: 'fetch_failed' }; });
  }

  /* 内部错误码 → 人话。
     为什么必须翻译：not_connected / ack_timeout 这类词是给代码看的，
     摆给人看等于没说。每种情况都要说清"现在能做什么"。 */
  // 原文摊开：剪贴板不可用（非 https、被策略拦）时的兜底。
  //   给一个可选中的文本框，人自己 Ctrl+C，不让复制能力决定能不能办事。
  function showRaw(txt) {
    var old = document.getElementById('injectRaw');
    if (old) old.parentNode.removeChild(old);
    var d = document.createElement('div');
    d.id = 'injectRaw';
    d.style.cssText = 'position:fixed;left:50%;top:50%;transform:translate(-50%,-50%);'
      + 'z-index:300;width:min(680px,90vw);max-height:70vh;display:flex;flex-direction:column;'
      + 'background:#fff;border:1px solid #dfe4df;border-radius:12px;padding:14px 16px;'
      + 'box-shadow:0 18px 48px rgba(20,28,24,.22);'
      + 'font:13px/1.6 -apple-system,"PingFang SC","Microsoft YaHei",system-ui,sans-serif';
    d.innerHTML = '<div style="font-weight:600;margin-bottom:6px">指令原文（全选后复制）</div>'
      + '<textarea readonly style="flex:1 1 auto;min-height:260px;width:100%;box-sizing:border-box;'
      + 'font:12px/1.5 ui-monospace,Menlo,Consolas,monospace;border:1px solid #e3e8e4;'
      + 'border-radius:8px;padding:8px;resize:vertical"></textarea>'
      + '<div style="text-align:right;margin-top:8px">'
      + '<button style="border:1px solid #cfd6d1;background:#fff;border-radius:7px;'
      + 'padding:4px 12px;cursor:pointer;font:inherit">关闭</button></div>';
    document.body.appendChild(d);
    var ta = d.querySelector('textarea');
    ta.value = String(txt || '');
    ta.focus();
    ta.select();
    d.querySelector('button').onclick = function () { d.parentNode.removeChild(d); };
  }

  function humanize(res) {
    var code = (res && res.code) || '';
    if (code === 'standalone') return '这个页面是单独打开的，没有会话通道。请从会话里打开应用。';
    if (code === 'not_connected') return '宿主还没把通道连上。请等页面右上角出现会话号后再点，或重开一次应用。';
    if (code === 'no_sdk') return '页面没拿到平台的会话脚本。请重开应用。';
    if (code === 'ack_timeout') return '交出去了但平台 12 秒没回执，通道可能已断。重开应用再试。';
    if (code === 'send_failed') return '交给平台时失败了。重开应用再试。';
    if (code === 'empty_item') return '这条记录的正文是空的，没法重发。';
    if (code === 'fetch_failed') return '读不到这条记录（本地服务没响应）。';
    return (res && res.why) || code || '未知原因';
  }

  function start() {
    if (started) return;
    started = true;
    // 三个触发点都要：定时、页面重新可见、窗口获得焦点。
    //   只靠定时的话，浏览器把后台标签页的定时器降频甚至冻结，
    //   一小时一次就成了不定时。
    timer = setInterval(function () { retryDue(); refreshStatus(); }, RETRY_MS);
    document.addEventListener('visibilitychange', function () {
      if (document.visibilityState === 'visible') { retryDue(); refreshStatus(); }
    });
    window.addEventListener('focus', function () { retryDue(); refreshStatus(); });
    // 那行字必须能点开：只说"待补发 1 条"却不给入口，等于让人干看着。
    //   DOM 可能还没就绪，所以用委托绑在 document 上，页头重绘也不丢。
    document.addEventListener('click', function (ev) {
      var t = ev.target;
      if (t && t.id === 'injectHint') { ev.preventDefault(); showPanel(); }
    });
    // 启动就查一次：关掉很久再打开，积压的那几条要立刻补上，不等一小时。
    refreshStatus().then(function () { return retryDue(); }).then(refreshStatus);
    // 打开就自报一次现场：通道通不通、父页是谁。
    //   为什么不等失败才报：失败时人未必点自检，日志就永远是空的；
    //   而"打开这一刻通道是什么状态"才是判断问题的关键证据。
    setTimeout(function () {
      var r = channelReport();
      reportDiag(r.has_instance ? 'page_load_ok' : 'page_load_no_channel',
        '打开页面时自动上报');
    }, 1500);
  }

  window.AppShell = {
    inject: inject,
    diag: function () { return channelReport(); },
    reportDiag: reportDiag,
    retryDue: retryDue,
    refreshStatus: refreshStatus,
    showPanel: showPanel,
    status: function () { return lastStatus; },
    channels: function () {
      return {
        inFrame: inFrame(),
        compose: !!composeInstance(),
        task: !!taskApi(),
        taskHandshakeAt: taskHandshakeAt
      };
    },
    start: start
  };

  // 外壳自己起来，不依赖业务代码记得初始化。
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', start);
  } else {
    start();
  }
})();
