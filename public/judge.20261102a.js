/* =====================================================================
   judge.20261102a.js -- 判定与返工面板（前端接线）
   ---------------------------------------------------------------------
   为什么单独一个文件，不改 app.js
     app.js 已经 5000 行、几十个渲染函数，一处改动可能牵动别的区块。
     这一批要加的「四态显示 / 返工面板 / 交付门 / 下载按钮」是相对独立的一块，
     单独一个文件、自己取自己的数据、自己渲染自己的容器，
     出问题也只影响这一块。改坏了删掉 script 标签就回滚，不用碰主脚本。

   它解决的具体问题
     后端已经能拒绝、能开返工单、能下文件，但页面完全看不见：
       · 环节只显示「已完成 / 失败」，分不出「我说我跑完了」和「判定通过了」；
       · 返工单存在库里，页面上没有一个字；
       · 交付门有结论，但没有任何一处显示「这一轮能不能交」；
       · 产物只有「取回并查看」，没有「下载」，也没有「会不会打扰会话」的区分。

   一条硬规矩
     这里只做「显示」和「发出动作」，不自己算结论。
     能不能交、过没过、要不要返工，全部读 /api/judge 给出的字段。
     页面自己推一遍结论，就是第二份判据 —— 那正是这次要消灭的东西。
   ===================================================================== */
(function () {
  'use strict';

  var API_BASE = (function () {
    try {
      var me = document.currentScript && document.currentScript.src;
      if (me) return new URL(me, location.href).pathname.replace(/[^/]*$/, '');
    } catch (e) {}
    var p = location.pathname || '/';
    if (p.charAt(p.length - 1) !== '/') p = p.slice(0, p.lastIndexOf('/') + 1);
    return p;
  })();

  function url(p) { return API_BASE + String(p == null ? '' : p).replace(/^\/+/, ''); }

  function esc(s) {
    return String(s == null ? '' : s).replace(/[&<>"']/g, function (c) {
      return ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c];
    });
  }

  function getJSON(p) {
    return fetch(url(p), { headers: { Accept: 'application/json' } }).then(function (r) {
      return r.text().then(function (t) {
        var body = null;
        try { body = JSON.parse(t); } catch (e) {}
        if (!r.ok) {
          var err = new Error((body && body.error) || ('HTTP ' + r.status));
          err.status = r.status; err.body = body;
          throw err;
        }
        return body;
      });
    });
  }

  function postJSON(p, obj) {
    return fetch(url(p), {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(obj || {})
    }).then(function (r) {
      return r.text().then(function (t) {
        var body = null;
        try { body = JSON.parse(t); } catch (e) {}
        if (!r.ok) {
          var err = new Error((body && body.error) || ('HTTP ' + r.status));
          err.status = r.status; err.body = body;
          throw err;
        }
        return body;
      });
    });
  }

  var S = { data: null, busy: false, openTicket: null, sessionId: null };

  function currentSession() {
    // 主脚本把当前分析记录存在 S.sessionId 上；读不到时退回 URL。
    try {
      if (window.S && window.S.sessionId) return window.S.sessionId;
    } catch (e) {}
    if (window.__paSessionId) return window.__paSessionId;
    var m = /[?&]session=([^&]+)/.exec(location.search || '');
    return m ? decodeURIComponent(m[1]) : '';
  }

  var STEP_CN = {
    doublet: '逐样本去双与质控', merge: '合并与批次校正', markers: '一次聚类 marker',
    filter: '筛群与剔样本后重聚类', final: '终版比例统计', fullmark: '全量 marker',
    diff: '组间差异', annotate: '细胞类型注释', figs: '出图', enrich: 'GO 富集',
    ppt: '生成交付 PPT', pack: '打包交付物'
  };

  var STATE_CN = {
    verified: { t: '已核验', c: 'ok' },
    released: { t: '人工放行', c: 'rel' },
    rejected: { t: '已退回', c: 'bad' },
    escalated: { t: '转人工', c: 'warn' },
    reported: { t: '已申报，待判定', c: 'wait' },
    delegated: { t: '已交给助手', c: 'wait' },
    running: { t: '进行中', c: 'wait' },
    missing: { t: '还没跑', c: 'idle' }
  };

  function host() { return document.getElementById('judgeHost'); }

  function showRoot() {
    var sec = document.getElementById('secJudge');
    if (sec) sec.style.display = '';
    var main = document.getElementById('main');
    if (main) main.style.display = '';
  }

  function render() {
    var h = host();
    if (!h) return;
    var d = S.data;

    if (!d) {
      h.innerHTML = '<div class="card" style="padding:16px 18px;color:var(--ink3)">'
        + '判定信息还没取到。</div>';
      return;
    }
    if (d.ok === false) {
      // 身份拿不到时 fail-closed：不显示任何已有记录，也不假装"一切正常"。
      h.innerHTML = '<div class="card" style="padding:16px 18px">'
        + '<b>身份未识别，判定信息不显示</b>'
        + '<div class="small" style="color:var(--ink2);margin-top:6px">'
        + esc(d.error || '') + '</div>'
        + '<div class="small" style="color:var(--ink3);margin-top:4px">'
        + esc(d.hint || '') + '</div>'
        + '<div style="margin-top:10px"><button class="btn btn-sm" data-judge-refresh>重试识别</button></div>'
        + '</div>';
      return;
    }

    var mode = d.mode || 'warn';
    var steps = d.steps || [];
    var tickets = d.tickets || [];
    var deliv = d.deliverable || null;

    var failed = steps.filter(function (s) { return s.status === 'rejected'; });
    var verified = steps.filter(function (s) {
      return s.status === 'verified' || s.status === 'released';
    });
    var pending = steps.filter(function (s) { return s.status === 'reported' || s.status === 'delegated'; });
    var openTickets = tickets.filter(function (t) {
      return t.state === 'open' || t.state === 'queued' || t.state === 'sent';
    });

    var html = '';

    // ---- 顶部红条：只在真有问题时出现 ----
    //   为什么做成条而不是卡片：卡片会被当成"一个区块"，看一眼就滑过去；
    //   条压在标题下面，是在说"这一屏的状态有问题"，位置本身就是语气。
    if (mode === 'warn' && failed.length) {
      html += '<div class="jbar jbar-warn">'
        + '<b>' + failed.length + ' 个环节没通过判定</b>'
        + '<span>当前档位是 warn：允许继续，但这些会记进交付清单的「未核验项」。</span>'
        + '<button class="btn btn-sm" data-judge-mode="block">改成拦截</button>'
        + '</div>';
    } else if (mode === 'off' && failed.length) {
      html += '<div class="jbar jbar-off">'
        + '<b>' + failed.length + ' 个环节没通过</b><span>档位是 off：只记录，不提示。</span>'
        + '<button class="btn btn-sm" data-judge-mode="warn">打开提示</button>'
        + '</div>';
    }

    // ---- 结论卡：这一轮能不能交 ----
    html += '<div class="jcard">';
    if (!d.version_id) {
      html += '<div class="jrow"><span class="jk">这一轮</span>'
        + '<span class="jv">还没有对应的版本记录。跑第一步并回写之后才会生成。</span></div>';
    } else if (!deliv) {
      html += '<div class="jrow"><span class="jk">交付门</span>'
        + '<span class="jv">还判不了：没有拿到交付门结果</span></div>';
    } else if (deliv.ok) {
      html += '<div class="jrow"><span class="jk">交付门</span>'
        + '<span class="jv jok">可以交付</span>'
        + '<span class="jnote">已核验环节全部通过，或被人工放行。</span></div>';
    } else {
      html += '<div class="jrow"><span class="jk">交付门</span>'
        + '<span class="jv jbad">现在不能交</span>'
        + '<span class="jnote">' + (deliv.reasons || []).length + ' 个环节未满足</span></div>';
    }
    html += '<div class="jrow"><span class="jk">判定档位</span>'
      + '<span class="jv">' + esc(mode) + '</span>'
      + '<span class="jnote">' + esc(d.mode_meaning || '') + '</span>'
      + '<span class="jacts">'
      + ['off', 'warn', 'block'].map(function (m) {
        return '<button class="btn btn-sm' + (m === mode ? ' primary' : '') + '"'
          + ' data-judge-mode="' + m + '">' + m + '</button>';
      }).join('') + '</span></div>';
    html += '<div class="jrow"><span class="jk">进度</span>'
      + '<span class="jv">已核验 ' + verified.length + ' / ' + steps.length + ' 环</span>'
      + '<span class="jnote">待判定 ' + pending.length + '　退回 ' + failed.length
      + '　待返工 ' + openTickets.length + '</span></div>';
    html += '</div>';

    // ---- 环节四态 ----
    if (steps.length) {
      html += '<div class="jsec-t">每个环节的状态</div>';
      html += '<div class="jsteps">';
      steps.forEach(function (s) {
        var cn = STATE_CN[s.status] || { t: s.status || '未知', c: 'idle' };
        var title = STEP_CN[s.step_id] || s.step_id;
        var tip = '提交 ' + (s.submit || '?') + '　执行 ' + (s.exec || '?')
          + '　同步 ' + (s.sync || '?') + '　质检 ' + (s.qc || '?');
        html += '<div class="jstep ' + cn.c + '" title="' + esc(tip) + '">'
          + '<span class="jsn">' + esc(title) + '</span>'
          + '<span class="jsb">' + esc(cn.t) + '</span>'
          + '<span class="jsr">第 ' + esc(s.attempt_no || 1) + ' 次</span>'
          + '</div>';
      });
      html += '</div>';
    }

    // ---- 交付门的拒绝原因：逐条列，不合并 ----
    if (deliv && !deliv.ok && (deliv.reasons || []).length) {
      html += '<div class="jsec-t">为什么还不能交</div><div class="jcard">';
      deliv.reasons.forEach(function (r) {
        var cn = STATE_CN[r.status] || { t: r.status, c: 'idle' };
        html += '<div class="jreason"><b>' + esc(STEP_CN[r.step_id] || r.step_id) + '</b>'
          + '<span class="jtag ' + cn.c + '">' + esc(cn.t) + '</span>'
          + '<span class="jnote">质检 ' + esc(r.qc || 'missing') + '</span></div>';
      });
      html += '</div>';
    }

    // ---- 返工单 ----
    if (tickets.length) {
      html += '<div class="jsec-t">返工单（' + tickets.length + '）</div>';
      tickets.forEach(function (t) {
        var open = S.openTicket === t.ticket_id;
        var stCn = { open: '待返工', queued: '排队中', sent: '已下发', nudged: '已催促',
          escalated: '转人工', closed: '已关闭' }[t.state] || t.state;
        html += '<div class="jtk">'
          + '<div class="jtk-h" data-ticket="' + esc(t.ticket_id) + '">'
          + '<span class="jid">' + esc(t.ticket_id) + '</span>'
          + '<span class="jname">' + esc(STEP_CN[t.step_id] || t.step_id) + '</span>'
          + '<span class="jtag ' + (t.state === 'escalated' ? 'warn' : 'bad') + '">'
          + esc(stCn) + ' ' + esc(t.attempt_no) + '/' + esc(t.max_auto) + '</span>'
          + '<span class="jnote">截止 ' + esc(String(t.due_at || '').replace('T', ' ')) + '</span>'
          + '<span class="jmore">' + (open ? '收起' : '展开') + '</span>'
          + '</div>';
        if (open) {
          var fc = t.failed_checks || [];
          html += '<div class="jtk-b">';
          if (fc.length) {
            html += '<div class="jsub">没通过的地方</div><ul class="jlist">';
            fc.forEach(function (f) {
              html += '<li><b>[' + esc(f.layer) + ' ' + esc(f.name) + ']</b> '
                + esc(f.expected || '')
                + '<div class="jnote">实际：' + esc(f.actual || '') + '</div>'
                + '<div class="jnote">证据：' + esc(f.evidence || '') + '</div></li>';
            });
            html += '</ul>';
          }
          if ((t.must_do || []).length) {
            html += '<div class="jsub">需要做的事</div><ul class="jlist">';
            (t.must_do || []).forEach(function (m) { html += '<li>' + esc(m) + '</li>'; });
            html += '</ul>';
          }
          if (t.command_hint) {
            html += '<div class="jsub">照这条命令重跑</div>'
              + '<pre class="jcmd">' + esc(t.command_hint) + '</pre>';
          }
          if (t.instruction) {
            html += '<div class="jsub">发给助手的原文</div>'
              + '<pre class="jcmd">' + esc(t.instruction) + '</pre>';
          }
          html += '<div class="jtk-acts">';
          if (t.attempt_id) {
            html += '<button class="btn btn-sm" data-release="' + esc(t.attempt_id) + '">'
              + '人工放行（需填理由）</button>';
          }
          if (t.command_hint) {
            html += '<button class="btn btn-sm" data-copy="' + esc(t.command_hint) + '">复制命令</button>';
          }
          html += '</div></div>';
        }
        html += '</div>';
      });
    }

    // ---- 最近判定 ----
    var reviews = d.reviews || [];
    if (reviews.length) {
      html += '<div class="jsec-t">最近的判定</div><div class="jcard">';
      reviews.slice(0, 8).forEach(function (r) {
        var c = r.counts || {};
        var vc = r.verdict === 'pass' ? 'ok' : (r.verdict === 'fail' ? 'bad'
          : (r.verdict === 'warn' ? 'warn' : 'wait'));
        html += '<div class="jreason"><b>' + esc(STEP_CN[r.step_id] || r.step_id) + '</b>'
          + '<span class="jtag ' + vc + '">' + esc(r.verdict) + '</span>'
          + '<span class="jnote">不通过 ' + esc(c.fail || 0)
          + '　判不了 ' + esc(c.unknown || 0)
          + '　提醒 ' + esc(c.warn || 0)
          + (c.not_impl ? ('　判据未实现 ' + esc(c.not_impl)) : '')
          + '　' + esc(String(r.at || '').replace('T', ' ')) + '</span></div>';
      });
      html += '</div>';
    }

    // ---- 产物下载 ----
    html += '<div class="jsec-t">取回与下载</div>'
      + '<div class="jcard"><div class="jrow"><span class="jk">说明</span>'
      + '<span class="jv">下载走应用自己的通道，不经过会话；'
      + '「交给助手取」会发一条指令给助手。</span></div>'
      + '<div class="jrow"><span class="jk">下载一个产物</span><span class="jv">'
      + '<input id="judgeRel" placeholder="例如 10.figs/1.cluster/umap.png" '
      + 'style="min-width:280px;padding:6px 8px;border:1px solid var(--line);border-radius:6px">'
      + ' <button class="btn btn-sm primary" data-dl="1">下载（不经过会话）</button>'
      + '</span></div>'
      + '<div id="judgeDlMsg" class="jnote"></div></div>';

    h.innerHTML = html;
  }

  function load() {
    S.sessionId = currentSession();
    var q = '/api/judge' + (S.sessionId ? ('?session=' + encodeURIComponent(S.sessionId)) : '');
    return getJSON(q).then(function (d) {
      S.data = d;
      showRoot();
      render();
    }).catch(function (e) {
      var h = host();
      if (h) {
        h.innerHTML = '<div class="card" style="padding:16px 18px;color:var(--ink2)">'
          + '判定信息读不到：' + esc(e.message) + '</div>';
      }
      showRoot();
    });
  }

  function setMode(m) {
    if (S.busy) return;
    S.busy = true;
    postJSON('/api/review/mode', { mode: m, who: (S.data && S.data.owner_key) || '' })
      .then(function () { return load(); })
      .then(function () { S.busy = false; })
      .catch(function (e) {
        S.busy = false;
        alert('改档位没成功：' + e.message);
      });
  }

  function doRelease(attemptId) {
    // 理由必须由人填：空理由的放行等于没有记录。
    var why = window.prompt('放行理由（会永久写进交付清单）：', '');
    if (why === null) return;
    if (!String(why).trim()) { alert('理由不能空。'); return; }
    postJSON('/api/review/release', {
      attempt_id: attemptId, why: String(why).trim(),
      who: (S.data && S.data.owner_key) || ''
    }).then(function () { return load(); })
      .catch(function (e) {
        alert('放行没成功：' + (e.body && e.body.error ? e.body.error : e.message));
      });
  }

  function doDownload() {
    var inp = document.getElementById('judgeRel');
    var msg = document.getElementById('judgeDlMsg');
    var rel = inp ? String(inp.value || '').trim() : '';
    if (!rel) { if (msg) msg.textContent = '先填一个产物路径。'; return; }
    var q = '/api/artifact/download?rel=' + encodeURIComponent(rel)
      + (S.data && S.data.version_id ? ('&version_id=' + encodeURIComponent(S.data.version_id)) : '');
    if (msg) msg.textContent = '正在检查本地是否已有…';
    fetch(url(q)).then(function (r) {
      var ct = r.headers.get('Content-Type') || '';
      if (ct.indexOf('application/json') >= 0) {
        return r.json().then(function (b) {
          if (msg) {
            msg.textContent = b.ready === false
              ? ('还没取回：' + (b.error || '') + ' 任务号 ' + (b.request_id || ''))
              : (b.error || '没拿到文件');
          }
        });
      }
      // 是文件流：直接交给浏览器下载，不读进内存二次转手。
      return r.blob().then(function (blob) {
        var a = document.createElement('a');
        var u = URL.createObjectURL(blob);
        a.href = u;
        a.download = rel.split('/').pop() || 'result.bin';
        document.body.appendChild(a); a.click(); a.remove();
        setTimeout(function () { URL.revokeObjectURL(u); }, 4000);
        if (msg) msg.textContent = '已开始下载。';
      });
    }).catch(function (e) {
      if (msg) msg.textContent = '下载失败：' + e.message;
    });
  }

  document.addEventListener('click', function (e) {
    var t = e.target;
    if (!t || !t.closest) return;

    var r = t.closest('[data-judge-refresh]');
    if (r) { load(); return; }

    var m = t.closest('[data-judge-mode]');
    if (m) { setMode(m.getAttribute('data-judge-mode')); return; }

    var tk = t.closest('[data-ticket]');
    if (tk) {
      var id = tk.getAttribute('data-ticket');
      S.openTicket = (S.openTicket === id) ? null : id;
      render();
      return;
    }

    var rl = t.closest('[data-release]');
    if (rl) { doRelease(rl.getAttribute('data-release')); return; }

    var cp = t.closest('[data-copy]');
    if (cp) {
      var txt = cp.getAttribute('data-copy');
      if (navigator.clipboard) navigator.clipboard.writeText(txt).catch(function () {});
      cp.textContent = '已复制';
      return;
    }

    var dl = t.closest('[data-dl]');
    if (dl) { doDownload(); return; }
  });

  // ---- 启动与跟随 ----
  //   不塞进主脚本的 renderAll：那一串函数各有各的时序，
  //   插进去会让"谁先跑完"变得依赖网络顺序。这里自己按节拍刷新。
  function boot() {
    load();
    setInterval(function () {
      // 只在页面可见时刷新：切到后台还每 15 秒打一次接口没有意义。
      if (document.hidden) return;
      load();
    }, 15000);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', boot);
  } else {
    boot();
  }
})();
