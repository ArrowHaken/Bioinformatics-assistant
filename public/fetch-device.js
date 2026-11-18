(() => {
  'use strict';
  const trigger = document.getElementById('btnFetchDevice');
  if (!trigger) return;
  const box = document.createElement('div');
  box.className = 'modal'; box.style.display = 'none';
  box.innerHTML = '<div class="modal-box"><div class="modal-h"><b>取回设备接入 / 更换</b><button class="btn ghost" data-close>关闭</button></div><div class="modal-body"><p>分析记录和待取回任务保存在应用端。换电脑不用重跑分析，新电脑必须能连接结果服务器。</p><p>接入配置仅允许取回您自己的产物，有效期七天。更换配置立即撤销旧设备凭证，不清空任务。</p><p data-state>正在读取接入状态…</p><label>设备名称 <input data-name value="fetch-device" maxlength="80" placeholder="字母、数字、点、下划线或短横线"></label><div class="ac-toolbar" style="margin-top:16px"><button class="btn" data-client>下载取回程序</button><button class="btn primary" data-config>接入 / 更换并下载配置</button><button class="btn" data-revoke>撤销取回设备</button></div><p>在新电脑把程序与配置放在私人目录，安装 Python 和 paramiko 后运行：python pa_fetch_client.py --config fetch-config.json。Linux/macOS 先执行 chmod 600 fetch-config.json。</p><p>服务器地址、账号、授权结果根目录和口令仅在该电脑输入，不写入下载配置。首次连接须先用系统 SSH 核验服务器主机指纹。程序运行时每十五秒检查一次待取回任务，关闭后暂停，重新启动可继续。</p><p>接入配置是凭证，请勿转发。旧版匿名程序不能代替此接入；页面有配置不代表设备已在线或已经取回。</p><p data-message role="status"></p></div></div>';
  document.body.appendChild(box);
  const state = box.querySelector('[data-state]');
  const message = box.querySelector('[data-message]');
  const call = async (path, body) => {
    const r = await fetch(apiUrl('/api/fetch-client/' + path), body === undefined ? {credentials:'same-origin'} : {method:'POST', credentials:'same-origin', headers:{'Content-Type':'application/json'}, body:JSON.stringify(body)});
    const data = await r.json();
    if (!r.ok || !data.ok) throw new Error(data.error || data.code || '接入失败');
    return data;
  };
  const save = (text, type, name) => {
    const url = URL.createObjectURL(new Blob([text], {type}));
    const a = document.createElement('a'); a.href = url; a.download = name;
    box.appendChild(a); a.click(); a.remove(); setTimeout(() => URL.revokeObjectURL(url), 30000);
  };
  const refresh = async () => {
    const r = await call('status');
    state.textContent = r.devices.length ? '当前有效接入：' + r.devices.map(x => x.device_id + '，到期 ' + new Date(x.expires_at * 1000).toLocaleString()).join('；') + '。在线状态尚未核验。' : '尚无有效取回设备接入，待取回任务仍保留。';
  };
  const action = (sel, fn) => box.querySelector(sel).addEventListener('click', async (e) => {
    e.target.disabled = true; message.textContent = '';
    try { await fn(); } catch (err) { message.textContent = err.message; }
    finally { e.target.disabled = false; }
  });
  trigger.addEventListener('click', async () => { box.style.display = 'flex'; try { await refresh(); } catch(e) { state.textContent = e.message; } });
  box.querySelector('[data-close]').onclick = () => { box.style.display = 'none'; };
  action('[data-client]', async () => { const r = await call('client'); save(r.source, 'text/x-python;charset=utf-8', 'pa_fetch_client.py'); message.textContent = '程序下载已发起，尚未启动取回。'; });
  action('[data-config]', async () => {
    if (!confirm('这会立即撤销旧设备凭证，保留待取回任务。确认在新设备接入？')) return;
    const device_id = box.querySelector('[data-name]').value.trim();
    if (!/^[A-Za-z0-9._-]{1,80}$/.test(device_id)) throw new Error('设备名称请使用字母、数字、点、下划线或短横线。');
    const r = await call('config', {device_id}); save(JSON.stringify(r.config, null, 2), 'application/json', 'fetch-config.json');
    message.textContent = '新配置下载已发起，旧凭证已撤销。请在能连服务器的设备启动取回程序。'; await refresh();
  });
  action('[data-revoke]', async () => { if (!confirm('撤销后取回暂停，但队列保留。确认？')) return; await call('revoke', {}); await refresh(); message.textContent = '设备凭证已撤销，待取回任务保留。'; });
})();
