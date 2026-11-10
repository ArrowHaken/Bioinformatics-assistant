'use strict';
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const code = fs.readFileSync('public/app-handoff.js', 'utf8');
const platform = 'https://platform.test';
const protocol = 'catsco.app-handoff.v1';
const now = 1900000000000;
const flush = async () => { for (let i = 0; i < 16; i++) await Promise.resolve(); };
function deferred() { let resolve, reject; const promise = new Promise((a,b) => { resolve=a; reject=b; }); return {promise,resolve,reject}; }
const response = data => ({ok:true, redirected:false, json:async()=>data});
const hello = (extra={}) => ({protocol,type:'hello',nonce:'nonce1',request_id:'request1',topic_id:'topic1',agent_uid:'agent1',...extra});
const envelope = (extra={}) => ({...hello(),type:'handoff',action:'initialize',parameters:{session_id:'session1'},expires_at:now+60000,...extra});
function harness(opts={}) {
  const sent=[], calls=[], switched=[], timers=new Map(); let timerId=0, listener;
  const parent={postMessage:(d,o)=>sent.push({d,o})};
  const window={parent,location:{href:'https://app.test/app/',origin:'https://app.test'},addEventListener:(n,f)=>{assert.equal(n,'message');listener=f;}};
  const S={sessionId:'original'};
  const context={window, URL, AbortController, Date:{now:()=>opts.clock ? opts.clock() : now}, S,
    setTimeout:(f,ms)=>{assert.equal(ms,10000);timers.set(++timerId,f);return timerId;},clearTimeout:id=>timers.delete(id),
    fetch:async(url,init)=>{calls.push({url,init});assert.equal(new URL(url).origin,'https://app.test');assert.equal(init.credentials,'same-origin');assert.equal(init.cache,'no-store');assert.equal(init.redirect,'error');
      if(init.method==='GET') return opts.config ? opts.config() : response({ok:true,platform_origin:platform});
      return opts.post ? opts.post(JSON.parse(init.body)) : response({ok:true,session_id:JSON.parse(init.body).parameters.session_id,request_id:JSON.parse(init.body).request_id});},
    switchSession:async id=>{switched.push(id); if(opts.switcher) await opts.switcher(id,S); else S.sessionId=id;}
  };
  if(opts.apiUrl) context.apiUrl=opts.apiUrl;
  vm.runInNewContext(code,context);
  return {sent,calls,switched,timers,S,emit:(d,origin=platform,source=parent)=>listener({data:d,origin,source}),parent};
}
(async()=>{
  let h=harness(); await flush();
  h.emit(hello(),platform,{}); h.emit(hello(),'https://evil.test'); h.emit(hello(),platform+'/');
  assert.equal(h.sent.length,0);
  h.emit(hello()); assert.equal(h.sent.length,1);
  assert.deepEqual(JSON.parse(JSON.stringify(h.sent[0].d.schema)),{session_id:{type:'string',required:true,maxLength:128}});
  assert.equal(h.sent[0].d.supports_run,false);assert.equal(h.sent[0].o,platform);
  h.emit(envelope());await flush();assert.deepEqual(h.switched,['session1']);assert.equal(h.sent.at(-1).d.status,'initialized');
  h.emit(envelope());await flush();assert.equal(h.switched.length,1);assert.equal(h.calls.length,3);
  h.emit(envelope({parameters:{session_id:'other'}}));await flush();assert.equal(h.sent.at(-1).d.status,'failed');assert.equal(h.switched.length,1);
  for(const field of ['nonce','request_id','topic_id','agent_uid']) {
    h=harness();await flush();h.emit(hello());h.emit(envelope({[field]:'forged'}));await flush();assert.equal(h.calls.length,1);assert.equal(h.switched.length,0);
  }
  const bads=[{action:'run'},{expires_at:now},{expires_at:now-1},{expires_at:now+300001},{expires_at:'1900000060000'},
    {parameters:{session_id:'session1',command:'whoami'}},{parameters:{session_id:{id:'session1'}}},{parameters:{session_id:'../state'}},
    {parameters:{session_id:'x'.repeat(129)}},{parameters:{path:'/tmp/x'}},{secret:'secret'},{parameters:[]}];
  for(const bad of bads){h=harness();await flush();h.emit(hello());h.emit(envelope(bad));await flush();assert.equal(h.calls.length,1);assert.equal(h.switched.length,0);assert.equal(h.sent.at(-1).d.status, Object.hasOwn(bad,'secret') ? undefined : 'failed');}
  for(const config of [{ok:false,platform_origin:platform},{ok:true},{ok:true,platform_origin:'http://platform.test'},{ok:true,platform_origin:platform+'/path'},{ok:true,platform_origin:platform+'/'}]){
    h=harness({config:()=>response(config)});await flush();h.emit(hello());h.emit(envelope());await flush();assert.equal(h.sent.length,0);assert.equal(h.switched.length,0);
  }
  let gate=deferred();h=harness({config:()=>gate.promise});h.emit(envelope());h.emit(hello());gate.resolve(response({ok:true,platform_origin:platform}));await flush();assert.equal(h.sent.length,1);assert.equal(h.calls.length,1);
  gate=deferred();h=harness({config:()=>gate.promise});h.emit(hello(),'https://evil.test');gate.resolve(response({ok:true,platform_origin:platform}));await flush();assert.equal(h.sent.length,0);
  gate=deferred();h=harness({post:()=>gate.promise});await flush();h.emit(hello());h.emit(envelope());await flush();assert.equal(h.S.sessionId,'original');assert.equal(h.switched.length,0);
  h.emit(hello({nonce:'nonce2',request_id:'request2'}));gate.resolve(response({ok:true,session_id:'session1',request_id:'request1'}));await flush();assert.equal(h.switched.length,0);assert.equal(h.sent.filter(x=>x.d.type==='ack').length,0);
  let attempt=0;h=harness({post:()=>++attempt===1 ? Promise.reject(new Error('SECRET')) : response({ok:true,session_id:'session1',request_id:'request1'})});await flush();h.emit(hello());h.emit(envelope());await flush();assert.equal(h.sent.at(-1).d.status,'failed');assert(!JSON.stringify(h.sent).includes('SECRET'));h.emit(envelope());await flush();assert.equal(h.sent.at(-1).d.status,'initialized');assert.equal(attempt,2);
  for(const data of [{ok:false,error:'SECRET'},{ok:true,session_id:'wrong',request_id:'request1'},{ok:true,session_id:'session1',request_id:'wrong'}]){
    h=harness({post:()=>response(data)});await flush();h.emit(hello());h.emit(envelope());await flush();assert.equal(h.switched.length,0);assert.equal(h.sent.at(-1).d.status,'failed');
  }
  h=harness({switcher:async()=>{}});await flush();h.emit(hello());h.emit(envelope());await flush();assert.equal(h.sent.at(-1).d.status,'failed');
  h=harness({apiUrl:()=> 'https://evil.test/api/app-handoff/config'});await flush();assert.equal(h.calls.length,0);
  gate=deferred();h=harness({post:()=>gate.promise});await flush();h.emit(hello());h.emit(envelope());h.emit(envelope());await flush();assert.equal(h.calls.length,2);for(const f of h.timers.values()) f();await flush();assert.equal(h.sent.at(-1).d.status,'failed');assert.equal(h.switched.length,0);
  gate.resolve(response({ok:true,session_id:'session1',request_id:'request1'}));await flush();assert.equal(h.switched.length,0);
  // Old request resolution must not overwrite a newer authorized selection.
  const old=deferred();h=harness({post:d=>d.request_id==='request1' ? old.promise : response({ok:true,session_id:d.parameters.session_id,request_id:d.request_id})});
  await flush();h.emit(hello());h.emit(envelope());await flush();
  h.emit(hello({nonce:'nonce2',request_id:'request2'}));h.emit(envelope({nonce:'nonce2',request_id:'request2',parameters:{session_id:'session2'}}));await flush();
  old.resolve(response({ok:true,session_id:'session1',request_id:'request1'}));await flush();assert.deepEqual(h.switched,['session2']);assert.equal(h.S.sessionId,'session2');
  const oldError=deferred();h=harness({post:()=>oldError.promise});await flush();h.emit(hello());h.emit(envelope());await flush();h.emit(hello({nonce:'nonce2',request_id:'request2'}));oldError.reject(new Error('SECRET'));await flush();assert.equal(h.sent.filter(x=>x.d.type==='ack').length,0);
  h=harness();await flush();h.emit(hello());h.emit(envelope(),platform,{});h.emit(envelope(),'https://evil.test');await flush();assert.equal(h.calls.length,1);assert.equal(h.switched.length,0);
  h=harness({post:()=>({ok:true,redirected:true,json:async()=>({ok:true,session_id:'session1',request_id:'request1'})})});await flush();h.emit(hello());h.emit(envelope());await flush();assert.equal(h.switched.length,0);assert.equal(h.sent.at(-1).d.status,'failed');
  // Noncancellable switch holds the accepted generation until completion.
  for (const followup of ['hello-only','rejected','disconnected','authorized']) {
    const switchGate=deferred();
    h=harness({switcher:async(id,s)=>{if(id==='session1')await switchGate.promise;s.sessionId=id;},post:d=>response(d.request_id==='request2'&&followup==='rejected'?{ok:false}:{ok:true,session_id:d.parameters.session_id,request_id:d.request_id})});
    await flush();h.emit(hello());h.emit(envelope());await flush();assert.deepEqual(h.switched,['session1']);
    const nextHello=hello({nonce:'nonce2',request_id:'request2'});
    const nextEnvelope=envelope({nonce:'nonce2',request_id:'request2',parameters:{session_id:'session2'}});
    h.emit(nextHello);h.emit(nextEnvelope);await flush();
    assert.equal(h.sent.filter(x=>x.d.type==='ready').length,1,'new hello not accepted during switch');
    assert.equal(h.calls.length,2,'handoff before ready ignored');
    switchGate.resolve();await flush();assert.equal(h.S.sessionId,'session1');
    assert.deepEqual(h.sent.slice(-2).map(x=>[x.d.type,x.d.request_id]),[['ack','request1'],['ready','request2']]);
    if(followup==='authorized'||followup==='rejected') {
      h.emit(nextEnvelope);await flush();
      assert.equal(h.S.sessionId,followup==='authorized'?'session2':'session1');
      assert.equal(h.sent.at(-1).d.status,followup==='authorized'?'initialized':'failed');
    }
  }
  // One latest valid hello; malformed or wrong-origin messages cannot replace it.
  const switchGate=deferred();h=harness({switcher:async(id,s)=>{await switchGate.promise;s.sessionId=id;}});
  await flush();h.emit(hello());h.emit(envelope());await flush();
  h.emit(hello({nonce:'nonce2',request_id:'request2'}));h.emit(hello({nonce:'nonce3',request_id:'request3'}));
  h.emit(hello({nonce:'nonce4',request_id:'request4',extra:true}));h.emit(hello({nonce:'evil',request_id:'evil'}),'https://evil.test');
  switchGate.resolve();await flush();assert.equal(h.sent.at(-1).d.request_id,'request3');
  const failedSwitch=deferred();h=harness({switcher:()=>failedSwitch.promise});await flush();h.emit(hello());h.emit(envelope());await flush();h.emit(hello({nonce:'nonce2',request_id:'request2'}));failedSwitch.reject(new Error('SECRET'));await flush();
  assert.deepEqual(h.sent.slice(-2).map(x=>[x.d.type,x.d.status]),[['ack','failed'],['ready',undefined]]);
  // Duplicates always reauthorize, and restore a manually changed selection only after success.
  h=harness();await flush();h.emit(hello());h.emit(envelope());await flush();h.S.sessionId='manual';h.emit(envelope());await flush();
  assert.equal(h.calls.length,3);assert.deepEqual(h.switched,['session1','session1']);assert.equal(h.S.sessionId,'session1');assert.equal(h.sent.at(-1).d.status,'initialized');
  for(const denial of ['revoked','identity','expired','offline']) {
    let count=0;h=harness({post:()=>{if(++count===1)return response({ok:true,session_id:'session1',request_id:'request1'});if(denial==='offline')return Promise.reject(new Error('SECRET'));return response(denial==='identity'?{ok:true,session_id:'other',request_id:'request1'}:{ok:false,error:denial});}});
    await flush();h.emit(hello());h.emit(envelope());await flush();h.S.sessionId='manual';h.emit(envelope());await flush();
    assert.equal(count,2);assert.equal(h.S.sessionId,'manual');assert.equal(h.switched.length,1);assert.equal(h.sent.at(-1).d.status,'failed');
  }
  let authCount=0;h=harness({post:()=>response(++authCount===1?{ok:true,session_id:'session1',request_id:'request1'}:{ok:false})});
  await flush();h.emit(hello());h.emit(envelope());await flush();h.emit(envelope());await flush();assert.equal(authCount,2);assert.equal(h.sent.at(-1).d.status,'failed');
  // Local expiry is checked again after backend latency and after UI switching.
  for(const phase of ['authorization','switch']) {
    let clock=now;const expiryGate=deferred();
    h=harness({clock:()=>clock,post:()=>phase==='authorization'?expiryGate.promise:response({ok:true,session_id:'session1',request_id:'request1'}),switcher:async(id,s)=>{if(phase==='switch')await expiryGate.promise;s.sessionId=id;}});
    await flush();h.emit(hello());h.emit(envelope());await flush();clock=now+60001;
    expiryGate.resolve(phase==='authorization'?response({ok:true,session_id:'session1',request_id:'request1'}):undefined);await flush();
    assert.equal(h.sent.at(-1).d.status,'failed');assert.equal(h.switched.length,phase==='authorization'?0:1);
  }
  let clock=now;h=harness({clock:()=>clock});await flush();h.emit(hello());h.emit(envelope());await flush();clock=now+60001;h.emit(envelope());await flush();assert.equal(h.calls.length,2);assert.equal(h.sent.at(-1).d.status,'failed');
  console.log('app_handoff.cjs: PASS (origin/source, schema, binding, duplicate/retry, pending config, backend denial, stale generation, timeout, same-origin, deferred ready, hello-only/denied/disconnected, manual selection, reauthorization/revocation, expiry)');
})().catch(e=>{console.error(e);process.exitCode=1;});
