"""Copied real serve.R HTTP regression; identity injection exists ONLY in fixture db_api.R.
No production state/credentials/services are touched. Retains /tmp evidence.
"""
import json, os, shutil, socket, subprocess, tempfile, time, urllib.request, urllib.error
from pathlib import Path
APP = Path(__file__).resolve().parents[1]
ROOT = Path(tempfile.mkdtemp(prefix='pa-envhandoff-', dir='/tmp'))
FIX = ROOT / 'app'; FIX.mkdir()
for d in ('engine', 'public', 'scripts'):
    shutil.copytree(APP / d, FIX / d)
shutil.copy2(APP / 'serve.R', FIX / 'serve.R')
# Fixed mock of a successfully authenticated gateway viewer, fixture only.
f = FIX / 'engine/db_api.R'
f.write_text(f.read_text() + '''\n.fixture_original_owner <- db_api_owner
db_api_owner <- function(app_dir, req) {
 if (identical(req$headers[["x-fixture-human"]], "yes"))
   return(list(key="viewer:fixture-human",kind="viewer",external_id="fixture-human"))
 .fixture_original_owner(app_dir, req)
}\n''')
subprocess.run(['Rscript','--vanilla','-e', '''source('engine/db.R'); source('engine/agent_gate.R'); source('engine/sessions.R')
c <- db_connect('.'); db_init(c)
DBI::dbExecute(c,"INSERT INTO sessions(session_id,owner_key,name,mode,run_dir,created_at,updated_at) VALUES ('fixture-session','viewer:fixture-human','test','server','/nonexistent-fixture-result',datetime('now'),datetime('now'))")
sess_write('.',list(current='fixture-session',sessions=list(list(id='fixture-session',name='test',mode='server',run_dir='/nonexistent-fixture-result'))))
agent_token_issue('.','viewer:wrong-owner'); DBI::dbDisconnect(c)
'''],cwd=FIX,check=True,stdout=subprocess.DEVNULL)
with socket.socket() as s:
    s.bind(('127.0.0.1',0)); port=s.getsockname()[1]
base='http://127.0.0.1:'+str(port)
checks=[]
def check(name, good, **data):
    checks.append(dict(name=name,passed=bool(good),**data)); print(('PASS ' if good else 'FAIL ')+name,flush=True)
def request(path,body=None,token=None,human=False):
    h={'Content-Type':'application/json'}
    if token: h['X-Agent-Token']=token
    if human: h['X-Fixture-Human']='yes'
    req=urllib.request.Request(base+path,data=None if body is None else json.dumps(body).encode(),headers=h)
    try: r=urllib.request.urlopen(req,timeout=15)
    except urllib.error.HTTPError as e: r=e
    with r: return r.status,json.loads(r.read())
log=(ROOT/'serve.log').open('w')
p=subprocess.Popen(['Rscript','serve.R','--port',str(port),'--no-open'],cwd=FIX,stdout=log,stderr=log)
try:
    for _ in range(400):
        try:
            if request('/api/ping')[0]==200: break
        except OSError: pass
        if p.poll() is not None: raise RuntimeError('fixture serve exited')
        time.sleep(.1)
    status,commit=request('/api/place/commit',{'session_id':'fixture-session','mode':'server','server':{'host':'fixture.invalid','user':'fixture','data_root':'/data','script_root':'/scripts','work_root':'/nonexistent-fixture-result'}},human=True)
    check('authenticated human server commit',status==200,status=status)
    body={'session_id':'fixture-session','req_id':commit.get('request_id'),'host':'fixture.invalid','result':{'ok':False,'exit_code':17,'error':'fixture probe failed'}}
    wrong=json.loads(next((FIX/'state/agent_handoffs').glob('*.json')).read_text())['token']
    for name,token in [('no credentials',None),('wrong owner',wrong),('invalid token','pa_agent_invalid_fixture')]:
        st,b=request('/api/envcheck/import',body,token)
        check(name+' denied',st==403,status=st,code=b.get('code'))
    hf=commit.get('agent_handoff_path')
    check('commit supplies private handoff path without secret',isinstance(hf,str) and 'pa_agent_' not in json.dumps(commit))
    if hf:
        hp=Path(hf); hp=hp if hp.is_absolute() else FIX/hp
        token=json.loads(hp.read_text())['token']
        check('handoff mode 0600',hp.stat().st_mode & 0o777 == 0o600)
        before=json.loads((FIX/'config/agent_tokens.json').read_text())['tokens']
        st,again=request('/api/place/commit',{'session_id':'fixture-session','mode':'server'},human=True)
        after=json.loads((FIX/'config/agent_tokens.json').read_text())['tokens']
        check('human reuses active handoff',st==200 and len(before)==len(after) and again.get('agent_handoff_path')==hf)
        body['req_id']=again['request_id']
        st,b=request('/api/envcheck/import',dict(body,req_id='wrong-request'),token)
        check('wrong request denied',st==409,status=st,code=b.get('code'))
        st,b=request('/api/envcheck/import',body,token)
        check('correct handoff accepts failed result',st==200 and b.get('session')=='fixture-session' and b.get('req_id')==body['req_id'],status=st)
        st,readback=request('/api/sessions/state?id=fixture-session',token=token)
        result=readback.get('session',{}).get('identify',{}).get('result',{})
        check('failed result persisted and read through HTTP',st==200 and result.get('exit_code')==17 and result.get('ok') is False,status=st)
        st,agent_commit=request('/api/place/commit',{'session_id':'fixture-session','mode':'server'},token=token)
        end=json.loads((FIX/'config/agent_tokens.json').read_text())['tokens']
        check('agent commit never issues new token',st==200 and len(after)==len(end))
        # Existing agent credential may not recreate a deleted handoff.
        hp.unlink()
        st,b=request('/api/place/commit',{'session_id':'fixture-session','mode':'server'},token=token)
        end2=json.loads((FIX/'config/agent_tokens.json').read_text())['tokens']
        check('agent cannot mint missing handoff',st==200 and not b.get('agent_handoff_path') and not hp.exists() and len(end)==len(end2))
finally:
    p.terminate()
    try: p.wait(timeout=5)
    except subprocess.TimeoutExpired: p.kill(); p.wait()
    log.close()
    (ROOT/'evidence.json').write_text(json.dumps({'fixture_identity_injection':True,'checks':checks},indent=2))
    print('EVIDENCE '+str(ROOT))
raise SystemExit(0 if checks and all(c['passed'] for c in checks) else 1)
