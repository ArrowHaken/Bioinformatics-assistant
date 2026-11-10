import sys,json,pathlib
from playwright.sync_api import sync_playwright
root=pathlib.Path('/srv/catsco-agent/work/personal-analysis-app/app/public')
source=pathlib.Path(sys.argv[1]).read_text()
# Boot is excluded so every API call belongs to a controlled fixture.
source=source.replace('boot();','/* fixture boot */')
with sync_playwright() as p:
 b=p.chromium.launch(headless=True,args=['--no-sandbox'])
 page=b.new_page()
 page.route('**/*',lambda r:r.fulfill(status=200,content_type='text/html',body='<html></html>'))
 page.goto('http://fixture.local/')
 html=root.joinpath('index.html').read_text()
 import re
 html=re.sub(r'<script\b[^>]*>.*?</script>','',html,flags=re.S)
 page.set_content(html)
 page.add_script_tag(content=source)
 page.evaluate('''() => {
 window.fixture={id:'old'};
 window.fetch=async (url,opts) => {
  const path=new URL(url,location.href).pathname;
  let d={ok:true};
  if(path.endsWith('/sessions/new')){fixture.id='new';d.session={id:'new',name:'new'};}
  else if(path.endsWith('/sessions/switch')){fixture.id=JSON.parse(opts.body).id;d.session={id:fixture.id,name:fixture.id};}
  else if(path.endsWith('/sessions/state'))d={ok:true,current:fixture.id,session:{id:fixture.id,name:fixture.id,mode:'server',run_dir:'/fixture/'+fixture.id,steps:{},identify:{mode:'server',result:{ok:true}}},sessions:[]};
  else if(path.endsWith('/place'))d={ok:true,place:{mode:'server',server:{}}};
  else if(path.endsWith('/status'))d={ok:true,status:'idle',remote:true,steps:[]};
  return {ok:true,text:async()=>JSON.stringify(d)};
 };
 window.prompt=()=> 'new';
 for(const fn of ['renderAll','renderSessionsBar','renderSidenav','renderIdentify','renderPlace','loadRun','loadPlace'])window[fn]=async()=>{};
 S.sessionId='old';S.needsRemote=true;S.steps=[];
 S.delegated={ok:true,title:'旧分析任务',step:'plots',session_id:'old'};
 renderRun();
}''')
 print('before_transition:',page.locator('#runState').inner_text())
 for action in ['new','switch']:
  page.evaluate("""async action => { S.delegated={ok:true,title:'旧分析任务',step:'plots',session_id:S.sessionId}; if(action==='new')await newSession();else await switchSession('third');S.needsRemote=true;S.steps=[];renderRun(); }""",action)
  text=page.locator('#runState').inner_text()
  print(action+'_result:',text)
  if '旧分析任务' in text: print('FAIL: stale delegation remains');b.close();sys.exit(1)
 print('PASS: real DOM new/switch do not retain old delegation')
 b.close()
