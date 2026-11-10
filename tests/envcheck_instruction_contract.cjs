const fs=require('fs'),vm=require('vm'),assert=require('assert');
const src=fs.readFileSync(require('path').join(__dirname,'../public/app.js'),'utf8');
const a=src.indexOf('function buildInstructionText(ask)'),b=src.indexOf('\nasync function renderTaskLog',a);
const ctx={S:{sessionId:'fixture-session',commitReq:'fixture-request',agentHandoffPath:'/private/fixture.json',placeDraft:{mode:'server',host:'fixture.invalid',exec_pc:'fixture-PC',data_root:'/data',script_root:'/scripts',work_root:'/results'}},placeDraftFromServer:()=>({})};
vm.createContext(ctx);vm.runInContext(src.slice(a,b),ctx);
for(const ask of ['识别','长'.repeat(4000)]){
 const text=ctx.buildInstructionText(ask);assert(text.length<=2000);
 for(const key of ['fixture-session','fixture-request','/private/fixture.json','X-Agent-Token','--transport local','--notebook-out'])assert(text.includes(key),key);
}
assert(src.includes('commitReq:null, agentHandoffPath:null'));
ctx.S.agentHandoffPath=null;assert(ctx.buildInstructionText('识别').includes('未交接回写凭据'));
console.log('PASS exact instruction identity/auth/server-Python fields survive truncation; missing handoff fails closed; record reset clears path');
