#!/usr/bin/env python3
"""Read-only evidence collection for the actual PPTX and result ZIP.

Runs beside make_ppt.py on the execution host. It reads source files and archive
members, never executes analysis or changes a reported task status.
"""
import argparse
import hashlib
import importlib.util
import json
import posixpath
import re
import zipfile
from pathlib import Path, PurePosixPath
from types import SimpleNamespace
from xml.etree import ElementTree as ET

PPT_REL='11.report/analysis_report.pptx'
MAN_REL=PPT_REL+'.manifest.json'
ZIP_REL='11.report/analysis_results.zip'
NS={'a':'http://schemas.openxmlformats.org/drawingml/2006/main',
    'p':'http://schemas.openxmlformats.org/presentationml/2006/main',
    'r':'http://schemas.openxmlformats.org/officeDocument/2006/relationships'}

def file_hash(path, algorithm='md5'):
    h=hashlib.new(algorithm)
    with open(path,'rb') as f:
        for part in iter(lambda:f.read(1024*1024),b''):h.update(part)
    return h.hexdigest()

def member_hash(z,name):
    h=hashlib.md5()
    with z.open(name) as f:
        for part in iter(lambda:f.read(1024*1024),b''):h.update(part)
    return h.hexdigest()

def safe_rel(rel):
    p=PurePosixPath(rel)
    return bool(rel) and not p.is_absolute() and '..' not in p.parts and '\\' not in rel and not re.match(r'^[A-Za-z]:',rel)

def collect_ppt(work_root):
    root=Path(work_root).resolve();ppt=root/PPT_REL;mf=root/MAN_REL
    out={'rel':PPT_REL,'ok':False,'slides':0,'images':0,'crc_ok':False,'xml_ok':False,
         'source_match_ok':False,'missing_images':[],'hash_mismatches':[]}
    try:
        out['md5']=file_hash(ppt);out['manifest_rel']=MAN_REL;out['manifest_md5']=file_hash(mf)
        manifest=json.loads(mf.read_text(encoding='utf-8'))
        if manifest.get('schema')!='sc-ppt-sources-v1' or manifest.get('pptx_md5')!=out['md5']:
            raise ValueError('PPT source manifest does not identify this PPTX')
        module_path=Path(__file__).with_name('make_ppt.py')
        loader=importlib.util.spec_from_file_location('delivery_make_ppt',module_path)
        m=importlib.util.module_from_spec(loader);loader.loader.exec_module(m)
        spec=m.load_spec(str(module_path.with_name('slide_spec.json')))
        args=SimpleNamespace(result_dir=str(root/'10.figs'),stats_json=None,title='',date='',on_missing='error')
        plan,summary=m.build_plan(spec,args,m.build_dir_index(args.result_dir),[])
        expected=[p for p in plan if not p['skipped']]
        actual=manifest.get('slides',[])
        if len(actual)!=len(expected):raise ValueError('Slide manifest count differs from current report plan')
        if summary!=manifest.get('summary'):raise ValueError('Sample summary differs from actual source table')
        table=manifest.get('sample_table',{})
        if not safe_rel(table.get('source_rel','')) or file_hash(root/'10.figs'/table['source_rel'])!=table.get('md5'):
            raise ValueError('Sample table hash differs from report source manifest')
        with zipfile.ZipFile(ppt) as z:
            names=z.namelist()
            if len(names)!=len(set(names)):raise ValueError('Duplicate PPTX ZIP member')
            bad=z.testzip()
            if bad:raise ValueError('PPTX CRC error: '+bad)
            out['crc_ok']=True
            for name in names:
                if name.endswith(('.xml','.rels')):
                    tree=ET.fromstring(z.read(name))
                    for txbody in tree.iter():
                        if txbody.tag in ('{'+NS['p']+'}txBody','{'+NS['a']+'}txBody'):
                            children=list(txbody)
                            if not children or children[0].tag!='{'+NS['a']+'}bodyPr':
                                raise ValueError('Invalid DrawingML text body in '+name)
            out['xml_ok']=True
            presentation=ET.fromstring(z.read('ppt/presentation.xml'))
            slide_ids=presentation.findall('p:sldIdLst/p:sldId',NS)
            pres_rels={x.attrib['Id']:x.attrib.get('Target','') for x in ET.fromstring(z.read('ppt/_rels/presentation.xml.rels'))}
            out['slides']=len(slide_ids)
            if len(slide_ids)!=len(expected):raise ValueError('PPTX actual slide count differs from report plan')
            count=0
            for i,(want,got,slide_id) in enumerate(zip(expected,actual,slide_ids),1):
                if got.get('slide')!=i or got.get('spec_page')!=want['no']:raise ValueError('Slide order mismatch')
                slide=posixpath.normpath(posixpath.join('ppt',pres_rels[slide_id.attrib['{'+NS['r']+'}id']]))
                xml=ET.fromstring(z.read(slide))
                rel_name=posixpath.join(posixpath.dirname(slide),'_rels',posixpath.basename(slide)+'.rels')
                rels={x.attrib['Id']:posixpath.normpath(posixpath.join(posixpath.dirname(slide),x.attrib.get('Target',''))) for x in ET.fromstring(z.read(rel_name)) if x.attrib.get('TargetMode')!='External'}
                embeds=[rels.get(x.attrib.get('{'+NS['r']+'}embed')) for x in xml.findall('.//a:blip',NS)]
                gi=got.get('images',[])
                if len(gi)!=len(want['images']):raise ValueError('Slide '+str(i)+' image manifest count mismatch')
                for wi,im in zip(want['images'],gi):
                    src=Path(wi['path']).resolve();source_rel=src.relative_to(root/'10.figs').as_posix()
                    if im.get('source_rel')!=source_rel:raise ValueError('Slide '+str(i)+' uses wrong figure')
                    source_md5=file_hash(src);part=im.get('media_part','')
                    if part not in embeds:out['missing_images'].append({'slide':i,'source_rel':source_rel});continue
                    embedded_md5=member_hash(z,part)
                    if source_md5!=im.get('md5') or embedded_md5!=source_md5:
                        out['hash_mismatches'].append({'slide':i,'source_rel':source_rel})
                    count+=1
            out['images']=count;out['expected_images']=sum(len(p['images']) for p in expected)
            out['source_match_ok']=not out['missing_images'] and not out['hash_mismatches'] and count==out['expected_images'] and count>0
        out['ok']=out['source_match_ok'];out['why']='Actual slide relationships, embedded image hashes and source tables checked' if out['ok'] else 'PPTX source mismatch'
    except (Exception,SystemExit) as e:out['why']=str(e)
    return out

def collect_zip(work_root):
    root=Path(work_root).resolve();archive=root/ZIP_REL
    out={'rel':ZIP_REL,'ok':False,'crc_ok':False,'entries':0,'figures':0,'has_pptx':False,
         'has_pptx_manifest':False,'source_match_ok':False,'missing_files':[],'unexpected_files':[],'hash_mismatches':[]}
    try:
        out['md5']=file_hash(archive)
        files=sorted((root/'10.figs').rglob('*'))+[root/PPT_REL,root/MAN_REL]
        expected={}
        for p in files:
            if p.is_symlink():raise ValueError('Source file is a symbolic link: '+p.name)
            if p.is_file():expected[p.relative_to(root).as_posix()]=p
        if PPT_REL not in expected or MAN_REL not in expected:raise ValueError('PPTX or source manifest missing')
        prefix=root.as_posix().lstrip('/')+'/'
        with zipfile.ZipFile(archive) as z:
            names=[x.filename for x in z.infolist() if not x.is_dir()]
            if len(names)!=len(set(names)):raise ValueError('Duplicate ZIP member')
            if any(not safe_rel(n) for n in names):raise ValueError('Unsafe archive path')
            bad=z.testzip()
            if bad:raise ValueError('ZIP CRC error: '+bad)
            out['crc_ok']=True;out['entries']=len(names)
            mapped={}
            for name in names:
                rel=name[len(prefix):] if name.startswith(prefix) else name
                if rel in mapped:raise ValueError('Duplicate normalized ZIP member')
                mapped[rel]=name
            out['missing_files']=sorted(set(expected)-set(mapped))
            out['unexpected_files']=sorted(set(mapped)-set(expected))
            out['figures']=sum(n.lower().endswith('.png') for n in mapped if n.startswith('10.figs/'))
            out['has_pptx']=PPT_REL in mapped;out['has_pptx_manifest']=MAN_REL in mapped
            for rel in sorted(set(expected)&set(mapped)):
                if member_hash(z,mapped[rel])!=file_hash(expected[rel]):out['hash_mismatches'].append(rel)
            out['expected_files']=len(expected);out['matched_files']=len(set(expected)&set(mapped))-len(out['hash_mismatches'])
            out['source_match_ok']=not out['missing_files'] and not out['unexpected_files'] and not out['hash_mismatches']
        out['ok']=out['source_match_ok'] and out['figures']>0
        out['why']='CRC and every current figure/report file hash checked' if out['ok'] else 'Archive does not match current source files'
    except Exception as e:out['why']=str(e)
    return out

if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--work-root',required=True);parser.add_argument('--step',choices=['ppt','pack'],required=True)
    args=parser.parse_args()
    evidence=collect_ppt(args.work_root) if args.step=='ppt' else collect_zip(args.work_root)
    print(json.dumps({('pptx_structure' if args.step=='ppt' else 'zip_structure'):evidence},ensure_ascii=False))
