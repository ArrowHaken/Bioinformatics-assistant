#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""sc-ppt-kit / make_ppt.py

把单细胞分析结果（六套结果目录）自动生成与客户交付 PPT 同款的 .pptx 文件。

实现要点
--------
* 只用 Python 标准库（zipfile / re / json / os / argparse ...），不依赖 python-pptx / officer。
* 以客户交付的 PPT 作为版式母版：读取它的 slideMasters / slideLayouts / theme / media，
  按 slide_spec.json 定义的 22 页页序，逐页由母版里的“版式皮肤”（cover / toc / table / content / end）
  复制出 slideN.xml，替换文本与图片引用，最后用 zipfile 重新打包。
* 文本替换采用“保留原段落属性（a:pPr / a:defRPr）只换文字”的方式，因此字号、字体、颜色与客户原稿一致。
* 图片缺失时：默认直接报错退出；--on-missing skip 时跳过该页并在报告中列出，绝不静默留空。

典型用法
--------
    # 只看计划
    python3 make_ppt.py --result-dir /path/to/result --template 客户模板.pptx --dry-run

    # 正式生成
    python3 make_ppt.py --result-dir /path/to/result --template 客户模板.pptx \\
        --config pipeline.json --title "<这次交付的标题>" --date 2026-01-22
"""

from __future__ import annotations

import argparse
import datetime as _dt
import fnmatch
import glob as _glob
import hashlib
import json
import os
import re
import struct
import sys
import zipfile

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
DEFAULT_SPEC = os.path.join(SCRIPT_DIR, "slide_spec.json")

# 母版里左上角那个客户 logo（每页都有），用 descr 里的关键字识别
LOGO_DESCR_KEY = "微信图片"

# 图片扩展名 -> Content_Types 里的默认类型
EXT_CONTENT_TYPE = {
    ".png": "image/png",
    ".jpeg": "image/jpeg",
    ".jpg": "image/jpeg",
    ".gif": "image/gif",
    ".bmp": "image/bmp",
    ".tif": "image/tiff",
    ".tiff": "image/tiff",
    ".emf": "image/x-emf",
    ".wmf": "image/x-wmf",
}

REL_NS = "http://schemas.openxmlformats.org/package/2006/relationships"
CT_SLIDE = "application/vnd.openxmlformats-officedocument.presentationml.slide+xml"

SHAPE_TAGS = "p:sp|p:pic|p:graphicFrame|p:grpSp|p:cxnSp"


# --------------------------------------------------------------------------------------
# 基础工具
# --------------------------------------------------------------------------------------
def norm_key(text):
    """归一化目录/字段名：只留字母数字与汉字，去掉空格标点，统一小写。

    用 str.isalnum() 保持 Unicode 语义，因此“样本名”“线粒体中位数(%)”“nFeature_RNA”
    都能收敛成稳定键（后两者分别是“样本名”“线粒体中位数”“nfeaturerna”）。
    """
    return "".join(ch for ch in str(text).lower() if ch.isalnum())


def xml_escape(text):
    return (str(text).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;"))


def xml_text(text):
    """XML 转义 + 换行转 &#10;（客户原稿就是用这种方式表示段内换行）。"""
    return xml_escape(text).replace("\r\n", "\n").replace("\r", "\n").replace("\n", "&#10;")


def log(msg):
    sys.stdout.write(msg + "\n")
    sys.stdout.flush()


def read_zip_parts(path):
    parts = {}
    with zipfile.ZipFile(path, "r") as zf:
        for info in zf.infolist():
            if info.is_dir():
                continue
            parts[info.filename] = zf.read(info.filename)
    return parts


def fmt_num(value, digits):
    try:
        return ("%." + str(digits) + "f") % float(value)
    except (TypeError, ValueError):
        return str(value)


# --------------------------------------------------------------------------------------
# 幻灯片 XML 里的形状定位与替换
# --------------------------------------------------------------------------------------
_SHAPE_RE = re.compile(r"<(/?)(" + SHAPE_TAGS + r")(?:\s[^>]*)?(/>|>)")


def top_level_shapes(body):
    """在 spTree 内容里找出顶层形状，返回 [{'tag','start','end','xml'}...]（相对 body 的偏移）。"""
    shapes = []
    stack = []
    for m in _SHAPE_RE.finditer(body):
        closing = bool(m.group(1))
        leaf = m.group(3) == "/>"
        tag = m.group(2)
        if closing:
            if stack and stack[-1][0] == tag:
                _, start = stack.pop()
                if not stack:
                    shapes.append({"tag": tag, "start": start, "end": m.end()})
            continue
        if leaf:
            continue
        stack.append((tag, m.start()))
    return shapes


def shape_name(shape_xml):
    m = re.search(r'<p:cNvPr id="(\d+)" name="([^"]*)"', shape_xml)
    if m:
        return m.group(2)
    m = re.search(r'name="([^"]*)"', shape_xml)
    return m.group(1) if m else ""


def shape_id(shape_xml):
    m = re.search(r'<p:cNvPr id="(\d+)"', shape_xml)
    return m.group(1) if m else None


def shape_descr(shape_xml):
    m = re.search(r'descr="([^"]*)"', shape_xml)
    return m.group(1) if m else None


def shape_embed(shape_xml):
    m = re.search(r'r:embed="([^"]+)"', shape_xml)
    return m.group(1) if m else None


def collect_shapes(slide_xml):
    """返回 slide_xml 中所有顶层形状及其在整段 XML 中的绝对 span。"""
    key = "<p:spTree>"
    end_key = "</p:spTree>"
    i = slide_xml.find(key)
    j = slide_xml.rfind(end_key)
    if i < 0 or j < 0:
        return []
    i += len(key)
    out = []
    for sh in top_level_shapes(slide_xml[i:j]):
        sh = dict(sh)
        sh["start"] += i
        sh["end"] += i
        sh["xml"] = slide_xml[sh["start"]:sh["end"]]
        sh["name"] = shape_name(sh["xml"])
        sh["descr"] = shape_descr(sh["xml"])
        sh["embed"] = shape_embed(sh["xml"])
        out.append(sh)
    return out


def apply_replacements(slide_xml, replacements):
    """replacements: [(start, end, new_xml)]，从后往前替换避免偏移错乱。"""
    for start, end, new_xml in sorted(replacements, key=lambda t: t[0], reverse=True):
        slide_xml = slide_xml[:start] + new_xml + slide_xml[end:]
    return slide_xml


def find_shape(shapes, name=None, embed=None, descr=None, tag=None):
    for sh in shapes:
        if name is not None and sh["name"] != name:
            continue
        if tag is not None and sh["tag"] != tag:
            continue
        if embed is not None and sh["embed"] != embed:
            continue
        if descr is not None and sh["descr"] != descr:
            continue
        return sh
    return None


# --- 整段文本框重写 -------------------------------------------------------------------
def _paragraphs(txbody_inner):
    return re.findall(r"<a:p\s*/>|<a:p>.*?</a:p>", txbody_inner, re.S)


def _paragraph_ppr(para):
    m = re.search(r"<a:pPr\b[^>]*(?:/>|>.*?</a:pPr>)", para, re.S)
    if m:
        return m.group(0)
    m = re.search(r"<a:pPr(?:\s[^>]*)?/>", para)
    return m.group(0) if m else ""


def _set_ppr_size(ppr, size):
    if not ppr or size is None:
        return ppr
    if re.search(r'<a:defRPr[^>]*\bsz="\d+"', ppr):
        return re.sub(r'(<a:defRPr[^>]*?)\bsz="\d+"', r'\1sz="%d"' % int(size), ppr, count=1)
    if "<a:defRPr" in ppr:
        return re.sub(r"(<a:defRPr)", r'\1 sz="%d"' % int(size), ppr, count=1)
    return ppr


def rewrite_txbody(shape_xml, paragraphs):
    """按 paragraphs=[{'text':..,'sz':..}] 重写形状的 <p:txBody>，段落属性沿用原稿模板。"""
    m = re.search(r"<p:txBody>(.*)</p:txBody>", shape_xml, re.S)
    if not m:
        return shape_xml
    inner = m.group(1)
    paras = _paragraphs(inner)

    lead_empty = 0
    tail_empty = 0
    tmpl_ppr = []
    tmpl_rpr = []
    seen_text = False
    for para in paras:
        has_text = "<a:t>" in para
        if has_text:
            seen_text = True
            tmpl_ppr.append(_paragraph_ppr(para))
            run_pr = re.search(r"<a:rPr\b[^>]*(?:/>|>.*?</a:rPr>)", para, re.S)
            tmpl_rpr.append(run_pr.group(0) if run_pr else "<a:rPr/>")
        else:
            if not seen_text:
                lead_empty += 1
            else:
                tail_empty += 1
    if not tmpl_ppr:
        tmpl_ppr = [""]
        tmpl_rpr = ["<a:rPr/>"]

    new_paras = []
    for idx, item in enumerate(paragraphs):
        text = item.get("text", "")
        ppr = tmpl_ppr[idx] if idx < len(tmpl_ppr) else tmpl_ppr[-1]
        ppr = _set_ppr_size(ppr, item.get("sz"))
        rpr = tmpl_rpr[idx] if idx < len(tmpl_rpr) else tmpl_rpr[-1]
        if item.get("sz") is not None:
            if re.search(r'\bsz="\d+"', rpr):
                rpr = re.sub(r'\bsz="\d+"', 'sz="%d"' % int(item['sz']), rpr, count=1)
            else:
                rpr = rpr.replace('<a:rPr', '<a:rPr sz="%d"' % int(item['sz']), 1)
        new_paras.append("<a:p>" + ppr + "<a:r>" + rpr + "<a:t>" + xml_text(text) + "</a:t></a:r></a:p>")

    # CT_TextBody requires bodyPr before paragraphs. Preserve the template's
    # body properties/list style; dropping them makes PowerPoint reject the deck
    # even though a generic XML parser can read it.
    body_pr = re.search(r"<a:bodyPr\b[^>]*(?:/>|>.*?</a:bodyPr>)", inner, re.S)
    list_style = re.search(r"<a:lstStyle\b[^>]*(?:/>|>.*?</a:lstStyle>)", inner, re.S)
    prefix = (body_pr.group(0) if body_pr else "<a:bodyPr/>")
    prefix += list_style.group(0) if list_style else "<a:lstStyle/>"
    new_inner = prefix + ("<a:p/>" * lead_empty) + "".join(new_paras) + ("<a:p/>" * tail_empty)
    return shape_xml[:m.start()] + "<p:txBody>" + new_inner + "</p:txBody>" + shape_xml[m.end():]


def set_xfrm(shape_xml, off, ext):
    """设置形状位置（EMU）。off/ext 为 (x, y) / (cx, cy)。"""
    if not off or not ext:
        return shape_xml
    m = re.search(r"<p:spPr>.*?</p:spPr>", shape_xml, re.S)
    if not m:
        return shape_xml
    block = m.group(0)
    off_tag = '<a:off x="%d" y="%d"/>' % (int(off[0]), int(off[1]))
    ext_tag = '<a:ext cx="%d" cy="%d"/>' % (int(ext[0]), int(ext[1]))
    new_block = block
    has_off = re.search(r"<a:off(?:\s[^>]*)?/>", new_block) is not None
    has_ext = re.search(r"<a:ext(?:\s[^>]*)?/>", new_block) is not None
    if has_off:
        new_block = re.sub(r"<a:off(?:\s[^>]*)?/>", off_tag, new_block, count=1)
    if has_ext:
        new_block = re.sub(r"<a:ext(?:\s[^>]*)?/>", ext_tag, new_block, count=1)
    if not (has_off and has_ext):
        if "<a:xfrm" in new_block:
            new_block = re.sub(r"<a:xfrm(?:\s[^>]*)?>", lambda mm: mm.group(0) + off_tag + ext_tag, new_block, count=1)
        else:
            new_block = new_block.replace("<p:spPr>", "<p:spPr><a:xfrm>" + off_tag + ext_tag + "</a:xfrm>", 1)
    if new_block == block:
        return shape_xml
    return shape_xml[:m.start()] + new_block + shape_xml[m.end():]


def set_embed(shape_xml, rel_id, descr=None):
    out = re.sub(r'r:embed="[^"]*"', 'r:embed="%s"' % rel_id, shape_xml, count=1)
    if descr is not None:
        if 'descr="' in out:
            out = re.sub(r'descr="[^"]*"', 'descr="%s"' % xml_escape(descr), out, count=1)
        else:
            out = out.replace('<p:cNvPr ', '<p:cNvPr descr="%s" ' % xml_escape(descr), 1)
    return out


def image_pixel_size(path):
    """读取 PNG/JPEG/GIF/BMP 的像素宽高；未知格式直接报错，不静默拉伸。"""
    with open(path, "rb") as f:
        head = f.read(32)
        if head.startswith(b"\x89PNG\r\n\x1a\n") and len(head) >= 24:
            return struct.unpack(">II", head[16:24])
        if head[:6] in (b"GIF87a", b"GIF89a") and len(head) >= 10:
            return struct.unpack("<HH", head[6:10])
        if head[:2] == b"BM" and len(head) >= 26:
            return (struct.unpack("<I", head[18:22])[0],
                    struct.unpack("<I", head[22:26])[0])

    if head[:2] == b"\xff\xd8":
        with open(path, "rb") as f:
            data = f.read()
        i = 2
        while i + 9 < len(data):
            if data[i] != 0xFF:
                i += 1
                continue
            marker = data[i + 1]
            if marker in (0xD8, 0xD9) or 0xD0 <= marker <= 0xD7:
                i += 2
                continue
            seg_len = struct.unpack(">H", data[i + 2:i + 4])[0]
            if marker in (0xC0, 0xC1, 0xC2, 0xC3, 0xC5, 0xC6, 0xC7,
                          0xC9, 0xCA, 0xCB, 0xCD, 0xCE, 0xCF):
                h, w = struct.unpack(">HH", data[i + 5:i + 9])
                return w, h
            i += 2 + seg_len

    raise SystemExit("无法读取图片像素尺寸：%s" % path)


def set_src_rect(shape_xml, src_w, src_h, ext):
    """按源图与框的比例写 a:srcRect，填充时只裁切、不改变像素纵横比。"""
    if not src_w or not src_h or not ext or not ext[0] or not ext[1]:
        return shape_xml
    src_ratio = float(src_w) / float(src_h)
    box_ratio = float(ext[0]) / float(ext[1])
    left = top = right = bottom = 0
    if src_ratio > box_ratio:
        crop = 0.5 * (1.0 - box_ratio / src_ratio) * 100000.0
        left = right = int(round(crop))
    elif box_ratio > src_ratio:
        crop = 0.5 * (1.0 - src_ratio / box_ratio) * 100000.0
        top = bottom = int(round(crop))
    rect = '<a:srcRect l="%d" t="%d" r="%d" b="%d"/>' % (left, top, right, bottom)

    out = re.sub(r"<a:srcRect\b[^>]*/>", "", shape_xml, count=1)
    m = re.search(r"<a:blip\b[^>]*/>", out)
    if m:
        return out[:m.end()] + rect + out[m.end():]
    m = re.search(r"<a:blipFill\b[^>]*>", out)
    if m:
        return out[:m.end()] + rect + out[m.end():]
    return shape_xml


# --------------------------------------------------------------------------------------
# 结果目录与素材解析
# --------------------------------------------------------------------------------------
def build_dir_index(root, max_depth=3):
    """把结果目录下所有子目录建成 {归一化相对名: 绝对路径}，用于宽容匹配六套结果目录。"""
    index = {}
    if not root or not os.path.isdir(root):
        return index
    index[""] = root
    root_depth = root.rstrip(os.sep).count(os.sep)
    for dirpath, dirnames, _ in os.walk(root):
        depth = dirpath.rstrip(os.sep).count(os.sep) - root_depth
        if depth >= max_depth:
            dirnames[:] = []
        rel = os.path.relpath(dirpath, root)
        if rel == ".":
            continue
        index.setdefault(norm_key(rel), dirpath)
        index.setdefault("~" + norm_key(os.path.basename(dirpath)), dirpath)
    return index


def resolve_dir(index, target):
    """宽容地把 spec 里的逻辑目录名（QC / cluster / markgene/heatmap ...）解析成真实路径。"""
    if not index:
        return None
    key = norm_key(target)
    if not key:
        return index.get("")
    if key in index:
        return index[key]
    # 容错：目录名带编号前缀（01.cluster / 02_markgene）
    best = None
    for k, path in index.items():
        if k.startswith("~") or not k:
            continue
        if k.endswith(key) or key in k:
            score = abs(len(k) - len(key))
            if best is None or score < best[0]:
                best = (score, path)
    if best:
        return best[1]
    if ("~" + key) in index:
        return index["~" + key]
    return None


def iter_image_candidates(root, max_depth=3):
    """递归列出目录下的图片文件（含子目录，如 diff_enrich/diff/<组>/xxx_volcano.png）。"""
    out = []
    if not root or not os.path.isdir(root):
        return out
    root_depth = root.rstrip(os.sep).count(os.sep)
    for dirpath, dirnames, filenames in os.walk(root):
        depth = dirpath.rstrip(os.sep).count(os.sep) - root_depth
        if depth >= max_depth:
            dirnames[:] = []
        for name in filenames:
            ext = os.path.splitext(name)[1].lower()
            if ext in EXT_CONTENT_TYPE:
                out.append(os.path.join(dirpath, name))
    return out


def _pretty_cmp(raw):
    """把 MicroplasticsvsMock 这类比较名还原成可读形式。

    比较名一律从文件名解析，不写死任何分组名；解析不出就原样返回，
    由调用方决定退回原 stem。这里只做字符串整理，不做任何推断。
    """
    m = re.match(r"^(.+?)vs(.+)$", str(raw or "").strip(), re.I)
    if m:
        return "%s vs %s" % (m.group(1).strip(), m.group(2).strip())
    return str(raw or "").strip()


def example_label(path):
    """从最终选中的图给出"本页示例"短标签。

    为什么必须由选中路径派生：示例图是按 patterns 通配选出来的，
    实际落到哪个簇、哪个细胞类型、哪一对比较由产物决定。若文字写死
    "簇 1"或写死分组名，选中的却是别的图，读者照着核对就会得出不同结论。
    这里只做名称解析，不重新计算、不推断任何科学结论。
    """
    if not path:
        return ""
    stem = os.path.splitext(os.path.basename(str(path)))[0]
    m = re.match(r"^featureplot_cluster(\d+)$", stem, re.I)
    if m:
        return "本页示例：簇%s的 marker 特征图" % m.group(1)
    m = re.match(r"^violin_cluster(\d+)$", stem, re.I)
    if m:
        return "本页示例：簇%s的 marker 小提琴图" % m.group(1)
    # 差异/富集图命名：<细胞类型>_<比较名>_<GO_BP|KEGG|volcano>。
    # 注意 GO_BP 自带下划线，不能直接 rsplit 三段，必须先剥已知后缀。
    low = stem.lower()
    kind = ""
    for suffix, kname in (("_go_bp", "GO_BP"), ("_kegg", "KEGG"), ("_volcano", "volcano")):
        if low.endswith(suffix):
            kind = kname
            stem = stem[: -len(suffix)]
            break
    if kind:
        ct, sep, cmp_raw = stem.rpartition("_")
        if sep and ct.strip():
            ct_disp = ct.replace("_", " ").strip()
            cmp_disp = _pretty_cmp(cmp_raw)
            if kind == "GO_BP":
                return "本页示例：%s（%s）的 GO BP 富集" % (ct_disp, cmp_disp)
            if kind == "KEGG":
                return "本页示例：%s（%s）的 KEGG 富集" % (ct_disp, cmp_disp)
            return "本页示例：%s（%s）的火山图" % (ct_disp, cmp_disp)
        # 剥了后缀却拆不出细胞类型：仍可给出比较名，但不编细胞类型。
        if "vs" in stem.lower():
            tail = {"GO_BP": "的 GO BP 富集", "KEGG": "的 KEGG 富集"}.get(kind, "的火山图")
            return "本页示例：%s%s" % (_pretty_cmp(stem), tail)
        return "本页示例：%s" % os.path.splitext(os.path.basename(str(path)))[0]
    # 解析不出且像差异图：直接展示去掉扩展名的原 stem，宁可朴素也不猜。
    if "vs" in stem.lower():
        return "本页示例：%s" % stem
    return ""


def find_image_file(candidates, patterns):
    """按 patterns 顺序在候选目录（含其子目录）里递归找图。

    返回 (path, tried, fallback)：
      fallback=False 表示命中是名字层面的确定匹配；
      fallback=True  表示只能靠纯通配（如 *.png）兜底，报告里要明确标注期望名与替代名。
    """
    tried = []
    for pattern in patterns:
        needle = pattern.lower()
        # An extension is not a figure identity. Never substitute an arbitrary
        # PNG for a missing volcano, annotation or enrichment figure.
        stem = os.path.splitext(os.path.basename(needle))[0]
        if not re.search(r"[a-z0-9\u4e00-\u9fff]", stem):
            continue
        for cand in candidates:
            if not cand or not os.path.isdir(cand):
                continue
            tried.append(os.path.join(cand, pattern))
            hits = [p for p in iter_image_candidates(cand)
                    if fnmatch.fnmatch(os.path.basename(p).lower(), needle)]
            if not hits:
                continue
            # 排序优先级沿用原来三条（png 优先、路径短优先、文件名短优先），
            #   末尾补"最终相对路径字典序"做 tie-breaker。
            #   为什么必须补：同长度候选原来靠 os.walk 的遍历顺序决定，
            #   而遍历顺序不保证稳定 —— 同一份产物两次生成可能选中不同图，
            #   报告却写着同一个示例，读者据此核对会得出不同结论。
            hits.sort(key=lambda p: (0 if p.lower().endswith(".png") else 1,
                                     len(os.path.relpath(p, cand)),
                                     len(os.path.basename(p)),
                                     os.path.relpath(p, cand).replace(os.sep, "/")))
            return hits[0], tried, False
    return None, tried, False


def read_text_any(path):
    with open(path, "rb") as fh:
        raw = fh.read()
    for enc in ("utf-8-sig", "utf-8", "gb18030", "utf-16"):
        try:
            return raw.decode(enc)
        except (UnicodeDecodeError, LookupError):
            continue
    return raw.decode("utf-8", "replace")


def parse_table_file(path):
    """把样本统计表读成 [[cell, ...], ...]。支持 tsv/csv/txt 以及 HTML 伪装的 xls。"""
    text = read_text_any(path)
    stripped = text.lstrip()
    if stripped.startswith("<"):
        rows = []
        for tr in re.findall(r"<tr[^>]*>(.*?)</tr>", text, re.S | re.I):
            cells = []
            for td in re.findall(r"<t[dh][^>]*>(.*?)</t[dh]>", tr, re.S | re.I):
                cell = re.sub(r"<[^>]+>", "", td)
                cell = (cell.replace("&nbsp;", " ").replace("&amp;", "&")
                            .replace("&lt;", "<").replace("&gt;", ">").strip())
                cells.append(cell)
            if cells:
                rows.append(cells)
        return rows

    lines = [ln for ln in text.splitlines() if ln.strip() != ""]
    if not lines:
        return []
    head = lines[0]
    delim = "\t"
    if head.count("\t") == 0:
        if head.count(",") >= head.count(";"):
            delim = ","
        else:
            delim = ";"
    rows = []
    for ln in lines:
        cells = [c.strip().strip('"') for c in ln.split(delim)]
        rows.append(cells)
    return rows


def median(values):
    vals = sorted(v for v in values if v is not None)
    if not vals:
        return None
    n = len(vals)
    mid = n // 2
    if n % 2:
        return vals[mid]
    return (vals[mid - 1] + vals[mid]) / 2.0


def to_float(cell):
    if cell is None:
        return None
    m = re.search(r"-?\d+(?:\.\d+)?", str(cell).replace(",", ""))
    if not m:
        return None
    try:
        return float(m.group(0))
    except ValueError:
        return None


def analyze_table(rows, spec):
    """从样本统计表里抽出样本名、六个指标行与汇总数字。"""
    table_spec = spec.get("table_spec", {})
    sample_keys = set(norm_key(k) for k in table_spec.get("sample_keys", []))
    header_idx = None
    for i, row in enumerate(rows):
        if not row:
            continue
        if norm_key(row[0]) in sample_keys:
            header_idx = i
            break
    result = {"samples": [], "rows": [], "n_samples": 0}
    if header_idx is None:
        return result
    header = rows[header_idx]
    # The plotting stage writes one sample per row (sample, cell_number).
    # Transpose only when the other headers are recognized metric names;
    # preserve the original wide table format used by existing templates.
    known_metrics = {norm_key(k) for mr in table_spec.get("metric_rows", [])
                     for k in mr.get("keys", [])}
    metric_columns = [i for i in range(1, len(header))
                      if norm_key(header[i]) in known_metrics]
    if metric_columns:
        data_rows = [r for r in rows[header_idx + 1:] if r and str(r[0]).strip()]
        samples = [str(r[0]).strip() for r in data_rows]
        if len(samples) != len(set(samples)):
            raise SystemExit("样本统计表存在重复样本，停止生成")
        wide = [[header[0]] + samples]
        for i in metric_columns:
            if any(len(r) <= i or not str(r[i]).strip() for r in data_rows):
                raise SystemExit("样本统计表有缺失指标，停止生成")
            wide.append([header[i]] + [r[i] for r in data_rows])
        return analyze_table(wide, spec)
    result["samples"] = [c for c in header[1:] if str(c).strip() != ""]

    metric_rows = table_spec.get("metric_rows", [])
    labels = [(idx, set(norm_key(k) for k in mr.get("keys", [])), mr.get("label", ""))
              for idx, mr in enumerate(metric_rows)]
    found = {}
    for row in rows[header_idx + 1:]:
        if not row:
            continue
        key = norm_key(row[0])
        for idx, keys, label in labels:
            if key in keys and idx not in found:
                found[idx] = {
                    "label": label,
                    "values": row[1: 1 + len(result["samples"])],
                }
                break
    result["rows"] = [found[i] for i in sorted(found)]
    result["n_samples"] = len(result["samples"])
    return result


def summarize(table_info, stats_json):
    """得出封面下方那段统计文字的变量。"""
    out = {
        "n_samples": None,
        "total_cells": None,
        "gene_median": None,
        "mt_median": None,
    }
    row_map = {}
    for row in table_info.get("rows", []):
        row_map[norm_key(row["label"])] = row["values"]

    def _values(*labels):
        for label in labels:
            if norm_key(label) in row_map:
                return row_map[norm_key(label)]
        return []

    filtered = [to_float(v) for v in _values("本次对象细胞数", "高质量细胞数")]
    filtered = [v for v in filtered if v is not None]
    raw = [to_float(v) for v in _values("原始细胞数")]
    raw = [v for v in raw if v is not None]
    if filtered:
        out["total_cells"] = int(sum(filtered))
    elif raw:
        out["total_cells"] = int(sum(raw))

    genes = [to_float(v) for v in _values("基因中位数")]
    genes = [v for v in genes if v is not None]
    if genes:
        out["gene_median"] = median(genes)

    mts = [to_float(v) for v in _values("线粒体中位数(%)", "线粒体中位数")]
    mts = [v for v in mts if v is not None]
    if mts:
        out["mt_median"] = median(mts)

    if table_info.get("n_samples"):
        out["n_samples"] = table_info["n_samples"]

    if stats_json:
        for key in ("n_samples", "total_cells", "gene_median", "mt_median"):
            if stats_json.get(key) is not None:
                out[key] = stats_json[key]
    return out


# --------------------------------------------------------------------------------------
# 生成计划
# --------------------------------------------------------------------------------------
class Asset(object):
    def __init__(self, descr, spec_img, resolved, tried):
        self.descr = descr
        self.spec = spec_img
        self.path = resolved
        self.tried = tried


def build_plan(spec, args, dir_index, report):
    deck = spec["deck"]
    stats_json = {}
    if args.stats_json:
        # 缺失不致命：提取阶段没跑或没产出时，退回从表格推导。
        #   原来直接 open()，文件不在就整个 PPT 阶段崩掉 ——
        #   一个可选的数据增强项，不该有阻断交付的能力。
        if os.path.isfile(args.stats_json):
            try:
                with open(args.stats_json, "r", encoding="utf-8") as fh:
                    stats_json = json.load(fh)
            except (ValueError, OSError) as e:
                report.append("WARN stats-json 读取失败，改用表格推导：%s" % e)
                stats_json = {}
        else:
            report.append("WARN stats-json 不存在，改用表格推导：%s" % args.stats_json)

    table_spec = spec.get("table_spec", {})
    table_rows = None
    table_source = None
    if stats_json.get("table_rows"):
        table_rows = [list(r) for r in stats_json["table_rows"]]
        table_source = "<stats-json:table_rows>"
    else:
        table_path = None
        for fname in table_spec.get("filenames", []):
            cand = os.path.join(args.result_dir, fname) if args.result_dir else None
            if cand and os.path.isfile(cand):
                table_path = cand
                break
        if table_path is None and args.result_dir:
            for pattern in table_spec.get("glob_patterns", []):
                hits = sorted(_glob.glob(os.path.join(args.result_dir, pattern)))
                if hits:
                    table_path = hits[0]
                    break
        if table_path:
            table_rows = parse_table_file(table_path)
            table_source = table_path

    table_info = analyze_table(table_rows or [], spec)
    if table_rows and (not table_info["samples"] or not table_info["rows"]):
        raise SystemExit("样本统计表没有可识别的样本和指标，停止生成")
    summary = summarize(table_info, stats_json)

    plan = []
    for page in spec["pages"]:
        entry = {
            "no": page["no"],
            "type": page["type"],
            "skin": page["skin"],
            "section": page.get("section"),
            "chapter": deck["chapters"].get(page.get("section"), "") if page.get("section") else "",
            "title": page.get("title"),
            "body": page.get("body"),
            "texts": [],
            "images": [],
            "missing": [],
            "required_assets": [],
            "fallbacks": [],
            "table": None,
            "dir_note": table_spec.get("dir_note", ""),
            "skipped": False,
        }

        for item in page.get("texts", []):
            text = item["text"]
            text = (text.replace("{title}", args.title)
                        .replace("{date}", args.date)
                        .replace("{toc_text}", deck["toc_text"]))
            entry["texts"].append({"slot": item["slot"], "text": text})

        if entry["type"] == "content":
            for item in page.get("body", {}).get("paragraphs", []):
                pass  # body 文案在渲染阶段直接用 spec 原文
            for img in page.get("images", []):
                candidates = []
                for d in img.get("dirs", []):
                    resolved = resolve_dir(dir_index, d)
                    if resolved:
                        candidates.append(resolved)
                if args.result_dir:
                    candidates.append(args.result_dir)
                found, tried, fallback = find_image_file(candidates, img.get("patterns", []))
                entry["images"].append({
                    "descr": img.get("descr", ""),
                    "path": found,
                    "tried": tried,
                    "fallback": fallback,
                    "off": img.get("off"),
                    "ext": img.get("ext"),
                })
                if not found and img.get("required", True):
                    entry["required_assets"].append("图片 " + img.get("descr", ""))
                    entry["missing"].append("图片 " + img.get("descr", ""))
                elif found and fallback:
                    entry["fallbacks"].append(
                        "第%d页 %s：期望 %s，实际用 %s"
                        % (entry["no"], entry["title"] or entry["type"],
                           img.get("descr", ""), os.path.basename(found)))

        if entry["type"] == "table":
            if table_rows:
                entry["table"] = {
                    "rows": build_table_payload(table_rows, table_info, deck),
                    "source": table_source,
                }
                entry["table_text"] = table_spec.get("summary_template", "").format(
                    n_samples=summary["n_samples"] if summary["n_samples"] is not None else "-",
                    total_cells=summary["total_cells"] if summary["total_cells"] is not None else "-",
                    gene_median=fmt_num(summary["gene_median"], 1) if summary["gene_median"] is not None else "-",
                    mt_median=fmt_num(summary["mt_median"], 2) if summary["mt_median"] is not None else "-",
                )
            else:
                entry["required_assets"].append("样本统计表")
                entry["missing"].append("样本统计表（未找到 %s）" % "，".join(table_spec.get("filenames", [])[:3]))
                entry["table"] = None
                entry["table_text"] = None

        if entry["missing"]:
            line = "第%d页 %s：缺失 %s" % (entry["no"], entry["title"] or entry["type"], "、".join(entry["missing"]))
            report.append(line)
            if args.on_missing == "skip":
                entry["skipped"] = True

        plan.append(entry)

    fatal = [e for e in plan if e["missing"] and not e["skipped"]]
    if fatal:
        raise SystemExit(
            "图片/素材缺失，已停止生成（不允许静默留空）：\n  - "
            + "\n  - ".join("%s" % m for e in fatal for m in e["missing"])
            + "\n如需跳过缺失页并继续生成，请加 --on-missing skip"
        )
    return plan, summary


def build_table_payload(table_rows, table_info, deck):
    """把统计表规整成 [表头行, 指标行...]，列数 = 样本数 + 1。"""
    samples = table_info.get("samples") or []
    if not samples:
        max_cols = max(len(r) for r in table_rows) if table_rows else 1
        samples = [""] * (max_cols - 1)
    payload = [["样本名"] + samples]
    for row in table_info.get("rows", []):
        values = list(row["values"])
        values = (values + [""] * len(samples))[: len(samples)]
        payload.append([row["label"]] + values)
    return payload


# --------------------------------------------------------------------------------------
# 打包
# --------------------------------------------------------------------------------------
class DeckWriter(object):
    def __init__(self, tpl_parts, spec):
        self.tpl = tpl_parts
        self.spec = spec
        self.deck = spec["deck"]
        self.slides = []            # [(slide_xml, rels_xml_bytes)]
        self.media = {}             # 'ppt/media/imageNNN.png' -> bytes
        self.logo_rel_id = None
        self.logo_descr = None
        self.logo_part = None
        self.notes = []

    # ---- 母版皮肤 ----------------------------------------------------------------
    def skin_xml(self, skin_no):
        name = "ppt/slides/slide%d.xml" % int(skin_no)
        if name not in self.tpl:
            raise SystemExit("模板不符合预期：缺少 %s（需要 22 页同款母版）" % name)
        return self.tpl[name].decode("utf-8")

    def skin_rels(self, skin_no):
        name = "ppt/slides/_rels/slide%d.xml.rels" % int(skin_no)
        if name not in self.tpl:
            raise SystemExit("模板不符合预期：缺少 %s" % name)
        return self.tpl[name].decode("utf-8")

    def detect_logo(self):
        """在母版第 1 页里找出客户 logo 图片，记录它的 rel id 与 media 部件。"""
        skin = self.skin_xml(1)
        rels = self.skin_rels(1)
        rel_map = dict(re.findall(r'Id="([^"]+)"[^>]*Type="[^"]*/image"[^>]*Target="([^"]+)"', rels))
        rel_map.update({i: t for t, i in re.findall(r'Type="[^"]*/image"[^>]*Target="([^"]+)"[^>]*Id="([^"]+)"', rels)})
        pics = [sh for sh in collect_shapes(skin) if sh["tag"] == "p:pic"]
        target = None
        for pic in pics:
            descr = pic["descr"] or ""
            embed = pic["embed"]
            if LOGO_DESCR_KEY in descr:
                target = rel_map.get(embed)
                self.logo_rel_id = embed
                self.logo_descr = descr
                break
        if target is None and pics:
            # 容错：取位于页面右上角的那张（客户 logo 固定贴右上）
            for pic in pics:
                m = re.search(r'<a:off x="(-?\d+)"', pic["xml"])
                if m and int(m.group(1)) > 9000000:
                    target = rel_map.get(pic["embed"])
                    self.logo_rel_id = pic["embed"]
                    self.logo_descr = pic["descr"]
                    break
        if target is None:
            raise SystemExit("模板不符合预期：第 1 页里没找到客户 logo 图片")
        part = os.path.normpath(os.path.join("ppt/slides", target)).replace(os.sep, "/")
        if part not in self.tpl:
            raise SystemExit("模板不符合预期：logo 图片 %s 不在包里" % part)
        self.logo_part = part
        self.media[part] = self.tpl[part]

    # ---- 单页渲染 ----------------------------------------------------------------
    def _skin_rel_items(self, skin_no):
        rels = self.skin_rels(skin_no)
        items = []
        for m in re.finditer(r"<Relationship\b[^>]*/>", rels):
            tag = m.group(0)
            rid = re.search(r'Id="([^"]+)"', tag)
            rtype = re.search(r'Type="([^"]+)"', tag)
            target = re.search(r'Target="([^"]+)"', tag)
            items.append({
                "tag": tag,
                "id": rid.group(1) if rid else "",
                "type": rtype.group(1) if rtype else "",
                "suffix": (rtype.group(1).rsplit("/", 1)[-1] if rtype else ""),
                "target": target.group(1) if target else "",
            })
        return items

    def _skin_logo_target(self, skin_no):
        """该母版页里客户 logo 对应的 media 目标（../media/imageN.xxx）。"""
        skin = self.skin_xml(skin_no)
        rels = self.skin_rels(skin_no)
        rel_map = dict(re.findall(r'Id="([^"]+)"[^>]*Target="([^"]+)"', rels))
        rel_map.update({i: t for t, i in re.findall(r'Target="([^"]+)"[^>]*Id="([^"]+)"', rels)})
        for sh in collect_shapes(skin):
            if sh["tag"] != "p:pic":
                continue
            if (sh["descr"] or "") == self.logo_descr:
                return rel_map.get(sh["embed"])
        return None

    def _base_rels(self, skin_no):
        """保留母版该页的 layout / tags 关系与本页 logo 图片关系，丢弃 notesSlide 与其余图片。"""
        logo_target = self._skin_logo_target(skin_no)
        keep = []
        for item in self._skin_rel_items(skin_no):
            if item["suffix"] == "notesSlide":
                continue
            if item["suffix"] == "image":
                if logo_target is None or item["target"] != logo_target:
                    continue
            keep.append(item["tag"])
        body = "".join(keep)
        return ('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\r\n'
                '<Relationships xmlns="%s">%s</Relationships>' % (REL_NS, body))

    def _next_rel_hint(self, rels_xml):
        ids = [int(x) for x in re.findall(r'Id="rId(\d+)"', rels_xml)]
        return (max(ids) + 1) if ids else 1

    def add_slide(self, slide_xml, rels_xml):
        self.slides.append((slide_xml, rels_xml))

    def add_media(self, source_path, index):
        ext = os.path.splitext(source_path)[1].lower()
        if ext not in EXT_CONTENT_TYPE:
            raise SystemExit("图片格式不支持：%s（%s）" % (source_path, ext))
        name = "ppt/media/image%d%s" % (100 + index, ext)
        while name in self.media:
            index += 1
            name = "ppt/media/image%d%s" % (100 + index, ext)
        with open(source_path, "rb") as fh:
            self.media[name] = fh.read()
        return name

    # ---- 各类页面 ----------------------------------------------------------------
    def render_cover(self, page):
        xml = self.skin_xml(page["skin"])
        shapes = collect_shapes(xml)
        reps = []
        slot_map = self.deck["shape_slots"]
        for item in page["texts"]:
            name = slot_map.get(item["slot"])
            sh = find_shape(shapes, name=name)
            if sh is None:
                raise SystemExit("模板不符合预期：%s 页缺少形状 %s" % (page["type"], name))
            reps.append((sh["start"], sh["end"], rewrite_txbody(sh["xml"], [{"text": item["text"]}])))
        xml = apply_replacements(xml, reps)
        self.add_slide(xml, self._base_rels(page["skin"]))

    def render_end(self, page):
        xml = self.skin_xml(page["skin"])
        shapes = collect_shapes(xml)
        reps = []
        for item in page["texts"]:
            name = self.deck["shape_slots"].get(item["slot"])
            sh = find_shape(shapes, name=name)
            if sh is None:
                # 尾页文案形状名可能不同：退化为该页唯一的文字形状
                cands = [s for s in shapes if s["tag"] == "p:sp" and re.search(r"<a:t>", s["xml"])]
                sh = cands[0] if cands and len(page["texts"]) == 1 else None
            if sh is None:
                raise SystemExit("模板不符合预期：尾页缺少文案形状")
            reps.append((sh["start"], sh["end"], rewrite_txbody(sh["xml"], [{"text": item["text"]}])))
        xml = apply_replacements(xml, reps)
        self.add_slide(xml, self._base_rels(page["skin"]))

    def render_text_page(self, page, table=None):
        """目录页 / 章节页共用：替换章节标题、章节编号、正文文案（表格页另加表格）。"""
        xml = self.skin_xml(page["skin"])
        shapes = collect_shapes(xml)
        slots = self.deck["shape_slots"]
        reps = []

        # 章节标题 + 编号方块
        sh = find_shape(shapes, name=slots["chapter_title"])
        if sh is None:
            raise SystemExit("模板不符合预期：缺少章节标题形状 %s" % slots["chapter_title"])
        reps.append((sh["start"], sh["end"],
                     rewrite_txbody(sh["xml"], [{"text": page["title"]}])))

        sh = find_shape(shapes, name=slots["chapter_badge"])
        if sh is not None:
            reps.append((sh["start"], sh["end"],
                         rewrite_txbody(sh["xml"], [{"text": page["section"]}])))

        # 正文文案
        body = page["body"]
        paragraphs = []
        if table is not None:
            paragraphs = [{"text": page["table_text"], "sz": 2000},
                          {"text": page.get("dir_note", "\n该分析结果所在目录：*QC/"), "sz": 2000}]
        else:
            for para in body.get("paragraphs", []):
                paragraphs.append({"text": para["text"], "sz": para.get("sz")})
        # 示例页标注：按本页最终选中的图追加一行短标签。
        #   只对能解析出簇号或细胞类型的图生效；解析不出就不加，不编造。
        for _im in (page.get("images") or []):
            _lab = example_label(_im.get("path"))
            if _lab:
                paragraphs.append({"text": _lab, "sz": 1400})
                break

        sh = find_shape(shapes, name=slots["body_copy"])
        if sh is None:
            raise SystemExit("模板不符合预期：缺少正文形状 %s" % slots["body_copy"])
        new_body = rewrite_txbody(sh["xml"], paragraphs)
        new_body = set_xfrm(new_body, body.get("off"), body.get("ext"))
        reps.append((sh["start"], sh["end"], new_body))

        # 表格
        if table is not None:
            sh = find_shape(shapes, tag="p:graphicFrame")
            if sh is None:
                raise SystemExit("模板不符合预期：样本信息页缺少表格框")
            reps.append((sh["start"], sh["end"], build_table_frame(table["rows"], self.spec["table_spec"])))

        xml = apply_replacements(xml, reps)

        # 图片
        xml, rels = self._apply_images(xml, page, page["skin"])
        self.add_slide(xml, rels)

    def render_toc(self, page):
        xml = self.skin_xml(page["skin"])
        shapes = collect_shapes(xml)
        reps = []
        slots = self.deck["shape_slots"]
        for item in page["texts"]:
            name = slots.get(item["slot"])
            sh = find_shape(shapes, name=name)
            if sh is None:
                cands = [s for s in shapes if s["tag"] == "p:sp" and re.search(r"<a:t>", s["xml"])]
                for s in cands:
                    if shape_name(s["xml"]) == name:
                        sh = s
                        break
            if sh is None:
                raise SystemExit("模板不符合预期：目录页缺少形状 %s" % name)
            reps.append((sh["start"], sh["end"], rewrite_txbody(sh["xml"], [{"text": item["text"]}])))
        xml = apply_replacements(xml, reps)
        self.add_slide(xml, self._base_rels(page["skin"]))

    def _apply_images(self, xml, page, skin_no):
        images = page.get("images", [])
        shapes = collect_shapes(xml)
        pics = [sh for sh in shapes if sh["tag"] == "p:pic"]

        logo_target = self._skin_logo_target(skin_no)
        skin_rel_map = {}
        for item in self._skin_rel_items(skin_no):
            skin_rel_map[item["id"]] = item["target"]

        def is_logo(pic):
            if (pic["descr"] or "") == self.logo_descr:
                return True
            if logo_target is not None and skin_rel_map.get(pic["embed"]) == logo_target:
                return True
            return False

        content_pics = [p for p in pics if not is_logo(p)]

        rels_xml = self._base_rels(skin_no)
        if len(content_pics) < len(images):
            raise SystemExit("模板不符合预期：模版页 slide%d 只有 %d 个图片占位，需要 %d 个"
                             % (skin_no, len(content_pics), len(images)))
        reps = []
        # 多余的图片占位直接删掉
        for extra in content_pics[len(images):]:
            reps.append((extra["start"], extra["end"], ""))

        for idx, img in enumerate(images):
            pic = content_pics[idx]
            rid = "rId%d" % (200 + idx)
            rels_xml = rels_xml.replace(
                "</Relationships>",
                '<Relationship Id="%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/'
                'relationships/image" Target="%s"/></Relationships>' % (
                    rid, "../media/" + os.path.basename(img["media_part"]))
            )
            new_pic = set_embed(pic["xml"], rid, descr=os.path.basename(img["path"]))
            new_pic = set_xfrm(new_pic, img["off"], img["ext"])
            src_w, src_h = image_pixel_size(img["path"])
            new_pic = set_src_rect(new_pic, src_w, src_h, img["ext"])
            reps.append((pic["start"], pic["end"], new_pic))

        xml = apply_replacements(xml, reps)
        return xml, rels_xml

    # ---- 输出包 ------------------------------------------------------------------
    def assemble(self, out_path, title, report_lines):
        parts = {}
        drop_prefixes = ("ppt/slides/", "ppt/notesSlides/")
        keep_extra = ("ppt/slides/_rels/",)
        for name, data in self.tpl.items():
            if name.startswith(drop_prefixes) and not name.startswith(keep_extra):
                continue
            if name.startswith("ppt/slides/"):
                continue
            if name.startswith("ppt/media/"):
                continue
            parts[name] = data

        # 新幻灯片 + 关系
        n = len(self.slides)
        pres_rels = []
        for i, (slide_xml, rels_xml) in enumerate(self.slides, start=1):
            parts["ppt/slides/slide%d.xml" % i] = slide_xml.encode("utf-8")
            parts["ppt/slides/_rels/slide%d.xml.rels" % i] = rels_xml.encode("utf-8")
            pres_rels.append((i, "rId%d" % (200 + i)))
        parts.update(self.media)

        # presentation.xml
        pres = parts["ppt/presentation.xml"].decode("utf-8")
        sld_ids = "".join('<p:sldId id="%d" r:id="%s"/>' % (256 + i, rid) for i, rid in pres_rels)
        pres = re.sub(r"<p:sldIdLst>.*?</p:sldIdLst>",
                      "<p:sldIdLst>" + sld_ids + "</p:sldIdLst>", pres, flags=re.S)
        parts["ppt/presentation.xml"] = pres.encode("utf-8")

        # presentation.xml.rels
        rels = parts["ppt/_rels/presentation.xml.rels"].decode("utf-8")
        rels = re.sub(r'<Relationship\b[^>]*Type="[^"]*/slide"[^>]*/>', "", rels)
        add = "".join(
            '<Relationship Id="%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/'
            'relationships/slide" Target="slides/slide%d.xml"/>' % (rid, i)
            for i, rid in pres_rels
        )
        rels = rels.replace("</Relationships>", add + "</Relationships>")
        parts["ppt/_rels/presentation.xml.rels"] = rels.encode("utf-8")

        # [Content_Types].xml
        ct = parts["[Content_Types].xml"].decode("utf-8")
        ct = re.sub(r'<Override PartName="/ppt/slides/slide\d+\.xml"[^>]*/>', "", ct)
        ct = re.sub(r'<Override PartName="/ppt/notesSlides/[^"]*"[^>]*/>', "", ct)
        overrides = "".join(
            '<Override PartName="/ppt/slides/slide%d.xml" ContentType="%s"/>' % (i, CT_SLIDE)
            for i in range(1, n + 1)
        )
        defaults = ""
        existing = set(x.lower() for x in re.findall(r'<Default Extension="([^"]+)"', ct))
        for media_name in self.media:
            ext = os.path.splitext(media_name)[1].lower().lstrip(".")
            if ext and ext.lower() not in existing:
                defaults += '<Default Extension="%s" ContentType="%s"/>' % (
                    ext, EXT_CONTENT_TYPE["." + ext])
                existing.add(ext.lower())
        ct = ct.replace("</Types>", overrides + "</Types>")
        if defaults:
            ct = ct.replace("<Default Extension=", defaults + "<Default Extension=", 1)
        parts["[Content_Types].xml"] = ct.encode("utf-8")

        # docProps 轻量同步
        if "docProps/app.xml" in parts:
            app = parts["docProps/app.xml"].decode("utf-8")
            app = re.sub(r"<Slides>\d+</Slides>", "<Slides>%d</Slides>" % n, app)
            parts["docProps/app.xml"] = app.encode("utf-8")
        if "docProps/core.xml" in parts:
            core = parts["docProps/core.xml"].decode("utf-8")
            core = re.sub(r"<dc:title>.*?</dc:title>",
                          "<dc:title>" + xml_escape(title) + "</dc:title>", core, flags=re.S)
            parts["docProps/core.xml"] = core.encode("utf-8")

        # 写 zip
        out_dir = os.path.dirname(os.path.abspath(out_path))
        if out_dir and not os.path.isdir(out_dir):
            os.makedirs(out_dir)
        order = ["[Content_Types].xml"]
        names = order + [k for k in sorted(parts) if k not in order]
        with zipfile.ZipFile(out_path, "w", zipfile.ZIP_DEFLATED) as zf:
            for name in names:
                if name in parts:
                    zf.writestr(name, parts[name])
        return n


def build_table_frame(rows, table_spec):
    xfrm = table_spec.get("xfrm", {"off": [540000, 3420000], "ext": [11160000, 720000]})
    ncols = max(len(r) for r in rows) if rows else 1
    total_cx = int(xfrm["ext"][0])
    col_w = total_cx // ncols
    nrows = len(rows)
    cell_sz = None
    if ncols > 13:
        cell_sz = 1000
    elif ncols > 10:
        cell_sz = 1100
    elif ncols > 8:
        cell_sz = 1200

    grid = "".join('<a:gridCol w="%d"/>' % col_w for _ in range(ncols))
    trs = []
    for row in rows:
        cells = []
        for col in range(ncols):
            value = row[col] if col < len(row) else ""
            rpr = '<a:rPr lang="zh-CN" altLang="en-US"%s/>' % (
                ' sz="%d"' % cell_sz if cell_sz else "")
            cells.append(
                "<a:tc><a:txBody><a:bodyPr/><a:lstStyle/><a:p><a:r>%s<a:t>%s</a:t></a:r>"
                "</a:p></a:txBody><a:tcPr/></a:tc>" % (rpr, xml_text(value))
            )
        trs.append('<a:tr h="0">' + "".join(cells) + "</a:tr>")

    return (
        "<p:graphicFrame><p:nvGraphicFramePr><p:cNvPr id=\"12\" name=\"Table 11\"/>"
        "<p:cNvGraphicFramePr><a:graphicFrameLocks noGrp=\"1\"/></p:cNvGraphicFramePr><p:nvPr/>"
        "</p:nvGraphicFramePr><p:xfrm><a:off x=\"%d\" y=\"%d\"/><a:ext cx=\"%d\" cy=\"%d\"/></p:xfrm>"
        "<a:graphic><a:graphicData uri=\"http://schemas.openxmlformats.org/drawingml/2006/table\">"
        "<a:tbl><a:tblPr firstRow=\"1\" bandRow=\"1\">"
        "<a:tableStyleId>{5C22544A-7EE6-4342-B048-85BDC9FD1C3A}</a:tableStyleId></a:tblPr>"
        "<a:tblGrid>%s</a:tblGrid>%s</a:tbl></a:graphicData></a:graphic></p:graphicFrame>"
        % (int(xfrm["off"][0]), int(xfrm["off"][1]), total_cx, int(xfrm["ext"][1]),
           grid, "".join(trs))
    )


# --------------------------------------------------------------------------------------
# CLI
# --------------------------------------------------------------------------------------
def parse_args(argv=None):
    ap = argparse.ArgumentParser(
        prog="make_ppt.py",
        description="把单细胞分析结果生成与客户交付 PPT 同款的 .pptx（纯标准库，不依赖 python-pptx）",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=(
            "示例：\n"
            "  python3 make_ppt.py --result-dir results --template 客户模板.pptx --dry-run\n"
            "  python3 make_ppt.py --result-dir results --template 客户模板.pptx \\\n"
            "      --config pipeline.json --date 2026-01-22\n"
            "  python3 make_ppt.py --result-dir results --template 客户模板.pptx --on-missing skip\n"
        ),
    )
    ap.add_argument("--result-dir", dest="result_dir", default=None,
                    help="六套结果目录的根目录（QC / cluster / markgene / annot / cell_ratio / diff_enrich）")
    ap.add_argument("--template", dest="template", default=None,
                    help="客户交付 PPT 模板（.pptx），用作版式母版；生成时必填")
    ap.add_argument("--out", dest="out", default=None,
                    help="输出路径；默认 analysis_report_<YYYYMMDD>.pptx")
    ap.add_argument("--date", dest="date", default=None,
                    help="封面日期，YYYY-MM-DD；默认取当天")
    ap.add_argument("--config", dest="config", default=None,
                    help="pipeline.json。给了就从 case.name 取交付标题，避免写死在模板里")
    ap.add_argument("--title", dest="title", default=None,
                    help="封面标题；默认使用 spec 里的默认标题")
    ap.add_argument("--spec", dest="spec", default=DEFAULT_SPEC,
                    help="页序/文案配置，默认 scripts/slide_spec.json")
    ap.add_argument("--dry-run", dest="dry_run", action="store_true",
                    help="只打印 22 页计划与图片来源，不写任何文件")
    ap.add_argument("--on-missing", dest="on_missing", choices=["error", "skip"], default="error",
                    help="缺图/缺表时的行为：error 直接报错（默认），skip 跳过该页并记录到报告")
    ap.add_argument("--report", dest="report", default=None,
                    help="把生成报告额外写到指定文件")
    ap.add_argument("--stats-json", dest="stats_json", default=None,
                    help="可选：补充/覆盖统计数字与表格行，如 {\"n_samples\":7,...}")
    return ap.parse_args(argv)


def load_spec(path):
    if not os.path.isfile(path):
        raise SystemExit("找不到页面配置：%s" % path)
    with open(path, "r", encoding="utf-8") as fh:
        spec = json.load(fh)
    for required in ("deck", "pages"):
        if required not in spec:
            raise SystemExit("页面配置缺少字段：%s" % required)
    return spec


def print_plan(plan, summary, result_dir, template):
    log("=" * 78)
    log("页序计划（共 %d 页）" % len(plan))
    log("=" * 78)
    if result_dir:
        log("结果目录：%s" % result_dir)
    else:
        log("结果目录：（未提供 --result-dir，所有素材按“未提供”处理）")
    if template:
        log("模板：%s  %s" % (template, "存在" if os.path.isfile(template) else "【不存在】"))
    else:
        log("模板：（未提供，dry-run 不做母版校验）")
    log("-" * 78)
    missing_total = 0
    for entry in plan:
        seg = "P%02d %-8s skin=slide%-3d" % (entry["no"], entry["type"], entry["skin"])
        if entry["section"]:
            seg += " [%s] %s" % (entry["section"], entry["chapter"])
        if entry["title"] and entry["title"] != entry["chapter"]:
            seg += " / %s" % entry["title"]
        log(seg)
        if entry["texts"]:
            for t in entry["texts"]:
                log("     文案 %-12s %s" % (t["slot"], t["text"].replace("\n", "⏎")[:52]))
        for img in entry["images"]:
            if img["path"]:
                log("     图片 %-34s <- %s" % (img["descr"], img["path"]))
            else:
                log("     图片 %-34s <- 【缺失】 已尝试：%s" % (img["descr"], "; ".join(img["tried"][:2])))
                missing_total += 1
        if entry["table"] is not None:
            log("     表格 %s，%d 行 × %d 列；文案：%s"
                % (entry["table"]["source"], len(entry["table"]["rows"]),
                   max(len(r) for r in entry["table"]["rows"]),
                   (entry.get("table_text") or "")[:40]))
        elif entry["type"] == "table":
            log("     表格 【缺失】")
            missing_total += 1
        if entry["skipped"]:
            log("     -> 该页按 --on-missing skip 跳过")
    log("-" * 78)
    log("统计：样本数 %s，总细胞数 %s，基因中位数 %s，线粒体中位数 %s"
        % (summary.get("n_samples"), summary.get("total_cells"),
           fmt_num(summary.get("gene_median"), 1) if summary.get("gene_median") is not None else None,
           fmt_num(summary.get("mt_median"), 2) if summary.get("mt_median") is not None else None))
    log("缺失素材：%d 处" % missing_total)
    log("（dry-run：未写出任何文件）")


def main(argv=None):
    args = parse_args(argv)
    spec = load_spec(args.spec)
    deck = spec["deck"]

    if not args.date:
        args.date = _dt.date.today().strftime("%Y-%m-%d")
    if not args.title:
        # 标题优先来自这一次运行的配置（pipeline.json -> case.name）。
        #   以前只从 slide_spec.json 的 default_result_title 取，而那里写着
        #   上一批数据的名字；换数据不改模板，交付稿第一页就顶着旧项目名。
        #   取不到就用中性标题，不拿任何具体数据集的名称当默认。
        if getattr(args, "config", None) and os.path.isfile(args.config):
            try:
                with open(args.config, "r", encoding="utf-8") as fh:
                    _cfg = json.load(fh)
                _nm = ((_cfg.get("case") or {}).get("name")) or ""
                if isinstance(_nm, dict):
                    _nm = _nm.get("value") or ""
                _nm = str(_nm).strip()
                if _nm:
                    args.title = _nm
                    log("交付标题取自配置 case.name：%s" % _nm)
            except Exception as _e:
                log("读取配置取标题失败，改用中性标题：%s" % _e)
        else:
            log("未给 --config，无法从配置取标题，使用中性标题。")
    if not args.title:
        args.title = deck.get("default_result_title", "单细胞分析结果汇报")

    if not args.result_dir and not args.dry_run:
        raise SystemExit("缺少 --result-dir（六套结果目录的根目录）")
    if args.result_dir and not os.path.isdir(args.result_dir):
        raise SystemExit("结果目录不存在：%s" % args.result_dir)

    template_parts = None
    if args.template:
        if not os.path.isfile(args.template):
            if args.dry_run:
                log("提示：模板不存在 %s，dry-run 继续（不做母版校验）。" % args.template)
            else:
                raise SystemExit("模板不存在：%s" % args.template)
        elif not args.dry_run:
            template_parts = read_zip_parts(args.template)
            for need in ("ppt/presentation.xml", "[Content_Types].xml", "ppt/slides/slide1.xml"):
                if need not in template_parts:
                    raise SystemExit("模板不符合预期：%s 里缺少 %s" % (args.template, need))

    dir_index = build_dir_index(args.result_dir) if args.result_dir else {}
    report_lines = []
    plan, summary = build_plan(spec, args, dir_index, report_lines)

    if args.dry_run:
        print_plan(plan, summary, args.result_dir, args.template)
        return 0

    if not args.template:
        raise SystemExit("生成 .pptx 必须提供 --template（客户交付 PPT 母版）")

    writer = DeckWriter(template_parts, spec)
    writer.detect_logo()
    media_index = 0
    rendered = 0
    for entry in plan:
        if entry["skipped"]:
            continue
        if entry["type"] == "cover":
            writer.render_cover(entry)
        elif entry["type"] == "toc":
            writer.render_toc(entry)
        elif entry["type"] == "table":
            writer.render_text_page(entry, table=entry["table"])
        elif entry["type"] == "content":
            for img in entry["images"]:
                img["media_part"] = writer.add_media(img["path"], media_index)
                media_index += 1
            writer.render_text_page(entry)
        elif entry["type"] == "end":
            writer.render_end(entry)
        else:
            raise SystemExit("未知页面类型：%s" % entry["type"])
        rendered += 1

    out_path = args.out or os.path.join(os.getcwd(), "analysis_report_%s.pptx"
                                        % args.date.replace("-", ""))
    if os.path.isdir(out_path):
        out_path = os.path.join(out_path, "analysis_report_%s.pptx" % args.date.replace("-", ""))
    n_slides = writer.assemble(out_path, args.title, report_lines)

    # Keep a traceable mapping from slide content to the exact source files.
    # This is a companion artifact, not a claim that biological interpretation
    # or visual layout has been reviewed.
    def file_digest(path, algorithm):
        digest = hashlib.new(algorithm)
        with open(path, "rb") as fh:
            for chunk in iter(lambda: fh.read(1024 * 1024), b""):
                digest.update(chunk)
        return digest.hexdigest()

    source_slides = []
    for entry in plan:
        if entry["skipped"]:
            continue
        images = [{"source_rel": os.path.relpath(img["path"], args.result_dir).replace(os.sep, "/"),
                   "description": img["descr"], "media_part": img["media_part"],
                   "md5": file_digest(img["path"], "md5"),
                   "sha256": file_digest(img["path"], "sha256")}
                  for img in entry["images"]]
        source_slides.append({"slide": len(source_slides) + 1, "spec_page": entry["no"],
                              "title": entry["title"], "type": entry["type"], "images": images})
    manifest = {"schema": "sc-ppt-sources-v1", "pptx": os.path.basename(out_path),
                "pptx_md5": file_digest(out_path, "md5"),
                "pptx_sha256": file_digest(out_path, "sha256"),
                "slides": source_slides, "summary": summary}
    table_source = next((entry["table"]["source"] for entry in plan
                         if not entry["skipped"] and entry["table"]), None)
    if table_source and os.path.isfile(table_source):
        manifest["sample_table"] = {"source_rel": os.path.relpath(table_source, args.result_dir).replace(os.sep, "/"),
                                    "md5": file_digest(table_source, "md5")}
    with open(out_path + ".manifest.json", "w", encoding="utf-8") as fh:
        json.dump(manifest, fh, ensure_ascii=False, indent=2)

    log("已生成：%s" % out_path)
    log("页数：%d（计划 %d 页，跳过 %d 页）" % (n_slides, len(plan), len(plan) - rendered))
    log("图片：%d 张（含母版 logo 1 张）" % (len(writer.media)))
    if report_lines:
        log("缺失素材报告：")
        for line in report_lines:
            log("  - " + line)
    if args.report:
        with open(args.report, "w", encoding="utf-8") as fh:
            fh.write("sc-ppt-kit 生成报告\n")
            fh.write("输出：%s\n" % out_path)
            fh.write("模板：%s\n" % args.template)
            fh.write("结果目录：%s\n" % (args.result_dir or "-"))
            fh.write("标题：%s\n日期：%s\n" % (args.title, args.date))
            fh.write("页数：%d\n" % n_slides)
            fh.write("\n缺失素材：\n")
            fh.write("\n".join("  - " + line for line in report_lines) if report_lines else "  （无）")
            fh.write("\n")
        log("报告已写入：%s" % args.report)
    return 0


if __name__ == "__main__":
    sys.exit(main())
