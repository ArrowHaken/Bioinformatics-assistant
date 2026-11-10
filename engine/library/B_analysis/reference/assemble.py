#!/usr/bin/env python3
"""assemble.py — 第三段：脚本组装 + 校验。

读取 LLM 产出的抽取结果（papers/<ref>_<slug>.extract.json），按细胞类型归并、
去重、按「有几篇文献支持」投票，组装成下游约定格式的词典：

  {"POD": {"markers": ["NPHS1","WT1"], "refs": ["1","2"], "full": "Podocyte"}, ...}

组装完立刻调用 validate.py 的同一套规则自检，报告与词典一起落地。

本脚本绝不自己发明基因名或引用编号：所有 marker 都必须来自某份抽取结果，
所有 refs 都必须能在 refs.json 里找到对应文献。

用法
----
  python3 scripts/assemble.py --batch runs/kidney-20261009
  python3 scripts/assemble.py --batch runs/kidney-20261009 \
      --out runs/kidney-20261009/lit_dict.json --min-votes 2 \
      --alias-map aliases.json --key-map keys.json

参数
----
  --batch DIR        collect.py 产出的批次目录（含 papers/ 与 refs.json）。
  --extract-dir DIR  抽取结果所在目录，默认 <batch>/papers。
  --refs PATH        refs.json 路径，默认 <batch>/refs.json。
  --out PATH         词典输出路径，默认 <batch>/lit_dict.json。
  --report PATH      组装报告（JSON）路径，默认 <batch>/assemble_report.json。
  --min-votes N      一个 marker 至少要被几篇文献支持才保留，默认 1。
  --min-markers N    每个细胞类型至少需要几个 marker，默认 3（传给校验）。
  --max-markers N    每个细胞类型最多保留几个 marker，默认 30。
  --alias-map PATH   细胞类型别名表，把同义写法归并到同一类型。
  --key-map PATH     细胞类型短键指派表，显式指定 POD / PTC 这类键。
  --allow-unref      允许 refs.json 里查不到的抽取结果参与组装（默认剔除并报错）。
  --require-fetch-ok 若 collect_report.json 显示有全文抓取失败，直接报错退出。

退出码
------
  0 词典产出且无 error/warning；2 参数错误；3 产出但有 error；
  4 产出但只有 warning；5 没有任何可用抽取结果，未产出词典。
"""
from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from litdict_common import (  # noqa: E402
    KEY_RE, derive_key, gene_key, is_gene_symbol, load_json, normalize_celltype,
    ref_sort_key, sort_refs, write_json,
)
from validate import validate_dict  # noqa: E402

EXTRACT_GLOB = "*.extract.json"
REF_PREFIX_RE = re.compile(r"^(\d+)")


# ------------------------------------------------------------------ 参数
def parse_args(argv=None):
    ap = argparse.ArgumentParser(
        description="把 LLM 抽取结果组装成 marker 词典并校验。",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__.split("用法")[-1])
    ap.add_argument("--batch", required=True, help="collect.py 产出的批次目录")
    ap.add_argument("--extract-dir", help="抽取结果目录，默认 <batch>/papers")
    ap.add_argument("--refs", help="refs.json 路径，默认 <batch>/refs.json")
    ap.add_argument("--out", help="词典输出路径，默认 <batch>/lit_dict.json")
    ap.add_argument("--report", help="组装报告路径，默认 <batch>/assemble_report.json")
    ap.add_argument("--min-votes", type=int, default=1, help="marker 最少支持文献数")
    ap.add_argument("--min-markers", type=int, default=3, help="每个细胞类型最少 marker 数")
    ap.add_argument("--max-markers", type=int, default=30, help="每个细胞类型最多保留 marker 数")
    ap.add_argument("--alias-map", help="细胞类型别名表 JSON")
    ap.add_argument("--key-map", help="细胞类型短键指派表 JSON")
    ap.add_argument("--allow-unref", action="store_true",
                    help="允许 refs.json 里查不到的抽取结果参与组装")
    ap.add_argument("--require-fetch-ok", action="store_true",
                    help="collect 阶段有全文抓取失败时报错退出")
    args = ap.parse_args(argv)

    if args.min_votes < 1:
        ap.error("--min-votes 至少为 1")
    if args.min_markers < 1:
        ap.error("--min-markers 至少为 1")
    if args.max_markers < args.min_markers:
        ap.error("--max-markers 不能小于 --min-markers")

    batch = pathlib.Path(args.batch).expanduser()
    if not batch.is_dir():
        ap.error(f"--batch 目录不存在：{batch}")
    args.batch_path = batch
    args.extract_path = pathlib.Path(args.extract_dir).expanduser() if args.extract_dir \
        else batch / "papers"
    args.refs_path = pathlib.Path(args.refs).expanduser() if args.refs else batch / "refs.json"
    args.out_path = pathlib.Path(args.out).expanduser() if args.out else batch / "lit_dict.json"
    args.report_path = (pathlib.Path(args.report).expanduser() if args.report
                        else batch / "assemble_report.json")
    return args


# ------------------------------------------------------------------ 别名 / 键
def load_alias_map(path, errors, warnings):
    """读别名表。支持两种写法：

      {"Podocytes": "Podocyte", "PTC": {"full": "Proximal Tubule Cell", "key": "PTC"}}

    键和值都按 normalize_celltype 归一后建立索引，所以大小写、复数差异都能命中。
    读不了不静默——记进 errors/warnings，然后按「没有别名」继续。
    """
    if not path:
        return {}, {}
    data, err = load_json(path, label="别名表")
    if err:
        errors.append({"code": "alias_map_unreadable", "message": err, "cell_type": None})
        return {}, {}
    if not isinstance(data, dict):
        errors.append({"code": "alias_map_bad_type",
                       "message": f"别名表顶层必须是对象，实际是 {type(data).__name__}",
                       "cell_type": None})
        return {}, {}
    alias_full, alias_key = {}, {}
    for k, v in data.items():
        nk = normalize_celltype(k)
        if not nk:
            warnings.append({"code": "alias_map_empty_key",
                             "message": f"别名表里有空键：{k!r}", "cell_type": None})
            continue
        if isinstance(v, str):
            alias_full[nk] = v.strip()
        elif isinstance(v, dict):
            if v.get("full"):
                alias_full[nk] = str(v["full"]).strip()
            if v.get("key"):
                alias_key[nk] = str(v["key"]).strip().upper()
        else:
            warnings.append({"code": "alias_map_bad_value",
                             "message": f"别名表 {k!r} 的值既不是字符串也不是对象：{v!r}",
                             "cell_type": None})
    return alias_full, alias_key


def load_key_map(path, errors):
    """读显式短键指派表：{"podocyte": "POD", "Proximal Tubule Cell": "PTC"}。"""
    if not path:
        return {}
    data, err = load_json(path, label="短键指派表")
    if err:
        errors.append({"code": "key_map_unreadable", "message": err, "cell_type": None})
        return {}
    if not isinstance(data, dict):
        errors.append({"code": "key_map_bad_type",
                       "message": f"短键指派表顶层必须是对象，实际是 {type(data).__name__}",
                       "cell_type": None})
        return {}
    out = {}
    for k, v in data.items():
        nk = normalize_celltype(k)
        if not nk:
            continue
        key = str(v).strip().upper()
        out[nk] = key
        out[k.strip()] = key          # 同时支持按全称精确命中
    return out


# ------------------------------------------------------------------ 读抽取
def parse_ref_from_filename(path: pathlib.Path):
    """从 0001_slug.extract.json 里取出引用编号 '1'；取不到返回 None。"""
    m = REF_PREFIX_RE.match(path.name)
    if not m:
        return None
    return str(int(m.group(1)))


def load_extractions(args, refs_index, errors, warnings):
    """读取所有抽取结果，返回 (records, file_stats)。

    records 里每项是 {ref, path, cell_types:[{cell_type,key,markers,evidence}]}。
    文件名编号与内容里的 ref 不一致、编号不在 refs.json 里，都如实记录。
    """
    if not args.extract_path.is_dir():
        return [], {"dir_exists": False, "files": 0}
    files = sorted(args.extract_path.glob(EXTRACT_GLOB))
    records = []
    for f in files:
        data, err = load_json(f, label="抽取结果")
        if err:
            errors.append({"code": "extract_unreadable", "message": err, "cell_type": None})
            continue
        if not isinstance(data, dict):
            errors.append({"code": "extract_bad_type",
                           "message": f"抽取结果顶层必须是对象：{f.name}", "cell_type": None})
            continue

        fn_ref = parse_ref_from_filename(f)
        body_ref = str(data.get("ref")).strip() if data.get("ref") is not None else None
        ref = fn_ref or body_ref
        if ref is None:
            errors.append({"code": "extract_no_ref",
                           "message": f"{f.name} 的文件名与内容里都没有引用编号，无法回填 refs",
                           "cell_type": None})
            continue
        if fn_ref and body_ref and str(int(body_ref)) != fn_ref and body_ref != fn_ref:
            warnings.append({"code": "extract_ref_mismatch",
                             "message": f"{f.name} 文件名编号 {fn_ref} 与内容里的 ref "
                                        f"{body_ref} 不一致，按文件名 {fn_ref} 处理",
                             "cell_type": None})
        if refs_index is not None and ref not in refs_index:
            msg = (f"{f.name} 的引用编号 {ref} 不在 refs.json 里"
                   f"（有效编号 {len(refs_index)} 个）")
            if args.allow_unref:
                warnings.append({"code": "extract_ref_unknown", "message": msg,
                                 "cell_type": None})
            else:
                errors.append({"code": "extract_ref_unknown", "message": msg,
                               "cell_type": None})
                continue

        cell_types = data.get("cell_types")
        if cell_types is None:
            errors.append({"code": "extract_no_field",
                           "message": f"{f.name} 缺少 cell_types 字段（schema 见 papers/EXTRACT_README.md）",
                           "cell_type": None})
            continue
        if not isinstance(cell_types, list):
            errors.append({"code": "extract_bad_field",
                           "message": f"{f.name} 的 cell_types 必须是数组", "cell_type": None})
            continue
        if not cell_types:
            warnings.append({"code": "extract_empty",
                             "message": f"{f.name} 的 cell_types 为空：这篇文献没有产出可用 marker",
                             "cell_type": None})

        clean = []
        for item in cell_types:
            if not isinstance(item, dict):
                errors.append({"code": "celltype_not_object",
                               "message": f"{f.name} 的 cell_types 里有非对象条目：{item!r}",
                               "cell_type": None})
                continue
            name = item.get("cell_type")
            if not isinstance(name, str) or not name.strip():
                errors.append({"code": "celltype_no_name",
                               "message": f"{f.name} 有 cell_types 条目缺少 cell_type 名称",
                               "cell_type": None})
                continue
            markers = item.get("markers")
            if markers is None:
                markers = []
            if not isinstance(markers, list):
                errors.append({"code": "celltype_bad_markers",
                               "message": f"{f.name} 的 {name} 的 markers 必须是数组",
                               "cell_type": name})
                continue
            evidence = item.get("evidence")
            if not isinstance(evidence, str) or len(evidence.strip()) < 15:
                warnings.append({"code": "celltype_weak_evidence",
                                 "message": f"{f.name} 的 {name} 缺少足够长度的 evidence 原文片段，"
                                            "无法回溯",
                                 "cell_type": name})
            clean.append({"cell_type": name.strip(),
                          "key": (item.get("key") or "").strip() or None,
                          "markers": markers,
                          "evidence": (evidence or "").strip()})
        records.append({"ref": ref, "path": str(f), "cell_types": clean})

    stats = {"dir_exists": True, "files": len(files), "usable": len(records),
             "extract_dir": str(args.extract_path)}
    return records, stats


# ------------------------------------------------------------------ 归并
class CellTypeBucket:
    def __init__(self, nkey):
        self.nkey = nkey
        self.names = {}          # 原始写法 -> 出现次数
        self.keys = {}           # 抽取里给的短键 -> 出现次数
        self.markers = {}        # gene_key -> {"display":..., "refs": set()}
        self.evidence = {}       # ref -> evidence（保留一段用于回溯）
        self.key_source = None   # 由 pick_key 填写：短键是从哪来的

    def add(self, name, key, markers, ref, evidence, errors, warnings, source):
        self.names[name] = self.names.get(name, 0) + 1
        if key:
            self.keys[key] = self.keys.get(key, 0) + 1
        if evidence:
            self.evidence.setdefault(ref, evidence)
        for raw in markers:
            if not isinstance(raw, str):
                errors.append({"code": "marker_not_str",
                               "message": f"{source} 的 {name} 里有非字符串 marker：{raw!r}",
                               "cell_type": name})
                continue
            g = raw.strip()
            if not g:
                warnings.append({"code": "marker_empty",
                                 "message": f"{source} 的 {name} 里有空 marker，已忽略",
                                 "cell_type": name})
                continue
            if not is_gene_symbol(g):
                warnings.append({"code": "marker_bad_format",
                                 "message": f"{source} 的 {name} 的 marker {raw!r} 不符合基因符号"
                                            "格式，已剔除（可能把通路/化合物名写进了 markers）",
                                 "cell_type": name})
                continue
            gk = gene_key(g)
            slot = self.markers.setdefault(gk, {"display": g, "refs": set()})
            # 显示拼写取「首次出现的原文」，但如果已有的全大写、新的混大小写，
            # 保留更规范的写法（全大写优先）——人类基因符号惯例是全大写。
            if g.isupper() and not slot["display"].isupper():
                slot["display"] = g
            slot["refs"].add(ref)

    @property
    def full(self):
        # 出现次数最多的写法作为 full；并列时取更短更「规范」的那个。
        return sorted(self.names.items(), key=lambda kv: (-kv[1], len(kv[0]), kv[0]))[0][0]


def assemble_dict(records, args, refs_index, errors, warnings):
    """把抽取结果归并成 (词典, 明细, 覆盖统计)。"""
    buckets = {}
    norm_cache = {}

    def resolve(nkey):
        """套别名表，把同义写法归到同一桶。返回归一后的键。"""
        if nkey in norm_cache:
            return norm_cache[nkey]
        seen, cur = set(), nkey
        while cur in args.alias_full and cur not in seen:
            seen.add(cur)
            cur = normalize_celltype(args.alias_full[cur]) or cur
        norm_cache[nkey] = cur
        return cur

    for rec in records:
        ref = rec["ref"]
        for ct in rec["cell_types"]:
            nkey = normalize_celltype(ct["cell_type"])
            if not nkey:
                errors.append({"code": "celltype_name_unusable",
                               "message": f"{rec['path']} 的 {ct['cell_type']!r} 归一化后为空",
                               "cell_type": ct["cell_type"]})
                continue
            bucket = buckets.setdefault(resolve(nkey), CellTypeBucket(resolve(nkey)))
            bucket.add(ct["cell_type"], ct["key"], ct["markers"], ref,
                       ct["evidence"], errors, warnings, pathlib.Path(rec["path"]).name)

    dictionary, detail = {}, {}
    all_marker_refs = {}
    for nkey, bucket in sorted(buckets.items()):
        full = bucket.full
        # marker 投票：按支持文献数降序，再按基因名字母序，保证输出稳定可复现
        kept, dropped = [], []
        for gk, slot in bucket.markers.items():
            votes = len(slot["refs"])
            item = {"marker": slot["display"], "votes": votes,
                    "refs": sorted(slot["refs"], key=ref_sort_key)}
            if votes < args.min_votes:
                dropped.append({**item, "reason": f"支持文献数 {votes} < --min-votes={args.min_votes}"})
            else:
                kept.append(item)
        kept.sort(key=lambda x: (-x["votes"], x["marker"].upper()))
        if len(kept) > args.max_markers:
            for extra in kept[args.max_markers:]:
                dropped.append({**extra, "reason": f"超出 --max-markers={args.max_markers}"})
            kept = kept[:args.max_markers]

        key = pick_key(nkey, full, bucket, args, errors, warnings)
        refs_used = sort_refs([r for item in kept for r in item["refs"]])
        dictionary[key] = {"markers": [i["marker"] for i in kept],
                           "refs": refs_used,
                           "full": full}
        detail[key] = {
            "full": full,
            "merged_from_names": sorted(bucket.names),
            "key_source": bucket.key_source,
            "markers": kept,
            "dropped_markers": dropped,
            "refs": refs_used,
            "evidence_by_ref": bucket.evidence,
        }
        for item in kept:
            all_marker_refs.setdefault(gene_key(item["marker"]), []).append(key)

    coverage = {
        "cell_types_found": len(dictionary),
        "markers_total": sum(len(v["markers"]) for v in dictionary.values()),
        "markers_unique": len(all_marker_refs),
        "markers_shared_across_celltypes": {
            k: v for k, v in sorted(all_marker_refs.items()) if len(v) > 1},
        "refs_index_size": len(refs_index) if refs_index else None,
        "refs_referenced": sort_refs({r for v in dictionary.values() for r in v["refs"]}),
    }
    return dictionary, detail, coverage


def pick_key(nkey, full, bucket, args, errors, warnings):
    """定这个细胞类型的短键。优先级：显式 key-map > 抽取里的多数 key > 自动推导。"""
    explicit = args.key_map.get(nkey) or args.key_map.get(full.strip())
    if explicit:
        bucket.key_source = "key-map"
        key = explicit
    elif bucket.keys:
        bucket.key_source = "抽取结果里的多数写法"
        key = sorted(bucket.keys.items(), key=lambda kv: (-kv[1], kv[0]))[0][0].upper()
    else:
        key = derive_key(full)
        bucket.key_source = "由全称自动推导，需人工确认"
        if key:
            warnings.append({"code": "key_autoderived",
                             "message": f"{full} 的短键 {key} 是脚本按全称推导的，"
                                        "请人工确认是否符合下游命名约定",
                             "cell_type": full})
    if not key:
        errors.append({"code": "key_underivable",
                       "message": f"{full} 推不出短键，请用 --key-map 显式指定",
                       "cell_type": full})
        return f"UNKNOWN_{abs(hash(nkey)) % 1000:03d}"
    if not KEY_RE.match(key) or len(key) > 10:
        errors.append({"code": "key_bad_format",
                       "message": f"{full} 的短键 {key!r} 不符合格式（大写字母/数字/下划线，≤10 字符）",
                       "cell_type": full})
    return key


def resolve_key_collisions(dictionary, detail, errors, warnings):
    """两个细胞类型推出同一个键时，后面的加 _2 后缀并显式记录。

    不静默覆盖：直接覆盖会让某个细胞类型的 marker 从词典里凭空消失。
    """
    seen = {}
    for key in list(dictionary.keys()):
        if key not in seen:
            seen[key] = key
            continue
        i = 2
        while f"{key}_{i}" in dictionary or f"{key}_{i}" in seen:
            i += 1
        new_key = f"{key}_{i}"
        dictionary[new_key] = dictionary.pop(key)
        detail[new_key] = detail.pop(key)
        detail[new_key]["key_collision_with"] = key
        errors.append({"code": "key_collision",
                       "message": f"{detail[new_key]['full']} 的短键与 {key} 冲突，已临时改成 "
                                  f"{new_key}；请用 --key-map 显式指派",
                       "cell_type": detail[new_key]["full"]})
        seen[new_key] = new_key
    return dictionary, detail


# ------------------------------------------------------------------ 报告
def render_markdown(report, dictionary):
    """组装报告的可读版：谁贡献了什么、什么被剔了、哪些文献没出结果。"""
    L = ["# marker 词典组装报告", "",
         f"批次目录：{report['batch_dir']}",
         f"抽取文件：{report['extract_stats'].get('files')} 个"
         f"（可用 {report['extract_stats'].get('usable')} 个）",
         f"参与引用的文献：{len(report['coverage']['refs_referenced'])} 篇",
         f"结论：{report['status']}（error {report['error_count']}，"
         f"warning {report['warning_count']}）", ""]

    L += ["## 每个细胞类型的 marker 与支持文献", ""]
    for key, info in sorted(dictionary.items()):
        L.append(f"### {key} — {info['full']}")
        L.append(f"- 引用编号：{', '.join(info['refs']) or '（无）'}")
        L.append(f"- 合并自以下写法：{', '.join(report['detail'][key]['merged_from_names'])}")
        L.append(f"- 短键来源：{report['detail'][key]['key_source']}")
        if info["markers"]:
            L.append("- marker（支持文献数）：")
            for m in report["detail"][key]["markers"]:
                L.append(f"  - {m['marker']}（{m['votes']} 篇：refs {', '.join(m['refs'])}）")
        else:
            L.append("- marker：（空）")
        dropped = report["detail"][key]["dropped_markers"]
        if dropped:
            L.append(f"- 被剔除的候选 {len(dropped)} 个：")
            for d in dropped:
                L.append(f"  - {d['marker']}（原因：{d['reason']}）")
        L.append("")

    L += ["## 覆盖与缺口", "",
          f"- 抽取目录：{report['extract_stats'].get('extract_dir')}",
          f"- refs.json：{report['refs_path']}"
          f"（{'已加载' if report['refs_loaded'] else '未加载'}）",
          f"- 没有抽取结果的引用编号：{', '.join(report['missing_extractions']) or '（无）'}",
          f"- collect 阶段记录的失败：{report['collect_failures_count']} 条"
          f"（见 {report.get('collect_failures_path') or 'failures.json'}）", ""]

    if report["errors"] or report["warnings"]:
        L += ["## 校验与组装发现的问题", ""]
        for it in report["errors"]:
            L.append(f"- [error] {it['code']}: {it['message']}")
        for it in report["warnings"]:
            L.append(f"- [warning] {it['code']}: {it['message']}")
        L.append("")
    return "\n".join(L)


# ------------------------------------------------------------------ main
def main(argv=None):
    args = parse_args(argv)
    errors, warnings = [], []

    refs_index = None
    refs_path = args.refs_path
    if refs_path.exists():
        refs_index, refs_err = load_json(refs_path, label="refs.json")
        if refs_err:
            errors.append({"code": "refs_unreadable", "message": refs_err, "cell_type": None})
            refs_index = None
        elif not isinstance(refs_index, dict):
            errors.append({"code": "refs_bad_type",
                           "message": f"refs.json 顶层必须是对象，实际是 {type(refs_index).__name__}",
                           "cell_type": None})
            refs_index = None
    else:
        warnings.append({"code": "refs_missing",
                         "message": f"找不到 refs.json（{refs_path}），跳过引用编号交叉核对；"
                                    "建议先跑 collect.py",
                         "cell_type": None})

    args.alias_full, args.alias_key = load_alias_map(args.alias_map, errors, warnings)
    args.key_map = load_key_map(args.key_map, errors)
    # 别名表里带的 key 也一并当显式指派（key-map 优先级更高，先 lay key-map）
    for nk, k in args.alias_key.items():
        args.key_map.setdefault(nk, k)

    # collect 阶段失败的全文抓取：如实带进报告，不当作成功
    collect_failures, collect_failures_path = [], None
    cf = args.batch_path / "failures.json"
    if cf.exists():
        collect_failures_path = str(cf)
        data, err = load_json(cf, label="collect failures")
        if err:
            warnings.append({"code": "collect_failures_unreadable", "message": err,
                             "cell_type": None})
        elif isinstance(data, dict):
            collect_failures = data.get("failures") or []
            if not collect_failures and data.get("stage") == "search":
                collect_failures = [data]
    if args.require_fetch_ok and collect_failures:
        print(f"错误：--require-fetch-ok 且 collect 阶段有 {len(collect_failures)} 条失败，"
              f"详见 {collect_failures_path}", file=sys.stderr)
        return 3

    records, extract_stats = load_extractions(args, refs_index, errors, warnings)
    if not records:
        report = {"stage": "assemble", "status": "no_input",
                  "batch_dir": str(args.batch_path),
                  "extract_stats": extract_stats,
                  "errors": errors, "warnings": warnings,
                  "error_count": len(errors), "warning_count": len(warnings),
                  "note": ("没有可用的抽取结果。抽取是 LLM 的活：先按 "
                           "papers/EXTRACT_README.md 的 schema 逐篇产出 "
                           "<ref>_<slug>.extract.json，再重跑本脚本。"),
                  "collect_failures": collect_failures,
                  "collect_failures_path": collect_failures_path}
        write_json(args.report_path, report)
        print("没有任何可用抽取结果，未产出词典。已写报告：", args.report_path, file=sys.stderr)
        return 5

    dictionary, detail, coverage = assemble_dict(records, args, refs_index, errors, warnings)
    dictionary, detail = resolve_key_collisions(dictionary, detail, errors, warnings)

    # 组装完立刻自检：结构与格式问题在这里就暴露，不等到下游
    check = validate_dict(dictionary, refs_index=refs_index,
                          min_markers=args.min_markers,
                          max_markers=args.max_markers, max_shared=5)
    for it in check["errors"]:
        errors.append({"code": f"validate:{it['code']}", "message": it["message"],
                       "cell_type": it.get("cell_type")})
    for it in check["warnings"]:
        warnings.append({"code": f"validate:{it['code']}", "message": it["message"],
                         "cell_type": it.get("cell_type")})

    # 覆盖缺口：refs.json 里有、但没人产出抽取结果
    missing = []
    if refs_index:
        produced = {r["ref"] for r in records}
        missing = sorted((ref for ref in refs_index if ref not in produced),
                         key=ref_sort_key)
        if missing:
            warnings.append({"code": "missing_extractions",
                             "message": f"{len(missing)} 篇文献没有抽取结果："
                                        f"{', '.join(missing)}",
                             "cell_type": None})

    status = "error" if errors else ("warning" if warnings else "ok")
    report = {
        "stage": "assemble",
        "status": status,
        "batch_dir": str(args.batch_path),
        "dict_path": str(args.out_path),
        "refs_path": str(refs_path),
        "refs_loaded": bool(refs_index),
        "extract_stats": extract_stats,
        "params": {"min_votes": args.min_votes, "min_markers": args.min_markers,
                   "max_markers": args.max_markers,
                   "alias_map": args.alias_map, "key_map": args.key_map,
                   "allow_unref": args.allow_unref},
        "coverage": coverage,
        "detail": detail,
        "missing_extractions": missing,
        "collect_failures": collect_failures,
        "collect_failures_path": collect_failures_path,
        "collect_failures_count": len(collect_failures),
        "validate": check,
        "errors": errors, "warnings": warnings,
        "error_count": len(errors), "warning_count": len(warnings),
    }

    # 只要还有细胞类型，就把词典写出来——带 error 的词典交给人判断，
    # 而不是让脚本悄悄不产出（那样反而更难发现问题）。
    if dictionary:
        write_json(args.out_path, dictionary)
    write_json(args.report_path, report)
    md_path = args.report_path.with_suffix(".md")
    md_path.write_text(render_markdown(report, dictionary), encoding="utf-8")

    print(f"词典：{args.out_path}")
    print(f"细胞类型 {len(dictionary)} 个，marker 去重后 {coverage['markers_unique']} 个，"
          f"用到文献 {len(coverage['refs_referenced'])} 篇。")
    print(f"结论：{status}（error {len(errors)}，warning {len(warnings)}）")
    if errors:
        print("--- error 摘要（最多 10 条）---")
        for it in errors[:10]:
            print(f"  {it['code']}: {it['message']}")
    print(f"完整报告：{args.report_path} 与 {md_path}")
    if warnings and not errors:
        print("有 warning 但没有 error；warning 明细见报告。")
    if errors:
        return 3
    if warnings:
        return 4
    return 0


if __name__ == "__main__":
    sys.exit(main())
