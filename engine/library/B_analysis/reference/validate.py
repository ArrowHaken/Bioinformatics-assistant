#!/usr/bin/env python3
"""validate.py — 第三段：脚本校验。

对组装好的 marker 词典做机械校验：结构是否合下游约定、基因名格式、去重、
每个细胞类型 marker 数量是否够、refs 是否为有效编号。只做能确定性判定的事，
不做「这个基因像不像该细胞类型的 marker」这类需要读文献的判断。

本模块同时被 assemble.py 复用（import validate_dict），所以校验规则只有一份。

用法
----
  python3 scripts/validate.py runs/kidney-20261009/lit_dict.json
  python3 scripts/validate.py runs/kidney-20261009/lit_dict.json \
      --refs runs/kidney-20261009/refs.json --min-markers 3 \
      --json runs/kidney-20261009/validate_report.json

参数
----
  位置参数 dict_path     要校验的词典 JSON。
  --refs PATH           collect.py 产出的 refs.json。给了就交叉核对引用编号是否越界。
  --min-markers N       每个细胞类型至少要有几个 marker，默认 3。
  --max-markers N       每个细胞类型最多允许几个 marker，默认 30（超出只告警）。
  --max-shared N        同一 marker 最多允许出现在几个细胞类型里，默认 5（超出告警）。
  --json PATH           校验报告输出路径。
  --quiet               只输出结论行。

退出码
------
  0 全部通过；2 参数错误；3 有 error（词典不可直接交付）；4 只有 warning。
"""
from __future__ import annotations

import argparse
import json
import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from litdict_common import (  # noqa: E402
    DICT_ENTRY_FIELDS, KEY_RE, MAX_KEY_LEN, gene_key, is_digit_ref, is_gene_symbol,
    load_json, looks_non_gene_symbol, ref_sort_key, write_json,
)

REQUIRED_FIELDS = ("markers", "refs", "full")


def _err(bucket, code, message, cell_type=None):
    bucket.append({"level": "error", "code": code, "message": message,
                   "cell_type": cell_type})


def _warn(bucket, code, message, cell_type=None):
    bucket.append({"level": "warning", "code": code, "message": message,
                   "cell_type": cell_type})


def validate_dict(dictionary, refs_index=None, min_markers=3, max_markers=30,
                  max_shared=5):
    """校验词典对象，返回报告 dict（不抛异常，不打印）。

    refs_index 传 collect.py 的 refs.json 内容（{ref: {...}}）时，会检查每个
    refs 里的编号是否在这个字典里；不传则跳过该项。
    """
    errors, warnings = [], []
    stats = {"cell_types": 0, "markers_total": 0, "markers_unique": 0,
             "refs_used": [], "empty_cell_types": []}
    per_celltype = {}

    if not isinstance(dictionary, dict):
        _err(errors, "not_object", f"词典顶层必须是对象，实际是 {type(dictionary).__name__}")
        return _report(errors, warnings, stats, per_celltype)
    if not dictionary:
        _err(errors, "empty_dict", "词典里没有任何细胞类型")
        return _report(errors, warnings, stats, per_celltype)

    valid_refs = None
    if refs_index is not None:
        if isinstance(refs_index, dict):
            valid_refs = {str(k).strip() for k in refs_index}
        else:
            _warn(warnings, "refs_index_not_object",
                  f"--refs 内容不是对象（{type(refs_index).__name__}），跳过引用编号交叉核对")

    marker_owners = {}          # gene_key -> [cell_type,...]
    seen_keys = {}

    for key, entry in dictionary.items():
        stats["cell_types"] += 1
        ct = {"markers": 0, "unique": 0, "refs": [], "dropped_markers": []}

        # --- 键格式
        if not isinstance(key, str) or not KEY_RE.match(key or ""):
            _err(errors, "bad_key", f"细胞类型键 {key!r} 不是短大写代码（示例：POD、PTC）", key)
        elif len(key) > MAX_KEY_LEN:
            _err(errors, "bad_key", f"细胞类型键 {key} 超过 {MAX_KEY_LEN} 字符", key)
        if isinstance(key, str) and key in seen_keys:
            _err(errors, "dup_key", f"细胞类型键重复：{key}", key)
        if isinstance(key, str):
            seen_keys[key] = True

        if not isinstance(entry, dict):
            _err(errors, "not_object", f"{key} 的值必须是对象", key)
            per_celltype[key] = ct
            continue

        missing = [f for f in REQUIRED_FIELDS if f not in entry]
        if missing:
            _err(errors, "missing_field",
                 f"{key} 缺少必需字段：{', '.join(missing)}（下游约定 markers/refs/full）", key)
        extra = [f for f in entry if f not in DICT_ENTRY_FIELDS]
        if extra:
            _warn(warnings, "extra_field",
                  f"{key} 含下游约定之外的字段：{', '.join(extra)}", key)

        # --- full 名称
        full = entry.get("full")
        if not isinstance(full, str) or not full.strip():
            _err(errors, "bad_full", f"{key} 的 full 不是非空字符串", key)

        # --- markers
        markers = entry.get("markers")
        if not isinstance(markers, list):
            _err(errors, "bad_markers", f"{key} 的 markers 必须是数组", key)
            markers = []
        elif not markers:
            _err(errors, "no_markers", f"{key} 的 markers 为空，下游无法据此判定细胞类型", key)
            stats["empty_cell_types"].append(key)

        seen_gene = {}
        for raw in markers:
            if not isinstance(raw, str):
                _err(errors, "marker_not_str", f"{key} 里有非字符串 marker：{raw!r}", key)
                ct["dropped_markers"].append(str(raw))
                continue
            gene = raw.strip()
            if not is_gene_symbol(gene):
                _err(errors, "marker_bad_format",
                     f"{key} 的 marker {raw!r} 不符合基因符号格式（字母开头、"
                     "仅含字母数字._-、不超过 15 字符）", key)
                ct["dropped_markers"].append(raw)
                continue
            if looks_non_gene_symbol(gene):
                _warn(warnings, "marker_suspect_word",
                      f"{key} 的 marker {gene} 是常见英文词，请人工确认它确实是基因", key)
            gk = gene_key(gene)
            if gk in seen_gene:
                _warn(warnings, "marker_dup_in_celltype",
                      f"{key} 内 marker {gene} 与 {seen_gene[gk]} 重复（忽略大小写）", key)
                continue
            seen_gene[gk] = gene
            marker_owners.setdefault(gk, []).append(key)

        ct["markers"] = len(seen_gene)
        ct["unique"] = len(seen_gene)
        stats["markers_total"] += len(markers)
        if ct["markers"] < min_markers:
            _err(errors, "too_few_markers",
                 f"{key} 只有 {ct['markers']} 个有效 marker，低于 --min-markers={min_markers}", key)
        if ct["markers"] > max_markers:
            _warn(warnings, "too_many_markers",
                  f"{key} 有 {ct['markers']} 个 marker，超过 --max-markers={max_markers}，"
                  "建议人工筛掉低证据的", key)

        # --- refs
        refs = entry.get("refs")
        if not isinstance(refs, list):
            _err(errors, "bad_refs", f"{key} 的 refs 必须是数组", key)
            refs = []
        elif not refs:
            _err(errors, "no_refs", f"{key} 的 refs 为空，无法追溯 marker 来源", key)
        seen_ref, clean_refs = set(), []
        for r in refs:
            if not is_digit_ref(r):
                _err(errors, "ref_not_number",
                     f"{key} 的引用编号 {r!r} 不是数字字符串（下游约定 [\"1\",\"2\"]）", key)
                continue
            rs = str(r).strip()
            if rs in seen_ref:
                _warn(warnings, "ref_dup", f"{key} 的引用编号 {rs} 重复", key)
                continue
            seen_ref.add(rs)
            clean_refs.append(rs)
            if valid_refs is not None and rs not in valid_refs:
                _err(errors, "ref_out_of_range",
                     f"{key} 引用了 refs.json 里不存在的编号 {rs}"
                     f"（有效编号 {len(valid_refs)} 个）", key)
        ct["refs"] = sorted(clean_refs, key=ref_sort_key)
        for rs in clean_refs:
            if rs not in stats["refs_used"]:
                stats["refs_used"].append(rs)
        if valid_refs is not None and clean_refs and not seen_ref.intersection(valid_refs):
            _err(errors, "refs_all_invalid",
                 f"{key} 的所有引用编号都不在 refs.json 里", key)

        per_celltype[key] = ct

    # --- 跨细胞类型：marker 共享度
    stats["markers_unique"] = len(marker_owners)
    for gk, owners in sorted(marker_owners.items()):
        if len(owners) > max_shared:
            _warn(warnings, "marker_over_shared",
                  f"marker {gk} 同时出现在 {len(owners)} 个细胞类型里"
                  f"（{', '.join(sorted(owners))}），超过 --max-shared={max_shared}，特异性存疑")

    stats["refs_used"] = sorted(stats["refs_used"], key=ref_sort_key)
    others = [n for n in marker_owners.values() if len(n) == 2]
    stats["markers_shared_by_2"] = len(others)
    return _report(errors, warnings, stats, per_celltype)


def _report(errors, warnings, stats, per_celltype):
    return {"status": ("error" if errors else ("warning" if warnings else "ok")),
            "errors": errors, "warnings": warnings,
            "error_count": len(errors), "warning_count": len(warnings),
            "stats": stats, "per_celltype": per_celltype}


def format_report(report, dict_path=None):
    """把报告变成人能直接读的多行文本。"""
    lines = []
    if dict_path:
        lines.append(f"词典：{dict_path}")
    s = report["stats"]
    lines.append(f"结论：{report['status']}（error {report['error_count']}，"
                 f"warning {report['warning_count']}）")
    lines.append(f"细胞类型 {s['cell_types']} 个，marker 去重后 {s['markers_unique']} 个"
                 f"（含重复共 {s['markers_total']} 次），用到引用编号 {len(s['refs_used'])} 个")
    if s["empty_cell_types"]:
        lines.append(f"markers 为空的细胞类型：{', '.join(s['empty_cell_types'])}")
    for level in ("errors", "warnings"):
        items = report[level]
        lines.append(f"--- {level}（{len(items)}）---")
        if not items:
            lines.append("  （无）")
        for it in items[:200]:
            where = f"[{it['cell_type']}] " if it.get("cell_type") else ""
            lines.append(f"  {where}{it['code']}: {it['message']}")
        if len(items) > 200:
            lines.append(f"  ... 另有 {len(items) - 200} 条，见 --json 报告")
    return "\n".join(lines)


def main(argv=None):
    ap = argparse.ArgumentParser(
        description="校验 marker 词典：结构、基因名格式、去重、marker 数量、引用编号。")
    ap.add_argument("dict_path", help="要校验的词典 JSON")
    ap.add_argument("--refs", help="refs.json，用于交叉核对引用编号")
    ap.add_argument("--min-markers", type=int, default=3, help="每个细胞类型最少 marker 数")
    ap.add_argument("--max-markers", type=int, default=30, help="每个细胞类型最多 marker 数")
    ap.add_argument("--max-shared", type=int, default=5, help="同一 marker 允许出现的细胞类型数上限")
    ap.add_argument("--json", dest="jsonout", help="校验报告输出路径")
    ap.add_argument("--quiet", action="store_true", help="只输出结论行")
    args = ap.parse_args(argv)

    if args.min_markers < 1:
        ap.error("--min-markers 至少为 1")
    if args.max_markers < args.min_markers:
        ap.error("--max-markers 不能小于 --min-markers")
    if args.max_shared < 1:
        ap.error("--max-shared 至少为 1")

    dictionary, e = load_json(args.dict_path, label="词典")
    if e:
        print(f"错误：{e}", file=sys.stderr)
        return 2

    refs_index, refs_err = None, None
    if args.refs:
        refs_index, refs_err = load_json(args.refs, label="refs.json")
        if refs_err:
            # 不静默降级：refs.json 读不了就说出来，并继续做其余校验
            print(f"警告：{refs_err}；跳过引用编号交叉核对", file=sys.stderr)

    report = validate_dict(dictionary, refs_index=refs_index,
                           min_markers=args.min_markers,
                           max_markers=args.max_markers,
                           max_shared=args.max_shared)
    if refs_err:
        report["warnings"].append({"level": "warning", "code": "refs_index_unreadable",
                                   "message": refs_err, "cell_type": None})
        report["warning_count"] = len(report["warnings"])
        if report["status"] == "ok":
            report["status"] = "warning"
    report["dict_path"] = str(pathlib.Path(args.dict_path))
    report["checks"] = {"min_markers": args.min_markers, "max_markers": args.max_markers,
                        "max_shared": args.max_shared, "refs_cross_checked": bool(refs_index)}

    if args.jsonout:
        write_json(args.jsonout, report)

    if args.quiet:
        print(f"{report['status']}: error {report['error_count']}, "
              f"warning {report['warning_count']}")
    else:
        print(format_report(report, args.dict_path))

    if report["errors"]:
        return 3
    if report["warnings"]:
        return 4
    return 0


if __name__ == "__main__":
    sys.exit(main())
