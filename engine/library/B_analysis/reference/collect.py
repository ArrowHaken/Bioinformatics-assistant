#!/usr/bin/env python3
"""collect.py — 第一段：脚本取证。

按「物种 + 组织 + 细胞类型关键词」调用 academic-db 做检索，再对命中文献逐篇
尝试取合法开放全文，全部落地到 runs/<批次>/ 下。本脚本不做任何生物学判断，
只负责「把可核查的原料摆上桌」。

它做完之后，轮到 LLM 读 papers/*.txt 产出抽取结果（见 SKILL.md 的人机分工）。
本脚本不猜基因名、不猜编号；检索失败、全文取不到一律写进 failures.json。

用法
----
  python3 scripts/collect.py --species human --tissue kidney \
      --celltype Podocyte,ProximalTubuleCell --outdir runs/kidney-20261009

  # 先只检索、不取全文（冒烟测试，秒级返回）
  python3 scripts/collect.py --species human --tissue kidney \
      --celltype Podocyte --outdir runs/smoke --max-fulltext 0

参数
----
  --species       必填。物种，如 human / mouse。会拼进检索式。
  --tissue        必填。组织，如 kidney / lung。会拼进检索式。
  --celltype      必填。细胞类型关键词，逗号分隔。每个关键词单独检索一次。
  --outdir        必填。批次目录，不存在则创建。
  --limit         每次检索返回条数上限，默认 20。
  --year-from     只保留该年份之后的文献（传给 academic-db），默认不限。
  --query-suffix  检索式后缀，默认 "marker gene single-cell RNA-seq"。
  --academic-db   academic-db skill 根目录，默认同级目录 ../academic-db。
  --max-fulltext  最多对前 N 篇取全文，默认 12；0 表示跳过取全文。
  --min-text-chars 全文少于该字符数视为无效，默认 2000。
  --timeout       单篇取全文的子进程超时秒数，默认 180。
  --dry-run       只打印将执行的检索式与路径，不联网、不写文件。

退出码
------
  0 正常；2 参数错误；3 一条文献都没检索到；4 检索到了但没有一篇取到有效全文。
"""
from __future__ import annotations

import argparse
import json
import pathlib
import re
import subprocess
import sys
import time

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from litdict_common import ensure_dir, slugify, write_json  # noqa: E402

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
SKILL_DIR = SCRIPT_DIR.parent
DEFAULT_ACADEMIC_DB = SKILL_DIR.parent / "academic-db"

# 供 LLM 抽取的输入文件后缀。assemble.py 按 "<ref>_<slug>.extract.json" 找抽取结果。
EXTRACT_SUFFIX = ".extract.json"

EXTRACT_README = """# 本目录的原料说明（给抽取环节）

papers/ 下每篇文献两份文件：

  <ref>_<slug>.txt         全文纯文本。这是抽取的唯一文本来源。
  <ref>_<slug>.meta.json   题录与取全文的 trace（是否开放获取、走了哪条合法路径）。

## 分工

脚本已经把「检索 + 取全文」做完。接下来请 LLM 逐篇阅读 .txt，产出抽取结果：

  <ref>_<slug>.extract.json

文件名中间的 <ref> 必须与 refs.json 里的编号一致，assemble.py 靠它回填 refs。

## 抽取结果固定 schema

{
  "ref": "1",                         // 必须与文件名前缀一致
  "doi": "10.xxxx/xxxx",              // 抄自同名 meta.json，找不到就写 null
  "cell_types": [
    {
      "cell_type": "Podocyte",        // 原文里的写法，别自己发明
      "key": "POD",                   // 可留空；留空则 assemble.py 按全称推导
      "markers": ["NPHS1", "NPHS2"],  // 原文明确当作该类型 marker 的基因符号
      "evidence": "原文片段，15-200 字，直接摘录"
    }
  ],
  "notes": ""                         // 可选：读不懂、文中有歧义的地方写这里
}

## 硬性要求

1. markers 只能写原文里出现的基因符号。不确定是不是基因，就不要写。
2. evidence 必须是从 .txt 里摘的原句，不要改写，不要编。
3. 这篇文献没有可用于判定细胞类型的 marker，就产出
   {"ref": "...", "doi": "...", "cell_types": [], "notes": "无可用 marker"}。
4. 不要为了凑数把功能描述、通路名、化合物名当基因写进来。
"""


# ------------------------------------------------------------------ 参数
def parse_args(argv=None):
    ap = argparse.ArgumentParser(
        description="检索文献并抓取开放全文，落地为 LLM 抽取的原料。",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__.split("用法")[-1],
    )
    ap.add_argument("--species", required=True, help="物种，如 human / mouse")
    ap.add_argument("--tissue", required=True, help="组织，如 kidney / lung")
    ap.add_argument("--celltype", required=True,
                    help="细胞类型关键词，逗号分隔，如 Podocyte,ProximalTubuleCell")
    ap.add_argument("--outdir", required=True, help="批次输出目录")
    ap.add_argument("--limit", type=int, default=20, help="每次检索条数上限")
    ap.add_argument("--year-from", type=int, default=None, help="起始年份")
    ap.add_argument("--query-suffix", default="marker gene single-cell RNA-seq",
                    help="检索式后缀")
    ap.add_argument("--academic-db", default=str(DEFAULT_ACADEMIC_DB),
                    help="academic-db skill 根目录")
    ap.add_argument("--max-fulltext", type=int, default=12,
                    help="最多取全文的篇数；0 表示跳过")
    ap.add_argument("--min-text-chars", type=int, default=2000,
                    help="全文少于该字符数视为无效")
    ap.add_argument("--timeout", type=int, default=180, help="单篇取全文超时秒数")
    ap.add_argument("--dry-run", action="store_true", help="只打印计划，不联网")
    args = ap.parse_args(argv)

    # ---- 参数校验：宁可在这里吵，也不要带着坏参数跑十分钟
    if not args.species.strip() or not args.tissue.strip():
        ap.error("--species 与 --tissue 都不能为空")
    args.celltypes = [c.strip() for c in args.celltype.split(",") if c.strip()]
    if not args.celltypes:
        ap.error("--celltype 至少要有一个非空关键词")
    if args.limit <= 0:
        ap.error("--limit 必须为正整数")
    if args.max_fulltext < 0:
        ap.error("--max-fulltext 不能为负")
    if args.min_text_chars < 0:
        ap.error("--min-text-chars 不能为负")
    if args.timeout <= 0:
        ap.error("--timeout 必须为正整数")
    if args.year_from is not None and (args.year_from < 1900 or args.year_from > 2100):
        ap.error("--year-from 应在 1900-2100 之间")

    adb = pathlib.Path(args.academic_db).expanduser().resolve()
    if not (adb / "scripts" / "search.py").exists():
        ap.error(f"--academic-db 目录下找不到 scripts/search.py：{adb}")
    if not (adb / "scripts" / "fulltext.py").exists():
        ap.error(f"--academic-db 目录下找不到 scripts/fulltext.py：{adb}")
    outdir = pathlib.Path(args.outdir).expanduser()
    if outdir.exists() and not outdir.is_dir():
        ap.error(f"--outdir 已存在但不是目录（是个文件）：{outdir}")
    args.academic_db_path = adb
    args.outdir_path = outdir
    return args


# ------------------------------------------------------------------ 子进程
def run_cmd(cmd, cwd, timeout):
    """跑一个子进程并抓 stdout/stderr。不抛异常，失败信息交给调用方记录。"""
    t0 = time.time()
    try:
        p = subprocess.run(cmd, cwd=str(cwd), capture_output=True, text=True,
                           timeout=timeout, check=False)
        return {"returncode": p.returncode, "stdout": p.stdout[-4000:],
                "stderr": p.stderr[-4000:], "elapsed": round(time.time() - t0, 2),
                "timeout": False}
    except subprocess.TimeoutExpired:
        return {"returncode": None, "stdout": "", "stderr": f"超过 {timeout}s 未返回",
                "elapsed": round(time.time() - t0, 2), "timeout": True}
    except OSError as e:
        return {"returncode": None, "stdout": "",
                "stderr": f"{type(e).__name__}: {e}", "elapsed": round(time.time() - t0, 2),
                "timeout": False}


def build_query(species, tissue, celltype, suffix):
    return " ".join(x for x in (species.strip(), tissue.strip(), celltype.strip(),
                                suffix.strip()) if x)


# ------------------------------------------------------------------ 检索
def search_one(args, query, idx, search_dir):
    """跑一次 academic-db 检索，返回 (原始 JSON, 失败说明)。"""
    raw_path = search_dir / f"query-{idx:02d}.json"
    cmd = [sys.executable, "scripts/search.py", query, "--limit", str(args.limit),
           "--json", str(raw_path)]
    if args.year_from:
        cmd += ["--year-from", str(args.year_from)]
    r = run_cmd(cmd, cwd=args.academic_db_path, timeout=args.timeout)
    entry = {"query": query, "raw_path": str(raw_path),
             "returncode": r["returncode"], "elapsed": r["elapsed"],
             "timeout": r["timeout"], "stderr": r["stderr"]}

    if r["timeout"]:
        entry["error"] = "检索子进程超时"
        return None, entry
    if not raw_path.exists():
        entry["error"] = ("academic-db 未产出结果文件"
                          + ("（返回码非 0）" if r["returncode"] else ""))
        return None, entry
    try:
        data = json.loads(raw_path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as e:
        entry["error"] = f"结果文件不是合法 JSON：第 {e.lineno} 行 {e.msg}"
        return None, entry
    entry["raw_count"] = data.get("raw_count")
    entry["unique_count"] = data.get("unique_count")
    entry["fanout"] = data.get("fanout")          # 逐源状态，原样保留进报告
    entry["wall_clock_s"] = data.get("wall_clock_s")
    return data, entry


def merge_records(per_query):
    """把多次检索的结果按 DOI 去重合并。保留每条命中的全部来源查询。"""
    by_key, order = {}, []
    for q_idx, (query, data) in enumerate(per_query):
        for r in (data or {}).get("results", []) or []:
            doi = (r.get("doi") or "").strip()
            key = doi.lower() if doi else (r.get("dedupe_key") or r.get("title") or "")
            key = str(key).strip().lower()
            if not key:
                continue
            if key not in by_key:
                rec = dict(r)
                rec["matched_queries"] = [query]
                rec["_order"] = len(order)
                by_key[key] = rec
                order.append(key)
            else:
                rec = by_key[key]
                if query not in rec["matched_queries"]:
                    rec["matched_queries"].append(query)
                # 补全先前为空的字段（后到的源可能带摘要或 OA 链接）
                for f in ("abstract", "oa_url", "year", "venue", "cited", "doi"):
                    if not rec.get(f) and r.get(f):
                        rec[f] = r[f]
    return [by_key[k] for k in order]


def assign_refs(records, max_fulltext):
    """给每篇文献一个固定的引用编号（下游 refs 用字符串数字）。

    排序规则固定且可复现：先按有无 DOI、再按年份降序、最后按 DOI 字典序。
    编号与是否取到全文无关——取全文失败不改变编号，否则 refs 会漂移。
    """
    def sort_key(r):
        return (0 if (r.get("doi") or "").strip() else 1,
                -(r.get("year") or 0),
                (r.get("doi") or r.get("title") or "").lower())

    ordered = sorted(records, key=sort_key)
    refs = {}
    for i, r in enumerate(ordered, start=1):
        ref = str(i)
        r["ref"] = ref
        r["fetch_fulltext"] = i <= max_fulltext
        refs[ref] = {
            "ref": ref,
            "title": r.get("title"),
            "authors": r.get("authors"),
            "year": r.get("year"),
            "venue": r.get("venue"),
            "doi": r.get("doi"),
            "url": r.get("url"),
            "cited": r.get("cited"),
            "oa_url": r.get("oa_url"),
            "abstract": r.get("abstract"),
            "sources": r.get("also_in") or ([r.get("source")] if r.get("source") else []),
            "matched_queries": r.get("matched_queries", []),
        }
    return ordered, refs


# ------------------------------------------------------------------ 取全文
def fetch_fulltext(args, papers_dir, rec, failures):
    """对一篇文献取合法开放全文，写 .txt 与 .meta.json。

    判定顺序完全交给 academic-db 的 fulltext.py（Europe PMC XML -> Unpaywall 合法
    OA PDF），本脚本不自己拼正文、不绕过付费墙。取不到就如实记 failure。
    """
    ref = rec["ref"]
    stem = f"{int(ref):04d}_{slugify(rec.get('title'), fallback='paper')}"
    txt_path = papers_dir / f"{stem}.txt"
    meta_path = papers_dir / f"{stem}.meta.json"

    base = {"ref": ref, "title": rec.get("title"), "doi": rec.get("doi"),
            "file_stem": stem, "txt_path": str(txt_path),
            "meta_path": str(meta_path),
            "extract_path": str(papers_dir / f"{stem}{EXTRACT_SUFFIX}")}

    doi = (rec.get("doi") or "").strip()
    if not doi:
        base["status"] = "no_doi"
        base["reason"] = "题录里没有 DOI，无法走合法全文路径"
        write_json(meta_path, base)
        failures.append({**base, "stage": "fetch"})
        return "no_doi"

    out_json = papers_dir / f"{stem}.fulltext.json"
    cmd = [sys.executable, "scripts/fulltext.py", "text", "--doi", doi,
           "--out", str(out_json)]
    r = run_cmd(cmd, cwd=args.academic_db_path, timeout=args.timeout)
    base["fetch_elapsed_s"] = r["elapsed"]

    if r["timeout"]:
        base.update(status="fetch_timeout",
                    reason=f"取全文子进程超过 {args.timeout}s 未返回")
        write_json(meta_path, base)
        failures.append({**base, "stage": "fetch"})
        return "fetch_timeout"

    if not out_json.exists():
        base.update(status="fetch_error",
                    reason=f"fulltext.py 未产出结果文件；stderr={r['stderr'][:200]}")
        write_json(meta_path, base)
        failures.append({**base, "stage": "fetch"})
        return "fetch_error"

    try:
        data = json.loads(out_json.read_text(encoding="utf-8"))
    except json.JSONDecodeError as e:
        base.update(status="fetch_error", reason=f"全文结果不是合法 JSON：{e.msg}")
        write_json(meta_path, base)
        failures.append({**base, "stage": "fetch"})
        return "fetch_error"

    status = data.get("status")
    base["route"] = data.get("route")
    base["chars"] = data.get("chars")
    base["trace"] = data.get("trace")

    if status != "ok":
        base.update(status=f"no_legal_fulltext:{status}",
                    reason="academic-db 未找到合法开放全文（不绕过付费墙）")
        write_json(meta_path, base)
        failures.append({**base, "stage": "fetch"})
        return "no_legal_fulltext"

    text = data.get("text") or ""
    if len(text) < args.min_text_chars:
        base.update(status="text_too_short",
                    reason=f"取到 {len(text)} 字符，低于 --min-text-chars={args.min_text_chars}")
        write_json(meta_path, base)
        failures.append({**base, "stage": "fetch"})
        return "text_too_short"

    txt_path.write_text(text, encoding="utf-8")
    base.update(status="ok", chars=len(text))
    write_json(meta_path, base)
    return "ok"


# ------------------------------------------------------------------ main
def main(argv=None):
    args = parse_args(argv)
    out = args.outdir_path
    search_dir = out / "search"
    papers_dir = out / "papers"

    queries = [build_query(args.species, args.tissue, c, args.query_suffix)
               for c in args.celltypes]

    if args.dry_run:
        print("【dry-run】不会联网、不会写文件。将执行：")
        print(f"  批次目录：{out}")
        for i, q in enumerate(queries, 1):
            print(f"  检索 {i}: {q}")
        print(f"  检索命令：python3 scripts/search.py \"<检索式>\" "
              f"--limit {args.limit}{' --year-from ' + str(args.year_from) if args.year_from else ''}"
              f" --json <批次>/search/query-NN.json   （工作目录 {args.academic_db_path}）")
        print(f"  取全文：前 {args.max_fulltext} 篇，逐篇 "
              f"python3 scripts/fulltext.py text --doi <DOI> --out <批次>/papers/NNNN_*.fulltext.json")
        print(f"  抽取指引：{papers_dir / 'EXTRACT_README.md'}")
        return 0

    try:
        ensure_dir(search_dir)
        ensure_dir(papers_dir)
    except OSError as e:
        # 抛栈对使用者没有信息量；这里换成「哪条路径、什么原因」
        print(f"错误：批次目录创建失败（{type(e).__name__}: {e}）。"
              f"请检查 --outdir 路径是否可写、父路径是否存在：{out}", file=sys.stderr)
        return 2
    write_json(out / "batch_params.json", {
        "species": args.species, "tissue": args.tissue,
        "celltypes": args.celltypes, "limit": args.limit,
        "year_from": args.year_from, "query_suffix": args.query_suffix,
        "max_fulltext": args.max_fulltext, "min_text_chars": args.min_text_chars,
        "academic_db": str(args.academic_db_path),
        "queries": queries, "started_at": time.strftime("%Y-%m-%dT%H:%M:%S"),
    })

    # ---- 第一段：检索
    per_query, search_log = [], []
    for i, q in enumerate(queries, 1):
        print(f"[检索 {i}/{len(queries)}] {q}", flush=True)
        data, entry = search_one(args, q, i, search_dir)
        search_log.append(entry)
        if data:
            per_query.append((q, data))
            print(f"  -> 命中 {data.get('unique_count')} 条，用时 {data.get('wall_clock_s')}s")
        else:
            print(f"  -> 失败：{entry.get('error')}（已记录，不静默跳过）")

    records = merge_records(per_query)
    if not records:
        write_json(out / "failures.json", {
            "stage": "search", "note": "所有检索均未返回可用文献",
            "queries": search_log})
        write_json(out / "search_log.json", search_log)
        print("\n没有检索到任何文献。已写 failures.json，未生成任何抽取原料。", file=sys.stderr)
        return 3

    ordered, refs = assign_refs(records, args.max_fulltext)
    write_json(out / "refs.json", refs)
    write_json(out / "search_log.json", search_log)

    # ---- 第二段：取全文
    failures = []
    fetched = 0
    if args.max_fulltext == 0:
        print("\n--max-fulltext=0：跳过取全文，只保留题录。")
    else:
        to_fetch = [r for r in ordered if r.get("fetch_fulltext")]
        for i, rec in enumerate(to_fetch, 1):
            print(f"[全文 {i}/{len(to_fetch)}] ref={rec['ref']} {str(rec.get('title'))[:60]}",
                  flush=True)
            st = fetch_fulltext(args, papers_dir, rec, failures)
            if st == "ok":
                fetched += 1
                print("  -> ok")
            else:
                print(f"  -> {st}（已记入 failures.json）")

    # ---- 抽取指引 + 报告
    (papers_dir / "EXTRACT_README.md").write_text(EXTRACT_README, encoding="utf-8")

    # 引用编号索引的简表，方便 LLM 与人核对 ref <-> 文献
    idx_lines = ["# 引用编号索引（refs.json 的可读版）", "",
                 "抽取结果的 ref 字段必须与下表编号一致。", ""]
    for ref in sorted(refs, key=lambda x: int(x)):
        m = refs[ref]
        idx_lines.append(f"- {ref}. {m.get('title')} "
                         f"({m.get('year')}) {m.get('venue') or ''} {m.get('doi') or ''}")
    (out / "REF_INDEX.md").write_text("\n".join(idx_lines) + "\n", encoding="utf-8")

    report = {
        "stage": "collect",
        "batch_dir": str(out),
        "params": {"species": args.species, "tissue": args.tissue,
                   "celltypes": args.celltypes},
        "queries_run": len(queries),
        "queries_failed": [e for e in search_log if e.get("error")],
        "records_total": len(ordered),
        "records_with_doi": sum(1 for r in ordered if (r.get("doi") or "").strip()),
        "fulltext_requested": len([r for r in ordered if r.get("fetch_fulltext")]),
        "fulltext_ok": fetched,
        "failures": failures,
        "next_step": ("把这些 papers/*.txt 交给 LLM 逐篇抽取，产出 "
                      f"<ref>_<slug>{EXTRACT_SUFFIX}（schema 见 papers/EXTRACT_README.md），"
                      "然后跑 assemble.py。"),
    }
    write_json(out / "failures.json", {"stage": "fulltext", "failures": failures})
    write_json(out / "collect_report.json", report)

    print(f"\n批次目录：{out}")
    print(f"文献 {len(ordered)} 篇（有 DOI {report['records_with_doi']} 篇），"
          f"取到全文 {fetched} 篇，失败 {len(failures)} 篇。")
    print("原料齐了：把 papers/*.txt 交给 LLM 抽取，再跑 assemble.py。")

    if args.max_fulltext > 0 and fetched == 0:
        print("一篇有效全文都没取到，无法进入抽取环节。", file=sys.stderr)
        return 4
    return 0


if __name__ == "__main__":
    sys.exit(main())
