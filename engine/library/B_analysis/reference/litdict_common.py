#!/usr/bin/env python3
"""sc-lit-dict 的共享工具：格式判定、JSON 读写、引用编号排序。

为什么单独放一个模块：collect.py / assemble.py / validate.py 三个脚本必须用
同一套判定标准（基因符号正则、细胞类型键正则、引用编号如何排序）。如果各写一份，
某天改了正则就会出现「collect 收下的基因名被 validate 判非法」这种自相矛盾。

三个脚本分别是独立的 CLI，本模块只是同目录导入项；导入失败时会打印明确提示，
而不是抛一个看不懂的 ImportError。
"""
from __future__ import annotations

import json
import pathlib
import re
import sys
import unicodedata

__all__ = [
    "GENE_RE", "KEY_RE", "MAX_GENE_LEN", "MAX_KEY_LEN", "NON_GENE_HINTS",
    "is_gene_symbol", "looks_non_gene_symbol", "gene_key", "normalize_celltype",
    "derive_key", "ref_sort_key", "sort_refs", "load_json", "write_json",
    "ensure_dir", "slugify", "die", "is_digit_ref", "EXTRACT_FIELDS",
    "CELLTYPE_FIELDS", "DICT_ENTRY_FIELDS",
]

# 基因符号：字母开头，后接字母/数字/点/下划线/连字符，总长 1-15。
# 覆盖 NPHS1、HLA-DRA、mt-Nd1、H2-Ab1、SNORD3A 这类真实写法；
# 同时挡住「Proximal tubule cell」这种带空格的整句。
GENE_RE = re.compile(r"^[A-Za-z][A-Za-z0-9._-]{0,14}$")
MAX_GENE_LEN = 15

# 细胞类型键：必须是短大写代码（POD / PTC / CD-PC 里的连字符不允许，故用下划线）。
KEY_RE = re.compile(r"^[A-Z][A-Z0-9_]{0,9}$")
MAX_KEY_LEN = 10

# 明显不是基因、但符合上面正则的英文词。只报 warning 不报 error：
# SET / CAT / WAS / BAD / MICE 都是真基因，硬拦会误杀。
NON_GENE_HINTS = frozenset({
    "marker", "markers", "gene", "genes", "cell", "cells", "type", "types",
    "expression", "positive", "negative", "high", "low", "signature",
    "signatures", "score", "scores", "cluster", "clusters", "unknown",
})

EXTRACT_FIELDS = ("ref", "cell_types")
# 抽取结果的 cell_types 条目允许出现的字段（assemble.py 读抽取时用）
CELLTYPE_FIELDS = ("cell_type", "key", "markers", "evidence")

# 最终词典里每个细胞类型允许出现的字段（validate.py 校验词典时用）。
# 这两个字段集合不同，别混用——下游约定只认 markers/refs/full。
DICT_ENTRY_FIELDS = ("markers", "refs", "full")


# ------------------------------------------------------------------ 判定
def is_gene_symbol(token: str) -> bool:
    """是否是格式合法的基因符号。空串、带空格、超长、纯数字都返回 False。"""
    if not isinstance(token, str):
        return False
    t = token.strip()
    if len(t) > MAX_GENE_LEN:
        return False
    return bool(GENE_RE.match(t))


def looks_non_gene_symbol(token: str) -> bool:
    """是否是「像基因但基本可以确定不是基因」的英文词，用于告警。"""
    return isinstance(token, str) and token.strip().lower() in NON_GENE_HINTS


def gene_key(token: str) -> str:
    """去重用的比较键：大小写不敏感。显示时仍用原文拼写。"""
    return token.strip().upper()


def normalize_celltype(name: str) -> str:
    """细胞类型名归一化，用于跨文献归并。

    做三件事：转小写、去掉 "podocytes" 这类复数尾部 s、把标点压成单空格。
    不做同义词合并（Podocyte 与 Visceral epithelial cell 不会被自动合并，
    要合并请在 assemble.py 用 --alias-map 显式声明）。
    """
    if not isinstance(name, str):
        return ""
    s = unicodedata.normalize("NFKC", name).lower()
    s = re.sub(r"[^a-z0-9]+", " ", s).strip()
    words = []
    for w in s.split():
        if len(w) > 3 and w.endswith("s") and not w.endswith("ss"):
            w = w[:-1]
        words.append(w)
    return " ".join(words)


def derive_key(full_name: str):
    """从细胞类型全称推导一个稳定的短键。

    规则（确定性，无随机）：
      - 单词全称：长度 >=3 取前 3 个字母，否则原样大写（如 Podocyte -> POD）
      - 多词全称：取每个词首字母拼起来，最多 6 位（如 Proximal Tubule Cell -> PTC）
    推导出来的键一律在报告里标成「自动推导，需人工确认」，因为下游词典的键
    是业务约定，脚本不应替用户拍板。
    """
    if not isinstance(full_name, str):
        return None
    words = [w for w in re.split(r"[^A-Za-z0-9]+", full_name) if w]
    if not words:
        return None
    if len(words) == 1:
        w = words[0].upper()
        return w[:3] if len(w) >= 3 else w
    return "".join(w[0] for w in words).upper()[:6]


def is_digit_ref(ref) -> bool:
    """引用编号是否是数字字符串（下游约定：["1","2"]）。"""
    return isinstance(ref, str) and ref.strip().isdigit()


def ref_sort_key(ref: str):
    """引用编号排序：数字按数值排，非数字排到最后并按字符串排。"""
    s = str(ref).strip()
    if s.isdigit():
        return (0, int(s), "")
    return (1, 0, s)


def sort_refs(refs):
    return sorted({str(r).strip() for r in refs if str(r).strip()}, key=ref_sort_key)


# ------------------------------------------------------------------ IO
def ensure_dir(path) -> pathlib.Path:
    p = pathlib.Path(path)
    p.mkdir(parents=True, exist_ok=True)
    return p


def load_json(path, required=True, label="JSON"):
    """读 JSON。返回 (对象, 错误说明)。错误不抛异常，交给调用方决定怎么记。"""
    p = pathlib.Path(path)
    if not p.exists():
        return None, f"{label} 不存在：{p}"
    try:
        return json.loads(p.read_text(encoding="utf-8")), None
    except json.JSONDecodeError as e:
        return None, f"{label} 不是合法 JSON（第 {e.lineno} 行 {e.msg}）：{p}"
    except OSError as e:
        return None, f"{label} 读取失败（{type(e).__name__}: {e}）：{p}"


def write_json(path, obj):
    p = pathlib.Path(path)
    ensure_dir(p.parent)
    p.write_text(json.dumps(obj, ensure_ascii=False, indent=2), encoding="utf-8")
    return p


def slugify(text, maxlen=48, fallback="paper") -> str:
    """把标题变成文件名安全的短串；只用 ASCII，空结果回落到 fallback。"""
    s = unicodedata.normalize("NFKD", str(text or ""))
    s = s.encode("ascii", "ignore").decode("ascii").lower()
    s = re.sub(r"[^a-z0-9]+", "_", s).strip("_")
    return (s[:maxlen].strip("_") or fallback)


def die(msg, code=2):
    """统一退出：使用错误走码 2，数据缺口走 3/4（由调用方指定）。"""
    print(f"错误：{msg}", file=sys.stderr)
    sys.exit(code)
