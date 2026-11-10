#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
workpc_scan_ref.py -- 核对「用户指定的那一份注释依据表」（只读）。

2026-11-04 改版：不再扫目录，只核对指定路径。
  上一版会在给的一批根目录里按文件名找候选表。实测问题很直接：
    · 用户其实知道自己要给哪一份，问一句比扫一遍快得多；
    · 扫一遍要限深、限数量、设时长闸门才不至于撞超时，而即使限了，
      第一版现场全盘深扫仍然白等 300 秒；
    · 扫出来的候选表还得人再挑一次，等于把同一个决定问了两遍。
  所以现在只做一件事：你给路径，它去核对这个文件在不在、多大、什么表头、
  表里声明的物种与组织是什么，然后输出一份可以直接回写的 JSON。
  没有任何递归遍历，没有 find，没有"顺便看看别的"。

用法：
  export SC_REMOTE_PWD='登录口令'          # 口令只走环境变量，不进 argv/日志
  python3 workpc_scan_ref.py \\
      --host 10.1.1.2 --user hhhong1 \\
      --path /share/home/.../Supplementary_Table_5.xlsx \\
      --out ./ref_check.json
  --path 可以给多次（一次要比对多份时）。给的是目录而不是文件时，
  只列这一层（maxdepth 1），不递归。

回写：
  POST /api/reference/scan   body 就是本次输出的 JSON 原文。

红线（写在这里，也写在下发的指令里）：
  · 不递归、不 find、不扫 / 或 /share；
  · 找不到就如实报 not_found，不要拿名字相近的文件凑数；
  · 物种与组织只从表头里读，读不出来就留空，不猜。
"""

import argparse
import json
import os
import posixpath
import sys
import time

try:
    import paramiko
except ImportError:
    sys.stderr.write("paramiko 没装。先装： python3 -m pip install paramiko\n")
    sys.exit(3)

EXT_OK = ("xlsx", "xls", "tsv", "csv", "json")


def sh(client, cmd, timeout=45):
    """一条远端命令。返回 (rc, stdout, stderr)。异常不抛，交给调用方判。"""
    try:
        _, stdout, stderr = client.exec_command(cmd, timeout=timeout)
        out = stdout.read().decode("utf-8", "replace")
        err = stderr.read().decode("utf-8", "replace")
        return stdout.channel.recv_exit_status(), out, err
    except Exception as e:
        return -1, "", str(e)


def preview_header(client, path, budget=8):
    """
    看一眼表头，给物种/组织留线索。只读、限时、失败就空着。
    为什么不用 openpyxl：服务器上不保证装了它；用 unzip 抽 sharedStrings
    是纯 shell，不挑环境。2026-11-04 那轮为了解析 xlsx 现场写了三版脚本
    都没稳住，这里一次固定下来。
    """
    ext = posixpath.splitext(path)[1].lower()
    if ext in (".xlsx", ".xlsm"):
        cmd = ("unzip -p '%s' xl/sharedStrings.xml 2>/dev/null "
               "| tr '<' '\\n' | grep -o 't>[^<]*' | sed 's/^t>//' | head -40 "
               "| tr '\\n' '|'" % path)
    elif ext == ".xls":
        cmd = "strings -n 3 '%s' 2>/dev/null | head -40 | tr '\\n' '|'" % path
    else:
        cmd = "head -c 2000 '%s' 2>/dev/null | tr '\\r\\n' '  '" % path
    rc, out, _ = sh(client, cmd, timeout=budget)
    if rc != 0 or not out.strip():
        return ""
    return " ".join(out.split())[:600]


def species_of(text):
    """从表头文本判物种。判不出返回空 —— 猜一个写上去，后面每个人都会当真。"""
    t = (text or "").lower()
    mouse = sum(t.count(k) for k in ("mouse", "mus musculus", "小鼠", "mm10", "grcm"))
    human = sum(t.count(k) for k in ("human", "homo sapiens", "人", "hg19", "hg38", "grch"))
    if mouse and not human:
        return "Mouse"
    if human and not mouse:
        return "Human"
    return ""


def tissue_of(text):
    t = (text or "").lower()
    for k, v in (("spleen", "脾"), ("脾", "脾"),
                 ("kidney", "肾"), ("肾", "肾"),
                 ("lung", "肺"), ("肺", "肺"),
                 ("liver", "肝"), ("肝", "肝"),
                 ("pbmc", "外周血"), ("blood", "血"), ("血", "血"),
                 ("brain", "脑"), ("脑", "脑"),
                 ("tumor", "肿瘤"), ("肿瘤", "肿瘤")):
        if k in t:
            return v
    return ""


def check_path(client, path):
    """核对一条路径。返回一条 item（与 /api/reference/scan 的 items 同构）。"""
    q = path.replace("'", "'\\''")
    rc, out, err = sh(client, "if [ -f '%s' ]; then "
                              "stat -c '%%s\\t%%y' '%s'; echo FILE; "
                              "elif [ -d '%s' ]; then echo DIR; "
                              "else echo MISSING; fi" % (q, q, q))
    if rc == -1:
        return {"path": path, "name": posixpath.basename(path), "bytes": 0,
                "status": "unreachable", "note": "连不上或命令超时：%s" % err[:120]}

    lines = [l.strip() for l in out.splitlines() if l.strip()]
    if "MISSING" in lines:
        return {"path": path, "name": posixpath.basename(path), "bytes": 0,
                "status": "not_found", "note": "服务器上没有这个文件"}

    if "DIR" in lines:
        # 给的是目录：只列这一层（maxdepth 1），不递归。
        rc2, out2, _ = sh(client, "ls -1p '%s' 2>/dev/null | head -60" % q)
        names = [n for n in out2.splitlines() if n.strip()]
        return {"path": path, "name": posixpath.basename(path), "bytes": 0,
                "status": "is_directory",
                "note": "这是目录，不是文件。这一层有 %d 项：%s"
                        % (len(names), ", ".join(names[:20]))}

    size, mtime = (lines[0].split("\t") + ["", ""])[:2]
    ext = posixpath.splitext(path)[1].lower().lstrip(".")
    if ext and ext not in EXT_OK:
        return {"path": path, "name": posixpath.basename(path),
                "bytes": int(size) if size.isdigit() else 0,
                "mtime": mtime, "status": "bad_ext",
                "note": "扩展名 .%s 不在 xlsx/xls/tsv/csv/json 之内" % ext}

    item = {"path": path, "name": posixpath.basename(path),
            "bytes": int(size) if size.isdigit() else 0,
            "mtime": mtime, "status": "ok", "note": ""}
    hd = preview_header(client, path)
    if hd:
        sp, ti = species_of(hd), tissue_of(hd)
        item["header_preview"] = hd[:200]
        if sp:
            item["species"] = sp
        if ti:
            item["tissue"] = ti
        item["note"] = "表头线索：" + hd[:120]
    else:
        item["note"] = "表头读不出来，物种与组织留空"
    return item


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--host", required=True)
    ap.add_argument("--port", type=int, default=22)
    ap.add_argument("--user", required=True)
    ap.add_argument("--path", action="append", required=True,
                    help="要核对的文件绝对路径，可给多次")
    ap.add_argument("--timeout", type=int, default=90,
                    help="总时长闸门（秒），到点用已完成部分收尾")
    ap.add_argument("--out", default="")
    a = ap.parse_args()

    pwd = os.environ.get("SC_REMOTE_PWD", "")
    if not pwd:
        sys.stderr.write("SC_REMOTE_PWD 是空的 —— 口令要走环境变量传\n")
        sys.exit(4)

    # 路径先自检：不接受 / 或 /share 这类根，也不接受空。
    paths = []
    for p in a.path:
        p = (p or "").strip()
        if not p:
            continue
        if p.rstrip("/") in ("", "/", "/share", "/home", "/backup2"):
            sys.stderr.write("拒绝核对过大的目录：%r（请给具体文件的绝对路径）\n" % p)
            sys.exit(5)
        paths.append(p)
    if not paths:
        sys.stderr.write("至少要给一个 --path\n")
        sys.exit(5)

    t0 = time.time()
    client = paramiko.SSHClient()
    client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
    client.connect(a.host, port=a.port, username=a.user, password=pwd,
                   timeout=15, banner_timeout=30, auth_timeout=30,
                   look_for_keys=False, allow_agent=False)

    items = []
    notes = []
    for p in paths:
        if time.time() - t0 > a.timeout:
            notes.append("到时长上限，剩下 %d 条未核对" % (len(paths) - len(items)))
            break
        items.append(check_path(client, p))
    client.close()

    okN = sum(1 for x in items if x.get("status") == "ok")
    obj = {
        "ok": True,
        "host": a.host,
        "items": [x for x in items if x.get("status") == "ok"],
        "checked": items,
        "checked_total": len(items),
        "ok_total": okN,
        "elapsed_sec": round(time.time() - t0, 1),
        "notes": notes,
    }
    txt = json.dumps(obj, ensure_ascii=False, indent=2)
    if a.out:
        with open(a.out, "w", encoding="utf-8") as fh:
            fh.write(txt)
    sys.stdout.write(txt + "\n")


if __name__ == "__main__":
    main()
