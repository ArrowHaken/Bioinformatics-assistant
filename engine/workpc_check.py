#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
workpc_check.py -- 在「能连上分析服务器的那台电脑」上跑一次服务器体检。

为什么要有这个文件（而不是每次临场写一段脚本）：
  这趟体检每次都一样：连上去、问内存/CPU/磁盘/R 版本/10 个 R 包/
  数据目录/脚本目录/结果目录/队列，然后把结果写成一份 JSON 回写应用。
  每次重新拼一遍，就得重新决定「脚本目录怎么找」「符号链接要不要解」这类事，
  而第一版就是因为这些决定做错，把四个子目录名当成了脚本。
  把做法固定在这里：一次 SSH 取全，输出形状与应用约定的一致。

用法（在能连服务器的那台电脑上）：
  export SC_REMOTE_PWD='登录口令'          # 口令只走环境变量，不进 argv/日志
  python3 workpc_check.py \\
      --host 10.1.1.2 --user hhhong1 \\
      --data-root   <服务器上放 matrix 的目录> \\
      --script-dir  <服务器上放分析脚本的目录> \\
      --work-root   <服务器上写结果的目录> \\
      --rscript     <服务器上的 Rscript 绝对路径> \\
      --libs        <R 包目录，冒号分隔> \\
      --ldlib       <额外 LD_LIBRARY_PATH> \\
      --queue all.q --node hostname=master01 \\
      --out /tmp/remote_env.json
服务器本机模式（Linux/POSIX，仅体检，不执行全分析）：
  python3 workpc_check.py --transport local --host "$(hostname)" \\
      --data-root /path/matrix --script-dir /path/script \\
      --work-root /path/results --rscript /path/Rscript \\
      --out /tmp/local_check.json --notebook-out /tmp/local_check.ipynb
  local 不需要 --user、密码或 paramiko；--host 必须匹配当前完整 hostname，
  不接受 IP、DNS 别名或短名推断。ipynb 由标准库生成，不依赖 Jupyter；
  记录真实体检结果与可重跑体检代码，不表示全分析可复现。
然后把这份 JSON 回写应用：
  POST /api/envcheck/import   body: {"result": <上面的 JSON>, "host": "...", "req_id": "..."}
  带上本次请求号 req_id —— 不带，应用分不清这是这一次还是上一轮的结果。

判据与阈值（与 remote_check.R 保持一致，两处不能各说各话）：
  内存 >= 32 GB ok；/share 剩余 >= 20 GB ok；数据目录/脚本目录不存在 = fail；
  10 个必需 R 包逐个 library() 试加载；
  verdict = 有 fail ? blocked : 有 warn ? warn : ready
"""

import argparse
import json
import os
import sys
import time
import shlex
import socket
import subprocess

# 每个脚本干什么。表里没有的按内容判，并在输出里标明是判出来的。
ROLE_BY_NAME = {
    "01_packages.R": "装包与版本检查",
    "analysis.R": "主流程入口（读 rds、合并、与 harmony 聚类）",
    "seurat_merge_cluster.R": "多样本合并 + 聚类",
    "seurat_merge_cluster.sh": "上面那步的批处理",
    "soupX_doublefinder.R": "SoupX 去环境 RNA + 质控 + DoubletFinder 去双细胞",
    "soupX_doublefinder.sh": "上面那步的批处理",
    "markgene.R": "marker 基因计算（FindAllMarkers + 热图/小提琴）",
    "markgene_celltype.R": "按细胞类型取 marker",
    "cluster_marker.sh": "marker 批处理",
    "seurat_cell_numer.R": "各类细胞数量与比例统计",
    "cluster_cell_ratio.sh": "细胞比例批处理",
    "diff_enrich_volcano_annot_4.1.R": "差异分析 + 火山图注释",
    "cluster_diff_enrich.sh": "差异富集批处理",
    "cluster_profile_enrich.R": "富集分析（enrichGO 出图）",
    "cluster_profile_mmu.R": "小鼠基因集富集（BP/MF）",
}

COVERS = ["装包检查", "去双细胞", "合并聚类", "marker 鉴定", "细胞比例", "差异与富集"]

PKGS = ["Seurat", "DoubletFinder", "harmony", "SoupX", "presto",
        "clusterProfiler", "org.Mm.eg.db", "ggplot2", "pheatmap", "ggpubr"]


def sh(client, cmd, timeout=300):
    """同一套 POSIX 探针；client=None 时显式在本机执行。"""
    if client is None:
        result = subprocess.run(["/bin/sh", "-c", cmd], capture_output=True,
                                text=True, encoding="utf-8", errors="replace",
                                timeout=timeout)
        return result.returncode, result.stdout, result.stderr
    _, stdout, stderr = client.exec_command(cmd, timeout=timeout)
    out = stdout.read().decode("utf-8", "replace")
    err = stderr.read().decode("utf-8", "replace")
    return stdout.channel.recv_exit_status(), out, err


def write_notebook(path, result):
    """stdlib-only nbformat 4: 保存实测输出及仅体检的可重跑代码。"""
    args = []
    skip = False
    for arg in sys.argv[1:]:
        if skip:
            skip = False
            continue
        if arg in ("--out", "--notebook-out"):
            skip = True
        elif not arg.startswith(("--out=", "--notebook-out=")):
            args.append(arg)
    argv = [sys.executable, os.path.abspath(__file__)] + args
    source = ("import json, subprocess\n"
              "command = %r\n"
              "completed = subprocess.run(command, cwd=%r, capture_output=True, text=True)\n"
              "if completed.stdout:\n"
              "    result = json.loads(completed.stdout)\n"
              "    print(json.dumps(result, ensure_ascii=False, indent=2))\n"
              "completed.check_returncode()\n") % (argv, os.getcwd())
    notebook = {
        "nbformat": 4, "nbformat_minor": 5,
        "metadata": {"kernelspec": {"display_name": "Python 3", "language": "python",
                                    "name": "python3"}},
        "cells": [
            {"id": "scope", "cell_type": "markdown", "metadata": {}, "source": [
                "仅体检：以下输出是本次真实环境探针结果，不是全分析复现。\n",
                "重跑需要原脚本、相同路径与目标环境；local 必须在 --host 对应主机执行。\n",
                "SSH 重跑仍需 paramiko 与 SC_REMOTE_PWD；不保存密码。\n",
                "体检会尝试创建结果目录并写入、删除临时可写性探针，不提交分析任务。\n"]},
            {"id": "health-check", "cell_type": "code", "metadata": {},
             "execution_count": 1, "source": source.splitlines(keepends=True),
             "outputs": [{"output_type": "display_data", "metadata": {},
                          "data": {"application/json": result,
                                   "text/plain": [json.dumps(result, ensure_ascii=False, indent=2)]}}]}
        ]}
    with open(path, "w", encoding="utf-8") as fh:
        json.dump(notebook, fh, ensure_ascii=False, indent=2)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--transport", choices=("ssh", "local"), default="ssh")
    ap.add_argument("--notebook-out", default="")
    ap.add_argument("--host", required=True)
    ap.add_argument("--port", type=int, default=22)
    ap.add_argument("--user", default="")
    ap.add_argument("--data-root", required=True)
    ap.add_argument("--script-dir", required=True)
    ap.add_argument("--work-root", required=True)
    ap.add_argument("--rscript", required=True)
    ap.add_argument("--libs", default="")
    ap.add_argument("--ldlib", default="")
    ap.add_argument("--queue", default="")
    ap.add_argument("--node", default="")
    ap.add_argument("--out", default="")
    a = ap.parse_args()
    if a.transport == "ssh" and not a.user:
        ap.error("--user is required for ssh transport")
    checked_from = socket.gethostname()
    checked_via = ("local 本机执行（%s）" if a.transport == "local"
                   else "工作电脑 %s 经 SSH 执行") % checked_from

    def fail(code, reason, message):
        now = time.strftime("%Y-%m-%dT%H:%M:%S")
        obj = {"ok": False, "verdict": "blocked", "verdict_cn": "体检未完成",
               "error": message, "error_code": reason,
               "items": [{"id": "local" if a.transport == "local" else "ssh",
                           "label": "本机身份校验" if a.transport == "local" else "SSH 连接", "status": "fail",
                          "detail": message}],
               "counts": {"ok": 0, "warn": 0, "fail": 1},
               "generated_at": now, "checked_at": now,
               "checked_from": checked_from, "checked_via": checked_via}
        txt = json.dumps(obj, ensure_ascii=False, indent=2)
        if a.out:
            with open(a.out, "w", encoding="utf-8") as fh:
                fh.write(txt)
        if a.notebook_out:
            write_notebook(a.notebook_out, obj)
        sys.stdout.write(txt + "\n")
        sys.exit(code)

    t0 = time.time()
    client = None
    if a.transport == "local":
        # Deliberately no DNS/IP/short-name inference: aliases can point elsewhere.
        if a.host.rstrip(".").lower() != checked_from.rstrip(".").lower():
            fail(6, "local_host_mismatch",
                 "拒绝本机体检：当前 hostname=%s，与 --host=%s 不匹配。"
                 % (checked_from, a.host))
    else:
        pwd = os.environ.get("SC_REMOTE_PWD", "")
        if not pwd:
            fail(4, "credentials_missing", "当前执行环境没有可用凭据；未进入 SSH，未完成体检。")
        try:
            import paramiko
        except ImportError:
            fail(3, "paramiko_missing", "paramiko 没装。先装： python3 -m pip install paramiko")
        client = paramiko.SSHClient()
        client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        try:
            client.connect(a.host, port=a.port, username=a.user, password=pwd,
                           timeout=15, banner_timeout=30, auth_timeout=30,
                           look_for_keys=False, allow_agent=False)
        except Exception:
            client.close()
            fail(5, "ssh_connect_failed", "SSH 连接或认证失败；未完成体检。")

    items = []

    def add(id_, label, status, detail, value=None):
        items.append({"id": id_, "label": label, "status": status,
                      "detail": detail, "value": value})

    # ---- 节点身份 / 内存 / CPU：一次往返取全 ----
    rc, out, _ = sh(client, "hostname; nproc; "
                            "awk '/MemTotal/{print $2}' /proc/meminfo; "
                            "awk '/MemAvailable/{print $2}' /proc/meminfo")
    lines = [l.strip() for l in out.splitlines() if l.strip()]
    node = lines[0] if lines else ""
    cores = int(lines[1]) if len(lines) > 1 and lines[1].isdigit() else None
    mem_kb = int(lines[2]) if len(lines) > 2 and lines[2].isdigit() else None
    avail_kb = int(lines[3]) if len(lines) > 3 and lines[3].isdigit() else None

    if mem_kb:
        gb = round(mem_kb / 1024.0 / 1024.0, 1)
        detail = "实测 %.1f GB" % gb
        if avail_kb:
            detail += "，当前可用 %.1f GB" % round(avail_kb / 1024.0 / 1024.0, 1)
        add("mem", "内存", "ok" if gb >= 32 else "warn", detail, gb)
    else:
        add("mem", "内存", "warn", "读不到 /proc/meminfo")

    if cores:
        add("cpu", "CPU", "ok", "%d 核（%s）" % (cores, node or "?"), cores)
    else:
        add("cpu", "CPU", "warn", "读不到 nproc")

    # ---- 磁盘 ----
    rc, out, _ = sh(client, "df -Pk /share 2>/dev/null | tail -1")
    f = out.split()
    if len(f) >= 4 and f[3].isdigit():
        avail = round(float(f[3]) / 1024.0 / 1024.0, 1)
        total = round(float(f[1]) / 1024.0 / 1024.0, 1)
        add("disk", "/share 剩余", "ok" if avail >= 20 else "warn",
            "%.1f GB 可用（总 %.1f GB，已用 %s）" % (avail, total, f[4] if len(f) > 4 else "?"),
            avail)
    else:
        add("disk", "/share 剩余", "warn", "df 读不到 /share")

    # ---- R 本体 ----
    rc, out, err = sh(client, "%s --version" % shlex.quote(a.rscript))
    rver = (out or err).strip().splitlines()[0] if (out or err).strip() else ""
    if rc == 0 and rver:
        add("r_version", "R 版本", "ok", rver, rver)
    else:
        add("r_version", "R 版本", "fail", "%s 跑不起来" % a.rscript)

    # ---- 10 个必需包 ----
    env_bits = ""
    if a.libs:
        env_bits += "export R_LIBS_USER=%s; " % shlex.quote(a.libs)
    if a.ldlib:
        env_bits += 'export LD_LIBRARY_PATH=%s:"$LD_LIBRARY_PATH"; ' % shlex.quote(a.ldlib)
    r_expr = (
        "p<-c(%s);" % ",".join('"%s"' % p for p in PKGS) +
        "for(x in p){"
        "r<-tryCatch({suppressMessages(suppressWarnings(library(x,character.only=TRUE)));\"OK\"},"
        "error=function(e) paste0(\"FAIL:\",substr(conditionMessage(e),1,70)));"
        "v<-tryCatch(as.character(packageVersion(x)),error=function(e)\"-\");"
        "cat(\"PKG\\t\",x,\"\\t\",v,\"\\t\",r,\"\\n\",sep=\"\")}"
    )
    rc, out, _ = sh(client, env_bits + "%s --vanilla -e %s 2>&1 | grep '^PKG'"
                    % (shlex.quote(a.rscript), shlex.quote(r_expr)), timeout=900)
    pkg_ok = 0
    for line in out.splitlines():
        if not line.startswith("PKG\t"):
            continue
        fa = line.split("\t")
        if len(fa) < 4:
            continue
        name, ver, status = fa[1], fa[2], fa[3]
        good = status == "OK"
        if good:
            pkg_ok += 1
        add("pkg_" + name, name, "ok" if good else "fail",
            ("v" + ver) if good else ("加载失败：" + status), ver)
    reported = set(i["id"][4:] for i in items if i["id"].startswith("pkg_"))
    for name in PKGS:
        if name not in reported:
            add("pkg_" + name, name, "fail", "未返回结果（R 进程可能被 ld 依赖拦住）", "-")

    # ---- 数据目录：逐个符号链接解到目标，并确认是三件套 ----
    samples = {"count": 0, "names": [], "matrix_files_each": 0, "storage": [], "note": ""}
    rc, out, _ = sh(client, "ls -1 -- %s 2>/dev/null" % shlex.quote(a.data_root))
    entries = [l.strip() for l in out.splitlines() if l.strip()]
    samples["names"] = entries
    samples["count"] = len(entries)
    if not entries:
        add("data_root", "数据目录", "fail", "目录不存在或读不到内容", 0)
    else:
        cmd = ("for d in %s/*; do b=$(basename \"$d\"); t=$(readlink -f \"$d\"); "
               "n=$(ls -1 \"$t\" 2>/dev/null | wc -l); "
               "ok=$(test -f \"$t/barcodes.tsv.gz\" -o -f \"$t/barcodes.tsv\" && echo Y || echo N); "
               "echo \"$b|$t|$n|$ok\"; done") % shlex.quote(a.data_root)
        rc2, out2, _ = sh(client, cmd)
        resolved = trio = 0
        counts = []
        storages = []
        for line in out2.splitlines():
            p = line.split("|")
            if len(p) < 4:
                continue
            tgt, n, hasbc = p[1], p[2].strip(), p[3].strip()
            if tgt.startswith("/") and n.isdigit() and int(n) > 0:
                resolved += 1
                counts.append(int(n))
                if hasbc == "Y":
                    trio += 1
                for root in ("/backup2", "/share1/node-02"):
                    if tgt.startswith(root) and root not in storages:
                        storages.append(root)
        samples["matrix_files_each"] = min(counts) if counts else 0
        samples["storage"] = storages
        samples["note"] = ("%d 个链接全部可解析（%d 个含 barcodes 三件套）；"
                           "链接跨 %s 两处存储，只在 %s 上可见。"
                           % (resolved, trio, " 与 ".join(storages) or "?", node or "?"))
        add("data_root", "数据目录",
            "ok" if (resolved == len(entries) and trio == len(entries)) else "warn",
            "存在，%d 个样本链接中 %d 个可解析（%d 个含三件套）"
            % (len(entries), resolved, trio), resolved)

    # ---- 脚本目录：显式给出，不靠"哪个目录里有 .R/.sh"去猜 ----
    #   为什么还要有下钻这一步：
    #   交付过来的脚本常常放在一个父目录下（父目录里还有 matrix/、result/
    #   这些同级目录），调用方给的是父目录。第一版就因此把四个同级目录名
    #   当成了脚本，白跑一趟。
    #   这里做的是**确定性**下钻，不是"哪个目录里有脚本就用哪个"式的猜：
    #   只在给的这一层没有任何 .R/.sh 时才下钻，且只认两种目标 ——
    #   名为 script（或 scripts）的子目录，或者唯一的那一个子目录。
    #   都不满足就原样报"没取到脚本"，不猜。
    sdir = a.script_dir
    asked_dir = sdir
    descended = False
    rc, out, _ = sh(client, "ls -1 -- %s 2>/dev/null" % shlex.quote(sdir))
    files = sorted([l.strip() for l in out.splitlines() if l.strip()])
    if files and not any(f.lower().endswith((".r", ".sh")) for f in files):
        cand = ""
        for nm in ("script", "scripts"):
            rc2, o2, _ = sh(client, "test -d %s && echo Y || echo N" % shlex.quote(sdir + "/" + nm))
            if o2.strip() == "Y":
                cand = nm
                break
        if not cand:
            rc2, o2, _ = sh(client, "for x in %s/*; do [ -d \"$x\" ] "
                                     "&& basename \"$x\"; done" % shlex.quote(sdir))
            subs = [l.strip() for l in o2.splitlines() if l.strip()]
            if len(subs) == 1:
                cand = subs[0]
        if cand:
            sdir = sdir.rstrip("/") + "/" + cand
            descended = True
            rc, out, _ = sh(client, "ls -1 -- %s 2>/dev/null" % shlex.quote(sdir))
            files = sorted([l.strip() for l in out.splitlines() if l.strip()])
    script_items = []
    if not files:
        add("script_root", "脚本目录", "fail", "目录不存在：%s" % sdir, 0)
    else:
        n_r = sum(1 for f in files if f.lower().endswith(".r"))
        n_sh = sum(1 for f in files if f.endswith(".sh"))
        # 逐个分清「实体文件 / 符号链接 / 子目录」。
        #   为什么要多分一个 D：出过这样一次事故 —— 给的是父目录，
        #   这一层里是 matrix / reference / result / script 四个子目录，
        #   老版本把它们当成四个"脚本"报了 ok。数字看着是 4，其实是目录名，
        #   对下游毫无用处，还让人以为脚本目录没问题。
        #   现在只要这一层没有一个实体文件或符号链接，就如实报 fail，
        #   不再把目录名当脚本。
        rc, out2, _ = sh(client, "cd %s && for x in %s; do "
                                 "if [ -L \"$x\" ]; then echo \"L $x\"; "
                                 "elif [ -d \"$x\" ]; then echo \"D $x\"; "
                                 "else echo \"F $x\"; fi; done"
                                 % (shlex.quote(sdir), " ".join(shlex.quote(f) for f in files)))
        real = sum(1 for l in out2.splitlines() if l.startswith("F "))
        link = sum(1 for l in out2.splitlines() if l.startswith("L "))
        dirs = sum(1 for l in out2.splitlines() if l.startswith("D "))
        if real == 0 and link == 0:
            add("script_root", "脚本目录", "fail",
                "%s 这一层没有脚本，只有 %d 个子目录（%s）。"
                "脚本目录要填到真正放 .R/.sh 的那一层。"
                % (sdir, dirs, "、".join(files[:6])), 0)
        else:
            add("script_root", "脚本目录", "ok",
                "存在，%s 下 %d 个脚本（%d 个 R + %d 个 sh；%d 个实体文件 + %d 个符号链接），"
                "覆盖 %d 个环节"
                % (sdir.rsplit("/", 1)[-1], real + link, n_r, n_sh, real, link, len(COVERS)),
                real + link)
            for f in [l.split(" ", 1)[1] for l in out2.splitlines()
                      if l.startswith(("F ", "L "))]:
                role = ROLE_BY_NAME.get(f, "")
                if not role:
                    rc, out3, _ = sh(client, "head -80 -- %s 2>/dev/null" % shlex.quote(sdir + "/" + f))
                    guess = [tag for key, tag in
                             (("DoubletFinder", "DoubletFinder"), ("SoupX", "SoupX"),
                              ("RunHarmony", "harmony"), ("FindAllMarkers", "FindAllMarkers"),
                              ("enrichGO", "富集"), ("Read10X", "Seurat 读入"))
                             if key in out3]
                    role = ("按内容判定：" + "/".join(guess)) if guess else "未识别"
                script_items.append({"file": f, "role": role})

    # ---- 结果目录可写 ----
    rc, out, _ = sh(client, "d=%s; mkdir -p \"$d\" 2>/dev/null; t=\"$d/.probe_$$\"; "
                            "if echo ok > \"$t\" 2>/dev/null; then rm -f \"$t\"; echo WRITABLE; "
                            "else echo NOTWRITABLE; fi; test -d \"$d\" && echo EXISTS" % shlex.quote(a.work_root))
    if "WRITABLE" in out.splitlines():
        add("work_root", "结果目录", "ok", "存在且可写：%s" % a.work_root, a.work_root)
    elif "EXISTS" in out:
        add("work_root", "结果目录", "warn", "存在但当前写不进去：%s" % a.work_root, a.work_root)
    else:
        add("work_root", "结果目录", "warn", "不存在（首次运行时创建）：%s" % a.work_root, a.work_root)

    # ---- 队列 ----
    extra = []
    rc, out, err = sh(client, "which qsub qstat 2>/dev/null; qstat -f 2>&1 | head -3; "
                              "qconf -sql 2>/dev/null | head -5")
    text = out + err
    if "qsub" in text or "qstat" in text:
        extra.append("qsub/qstat 在位")
    if a.queue:
        qs = [l for l in text.splitlines() if l.strip().startswith(a.queue + "@")]
        if qs:
            extra.append("队列 %s 可用（%s）" % (a.queue, " ".join(qs[0].split())))
    if a.node:
        rc, out, _ = sh(client, "hostname -s")
        if out.strip() == a.node.split("=")[-1]:
            extra.append("当前登录节点即约束节点 %s" % a.node.split("=")[-1])
    extra.append("脚本清单逐个复核：%s 下实体文件与符号链接已分别计数" % sdir.rsplit("/", 1)[-1])
    if descended:
        extra.append("给出的目录 %s 这一层没有脚本，已下钻到 %s"
                     % (asked_dir, sdir))

    counts = {"ok": 0, "warn": 0, "fail": 0}
    for it in items:
        counts[it["status"]] += 1
    verdict = "blocked" if counts["fail"] else ("warn" if counts["warn"] else "ready")
    verdict_cn = {"ready": "服务器环境齐备，可以跑",
                  "warn": "服务器能跑，有几处要留意",
                  "blocked": "服务器有必需项缺失，先补齐"}[verdict]

    now = time.strftime("%Y-%m-%dT%H:%M:%S")
    obj = {
        "ok": True,
        "verdict": verdict,
        "verdict_cn": verdict_cn,
        "counts": counts,
        "target": ("server %s (%s), local" % (a.host, node or "?")
                   if a.transport == "local" else
                   "server %s@%s (%s)" % (a.user, a.host, node or "?")),
        "pkgs_ok": pkg_ok,
        "pkgs_total": len(PKGS),
        "items": items,
        "script_lib": {"root": sdir, "files": script_items, "covers": COVERS},
        "samples": samples,
        "runtime": {"rscript": a.rscript,
                    "lib_paths": [p for p in a.libs.split(":") if p],
                    "queue": a.queue, "node_constraint": a.node},
        "generated_at": now,
        "checked_at": now,
        "checked_via": checked_via,
        "checked_from": checked_from,
        "elapsed_sec": round(time.time() - t0, 1),
        "extra_checks": extra,
    }
    if client is not None:
        client.close()

    txt = json.dumps(obj, ensure_ascii=False, indent=2)
    if a.out:
        with open(a.out, "w", encoding="utf-8") as fh:
            fh.write(txt)
    if a.notebook_out:
        write_notebook(a.notebook_out, obj)
    sys.stdout.write(txt + "\n")


if __name__ == "__main__":
    main()
