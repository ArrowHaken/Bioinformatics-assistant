#!/usr/bin/env python3
"""自然输入 → 分析记录 → 派活：一条命令走完这条路。

它只做三件事：
  1. 把已经核实好的参数拼成 pa.intent.v1；
  2. 用当前用户已有的 handoff 凭据 POST /api/intent/import（首次只建记录）；
  3. 明确给了 step 时，再用 /api/agent/dispatch 派活。

三件事刻意分开成两个请求，不是一步 auto_dispatch：
  建记录与派活是两种后果。合并之后一旦派活被闸门拒绝，
  调用方拿到的是一句混合错误，分不清「记录没建」还是「记录建了但没派出去」。
  分开做，每一步都能单独核对，重试也只重试失败的那一步。

不做的事（这些不是遗漏，是有意不猜）：
  · 不签发、不续期授权。只读已经存在的 handoff 文件，且必须属于当前这个人。
  · 不从路径名猜物种与组织。
  · 不把「跑到 08 之前」这类笔记本编号翻译成执行阶段。编号到阶段的对应关系
    源码里没有定义，翻译等于编造；这一项留给人在参数里明说阶段 id。
  · 不绕到 SSH 自己跑分析。派活只进应用自己的执行队列。

用法：
  pa_natural_dispatch.py --intent-file body.json --handoff /path/to/handoff.json
  加 --dry-run 只校验并打印将要发送的请求体，不发任何请求。
  加 --step doublet --req-id req-20261118-001 才派活。
"""

import argparse
import json
import os
import re
import stat
import sys
import time
import urllib.error
import urllib.parse
import urllib.request

SCHEMA = "pa.intent.v1"
ALLOWED = {
    "schema", "project_name", "mode", "server", "paths", "samples",
    "reference", "scope", "annotation", "species", "tissue", "missing", "source",
}
STAGE_ID = re.compile(r"^[a-z][a-z0-9_]{0,31}$")
REQ_ID = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$")
TOKEN = re.compile(r"^pa_agent_[0-9a-f]{48}$")
MAX_BYTES = 1024 * 1024


class Failure(Exception):
    """只允许固定错误码跨出命令行边界，避免把路径和凭据带进日志。"""


def read_private(path):
    fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW)
    try:
        info = os.fstat(fd)
        if not stat.S_ISREG(info.st_mode) or info.st_size > MAX_BYTES:
            raise Failure("handoff_invalid")
        if info.st_mode & 0o077:
            raise Failure("handoff_not_private")
        with os.fdopen(fd, "rb") as fh:
            fd = None
            return fh.read(MAX_BYTES + 1)
    finally:
        if fd is not None:
            os.close(fd)


def load_handoff(path):
    try:
        data = json.loads(read_private(path).decode("utf-8"))
    except (ValueError, UnicodeError):
        raise Failure("handoff_invalid")
    token = data.get("token") if isinstance(data, dict) else None
    owner = data.get("owner_key") if isinstance(data, dict) else None
    if not isinstance(token, str) or not TOKEN.match(token):
        raise Failure("handoff_invalid")
    if not isinstance(owner, str) or not re.match(r"^(viewer|topic):\S+$", owner):
        raise Failure("handoff_invalid")
    return token, owner


def load_intent(path):
    with open(path, "rb") as fh:
        raw = fh.read(MAX_BYTES + 1)
    if len(raw) > MAX_BYTES:
        raise Failure("intent_too_large")
    try:
        body = json.loads(raw.decode("utf-8"))
    except (ValueError, UnicodeError):
        raise Failure("intent_invalid")
    validate(body)
    return body


def validate(body):
    if not isinstance(body, dict) or body.get("schema") != SCHEMA:
        raise Failure("intent_schema")
    extra = set(body) - ALLOWED
    if extra:
        raise Failure("intent_unknown_field:" + ",".join(sorted(extra)))
    if not body.get("project_name"):
        raise Failure("intent_project_name_required")
    source = body.get("source")
    if not isinstance(source, dict) or not source.get("message_id"):
        raise Failure("intent_message_id_required")
    paths = body.get("paths") or {}
    if not paths.get("work_root"):
        raise Failure("intent_work_root_required")
    if body.get("mode", "server") == "server":
        server = body.get("server") or {}
        if not server.get("host"):
            raise Failure("intent_server_host_required")
    scope = body.get("scope") or {}
    bound = scope.get("run_until")
    if bound is not None and not (isinstance(bound, str) and STAGE_ID.match(bound)):
        # 笔记本编号（08）、中文短语（08之前）都走到这里：不翻译，直接拒绝。
        raise Failure("intent_scope_not_a_stage_id")
    gate = scope.get("annotation_gate")
    if gate is not None and not isinstance(gate, bool):
        raise Failure("intent_annotation_gate_not_boolean")


def endpoint(base, path):
    if not base or any(ord(c) <= 32 or ord(c) >= 127 for c in base):
        raise Failure("base_url_invalid")
    url = urllib.parse.urlsplit(base)
    if url.scheme not in ("http", "https") or not url.hostname:
        raise Failure("base_url_invalid")
    if any(seg in (".", "..") for seg in url.path.split("/")):
        raise Failure("base_url_invalid")
    return base.rstrip("/") + path


def post(base, path, token, payload, timeout=15):
    data = json.dumps(payload, ensure_ascii=False).encode("utf-8")
    req = urllib.request.Request(
        endpoint(base, path), data=data, method="POST",
        headers={"Content-Type": "application/json; charset=utf-8",
                 "Accept": "application/json", "X-Agent-Token": token})
    opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
    try:
        with opener.open(req, timeout=timeout) as resp:
            return resp.status, json.loads(resp.read(MAX_BYTES + 1).decode("utf-8"))
    except urllib.error.HTTPError as err:
        status = err.code
        try:
            body = json.loads(err.read(MAX_BYTES + 1).decode("utf-8"))
        except Exception:
            body = {}
        err.close()
        return status, body
    except (urllib.error.URLError, OSError):
        raise Failure("network_unavailable")


def summary(body):
    """只回可核对的字段，不回原始报文。"""
    keys = ("ok", "code", "session_id", "duplicate", "missing", "record_created",
            "queued", "step", "req_id", "exec_id", "dispatch_status", "error", "retryable")
    out = {k: body[k] for k in keys if k in body}
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(add_help=True)
    ap.add_argument("--intent-file", required=True)
    ap.add_argument("--handoff", required=True)
    ap.add_argument("--base-url", default="http://127.0.0.1:19980")
    ap.add_argument("--step", default="")
    ap.add_argument("--req-id", default="")
    ap.add_argument("--session-id", default="", help="派活目标；建记录后可省略并在第二次调用时给出")
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args(argv)

    try:
        intent = load_intent(args.intent_file)
        token, owner = load_handoff(args.handoff)
        if bool(args.step) != bool(args.req_id):
            raise Failure("dispatch_requires_step_and_req_id")
        if args.step and not STAGE_ID.match(args.step):
            raise Failure("dispatch_step_not_a_stage_id")
        if args.req_id and not REQ_ID.match(args.req_id):
            raise Failure("dispatch_req_id_invalid")
        if args.step and not args.session_id:
            raise Failure("dispatch_requires_session_id")

        if args.dry_run:
            print(json.dumps({"dry_run": True, "owner_key": owner,
                              "import": intent,
                              "dispatch": ({"session_id": args.session_id,
                                            "step": args.step,
                                            "req_id": args.req_id}
                                           if args.step else None)},
                             ensure_ascii=False, indent=2))
            return 0

        status, body = post(args.base_url, "/api/intent/import", token, intent)
        print("import:", json.dumps(summary(body), ensure_ascii=False))
        if status != 200 or not body.get("ok"):
            return 1
        sid = args.session_id or body.get("session_id") or ""
        if not args.step:
            print("record_only:", sid)
            return 0

        dispatch = {"session_id": sid, "step": args.step, "req_id": args.req_id}
        status, body = post(args.base_url, "/api/agent/dispatch", token, dispatch)
        print("dispatch:", json.dumps(summary(body), ensure_ascii=False))
        return 0 if status == 200 and body.get("ok") else 1
    except Failure as exc:
        print("pa_natural_dispatch:", exc, file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
