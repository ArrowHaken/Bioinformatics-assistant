#!/usr/bin/env python3
"""Fetch-only bridge. No analysis, shell commands, SSH exec, or worker heartbeat.
Usage: python pa_fetch_client.py --config fetch-config.json
Optional dependency: pip install paramiko
Trust the SSH host first using your normal SSH client; unknown host keys are rejected.
SSH host/user/password and an allowed remote root are entered locally, never exported.
The downloaded config is a secret: store it privately and delete it when replaced.
"""
import argparse
import base64
import contextlib
import getpass
import hashlib
import json
import math
import os
import posixpath
import re
import stat
import sys
import tempfile
import time
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path

CHUNK_BYTES = 384 * 1024
MAX_BYTES = 4 * 1024 ** 3


class SafeError(Exception):
    pass


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        raise SafeError("redirect_rejected")


class API:
    def __init__(self, config):
        self.base = str(config.get("base_url", "")).rstrip("/")
        u = urllib.parse.urlsplit(self.base)
        if (u.scheme != "https" or not u.hostname or u.username or u.password
                or u.query or u.fragment or "\\" in self.base
                or any(ord(c) < 33 for c in self.base)):
            raise SafeError("invalid_base_url")
        self.token = config.get("fetch_token", "")
        if not re.fullmatch(r"pa_fetch_[0-9a-f]{64}", self.token):
            raise SafeError("invalid_fetch_config")
        self.opener = urllib.request.build_opener(urllib.request.ProxyHandler({}), NoRedirect())

    def call(self, route, body=None):
        if route not in ("pending", "put", "put-chunk", "fail"):
            raise SafeError("route_denied")
        data = None if body is None else json.dumps(body, separators=(",", ":")).encode()
        request = urllib.request.Request(self.base + "/api/mirror/" + route, data=data,
                                        headers={"X-Fetch-Token": self.token,
                                                 "Content-Type": "application/json"})
        try:
            with self.opener.open(request, timeout=60) as response:
                raw = response.read(8 * 1024 * 1024 + 1)
                if len(raw) > 8 * 1024 * 1024:
                    raise SafeError("response_too_large")
                result = json.loads(raw)
        except urllib.error.HTTPError as exc:
            # Never print URLs, request bodies, headers, or remote exception text.
            raise SafeError("authorization_denied" if exc.code in (401, 403) else "upload_rejected") from None
        except (OSError, ValueError):
            raise SafeError("network_or_response_error") from None
        if not isinstance(result, dict) or not result.get("ok"):
            raise SafeError("api_rejected")
        return result


def inside(root, path):
    return path == root or path.startswith(root.rstrip("/") + "/")


def remote_path(sftp, root, item):
    rel = item.get("rel", "")
    run = item.get("run_dir", "")
    if (not isinstance(rel, str) or not re.fullmatch(r"[A-Za-z0-9._/-]+", rel)
            or any(p in ("", ".", "..") for p in rel.split("/"))
            or rel.startswith("/") or not isinstance(run, str)
            or not run.startswith("/") or "\\" in run or "\x00" in run
            or ".." in run.split("/")):
        raise SafeError("unsafe_path")
    target = posixpath.join(run, rel)
    canonical_root = sftp.normalize(root).rstrip("/") or "/"
    canonical_run = sftp.normalize(run)
    canonical_target = sftp.normalize(target)
    if not inside(canonical_root, canonical_run) or not inside(canonical_run, canonical_target):
        raise SafeError("path_outside_authorized_root")
    # Reject symlinks at every component, not just the leaf.
    current = "/"
    for part in target.split("/"):
        if not part:
            continue
        current = posixpath.join(current, part)
        if stat.S_ISLNK(sftp.lstat(current).st_mode):
            raise SafeError("symlink_rejected")
    if not stat.S_ISREG(sftp.lstat(target).st_mode):
        raise SafeError("not_regular_file")
    return target


def binding(item):
    sid, rel = item.get("session"), item.get("rel")
    size, md5 = item.get("bytes"), item.get("want_md5")
    if (not isinstance(sid, str) or not sid or not isinstance(rel, str)
            or isinstance(size, bool) or not isinstance(size, (int, float))
            or not math.isfinite(size) or size != int(size) or not 0 < size <= MAX_BYTES
            or not isinstance(md5, str) or not re.fullmatch(r"[0-9a-fA-F]{32}", md5)):
        raise SafeError("invalid_binding")
    return {"session_id": sid, "rel": rel, "bytes": int(size), "md5": md5.lower()}


def transfer(api, sftp, root, item):
    fields = binding(item)
    path = remote_path(sftp, root, item)
    expected = fields["bytes"]
    if sftp.stat(path).st_size != expected:
        raise SafeError("size_mismatch")
    # Spool before upload so a changing source cannot mix two file versions.
    # This is on the client, never on the application server.
    with tempfile.TemporaryFile() as spool, sftp.open(path, "rb") as source:
        digest = hashlib.md5()
        total = 0
        while True:
            data = source.read(min(CHUNK_BYTES, expected - total + 1))
            if not data:
                break
            total += len(data)
            if total > expected:
                raise SafeError("size_mismatch")
            digest.update(data)
            spool.write(data)
        if total != expected:
            raise SafeError("size_mismatch")
        if digest.hexdigest() != fields["md5"]:
            raise SafeError("md5_mismatch")
        spool.seek(0)
        chunks = (expected + CHUNK_BYTES - 1) // CHUNK_BYTES
        for idx in range(chunks):
            body = dict(fields, b64=base64.b64encode(spool.read(CHUNK_BYTES)).decode("ascii"))
            route = "put"
            if chunks > 1:
                route = "put-chunk"
                body.update(chunk_index=idx, chunks=chunks)
            result = api.call(route, body)
            if chunks > 1 and idx == chunks - 1 and result.get("complete") is not True:
                raise SafeError("upload_incomplete")


@contextlib.contextmanager
def single_instance(path):
    try:
        os.mkdir(path, 0o700)
    except FileExistsError:
        raise SafeError("client_already_running_or_stale_lock") from None
    try:
        yield
    finally:
        os.rmdir(path)


def run(config, once=False):
    try:
        import paramiko
    except ImportError:
        raise SafeError("install_optional_paramiko_dependency") from None
    api = API(config)
    host = input("SSH host (local only): ").strip()
    port = int(input("SSH port [22]: ").strip() or "22")
    user = input("SSH user (local only): ").strip()
    root = input("Authorized absolute remote root: ").strip()
    if not root.startswith("/") or ".." in root.split("/") or "\\" in root:
        raise SafeError("invalid_authorized_root")
    password = getpass.getpass("SSH password (empty for SSH key): ") or None
    with paramiko.SSHClient() as ssh:
        ssh.load_system_host_keys()
        ssh.set_missing_host_key_policy(paramiko.RejectPolicy())
        ssh.connect(host, port=port, username=user, password=password, timeout=20,
                    auth_timeout=20, banner_timeout=20)
        password = None
        with ssh.open_sftp() as sftp:
            while True:
                pending = api.call("pending").get("items", [])
                if not isinstance(pending, list):
                    raise SafeError("invalid_queue")
                for item in pending:
                    try:
                        transfer(api, sftp, root, item)
                        print("Artifact verified and uploaded.")
                    except SafeError as exc:
                        if str(exc) == "authorization_denied":
                            raise
                        print("Artifact not uploaded; task retained.")
                    except (OSError, ValueError, TypeError, KeyError):
                        print("Artifact not uploaded; task retained.")
                if once:
                    return
                time.sleep(15)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--config", required=True)
    parser.add_argument("--once", action="store_true")
    args = parser.parse_args()
    try:
        path = Path(args.config)
        if os.name == "posix" and path.stat().st_mode & 0o077:
            raise SafeError("config_permissions_require_chmod_600")
        with path.open(encoding="utf-8") as handle:
            config = json.load(handle)
        with single_instance(str(Path.home() / ".pa-fetch-client.lock")):
            run(config, args.once)
    except KeyboardInterrupt:
        return 0
    except SafeError as exc:
        print("Fetch client stopped: " + str(exc), file=sys.stderr)
        return 1
    except Exception:
        print("Fetch client stopped; check local SSH settings, host trust, and connectivity.", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
