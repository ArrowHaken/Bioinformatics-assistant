#!/usr/bin/env python3
"""Import model-extracted pa.intent.v1 JSON; this is not an NLP extractor.

Credentials are read only from an existing private agent_gate.R handoff.
Identity is established by the API's token lookup, never by a caller owner.
"""

import argparse
import http.client
import ipaddress
import json
import os
import re
import stat
import sys
import time
import urllib.error
import urllib.parse
import urllib.request


MAX_BYTES = 1024 * 1024
ATTEMPTS = 3
TIMEOUT = 10
MISSING_KEYS = frozenset(("species", "tissue", "samples.groups", "paths.data_root"))
INTENT_KEYS = frozenset(("schema", "project_name", "mode", "server", "paths",
                         "samples", "reference", "scope", "annotation", "species",
                         "tissue", "missing", "source"))


class ClientError(Exception):
    """Only fixed, non-sensitive error codes may cross the CLI boundary."""


class SafeParser(argparse.ArgumentParser):
    def error(self, message):
        raise ClientError("arguments_invalid") from None


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        return None


def strict_json(data):
    def pairs(items):
        out = {}
        for key, value in items:
            if key in out:
                raise ValueError()
            out[key] = value
        return out

    def invalid_constant(value):
        raise ValueError()

    try:
        return json.loads(data.decode("utf-8"), object_pairs_hook=pairs,
                          parse_constant=invalid_constant)
    except (ValueError, UnicodeError, RecursionError):
        raise ClientError("json_invalid") from None


def read_input(path, private=False):
    fd = None
    try:
        fd = os.open(path, os.O_RDONLY | os.O_NONBLOCK | os.O_NOFOLLOW)
        info = os.fstat(fd)
        if not stat.S_ISREG(info.st_mode) or info.st_size > MAX_BYTES:
            raise ClientError("file_invalid")
        if private and (info.st_uid != os.getuid() or info.st_mode & 0o077):
            raise ClientError("handoff_not_private")
        with os.fdopen(fd, "rb") as stream:
            fd = None
            data = stream.read(MAX_BYTES + 1)
        if len(data) > MAX_BYTES:
            raise ClientError("file_invalid")
        return data
    except OSError:
        raise ClientError("file_unavailable") from None
    finally:
        if fd is not None:
            os.close(fd)


def endpoint(base_url):
    try:
        if not base_url or any(ord(c) <= 32 or ord(c) >= 127 for c in base_url):
            raise ValueError()
        url = urllib.parse.urlsplit(base_url)
        if (url.scheme not in ("https", "http") or not url.hostname
                or url.username is not None or url.password is not None
                or "?" in base_url or "#" in base_url or "\\" in base_url
                or "%" in url.netloc or url.port == 0):
            raise ValueError()
        if url.scheme == "http":
            if url.hostname != "localhost" and not ipaddress.ip_address(url.hostname).is_loopback:
                raise ValueError()
        # Preserve an explicitly supplied application mount prefix, not urljoin semantics.
        if any(segment in (".", "..") for segment in url.path.split("/")):
            raise ValueError()
        return base_url.rstrip("/") + "/api/intent/import"
    except ValueError:
        raise ClientError("base_url_invalid") from None


def load_handoff(path):
    handoff = strict_json(read_input(path, private=True))
    if not isinstance(handoff, dict):
        raise ClientError("handoff_invalid")
    token = handoff.get("token")
    owner = handoff.get("owner_key")
    if (not isinstance(token, str) or not re.fullmatch(r"pa_agent_[0-9a-f]{48}", token)
            or not isinstance(owner, str) or not re.fullmatch(r"(?:viewer|topic):[^\s]+", owner)):
        raise ClientError("handoff_invalid")
    return token


def load_intent(path):
    body = read_input(path)
    intent = strict_json(body)
    if (not isinstance(intent, dict) or intent.get("schema") != "pa.intent.v1"
            or set(intent) - INTENT_KEYS):
        raise ClientError("intent_invalid")
    source = intent.get("source")
    message = source.get("message_id") if isinstance(source, dict) else None
    if (not isinstance(message, str) or not message.strip() or len(message.strip()) > 200
            or any(ord(c) < 32 or ord(c) == 127 for c in message)):
        raise ClientError("message_id_invalid")
    return body


def receipt(data, token):
    result = strict_json(data)
    if not isinstance(result, dict) or result.get("ok") is not True:
        raise ClientError("receipt_invalid")
    sid, duplicate, missing = (result.get(k) for k in ("session_id", "duplicate", "missing"))
    # API-generated session IDs and schema-defined missing keys cannot echo free text.
    if (not isinstance(sid, str) or not re.fullmatch(r"s-\d{14}-[a-z0-9]{4}", sid)
            or type(duplicate) is not bool or not isinstance(missing, list)
            or any(not isinstance(k, str) or k not in MISSING_KEYS for k in missing)):
        raise ClientError("receipt_invalid")
    safe = dict(session_id=sid, duplicate=duplicate, missing=missing, ok=True)
    if token in json.dumps(safe):
        raise ClientError("receipt_invalid")
    return safe


def import_intent(base_url, handoff_path, intent_path):
    url = endpoint(base_url)
    body = load_intent(intent_path)
    token = load_handoff(handoff_path)
    # No environment proxies, browser cookies, netrc, or redirect credential forwarding.
    opener = urllib.request.build_opener(urllib.request.ProxyHandler({}), NoRedirect())
    for attempt in range(ATTEMPTS):
        request = urllib.request.Request(url, data=body, method="POST", headers={
            "Content-Type": "application/json", "Accept": "application/json",
            "X-Agent-Token": token,
        })
        try:
            with opener.open(request, timeout=TIMEOUT) as response:
                if not 200 <= response.status < 300:
                    raise ClientError("http_rejected")
                data = response.read(MAX_BYTES + 1)
                if len(data) > MAX_BYTES:
                    raise ClientError("receipt_invalid")
                return receipt(data, token)
        except urllib.error.HTTPError as error:
            status = error.code
            error.close()
            if not 500 <= status <= 599:
                raise ClientError("http_" + str(status)) from None
            failure = "server_unavailable"
        except (urllib.error.URLError, OSError, http.client.HTTPException):
            failure = "network_unavailable"
        if attempt + 1 < ATTEMPTS:
            time.sleep(0.1 * (2 ** attempt))
    raise ClientError(failure) from None


def main(argv=None):
    try:
        parser = SafeParser(description=__doc__, allow_abbrev=False)
        parser.add_argument("--base-url", required=True)
        parser.add_argument("--handoff", required=True)
        parser.add_argument("--intent-file", required=True)
        args = parser.parse_args(argv)
        result = import_intent(args.base_url, args.handoff, args.intent_file)
        print(json.dumps(result, ensure_ascii=True, separators=(",", ":")))
        return 0
    except ClientError as error:
        print("analysis_agent_client: " + str(error), file=sys.stderr)
        return 1
    except Exception:
        # Library/OS failures may contain URLs, headers, paths or untrusted bodies.
        print("analysis_agent_client: operation_failed", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
