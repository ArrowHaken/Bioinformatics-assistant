"""Real loopback HTTP fixtures only; never starts or contacts the application."""
import contextlib
import importlib.util
import json
import os
from pathlib import Path
import socket
import subprocess
import sys
import tempfile
import threading
import unittest
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

CLIENT = Path(__file__).resolve().parents[1] / "scripts" / "analysis_agent_client.py"
SPEC = importlib.util.spec_from_file_location("analysis_agent_client", CLIENT)
client = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(client)
TOKEN = "pa_agent_" + "a" * 48
RAW = "sensitive original user text"
GOOD = {"ok": True, "session_id": "s-20261109080000-ab12", "duplicate": False,
        "missing": ["species", "tissue"]}


@contextlib.contextmanager
def fixture(replies):
    requests = []

    class Handler(BaseHTTPRequestHandler):
        def log_message(self, *args):
            pass

        def do_POST(self):
            body = self.rfile.read(int(self.headers["Content-Length"]))
            requests.append((self.path, dict(self.headers), body))
            reply = replies[min(len(requests) - 1, len(replies) - 1)]
            if reply is None:
                self.connection.shutdown(socket.SHUT_RDWR)
                self.connection.close()
                return
            status, payload, headers = reply
            self.send_response(status)
            for key, value in headers.items():
                self.send_header(key, value)
            self.end_headers()
            self.wfile.write(payload if isinstance(payload, bytes) else json.dumps(payload).encode())

    server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield "http://127.0.0.1:" + str(server.server_port), requests
    finally:
        server.shutdown()
        server.server_close()
        thread.join()


class ClientTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="agent-client-")
        self.addCleanup(self.tmp.cleanup)
        self.handoff = Path(self.tmp.name) / "handoff.json"
        self.intent = Path(self.tmp.name) / "intent.json"
        self.handoff.write_text(json.dumps({"token": TOKEN, "owner_key": "viewer:fixture",
                                         "token_id": "fixture", "expires_at": "2099-01-01T00:00:00"}))
        self.handoff.chmod(0o600)
        self.payload = {"schema": "pa.intent.v1", "project_name": "fixture", "mode": "local",
                        "paths": {"work_root": "/fixture"},
                        "source": {"message_id": "fixture-message", "raw_text": RAW}}
        self.save()

    def save(self):
        self.intent.write_text(json.dumps(self.payload, indent=2))

    def run_cli(self, url, extra=()):
        before = (self.handoff.read_bytes(), self.intent.read_bytes())
        result = subprocess.run([sys.executable, "-B", str(CLIENT), "--base-url", url,
                                 "--handoff", str(self.handoff), "--intent-file", str(self.intent),
                                 *extra], text=True, capture_output=True, timeout=8)
        self.assertEqual(before, (self.handoff.read_bytes(), self.intent.read_bytes()))
        for secret in (TOKEN, RAW, "viewer:fixture"):
            self.assertNotIn(secret, result.stdout + result.stderr)
        return result

    def test_success_allowlist_mount_and_private_token(self):
        with fixture([(200, dict(GOOD, raw_text=RAW, token=TOKEN), {})]) as (url, requests):
            result = self.run_cli(url + "/mount")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout), GOOD)
        self.assertEqual(result.stderr, "")
        path, headers, body = requests[0]
        self.assertEqual(path, "/mount/api/intent/import")
        self.assertEqual(headers["X-Agent-Token"], TOKEN)
        self.assertNotIn("owner", " ".join(headers).lower())
        self.assertEqual(body, self.intent.read_bytes())

    def test_5xx_retry_identical_body_and_rerun_duplicate(self):
        duplicate = dict(GOOD, duplicate=True)
        with fixture([(503, {"token": TOKEN}, {}), (200, duplicate, {})]) as (url, requests):
            first = self.run_cli(url)
            second = self.run_cli(url)
        self.assertEqual(first.returncode, 0, first.stderr)
        self.assertEqual(json.loads(first.stdout), duplicate)
        self.assertEqual(json.loads(second.stdout), duplicate)
        self.assertEqual(len(requests), 3)
        self.assertTrue(all(row[2] == self.intent.read_bytes() for row in requests))

    def test_disconnect_retries_real_http(self):
        with fixture([None, (200, GOOD, {})]) as (url, requests):
            result = self.run_cli(url)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(len(requests), 2)
        self.assertEqual(requests[0][2], requests[1][2])

    def test_retry_limit(self):
        with fixture([(502, (TOKEN + RAW).encode(), {})]) as (url, requests):
            result = self.run_cli(url)
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stdout, "")
        self.assertEqual(len(requests), 3)

    def test_foreign_redirect_never_receives_credentials(self):
        with fixture([(200, GOOD, {})]) as (foreign, foreign_requests):
            for status in (301, 302, 303, 307, 308):
                with self.subTest(status=status):
                    with fixture([(status, TOKEN.encode(), {"Location": foreign + "/steal"})]) as (url, requests):
                        result = self.run_cli(url)
                    self.assertEqual(result.returncode, 1)
                    self.assertEqual(len(requests), 1)
            self.assertEqual(foreign_requests, [])

    def test_4xx_never_retries(self):
        for status in (400, 401, 403, 429):
            with self.subTest(status=status):
                with fixture([(status, (TOKEN + RAW).encode(), {})]) as (url, requests):
                    result = self.run_cli(url)
                self.assertEqual(result.returncode, 1)
                self.assertEqual(len(requests), 1)
                self.assertEqual(result.stdout, "")

    def test_malformed_response_json_and_receipt_injection(self):
        for payload in (b"not json " + TOKEN.encode(), b"[]", b'{"ok":true,"ok":true}',
                        dict(GOOD, session_id=TOKEN), dict(GOOD, missing=[RAW]),
                        dict(GOOD, duplicate=1), dict(GOOD, ok=False)):
            with self.subTest(payload_type=type(payload).__name__):
                with fixture([(200, payload, {})]) as (url, requests):
                    result = self.run_cli(url)
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stdout, "")
                self.assertEqual(len(requests), 1)

    def test_input_validation_no_http(self):
        invalid = [b"not json", b'{"schema":"pa.intent.v1","schema":"pa.intent.v1"}',
                   json.dumps(dict(self.payload, schema="other")).encode(),
                   json.dumps(dict(self.payload, source={})).encode(),
                   json.dumps(dict(self.payload, owner_key="viewer:forged")).encode()]
        with fixture([(200, GOOD, {})]) as (url, requests):
            for body in invalid:
                self.intent.write_bytes(body)
                self.assertEqual(self.run_cli(url).returncode, 1)
            self.assertEqual(requests, [])

    def test_handoff_privacy_and_cli_owner_rejected(self):
        with fixture([(200, GOOD, {})]) as (url, requests):
            self.handoff.chmod(0o644)
            self.assertEqual(self.run_cli(url).returncode, 1)
            self.handoff.chmod(0o600)
            self.assertEqual(self.run_cli(url, ("--owner", TOKEN)).returncode, 1)
            self.handoff.write_text('{"token":"' + TOKEN + '"')
            self.assertEqual(self.run_cli(url).returncode, 1)
            self.assertEqual(requests, [])

    def test_base_url_restrictions(self):
        for url in ("http://example.com", "https://user:secret@example.com",
                    "https://example.com?", "https://example.com#", "ftp://127.0.0.1",
                    "http://127.0.0.1:0", "https://example.com/../other", "https://exa mple.com",
                    "https://example.com\\@evil.com", "http://127.0.0.1.evil.com"):
            with self.subTest(url=url):
                with self.assertRaises(client.ClientError):
                    client.endpoint(url)
        for url in ("https://example.com", "http://127.0.0.1:8123", "http://[::1]:8123", "http://localhost:8123"):
            self.assertTrue(client.endpoint(url).endswith("/api/intent/import"))

    def test_public_exception_contains_no_credentials(self):
        with fixture([(403, (TOKEN + RAW).encode(), {})]) as (url, requests):
            with self.assertRaises(client.ClientError) as caught:
                client.import_intent(url, self.handoff, self.intent)
        self.assertEqual(str(caught.exception), "http_403")
        self.assertIsNone(caught.exception.__cause__)


if __name__ == "__main__":
    unittest.main()
