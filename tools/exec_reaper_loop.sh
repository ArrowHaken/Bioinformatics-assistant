#!/bin/sh
# Optional sidecar entry: one reaper run per minute, no in-process service.
# Prefer a systemd timer when available; this loop is for hosts without one.
set -eu
APP_DIR="${PA_APP_DIR:-$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)}"
INTERVAL="${PA_EXEC_REAPER_INTERVAL:-60}"
while :; do
  PA_APP_DIR="$APP_DIR" Rscript "$APP_DIR/engine/exec_reaper.R" || true
  sleep "$INTERVAL"
done
