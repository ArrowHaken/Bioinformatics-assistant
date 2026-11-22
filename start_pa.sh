#!/bin/bash
# 启动个性化分析助手。用脚本启动，避免在命令行里出现 serve.R 特征串
# 而被 pkill 误伤（2026-11-21 踩过：pkill -f 把自己的 shell 也杀了）。
set -u
APP=/srv/catsco-agent/work/personal-analysis-app/app
PORT=${1:-19980}
LOG=/tmp/pa-serve.log
export R_HOME=/usr/lib/R
cd "$APP" || exit 1
nohup /usr/lib/R/bin/exec/R --no-echo --no-restore --vanilla \
  --file="$APP/serve.R" --args --port "$PORT" --no-open > "$LOG" 2>&1 &
echo "started pid=$! port=$PORT log=$LOG"
