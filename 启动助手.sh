#!/usr/bin/env bash
# =====================================================================
#  启动助手.sh -- 在 Linux / 远程服务器上启动这个助手
#
#  用法：
#      bash 启动助手.sh                 # 默认端口 18980，尽量自动开浏览器
#      bash 启动助手.sh --port 18980
#      bash 启动助手.sh --no-open       # 服务器上通常用这个（没有图形界面）
#      bash 启动助手.sh --host 0.0.0.0  # 只在明确需要外部访问时才用，见下面警告
#
#  【服务器上怎么用（最常见的情形）】
#    服务器一般没有图形界面，浏览器在您自己电脑上。做法是「SSH 隧道」：
#
#      1) 在服务器上跑：      bash 启动助手.sh --no-open
#      2) 在您自己电脑上开一个终端，建隧道：
#           ssh -N -L 18980:127.0.0.1:18980 用户名@服务器地址
#      3) 然后在自己电脑浏览器打开：  http://127.0.0.1:18980/
#
#    这样网页只走 SSH 加密通道，服务器不需要对外开放任何端口。
#
#  【安全提醒】
#    默认只绑定 127.0.0.1，也就是只有本机能连。
#    不要为了图方便用 --host 0.0.0.0 把它暴露到公网 ——
#    这个服务没有登录、没有权限控制，谁能连上谁就能读写这台机器上的文件。
#    要远程用，请用上面的 SSH 隧道。
#
#  本文件只用 ASCII 写逻辑，中文只出现在提示文本里（避免少数旧系统的编码问题）。
# =====================================================================

set -u

# ---- 找到脚本自己所在的目录（不依赖调用时的工作目录）----
SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SELF_DIR" || exit 1

PORT=18980
HOST="127.0.0.1"
AUTO_OPEN=1

while [ $# -gt 0 ]; do
  case "$1" in
    --port)    PORT="${2:-18980}"; shift 2 ;;
    --host)    HOST="${2:-127.0.0.1}"; shift 2 ;;
    --no-open) AUTO_OPEN=0; shift ;;
    -h|--help)
      sed -n '2,40p' "$SELF_DIR/启动助手.sh" | sed 's/^# \{0,1\}//'
      exit 0 ;;
    *) echo "未知参数: $1"; echo "用 bash 启动助手.sh --help 看用法"; exit 2 ;;
  esac
done

echo ""
echo "==========================================================="
echo "  个性化分析助手"
echo "==========================================================="
echo ""

# ---- 找 R ----
RSCRIPT=""
if [ -n "${RSCRIPT_PATH:-}" ] && [ -x "${RSCRIPT_PATH}" ]; then
  RSCRIPT="${RSCRIPT_PATH}"
elif command -v Rscript >/dev/null 2>&1; then
  RSCRIPT="$(command -v Rscript)"
else
  for c in /usr/bin/Rscript /usr/local/bin/Rscript /opt/R/*/bin/Rscript; do
    if [ -x "$c" ]; then RSCRIPT="$c"; break; fi
  done
fi

if [ -z "$RSCRIPT" ]; then
  echo "  [X] 没有找到 R。"
  echo ""
  echo "      这台机器上需要先装 R："
  echo "        Ubuntu/Debian:  sudo apt-get install -y r-base"
  echo "        CentOS/RHEL:    sudo yum install -y R"
  echo "        conda 环境:     conda install -y r-base"
  echo ""
  echo "      已经装了但不在默认位置的话，指定一下："
  echo "        RSCRIPT_PATH=/你的路径/Rscript bash 启动助手.sh"
  echo ""
  exit 1
fi

echo "  [1/2] R： $RSCRIPT"
"$RSCRIPT" -e 'cat("       版本 ", R.version.string, "\n")' 2>/dev/null

# ---- 检查必需包，缺了先提醒（不自动装，避免在别人机器上擅自改环境）----
echo "  [2/2] 检查依赖…"
MISSING=$("$RSCRIPT" --vanilla -e '
pkgs <- c("Seurat","Matrix","dplyr","ggplot2","patchwork","jsonlite")
miss <- pkgs[!vapply(pkgs, function(p) requireNamespace(p, quietly=TRUE), logical(1))]
if (length(miss)) cat(paste(miss, collapse=", ")) else cat("")
' 2>/dev/null)

if [ -n "$MISSING" ]; then
  echo ""
  echo "  [!] 缺这些 R 包： $MISSING"
  echo "      先装好再跑。可以双击/执行包里的 安装R依赖.R："
  echo "        $RSCRIPT --vanilla \"$SELF_DIR/安装R依赖.R\""
  echo ""
  echo "      或者继续启动（页面上的「环境体检」会再报一次缺什么）。"
  echo ""
fi

# ---- 端口占用检查 ----
if command -v ss >/dev/null 2>&1 && ss -ltn 2>/dev/null | grep -q ":${PORT} "; then
  echo "  [!] 端口 ${PORT} 已被占用。
      换一个：  bash 启动助手.sh --port $((PORT+1))"
  echo ""
  exit 1
fi

# ---- 提示访问方式 ----
if [ "$HOST" != "127.0.0.1" ]; then
  echo ""
  echo "  !! 注意：现在绑定的是 ${HOST}，本机之外也能连上。"
  echo "     这个服务没有登录与权限控制，请确认这台机器在可信网络内。"
  echo "     不需要外部访问的话，去掉 --host 重新启动。"
  echo ""
fi

if [ "$AUTO_OPEN" = "1" ] && [ -z "${DISPLAY:-}" ] && [ ! -d /mnt/c/Windows ]; then
  echo "  （这台机器看起来没有图形界面，所以不会自动开浏览器）"
  echo "  在您自己电脑上建隧道后访问："
  echo "      ssh -N -L ${PORT}:127.0.0.1:${PORT} 用户名@这台机器的地址"
  echo "      浏览器打开  http://127.0.0.1:${PORT}/"
  echo ""
fi

echo ""
echo "  服务地址： http://${HOST}:${PORT}/"
echo "  停止服务： 按 Ctrl+C，或关掉这个终端"
echo "  所有计算都在本机，不上传任何数据。"
echo ""

# ---- 启动 ----
if [ "$AUTO_OPEN" = "1" ]; then
  "$RSCRIPT" --vanilla "$SELF_DIR/serve.R" --port "$PORT"
else
  "$RSCRIPT" --vanilla "$SELF_DIR/serve.R" --port "$PORT" --no-open
fi
