#!/usr/bin/env bash
# =====================================================================
#  启动助手.sh -- 在 Linux / 远程服务器上启动这个助手
#
#  用法：
#      bash 启动助手.sh                 # 默认端口 18980，尽量自动开浏览器
#      bash 启动助手.sh --port 18980
#      bash 启动助手.sh --no-open       # 服务器上通常用这个（没有图形界面）
#
#  【服务器上怎么用（最常见的情形）】
#    服务器一般没有图形界面，浏览器在您自己电脑上。做法是「SSH 隧道」：
#
#      1) 在服务器上跑：      bash 启动助手.sh --no-open
#      2) 在您自己电脑上开一个终端，建隧道：
#           ssh -N -L 18980:127.0.0.1:18980 用户名@服务器地址
#      3) 然后在自己电脑浏览器打开：  http://127.0.0.1:18980/
#
#    隧道仍然可用，但它不是唯一入口。
#
#  【安全提醒：端口会监听 0.0.0.0】
#    R 的 serverSocket(port) 没有地址参数，端口一开就绑在所有网卡上，
#    脚本无法改成只绑 127.0.0.1。事实是：同网段其他机器也能连上。
#    这个服务的身份靠平台网关传入，不经网关直连时读不到任何记录（会返回 403），
#    但页面本身仍然可访问。只给自己用的话，用防火墙只放行本机：
#      sudo iptables -I INPUT ! -i lo -p tcp --dport 18980 -j DROP
#    撤销：
#      sudo iptables -D INPUT ! -i lo -p tcp --dport 18980 -j DROP
#
#  本文件只用 ASCII 写逻辑，中文只出现在提示文本里（避免少数旧系统的编码问题）。
# =====================================================================

set -u

# ---- 找到脚本自己所在的目录（不依赖调用时的工作目录）----
SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SELF_DIR" || exit 1

PORT=18980
AUTO_OPEN=1

while [ $# -gt 0 ]; do
  case "$1" in
    --port)    PORT="${2:-18980}"; shift 2 ;;
    --host)
      echo "  --host 不可用：R 的 serverSocket(port) 没有地址参数，"
      echo "  端口一开就监听在 0.0.0.0，脚本改不了这件事。"
      echo "  要限制来源，用防火墙只放行本机，例如："
      echo "    sudo iptables -I INPUT ! -i lo -p tcp --dport ${PORT:-18980} -j DROP"
      exit 2 ;;
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
# 这里不说「只绑本机」。R 的 serverSocket(port) 没有地址参数，
#   端口一开就是 0.0.0.0，同网段都能连。脚本改不动这件事，只能如实说，
#   并把限制办法给出来，而不是印一句做不到的承诺。
echo ""
echo "  !! 这个端口会监听 0.0.0.0，同网段其他机器也能连上。"
echo "     服务本身没有登录与账号体系，身份靠平台网关传入。"
echo "     只给自己用的话，用防火墙只放行本机："
echo "       sudo iptables -I INPUT ! -i lo -p tcp --dport ${PORT} -j DROP"
echo "     撤销："
echo "       sudo iptables -D INPUT ! -i lo -p tcp --dport ${PORT} -j DROP"
echo ""

if [ "$AUTO_OPEN" = "1" ] && [ -z "${DISPLAY:-}" ] && [ ! -d /mnt/c/Windows ]; then
  echo "  （这台机器看起来没有图形界面，所以不会自动开浏览器）"
  echo "  在您自己电脑上建隧道后访问："
  echo "      ssh -N -L ${PORT}:127.0.0.1:${PORT} 用户名@这台机器的地址"
  echo "      浏览器打开  http://127.0.0.1:${PORT}/"
  echo ""
fi

echo ""
echo "  服务地址： http://<本机地址>:${PORT}/"
echo "  端口监听： 0.0.0.0:${PORT}（同网段可连，限制方法见上面的防火墙命令）"
echo "  停止服务： 按 Ctrl+C，或关掉这个终端"
echo "  所有计算都在本机，不上传任何数据。"
echo ""

# ---- 启动 ----
if [ "$AUTO_OPEN" = "1" ]; then
  "$RSCRIPT" --vanilla "$SELF_DIR/serve.R" --port "$PORT"
else
  "$RSCRIPT" --vanilla "$SELF_DIR/serve.R" --port "$PORT" --no-open
fi
