#!/usr/bin/env bash
# 从 engine/ 重建 engine_pack.tar.xz —— 唯一权威来源是仓库里的 engine/ 目录。
#
# 为什么要有这个脚本（2026-11-14 figs 首任务失败）：
#   集群 _engine 靠整包下发，而包一直是手工打的。这次 plot_all_v2.R 已更新、
#   它依赖的 lib/diff_skip.R 是新增文件，手工打包漏了它，集群上脚本一启动就拒绝：
#     拒绝运行：找不到 lib/diff_skip.R（差异跳过记账依赖它）
#   结果是 exit_status=1、0 张图，看起来像绘图脚本坏了。
#
# 2026-11-14 第二轮加固（gina 指出）：
#   1) 不再直接写正式包：先在同目录写临时包，校验通过后原子 rename。
#      否则别的 worker 可能在 tar 写到一半时读到半包。
#   2) 关键文件缺失一律 exit 非零，并保留原正式包（不再只 echo）。
#   3) 校验分三步：tar 可读、关键文件齐全、包内每个关键文件的 sha256 与
#      engine/ 源文件一致。只核"在不在包里"证明不了内容对。
#
# 用法：
#   bash tools/build_engine_pack.sh            # 正式构建
#   PA_APP_DIR=/tmp/x bash tools/build_engine_pack.sh   # 隔离目录自测
# 退出码：0 成功；1 环境不对；2 依赖缺失；3 打包/校验失败。
set -euo pipefail

APP_DIR="${PA_APP_DIR:-$(cd "$(dirname "$0")/.." && pwd)}"
SRC="$APP_DIR/engine"
OUT_DIR="$APP_DIR/state/exports"
OUT="$OUT_DIR/engine_pack.tar.xz"

[ -d "$SRC" ] || { echo "FAIL 缺少 engine/ 目录：$SRC" >&2; exit 1; }
mkdir -p "$OUT_DIR"

# ---- 1) 依赖自检：脚本引用的 lib/*.R 必须真的存在 ----
# 逐个脚本目录单独核对：每个目录的 lib 引用都相对它自己。
#   不能把所有目录的引用统一套一个前缀 —— sc-pipeline 的 lib/ref_table.R
#   就在它自己的 scripts/lib 下，套成 sc-plot-kit 会误报缺失。
missing=0
for SDIR in "$SRC/sc-plot-kit/scripts" "$SRC/sc-pipeline/scripts"; do
  [ -d "$SDIR" ] || continue
  while IFS= read -r need; do
    [ -z "$need" ] && continue
    if [ ! -f "$SDIR/$need" ]; then
      echo "FAIL MISSING_DEP ${SDIR#$APP_DIR/}/$need" >&2
      missing=1
    fi
  done < <(grep -rhoE 'lib/[A-Za-z0-9_.-]+\.R' "$SDIR" 2>/dev/null | sort -u)
done
if [ "$missing" -ne 0 ]; then
  echo "FAIL 依赖自检未通过，正式包保持原样" >&2
  exit 2
fi

# ---- 2) 打包到同目录临时文件 ----
TMP="$(mktemp "$OUT_DIR/.engine_pack.XXXXXX.tar.xz")"
# 注意：trap 里最后一条语句的状态会成为脚本退出码。
#   这里必须显式 return 0，否则成功构建也会被 reported 成 exit 1（已实测踩到）。
cleanup() {
  if [ -n "${TMP:-}" ] && [ -f "$TMP" ]; then rm -f "$TMP"; fi
  return 0
}
trap cleanup EXIT
if ! tar -cJf "$TMP" -C "$APP_DIR" engine; then
  echo "FAIL 打包失败，正式包保持原样" >&2
  exit 3
fi

# ---- 3) 校验：可读 + 关键文件齐全 + 内容 hash 与源一致 ----
if ! tar -tJf "$TMP" >/dev/null 2>&1; then
  echo "FAIL 临时包不可读，正式包保持原样" >&2
  exit 3
fi
LIST="$(mktemp)"
trap 'cleanup; rm -f "$LIST"' EXIT
tar -tJf "$TMP" > "$LIST"

KEY_FILES=(
  engine/sc-plot-kit/scripts/plot_all_v2.R
  engine/sc-plot-kit/scripts/lib/diff_skip.R
  engine/sc-pipeline/scripts/02_annotate.R
  engine/sc-ppt-kit/scripts/make_ppt.py
  engine/verdict.R
  engine/annotation_review.R
)
bad=0
for f in "${KEY_FILES[@]}"; do
  if ! grep -qx "$f" "$LIST"; then
    echo "FAIL MISSING_IN_PACK $f" >&2
    bad=1
  fi
done
[ "$bad" -ne 0 ] && { echo "FAIL 关键文件不全，正式包保持原样" >&2; exit 3; }

XDIR="$(mktemp -d)"
trap 'cleanup; rm -f "$LIST"; rm -rf "$XDIR"' EXIT
for f in "${KEY_FILES[@]}"; do
  tar -xJf "$TMP" -C "$XDIR" "$f" 2>/dev/null || { echo "FAIL EXTRACT $f" >&2; exit 3; }
  got="$(sha256sum "$XDIR/$f" | cut -d' ' -f1)"
  want="$(sha256sum "$APP_DIR/$f" | cut -d' ' -f1)"
  if [ "$got" != "$want" ]; then
    echo "FAIL HASH_MISMATCH $f pack=$got src=$want" >&2
    exit 3
  fi
done

# ---- 4) 原子发布 ----
[ -f "$OUT" ] && cp -p "$OUT" "$OUT.bak-build-$(date +%Y%m%d-%H%M%S)"
mv -f "$TMP" "$OUT"

echo "pack=$OUT"
echo "pack_md5=$(md5sum "$OUT" | cut -d' ' -f1)"
echo "pack_bytes=$(stat -c %s "$OUT")"
echo "--- 关键依赖（包内存在且 hash 与 engine/ 一致）---"
for f in "${KEY_FILES[@]}"; do echo "OK   $f"; done
echo "entries=$(wc -l < "$LIST")"
echo "NOTE 本脚本只保证包内容正确；集群是否真的递归安装了 scripts/lib，需另验同步链路。"