#!/usr/bin/env bash
# 隔离目录自测 build_engine_pack.sh 三种场景；不触碰生产 engine/ 与正式包。
set -uo pipefail
APP=/srv/catsco-agent/work/personal-analysis-app/app
BUILD="$APP/tools/build_engine_pack.sh"
B="$(mktemp -d /tmp/bp-test.XXXXXX)"
mkdir -p "$B/engine/sc-plot-kit/scripts/lib" "$B/engine/sc-pipeline/scripts" \
         "$B/engine/sc-ppt-kit/scripts" "$B/state/exports"
printf 'source("lib/diff_skip.R")\ncat("ok\\n")\n' > "$B/engine/sc-plot-kit/scripts/plot_all_v2.R"
printf 'diff_skip_init <- function() invisible(TRUE)\n' > "$B/engine/sc-plot-kit/scripts/lib/diff_skip.R"
printf 'x<-1\n' > "$B/engine/sc-pipeline/scripts/02_annotate.R"
printf 'print(1)\n' > "$B/engine/sc-ppt-kit/scripts/make_ppt.py"
printf 'verdict <- 1\n' > "$B/engine/verdict.R"
printf 'annotation_review <- 1\n' > "$B/engine/annotation_review.R"
echo OLDPACK > "$B/state/exports/engine_pack.tar.xz"
OLD=$(md5sum "$B/state/exports/engine_pack.tar.xz" | cut -d' ' -f1)

echo "=== A 正常构建 ==="
PA_APP_DIR="$B" bash "$BUILD" > /tmp/bp-a.log 2>&1
echo "exit=$?"
tail -4 /tmp/bp-a.log
PACK="$B/state/exports/engine_pack.tar.xz"
if tar -tJf "$PACK" > /dev/null 2>&1; then echo "pack_readable=yes"; else echo "pack_readable=no"; fi
echo "lib_in_pack=$(tar -tJf "$PACK" | grep -c 'scripts/lib/diff_skip.R')"
echo "leftover_tmp=$(ls -A "$B/state/exports" | grep -c '^\.engine_pack' || true)"

# A 成功会替换正式包；为验证 B 的"原包保留"，这里重建一个标记包
echo OLDPACK2 > "$B/state/exports/engine_pack.tar.xz"
OLD=$(md5sum "$B/state/exports/engine_pack.tar.xz" | cut -d' ' -f1)

echo "=== B 缺 diff_skip ==="
mv "$B/engine/sc-plot-kit/scripts/lib/diff_skip.R" "$B/_ds_hold"
PA_APP_DIR="$B" bash "$BUILD" > /tmp/bp-b.log 2>&1
echo "exit=$?"
grep -E 'MISSING_DEP|依赖自检' /tmp/bp-b.log | head -2
NOW=$(md5sum "$PACK" | cut -d' ' -f1)
if [ "$NOW" = "$OLD" ]; then echo "old_pack=preserved"; else echo "old_pack=changed"; fi
mv "$B/_ds_hold" "$B/engine/sc-plot-kit/scripts/lib/diff_skip.R"

echo "=== C 打包失败（不可读文件） ==="
head -c 1024 /dev/urandom > "$B/engine/unreadable.bin"
chmod 000 "$B/engine/unreadable.bin"
BEFORE=$(md5sum "$PACK" | cut -d' ' -f1)
PA_APP_DIR="$B" bash "$BUILD" > /tmp/bp-c.log 2>&1
echo "exit=$?"
grep -E '打包失败|不可读|FAIL' /tmp/bp-c.log | head -2
AFTER=$(md5sum "$PACK" | cut -d' ' -f1)
if [ "$AFTER" = "$BEFORE" ]; then echo "pack_unchanged=yes"; else echo "pack_unchanged=no"; fi
echo "leftover_tmp=$(ls -A "$B/state/exports" | grep -c '^\.engine_pack' || true)"
chmod 644 "$B/engine/unreadable.bin"
echo "fixture_dir=$B"