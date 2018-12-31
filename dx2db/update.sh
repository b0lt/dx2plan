#!/bin/bash
set -euo pipefail

DESTDIR="$(dirname $0)/shared/src/main/resources"
for i in `seq 1 3`; do
  EN_SRCFILE="/sdcard/Android/data/com.sega.d2megaten.en/cache/data$i.d" 
  JP_SRCFILE="/sdcard/Android/data/com.sega.d2megaten/cache/data$i.d" 
  adb shell cat $EN_SRCFILE | gunzip | python -m json.tool > "$DESTDIR/en/data$i"
  adb shell cat $JP_SRCFILE | gunzip | python -m json.tool > "$DESTDIR/jp/data$i"
done
