#!/bin/bash

sd=src/main/sass
td=src/main/webapp/assets

for f in $sd/*.s?ss; do
  sf=${f##*/}
  tf=${sf%.s?ss}.css
  s=$sd/$sf
  t=$td/$tf
  echo "$s -> $t"
  sass $s --style compressed > $t
done

echo
ls -l $td
