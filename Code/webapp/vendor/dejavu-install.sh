#!/bin/bash

cd "$(dirname "$0")"
tgt="$(cd ../src/main/webapp/assets && pwd)"
echo "TARGET: $tgt"
[ ! -e "$tgt" ] && echo "Dir not found." && exit 2

pkg="$(pwd)/$(ls -1rt | GREP_OPTIONS= grep '^dejavu-fonts-ttf-.*\.tar\.bz2$' | tail -1)"
echo "PACKAGE: $pkg"
[ ! -e "$pkg" ] && echo "Package not found." && exit 2

tmp=/tmp/$(date +%Y%m%d-%H%M%S)-$$
mkdir $tmp && cd $tmp || exit 3
tar xjf "$pkg"
cd * || exit 3

v=$(pwd | perl -pe 's/^.+?([\d.]+)$/\1/')
echo "VERSION: $v"

echo
files=()
for n in DejaVuSans DejaVuSans-Bold; do
  ttf="ttf/$n.ttf"
  woff=${ttf%ttf}woff
  echo "Generating: $woff"
  sfnt2woff -v $v $ttf || exit 3
  files+=($ttf $woff)
done

echo
cp -v ${files[@]} "$tgt" \
 && cd "$tgt" \
 && rm -rf $tmp

echo
echo "Done."

