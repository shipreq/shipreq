#!/bin/bash

rr=$(cd "$(dirname "$0")/.." && pwd)
[ ! -d "$rr" ] && echo "Couldn't determine target dir: $rr" && exit 1

tmp=/tmp/golly-rr-twb
rm -rf $tmp
mkdir -p $tmp
cd $tmp

wget http://twitter.github.io/bootstrap/assets/bootstrap.zip
unzip bootstrap \
  && cp -pv bootstrap/js/bootstrap.min.js   "$rr"/src/main/webapp/js/vendor/bootstrap.js \
  && cp -pv bootstrap/css/bootstrap.min.css "$rr"/src/main/webapp/css/vendor/bootstrap.css

