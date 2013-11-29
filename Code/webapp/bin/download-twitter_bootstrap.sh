#!/bin/bash

rr=$(cd "$(dirname "$0")/.." && pwd)
[ ! -d "$rr" ] && echo "Couldn't determine target dir: $rr" && exit 1

tmp=/tmp/golly-rr-twb
rm -rf $tmp
mkdir -p $tmp
cd $tmp

v=3.0.0 # Keep this version in sync with the CDNs in default.scaml & blank.scaml

wget https://github.com/twbs/bootstrap/releases/download/v$v/bootstrap-$v-dist.zip
unzip bootstrap-$v-dist.zip \
  && cp -pv dist/js/bootstrap.min.js         "$rr"/src/main/javascript/vendor/bootstrap.js \
  && cp -pv dist/css/bootstrap.min.css       "$rr"/src/main/webapp/css/vendor/bootstrap.css \
  && cp -pv dist/css/bootstrap-theme.min.css "$rr"/src/main/webapp/css/vendor/bootstrap-theme.css

