#!/bin/bash
set -e
cp -r /to-home/. /root/.
find /root/.cache/coursier /root/.ivy2 /root/.sbt -name '*.lock' -type f -exec rm -f {} +
