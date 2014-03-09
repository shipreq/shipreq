#!/bin/bash
cd "$(dirname "$0")/.." && \
sbt 'project taskman' run "$@"
