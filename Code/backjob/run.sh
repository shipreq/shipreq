#!/bin/bash
cd "$(dirname "$0")/.." && \
sbt 'project backjob' run "$@"
