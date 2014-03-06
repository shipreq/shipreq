#!/bin/bash
cd "$(dirname "$0")/.." && \
sbt 'project webapp' '~;container:start; container:reload /'
