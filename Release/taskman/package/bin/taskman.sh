#!/bin/bash
$(dirname $(readlink -e "$0"))/run.sh shipreq.taskman.server.app.Server "$@"
