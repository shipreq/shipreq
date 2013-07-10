#!/bin/bash

sudo -u postgres pg_dump --encoding=UTF-8 --data-only --column-inserts --exclude-table=schema_version "$@"
