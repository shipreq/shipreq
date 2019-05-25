Local connections
=================

```sh
# Dev
psql -h localhost -p 14032 -U dev -d shipreq_dev
```


Migration from Postgres 9 to 11
===============================

```sh
bin/env dev up -d postgres
f=/tmp/shipreq-dump.sql
pg_dumpall -h localhost -p 14032 -U dev -l shipreq_dev -f $f --quote-all-identifiers
# Enter password 'sqd' 4 times
bin/env dev stop postgres
bin/env dev rm postgres
bin/env test rm postgres
# Edit postgres version in docker-compose.yml
bin/env dev up -d postgres
psql -h localhost -p 14032 -U dev -f $f shipreq_dev
# Enter password 'sqd'
```

Backup & restore of data only
=============================

#### Common

```sh
f=/tmp/shipreq-data.sql
```

#### Backup

```sh
pg_dump -h localhost -p 14032 -U dev -d shipreq_dev --quote-all-identifiers --data-only --exclude-table='*.schema_version' --exclude-table='*.*flyway*' -f $f
# Enter password 'sqd'
```

#### Restore

```sh
psql -h localhost -p 14032 -U dev -f $f shipreq_dev
# Enter password 'sqd'
```