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

#### Backup

```sh
db-dump-data dev
```

#### Restore

```sh
psql -h localhost -p 14032 -U dev -f /tmp/xxxxxxxxxx.sql shipreq_dev
# Enter password 'sqd'
```


Copying Docker Database
=======================

Make sure docker images are up.

    docker ps

Find the DB image volume location on source and dest machines:

    docker inspect shipreq_dev_postgres | jq '.[0].Mounts[0].Source' | perl -pe 's/^"|\/_data"$//g'

Simply replace the contents of one with the other.
