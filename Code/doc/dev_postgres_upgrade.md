After upgrading the postgres version in the `shipreq/dev/postgres` image,
starting the dev postgres instance will fail because a migration is needed.

# How to fix after upgrading and watching it fail

```sh
> # In terminal 2
> sudo -i
> cd /var/lib/docker/volumes

bin/dev stop postgres
docker ps -a | fgrep shipreq_dev_postgres # and copy the container id

> b=shipreq_dev_postgres_backup
> cat /var/lib/docker/containers/<container id>*/config.v2.json | jq .
> # Copy the volume ID from MountPoints
> cp -rp 970edeb81d1a0d2a34f92cb4cfee32ab88099db3408bb8f47a23ad4e7e652fec $b

bin/dev down postgres
docker images shipreq/dev/postgres
# Select an old version and set it in docker-compose
bin/dev up postgres
# ctrl-c
docker ps -a | fgrep shipreq_dev_postgres # and copy the container id

> cat /var/lib/docker/containers/<container id>*/config.v2.json | jq .
> c=<volume id> # Copy the volume ID from MountPoints
> rm -rf $c
> cp -rp $b $c

bin/dev up -d postgres
cd envs/dev
export DOCKER0="$(ip route show | grep docker0 | awk '{print $9}')"
docker-compose -p shipreqdev exec -T postgres pg_dumpall -l shipreq_dev -U dev > dump.sql
../../bin/dev down postgres
# Update docker-compose to use new postgres version
../../bin/dev up -d postgres
docker-compose -p shipreqdev exec -T postgres psql -d shipreq_dev -U dev < dump.sql
mv dump.sql /tmp/shipreq_dev_postgres.sql
```
