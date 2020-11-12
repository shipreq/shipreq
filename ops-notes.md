DB Login
========

```sh
ssh shipreq-bastion-prod
psql -h postgres.prod.internal shipreq shipreq
```


Ops API
========

### Prerequisite: Establish a connection

```sh
ssh shipreq-bastion-prod

ssh app.prod.sd.internal

port=$(sudo docker ps | fgrep webapp | sed -e 's/.*:\([0-9][0-9]*\)->8080.*/\1/')

 auth="-F secret=Hooquail2aehiey1viemiefaayengeiGhuch8Eishee3OHu4aiKieth3lieshaid"
```

### Register an account for someone

```sh
 curl http://localhost:$port/ops/register1 -X POST $auth -F email=japgolly@gmail.com
```

### Download a project as json

ShipReq is #3

```sh
id=3
 curl http://localhost:$port/ops/project/events -X POST $auth -F id=$id -o /tmp/project-$id.json
 wc -l /tmp/project-$id.json
 aws s3 mv /tmp/project-$id.json s3://shipreq-tmp
```

and then locally, with ShipReq local/dev running

```sh
id=3
aws s3 cp s3://shipreq-tmp/project-$id.json .
 auth="-F secret=Hooquail2aehiey1viemiefaayengeiGhuch8Eishee3OHu4aiKieth3lieshaid"
 curl -v http://localhost:8080/ops/project/create -X POST $auth -F events='<'project-$id.json -F user=japgolly
```


Prod Issues
============

There are currently two issues that occur in prod semi-regularly.

1. NAT going down.
  Usually the entire EC2 will be screwed, it will likely have disappeared from metrics.
  In which case, terminate the EC2 then re-run Terraform.

2. ElasticSearch being out of space.

   Modify `elasticsearch_retention_days` in `prod.tf`, and apply it. Then:

  ```sh
  ssh shipreq-bastion-prod
  ssh ops.prod.sd.internal
  ```

  and then either

  ```sh
  sudo elasticsearch_maintenance
  ```

  or

  ```sh
  curl -s -k -X GET "https://es.prod.internal:443/_cat/indices?v&bytes=mb"
  curl -k -X DELETE "https://es.prod.internal:443/filebeat-2020.11.11"
  ```
