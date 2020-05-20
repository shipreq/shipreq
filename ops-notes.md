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

` curl http://localhost:$port/ops/register1 -X POST $auth -F email=japgolly@gmail.com`

### Download a project as json

```sh
id=1
 curl http://localhost:$port/ops/project/events -X POST $auth -F id=$id -o /tmp/project-$id.json
aws s3 mv /tmp/project-$id.json s3://shipreq-tmp
```

and then locally

```sh
aws s3 cp s3://shipreq-tmp/project-3.json .
```


ElasticSearch Maintainence
==========================

```sh
es=https://es.prod.internal:443
tmp=/tmp/es
curl -s -k -X GET "$es/_cat/indices?v&bytes=mb" | tee $tmp
<$tmp sort -k3

# Delete old data
retention_days=45
del=/tmp/es-del
<$tmp sort -k3 | fgrep filebeat- | head -n-$retention_days | tee $del
for i in $(awk '{print $3}' <$del); do echo "Deleting $i"; curl -k -X DELETE "$es/$i"; echo; done
```