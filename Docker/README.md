Building
========

This is built on AWS. Trigger it via:

    make publish

You can also do a local build by running:

    make build-local


Contents
========

* `analytics_proxy`       - A proxy server for analytics services (like Google Analytics) to avoid ad-blockers. Hosted at `ap.shipreq.com`.
* `dev-build_env`         - A dev environment with everything needed to compile, test and build Shipreq Webapp & Taskman.
* `dev-node`              - A consistent, local-platform-independent Node in a container for use by `Code/frontend`.
* `dev-postgres`          - Postgres with the HLL extension enabled for use in local dev & test environments.
* `nat`                   - The NAT image that servces internet traffic to everything on private subnets/clusters.
* `ops-cadvisor`          - Collects metrics about running Docker containers and serves them to Prometheus.
* `ops-ecs_exporter`      - Collects metrics about ECS and serves them to Prometheus.
* `ops-filebeat`          - Collects logging output from Docker containers and sends them to ElasticSearch.
* `ops-grafana`           - xxx
* `ops-node_exporter`     - Collects metrics about the EC2s on which everything is running, and serves them to Prometheus.
* `ops-portal`            - Web server that serves a portal for ops staff, and reverse-proxied access to ops services. It's served by the bastion.
* `ops-postgres_exporter` - Collects business metrics from the DB (i.e. contains custom SQL queries) and serves them to Prometheus.
* `ops-prometheus-biz`    - The Prometheus service that collects and retains business metrics.
* `ops-prometheus-tech`   - The Prometheus service that collects and retains tech/ops metrics.
* `ops-squid_exporter`    - Collects metrics from the NAT and serves them to Prometheus.
* `shipreq-base`          - The base image upon which real Shipreq Webapp & Taskman services run.


Strategy
========

* All Images are tagged with `git-<sha>`

* Images under this directory are tagged with a source content hash: `src-<md5>`

* In Production (and therefore all non-prod envs too), task definitions will only refer to docker images in internal
  ECRs for the following reasons:

  1. Security: Prevent an attacker replacing a legitimate online image, with an malicious one.

  2. Stability: If the image is removed or renamed, or if Docker Hub is down (which happens), it shouldn't prevent our
     services from (re)starting.

* Base images in SBT are currently just hardcoded to "latest". I don't care. This can be improved in future if necessary.

* All third-party images are extended locally. In addition to satisfying the above requirement that all images come from
  ECR, it also provides a means of injecting config files into containers, which is otherwise a pain with ECS and images
  without built-in support for, say, S3.

* Config files are injectable into ShipReq containers by storing them in S3 and calling `import-files` which is included
  in the `shipreq-base` image.

* Image versions in ECS Task Definitions. This isn't set in stone but I think I'll go like this:
  * Terraform manages the task definitions
  * There will be a variable in Terraform for the tag of all ops images and in practice, values will be `git-<sha>`
  * Similarly, there will be another variable in Terraform for the tag of all app images.