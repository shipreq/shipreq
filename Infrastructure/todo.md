* Bastion
  * Stop using a shared key - key should be per user
  * Use AWS Session Manager instead of opening an SSH port
  * Log commands (?)
  * Log portal (?)
  * node_exporter (?)
  * cadvisor (?)

* NAT
  * Collect logs / filebeat (?)
  * node_exporter (?)

* Cluster EC2s
  * Collect logs (?)

* Healthcheck: node_exporter

* Healthcheck: filebeat

* Healthcheck: taskman (what's `taskman.healthFile` for?)

* A public `<env>.shipreq.com` record is probably a bad idea

====================================================================================================

* ALB
  * Cert
  * Allow HTTP for /robots.txt
  * Allow HTTP for /ops/metrics
  * Allow HTTP for /ops/ok

* postgres_exporter

* Alerting

* DR
  * Postgres
  * EBS

* Updating webapp task def breaks UX; ALB serves 503s for a while