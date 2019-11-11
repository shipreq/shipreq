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

====================================================================================================

* App
  * ALB
  * ShipReq
  * Taskman

* Ops
  * postgres_exporter

* Alerting

* DR
  * Postgres
  * EBS
