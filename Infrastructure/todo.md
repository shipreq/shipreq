* Bastion
  * Stop using a shared key - key should be per user
  * Use AWS Session Manager instead of opening an SSH port
  * Log logins
  * Log commands (?)
  * Log portal (?)
  * Healthcheck & recovery

* NAT
  * Collect logs
  * Healthcheck & recovery

* Cluster EC2s
  * Collect logs (?)

====================================================================================================

* Bastion
  * node_exporter (?)
  * cadvisor (?)
  * filebeat (?)

* App
  * ALB
  * ShipReq
  * Taskman
  * filebeat
  * cadvisor
  * node_exporter

* Ops
  * node_exporter
  * postgres_exporter

* Alerting

* DR
  * Postgres
  * EBS
