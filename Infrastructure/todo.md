* Bastion
  * Stop using a shared key - key should be per user
  * Use AWS Session Manager instead of opening an SSH port
  * Log commands (?)
  * Log portal (?)
  * node_exporter (?)
  * cadvisor (?)
  * add IP to SG, add lambda to auto purge SG

* NAT
  * node_exporter (?)

* Cluster EC2s
  * Collect logs (?)

====================================================================================================

* postgres_exporter

* Alerting

* DR
  * Postgres
  * EBS

* Separate account for prod or non-prod (?)

* Refactor Terraform