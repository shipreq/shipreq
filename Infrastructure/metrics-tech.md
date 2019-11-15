Intents
=======

* What's the health of the system been like?
  * THEME: history, trends, configurable time range

* What's the health of the system right now?
  * THEME: Single values, gauges: current/capacity, minimal history where necessary
  * {nodes,containers} quantity up
  * {nodes,containers} uptime
  * {nodes,containers} cpu: load avg vs capacity
  * {nodes,containers} mem: used vs capacity
  * {nodes,containers} mem: thrashing/swapping (?)
  * {nodes,containers} fds: used vs capacity (?)
  * disk space: used vs capacity
  * network: ... up? errors? although if currently down, then no metrics available until back online
  * logs: {warn,error} rate (maybe over 1/5/15 like load?)

* What's the health of the system going to be? Is there any preemptive action to take?
  * THEME: trend/capacity
  * disk space: trend vs capacity



* Can we forecast?
  * https://developer.bring.com/blog/forecasted-alerts-with-grafana-and-influxdb/


Queries
=======

* Node counts
  * `sum (node_exporter_build_info)`
  * `sum by (job) (node_exporter_build_info)`