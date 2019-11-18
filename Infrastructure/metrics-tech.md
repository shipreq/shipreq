Intents
=======

* Has everything been healthy?
  * THEME: history, trends, configurable time range
  * {node,container} uptime
  * {node,container} count / bounces
  * {node,container} cpu maxed?
  * {node,container} mem maxed? (swapping, paging, ctx switching?)
  * node network connectivity (other forms of health?)
  * free space
  * free fds

* Is everything healthy now?
  * THEME: Single values, gauges: current/capacity, minimal history where necessary
  * {node,container} quantity up vs expected
  * {node,container} uptime
  * {node,container} cpu: load avg vs capacity
  * {node,container} mem: used vs capacity
  * {node,container} mem: thrashing/swapping (?)
  * {node,container} fds: used vs capacity (?)
  * disk space: used vs capacity
  * network: ... up? errors? although if currently down, then no metrics available until back online
  * logs: {warn,error} rate (maybe over 1/5/15 like load?)

* Is everything going to stay healthy? Is there any preemptive action to take?
  * THEME: trend/capacity
  * Disk space: free vs trend
  * Memory: free vs trend
  * CPU: capacity vs trend

* What's happening with/in Node/Container/X
