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
  * average response time

* Is everything healthy now?
  * THEME: Current values, percentages: current/capacity, no history but minimal (1m) interval where required
  * {node,container} quantity up vs expected
  * {node,container} uptime
  * {node,container} cpu: load avg vs capacity
  * {node,container} mem: used vs capacity
  * {node,container} mem: thrashing/swapping (?)
  * {node,container} fds: used vs capacity (?)
  * disk space: used vs capacity
  * network: ... up? errors? although if currently down, then no metrics available until back online
  * logs: {warn,error} rate (maybe over 1/5/15 like load?)
  * average response time

* Is everything going to stay healthy? Is there any preemptive action to take?
  * THEME: trend/capacity
  * Disk space: free vs trend
  * Memory: free vs trend
  * CPU: capacity vs trend

* What's happening with/in Node/Container/X

* How is everything being used?
  * What kind of volume are we experiencing?
  * Breadth: how many requests, unique users
  * Depth: how many interactions/user



Business metrics
=================

For nearly all of the following, have the ability to show
  - total
  - today
  - this week
  - per day
  - per week
  - per month

Metrics to present
  - landing page reqs
  - users
    - registered vs not
    - last login
    - customer engagement score
  - projects
    - total created & deleted
  - requirements
    - total
    - per project
  - events
    - total
    - per project
  - disk space
    - total
    - per project
    - per user
    - per event
    - per requirement
  - usage/engagement
    - unique users logging in
    - events created
    - project reads
    - unique projects read
    - avg session time per user

Metrics to collect:
  - shipreq_users_total                     {registered=y/n}
  - shipreq_users_last_login_seconds_ago    (summary)
  - shipreq_projects_total                  {live=y/n}
  - shipreq_events_total                    {live_project=y/n, initial=y/n}
  - shipreq_reqs_total                      {live_project=y/n, live_req=y/n}
  - shipreq_db_table_bytes_total            {table=xxx, type=table/indexes}
  - shipreq_project_last_access_seconds_ago {access=w/rw} (summary)
  - shipreq_daily_logins_total              {unique_user=y/n}
  - shipreq_daily_project_reads_total       {unique_project=y/n, access=r/w/rw} -- note rw needed because when unique_project=y, ¬(rw = r+w)

Metrics pending:
  - customer engagement score
  - landing page reqs
  - avg session time per user
