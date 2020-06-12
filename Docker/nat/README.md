Ports
=====

Port 3128 is for use as an explicit proxy (eg. `http_proxy` env var)

Ports 3129 and 3130 are for use as a transparent proxy and require iptables to route traffic.
This is actually how it's used in prod because the routing table in the AWS VPC is configured to send
all traffic from private subnets to the NAT, which via the iptable rules in the ec2 init script, route
to ports 3129 and 3130, in place of 80 and 443 respectively.

I never managed to get iptables routing working locally so I can't test the ssl-bump rules locally.


Testing build & squid config
============================

1. `./test setup`
2. `make watch` and make edits until happy
3. `./test teardown`


Testing iptables
================

I never got this working. Fuck it.

1. In terminal 1: make run
2. In terminal 2: ./test full
3. Edit, repeat.
