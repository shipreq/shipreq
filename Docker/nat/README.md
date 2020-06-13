Ports
=====

Ports 3129 and 3130 are for use as a transparent proxy and require iptables to route traffic.
This is actually how it's used in prod because the routing table in the AWS VPC is configured to send
all traffic from private subnets to the NAT, which via the iptable rules in the ec2 init script, route
to ports 3129 and 3130, in place of 80 and 443 respectively.

I never managed to get iptables routing working locally so I can't test the ssl-bump rules locally.

Port 3128 is for use as an explicit proxy (eg. `http_proxy` env var)
I ended up testing in prod to get ports 3129/3130 working properly and now that I have,
https whitelisting via the explicit proxy has stopped working.
I'll just live with it for now as it's not used in prod.


Testing build & squid config
============================

Note: https whitelisting via the explicit proxy has stopped working as mentioned above.

1. `make watch` and make edits until happy


Testing iptables (locally)
==========================

I never got this working. Fuck it.

1. In terminal 1: make run
2. In terminal 2: ./test-iptables
3. Edit, repeat.


Testing iptables (in prod)
==========================

1. (Locally) `make export`
2. (AWS Web) Delete the `prod-nat` service
3. (prod-nat) sudo -i
4. (prod-nat) `aws s3 cp s3://shipreq-tmp/squid.tar.gz /tmp`
5. (prod-nat) `mkdir 1 && cd 1 && tar xvzf /tmp/squid.tar.gz`
6. Edit files, `make run`, test usage from `app.prod.sd.internal`, repeat until happy
7. (prod-nat) `make export`
8. (Locally) `make import`
9. (Locally) Extract and commit changes
10. (Locally) Run Terraform to reinstate the `prod-nat` service
