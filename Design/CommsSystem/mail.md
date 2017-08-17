How Mail to contact@shipreq.com is Received
===========================================

1. [Gandi] Gandi points shipreq.com to Amazon for DNS.
2. [Route 53] MX record directs mail to gandi.net.
3. [Gandi] Forwards contact@shipreq.com to shipreq@gmail.com.
4. [Gmail] Arrive.

Security
========
DNS records TXT and SPF declare the whitelist of mail exchanges allowed to send mail on behalf of shipreq.com.
DKIM signatures are currently missing though.

Debugging
=========

nslookup -q=mx shipreq.com
