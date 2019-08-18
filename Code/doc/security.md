Security
========

*Note: These instructions from 18th December, 2017.*

Install testssl.sh
```sh
pacman -S testssl.sh
```

```sh
bin/env dev up webapp

testssl shipreq.com:14443
```

Ignore LUCKY13 (CVE-2013-0169), it's fixed in Java 8.
https://www.oracle.com/technetwork/topics/security/javacpufeb2013update-1905892.html
