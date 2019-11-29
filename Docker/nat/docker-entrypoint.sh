#!/bin/sh

set -e

chown squid:squid /dev/stdout
chmod ugo+w /dev/stdout

test -d /var/cache/squid/ssl_db || /usr/lib/squid/security_file_certgen -c -s /var/cache/squid/ssl_db -M 4MB

/usr/sbin/squid --foreground -z
exec /usr/sbin/squid --foreground -YCd 1 "$@"