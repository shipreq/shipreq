#!/bin/sh

set -e

chown squid:squid /dev/stdout
chmod ugo+w /dev/stdout

ssldb=/var/cache/squid/ssl_db
test -d $ssldb || /usr/lib/squid/security_file_certgen -c -s $ssldb -M 4MB
chown -R squid:squid $ssldb

/usr/sbin/squid --foreground -z
exec /usr/sbin/squid --foreground -YCd 1 "$@"
