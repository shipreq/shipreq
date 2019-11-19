#!/bin/bash
set -euo pipefail

cd /root

# Remove trailing slashes
CADVISOR_URL=${CADVISOR_URL%/}
GRAFANA_URL=${GRAFANA_URL%/}
KIBANA_URL=${KIBANA_URL%/}
PROMETHEUS_TECH_URL=${PROMETHEUS_TECH_URL%/}
PROMETHEUS_BIZ_URL=${PROMETHEUS_BIZ_URL%/}

export DNS_IP="$(cat /etc/resolv.conf | egrep ' *nameserver ' | head -1 | sed 's/\s*\S\S*\s*//; s/\s.*//')"

vars=(
  CADVISOR_URL
  DNS_IP
  DNS_TTL
  FRESHDESK_DOMAIN
  GA_TRACKING_ID
  GRAFANA_URL
  JAEGER_URL
  KIBANA_URL
  PROMETHEUS_BIZ_URL
  PROMETHEUS_TECH_URL
  SHIPREQ_ENV
  SHIPREQ_URL
)

varstring=

for v in "${vars[@]}"; do
  printf "%20s = %s\n" $v ${!v}
  varstring="$varstring "'${'"$v}"
done
echo

function reify {
  cat "$1" | envsubst "$varstring" > "$2"
  # echo "========================================================================================"
  # echo "$2"
  # echo "========================================================================================"
  # cat "$2"
  # echo "========================================================================================"
  # echo
}

reify portal.html /usr/share/nginx/html/index.html
reify nginx.conf /etc/nginx/nginx.conf

exec nginx -g 'daemon off;'