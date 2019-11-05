#!/bin/bash

set -euo pipefail

serviceId='${serviceId}'
recordId='auto-generated'
tagValue='${tagValue}'

echo "Discovering $tagValue IPs..."

ips="$(
  aws ec2 describe-instances \
    --filters "Name=tag:Name,Values=$tagValue" \
    --query 'Reservations[].Instances[].PrivateIpAddress' \
    --output text \
  | sed 's/\s\s*/,/g'
)"

echo "  $ips"

[ -z "$ips" ] && echo "No IPs found! At a minimum this current instance should be in the results." && exit 1

echo "Updating service registry..."

aws servicediscovery register-instance \
  --service-id "$serviceId" \
  --instance-id "$recordId" \
  --attributes=AWS_INSTANCE_IPV4="$ips"

echo "Done"
