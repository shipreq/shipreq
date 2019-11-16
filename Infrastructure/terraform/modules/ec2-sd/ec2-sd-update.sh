#!/bin/bash

set -euo pipefail

serviceId='${serviceId}'
instanceId="$(curl -s http://169.254.169.254/latest/meta-data/instance-id)"
ip="$(curl -s http://169.254.169.254/latest/meta-data/local-ipv4)"
tagValue='${tagValue}'

echo "Registering own ip..."

aws servicediscovery register-instance \
  --service-id "$serviceId" \
  --instance-id "$instanceId" \
  --attributes=AWS_INSTANCE_IPV4="$ip"

echo "Discovering $tagValue IPs..."

ips="$(
  aws ec2 describe-instances \
    --filters "Name=tag:Name,Values=$tagValue" \
    --query 'Reservations[].Instances[].PrivateIpAddress' \
    --output text \
  | sed 's/\s\s*/,/g'
)"

echo "  $ips"
ipstr=",$ips,"

echo "Discovering records..."

records="$(
  aws servicediscovery list-instances \
    --service-id "$serviceId" \
    --query 'Instances[].[Id,Attributes.AWS_INSTANCE_IPV4]' \
    --output text
)"

while read -r rec; do
  echo "Found record: [$rec]"
  id="$${rec%$'\t'*}"
  ip="$${rec#*$'\t'}"
  if [ -z "$id" -o -z "$ip" ]; then
    echo "Ignoring result: [$rec]"
  else
    echo "Checking record: id=$id, ip=$ip"
    if [[ "$ipstr" == *",$ip,"* ]]; then
      echo "  ok"
    else
      echo "  IP is stale. Removing record..."
      aws servicediscovery deregister-instance \
        --service-id "$serviceId" \
        --instance-id "$id"
    fi
  fi
done <<< "$records"

echo "Done"
