#!/bin/bash

[ $# -ne 1 ] && echo "Usage: $0 <service-id>" && exit 1

serviceId="--service-id=$1"

echo "Draining servicediscovery instances from $1 ..."
ids="$(aws servicediscovery list-instances $serviceId --query 'Instances[].Id' --output text)"

found=
while read -r id; do
  if [ -n "$id" ]; then
    echo "Deregistering $1 / $id ..."
    aws servicediscovery deregister-instance $serviceId --instance-id "$id"
    found=1
  fi
done <<< "$ids"

# Yes, I'm being lazy here...
[ -n "$found" ] && echo sleep 3 || true