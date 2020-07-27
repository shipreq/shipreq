#!/bin/bash

# Vars
# es_url             = ${es_url}
# region             = ${region}
# ssm_retention_days = ${ssm_retention_days}

set -euo pipefail

tmp1=/tmp/elasticsearch-maintenance-1.tmp
tmp2=/tmp/elasticsearch-maintenance-2.tmp

# Read parameter
retention_days="$(aws --region=${region} ssm get-parameter --name '${ssm_retention_days}' --query Parameter.Value --output text)"
echo "retention_days = $retention_days"

# Download indices
curl -s -k -X GET "${es_url}/_cat/indices?v&bytes=mb" > $tmp1

# Filter
<$tmp1 sort -k3 | fgrep filebeat- | head -n-$retention_days > $tmp2

# Delete
for i in $(awk '{print $3}' <$tmp2); do
  echo "Deleting $i"
  curl -k -X DELETE "${es_url}/$i"
  echo
done
