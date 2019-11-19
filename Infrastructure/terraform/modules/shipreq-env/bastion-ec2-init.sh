#!/bin/bash

# Bash "strict mode"
set -euo pipefail

# Echo commands before running them
set -x

####################################################################################################
# System & environment

for f in ~{root,ec2-user}/.bashrc; do
  cat >> $f << 'EOB'
    export LS_OPTIONS='--color=auto'
    alias ls='ls $LS_OPTIONS'
    alias ll='ls $LS_OPTIONS -l'
    alias la='ls $LS_OPTIONS -la'
    alias yy='sudo yum -y install'
EOB
done
cat >> ~root/.bashrc << 'EOB'
  export PS1='\n\[\e[95m[bastion-${ENV}]\e[0m \[\e[31m\]\u\[\e[32m\]@\h: \[\e[33m\]\w\[\e[0m\]\n> '
EOB
cat >> ~ec2-user/.bashrc << 'EOB'
  export PS1='\n\[\e[95m[bastion-${ENV}]\e[0m \[\e[32m\]\u@\h: \[\e[33m\]\w\[\e[0m\]\n> '
EOB

yum -y update
yum -y install htop nc tree

####################################################################################################
# SSH

# sed -i -e 's/^ *\$ActionFileDefaultTemplate .*/$ActionFileDefaultTemplate RSYSLOG_FileFormat/' /etc/rsyslog.conf
# service rsyslog restart

cat >> /etc/ssh/sshd_config << 'EOB'
  Port 22
  Port 36017
EOB

systemctl restart sshd

####################################################################################################
# Security

echo '0 0 * * * root yum -y update --security' > /etc/cron.d/security-updates

# Prevent bastion host users from viewing processes owned by other users.
# See: https://github.com/aws-quickstart/quickstart-linux-bastion/blob/master/scripts/bastion_bootstrap.sh
mount -o remount,rw,hidepid=2 /proc
awk '!/proc/' /etc/fstab > temp && mv temp /etc/fstab
echo "proc /proc proc defaults,hidepid=2 0 0" >> /etc/fstab


####################################################################################################
# Docker

amazon-linux-extras install -y docker
systemctl start docker

$(aws ecr get-login --no-include-email --region ap-southeast-2)

####################################################################################################
# Filebeat

filebeat=/usr/bin/start-filebeat

cat > $filebeat << 'EOB'
#!/bin/bash
docker pull ${FILEBEAT_IMAGE}
docker run \
  -d \
  --restart unless-stopped \
  -v /var/log:/host/var/log:ro \
  -v /var/lib/docker/containers:/var/lib/docker/containers:ro \
  -v /var/run/docker.sock:/var/run/docker.sock:ro \
  -e CLUSTER=bastion \
  -e ES_HOSTS='${ES_HOSTS}' \
  --name filebeat \
  ${FILEBEAT_IMAGE}
EOB

chmod 700 $filebeat

$filebeat

####################################################################################################
# Portal

portal=/usr/bin/start-portal

cat > $portal << 'EOB'
#!/bin/bash
docker pull ${PORTAL_IMAGE}
docker run \
  -d \
  --restart unless-stopped \
  -p 8000:80 \
  -e CADVISOR_URL=${CADVISOR_URL} \
  -e DNS_TTL=${DNS_TTL} \
  -e FRESHDESK_DOMAIN=${FRESHDESK_DOMAIN} \
  -e GA_TRACKING_ID=${GA_TRACKING_ID} \
  -e GRAFANA_URL=${GRAFANA_URL} \
  -e JAEGER_URL= \
  -e KIBANA_URL=${KIBANA_URL} \
  -e PROMETHEUS_BIZ_URL=${PROMETHEUS_BIZ_URL} \
  -e PROMETHEUS_TECH_URL=${PROMETHEUS_TECH_URL} \
  -e SHIPREQ_ENV=${ENV_NAME} \
  -e SHIPREQ_URL=${SHIPREQ_URL} \
  --name portal \
  ${PORTAL_IMAGE}
EOB

chmod 700 $portal

$portal

####################################################################################################
# Postgres

amazon-linux-extras install -y postgresql11
cat >> ~ec2-user/.bashrc << 'EOB'
  alias postgres='psql -h ${POSTGRES_DOMAIN} postgres'
EOB

####################################################################################################
# redis-cli

cat >> /usr/bin/redis-cli << 'EOB'
#!/bin/bash
args=("$@")
[ $# -eq 0 ] && args=(-h ${REDIS_HOST} -p 6379)
exec sudo docker run --rm -it bitnami/redis:${REDIS_VER} redis-cli "$${args[@]}"
EOB

chmod 700 /usr/bin/redis-cli

docker pull bitnami/redis:${REDIS_VER}
