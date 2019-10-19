#!/bin/bash

# Bash "strict mode"
set -euo pipefail

# Echo commands before running them
set -x

####################################################################################################
# System & environment

for f in ~{root,ec2-user}/.bashrc; do
  cat >> $f << 'EOB'
    export PS1='\n\[\e[91m[bastion-${ENV}]\e[0m \[\e[32m\]\u@\h: \[\e[33m\]\w\[\e[0m\]\n> '
    export LS_OPTIONS='--color=auto'
    alias ls='ls $LS_OPTIONS'
    alias ll='ls $LS_OPTIONS -l'
    alias la='ls $LS_OPTIONS -la'
    alias yy='sudo yum -y install'
EOB
done

yum -y update
yum -y install htop nc tree

####################################################################################################
# SSH

cat >> /etc/ssh/sshd_config << 'EOB'
  Port 22
  Port 36017
EOB

systemctl restart sshd

####################################################################################################
# Docker

amazon-linux-extras install -y docker
systemctl start docker

####################################################################################################
# redis-cli

cat >> /usr/bin/redis-cli << 'EOB'
#!/bin/bash
args=("$@")
[ $# -eq 0 ] && args=(-h ${REDIS_HOST} -p 6379)
exec sudo docker run --rm -it bitnami/redis:${REDIS_VER} redis-cli "$${args[@]}"
EOB

chmod 755 /usr/bin/redis-cli

docker pull bitnami/redis:${REDIS_VER}

####################################################################################################
# Start portal

$(aws ecr get-login --no-include-email --region ap-southeast-2)

docker run \
  -d \
  --restart unless-stopped \
  -p 8000:80 \
  -e DNS_TTL=10s \
  -e SHIPREQ_ENV=${ENV_NAME} \
  -e SHIPREQ_URL=TODO \
  -e SHIPREQ_URL_HTTP=TODO \
  -e PROMETHEUS_URL=${PROMETHEUS_URL} \
  -e GRAFANA_URL=TODO \
  -e KIBANA_URL=TODO \
  -e JAEGER_URL= \
  --name portal \
  ${PORTAL_IMAGE}
