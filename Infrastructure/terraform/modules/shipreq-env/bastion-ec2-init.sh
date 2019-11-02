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

cat >> /etc/ssh/sshd_config << 'EOB'
  Port 22
  Port 36017
EOB

systemctl restart sshd

####################################################################################################
# Postgres

amazon-linux-extras install -y postgresql11
cat >> ~ec2-user/.bashrc << 'EOB'
  alias postgres='psql -h ${POSTGRES_DOMAIN} postgres'
EOB


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
# Portal

$(aws ecr get-login --no-include-email --region ap-southeast-2)

cat > /usr/bin/portal-run << 'EOB'
#!/bin/bash
docker pull ${PORTAL_IMAGE}
docker run \
  -d \
  --restart unless-stopped \
  -p 8000:80 \
  -e CADVISOR_URL=TODO \
  -e DNS_TTL=${DNS_TTL} \
  -e GRAFANA_URL=TODO \
  -e JAEGER_URL= \
  -e KIBANA_URL=${KIBANA_URL} \
  -e PROMETHEUS_URL=${PROMETHEUS_URL} \
  -e SHIPREQ_ENV=${ENV_NAME} \
  -e SHIPREQ_URL_HTTP=TODO \
  -e SHIPREQ_URL=TODO \
  --name portal \
  ${PORTAL_IMAGE}
EOB

chmod 700 /usr/bin/portal-run

/usr/bin/portal-run