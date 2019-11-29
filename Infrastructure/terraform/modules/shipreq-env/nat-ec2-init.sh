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
  export PS1='\n\[\e[95m[nat-${ENV}]\e[0m \[\e[31m\]\u\[\e[32m\]@\h: \[\e[33m\]\w\[\e[0m\]\n> '
EOB
cat >> ~ec2-user/.bashrc << 'EOB'
  export PS1='\n\[\e[95m[nat-${ENV}]\e[0m \[\e[32m\]\u@\h: \[\e[33m\]\w\[\e[0m\]\n> '
EOB

yum -y update
yum -y install htop nc tree

####################################################################################################
# Security

echo '0 0 * * * root yum -y update --security' > /etc/cron.d/security-updates

####################################################################################################
# Docker

amazon-linux-extras install -y docker
systemctl start docker

$(aws ecr get-login --no-include-email --region ap-southeast-2)

####################################################################################################
# NAT

iptables -t nat -A PREROUTING -p tcp --dport 80 -j REDIRECT --to-port 3129
iptables -t nat -A PREROUTING -p tcp --dport 443 -j REDIRECT --to-port 3130

nat=/usr/bin/start-nat

cat > $nat << 'EOB'
#!/bin/bash
docker pull ${NAT_IMAGE}
docker run \
  -d \
  --restart unless-stopped \
  --network host \
  --name nat \
  ${NAT_IMAGE}
EOB

chmod 700 $nat

$nat

####################################################################################################
# Filebeat

filebeat=/usr/bin/start-filebeat

cat > $filebeat << 'EOB'
#!/bin/bash
docker pull ${FILEBEAT_IMAGE}
docker run \
  -d \
  --restart unless-stopped \
  --network host \
  -v /var/log:/host/var/log:ro \
  -v /var/lib/docker/containers:/var/lib/docker/containers:ro \
  -v /var/run/docker.sock:/var/run/docker.sock:ro \
  -e CLUSTER=nat \
  -e ES_HOSTS='${ES_HOSTS}' \
  --name filebeat \
  ${FILEBEAT_IMAGE}
EOB

chmod 700 $filebeat

$filebeat
