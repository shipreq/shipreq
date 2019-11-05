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
  export PS1='\n\[\e[91m[${cluster}-cluster]\e[0m \[\e[91m\]\u\[\e[32m\]@\h: \[\e[33m\]\w\[\e[0m\]\n> '
EOB
cat >> ~ec2-user/.bashrc << 'EOB'
  export PS1='\n\[\e[91m[${cluster}-cluster]\e[0m \[\e[32m\]\u@\h: \[\e[33m\]\w\[\e[0m\]\n> '
EOB

yum -y update
yum -y install htop nc tree

####################################################################################################

cat <<'EOB' >> /etc/ecs/ecs.config
ECS_CLUSTER=${cluster}
EOB

${install_prometheus_tech_ebs}
${install_prometheus_biz_ebs}