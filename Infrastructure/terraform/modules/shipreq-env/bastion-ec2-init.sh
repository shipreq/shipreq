#!/bin/bash

# Bash "strict mode"
set -euo pipefail

for f in ~{root,ec2-user}/.bashrc; do
  cat >> $f << 'EOB'
    export PS1='\n\[\e[91m[bastion-${env}]\e[0m \[\e[32m\]\u@\h: \[\e[33m\]\w\[\e[0m\]\n> '
    export LS_OPTIONS='--color=auto'
    alias ls='ls $LS_OPTIONS'
    alias ll='ls $LS_OPTIONS -l'
    alias la='ls $LS_OPTIONS -la'
    alias yy='sudo yum -y install'
EOB
done

cat >> /etc/ssh/sshd_config << 'EOB'
  Port 22
  Port 36017
EOB

# Echo commands before running them
set -x

systemctl restart sshd

yum -y update
yum -y install htop tree

amazon-linux-extras install -y docker
