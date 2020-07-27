locals {

  update_sh_path = "/usr/bin/ec2-sd-update"

  ######################################################################################################################
  user_data = <<EOB

yum -y install awscli
aws configure set default.region "$(curl -s http://169.254.169.254/latest/meta-data/placement/availability-zone | sed 's/[a-z]*$//')"

cat <<'EOF' > ${local.update_sh_path}
${templatefile("${path.module}/ec2-sd-update.sh", {
  serviceId = aws_service_discovery_service.main.id
  tagValue  = var.ec2_name_tag
})}
EOF

chmod 755 ${local.update_sh_path}

echo '${var.cron_schedule} root ${local.update_sh_path}' > "/etc/cron.d/ec2-sd-update"

${local.update_sh_path} || true

EOB
########################################################################################################################
}
