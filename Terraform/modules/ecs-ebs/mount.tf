locals {

  mount_dir = "/mnt/${local.tag_value}"

  mount_sh = "\"/usr/bin/mount-${var.name}\""

  ######################################################################################################################
  user_data = <<EOB

yum -y install awscli
aws configure set default.region "$(curl -s http://169.254.169.254/latest/meta-data/placement/availability-zone | sed 's/[a-z]*$//')"

cat <<'EOF' > ${local.mount_sh}
${templatefile("${path.module}/mount.sh", {
  _device   = var.device_path
  _tagValue = local.tag_value
  _mount    = local.mount_dir
})}
EOF

chmod 755 ${local.mount_sh}

${local.mount_sh} || echo "${local.mount_sh} failed."

echo '${var.cron_schedule} root ${local.mount_sh}' > "/etc/cron.d/mount-${var.name}"

EOB
########################################################################################################################

}
