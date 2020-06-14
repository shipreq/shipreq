locals {

  elasticsearch_maintenance_sh = trimspace(templatefile("${path.module}/elasticsearch-maintenance.sh", {
    es_url         = local.es_root_url_with_port
    retention_days = var.elasticsearch_retention_days
  }))

  elasticsearch_maintenance_sh_path = "/usr/bin/elasticsearch_maintenance"

  install_elasticsearch_maintenance = <<EOB
########################################################################################################################
# Elasticsearch maintenance

cat <<'EOF' > ${local.elasticsearch_maintenance_sh_path}
${local.elasticsearch_maintenance_sh}
EOF

chmod 755 ${local.elasticsearch_maintenance_sh_path}

echo '${var.elasticsearch_maintenance_cron_schedule} root ${local.elasticsearch_maintenance_sh_path}' > "/etc/cron.d/elasticsearch_maintenance"

EOB

}
