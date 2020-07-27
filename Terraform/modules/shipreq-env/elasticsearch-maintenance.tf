locals {

  elasticsearch_maintenance_param_retention_days = "/${var.env}/elasticsearch_maintenance_retention_days"

  elasticsearch_maintenance_sh = trimspace(templatefile("${path.module}/elasticsearch-maintenance.sh", {
    es_url             = local.es_root_url_with_port
    region             = local.region
    ssm_retention_days = local.elasticsearch_maintenance_param_retention_days
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

resource "aws_ssm_parameter" "elasticsearch_maintenance_retention_days" {
  name            = local.elasticsearch_maintenance_param_retention_days
  type            = "String"
  value           = tostring(var.elasticsearch_retention_days)
  description     = "Number of days worth of data to retain in ElasticSearch"
  allowed_pattern = "^\\d+$"
  tags            = local.default_tags
}

resource "aws_iam_policy" "read_elasticsearch_maintenance_params" {
  name = "${var.env}_read_elasticsearch_maintenance_params"

  policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "ssm:GetParameter"
      ],
      "Resource": [
        "${aws_ssm_parameter.elasticsearch_maintenance_retention_days.arn}"
      ]
    }
  ]
}
EOB
}
