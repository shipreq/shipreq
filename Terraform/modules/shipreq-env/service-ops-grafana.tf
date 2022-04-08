locals {
  grafana_tags = merge(local.default_tags, { Name = "${var.env}-ops-grafana" })
}

resource "aws_ecs_service" "grafana" {
  count                              = local.enable_ops_grafana ? 1 : 0
  name                               = "${var.env}-ops-grafana"
  cluster                            = aws_ecs_cluster.ops[0].id
  task_definition                    = aws_ecs_task_definition.grafana.arn
  desired_count                      = 1
  propagate_tags                     = "SERVICE"
  deployment_minimum_healthy_percent = 0 # Because port is fixed, let ECS bring down old and spin up new
  tags                               = local.grafana_tags
}

resource "aws_ecs_task_definition" "grafana" {
  family = "${var.env}-ops-grafana"
  tags   = local.grafana_tags

  container_definitions = <<EOB
[
  {
    "name": "${var.env}-ops-grafana",
    "image": "${data.aws_ecr_repository.grafana.repository_url}:${var.ops_grafana_image_tag}",
    "environment": [
      {
        "name": "GF_DATABASE_URL",
        "value": "postgres://${var.grafana_db_username}:${var.grafana_db_password}@${local.postgres_domain}/${var.grafana_db_name}"
      },
      {
        "name": "PROMETHEUS_TECH_URL",
        "value": "${local.prometheus_tech_url}"
      },
      {
        "name": "PROMETHEUS_TECH_SCRAPE_INTERVAL",
        "value": "${var.prometheus_tech_scrape_interval_sec}s"
      },
      {
        "name": "PROMETHEUS_BIZ_URL",
        "value": "${local.prometheus_biz_url}"
      },
      {
        "name": "PROMETHEUS_BIZ_SCRAPE_INTERVAL",
        "value": "${var.prometheus_biz_scrape_interval}"
      }
    ],
    "portMappings": [
      {
        "protocol": "tcp",
        "hostPort": ${local.ports.ops.grafana},
        "containerPort": 3000
      }
    ],
    "cpu": ${local.ops_cluster_cpu.grafana},
    "memoryReservation": ${local.ops_cluster_mem_res.grafana},
    "healthCheck": {
      "command": [
        "CMD-SHELL",
        "curl -f http://localhost:3000${local.grafana_path}/api/health || exit 1"
      ],
      "startPeriod": ${local.ops_healthcheck.startPeriod},
      "interval": ${local.ops_healthcheck.interval},
      "timeout": ${local.ops_healthcheck.timeout},
      "retries": ${local.ops_healthcheck.retries}
    }
  }
]
EOB
}
