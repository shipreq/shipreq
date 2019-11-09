locals {
  grafana_tags = merge(local.default_tags, { Name = "${var.env}-ops-grafana" })
}

resource "aws_ecs_service" "grafana" {
  name            = "${var.env}-ops-grafana"
  cluster         = aws_ecs_cluster.ops.id
  task_definition = aws_ecs_task_definition.grafana.arn
  desired_count   = 1
  propagate_tags  = "SERVICE"
  tags            = local.grafana_tags

  # TODO health check
}

resource "aws_ecs_task_definition" "grafana" {
  family = "${var.env}-ops-grafana"
  tags   = local.grafana_tags

  container_definitions = <<EOB
[
  {
    "name": "${var.env}-ops-grafana",
    "image": "${data.aws_ecr_repository.grafana.repository_url}:${var.ops_images_tag}",
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
        "value": "${var.prometheus_tech_scrape_interval}"
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
        "hostPort": ${local.ops_cluster_ports.grafana},
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
      "startPeriod": 60,
      "interval": 60,
      "timeout": 10,
      "retries": 2
    }
  }
]
EOB
}
