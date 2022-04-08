locals {
  postgres_exporter_tags = merge(local.default_tags, { Name = "${var.env}-ops-postgres_exporter" })
}

resource "aws_ecs_service" "postgres_exporter" {
  count                              = local.enable_ops_postgres_exporter ? 1 : 0
  name                               = "${var.env}-ops-postgres_exporter"
  cluster                            = aws_ecs_cluster.ops[0].id
  task_definition                    = aws_ecs_task_definition.postgres_exporter.arn
  desired_count                      = 1
  propagate_tags                     = "SERVICE"
  deployment_minimum_healthy_percent = 0 # Because port is fixed, let ECS bring down old and spin up new
  tags                               = local.postgres_exporter_tags
}

resource "aws_ecs_task_definition" "postgres_exporter" {
  family = "${var.env}-ops-postgres_exporter"
  tags   = local.postgres_exporter_tags

  container_definitions = <<EOB
[
  {
    "name": "${var.env}-ops-postgres_exporter",
    "image": "${data.aws_ecr_repository.postgres_exporter.repository_url}:${var.ops_postgres_exporter_image_tag}",
    "environment": [
      {
        "name": "DATA_SOURCE_NAME",
        "value": "postgres://${var.postgres_exporter_db_username}:${var.postgres_exporter_db_password}@${local.postgres_domain}/${var.shipreq_db_name}"
      }
    ],
    "portMappings": [
      {
        "protocol": "tcp",
        "hostPort": ${local.ports.ops.postgres_exporter},
        "containerPort": 9187
      }
    ],
    "cpu": ${local.ops_cluster_cpu.postgres_exporter},
    "memoryReservation": ${local.ops_cluster_mem_res.postgres_exporter},
    "memory": 32
  }
]
EOB
}
