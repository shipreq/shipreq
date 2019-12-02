locals {
  shipreq_taskman_tags             = merge(local.default_tags, { Name = "${var.env}-shipreq-taskman" })
  shipreq_taskman_healthcheck_file = "/tmp/taskman.health"
  s3_config_taskman_folder         = "taskman"

  s3_config_taskman_content_hash = md5(join(":", [
    var.shipreq_taskman_properties,
  ]))
}

resource "aws_ecs_service" "shipreq_taskman" {
  name                = "${var.env}-shipreq-taskman"
  cluster             = aws_ecs_cluster.app.id
  task_definition     = aws_ecs_task_definition.shipreq_taskman.arn
  scheduling_strategy = "DAEMON"
  propagate_tags      = "SERVICE"
  tags                = local.shipreq_taskman_tags
}

resource "aws_ecs_task_definition" "shipreq_taskman" {
  family        = "${var.env}-shipreq-taskman"
  task_role_arn = aws_iam_role.shipreq_taskman.arn
  tags          = local.shipreq_taskman_tags

  container_definitions = <<EOB
[
  {
    "name": "${var.env}-shipreq-taskman",
    "image": "${data.aws_ecr_repository.taskman.repository_url}:${var.shipreq_images_tag}",
    "environment": [
      {
        "name": "SHIPREQ_INLINE_PROPERTIES",
        "value": ${jsonencode(trimspace(var.shipreq_taskman_properties))}
      },
      {
        "name": "LOG_LEVEL_ROOT",
        "value": "${var.shipreq_taskman_log_level_root}"
      },
      {
        "name": "LOG_LEVEL_SHIPREQ",
        "value": "${var.shipreq_taskman_log_level_shipreq}"
      },
      {
        "name": "db.host",
        "value": "${local.postgres_domain}"
      },
      {
        "name": "db.database",
        "value": "${var.shipreq_db_name}"
      },
      {
        "name": "db.username",
        "value": "${var.shipreq_db_username}"
      },
      {
        "name": "db.password",
        "value": "${var.shipreq_db_password}"
      },
      {
        "name": "db.schema",
        "value": "${var.shipreq_db_taskman_schema}"
      },
      {
        "name": "freshdesk.domain",
        "value": "${var.freshdesk_domain}"
      },
      {
        "name": "taskman.healthFile",
        "value": "${local.shipreq_taskman_healthcheck_file}"
      }
    ],
    "portMappings": [
      {
        "protocol": "tcp",
        "hostPort": ${local.ports.app.shipreq_taskman},
        "containerPort": 9031
      }
    ],
    "cpu": ${local.app_cluster_cpu.shipreq_taskman},
    "memoryReservation": ${local.app_cluster_mem_res.shipreq_taskman},
    "memory": 300,
    "healthCheck": {
      "command": [
        "CMD",
        "/taskman/bin/healthcheck",
        "${local.shipreq_taskman_healthcheck_file}",
        "60"
      ],
      "startPeriod": 30,
      "interval": 60,
      "timeout": 10,
      "retries": 2
    }
  }
]
EOB
}

resource "aws_iam_role" "shipreq_taskman" {
  name = "${var.env}_ecs_shipreq_taskman"
  tags = local.shipreq_taskman_tags

  assume_role_policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Effect": "Allow",
      "Principal": { "Service": "ecs-tasks.amazonaws.com" }
    }
  ]
}
EOB
}
