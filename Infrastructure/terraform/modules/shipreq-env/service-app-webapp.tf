locals {
  shipreq_webapp_tags           = merge(local.default_tags, { Name = "${var.env}-shipreq-webapp" })
  shipreq_webapp_container_name = "${var.env}-shipreq-webapp"
  s3_config_webapp_folder       = "webapp"

  s3_config_webapp_content_hash = md5(join(":", [
    var.shipreq_webapp_properties,
  ]))
}

resource "aws_service_discovery_service" "webapp" {
  name = local.shipreq_webapp_sd_subdomain

  dns_config {
    namespace_id   = aws_service_discovery_private_dns_namespace.internal.id
    routing_policy = "MULTIVALUE"

    dns_records {
      ttl  = 30
      type = "SRV"
    }
  }

  health_check_custom_config {
    failure_threshold = 2
  }

  # Remove after https://github.com/terraform-providers/terraform-provider-aws/issues/4853 is resolved
  provisioner "local-exec" {
    when    = destroy
    command = "${path.module}/../ec2-sd/servicediscovery-drain.sh ${aws_service_discovery_service.webapp.id}"
  }
}

resource "aws_ecs_service" "shipreq_webapp" {
  count                              = var.enable_db_dependant_services ? 1 : 0
  name                               = "${var.env}-shipreq-webapp"
  cluster                            = aws_ecs_cluster.app.id
  task_definition                    = aws_ecs_task_definition.shipreq_webapp.arn
  scheduling_strategy                = "DAEMON"
  propagate_tags                     = "SERVICE"
  deployment_minimum_healthy_percent = local.app_min_healthy_percent
  health_check_grace_period_seconds  = 40
  tags                               = local.shipreq_webapp_tags

  load_balancer {
    target_group_arn = aws_lb_target_group.webapp.arn
    container_name   = local.shipreq_webapp_container_name
    container_port   = 8080
  }

  service_registries {
    registry_arn   = aws_service_discovery_service.webapp.arn
    container_name = local.shipreq_webapp_container_name
    container_port = 8080
  }
}

resource "aws_ecs_task_definition" "shipreq_webapp" {
  family        = "${var.env}-shipreq-webapp"
  task_role_arn = aws_iam_role.shipreq_webapp.arn
  tags          = local.shipreq_webapp_tags

  container_definitions = <<EOB
[
  {
    "name": "${local.shipreq_webapp_container_name}",
    "image": "${data.aws_ecr_repository.webapp.repository_url}:${var.app_shipreq_images_tag}",
    "environment": [
      {
        "name": "SHIPREQ_INLINE_PROPERTIES",
        "value": ${jsonencode(trimspace(var.shipreq_webapp_properties))}
      },
      {
        "name": "LOG_LEVEL_ROOT",
        "value": "${var.shipreq_webapp_log_level_root}"
      },
      {
        "name": "LOG_LEVEL_SHIPREQ",
        "value": "${var.shipreq_webapp_log_level_shipreq}"
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
        "name": "redis.url",
        "value": "redis://${local.redis_domain}:6379"
      },
      {
        "name": "shipreq.googleAnalytics.trackingId",
        "value": "${var.shipreq_webapp_google_analytics_id}"
      },
      {
        "name": "shipreq.taskman.schema",
        "value": "${var.shipreq_db_taskman_schema}"
      },
      {
        "name": "shipreq.url",
        "value": "${local.shipreq_url}"
      },
      {
        "name": "run.mode",
        "value": "production"
      }
    ],
    "portMappings": [
      {
        "protocol": "tcp",
        "hostPort": 0,
        "containerPort": 8080
      }
    ],
    "cpu": ${local.app_cluster_cpu.shipreq_webapp},
    "memoryReservation": ${local.app_cluster_mem_res.shipreq_webapp},
    "healthCheck": {
      "command": [
        "CMD-SHELL",
        "curl -f http://localhost:8080/ops/ok || exit 1"
      ],
      "startPeriod": 90,
      "interval": 60,
      "timeout": 10,
      "retries": 3
    }
  }
]
EOB
}

resource "aws_iam_role" "shipreq_webapp" {
  name = "${var.env}_ecs_shipreq_webapp"
  tags = local.shipreq_webapp_tags

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
