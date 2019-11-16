locals {
  shipreq_webapp_tags           = merge(local.default_tags, { Name = "${var.env}-shipreq-webapp" })
  shipreq_webapp_container_name = "${var.env}-shipreq-webapp"
  s3_config_webapp_folder       = "webapp"

  s3_config_webapp_content_hash = md5(join(":", [
    var.shipreq_webapp_properties,
    var.shipreq_webapp_logback_xml,
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
}

resource "aws_ecs_service" "shipreq_webapp" {
  name                = "${var.env}-shipreq-webapp"
  cluster             = aws_ecs_cluster.app.id
  task_definition     = aws_ecs_task_definition.shipreq_webapp.arn
  scheduling_strategy = "DAEMON"
  propagate_tags      = "SERVICE"
  tags                = local.shipreq_webapp_tags

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

  # Ensure that S3 is updated before we allow tasks to start
  depends_on = [
    aws_s3_bucket_object.webapp_properties,
    aws_s3_bucket_object.webapp_logback,
  ]
}

resource "aws_ecs_task_definition" "shipreq_webapp" {
  family        = "${var.env}-shipreq-webapp"
  task_role_arn = aws_iam_role.shipreq_webapp.arn
  tags          = local.shipreq_webapp_tags

  container_definitions = <<EOB
[
  {
    "name": "${local.shipreq_webapp_container_name}",
    "image": "${data.aws_ecr_repository.webapp.repository_url}:${var.shipreq_images_tag}",
    "environment": [
      {
        "name": "S3_CONTENT_HASH",
        "value": "${local.s3_config_webapp_content_hash}"
      },
      {
        "name": "IMPORT_S3",
        "value": "s3://${aws_s3_bucket.config.bucket}/${local.s3_config_webapp_folder}"
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

resource "aws_s3_bucket_object" "webapp_properties" {
  bucket  = aws_s3_bucket.config.bucket
  key     = "${local.s3_config_webapp_folder}/resources/shipreq.properties"
  content = var.shipreq_webapp_properties
}

resource "aws_s3_bucket_object" "webapp_logback" {
  bucket  = aws_s3_bucket.config.bucket
  key     = "${local.s3_config_webapp_folder}/resources/logback.xml"
  content = var.shipreq_webapp_logback_xml
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

resource "aws_iam_role_policy_attachment" "shipreq_webapp_s3_config" {
  role       = aws_iam_role.shipreq_webapp.name
  policy_arn = aws_iam_policy.s3_config_ro.arn
}
