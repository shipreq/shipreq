locals {
  analytics_proxy_tags           = merge(local.default_tags, { Name = "${var.env}-analytics_proxy" })
  analytics_proxy_container_name = "${var.env}-analytics_proxy"
}

resource "aws_ecs_service" "analytics_proxy" {
  name                               = "${var.env}-analytics_proxy"
  cluster                            = aws_ecs_cluster.app.id
  task_definition                    = aws_ecs_task_definition.analytics_proxy.arn
  scheduling_strategy                = "DAEMON"
  propagate_tags                     = "SERVICE"
  deployment_minimum_healthy_percent = local.app_min_healthy_percent
  health_check_grace_period_seconds  = 5
  tags                               = local.analytics_proxy_tags

  load_balancer {
    target_group_arn = aws_lb_target_group.analytics_proxy.arn
    container_name   = local.analytics_proxy_container_name
    container_port   = 80
  }
}

resource "aws_ecs_task_definition" "analytics_proxy" {
  family        = "${var.env}-analytics_proxy"
  task_role_arn = aws_iam_role.analytics_proxy.arn
  tags          = local.analytics_proxy_tags

  container_definitions = <<EOB
[
  {
    "name": "${local.analytics_proxy_container_name}",
    "image": "${data.aws_ecr_repository.analytics_proxy.repository_url}:${var.app_analytics_proxy_image_tag}",
    "portMappings": [
      {
        "protocol": "tcp",
        "hostPort": 0,
        "containerPort": 80
      }
    ],
    "cpu": ${local.app_cluster_cpu.analytics_proxy},
    "memoryReservation": ${local.app_cluster_mem_res.analytics_proxy},
    "memory": 92,
    "healthCheck": {
      "command": [
        "CMD-SHELL",
        "curl -f http://localhost/ok || exit 1"
      ],
      "startPeriod": 5,
      "interval": 30,
      "timeout": 5,
      "retries": 3
    }
  }
]
EOB
}

resource "aws_iam_role" "analytics_proxy" {
  name = "${var.env}_ecs_analytics_proxy"
  tags = local.analytics_proxy_tags

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
