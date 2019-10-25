locals {
  prometheus_tags = merge(local.default_tags, { Name = "${var.env}-prometheus" })
}

resource "aws_service_discovery_service" "prometheus" {
  name = local.prometheus_subdomain

  dns_config {
    namespace_id   = aws_service_discovery_private_dns_namespace.internal.id
    routing_policy = "MULTIVALUE"

    dns_records {
      ttl  = 30
      type = "A"
    }
  }

  health_check_custom_config {
    failure_threshold = 1
  }
}

resource "aws_ecs_service" "prometheus" {
  name            = "${var.env}-ops-prometheus"
  cluster         = aws_ecs_cluster.ops.id
  task_definition = aws_ecs_task_definition.prometheus.arn
  desired_count   = 1
  propagate_tags  = "SERVICE"
  tags            = local.prometheus_tags

  service_registries {
    registry_arn = aws_service_discovery_service.prometheus.arn
  }

  network_configuration {
    subnets         = [aws_subnet.private.id]
    security_groups = [aws_security_group.prometheus.id]
  }

  # TODO health check
}

resource "aws_ecs_task_definition" "prometheus" {
  family        = "${var.env}-ops-prometheus"
  network_mode  = "awsvpc"
  task_role_arn = aws_iam_role.prometheus-task.arn
  tags          = local.prometheus_tags

  container_definitions = <<EOB
[
  {
    "name": "${var.env}-ops-prometheus",
    "image": "prom/prometheus:v2.13.0",
    "command": [
      "--config.file=/etc/prometheus/prometheus.yml",
      "--web.external-url=http://internal:9090/prometheus/"
    ],
    "cpu": 1536,
    "memoryReservation": 64,
    "portMappings": [
      {
        "hostPort": ${local.prometheus_port},
        "containerPort": 9090
      }
    ]
  }
]
EOB

  # volume {
  #   name      = "service-storage"
  #   host_path = "/ecs/service-storage"
  # }
}

module "ecs_ebs_prometheus" {
  source   = "../ecs-ebs"
  name     = "${var.env}-ops-prometheus"
  size     = 1
  ec2_role = aws_iam_role.ops-ecs
  tags     = local.prometheus_tags

  manifest = [
    {
      availability_zone = var.availability_zone
      count             = 1
    }
  ]
}

resource "aws_security_group" "prometheus" {
  name   = "sg_${var.env}_ops_prometheus"
  vpc_id = aws_vpc.main.id
  tags   = local.prometheus_tags

  ingress {
    protocol        = "tcp"
    from_port       = 9090
    to_port         = 9090
    security_groups = [aws_security_group.bastion.id]
  }

  egress {
    protocol    = "tcp"
    from_port   = 0
    to_port     = 0
    cidr_blocks = [aws_subnet.private.cidr_block]
  }
}

resource "aws_iam_role" "prometheus-task" {
  name = "${var.env}_ops_prometheus_task_role"

  assume_role_policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
      {
          "Action": "sts:AssumeRole",
          "Principal": {
             "Service": "ecs-tasks.amazonaws.com"
          },
          "Effect": "Allow",
          "Sid": ""
      }
  ]
}
EOB
}
