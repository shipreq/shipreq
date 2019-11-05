locals {
  prometheus_tech_tags = merge(local.default_tags, { Name = "${var.env}-prometheus-tech" })
}

resource "aws_service_discovery_service" "prometheus_tech" {
  name = local.prometheus_tech_subdomain

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

resource "aws_ecs_service" "prometheus_tech" {
  name            = "${var.env}-ops-prometheus-tech"
  cluster         = aws_ecs_cluster.ops.id
  task_definition = aws_ecs_task_definition.prometheus_tech.arn
  desired_count   = 1
  propagate_tags  = "SERVICE"
  tags            = local.prometheus_tech_tags

  service_registries {
    registry_arn = aws_service_discovery_service.prometheus_tech.arn
  }

  network_configuration {
    subnets         = [aws_subnet.private.id]
    security_groups = [aws_security_group.prometheus_tech.id]
  }

  # TODO health check
}

resource "aws_ecs_task_definition" "prometheus_tech" {
  family        = "${var.env}-ops-prometheus-tech"
  network_mode  = "awsvpc"
  task_role_arn = aws_iam_role.prometheus_tech_task.arn
  tags          = local.prometheus_tech_tags

  volume {
    name      = "data"
    host_path = module.ecs_ebs_prometheus_tech.mount_dir
  }

  container_definitions = <<EOB
[
  {
    "name": "${var.env}-ops-prometheus-tech",
    "image": "${data.aws_ecr_repository.prometheus-tech.repository_url}:${var.ops_images_tag}",
    "logConfiguration": {
      "logDriver": "json-file"
    },
    "environment": [
      {
        "name": "CONFIG",
        "value": ${jsonencode(templatefile("${path.module}/service-ops-prometheus-tech.yml", {}))}
      }
    ],
    "command": [
      "--storage.tsdb.retention.${var.prometheus_tech_retention}"
    ],
    "mountPoints": [
      {
        "containerPath": "/data",
        "sourceVolume": "data",
        "readOnly": false
      }
    ],
    "portMappings": [
      {
        "protocol": "tcp",
        "hostPort": ${local.prometheus_tech_port},
        "containerPort": 9090
      }
    ],
    "cpu": ${local.ops_cluster_cpu.prometheus_tech},
    "memoryReservation": ${local.ops_cluster_mem_res.prometheus_tech}
  }
]
EOB
}

module "ecs_ebs_prometheus_tech" {
  source      = "../ecs-ebs"
  name        = "${var.env}-ops-prometheus-tech"
  size        = var.prometheus_tech_ebs_size
  ec2_role    = aws_iam_role.ops-ecs
  device_path = local.ops_device.prometheus_tech
  tags        = local.prometheus_tech_tags

  manifest = [
    {
      availability_zone = var.availability_zone
      count             = 1
    }
  ]
}

resource "aws_security_group" "prometheus_tech" {
  name   = "sg_${var.env}_ops_prometheus_tech"
  vpc_id = aws_vpc.main.id
  tags   = local.prometheus_tech_tags

  ingress {
    protocol        = "tcp"
    from_port       = local.prometheus_tech_port
    to_port         = local.prometheus_tech_port
    security_groups = [aws_security_group.bastion.id]
  }

  egress {
    protocol    = "tcp"
    from_port   = 0
    to_port     = 0
    cidr_blocks = [aws_subnet.private.cidr_block]
  }
}

resource "aws_iam_role" "prometheus_tech_task" {
  name = "${var.env}_ops_prometheus_tech_task_role"

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
