locals {
  prometheus_biz_tags = merge(local.default_tags, { Name = "${var.env}-prometheus-biz" })
}

resource "aws_ecs_service" "prometheus_biz" {
  name            = "${var.env}-ops-prometheus-biz"
  cluster         = aws_ecs_cluster.ops.id
  task_definition = aws_ecs_task_definition.prometheus_biz.arn
  desired_count   = 1
  propagate_tags  = "SERVICE"
  tags            = local.prometheus_biz_tags

  # TODO health check
}

resource "aws_ecs_task_definition" "prometheus_biz" {
  family        = "${var.env}-ops-prometheus-biz"
  task_role_arn = aws_iam_role.prometheus_biz_task.arn
  tags          = local.prometheus_biz_tags

  volume {
    name      = "data"
    host_path = module.ecs_ebs_prometheus_biz.mount_dir
  }

  container_definitions = <<EOB
[
  {
    "name": "${var.env}-ops-prometheus-biz",
    "image": "${data.aws_ecr_repository.prometheus-biz.repository_url}:${var.ops_images_tag}",
    "logConfiguration": {
      "logDriver": "json-file"
    },
    "environment": [
      {
        "name": "CONFIG",
        "value": ${jsonencode(templatefile("${path.module}/service-ops-prometheus-biz.yml", {}))}
      }
    ],
    "command": [
      "--storage.tsdb.retention.${var.prometheus_biz_retention}"
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
        "hostPort": ${local.prometheus_biz_port},
        "containerPort": 9091
      }
    ],
    "cpu": ${local.ops_cluster_cpu.prometheus_biz},
    "memoryReservation": ${local.ops_cluster_mem_res.prometheus_biz}
  }
]
EOB
}

module "ecs_ebs_prometheus_biz" {
  source      = "../ecs-ebs"
  name        = "${var.env}-ops-prometheus-biz"
  size        = var.prometheus_biz_ebs_size
  ec2_role    = aws_iam_role.ops-ecs
  device_path = local.ops_device.prometheus_biz
  tags        = local.prometheus_biz_tags

  manifest = [
    {
      availability_zone = var.availability_zone
      count             = 1
    }
  ]
}

resource "aws_iam_role" "prometheus_biz_task" {
  name = "${var.env}_ops_prometheus_biz_task_role"

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
