locals {
  prometheus_biz_tags = merge(local.default_tags, { Name = "${var.env}-prometheus-biz" })

  prometheus_biz_config_yml = templatefile("${path.module}/service-ops-prometheus-biz.yml", {
    OPS_HOST                       = local.ops_host
    POSTGRES_EXPORTER_PORT         = local.ports.ops.postgres_exporter
    PROMETHEUS_BIZ_SCRAPE_INTERVAL = var.prometheus_biz_scrape_interval
  })
}

resource "aws_ecs_service" "prometheus_biz" {
  name                               = "${var.env}-ops-prometheus-biz"
  cluster                            = aws_ecs_cluster.ops.id
  task_definition                    = aws_ecs_task_definition.prometheus_biz.arn
  desired_count                      = 1
  propagate_tags                     = "SERVICE"
  deployment_minimum_healthy_percent = 0 # Because port is fixed, let ECS bring down old and spin up new
  tags                               = local.prometheus_biz_tags
}

resource "aws_ecs_task_definition" "prometheus_biz" {
  family = "${var.env}-ops-prometheus-biz"
  tags   = local.prometheus_biz_tags

  volume {
    name      = "data"
    host_path = module.ecs_ebs_prometheus_biz.mount_dir
  }

  container_definitions = <<EOB
[
  {
    "name": "${var.env}-ops-prometheus-biz",
    "image": "${data.aws_ecr_repository.prometheus-biz.repository_url}:${var.prometheus_biz_image_tag}",
    "logConfiguration": {
      "logDriver": "json-file"
    },
    "environment": [
      {
        "name": "CONFIG",
        "value": ${jsonencode(local.prometheus_biz_config_yml)}
      }
    ],
    "command": [
      "--storage.tsdb.retention.${var.prometheus_biz_data_retention}"
    ],
    "mountPoints": [
      {
        "sourceVolume": "data",
        "containerPath": "/data",
        "readOnly": false
      }
    ],
    "portMappings": [
      {
        "protocol": "tcp",
        "hostPort": ${local.ports.ops.prometheus_biz},
        "containerPort": 9091
      }
    ],
    "cpu": ${local.ops_cluster_cpu.prometheus_biz},
    "memoryReservation": ${local.ops_cluster_mem_res.prometheus_biz},
    "healthCheck": {
      "command": [
        "CMD-SHELL",
        "wget -qO - localhost:9091${local.prometheus_biz_path}/-/healthy || exit 1"
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
