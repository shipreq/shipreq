locals {
  prometheus_tech_tags = merge(local.default_tags, { Name = "${var.env}-prometheus-tech" })

  prometheus_tech_config_yml = templatefile("${path.module}/service-ops-prometheus-tech.yml", {
    APP_HOST                        = local.app_host
    CADVISOR_PATH                   = local.cadvisor_path
    CADVISOR_PORT                   = local.ports.cadvisor
    ECS_EXPORTER_PORT               = local.ports.ops.ecs_exporter
    ECS_EXPORTER_SCRAPE_INTERVAL    = "${max(60, var.prometheus_tech_scrape_interval_sec)}s"
    NAT_HOST                        = local.nat_domain
    NODE_EXPORTER_PORT              = local.ports.node_exporter
    OPS_HOST                        = local.ops_host
    PROMETHEUS_BIZ_HOST             = local.prometheus_biz_host
    PROMETHEUS_BIZ_PATH             = local.prometheus_biz_path
    PROMETHEUS_BIZ_PORT             = local.ports.ops.prometheus_biz
    PROMETHEUS_TECH_HOST            = local.prometheus_tech_host
    PROMETHEUS_TECH_PATH            = local.prometheus_tech_path
    PROMETHEUS_TECH_PORT            = local.ports.ops.prometheus_tech
    PROMETHEUS_TECH_SCRAPE_INTERVAL = "${var.prometheus_tech_scrape_interval_sec}s"
    SHIPREQ_TASKMAN_PORT            = local.ports.app.shipreq_taskman
    SHIPREQ_WEBAPP_SD_DOMAIN        = local.shipreq_webapp_sd_domain
    SQUID_EXPORTER_PORT             = local.ports.nat.squid_exporter
  })
}

resource "aws_ecs_service" "prometheus_tech" {
  name                               = "${var.env}-ops-prometheus-tech"
  cluster                            = aws_ecs_cluster.ops.id
  task_definition                    = aws_ecs_task_definition.prometheus_tech.arn
  desired_count                      = 1
  propagate_tags                     = "SERVICE"
  deployment_minimum_healthy_percent = 0 # Because port is fixed, les ECS bring down old and spin up new
  tags                               = local.prometheus_tech_tags
}

resource "aws_ecs_task_definition" "prometheus_tech" {
  family = "${var.env}-ops-prometheus-tech"
  tags   = local.prometheus_tech_tags

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
        "value": ${jsonencode(local.prometheus_tech_config_yml)}
      }
    ],
    "command": [
      "--storage.tsdb.retention.${var.prometheus_tech_retention}"
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
        "hostPort": ${local.ports.ops.prometheus_tech},
        "containerPort": 9090
      }
    ],
    "cpu": ${local.ops_cluster_cpu.prometheus_tech},
    "memoryReservation": ${local.ops_cluster_mem_res.prometheus_tech},
    "healthCheck": {
      "command": [
        "CMD-SHELL",
        "wget -qO - localhost:9090${local.prometheus_tech_path}/-/healthy || exit 1"
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
