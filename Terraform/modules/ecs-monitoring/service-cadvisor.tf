locals {
  cadvisor_tags = merge(var.default_tags, { Name = "${var.name_prefix}-cadvisor" })
}

resource "aws_ecs_service" "cadvisor" {
  count               = local.cadvisor_enabled ? 1 : 0
  name                = "${var.name_prefix}-cadvisor"
  cluster             = var.cluster_id
  task_definition     = aws_ecs_task_definition.cadvisor.arn
  scheduling_strategy = "DAEMON"
  propagate_tags      = "SERVICE"
  tags                = local.cadvisor_tags
}

resource "aws_ecs_task_definition" "cadvisor" {
  family = "${var.name_prefix}-cadvisor"
  tags   = local.cadvisor_tags

  container_definitions = <<EOB
[
  {
    "name": "${var.name_prefix}-cadvisor",
    "image": "${var.cadvisor_image}",
    "privileged": true,
    "mountPoints": [
      {
        "sourceVolume": "rootfs",
        "containerPath": "/rootfs",
        "readOnly": true
      },
      {
        "sourceVolume": "dev_disk",
        "containerPath": "/dev/disk",
        "readOnly": true
      },
      {
        "sourceVolume": "sys",
        "containerPath": "/sys",
        "readOnly": true
      },
      {
        "sourceVolume": "var_lib_docker",
        "containerPath": "/var/lib/docker",
        "readOnly": true
      },
      {
        "sourceVolume": "var_run",
        "containerPath": "/var/run",
        "readOnly": true
      }
    ],
    "portMappings": [
      {
        "protocol": "tcp",
        "hostPort": ${var.cadvisor_port},
        "containerPort": 8080
      }
    ],
    "cpu": ${var.cadvisor_cpu},
    "memoryReservation": ${var.cadvisor_mem_res},
    "memory": 128,
    "healthCheck": {
      "command": [
        "CMD-SHELL",
        "wget --quiet --tries=1 --spider http://localhost:8080${var.cadvisor_path}/healthz || exit 1"
      ],
      "startPeriod": ${local.healthcheck.startPeriod},
      "interval": ${local.healthcheck.interval},
      "timeout": ${local.healthcheck.timeout},
      "retries": ${local.healthcheck.retries}
    }
  }
]
EOB

  volume {
    name      = "rootfs"
    host_path = "/"
  }

  volume {
    name      = "dev_disk"
    host_path = "/dev/disk"
  }

  volume {
    name      = "sys"
    host_path = "/sys"
  }

  volume {
    name      = "var_lib_docker"
    host_path = "/var/lib/docker"
  }

  volume {
    name      = "var_run"
    host_path = "/var/run"
  }
}
