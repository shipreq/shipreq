locals {
  node_exporter_tags = merge(var.default_tags, { Name = "${var.name_prefix}-node_exporter" })

  node_exporter_args = [
    "--path.rootfs=/rootfs",
    "--path.procfs=/host/proc",
    "--path.sysfs=/host/sys"
  ]
}

resource "aws_ecs_service" "node_exporter" {
  name                = "${var.name_prefix}-node_exporter"
  cluster             = var.cluster_id
  task_definition     = aws_ecs_task_definition.node_exporter.arn
  scheduling_strategy = "DAEMON"
  propagate_tags      = "SERVICE"
  tags                = local.node_exporter_tags
}

resource "aws_ecs_task_definition" "node_exporter" {
  family       = "${var.name_prefix}-node_exporter"
  network_mode = "host"
  pid_mode     = "host"
  tags         = local.node_exporter_tags

  container_definitions = <<EOB
[
  {
    "name": "${var.name_prefix}-node_exporter",
    "image": "${var.node_exporter_image}",
    "privileged": true,
    "command": ${jsonencode(local.node_exporter_args)},
    "mountPoints": [
      {
        "sourceVolume": "rootfs",
        "containerPath": "/rootfs",
        "readOnly": true
      },
      {
        "sourceVolume": "proc",
        "containerPath": "/host/proc",
        "readOnly": true
      },
      {
        "sourceVolume": "sys",
        "containerPath": "/host/sys",
        "readOnly": true
      }
    ],
    "portMappings": [
      {
        "protocol": "tcp",
        "hostPort": ${var.node_exporter_port},
        "containerPort": 9100
      }
    ],
    "cpu": ${var.node_exporter_cpu},
    "memoryReservation": ${var.node_exporter_mem_res},
    "memory": 92
  }
]
EOB

  volume {
    name      = "rootfs"
    host_path = "/"
  }

  volume {
    name      = "proc"
    host_path = "/proc"
  }

  volume {
    name      = "sys"
    host_path = "/sys"
  }
}
