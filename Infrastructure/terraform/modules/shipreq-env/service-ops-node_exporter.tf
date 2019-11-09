locals {
  node_exporter_tags = merge(local.default_tags, { Name = "${var.env}-ops-node_exporter" })
}

resource "aws_ecs_service" "node_exporter" {
  name                = "${var.env}-ops-node_exporter"
  cluster             = aws_ecs_cluster.ops.id
  task_definition     = aws_ecs_task_definition.node_exporter.arn
  scheduling_strategy = "DAEMON"
  propagate_tags      = "SERVICE"
  tags                = local.node_exporter_tags

  # TODO health check
}

resource "aws_ecs_task_definition" "node_exporter" {
  family = "${var.env}-ops-node_exporter"
  tags   = local.node_exporter_tags

  container_definitions = <<EOB
[
  {
    "name": "${var.env}-ops-node_exporter",
    "image": "${data.aws_ecr_repository.node_exporter.repository_url}:${var.ops_images_tag}",
    "privileged": true,
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
        "hostPort": ${local.ops_cluster_ports.node_exporter},
        "containerPort": 9100
      }
    ],
    "cpu": ${local.ops_cluster_cpu.node_exporter},
    "memoryReservation": ${local.ops_cluster_mem_res.node_exporter},
    "memory": 92,
    "healthCheck": {
      "command": [
        "CMD-SHELL",
        "wget -qO - http://localhost:9100/metrics | fgrep -q '\"} ' || exit 1"
      ],
      "startPeriod": 60,
      "interval": 60,
      "timeout": 10,
      "retries": 2
    }
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
