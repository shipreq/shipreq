locals {
  cadvisor_tags = merge(local.default_tags, { Name = "${var.env}-ops-cadvisor" })
}

resource "aws_ecs_service" "cadvisor" {
  name                = "${var.env}-ops-cadvisor"
  cluster             = aws_ecs_cluster.ops.id
  task_definition     = aws_ecs_task_definition.cadvisor.arn
  scheduling_strategy = "DAEMON"
  propagate_tags      = "SERVICE"
  tags                = local.cadvisor_tags

  # TODO health check
}

resource "aws_ecs_task_definition" "cadvisor" {
  family = "${var.env}-ops-cadvisor"
  tags   = local.cadvisor_tags

  container_definitions = <<EOB
[
  {
    "name": "${var.env}-ops-cadvisor",
    "image": "${data.aws_ecr_repository.cadvisor.repository_url}:${var.ops_images_tag}",
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
        "hostPort": ${local.ops_cluster_ports.cadvisor},
        "containerPort": 8080
      }
    ],
    "cpu": ${local.ops_cluster_cpu.cadvisor},
    "memoryReservation": ${local.ops_cluster_mem_res.cadvisor},
    "memory": 128,
    "healthCheck": {
      "command": [
        "CMD-SHELL",
        "wget --quiet --tries=1 --spider http://localhost:8080${local.ops_cadvisor_path}/healthz || exit 1"
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
