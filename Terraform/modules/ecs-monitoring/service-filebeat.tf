locals {
  filebeat_tags = merge(var.default_tags, { Name = "${var.name_prefix}-filebeat" })
}

resource "aws_ecs_service" "filebeat" {
  name                = "${var.name_prefix}-filebeat"
  cluster             = var.cluster_id
  task_definition     = aws_ecs_task_definition.filebeat.arn
  scheduling_strategy = "DAEMON"
  propagate_tags      = "SERVICE"
  tags                = local.filebeat_tags
}

resource "aws_ecs_task_definition" "filebeat" {
  family       = "${var.name_prefix}-filebeat"
  network_mode = var.filebeat_network_mode
  tags         = local.filebeat_tags

  container_definitions = <<EOB
[
  {
    "name": "${var.name_prefix}-filebeat",
    "image": "${var.filebeat_image}",
    "environment": [
      {
        "name": "CLUSTER",
        "value": "${var.cluster_log_name}"
      },
      {
        "name": "ES_HOSTS",
        "value": "${var.filebeat_es_hosts}"
      }
    ],
    "mountPoints": [
      {
        "sourceVolume": "docker_containers",
        "containerPath": "/var/lib/docker/containers",
        "readOnly": true
      },
      {
        "sourceVolume": "docker_sock",
        "containerPath": "/var/run/docker.sock",
        "readOnly": true
      },
      {
        "sourceVolume": "host_var_log",
        "containerPath": "/host/var/log",
        "readOnly": true
      }
    ],
    "cpu": ${var.filebeat_cpu},
    "memoryReservation": ${var.filebeat_mem_res},
    "memory": 128
  }
]
EOB

  volume {
    name      = "docker_containers"
    host_path = "/var/lib/docker/containers"
  }

  volume {
    name      = "docker_sock"
    host_path = "/var/run/docker.sock"
  }

  volume {
    name      = "host_var_log"
    host_path = "/var/log"
  }
}
