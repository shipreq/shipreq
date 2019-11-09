locals {
  filebeat_tags = merge(local.default_tags, { Name = "${var.env}-ops-filebeat" })
}

resource "aws_ecs_service" "filebeat" {
  name                = "${var.env}-ops-filebeat"
  cluster             = aws_ecs_cluster.ops.id
  task_definition     = aws_ecs_task_definition.filebeat.arn
  scheduling_strategy = "DAEMON"
  propagate_tags      = "SERVICE"
  tags                = local.filebeat_tags
}

resource "aws_ecs_task_definition" "filebeat" {
  family = "${var.env}-ops-filebeat"
  tags   = local.filebeat_tags

  container_definitions = <<EOB
[
  {
    "name": "${var.env}-ops-filebeat",
    "image": "${data.aws_ecr_repository.filebeat.repository_url}:${var.ops_images_tag}",
    "environment": [
      {
        "name": "ES_HOSTS",
        "value": "${local.es_root_url_with_port}"
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
    "cpu": ${local.ops_cluster_cpu.filebeat},
    "memoryReservation": ${local.ops_cluster_mem_res.filebeat},
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
