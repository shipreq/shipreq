locals {
  nat_service_tags = merge(local.default_tags, { Name = "${var.env}-nat" })
}

resource "aws_ecs_service" "nat" {
  name                = "${var.env}-nat"
  cluster             = aws_ecs_cluster.nat.id
  task_definition     = aws_ecs_task_definition.nat.arn
  scheduling_strategy = "DAEMON"
  propagate_tags      = "SERVICE"
  tags                = local.nat_service_tags
}

resource "aws_ecs_task_definition" "nat" {
  family       = "${var.env}-nat"
  network_mode = "host"
  tags         = local.nat_service_tags

  container_definitions = <<EOB
[
  {
    "name": "${var.env}-nat",
    "image": "${data.aws_ecr_repository.nat.repository_url}:${var.nat_image_tag}",
    "cpu": ${local.nat_cluster_cpu.nat},
    "memoryReservation": ${local.nat_cluster_mem_res.nat},
    "healthCheck": {
      "command": [ "CMD", "/healthcheck" ],
      "startPeriod": 15,
      "interval": 60,
      "timeout": 10,
      "retries": 2
    }
  }
]
EOB
}
