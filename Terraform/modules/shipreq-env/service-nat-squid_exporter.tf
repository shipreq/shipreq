locals {
  squid_exporter_tags = merge(local.default_tags, { Name = "${var.env}-nat-squid_exporter" })
}

resource "aws_ecs_service" "squid_exporter" {
  count               = local.enable_nat_metrics ? 1 : 0
  name                = "${var.env}-nat-squid_exporter"
  cluster             = aws_ecs_cluster.nat[0].id
  task_definition     = aws_ecs_task_definition.squid_exporter.arn
  scheduling_strategy = "DAEMON"
  propagate_tags      = "SERVICE"
  tags                = local.squid_exporter_tags
}

resource "aws_ecs_task_definition" "squid_exporter" {
  family       = "${var.env}-nat-squid_exporter"
  network_mode = "host"
  tags         = local.squid_exporter_tags

  container_definitions = <<EOB
[
  {
    "name": "${var.env}-nat-squid_exporter",
    "image": "${data.aws_ecr_repository.squid_exporter.repository_url}:${var.nat_squid_exporter_image_tag}",
    "command": [
      "-listen", "0.0.0.0:${local.ports.nat.squid_exporter}"
    ],
    "cpu": ${local.nat_cluster_cpu.squid_exporter},
    "memoryReservation": ${local.nat_cluster_mem_res.squid_exporter},
    "memory": 64
  }
]
EOB
}
