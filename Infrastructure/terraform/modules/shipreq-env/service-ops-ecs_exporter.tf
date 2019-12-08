locals {
  ecs_exporter_tags = merge(local.default_tags, { Name = "${var.env}-ops-ecs_exporter" })
}

resource "aws_ecs_service" "ecs_exporter" {
  name                = "${var.env}-ops-ecs_exporter"
  cluster             = aws_ecs_cluster.ops.id
  task_definition     = aws_ecs_task_definition.ecs_exporter.arn
  scheduling_strategy = "DAEMON"
  propagate_tags      = "SERVICE"
  tags                = local.ecs_exporter_tags
}

resource "aws_ecs_task_definition" "ecs_exporter" {
  family = "${var.env}-ops-ecs_exporter"
  tags   = local.ecs_exporter_tags

  container_definitions = <<EOB
[
  {
    "name": "${var.env}-ops-ecs_exporter",
    "image": "${data.aws_ecr_repository.ecs_exporter.repository_url}:${var.ecs_exporter_image_tag}",
    "command": [
      "-aws.region=${local.region}",
      "-aws.cluster-filter=${var.env}-.*"
    ],
    "portMappings": [
      {
        "protocol": "tcp",
        "hostPort": ${local.ports.ops.ecs_exporter},
        "containerPort": 9222
      }
    ],
    "cpu": ${local.ops_cluster_cpu.ecs_exporter},
    "memoryReservation": ${local.ops_cluster_mem_res.ecs_exporter},
    "memory": 48
  }
]
EOB
}

resource "aws_iam_policy" "ecs_exporter" {
  name = "${var.env}_ops_ecs_exporter_policy"

  policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "ecs:DescribeClusters",
        "ecs:DescribeContainerInstances",
        "ecs:DescribeServices",
        "ecs:ListClusters",
        "ecs:ListContainerInstances",
        "ecs:ListServices"
      ],
      "Resource": "*"
    }
  ]
}
EOB
}

resource "aws_iam_role_policy_attachment" "ops-ecs-monitoring" {
  role       = aws_iam_role.ops-ecs.id
  policy_arn = aws_iam_policy.ecs_exporter.arn
}
