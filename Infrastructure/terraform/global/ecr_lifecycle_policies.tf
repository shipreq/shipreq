locals {

  ecr_lifecycle_policy_latest4 = <<EOB
{
  "rules": [
    {
      "rulePriority": 1,
      "description": "Only keep latest 4 images",
      "selection": {
        "tagStatus": "any",
        "countType": "imageCountMoreThan",
        "countNumber": 4
      },
      "action": {
        "type": "expire"
      }
    }
  ]
}
EOB

  ecr_lifecycle_policy_latest32 = <<EOB
{
  "rules": [
    {
      "rulePriority": 1,
      "description": "Only keep latest 32 images",
      "selection": {
        "tagStatus": "any",
        "countType": "imageCountMoreThan",
        "countNumber": 32
      },
      "action": {
        "type": "expire"
      }
    }
  ]
}
EOB
}

resource "aws_ecr_lifecycle_policy" "analytics_proxy" {
  repository = aws_ecr_repository.analytics_proxy.name
  policy     = local.ecr_lifecycle_policy_latest32
}

resource "aws_ecr_lifecycle_policy" "base" {
  repository = aws_ecr_repository.base.name
  policy     = local.ecr_lifecycle_policy_latest4
}

resource "aws_ecr_lifecycle_policy" "cadvisor" {
  repository = aws_ecr_repository.cadvisor.name
  policy     = local.ecr_lifecycle_policy_latest32
}

resource "aws_ecr_lifecycle_policy" "ecs_exporter" {
  repository = aws_ecr_repository.ecs_exporter.name
  policy     = local.ecr_lifecycle_policy_latest32
}

resource "aws_ecr_lifecycle_policy" "filebeat" {
  repository = aws_ecr_repository.filebeat.name
  policy     = local.ecr_lifecycle_policy_latest32
}

resource "aws_ecr_lifecycle_policy" "grafana" {
  repository = aws_ecr_repository.grafana.name
  policy     = local.ecr_lifecycle_policy_latest32
}

resource "aws_ecr_lifecycle_policy" "nat" {
  repository = aws_ecr_repository.nat.name
  policy     = local.ecr_lifecycle_policy_latest32
}

resource "aws_ecr_lifecycle_policy" "node_exporter" {
  repository = aws_ecr_repository.node_exporter.name
  policy     = local.ecr_lifecycle_policy_latest32
}

resource "aws_ecr_lifecycle_policy" "portal" {
  repository = aws_ecr_repository.portal.name
  policy     = local.ecr_lifecycle_policy_latest32
}

resource "aws_ecr_lifecycle_policy" "postgres_exporter" {
  repository = aws_ecr_repository.postgres_exporter.name
  policy     = local.ecr_lifecycle_policy_latest32
}

resource "aws_ecr_lifecycle_policy" "prometheus-biz" {
  repository = aws_ecr_repository.prometheus-biz.name
  policy     = local.ecr_lifecycle_policy_latest32
}

resource "aws_ecr_lifecycle_policy" "prometheus-tech" {
  repository = aws_ecr_repository.prometheus-tech.name
  policy     = local.ecr_lifecycle_policy_latest32
}

resource "aws_ecr_lifecycle_policy" "squid_exporter" {
  repository = aws_ecr_repository.squid_exporter.name
  policy     = local.ecr_lifecycle_policy_latest32
}

resource "aws_ecr_lifecycle_policy" "taskman" {
  repository = aws_ecr_repository.taskman.name
  policy     = local.ecr_lifecycle_policy_latest32
}

resource "aws_ecr_lifecycle_policy" "webapp" {
  repository = aws_ecr_repository.webapp.name
  policy     = local.ecr_lifecycle_policy_latest32
}
