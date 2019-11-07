resource "aws_ecr_repository" "base" {
  name = "shipreq/base"
  tags = local.default_tags
}

resource "aws_ecr_repository" "taskman" {
  name = "shipreq/taskman"
  tags = local.default_tags
}

resource "aws_ecr_repository" "webapp" {
  name = "shipreq/webapp"
  tags = local.default_tags
}

resource "aws_ecr_repository" "portal" {
  name = "shipreq/ops/portal"
  tags = local.default_tags
}

resource "aws_ecr_repository" "grafana" {
  name = "shipreq/ops/grafana"
  tags = local.default_tags
}

resource "aws_ecr_repository" "prometheus-tech" {
  name = "shipreq/ops/prometheus-tech"
  tags = local.default_tags
}

resource "aws_ecr_repository" "prometheus-biz" {
  name = "shipreq/ops/prometheus-biz"
  tags = local.default_tags
}

resource "aws_ecr_repository" "node_exporter" {
  name = "shipreq/ops/node_exporter"
  tags = local.default_tags
}

resource "aws_ecr_repository" "cadvisor" {
  name = "shipreq/ops/cadvisor"
  tags = local.default_tags
}

resource "aws_ecr_repository" "postgres_exporter" {
  name = "shipreq/ops/postgres_exporter"
  tags = local.default_tags
}

resource "aws_ecr_repository" "filebeat" {
  name = "shipreq/ops/filebeat"
  tags = local.default_tags
}
