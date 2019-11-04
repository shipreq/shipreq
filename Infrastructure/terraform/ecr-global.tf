# The following are all created by ./global

data "aws_ecr_repository" "shipreq_base" {
  name = "shipreq/base"
}

data "aws_ecr_repository" "taskman" {
  name = "shipreq/taskman"
}

data "aws_ecr_repository" "webapp" {
  name = "shipreq/webapp"
}

data "aws_ecr_repository" "ops_portal" {
  name = "shipreq/ops/portal"
}

data "aws_ecr_repository" "grafana" {
  name = "shipreq/ops/grafana"
}

data "aws_ecr_repository" "prometheus-tech" {
  name = "shipreq/ops/prometheus-tech"
}

data "aws_ecr_repository" "prometheus-biz" {
  name = "shipreq/ops/prometheus-biz"
}

data "aws_ecr_repository" "node_exporter" {
  name = "shipreq/ops/node_exporter"
}

data "aws_ecr_repository" "cadvisor" {
  name = "shipreq/ops/cadvisor"
}

data "aws_ecr_repository" "postgres_exporter" {
  name = "shipreq/ops/postgres_exporter"
}
