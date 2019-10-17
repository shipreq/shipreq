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
