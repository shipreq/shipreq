resource "aws_ecr_repository" "shipreq_dev_build_env" {
  name = "shipreq/dev/build_env"
  tags = local.default_tags
}

resource "aws_ecr_repository" "shipreq_dev_node" {
  name = "shipreq/dev/node"
  tags = local.default_tags
}

resource "aws_ecr_repository" "shipreq_dev_postgres" {
  name = "shipreq/dev/postgres"
  tags = local.default_tags
}
