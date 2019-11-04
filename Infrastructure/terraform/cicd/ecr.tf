resource "aws_ecr_repository" "shipreq_build" {
  name = "shipreq/build"
  tags = local.default_tags
}
