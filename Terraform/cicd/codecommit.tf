resource "aws_codecommit_repository" "shipreq" {
  repository_name = "shipreq"
  tags            = local.default_tags
}
