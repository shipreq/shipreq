resource "aws_route53_zone" "shipwreck" {
  name = "shipwreck.space"
  tags = local.default_tags
}
