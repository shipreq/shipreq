locals {
  domain = "blog-test.shipwreck.space"
}

module "blog" {
  source = "../modules/web-static"

  providers = {
    aws           = aws
    aws.us_east_1 = aws.us_east_1
  }

  dns_domain     = local.domain
  dns_zone_id    = data.aws_route53_zone.shipwreck.zone_id
  s3_bucket_name = "shipreq-blog"
  tags           = local.default_tags
}

output "url" {
  value = "https://${local.domain}"
}
