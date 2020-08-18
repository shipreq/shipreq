locals {
  domain = "blog.shipreq.com"
}

module "blog" {
  source = "../modules/web-static"

  providers = {
    aws           = aws
    aws.us_east_1 = aws.us_east_1
  }

  dns_domain     = local.domain
  dns_zone_id    = data.aws_route53_zone.shipreq.zone_id
  s3_bucket_name = "shipreq-blog-0127a85de508571c1197d210"
  tags           = local.default_tags
}

output "cloudfront_id" {
  value = module.blog.cloudfront_id
}

output "url" {
  value = "https://${local.domain}"
}
