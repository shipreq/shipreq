# An "altsite" is a S3 bucket with a static website to display instead of serving real ShipReq requests.
#
# For example, if we were having a day of maintainence downtime, we would enable the altsite, put a static site in there
# saying "down for maintainence" or whatever, and deploy it before starting the upgrade.
#
module "altsite" {
  count  = local.enable_altsite_infra ? 1 : 0
  source = "../web-static"

  providers = {
    aws           = aws
    aws.us_east_1 = aws.us_east_1
  }

  cert_arn       = module.cert.arn
  cert_create    = false
  dns_domain     = local.shipreq_domain
  dns_enable     = local.use_altsite
  dns_www_alias  = false # This is already defined in alb-webapp.tf
  dns_zone_id    = local.shipreq_zone_id
  s3_bucket_name = "shipreq-${var.env}-altsite"
  tags           = local.default_tags
}
