module "cert" {
  count  = var.cert_create ? 1 : 0
  source = "../acm-cert"

  providers = {
    aws           = aws
    aws.us_east_1 = aws.us_east_1
  }

  domain_name               = var.dns_domain
  subject_alternative_names = ["www.${var.dns_domain}"]
  tags                      = local.default_tags
  zone_id                   = var.dns_zone_id
}
