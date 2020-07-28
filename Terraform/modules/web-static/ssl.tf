resource "aws_acm_certificate" "web" {
  domain_name               = var.dns_domain
  provider                  = aws.us_east_1 // https://registry.terraform.io/providers/hashicorp/aws/latest/docs/resources/cloudfront_distribution#acm_certificate_arn
  subject_alternative_names = ["www.${var.dns_domain}"]
  tags                      = local.default_tags
  validation_method         = "DNS"

  lifecycle { create_before_destroy = true }
}

# DNS records for cert validation
resource "aws_route53_record" "cert_validation_0" {
  name    = aws_acm_certificate.web.domain_validation_options.0.resource_record_name
  type    = aws_acm_certificate.web.domain_validation_options.0.resource_record_type
  records = [aws_acm_certificate.web.domain_validation_options.0.resource_record_value]
  zone_id = var.dns_zone_id
  ttl     = 10800
}
resource "aws_route53_record" "cert_validation_1" {
  name    = aws_acm_certificate.web.domain_validation_options.1.resource_record_name
  type    = aws_acm_certificate.web.domain_validation_options.1.resource_record_type
  records = [aws_acm_certificate.web.domain_validation_options.1.resource_record_value]
  zone_id = var.dns_zone_id
  ttl     = 10800
}

resource "aws_acm_certificate_validation" "cert" {
  provider        = aws.us_east_1
  certificate_arn = aws_acm_certificate.web.arn
  validation_record_fqdns = [
    aws_route53_record.cert_validation_0.fqdn,
    aws_route53_record.cert_validation_1.fqdn,
  ]
}
