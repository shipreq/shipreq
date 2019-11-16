# This creates a certificate for <env>.shipreq.com

resource "aws_acm_certificate" "shipreq" {
  domain_name       = local.shipreq_domain
  validation_method = "DNS"
  tags              = local.default_tags
  # subject_alternative_names = ["*.${local.shipreq_domain}"]
}

# DNS record for cert validation - keep in mind that this isn't the ALB endpoint record
resource "aws_route53_record" "cert_validation" {
  name    = aws_acm_certificate.shipreq.domain_validation_options.0.resource_record_name
  type    = aws_acm_certificate.shipreq.domain_validation_options.0.resource_record_type
  records = [aws_acm_certificate.shipreq.domain_validation_options.0.resource_record_value]
  zone_id = data.aws_route53_zone.shipreq.id
  ttl     = 10800
}

resource "aws_acm_certificate_validation" "cert" {
  certificate_arn         = aws_acm_certificate.shipreq.arn
  validation_record_fqdns = [aws_route53_record.cert_validation.fqdn]
}
