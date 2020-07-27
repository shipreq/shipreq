resource "aws_acm_certificate" "shipreq" {
  domain_name               = local.shipreq_domain
  validation_method         = "DNS"
  tags                      = local.default_tags
  subject_alternative_names = ["www.${local.shipreq_domain}", local.analytics_proxy_domain]

  lifecycle { create_before_destroy = true }
}

# DNS records for cert validation - keep in mind that this isn't the ALB endpoint record
resource "aws_route53_record" "cert_validation_0" {
  name    = aws_acm_certificate.shipreq.domain_validation_options.0.resource_record_name
  type    = aws_acm_certificate.shipreq.domain_validation_options.0.resource_record_type
  records = [aws_acm_certificate.shipreq.domain_validation_options.0.resource_record_value]
  zone_id = local.shipreq_zone_id
  ttl     = 10800
}
resource "aws_route53_record" "cert_validation_1" {
  name    = aws_acm_certificate.shipreq.domain_validation_options.1.resource_record_name
  type    = aws_acm_certificate.shipreq.domain_validation_options.1.resource_record_type
  records = [aws_acm_certificate.shipreq.domain_validation_options.1.resource_record_value]
  zone_id = local.shipreq_zone_id
  ttl     = 10800
}
resource "aws_route53_record" "cert_validation_2" {
  name    = aws_acm_certificate.shipreq.domain_validation_options.2.resource_record_name
  type    = aws_acm_certificate.shipreq.domain_validation_options.2.resource_record_type
  records = [aws_acm_certificate.shipreq.domain_validation_options.2.resource_record_value]
  zone_id = local.shipreq_zone_id
  ttl     = 10800
}

resource "aws_acm_certificate_validation" "cert" {
  certificate_arn = aws_acm_certificate.shipreq.arn
  validation_record_fqdns = [
    aws_route53_record.cert_validation_0.fqdn,
    aws_route53_record.cert_validation_1.fqdn,
    aws_route53_record.cert_validation_2.fqdn,
  ]
}

# www.shipreq.com -> shipreq.com
resource "aws_route53_record" "www" {
  zone_id = local.shipreq_zone_id
  name    = "www.${local.shipreq_domain}."
  type    = "CNAME"
  ttl     = "21600"
  records = ["${local.shipreq_domain}."]
}
