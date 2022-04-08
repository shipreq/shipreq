resource "aws_acm_certificate" "sole" {
  domain_name               = var.domain_name
  provider                  = aws.us_east_1 // https://registry.terraform.io/providers/hashicorp/aws/latest/docs/resources/cloudfront_distribution#acm_certificate_arn
  subject_alternative_names = var.subject_alternative_names
  tags                      = var.tags
  validation_method         = "DNS"

  lifecycle { create_before_destroy = true }
}

# DNS records for cert validation
resource "aws_route53_record" "sole" {
  for_each = {
    for o in aws_acm_certificate.sole.domain_validation_options : o.domain_name => {
      name   = o.resource_record_name
      record = o.resource_record_value
      type   = o.resource_record_type
    }
  }

  name    = each.value.name
  type    = each.value.type
  records = [each.value.record]
  zone_id = var.zone_id
  ttl     = var.ttl
}

resource "aws_acm_certificate_validation" "sole" {
  provider                = aws.us_east_1 // https://registry.terraform.io/providers/hashicorp/aws/latest/docs/resources/cloudfront_distribution#acm_certificate_arn
  certificate_arn         = aws_acm_certificate.sole.arn
  validation_record_fqdns = [for r in aws_route53_record.sole : r.fqdn]
}
