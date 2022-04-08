locals {
  shipreq_cdn_origin_id = "webapp"
  cdn_tags              = merge(local.default_tags, { Name = "${var.env}-cdn" })
}

resource "aws_cloudfront_distribution" "static" {
  count           = local.enable_app_cdn ? 1 : 0
  aliases         = [local.shipreq_cdn_domain]
  depends_on      = [module.cert]
  enabled         = true
  is_ipv6_enabled = true
  price_class     = var.shipreq_cdn_price_class
  tags            = local.cdn_tags

  origin {
    origin_id   = local.shipreq_cdn_origin_id
    domain_name = local.shipreq_domain
    origin_path = "/s" # Do not include a / at the end of the directory name

    custom_origin_config {
      http_port              = 80
      https_port             = 443
      origin_protocol_policy = "https-only"
      origin_ssl_protocols   = ["TLSv1.2"]
    }
  }

  default_cache_behavior {
    allowed_methods        = ["GET", "HEAD", "OPTIONS"]
    cached_methods         = ["GET", "HEAD", "OPTIONS"]
    target_origin_id       = local.shipreq_cdn_origin_id
    viewer_protocol_policy = "https-only"
    compress               = false # assets are already compressed
    default_ttl            = local.seconds_in_a_year * 8
    min_ttl                = local.seconds_in_a_year * 8
    max_ttl                = local.seconds_in_a_year * 8

    forwarded_values {
      headers = [
        "Accept-Encoding", # use compressed assets
        "Origin"           # for CORS
      ]
      cookies {
        forward = "none"
      }
      query_string = false
    }
  }

  restrictions {
    geo_restriction {
      restriction_type = "none"
    }
  }

  viewer_certificate {
    acm_certificate_arn      = aws_acm_certificate.cdn[0].arn
    minimum_protocol_version = "TLSv1.2_2019"
    ssl_support_method       = "sni-only"
  }
}

resource "aws_acm_certificate" "cdn" {
  count             = local.enable_app_cdn ? 1 : 0
  provider          = aws.us_east_1 // https://registry.terraform.io/providers/hashicorp/aws/latest/docs/resources/cloudfront_distribution#acm_certificate_arn
  domain_name       = local.shipreq_cdn_domain
  validation_method = "DNS"
  tags              = local.cdn_tags

  lifecycle { create_before_destroy = true }
}

# DNS records for cert validation - keep in mind that this isn't the ALB endpoint record
resource "aws_route53_record" "cdn_cert_validation" {
  for_each = {
    for o in flatten(aws_acm_certificate.cdn[*].domain_validation_options) : o.domain_name => {
      name   = o.resource_record_name
      record = o.resource_record_value
      type   = o.resource_record_type
    }
  }

  name    = each.value.name
  type    = each.value.type
  records = [each.value.record]
  zone_id = local.shipreq_zone_id
  ttl     = 10800
}

resource "aws_acm_certificate_validation" "cdn" {
  count                   = local.enable_app_cdn ? 1 : 0
  provider                = aws.us_east_1 // https://registry.terraform.io/providers/hashicorp/aws/latest/docs/resources/cloudfront_distribution#acm_certificate_arn
  certificate_arn         = aws_acm_certificate.cdn[0].arn
  validation_record_fqdns = [for r in aws_route53_record.cdn_cert_validation : r.fqdn]
}

resource "aws_route53_record" "static" {
  count   = local.enable_app_cdn ? 1 : 0
  zone_id = local.shipreq_zone_id
  name    = "${local.shipreq_cdn_domain}."
  type    = "A"
  alias {
    name                   = aws_cloudfront_distribution.static[0].domain_name
    zone_id                = aws_cloudfront_distribution.static[0].hosted_zone_id
    evaluate_target_health = false
  }
}
