# web.shipreq.com -> CDN
resource "aws_route53_record" "web" {
  count   = (var.dns_enable && length(aws_cloudfront_distribution.web) > 0) ? 1 : 0
  zone_id = var.dns_zone_id
  name    = "${var.dns_domain}."
  type    = "A"
  alias {
    name                   = aws_cloudfront_distribution.web[0].domain_name
    zone_id                = aws_cloudfront_distribution.web[0].hosted_zone_id
    evaluate_target_health = false
  }
}

# www.web.shipreq.com -> web.shipreq.com
resource "aws_route53_record" "www" {
  count   = (var.dns_enable && var.dns_www_alias) ? 1 : 0
  zone_id = var.dns_zone_id
  name    = "www.${var.dns_domain}."
  type    = "CNAME"
  ttl     = "21600"
  records = ["${var.dns_domain}."]
}
