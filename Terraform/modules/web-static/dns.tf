# web.shipreq.com -> CDN
resource "aws_route53_record" "web" {
  zone_id = var.dns_zone_id
  name    = "${var.dns_domain}."
  type    = "A"
  alias {
    name                   = aws_cloudfront_distribution.web.domain_name
    zone_id                = aws_cloudfront_distribution.web.hosted_zone_id
    evaluate_target_health = false
  }
}

# www.web.shipreq.com -> web.shipreq.com
resource "aws_route53_record" "www" {
  zone_id = var.dns_zone_id
  name    = "www.${var.dns_domain}."
  type    = "CNAME"
  ttl     = "21600"
  records = ["${var.dns_domain}."]
}
