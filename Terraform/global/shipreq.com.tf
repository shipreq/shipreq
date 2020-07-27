resource "aws_route53_zone" "shipreq" {
  name = "shipreq.com"
  tags = local.default_tags
}

resource "aws_route53_record" "mx" {
  zone_id = aws_route53_zone.shipreq.zone_id
  name    = "shipreq.com."
  type    = "MX"
  ttl     = "3600"

  records = [
    "15 q25vnpoqyi5drrdjclv57uqze6sf3pqlc5kddm7ylsghxkoqjzsq.mx-verification.google.com",
    "1 aspmx.l.google.com",
    "5 alt1.aspmx.l.google.com",
    "5 alt2.aspmx.l.google.com",
    "10 alt3.aspmx.l.google.com",
    "10 alt4.aspmx.l.google.com",
  ]
}

resource "aws_route53_record" "spf" {
  zone_id = aws_route53_zone.shipreq.zone_id
  name    = "shipreq.com."
  type    = "TXT"
  ttl     = "3600"
  records = ["v=spf1 include:_spf.google.com include:servers.mcsv.net ~all"]
}

resource "aws_route53_record" "unknown_1" {
  zone_id = aws_route53_zone.shipreq.zone_id
  name    = "_9a9403865e5e41bb6a547974f203fda6.shipreq.com."
  type    = "CNAME"
  ttl     = "10800"
  records = ["d886238ab891c42d40d2221c41429b67.280c137970857031bcfced4a59f7f74e.9b5d7c532f524acbb131.comodoca.com."]
}

resource "aws_route53_record" "gsuite_dkim" {
  zone_id = aws_route53_zone.shipreq.zone_id
  name    = "google._domainkey.shipreq.com."
  type    = "TXT"
  ttl     = "21600"
  records = ["v=DKIM1; k=rsa; p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQClSURm++WKJLgZ2oYOkxFPrKl7Xr4z5rYF4jvNvRjVqsyqOp5TpdMB50Jysk2Br+E70JGFO4IfB1HnmCaTkr1BAKLCaNsH2XIfPNg+NG3IAC8EBd0TAZ5/yXcufMk4pEEhfWvulkFTsX/T+D5oPTEvbq0bVLFzHSkV0iK1XP/jCwIDAQAB"]
}

resource "aws_route53_record" "unknown_2" {
  zone_id = aws_route53_zone.shipreq.zone_id
  name    = "k1._domainkey.shipreq.com."
  type    = "CNAME"
  ttl     = "3600"
  records = ["dkim.mcsv.net."]
}

resource "aws_route53_record" "freshdesk_dkim_1" {
  zone_id = aws_route53_zone.shipreq.zone_id
  name    = "m1._domainkey.shipreq.com."
  type    = "CNAME"
  ttl     = "3600"
  records = ["acc117690.domainkey.freshdesk.com."]
}

resource "aws_route53_record" "freshdesk_dkim_2" {
  zone_id = aws_route53_zone.shipreq.zone_id
  name    = "fddkim.shipreq.com."
  type    = "CNAME"
  ttl     = "3600"
  records = ["spfmx.domainkey.freshdesk.com."]
}

resource "aws_route53_record" "freshdesk_dkim_3" {
  zone_id = aws_route53_zone.shipreq.zone_id
  name    = "s1._domainkey.shipreq.com."
  type    = "CNAME"
  ttl     = "3600"
  records = ["s1acc117690.domainkey.freshdesk.com."]
}

resource "aws_route53_record" "freshdesk_dkim_4" {
  zone_id = aws_route53_zone.shipreq.zone_id
  name    = "s2._domainkey.shipreq.com."
  type    = "CNAME"
  ttl     = "3600"
  records = ["s2acc117690.domainkey.freshdesk.com."]
}

resource "aws_route53_record" "mailgun_1" {
  zone_id = aws_route53_zone.shipreq.zone_id
  name    = "mg.shipreq.com."
  type    = "TXT"
  ttl     = "3600"
  records = ["v=spf1 include:mailgun.org ~all"]
}

resource "aws_route53_record" "mailgun_2" {
  zone_id = aws_route53_zone.shipreq.zone_id
  name    = "smtp._domainkey.mg.shipreq.com."
  type    = "TXT"
  ttl     = "3600"
  records = ["k=rsa; p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDrRxXLtYiuJl+5SF5iXlZMASCd93foKPWbO+9Te9gKT1p8cpn9eipDIOLqQlAE89c0OhHx8Hpb0SV9dCETtgXRQRO1PkYHlk7yqR5ZhCTn65HGtQyu3OrFKgYljWGEi4Um4dZr8aBw1Yl6cW0YeX2NacWNxdn8XNoP//EYJKl4iQIDAQAB"]
}

resource "aws_route53_record" "mailgun_3" {
  zone_id = aws_route53_zone.shipreq.zone_id
  name    = "mg.shipreq.com."
  type    = "MX"
  ttl     = "3600"
  records = ["10 mxa.mailgun.org.", "10 mxb.mailgun.org."]
}

resource "aws_route53_record" "mailgun_4" {
  zone_id = aws_route53_zone.shipreq.zone_id
  name    = "email.mg.shipreq.com."
  type    = "CNAME"
  ttl     = "21600"
  records = ["mailgun.org."]
}
