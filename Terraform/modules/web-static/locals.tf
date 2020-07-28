locals {
  cdn_error_4xx_path = "/404.html"
  cdn_error_4xx_ttl  = 30
  cdn_origin_id      = "s3"
  default_tags       = var.tags
}
