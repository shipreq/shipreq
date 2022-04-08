data "aws_region" "s3" {}

resource "aws_cloudfront_distribution" "web" {
  count = var.cdn_enable ? 1 : 0

  aliases             = [var.dns_domain]
  default_root_object = "index.html"
  depends_on          = [module.cert, aws_s3_bucket.web]
  enabled             = true
  is_ipv6_enabled     = true
  price_class         = var.cdn_price_class
  tags                = local.default_tags

  origin {
    origin_id = local.cdn_origin_id

    # Temporary workaround for https://github.com/terraform-providers/terraform-provider-aws/issues/13393
    # domain_name = aws_s3_bucket.web.website_endpoint
    domain_name = "${var.s3_bucket_name}.s3-website-${data.aws_region.s3.name}.amazonaws.com"

    # Custom origin with S3 website as source
    # This ensures subdirectories redirect to their associated index.html
    custom_origin_config {
      http_port              = 80
      https_port             = 443
      origin_protocol_policy = "http-only"
      origin_ssl_protocols   = ["TLSv1.2"]
    }
  }

  default_cache_behavior {
    allowed_methods        = ["DELETE", "GET", "HEAD", "OPTIONS", "PATCH", "POST", "PUT"]
    cached_methods         = ["GET", "HEAD"]
    compress               = true     # cos even with Accept-Encoding forwarded, it doesn't seem to serve compressed
    default_ttl            = 31536000 # 1 yr cos we do an invalidation on publish
    max_ttl                = 31536000 # 1 yr cos we do an invalidation on publish
    min_ttl                = 31536000 # 1 yr cos we do an invalidation on publish
    target_origin_id       = local.cdn_origin_id
    viewer_protocol_policy = "redirect-to-https"

    forwarded_values {
      # headers      = ["Accept-Encoding"] # use compressed assets
      query_string = true
      cookies {
        forward = "none"
      }
    }
  }

  restrictions {
    geo_restriction {
      restriction_type = "none"
    }
  }

  viewer_certificate {
    acm_certificate_arn      = var.cert_arn != null ? var.cert_arn : var.cert_create ? module.cert[0].arn : null
    minimum_protocol_version = "TLSv1.2_2019"
    ssl_support_method       = "sni-only"
  }

  custom_error_response {
    error_caching_min_ttl = local.cdn_error_4xx_ttl
    error_code            = 400
    response_code         = 404
    response_page_path    = local.cdn_error_4xx_path
  }

  custom_error_response {
    error_caching_min_ttl = local.cdn_error_4xx_ttl
    error_code            = 403
    response_code         = 404
    response_page_path    = local.cdn_error_4xx_path
  }

  custom_error_response {
    error_caching_min_ttl = local.cdn_error_4xx_ttl
    error_code            = 404
    response_code         = 404
    response_page_path    = local.cdn_error_4xx_path
  }
}
