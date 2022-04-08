output "cloudfront_id" {
  value = length(aws_cloudfront_distribution.web) == 0 ? null : aws_cloudfront_distribution.web[0].id
}

output "s3_bucket_name" {
  value = var.s3_bucket_name
}
