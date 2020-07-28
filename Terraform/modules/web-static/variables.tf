provider "aws" {
}

// https://registry.terraform.io/providers/hashicorp/aws/latest/docs/resources/cloudfront_distribution#acm_certificate_arn
provider "aws" {
  alias = "us_east_1"
}

variable "cdn_price_class" {
  type    = string
  default = "PriceClass_All"
}

variable "dns_domain" { type = string }
variable "dns_zone_id" { type = string }
variable "s3_bucket_name" { type = string }
variable "tags" { type = map }
