terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = ">= 3.31"
      configuration_aliases = [
        aws.us_east_1, // Needed for CloudFront SSL
      ]
    }
  }
}

variable "cdn_price_class" {
  type    = string
  default = "PriceClass_All"
}

variable "dns_domain" { type = string }
variable "dns_zone_id" { type = string }
variable "s3_bucket_name" { type = string }
variable "tags" { type = map(any) }
