terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = ">= 4, < 5"
      configuration_aliases = [
        aws.us_east_1, // Needed for CloudFront SSL
      ]
    }
  }
}

variable "cert_arn" {
  type    = string
  default = null
}

variable "cert_create" {
  type    = bool
  default = true
}

variable "cdn_enable" {
  type    = bool
  default = true
}

variable "cdn_price_class" {
  type    = string
  default = "PriceClass_All"
}

variable "dns_domain" {
  type = string
}

variable "dns_enable" {
  type    = bool
  default = true
}

variable "dns_www_alias" {
  type    = bool
  default = true
}

variable "dns_zone_id" {
  type = string
}

variable "s3_bucket_name" {
  type = string
}

variable "tags" {
  type = map(any)
}
