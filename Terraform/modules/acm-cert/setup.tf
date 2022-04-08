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

variable "domain_name" {
  type = string
}

variable "subject_alternative_names" {
  type    = list(string)
  default = []
}

variable "tags" {
  type    = map(any)
  default = {}
}

variable "ttl" {
  type    = number
  default = 10800
}

variable "zone_id" {
  type = string
}

output "arn" {
  value = aws_acm_certificate.sole.arn
}
