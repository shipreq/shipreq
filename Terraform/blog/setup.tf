provider "aws" {
  region  = "ap-southeast-2" // Comment for blog/gatsy-config.ts
  version = "~> 2.70"
}

provider "aws" {
  alias   = "us_east_1"
  region  = "us-east-1"
  version = "~> 2.70"
}

terraform {
  required_version = ">= 0.12"

  backend "s3" {
    bucket = "shipreq-terraform-state"
    key    = "blog.tfstate"
    region = "ap-southeast-2"
  }
}

locals {
  default_tags = {
    createdBy = "terraform"
    env       = "n/a"
    terraform = "blog"
  }
}
