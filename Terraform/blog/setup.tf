terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = ">= 3.4"
    }
  }

  backend "s3" {
    bucket = "shipreq-terraform-state"
    key    = "blog.tfstate"
    region = "ap-southeast-2"
  }
}

provider "aws" {
  region = "ap-southeast-2" // Comment for blog/gatsy-config.ts
}

provider "aws" {
  alias  = "us_east_1"
  region = "us-east-1"
}

locals {
  default_tags = {
    createdBy = "terraform"
    env       = "n/a"
    terraform = "blog"
  }
}
