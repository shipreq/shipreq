terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = ">= 4, < 5"
    }
  }

  backend "s3" {
    bucket  = "shipreq-terraform-state"
    key     = "blog.tfstate"
    region  = "ap-southeast-2"
    profile = "shipreq"
  }
}

provider "aws" {
  profile = "shipreq"
  region  = "ap-southeast-2" // Comment for blog/gatsy-config.ts
}

provider "aws" {
  profile = "shipreq"
  alias   = "us_east_1"
  region  = "us-east-1"
}

locals {
  default_tags = {
    createdBy = "terraform"
    env       = "n/a"
    terraform = "blog"
  }
}
