provider "aws" {
  region  = "ap-southeast-2"
  version = "~> 2.31"
}

terraform {
  required_version = ">= 0.12"

  backend "s3" {
    bucket = "shipreq-terraform-state"
    key    = "global.tfstate"
    region = "ap-southeast-2"
  }
}

locals {
  default_tags = {
    createdBy = "terraform"
    env       = "n/a"
    terraform = "global"
  }
}
