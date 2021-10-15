terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 3.4"
    }
  }

  backend "s3" {
    bucket  = "shipreq-terraform-state"
    key     = "cicd.tfstate"
    region  = "ap-southeast-2"
    profile = "shipreq"
  }
}

provider "aws" {
  profile = "shipreq"
  region  = "ap-southeast-2"
}

locals {
  default_tags = {
    createdBy = "terraform"
    env       = "n/a"
    terraform = "cicd"
  }
}
