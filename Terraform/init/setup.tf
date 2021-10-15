terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 2.31"
    }
  }
}

provider "aws" {
  profile = "shipreq"
  region  = "ap-southeast-2"
}
