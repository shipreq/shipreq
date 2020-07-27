terraform {
  required_version = ">= 0.12"
}

provider "aws" {
  region  = "ap-southeast-2"
  version = "~> 2.31"
}
