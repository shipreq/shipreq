terraform {
  required_version = ">= 0.12"

  backend "s3" {
    bucket = "shipreq-terraform-state"
    key    = "billing.tfstate"
    region = "ap-southeast-2"
  }
}

provider "aws" {
  region  = "ap-southeast-2"
  version = "~> 2.70"
}

provider "aws" {
  alias   = "cur" // cur = "Cost and Usage Report"
  region  = "us-east-1"
  version = "~> 2.70"
}
