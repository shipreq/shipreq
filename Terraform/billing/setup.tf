terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 3.4"
    }
  }

  backend "s3" {
    bucket  = "shipreq-terraform-state"
    key     = "billing.tfstate"
    region  = "ap-southeast-2"
    profile = "shipreq"
  }
}

provider "aws" {
  profile = "shipreq"
  region  = "ap-southeast-2"
}

provider "aws" {
  profile = "shipreq"
  alias   = "cur" // cur = "Cost and Usage Report"
  region  = "us-east-1"
}
