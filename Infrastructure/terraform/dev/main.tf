provider "aws" {
  region  = "us-east-2"
  version = "~> 2.32"
}

module "shipreq" {
  source = "../modules/shipreq-env"

  providers = {
    aws     = aws
    aws.ecr = aws.ap-southeast-2
  }

  env                = "dev"
  name               = "Dev"
  vpc_ip_prefix      = "10.0"
  availability_zone  = "us-east-2b"
  bastion_public_key = file("key-bastion.rsa.pub")
  nat_public_key     = file("key-nat.rsa.pub")
  ops_public_key     = file("key-ops.rsa.pub")
  ops_instance_type  = "t3a.nano"
}

output "bastion_host" {
  value = module.shipreq.bastion_host
}

####################################################################################################

provider "aws" {
  alias   = "ap-southeast-2"
  region  = "ap-southeast-2"
  version = "~> 2.32"
}

terraform {
  required_version = ">= 0.12"

  backend "s3" {
    bucket = "shipreq-terraform-state"
    key    = "env-dev.tfstate"
    region = "ap-southeast-2"
  }
}
