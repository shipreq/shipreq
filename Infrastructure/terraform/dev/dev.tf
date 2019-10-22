provider "aws" {
  region  = "ap-southeast-2"
  version = "~> 2.32"
}

module "shipreq" {
  source = "../modules/shipreq-env"

  providers = {
    aws     = aws
    aws.ecr = aws.ap-southeast-2
  }

  env                  = "dev"
  name                 = "Dev"
  vpc_ip_prefix        = "10.0"
  availability_zone    = "ap-southeast-2b"
  bastion_public_key   = file("key-bastion.rsa.pub")
  nat_public_key       = file("key-nat.rsa.pub")
  app_public_key       = file("key-app.rsa.pub")
  app_instance_type    = "t3a.medium"
  app_cluster_size     = 0
  ops_public_key       = file("key-ops.rsa.pub")
  ops_instance_type    = "t3a.nano"
  ecs_root_volume_type = "standard" # Save money
  enable_redis         = false
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
