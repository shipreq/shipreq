module "shipreq" {
  source = "../modules/shipreq-env"

  name               = "dev"
  env                = "dev"
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
