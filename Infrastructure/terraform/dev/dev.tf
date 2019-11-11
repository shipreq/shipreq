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

  env                                       = "dev"
  name                                      = "Dev"
  vpc_ip_prefix                             = "10.0"
  availability_zone                         = "ap-southeast-2b"
  availability_zone_2                       = "ap-southeast-2c"
  bastion_public_key                        = file("key-bastion.rsa.pub")
  nat_public_key                            = file("key-nat.rsa.pub")
  app_public_key                            = file("key-app.rsa.pub")
  app_instance_type                         = "t3a.medium"
  app_cluster_size                          = 1
  ops_public_key                            = file("key-ops.rsa.pub")
  ops_instance_type                         = "t3a.micro"
  ecs_root_volume_type                      = "standard" # Save money
  enable_redis                              = false
  enable_elasticsearch                      = true
  elasticsearch_instance_type               = "t2.small.elasticsearch"
  elasticsearch_volume_type                 = "standard" # Save money
  elasticsearch_volume_size                 = 10
  shipreq_webapp_keystore_filename          = "../../../Secrets/ssl/keystore"
  shipreq_webapp_ssl_passwords_ini_filename = "../../../Secrets/ssl/ssl-passwords.ini"
  postgres_instance_type                    = "db.t3.micro"
  postgres_root_password                    = "dev-1234"
  postgres_deletion_protection              = false
  postgres_backup_retention_period          = 0
  ops_images_tag                            = "latest"
  prometheus_tech_ebs_size                  = 4
  prometheus_tech_retention                 = "time=12w"
  prometheus_tech_scrape_interval           = "30s"
  prometheus_biz_ebs_size                   = 1
  prometheus_biz_retention                  = "time=10y"
  prometheus_biz_scrape_interval            = "2m"
  grafana_db_name                           = "grafana"
  grafana_db_username                       = "grafana"
  grafana_db_password                       = "grafana"
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
