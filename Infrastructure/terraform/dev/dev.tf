terraform {
  required_version = ">= 0.12"

  backend "s3" {
    bucket = "shipreq-terraform-state"
    key    = "env-dev.tfstate"
    region = "ap-southeast-2"
  }
}

provider "aws" {
  region  = "ap-southeast-2"
  version = "~> 2.32"
}

provider "aws" {
  alias   = "ap-southeast-2"
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
  availability_zone    = "ap-southeast-2b"
  availability_zone_2  = "ap-southeast-2c"
  deletion_protection  = false
  ecs_root_volume_type = "standard" # Save money
  vpc_ip_prefix        = "10.0"

  app_cluster_size                      = 2
  app_instance_type                     = "t3a.small"
  app_public_key                        = file("key-app.rsa.pub")
  bastion_public_key                    = file("key-bastion.rsa.pub")
  elasticsearch_enable                  = true
  elasticsearch_instance_type           = "t2.small.elasticsearch"
  elasticsearch_volume_size             = 10
  elasticsearch_volume_type             = "standard" # Save money
  grafana_db_name                       = "grafana"
  grafana_db_password                   = "grafana"
  grafana_db_username                   = "grafana"
  nat_image_tag                         = "latest"
  nat_public_key                        = file("key-nat.rsa.pub")
  ops_images_tag                        = "latest"
  ops_instance_type                     = "t3a.micro"
  ops_public_key                        = file("key-ops.rsa.pub")
  postgres_backup_retention_days        = 2
  postgres_exporter_db_password         = "dev-metrics"
  postgres_exporter_db_username         = "postgres_exporter"
  postgres_instance_type                = "db.t3.micro"
  postgres_root_password                = "dev-1234"
  prometheus_biz_backup_retention_days  = 7
  prometheus_biz_data_retention         = "time=10y"
  prometheus_biz_ebs_size               = 1
  prometheus_biz_scrape_interval        = "5m"
  prometheus_tech_backup_retention_days = 7
  prometheus_tech_data_retention        = "time=8w"
  prometheus_tech_ebs_size              = 3
  prometheus_tech_scrape_interval_sec   = 30
  shipreq_db_name                       = "shipreq"
  shipreq_db_password                   = "dev"
  shipreq_db_taskman_schema             = "taskman"
  shipreq_db_username                   = "shipreq"
  shipreq_images_tag                    = "latest"
  shipreq_taskman_freshdesk_domain      = "yoarmum"
  shipreq_taskman_properties            = file("taskman.properties")
  shipreq_webapp_google_analytics_id    = "UA-105581783-2"
  shipreq_webapp_properties             = file("webapp.properties")
}

output "bastion_host" {
  value = module.shipreq.bastion_host
}

output "public_endpoint" {
  value = module.shipreq.public_endpoint
}
