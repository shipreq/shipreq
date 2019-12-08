terraform {
  required_version = ">= 0.12"

  backend "s3" {
    bucket = "shipreq-terraform-state"
    key    = "env-prod.tfstate"
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

  env                 = "prod"
  name                = "Prod"
  availability_zone   = "ap-southeast-2b"
  availability_zone_2 = "ap-southeast-2c"
  deletion_protection = true
  vpc_ip_prefix       = "10.0"

  app_cluster_size                      = 2
  app_instance_type                     = "t3a.small"
  app_public_key                        = file("key-app.rsa.pub")
  bastion_public_key                    = file("key-bastion.rsa.pub")
  cadvisor_image_tag                    = local.versions.cadvisor
  ecs_exporter_image_tag                = local.versions.ecs_exporter
  elasticsearch_instance_type           = "t2.small.elasticsearch"
  elasticsearch_volume_size             = 10
  filebeat_image_tag                    = local.versions.filebeat
  grafana_db_name                       = "grafana"
  grafana_db_password                   = local.passwords.db.grafana
  grafana_db_username                   = "grafana"
  grafana_image_tag                     = local.versions.grafana
  nat_image_tag                         = local.versions.nat
  nat_public_key                        = file("key-nat.rsa.pub")
  node_exporter_image_tag               = local.versions.node_exporter
  ops_instance_type                     = "t3a.micro"
  ops_public_key                        = file("key-ops.rsa.pub")
  portal_image_tag                      = local.versions.portal
  postgres_backup_retention_days        = 6 * 7
  postgres_exporter_db_password         = local.passwords.db.postgres_exporter
  postgres_exporter_db_username         = "postgres_exporter"
  postgres_exporter_image_tag           = local.versions.postgres_exporter
  postgres_instance_type                = "db.t3.micro"
  postgres_root_password                = local.passwords.db.root
  prometheus_biz_backup_retention_days  = 6 * 7
  prometheus_biz_data_retention         = "time=10y"
  prometheus_biz_ebs_size               = 2
  prometheus_biz_image_tag              = local.versions.prometheus_biz
  prometheus_biz_scrape_interval        = "15m"
  prometheus_tech_backup_retention_days = 6 * 7
  prometheus_tech_data_retention        = "time=53w"
  prometheus_tech_ebs_size              = 20
  prometheus_tech_image_tag             = local.versions.prometheus_tech
  prometheus_tech_scrape_interval_sec   = 30
  shipreq_db_name                       = "shipreq"
  shipreq_db_password                   = local.passwords.db.shipreq
  shipreq_db_taskman_schema             = "taskman"
  shipreq_db_username                   = "shipreq"
  shipreq_images_tag                    = local.versions.shipreq
  shipreq_taskman_freshdesk_domain      = "shipreq"
  shipreq_taskman_properties            = file("taskman.properties")
  shipreq_webapp_google_analytics_id    = "UA-105581783-1"
  shipreq_webapp_properties             = file("webapp.properties")
  squid_exporter_image_tag              = local.versions.squid_exporter
}

output "bastion_host" {
  value = module.shipreq.bastion_host
}

output "public_endpoint" {
  value = module.shipreq.public_endpoint
}
