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
  version = "~> 2.39"
}

provider "aws" {
  alias   = "ap-southeast-2"
  region  = "ap-southeast-2"
  version = "~> 2.39"
}

module "shipreq" {
  source = "../modules/shipreq-env"

  providers = {
    aws     = aws
    aws.ecr = aws.ap-southeast-2
  }

  env                 = "dev"
  name                = "Dev"
  availability_zone   = "ap-southeast-2b"
  availability_zone_2 = "ap-southeast-2c"
  deletion_protection = false
  vpc_ip_prefix       = "10.0"

  app_cluster_size                      = 0
  app_instance_type                     = "t3a.small"
  app_public_key                        = file("key-app.rsa.pub")
  bastion_public_key                    = file("key-bastion.rsa.pub")
  elasticsearch_enable                  = true
  elasticsearch_instance_type           = "t2.small.elasticsearch"
  elasticsearch_retention_days          = 45
  elasticsearch_volume_size             = 10
  elasticsearch_volume_type             = "standard" # Save money
  grafana_db_name                       = "grafana"
  grafana_db_password                   = "grafana"
  grafana_db_username                   = "grafana"
  nat_public_key                        = file("key-nat.rsa.pub")
  ops_cluster_ebs_volume_type           = "standard" # Save money
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
  shipreq_taskman_freshdesk_domain      = "yoarmum"
  shipreq_taskman_properties            = file("taskman.properties")
  shipreq_webapp_google_analytics_id    = "UA-105581783-2"
  shipreq_webapp_properties             = file("webapp.properties")

  app_cadvisor_image_tag          = "latest"
  app_filebeat_image_tag          = "latest"
  app_node_exporter_image_tag     = "latest"
  app_shipreq_images_tag          = "latest"
  bastion_filebeat_image_tag      = "latest"
  bastion_portal_image_tag        = "latest"
  nat_cadvisor_image_tag          = "latest"
  nat_filebeat_image_tag          = "latest"
  nat_image_tag                   = "latest"
  nat_node_exporter_image_tag     = "latest"
  nat_squid_exporter_image_tag    = "latest"
  ops_cadvisor_image_tag          = "latest"
  ops_ecs_exporter_image_tag      = "latest"
  ops_filebeat_image_tag          = "latest"
  ops_grafana_image_tag           = "latest"
  ops_node_exporter_image_tag     = "latest"
  ops_postgres_exporter_image_tag = "latest"
  ops_prometheus_biz_image_tag    = "latest"
  ops_prometheus_tech_image_tag   = "latest"

  # enable_db_dependant_services = false
}

output "bastion_host" {
  value = module.shipreq.bastion_host
}

output "public_endpoint" {
  value = module.shipreq.public_endpoint
}
