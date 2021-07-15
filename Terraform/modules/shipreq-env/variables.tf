terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = ">= 3.31"
      configuration_aliases = [
        aws.ecr,
        aws.us_east_1, // Needed for CloudFront SSL
      ]
    }
  }
}

variable "env" {
  description = "The short textual ID of this environment"
  type        = string
}

variable "name" {
  description = "The human-readable name (and optionally: desc) of this environment"
  type        = string
}

variable "vpc_ip_prefix" {
  description = "The first two IP4 values of the VPC"
  type        = string
}

variable "availability_zone" {
  type = string
}

variable "availability_zone_2" {
  description = "Secondary AZ required for RDS"
  type        = string
}

variable "bastion_public_key" {
  type = string
}

variable "nat_ami" {
  type    = string
  default = null
}

variable "nat_public_key" {
  type = string
}

variable "app_public_key" {
  type = string
}

variable "app_instance_type" {
  description = "EC2 instance type for machines in the app cluster"
  type        = string
}

variable "app_cluster_size" {
  description = "Number of machines in the app cluster"
  type        = number
}

variable "ops_public_key" {
  type = string
}

variable "ops_instance_type" {
  description = "EC2 instance type for machines in the ops cluster"
  type        = string
}

variable "ops_cluster_ebs_volume_type" {
  description = "The volume type for ECS instance root drives in the ops cluster. Configurable because AMI snapshot demands min 30GB which costs money."
  type        = string
  default     = "gp3"
}

variable "elasticsearch_enable" {
  type    = bool
  default = true
}

variable "elasticsearch_instance_type" {
  type = string
}

variable "elasticsearch_retention_days" {
  type = number
}

variable "elasticsearch_maintenance_cron_schedule" {
  description = "Cron schedule upon which ES maintenance should be carried out."
  type        = string
  default     = "0 19 * * *" # Daily at 19:00 UTC which is 5am +10
}

variable "elasticsearch_volume_type" {
  type = string
}

variable "elasticsearch_volume_size" {
  description = "Min:10GB, Max:35GB"
  type        = number
}

variable "postgres_instance_type" {
  description = "EC2 instance type for Postgres"
  type        = string
}

variable "postgres_root_password" {
  type = string
}

variable "deletion_protection" {
  type = bool
}

variable "postgres_backup_retention_days" {
  type = number
}

variable "prometheus_tech_backup_retention_days" {
  type = number
}

variable "prometheus_biz_backup_retention_days" {
  type = number
}

variable "postgres_monitoring_interval_sec" {
  description = "The interval, in seconds, between points when Enhanced Monitoring metrics are collected. Valid Values: 0 (disable), 1, 5, 10, 15, 30, 60."
  type        = number
  default     = 0
}

variable "postgres_final_snapshot" {
  description = "The name of the final snapshot when the DB instance is deleted"
  type        = string
  default     = ""
}

variable "app_analytics_proxy_image_tag" { type = string }
variable "app_cadvisor_image_tag" { type = string }
variable "app_filebeat_image_tag" { type = string }
variable "app_node_exporter_image_tag" { type = string }
variable "app_shipreq_images_tag" { type = string }
variable "bastion_filebeat_image_tag" { type = string }
variable "bastion_portal_image_tag" { type = string }
variable "nat_cadvisor_image_tag" { type = string }
variable "nat_filebeat_image_tag" { type = string }
variable "nat_image_tag" { type = string }
variable "nat_node_exporter_image_tag" { type = string }
variable "nat_squid_exporter_image_tag" { type = string }
variable "ops_cadvisor_image_tag" { type = string }
variable "ops_ecs_exporter_image_tag" { type = string }
variable "ops_filebeat_image_tag" { type = string }
variable "ops_grafana_image_tag" { type = string }
variable "ops_node_exporter_image_tag" { type = string }
variable "ops_postgres_exporter_image_tag" { type = string }
variable "ops_prometheus_biz_image_tag" { type = string }
variable "ops_prometheus_tech_image_tag" { type = string }

variable "prometheus_tech_ebs_size" {
  description = "The size in GB of EBS volumes per Prometheus (tech) task"
  type        = number
}

variable "prometheus_biz_ebs_size" {
  description = "The size in GB of EBS volumes per Prometheus (biz) task"
  type        = number
}

variable "prometheus_tech_data_retention" {
  description = "Either 'size=xxx' or 'time=xxx'. See https://prometheus.io/docs/prometheus/latest/storage/"
  type        = string
}

variable "prometheus_biz_data_retention" {
  description = "Either 'size=xxx' or 'time=xxx'. See https://prometheus.io/docs/prometheus/latest/storage/"
  type        = string
}

variable "prometheus_tech_scrape_interval_sec" {
  type = number
}

variable "prometheus_biz_scrape_interval" {
  type = string
}

variable "grafana_db_name" {
  type = string
}

variable "grafana_db_username" {
  type = string
}

variable "grafana_db_password" {
  type = string
}

variable "shipreq_db_name" {
  type = string
}

variable "shipreq_db_username" {
  type = string
}

variable "shipreq_db_password" {
  type = string
}

variable "shipreq_webapp_properties" {
  description = "Content of webapp's shipreq.properties"
  type        = string
}

variable "shipreq_taskman_properties" {
  description = "Content of taskman's shipreq.properties"
  type        = string
}

variable "shipreq_webapp_log_level_root" {
  type    = string
  default = "INFO"
}

variable "shipreq_webapp_log_level_shipreq" {
  type    = string
  default = "INFO"
}

variable "shipreq_taskman_log_level_root" {
  type    = string
  default = "INFO"
}

variable "shipreq_taskman_log_level_shipreq" {
  type    = string
  default = "INFO"
}

variable "shipreq_db_taskman_schema" {
  type = string
}

variable "shipreq_webapp_allow_ops_routes_publically" {
  description = "When false, add a rule to the webapp ALB to block calls to /ops/*"
  type        = bool
  default     = false
}

variable "shipreq_webapp_google_analytics_id" {
  type = string
}

variable "shipreq_taskman_freshdesk_domain" {
  type = string
}

variable "postgres_exporter_db_username" {
  type = string
}

variable "postgres_exporter_db_password" {
  type = string
}

variable "enable_db_dependant_services" {
  type    = bool
  default = true
}

variable "kibana_default_path" {
  type    = string
  default = ""
}

variable "shipreq_cdn_subdomain" {
  description = "When specified, this will create a CDN and route webapp static assets through it."
  type        = string // | null
  default     = null
}

variable "shipreq_webapp_use_cdn" {
  description = "When specified, this will create a CDN and route webapp static assets through it."
  type        = bool
  default     = true
}

variable "shipreq_cdn_price_class" {
  type    = string
  default = "PriceClass_All"
}
