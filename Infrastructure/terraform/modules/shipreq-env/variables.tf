provider "aws" {
}

provider "aws" {
  alias = "ecr"
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

variable "ecs_root_volume_type" {
  description = "The volume type for ECS instance root drives. Configurable because AMI snapshot demands min 30GB which costs money."
  type        = string
  default     = "gp2"
}

variable "enable_elasticsearch" {
  type    = bool
  default = true
}

variable "elasticsearch_instance_type" {
  type = string
}

variable "elasticsearch_volume_type" {
  type    = string
  default = "gp2"
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

variable "nat_image_tag" {
  description = "The docker tag for the nat image. Eg. `git-<sha>` or `latest`"
  type        = string
}

variable "ops_images_tag" {
  description = "The docker tag for all ops images. Eg. `git-<sha>` or `latest`"
  type        = string
}

variable "shipreq_images_tag" {
  description = "The docker tag for all shipreq images. Eg. `git-<sha>` or `latest`"
  type        = string
}

variable "prometheus_tech_ebs_size" {
  description = "The size in GB of EBS volumes per Prometheus (tech) task"
  type        = number
}

variable "prometheus_biz_ebs_size" {
  description = "The size in GB of EBS volumes per Prometheus (biz) task"
  type        = number
}

variable "prometheus_tech_retention" {
  description = "Either 'size=xxx' or 'time=xxx'. See https://prometheus.io/docs/prometheus/latest/storage/"
  type        = string
}

variable "prometheus_biz_retention" {
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

variable "block_public_ops_calls" {
  description = "Add a rule to the webapp ALB to block calls to /ops/*"
  type        = bool
  default     = true
}

variable "google_analytics_tracking_id" {
  type = string
}

variable "freshdesk_domain" {
  type = string
}

variable "postgres_exporter_db_username" {
  type = string
}

variable "postgres_exporter_db_password" {
  type = string
}
