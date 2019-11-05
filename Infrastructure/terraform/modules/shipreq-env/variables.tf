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

variable "enable_redis" {
  type    = bool
  default = true
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

variable "shipreq_webapp_keystore_filename" {
  description = "Path to the keystore"
  type        = string
}

variable "shipreq_webapp_ssl_passwords_ini_filename" {
  description = "Path to the ssl-passwords.ini"
  type        = string
}

variable "postgres_instance_type" {
  description = "EC2 instance type for Postgres"
  type        = string
}

variable "postgres_root_password" {
  type = string
}

variable "postgres_deletion_protection" {
  type = bool
}

variable "postgres_backup_retention_period" {
  type = number
}

variable "postgres_final_snapshot" {
  description = "The name of the final snapshot when the DB instance is deleted"
  type        = string
  default     = ""
}

variable "ops_images_tag" {
  description = "The docker tag for all ops images. Eg. `git-<sha>` or `latest`"
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

variable "prometheus_tech_scrape_interval" {
  type = string
}

variable "prometheus_biz_scrape_interval" {
  type = string
}
