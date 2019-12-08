variable "name" {
  description = "Name of new resources"
  type        = string
}

variable "ec2_name_tag" {
  description = "Value associated with the Name tag of EC2 to discover"
  type        = string
}

variable "ec2_role_name" {
  type = string
}

variable "sd_name" {
  description = "Service discovery name (i.e. subdomain)"
  type        = string
}

variable "sd_namespace_id" {
  description = "A value like: aws_service_discovery_private_dns_namespace.xxxx.id"
  type        = string
}

variable "cron_schedule" {
  description = "Cron schedule upon which stale EC2s should be pruned from service-discovery."
  type        = string
  default     = "*/4 * * * *"
}
