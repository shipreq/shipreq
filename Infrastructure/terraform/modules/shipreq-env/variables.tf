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

variable "bastion_public_key" {
  type = string
}

variable "nat_public_key" {
  type = string
}

variable "ops_public_key" {
  type = string
}

variable "ops_instance_type" {
  description = "EC2 instance type for machines in the ops cluster"
  type        = string
}
