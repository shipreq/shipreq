#############
# Mandatory #
#############

variable "name" {
  description = "Globally-unique name for the set of drives. Typically this should be the ecs cluster name."
  type        = string
}

variable "size" {
  description = "Size in GB of each drive."
  type        = number
}

variable "manifest" {
  description = "Which AZs in which to create drives, and how many drives to create."
  type = list(object({
    availability_zone = string
    count             = number
  }))
}

variable "ec2_role" {
  description = "The IAM role of the EC2s that form the ECS cluster."
  type        = object({ name = string })
}

############
# Optional #
############

variable "type" {
  description = "Volumes type of each drive."
  type        = string
  default     = "gp2"
}

variable "device_path" {
  description = "Device path to which the volume should be mounted in EC2s."
  type        = string
  default     = "/dev/xvdf"
}

variable "tags" {
  type    = map(string)
  default = {}
}

variable "cron_schedule" {
  description = "Cron schedule upon which drives should be confirmed as attached, and re-attached if not."
  type        = string
  default     = "*/2 * * * *"
}
