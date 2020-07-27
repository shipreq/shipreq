output "user_data" {
  description = "Bash code that should be inserted into the user_data of the ECS cluster's EC2s."
  value       = trimspace(local.user_data)
}

output "mount_dir" {
  description = "The path on the ECS cluster's EC2s into which EBS volumes are mounted."
  value       = local.mount_dir
}

output "tag_key" {
  description = "The tag name/key used to identify all volumes created by this module"
  value       = "ecs-ebs"
}

output "tag_value" {
  description = "The tag value used to identify all volumes created by this module"
  value       = local.tag_value
}

output "formatted_tag_key" {
  description = "The tag name/key used to identify a volume has been formatted or not"
  value       = "formatted"
}

output "formatted_tag_value" {
  description = "The tag value used to identify a volume has been formatted or not"
  value       = "y"
}
