output "user_data1" {
  description = "Bash code that should be inserted into the user_data of the ECS cluster's EC2s. (Part 1)"
  value       = trimspace(local.user_data1)
}

output "user_data2" {
  description = "Bash code that should be inserted into the user_data of the ECS cluster's EC2s. (Part 2)"
  value       = trimspace(local.user_data2)
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
