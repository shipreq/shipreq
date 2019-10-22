output "user_data" {
  description = "Bash code that should be inserted into the user_data of the ECS cluster's EC2s."
  value       = trimspace(local.user_data)
}

output "mount_dir" {
  description = "The path on the ECS cluster's EC2s into which EBS volumes are mounted."
  value       = local.mount_dir
}
