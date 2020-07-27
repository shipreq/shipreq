output "user_data" {
  description = "Bash code that should be inserted into the user_data of discoverable EC2s."
  value       = trimspace(local.user_data)
}
