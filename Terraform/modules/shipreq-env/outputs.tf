output "bastion_host" {
  description = "The public hostname or IP of the bastion instance."
  value = (
    local.bastion_domain != null ? local.bastion_domain :
    length(aws_eip.bastion) > 0 ? aws_eip.bastion[0].public_ip :
    null
  )
}

output "public_endpoint" {
  description = "The endpoint for ShipReq, the public service."
  value       = local.shipreq_url
}
