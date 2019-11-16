output "bastion_host" {
  value       = aws_eip.bastion.public_ip
  description = "The public hostname or IP of the bastion instance."
}

output "public_endpoint" {
  value       = local.shipreq_url
  description = "The endpoint for ShipReq, the public service."
}
