output "altsite_cloudfront_id" {
  value = length(module.altsite) == 0 ? null : module.altsite[0].cloudfront_id
}

output "altsite_s3_bucket" {
  value = length(module.altsite) == 0 ? null : module.altsite[0].s3_bucket_name
}

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
