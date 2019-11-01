locals {

  default_tags = {
    createdBy = "terraform"
    env       = var.env
    terraform = "env-${var.env}"
    Name      = var.name
  }

  # TTL for DNS entries pointed at targets I expect to change rarely/never
  dns_stable_ttl = 120

  internal_domain    = "${var.env}.internal"
  internal_sd_domain = "${var.env}.sd.internal"

  nat_domain = "nat.${local.internal_domain}"

  es_domain = "es.${local.internal_domain}"
  es_url    = "https://${local.es_domain}"

  redis_domain  = "redis.${local.internal_domain}"
  redis_version = "5.0.5"

  prometheus_subdomain = "prometheus"
  prometheus_port      = 9090
  prometheus_host      = "${local.prometheus_subdomain}.${local.internal_sd_domain}"
  prometheus_url       = "http://${local.prometheus_host}:${local.prometheus_port}"

  postgres_domain = "postgres.${local.internal_domain}"
}
