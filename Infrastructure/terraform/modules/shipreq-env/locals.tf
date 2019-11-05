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

  prometheus_tech_subdomain = "tech.prometheus"
  prometheus_tech_port      = 9090
  prometheus_tech_host      = "${local.prometheus_tech_subdomain}.${local.internal_sd_domain}"
  prometheus_tech_url       = "http://${local.prometheus_tech_host}:${local.prometheus_tech_port}"

  prometheus_biz_subdomain = "biz.prometheus"
  prometheus_biz_port      = 9091
  prometheus_biz_host      = "${local.prometheus_biz_subdomain}.${local.internal_sd_domain}"
  prometheus_biz_url       = "http://${local.prometheus_biz_host}:${local.prometheus_biz_port}"

  postgres_domain = "postgres.${local.internal_domain}"

  # CPU shares for everything in the Ops cluster
  #
  # NOTE: Linux containers share unallocated CPU units with other containers on the container instance with the same ratio as their allocated amount.
  #       Therefore, these are ratios and not total shares.
  #
  # This only matters when CPU is maxed out, in which I've prioritised metric-collection over user response-time.
  # If the services are slow I want to be able to look at the metrics to figure out what needs more resources.
  ops_cluster_cpu = {
    cadvisor          = 9
    filebeat          = 9
    grafana           = 1
    node_exporter     = 9
    postgres_exporter = 9
    prometheus_biz    = 5
    prometheus_tech   = 5
  }

  # Memory reservation for everything in the Ops cluster
  ops_cluster_mem_res = {
    cadvisor          = 50
    filebeat          = 32
    grafana           = 32
    node_exporter     = 32
    postgres_exporter = 32
    prometheus_biz    = 90
    prometheus_tech   = 100
  }

  ops_device = {
    prometheus_tech = "/dev/xvdf"
    prometheus_biz  = "/dev/xvdg"
  }
}
