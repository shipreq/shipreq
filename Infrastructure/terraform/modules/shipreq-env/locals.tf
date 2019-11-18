locals {

  default_tags = {
    createdBy = "terraform"
    env       = var.env
    terraform = "env-${var.env}"
    Name      = var.name
  }

  region = regex("^[a-z]+-[a-z]+-\\d+", var.availability_zone)

  min_healthy_percent = var.app_cluster_size > 1 ? 50 : 0

  shipreq_zone_id = var.env == "prod" ? data.aws_route53_zone.shipreq.zone_id : data.aws_route53_zone.shipwreck.zone_id
  shipreq_domain  = var.env == "prod" ? "shipreq.com" : "${var.env}.shipwreck.space"
  shipreq_url     = "https://${local.shipreq_domain}"

  bastion_zone_id = var.env == "prod" ? null : data.aws_route53_zone.shipwreck.zone_id
  bastion_domain  = var.env == "prod" ? null : "b.${var.env}.shipwreck.space"

  # TTL for DNS entries pointed at targets I expect to change rarely/never
  dns_stable_ttl = 120

  internal_domain    = "${var.env}.internal"
  internal_sd_domain = "${var.env}.sd.internal"

  nat_domain = "nat.${local.internal_domain}"

  es_domain             = "es.${local.internal_domain}"
  es_root_url           = "https://${local.es_domain}"
  es_root_url_with_port = "${local.es_root_url}:443"

  redis_domain  = "redis.${local.internal_domain}"
  redis_version = "5.0.5"

  postgres_domain = "postgres.${local.internal_domain}"

  cadvisor_path = "/cadvisor"

  # =================================================================================================================================================
  # App cluster

  # CPU shares for everything in the App cluster
  #
  # NOTE: Linux containers share unallocated CPU units with other containers on the container instance with the same ratio as their allocated amount.
  #       Therefore, these are ratios and not total shares.
  #
  # This only matters when CPU is maxed out, in which I've prioritised metric-collection over user response-time.
  # If the services are slow I want to be able to look at the metrics to figure out what needs more resources.
  app_cluster_cpu = {
    cadvisor        = 3
    filebeat        = 3
    node_exporter   = 3
    shipreq_taskman = 1
    shipreq_webapp  = 9
  }

  # Memory reservation for everything in the App cluster
  app_cluster_mem_res = {
    cadvisor        = 48
    filebeat        = 32
    node_exporter   = 24
    shipreq_taskman = 400
    shipreq_webapp  = 1200
  }

  app_cluster_ports = {
    cadvisor        = 9080
    node_exporter   = 9100
    shipreq_taskman = 9031
  }

  app_subdomain = "app"
  app_host      = "${local.app_subdomain}.${local.internal_sd_domain}"

  shipreq_webapp_sd_subdomain = "webapp"
  shipreq_webapp_sd_domain    = "${local.shipreq_webapp_sd_subdomain}.${local.internal_sd_domain}"

  # =================================================================================================================================================
  # Ops cluster

  # CPU shares for everything in the Ops cluster
  #
  # NOTE: Linux containers share unallocated CPU units with other containers on the container instance with the same ratio as their allocated amount.
  #       Therefore, these are ratios and not total shares.
  #
  # This only matters when CPU is maxed out, in which I've prioritised metric-collection over user response-time.
  # If the services are slow I want to be able to look at the metrics to figure out what needs more resources.
  ops_cluster_cpu = {
    cadvisor          = 9
    ecs_exporter      = 1
    filebeat          = 9
    grafana           = 1
    node_exporter     = 9
    postgres_exporter = 9
    prometheus_biz    = 5
    prometheus_tech   = 5
  }

  # Memory reservation for everything in the Ops cluster
  ops_cluster_mem_res = {
    cadvisor          = 48
    ecs_exporter      = 8
    filebeat          = 32
    grafana           = 32
    node_exporter     = 24
    postgres_exporter = 16
    prometheus_biz    = 64
    prometheus_tech   = 128
  }

  ops_cluster_ports = {
    cadvisor          = 8080
    ecs_exporter      = 9222
    grafana           = 3000
    node_exporter     = 9100
    postgres_exporter = 9187
    prometheus_biz    = 9091
    prometheus_tech   = 9090
  }

  ops_device = {
    prometheus_tech = "/dev/xvdf"
    prometheus_biz  = "/dev/xvdg"
  }

  ops_healthcheck = {
    startPeriod = 60
    interval    = 60
    timeout     = 10
    retries     = 2
  }

  ops_subdomain = "ops"
  ops_host      = "${local.ops_subdomain}.${local.internal_sd_domain}"

  ops_cadvisor_root_url = "http://${local.ops_host}:${local.ops_cluster_ports.cadvisor}"

  prometheus_tech_host     = local.ops_host
  prometheus_tech_root_url = "http://${local.prometheus_tech_host}:${local.ops_cluster_ports.prometheus_tech}"
  prometheus_tech_path     = "/prometheus/tech"
  prometheus_tech_url      = "${local.prometheus_tech_root_url}${local.prometheus_tech_path}/"

  prometheus_biz_host     = local.ops_host
  prometheus_biz_root_url = "http://${local.prometheus_biz_host}:${local.ops_cluster_ports.prometheus_biz}"
  prometheus_biz_path     = "/prometheus/biz"
  prometheus_biz_url      = "${local.prometheus_biz_root_url}${local.prometheus_biz_path}/"

  grafana_host     = local.ops_host
  grafana_path     = "/grafana"
  grafana_root_url = "http://${local.grafana_host}:${local.ops_cluster_ports.grafana}"
}
