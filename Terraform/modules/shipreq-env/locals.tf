# Note regarding CPU shares
# =========================
#
# Linux containers share unallocated CPU units with other containers on the container instance with the same ratio as their allocated amount.
# Therefore, these are ratios and not total shares. The values only matter when CPU is maxed out.

locals {

  default_tags = {
    createdBy = "terraform"
    env       = var.env
    terraform = "env-${var.env}"
    Name      = var.name
  }

  enable_analytics_proxy       = var.enable_app
  enable_app_alb               = var.enable_app
  enable_app_backup_db         = local.enable_app_webapp
  enable_app_cdn               = local.enable_app_ec2 && var.shipreq_cdn_subdomain != null
  enable_app_ec2               = var.enable_app
  enable_app_redis             = var.enable_app && var.enable_redis
  enable_app_taskman           = var.enable_app && var.enable_db_dependant_services
  enable_app_webapp            = var.enable_app && var.enable_db_dependant_services
  enable_bastion               = var.enable_bastion
  enable_cadvisor              = local.enable_metrics_collection && var.enable_cadvisor
  enable_elasticsearch         = var.enable_elasticsearch
  enable_filebeat              = local.enable_elasticsearch && var.enable_filebeat
  enable_metrics_collection    = var.enable_metrics_collection && local.enable_ops_prometheus
  enable_metrics_services      = var.enable_metrics_services && local.enable_ops_ec2
  enable_nat                   = local.enable_app_ec2 || local.enable_ops_ec2
  enable_nat_metrics           = local.enable_nat && local.enable_metrics_collection
  enable_node_exporter         = local.enable_metrics_collection && var.enable_node_exporter
  enable_ops_backup_metrics    = local.enable_ops_prometheus
  enable_ops_ec2               = var.enable_ops
  enable_ops_ecs_exporter      = local.enable_metrics_collection && var.enable_ecs_exporter
  enable_ops_grafana           = local.enable_metrics_services && var.enable_db_dependant_services
  enable_ops_postgres_exporter = local.enable_metrics_collection && var.enable_db_dependant_services
  enable_ops_prometheus        = local.enable_metrics_services
  enable_service_discovery     = local.enable_app_ec2 || local.enable_ops_ec2

  is_prod = var.env == "prod"

  seconds_in_a_year = 31556952

  nat_cert = file("${path.module}/../../../Docker/nat/ssl/squid.crt")

  # I'm not sure this is necessary but it's a logical thing to do
  install_nat_cert = <<EOB
update-ca-trust force-enable
echo '${local.nat_cert}' > /etc/pki/ca-trust/source/anchors/nat.crt
update-ca-trust extract
EOB

  wait_for_nat = <<EOB
nat_check=
while [ -z "$nat_check" ]; do
  echo "Waiting for NAT..."
  nat_check="$(curl -s -m 7 duckduckgo.com | fgrep -v ERR_ACCESS_DENIED || sleep 3)"
done
echo "NAT confirmed."
EOB

  region = regex("^[a-z]+-[a-z]+-\\d+", var.availability_zone)

  shipreq_zone_id = local.is_prod ? data.aws_route53_zone.shipreq.zone_id : data.aws_route53_zone.shipwreck.zone_id
  shipreq_domain  = local.is_prod ? "shipreq.com" : "${var.env}.shipwreck.space"
  shipreq_url     = "https://${local.shipreq_domain}"

  analytics_proxy_subdomain = "ap"
  analytics_proxy_domain    = "${local.analytics_proxy_subdomain}.${local.shipreq_domain}"
  analytics_proxy_url       = "https://${local.analytics_proxy_domain}"

  shipreq_cdn_domain = var.shipreq_cdn_subdomain == null ? null : "${var.shipreq_cdn_subdomain}.${local.shipreq_domain}"
  shipreq_cdn_url    = var.shipreq_cdn_subdomain == null ? null : "https://${local.shipreq_cdn_domain}"

  # TTL for DNS entries pointed at targets I expect to change rarely/never
  dns_stable_ttl = 120

  internal_domain    = "${var.env}.internal"
  internal_sd_domain = "${var.env}.sd.internal"

  nat_domain = "nat.${local.internal_domain}"

  bastion_zone_id = (!local.enable_bastion || local.is_prod) ? null : data.aws_route53_zone.shipwreck.zone_id
  bastion_domain  = (!local.enable_bastion || local.is_prod) ? null : "b.${var.env}.shipwreck.space"

  es_domain             = "es.${local.internal_domain}"
  es_root_url           = "https://${local.es_domain}"
  es_root_url_with_port = "${local.es_root_url}:443"

  redis_domain  = "redis.${local.internal_domain}"
  redis_version = "5.0.5"

  postgres_domain = "postgres.${local.internal_domain}"

  cadvisor_path = "/cadvisor"

  ports = {
    cadvisor      = 9080
    node_exporter = 9100

    app = {
      shipreq_taskman = 9031
    }

    nat = {
      squid_exporter = 9301
    }

    ops = {
      ecs_exporter      = 9222
      grafana           = 3000
      postgres_exporter = 9187
      prometheus_biz    = 9091
      prometheus_tech   = 9090
    }
  }

  # =================================================================================================================================================
  # NAT cluster

  nat_cluster_cpu = {
    cadvisor       = 1
    filebeat       = 1
    node_exporter  = 3
    nat            = 9
    squid_exporter = 1
  }

  nat_cluster_mem_res = {
    cadvisor       = 48
    filebeat       = 32
    node_exporter  = 24
    nat            = 100
    squid_exporter = 8
  }

  # =================================================================================================================================================
  # App cluster

  app_cluster_cpu = {
    analytics_proxy = 8
    cadvisor        = 3
    filebeat        = 3
    node_exporter   = 3
    shipreq_taskman = 1
    shipreq_webapp  = 9
  }

  app_cluster_mem_res = {
    analytics_proxy = 40
    cadvisor        = 48
    filebeat        = 32
    node_exporter   = 24
    shipreq_taskman = 160
    shipreq_webapp  = 800
  }

  app_cluster_size = !var.enable_app ? 0 : var.app_cluster_size

  app_min_healthy_percent = local.app_cluster_size > 1 ? 50 : 0

  app_subdomain = "app"
  app_host      = "${local.app_subdomain}.${local.internal_sd_domain}"

  shipreq_webapp_sd_subdomain = "webapp"
  shipreq_webapp_sd_domain    = "${local.shipreq_webapp_sd_subdomain}.${local.internal_sd_domain}"

  # =================================================================================================================================================
  # Ops cluster

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

  ops_cluster_mem_res = {
    cadvisor          = 48
    ecs_exporter      = 8
    filebeat          = 32
    grafana           = 32
    node_exporter     = 24
    postgres_exporter = 8
    prometheus_biz    = 64
    prometheus_tech   = 128
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

  ops_cadvisor_root_url = "http://${local.ops_host}:${local.ports.cadvisor}"

  prometheus_tech_host     = local.ops_host
  prometheus_tech_root_url = "http://${local.prometheus_tech_host}:${local.ports.ops.prometheus_tech}"
  prometheus_tech_path     = "/prometheus/tech"
  prometheus_tech_url      = "${local.prometheus_tech_root_url}${local.prometheus_tech_path}/"

  prometheus_biz_host     = local.ops_host
  prometheus_biz_root_url = "http://${local.prometheus_biz_host}:${local.ports.ops.prometheus_biz}"
  prometheus_biz_path     = "/prometheus/biz"
  prometheus_biz_url      = "${local.prometheus_biz_root_url}${local.prometheus_biz_path}/"

  grafana_host     = local.ops_host
  grafana_path     = "/grafana"
  grafana_root_url = "http://${local.grafana_host}:${local.ports.ops.grafana}"
}
