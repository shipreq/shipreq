locals {

  cluster_enabled       = var.cluster_id != null
  cadvisor_enabled      = local.cluster_enabled && var.cadvisor_enabled
  filebeat_enabled      = local.cluster_enabled && var.filebeat_enabled
  node_exporter_enabled = local.cluster_enabled && var.node_exporter_enabled

  healthcheck = {
    startPeriod = 60
    interval    = 60
    timeout     = 10
    retries     = 2
  }

}
