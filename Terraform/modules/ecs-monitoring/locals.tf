locals {

  enabled               = var.enabled
  cadvisor_enabled      = local.enabled && var.cadvisor_enabled
  filebeat_enabled      = local.enabled && var.filebeat_enabled
  node_exporter_enabled = local.enabled && var.node_exporter_enabled

  healthcheck = {
    startPeriod = 60
    interval    = 60
    timeout     = 10
    retries     = 2
  }

}
