module "nat_ecs_monitoring" {
  source = "../ecs-monitoring"

  name_prefix      = "${var.env}-nat"
  cluster_id       = length(aws_ecs_cluster.nat) == 0 ? null : aws_ecs_cluster.nat[0].id
  cluster_log_name = "nat"
  default_tags     = local.default_tags

  cadvisor_cpu     = local.nat_cluster_cpu.cadvisor
  cadvisor_enabled = local.enable_cadvisor
  cadvisor_image   = "${data.aws_ecr_repository.cadvisor.repository_url}:${var.nat_cadvisor_image_tag}"
  cadvisor_mem_res = local.nat_cluster_mem_res.cadvisor
  cadvisor_path    = local.cadvisor_path
  cadvisor_port    = local.ports.cadvisor

  filebeat_cpu          = local.nat_cluster_cpu.filebeat
  filebeat_enabled      = local.enable_filebeat
  filebeat_es_hosts     = local.es_root_url_with_port
  filebeat_image        = "${data.aws_ecr_repository.filebeat.repository_url}:${var.nat_filebeat_image_tag}"
  filebeat_mem_res      = local.nat_cluster_mem_res.filebeat
  filebeat_network_mode = "host"

  node_exporter_cpu     = local.nat_cluster_cpu.node_exporter
  node_exporter_enabled = local.enable_node_exporter
  node_exporter_image   = "${data.aws_ecr_repository.node_exporter.repository_url}:${var.nat_node_exporter_image_tag}"
  node_exporter_mem_res = local.nat_cluster_mem_res.node_exporter
  node_exporter_port    = local.ports.node_exporter
}
