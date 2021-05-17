module "app_ecs_monitoring" {
  source = "../ecs-monitoring"

  name_prefix      = "${var.env}-app"
  cluster_id       = aws_ecs_cluster.app.id
  cluster_log_name = "app"
  default_tags     = local.default_tags

  cadvisor_cpu     = local.app_cluster_cpu.cadvisor
  cadvisor_image   = "${data.aws_ecr_repository.cadvisor.repository_url}:${var.app_cadvisor_image_tag}"
  cadvisor_mem_res = local.app_cluster_mem_res.cadvisor
  cadvisor_path    = local.cadvisor_path
  cadvisor_port    = local.ports.cadvisor

  filebeat_cpu      = local.app_cluster_cpu.filebeat
  filebeat_enabled  = local.filebeat_enabled
  filebeat_es_hosts = local.es_root_url_with_port
  filebeat_image    = "${data.aws_ecr_repository.filebeat.repository_url}:${var.app_filebeat_image_tag}"
  filebeat_mem_res  = local.app_cluster_mem_res.filebeat

  node_exporter_cpu     = local.app_cluster_cpu.node_exporter
  node_exporter_image   = "${data.aws_ecr_repository.node_exporter.repository_url}:${var.app_node_exporter_image_tag}"
  node_exporter_mem_res = local.app_cluster_mem_res.node_exporter
  node_exporter_port    = local.ports.node_exporter
}
