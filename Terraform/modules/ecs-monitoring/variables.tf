variable "name_prefix" { type = string }
variable "cluster_id" { type = string }
variable "cluster_log_name" { type = string }
variable "default_tags" { type = map(any) }

variable "cadvisor_cpu" { type = number }
variable "cadvisor_image" { type = string }
variable "cadvisor_mem_res" { type = number }
variable "cadvisor_path" { type = string }
variable "cadvisor_port" { type = number }

variable "filebeat_cpu" { type = number }
variable "filebeat_es_hosts" { type = string }
variable "filebeat_image" { type = string }
variable "filebeat_mem_res" { type = number }
variable "filebeat_network_mode" {
  type    = string
  default = null
}
variable "filebeat_enabled" {
  type    = bool
  default = true
}

variable "node_exporter_cpu" { type = number }
variable "node_exporter_image" { type = string }
variable "node_exporter_mem_res" { type = number }
variable "node_exporter_port" { type = number }
