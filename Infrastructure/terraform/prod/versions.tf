locals {

  versions_common = {
    cadvisor      = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
    node_exporter = "git-25d47cbe77ac423485a372ebf3c8f7ceffae3aad"
  }

  versions = {

    app = {
      analytics_proxy = "git-10f2f46aad4df3ea32622dad2ff3e8eef4d0a81a"
      cadvisor        = local.versions_common.cadvisor
      filebeat        = "git-10f2f46aad4df3ea32622dad2ff3e8eef4d0a81a"
      node_exporter   = local.versions_common.node_exporter
      shipreq         = "git-14ae61cff8b318bad38d00c07e88ad3bfa86e8e4"
    }

    bastion = {
      filebeat = "git-96d1fe4e0a6993700618b7cc936178bb05983106"
      portal   = "git-96d1fe4e0a6993700618b7cc936178bb05983106"
    }

    nat = {
      cadvisor       = local.versions_common.cadvisor
      filebeat       = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      nat            = "git-b8c297445638eefbf3f76dfbcb0922a9a9d55ee6"
      node_exporter  = local.versions_common.node_exporter
      squid_exporter = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
    }

    ops = {
      cadvisor          = local.versions_common.cadvisor
      ecs_exporter      = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      filebeat          = "git-10f2f46aad4df3ea32622dad2ff3e8eef4d0a81a"
      grafana           = "git-96d1fe4e0a6993700618b7cc936178bb05983106"
      node_exporter     = local.versions_common.node_exporter
      postgres_exporter = "git-7a5aaecd3b72ac4796faf1147b27785d9341ac16"
      prometheus_biz    = "git-1712990505efa7866db192e08e7bcc6983ac2f8a"
      prometheus_tech   = "git-1712990505efa7866db192e08e7bcc6983ac2f8a"
    }

  }
}
