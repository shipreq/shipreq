locals {

  versions_common = {
    cadvisor      = "git-ccb541835e7129c9c4b649bdf624f4320990bf95"
    node_exporter = "git-25d47cbe77ac423485a372ebf3c8f7ceffae3aad"
  }

  versions = {

    app = {
      analytics_proxy = "git-d64a217fd2b96837959e40c37d322d5667cddc0b"
      cadvisor        = local.versions_common.cadvisor
      filebeat        = "git-10f2f46aad4df3ea32622dad2ff3e8eef4d0a81a"
      node_exporter   = local.versions_common.node_exporter
      shipreq         = "git-9e4774310c97e3436bde2779bb6f74cb008c55f2"
    }

    bastion = {
      filebeat = "git-96d1fe4e0a6993700618b7cc936178bb05983106"
      portal   = "git-96d1fe4e0a6993700618b7cc936178bb05983106"
    }

    nat = {
      cadvisor       = local.versions_common.cadvisor
      filebeat       = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      nat            = "git-af7543ea0f942a79531b8a970c74702c1a7bd96b"
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
      prometheus_biz    = "git-c5c257ffcdde27c30e7f5b8c4a77e1ce120b491e"
      prometheus_tech   = "git-c5c257ffcdde27c30e7f5b8c4a77e1ce120b491e"
    }

  }
}
