locals {
  versions = {

    app = {
      analytics_proxy = "git-10f2f46aad4df3ea32622dad2ff3e8eef4d0a81a"
      cadvisor        = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      filebeat        = "git-10f2f46aad4df3ea32622dad2ff3e8eef4d0a81a"
      node_exporter   = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      shipreq         = "git-9077585cbfe216a3b772125ed2f9dcc1e3d5e092"
    }

    bastion = {
      filebeat = "git-96d1fe4e0a6993700618b7cc936178bb05983106"
      portal   = "git-96d1fe4e0a6993700618b7cc936178bb05983106"
    }

    nat = {
      cadvisor       = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      filebeat       = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      nat            = "git-b8c297445638eefbf3f76dfbcb0922a9a9d55ee6"
      node_exporter  = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      squid_exporter = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
    }

    ops = {
      cadvisor          = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      ecs_exporter      = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      filebeat          = "git-10f2f46aad4df3ea32622dad2ff3e8eef4d0a81a"
      grafana           = "git-96d1fe4e0a6993700618b7cc936178bb05983106"
      node_exporter     = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      postgres_exporter = "git-7a5aaecd3b72ac4796faf1147b27785d9341ac16"
      prometheus_biz    = "git-10f2f46aad4df3ea32622dad2ff3e8eef4d0a81a"
      prometheus_tech   = "git-10f2f46aad4df3ea32622dad2ff3e8eef4d0a81a"
    }

  }
}
