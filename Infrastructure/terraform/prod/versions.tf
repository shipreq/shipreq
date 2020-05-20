locals {
  versions = {

    app = {
      analytics_proxy = "git-10f2f46aad4df3ea32622dad2ff3e8eef4d0a81a"
      cadvisor        = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      filebeat        = "git-10f2f46aad4df3ea32622dad2ff3e8eef4d0a81a"
      node_exporter   = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      shipreq         = "git-ffad62b35f2a7c253b14c8332550dfe7fe8e7ef6"
    }

    bastion = {
      filebeat = "git-96d1fe4e0a6993700618b7cc936178bb05983106"
      portal   = "git-96d1fe4e0a6993700618b7cc936178bb05983106"
    }

    nat = {
      cadvisor       = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      filebeat       = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      nat            = "git-fe6edbf3ebe0c69dda3e7fd7c3134dc461a71155"
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
