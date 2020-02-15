locals {
  versions = {

    app = {
      analytics_proxy = "git-56cf0959cb10f95fe6bdaf906ddfe0f18a313191"
      cadvisor        = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      filebeat        = "git-8fb0c6e319fc6d4eab55bf925286d8054ba1cfac"
      node_exporter   = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      shipreq         = "git-30ca9caa5139cac30465ce2d38c9890b2fbbe86b"
    }

    bastion = {
      filebeat = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      portal   = "git-317bf685eeae06b6dc382493e6e1d6f119c857ee"
    }

    nat = {
      cadvisor       = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      filebeat       = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      nat            = "git-8fb0c6e319fc6d4eab55bf925286d8054ba1cfac"
      node_exporter  = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      squid_exporter = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
    }

    ops = {
      cadvisor          = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      ecs_exporter      = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      filebeat          = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      grafana           = "git-dbb052e5f10996e420372ad03067307a0bc5d067"
      node_exporter     = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      postgres_exporter = "git-7a5aaecd3b72ac4796faf1147b27785d9341ac16"
      prometheus_biz    = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      prometheus_tech   = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
    }

  }
}
