locals {
  versions = {

    app = {
      cadvisor      = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      filebeat      = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      node_exporter = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      shipreq       = "git-0dc18a0915edf61737903726035a2f1c55ac321b"
    }

    bastion = {
      filebeat = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      portal   = "git-317bf685eeae06b6dc382493e6e1d6f119c857ee"
    }

    nat = {
      cadvisor       = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      filebeat       = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      nat            = "git-2796d7d99922370dbf205ca3b5072cee740499c0"
      node_exporter  = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      squid_exporter = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
    }

    ops = {
      cadvisor          = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      ecs_exporter      = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      filebeat          = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      grafana           = "git-dbb052e5f10996e420372ad03067307a0bc5d067"
      node_exporter     = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      postgres_exporter = "git-42ba5612d1e15aadfa79995ee14458b623d7db3d"
      prometheus_biz    = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      prometheus_tech   = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
    }

  }
}
