locals {
  versions = {

    app = {
      cadvisor      = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      filebeat      = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      node_exporter = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      shipreq       = "git-b5d9399046dfcffc5ca6e6d74108ea97f7645932"
    }

    bastion = {
      filebeat = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      portal   = "git-317bf685eeae06b6dc382493e6e1d6f119c857ee"
    }

    nat = {
      filebeat       = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      nat            = "git-d3fe30d8cced3649a16ecd3fb424861de10e28d0"
      squid_exporter = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
    }

    ops = {
      cadvisor          = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      ecs_exporter      = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      filebeat          = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      grafana           = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      node_exporter     = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      postgres_exporter = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      prometheus_biz    = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      prometheus_tech   = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
    }

  }
}
