locals {
  versions = {

    app = {
      cadvisor      = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      filebeat      = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      node_exporter = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      shipreq       = "git-37adb99bc9dbe0cb37afde96abcd002237844aa0"
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
      grafana           = "git-6cb1f6ba6fa347b9bda12d0c5441e9c29e2627fa"
      node_exporter     = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      postgres_exporter = "git-2c211e7109c443dcba236466a5a7029d3de93cee"
      prometheus_biz    = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      prometheus_tech   = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
    }

  }
}
