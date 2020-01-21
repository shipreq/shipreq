locals {
  versions = {

    app = {
      cadvisor      = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      filebeat      = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      node_exporter = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      shipreq       = "git-69d6b2dd8a0d03481e60f1a573acab610fc71c2a"
    }

    bastion = {
      filebeat = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      portal   = "git-317bf685eeae06b6dc382493e6e1d6f119c857ee"
    }

    nat = {
      cadvisor       = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      filebeat       = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
      nat            = "git-d3fe30d8cced3649a16ecd3fb424861de10e28d0"
      node_exporter  = "git-c0d3aea21afe262461041b5454554ba8dc0129da"
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
