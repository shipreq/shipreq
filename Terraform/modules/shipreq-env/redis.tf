locals {
  redis_tags = merge(local.default_tags, { Name = "${var.env}-redis" })
}

resource "aws_elasticache_cluster" "redis" {
  count                = local.enable_app_redis ? 1 : 0
  cluster_id           = "${var.env}-redis"
  engine               = "redis"
  node_type            = "cache.t3.micro"
  num_cache_nodes      = 1 # must be 1 unless replication_group_id is provided
  parameter_group_name = aws_elasticache_parameter_group.redis[0].name
  engine_version       = local.redis_version
  port                 = 6379
  availability_zone    = var.availability_zone
  subnet_group_name    = aws_elasticache_subnet_group.redis[0].name
  security_group_ids   = aws_security_group.redis[*].id
  tags                 = local.redis_tags
}

resource "aws_elasticache_parameter_group" "redis" {
  count  = local.enable_app_redis ? 1 : 0
  name   = "${var.env}-redis-params"
  family = "redis${regex("^\\d+\\.\\d+", local.redis_version)}"

  parameter {
    name  = "maxmemory-policy"
    value = "allkeys-lru"
  }
}

resource "aws_elasticache_subnet_group" "redis" {
  count      = local.enable_app_redis ? 1 : 0
  name       = "${var.env}-redis"
  subnet_ids = [aws_subnet.private.id]
}

resource "aws_security_group" "redis" {
  count  = local.enable_app_redis ? 1 : 0
  name   = "sg_${var.env}_redis"
  vpc_id = aws_vpc.main.id
  tags   = local.redis_tags

  dynamic "ingress" {
    for_each = aws_security_group.bastion
    content {
      protocol        = "tcp"
      from_port       = 6379
      to_port         = 6379
      security_groups = [ingress.value.id]
      description     = "Bastion access"
    }
  }

  dynamic "ingress" {
    for_each = aws_security_group.app
    content {
      protocol        = "tcp"
      from_port       = 6379
      to_port         = 6379
      security_groups = [ingress.value.id]
      description     = "Shipreq access"
    }
  }
}

resource "aws_route53_record" "redis" {
  count   = local.enable_app_redis ? 1 : 0
  zone_id = aws_route53_zone.internal.zone_id
  name    = local.redis_domain
  type    = "CNAME"
  ttl     = local.dns_stable_ttl
  records = aws_elasticache_cluster.redis[0].cache_nodes[*].address
}
