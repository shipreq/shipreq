locals {
  redis_tags = merge(local.default_tags, { Name = "${var.env}-redis" })
}

resource "aws_elasticache_cluster" "redis" {
  cluster_id           = "${var.env}-redis"
  engine               = "redis"
  node_type            = "cache.t2.micro"
  num_cache_nodes      = 1 # must be 1 unless replication_group_id is provided
  parameter_group_name = aws_elasticache_parameter_group.redis.name
  engine_version       = local.redis_version
  port                 = 6379
  availability_zone    = var.availability_zone
  subnet_group_name    = aws_elasticache_subnet_group.redis.name
  security_group_ids   = [aws_security_group.redis.id]
  tags                 = local.redis_tags
}

resource "aws_elasticache_parameter_group" "redis" {
  name   = "${var.env}-redis-params"
  family = "redis${regex("^\\d+\\.\\d+", local.redis_version)}"

  parameter {
    name  = "maxmemory-policy"
    value = "allkeys-lru"
  }
}

resource "aws_elasticache_subnet_group" "redis" {
  name       = "${var.env}-redis"
  subnet_ids = [aws_subnet.private-app.id]
}

resource "aws_security_group" "redis" {
  name   = "sg_${var.env}_redis"
  vpc_id = aws_vpc.main.id
  tags   = local.redis_tags

  ingress {
    protocol        = "tcp"
    from_port       = 6379
    to_port         = 6379
    security_groups = [aws_security_group.bastion.id]
  }
}

resource "aws_route53_record" "redis" {
  zone_id = aws_route53_zone.internal.zone_id
  name    = local.redis_domain
  type    = "CNAME"
  ttl     = local.dns_stable_ttl
  records = aws_elasticache_cluster.redis.cache_nodes[*].address
}

