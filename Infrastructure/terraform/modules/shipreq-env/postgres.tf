locals {
  postgres_tags = merge(local.default_tags, { Name = "${var.env}-postgres" })
}

resource "aws_db_instance" "postgres" {
  identifier                  = "${var.env}-postgres"
  engine                      = "postgres"
  engine_version              = "11.5"
  instance_class              = var.postgres_instance_type
  storage_type                = "gp2"
  storage_encrypted           = true
  allocated_storage           = 20  # Minimum for gp2
  max_allocated_storage       = 100 # For auto-scaling
  username                    = "root"
  password                    = var.postgres_root_password
  availability_zone           = var.availability_zone
  db_subnet_group_name        = aws_db_subnet_group.postgres.name
  vpc_security_group_ids      = [aws_security_group.postgres.id]
  publicly_accessible         = false
  multi_az                    = false
  apply_immediately           = true
  allow_major_version_upgrade = true
  auto_minor_version_upgrade  = true
  maintenance_window          = "Sat:18:00-Sat:18:59" # in AEDT: Sunday 5am-6am
  backup_retention_period     = var.postgres_backup_retention_period
  backup_window               = "17:00-17:59" # 4am-5am
  deletion_protection         = var.postgres_deletion_protection
  skip_final_snapshot         = var.postgres_final_snapshot == ""
  final_snapshot_identifier   = (var.postgres_final_snapshot == "") ? null : var.postgres_final_snapshot
  copy_tags_to_snapshot       = false
  tags                        = local.postgres_tags

  # monitoring_interval - (Optional) The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0. Valid Values: 0, 1, 5, 10, 15, 30, 60.
}

resource "aws_db_subnet_group" "postgres" {
  name       = "${var.env}-postgres"
  subnet_ids = [aws_subnet.private.id, aws_subnet.private_2.id]
  tags       = local.postgres_tags
}

resource "aws_security_group" "postgres" {
  name   = "sg_${var.env}_postgres"
  vpc_id = aws_vpc.main.id
  tags   = local.postgres_tags

  ingress {
    protocol        = "tcp"
    from_port       = 5432
    to_port         = 5432
    security_groups = [aws_security_group.bastion.id]
  }
}

resource "aws_route53_record" "postgres" {
  zone_id = aws_route53_zone.internal.zone_id
  name    = local.postgres_domain
  type    = "CNAME"
  ttl     = local.dns_stable_ttl
  records = [aws_db_instance.postgres.address]
}
