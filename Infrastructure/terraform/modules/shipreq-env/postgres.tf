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
  maintenance_window          = "Sat:15:00-Sat:17:00" # In Aussie East: Sunday 1am/2am + 2hrs
  monitoring_interval         = var.postgres_monitoring_interval_sec
  deletion_protection         = var.deletion_protection
  skip_final_snapshot         = var.postgres_final_snapshot == ""
  final_snapshot_identifier   = (var.postgres_final_snapshot == "") ? null : var.postgres_final_snapshot
  copy_tags_to_snapshot       = false
  tags                        = local.postgres_tags
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
    description     = "Bastion access"
  }

  ingress {
    protocol    = "tcp"
    from_port   = 5432
    to_port     = 5432
    cidr_blocks = [aws_subnet.private.cidr_block]
    description = "Access from private subnet"
  }
}

resource "aws_route53_record" "postgres" {
  zone_id = aws_route53_zone.internal.zone_id
  name    = local.postgres_domain
  type    = "CNAME"
  ttl     = local.dns_stable_ttl
  records = [aws_db_instance.postgres.address]
}
