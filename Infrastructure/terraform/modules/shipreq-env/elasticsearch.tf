locals {
  es_tags  = merge(local.default_tags, { Name = "${var.env}-elasticsearch" })
  es_count = var.enable_elasticsearch ? 1 : 0
}

resource "aws_elasticsearch_domain" "es" {
  count                 = local.es_count
  domain_name           = var.env
  elasticsearch_version = "7.1"
  tags                  = local.es_tags

  vpc_options {
    subnet_ids         = [aws_subnet.private.id]
    security_group_ids = [aws_security_group.es.id]
  }

  cluster_config {
    instance_type  = var.elasticsearch_instance_type
    instance_count = 1
  }

  ebs_options {
    ebs_enabled = true
    volume_type = var.elasticsearch_volume_type
    volume_size = var.elasticsearch_volume_size
  }

  # log_publishing_options {
  # log_type - (Required) A type of Elasticsearch log. Valid values: INDEX_SLOW_LOGS, SEARCH_SLOW_LOGS, ES_APPLICATION_LOGS
  # cloudwatch_log_group_arn - (Required) ARN of the Cloudwatch log group to which log needs to be published.
  # enabled - (Optional, Default: true) Specifies whether given log publishing option is enabled or not.
  # }

  # snapshot_options {
  #   automated_snapshot_start_hour = "${var.snapshot_start}"
  # }

  depends_on = [aws_iam_service_linked_role.es]
}

resource "aws_iam_service_linked_role" "es" {
  aws_service_name = "es.amazonaws.com"
}

resource "aws_elasticsearch_domain_policy" "es" {
  count           = local.es_count
  domain_name     = aws_elasticsearch_domain.es[count.index].domain_name
  access_policies = <<EOB
  {
    "Version": "2012-10-17",
    "Statement": [
      {
        "Effect": "Allow",
        "Action": "es:*",
        "Principal": { "AWS": [ "*" ] },
        "Resource": "${aws_elasticsearch_domain.es[count.index].arn}/*"
      }
    ]
  }
EOB
}

resource "aws_security_group" "es" {
  name   = "sg_${var.env}_elasticsearch"
  vpc_id = aws_vpc.main.id
  tags   = local.es_tags

  ingress {
    protocol        = "tcp"
    from_port       = 443
    to_port         = 443
    security_groups = [aws_security_group.bastion.id]
    description     = "Bastion access"
  }

  ingress {
    protocol        = "tcp"
    from_port       = 443
    to_port         = 443
    security_groups = [aws_security_group.nat.id]
    description     = "NAT access"
  }

  ingress {
    protocol    = "tcp"
    from_port   = 443
    to_port     = 443
    cidr_blocks = [aws_subnet.private.cidr_block]
    description = "Access from private subnet"
  }
}

resource "aws_route53_record" "es" {
  count   = local.es_count
  zone_id = aws_route53_zone.internal.zone_id
  name    = local.es_domain
  type    = "CNAME"
  ttl     = local.dns_stable_ttl
  records = [aws_elasticsearch_domain.es[count.index].endpoint]
}
