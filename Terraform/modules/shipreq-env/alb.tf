locals {
  robots_txt = "${trimspace(file("${path.module}/robots.txt"))}\n"

  s3_logs_bucket = "shipreq-${var.env}-logs"
  alb_log_prefix = "webapp-alb"
}

data "aws_elb_service_account" "main" {}

resource "aws_lb" "webapp" {
  name                       = "${var.env}-shipreq-webapp"
  internal                   = false
  load_balancer_type         = "application"
  subnets                    = [aws_subnet.public.id, aws_subnet.public_2.id]
  security_groups            = [aws_security_group.webapp-alb.id]
  enable_http2               = true
  enable_deletion_protection = var.deletion_protection
  tags                       = local.default_tags

  access_logs {
    bucket  = aws_s3_bucket.logs.bucket
    prefix  = local.alb_log_prefix
    enabled = true
  }
}

resource "aws_security_group" "webapp-alb" {
  name   = "sg_${var.env}_alb_webapp"
  vpc_id = aws_vpc.main.id
  tags   = merge(local.app_tags, { Name = "${var.env}-alb-webapp" })

  ingress {
    description = "Allow public HTTP"
    protocol    = "tcp"
    from_port   = 80
    to_port     = 80
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    description = "Allow public HTTPS"
    protocol    = "tcp"
    from_port   = 443
    to_port     = 443
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    description = "Allow public ping"
    from_port   = 8
    to_port     = 0
    protocol    = "icmp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    protocol        = "tcp"
    from_port       = 32768
    to_port         = 65535
    security_groups = [aws_security_group.app.id]
    description     = "Containers with dynamic ports"
  }

  lifecycle { create_before_destroy = true }
}

resource "aws_s3_bucket" "logs" {
  bucket        = local.s3_logs_bucket
  force_destroy = ! var.deletion_protection
  policy        = <<EOB
{
  "Id": "Policy",
  "Version": "2012-10-17",
  "Statement": [
    {
      "Principal": {
        "AWS": [ "${data.aws_elb_service_account.main.arn}" ]
      },
      "Action": [
        "s3:PutObject"
      ],
      "Effect": "Allow",
      "Resource": "arn:aws:s3:::${local.s3_logs_bucket}/${local.alb_log_prefix}/AWSLogs/*"
    }
  ]
}
EOB
}

resource "aws_s3_bucket_public_access_block" "logs" {
  bucket                  = aws_s3_bucket.logs.id
  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}
