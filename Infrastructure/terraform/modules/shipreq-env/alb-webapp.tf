locals {
  robots_txt = "${trimspace(file("${path.module}/robots.txt"))}\n"

  s3_logs_bucket = "shipreq-${var.env}-logs"
  alb_log_prefix = "webapp-alb"
}

data "aws_elb_service_account" "main" {}

resource "aws_route53_record" "shipreq" {
  zone_id = local.shipreq_zone_id
  name    = local.shipreq_domain
  type    = "A"
  alias {
    name                   = aws_lb.webapp.dns_name
    zone_id                = aws_lb.webapp.zone_id
    evaluate_target_health = false
  }
}

resource "aws_route53_record" "analytics_proxy" {
  zone_id = local.shipreq_zone_id
  name    = local.analytics_proxy_domain
  type    = "A"
  alias {
    name                   = aws_lb.webapp.dns_name
    zone_id                = aws_lb.webapp.zone_id
    evaluate_target_health = false
  }
}

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

resource "aws_lb_listener" "webapp-http" {
  load_balancer_arn = aws_lb.webapp.arn
  port              = 80
  protocol          = "HTTP"
  default_action {
    type = "redirect"
    redirect {
      port        = 443
      protocol    = "HTTPS"
      status_code = "HTTP_301"
    }
  }
}

resource "aws_lb_listener" "webapp-https" {
  load_balancer_arn = aws_lb.webapp.arn
  port              = 443
  protocol          = "HTTPS"
  ssl_policy        = "ELBSecurityPolicy-TLS-1-2-Ext-2018-06"
  certificate_arn   = aws_acm_certificate.shipreq.arn
  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.webapp.arn
  }
}

# http://shipreq.com/robots.txt
resource "aws_lb_listener_rule" "webapp-http-robots" {
  listener_arn = aws_lb_listener.webapp-http.arn
  condition {
    path_pattern {
      values = ["/robots.txt"]
    }
  }
  action {
    type = "fixed-response"
    fixed_response {
      content_type = "text/plain"
      message_body = local.robots_txt
      status_code  = "200"
    }
  }
}

# https://shipreq.com/robots.txt
resource "aws_lb_listener_rule" "webapp-https-robots" {
  listener_arn = aws_lb_listener.webapp-https.arn
  condition {
    path_pattern {
      values = ["/robots.txt"]
    }
  }
  action {
    type = "fixed-response"
    fixed_response {
      content_type = "text/plain"
      message_body = local.robots_txt
      status_code  = 200
    }
  }
}

# https://shipreq.com/ops/*
resource "aws_lb_listener_rule" "webapp-https-ops" {
  count        = var.shipreq_webapp_allow_ops_routes_publically ? 0 : 1
  listener_arn = aws_lb_listener.webapp-https.arn
  condition {
    path_pattern {
      values = ["/ops/*"]
    }
  }
  action {
    type = "fixed-response"
    fixed_response {
      content_type = "text/plain"
      status_code  = 404
    }
  }
}

# https://shipreq.com/ap/*
resource "aws_lb_listener_rule" "analytics_proxy" {
  listener_arn = aws_lb_listener.webapp-https.arn
  condition {
    host_header {
      values = [local.analytics_proxy_domain]
    }
  }
  action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.analytics_proxy.arn
  }
}

resource "aws_lb_target_group" "webapp" {
  name                 = "${var.env}-shipreq-webapp"
  vpc_id               = aws_vpc.main.id
  target_type          = "instance"
  port                 = 80
  protocol             = "HTTP"
  deregistration_delay = 30

  health_check {
    path                = "/ops/ok"
    protocol            = "HTTP"
    healthy_threshold   = 2
    unhealthy_threshold = 2
    interval            = 30
    timeout             = 5
    matcher             = "200"
  }

  depends_on = [aws_lb.webapp]
}

resource "aws_lb_target_group" "analytics_proxy" {
  name                 = "${var.env}-analytics-proxy"
  vpc_id               = aws_vpc.main.id
  target_type          = "instance"
  port                 = 80
  protocol             = "HTTP"
  deregistration_delay = 5

  health_check {
    path                = "/ok"
    protocol            = "HTTP"
    healthy_threshold   = 2
    unhealthy_threshold = 2
    interval            = 30
    timeout             = 5
    matcher             = "200"
  }

  depends_on = [aws_lb.webapp]
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
