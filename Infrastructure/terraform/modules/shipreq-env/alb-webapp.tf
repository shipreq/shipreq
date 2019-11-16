resource "aws_route53_record" "shipreq" {
  zone_id = data.aws_route53_zone.shipreq.zone_id
  name    = local.shipreq_domain
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

  # access_logs {
  #   bucket  = "${aws_s3_bucket.lb_logs.bucket}"
  #   prefix  = "test-lb"
  #   enabled = true
  # }
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
    interval            = 5
    timeout             = 4
    matcher             = "200"
  }
}

resource "aws_security_group" "webapp-alb" {
  name   = "${var.env}-webapp-alb"
  vpc_id = aws_vpc.main.id
  tags   = local.app_tags

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
}
