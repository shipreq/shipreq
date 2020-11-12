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
    matcher             = "200"
    healthy_threshold   = 2  # The number of consecutive health checks successes required before considering an unhealthy target healthy.
    unhealthy_threshold = 4  # The number of consecutive health check failures required before considering the target unhealthy.
    interval            = 30 # The approximate amount of time, in seconds, between health checks of an individual target. [5,300]
    timeout             = 10 # The amount of time, in seconds, during which no response means a failed health check. [2,120]
    # There's also the health_check_grace_period_seconds setting in aws_ecs_service.shipreq_webapp
  }

  depends_on = [aws_lb.webapp]
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
