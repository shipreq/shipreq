resource "aws_route53_record" "analytics_proxy" {
  count   = local.enable_analytics_proxy ? 1 : 0
  zone_id = local.shipreq_zone_id
  name    = local.analytics_proxy_domain
  type    = "A"
  alias {
    name                   = aws_lb.webapp[0].dns_name
    zone_id                = aws_lb.webapp[0].zone_id
    evaluate_target_health = false
  }
}

resource "aws_lb_target_group" "analytics_proxy" {
  count                = local.enable_analytics_proxy ? 1 : 0
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

  depends_on = [aws_lb.webapp[0]]
}

# https://shipreq.com/ap/*
resource "aws_lb_listener_rule" "analytics_proxy" {
  count        = local.enable_analytics_proxy ? 1 : 0
  listener_arn = aws_lb_listener.webapp-https[0].arn
  condition {
    host_header {
      values = [local.analytics_proxy_domain]
    }
  }
  action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.analytics_proxy[0].arn
  }
}
