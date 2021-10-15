locals {
  bastion_tags = merge(local.default_tags, { Name = "${var.env}-bastion" })
}

resource "aws_key_pair" "bastion" {
  key_name   = "${var.env}-bastion"
  public_key = var.bastion_public_key
}

resource "aws_eip" "bastion" {
  vpc        = true
  instance   = aws_instance.bastion.id
  depends_on = [aws_internet_gateway.public]
  tags       = local.bastion_tags
}

resource "aws_security_group" "bastion" {
  name   = "sg_${var.env}_bastion"
  vpc_id = aws_vpc.main.id
  tags   = local.bastion_tags

  ingress {
    protocol    = "tcp"
    from_port   = 36017
    to_port     = 36017
    cidr_blocks = ["0.0.0.0/0"]
    description = "Incoming SSH"
  }

  egress {
    protocol    = -1
    from_port   = 0
    to_port     = 0
    cidr_blocks = ["0.0.0.0/0"]
    description = "Full internet access"
  }
}

resource "aws_instance" "bastion" {
  ami                    = data.aws_ssm_parameter.ami-ec2.value
  availability_zone      = var.availability_zone
  instance_type          = "t3a.nano"
  subnet_id              = aws_subnet.public.id
  vpc_security_group_ids = [aws_security_group.bastion.id]
  iam_instance_profile   = aws_iam_instance_profile.bastion.id
  key_name               = aws_key_pair.bastion.key_name
  tags                   = local.bastion_tags
  volume_tags            = local.bastion_tags

  credit_specification {
    cpu_credits = "standard"
  }

  user_data = templatefile("${path.module}/bastion-ec2-init.sh", {
    CADVISOR_URL        = local.ops_cadvisor_root_url
    DNS_TTL             = "${local.dns_stable_ttl / 2}s"
    ENV                 = var.env
    ENV_NAME            = var.name
    ES_HOSTS            = local.es_root_url_with_port
    FILEBEAT_IMAGE      = "${data.aws_ecr_repository.filebeat.repository_url}:${var.bastion_filebeat_image_tag}"
    FRESHDESK_DOMAIN    = var.shipreq_taskman_freshdesk_domain
    GA_TRACKING_ID      = var.shipreq_webapp_google_analytics_id
    GRAFANA_URL         = local.grafana_root_url
    KIBANA_DEFAULT_PATH = var.kibana_default_path
    KIBANA_URL          = local.es_root_url
    PORTAL_IMAGE        = "${data.aws_ecr_repository.ops_portal.repository_url}:${var.bastion_portal_image_tag}"
    POSTGRES_DOMAIN     = local.postgres_domain
    PROMETHEUS_BIZ_URL  = local.prometheus_biz_root_url
    PROMETHEUS_TECH_URL = local.prometheus_tech_root_url
    REDIS_HOST          = local.redis_domain
    REDIS_VER           = local.redis_version
    SHIPREQ_URL         = local.shipreq_url
  })

  root_block_device {
    volume_type = "standard"
  }

  lifecycle { create_before_destroy = true }
}

resource "aws_cloudwatch_metric_alarm" "bastion-recovery" {
  alarm_name          = "${var.env}-bastion-recovery"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 1
  metric_name         = "StatusCheckFailed_System"
  namespace           = "AWS/EC2"
  period              = 60
  statistic           = "Minimum"
  threshold           = 0
  alarm_actions       = ["arn:aws:automate:${local.region}:ec2:recover"]
  dimensions          = { InstanceId = aws_instance.bastion.id }
  tags                = local.bastion_tags
}

resource "aws_iam_instance_profile" "bastion" {
  name = "${var.env}_bastion_instance_profile"
  role = aws_iam_role.bastion.name
}

resource "aws_iam_role" "bastion" {
  name = "${var.env}_bastion_instance_role"
  tags = local.bastion_tags

  assume_role_policy = <<EOB
{
  "Version": "2008-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": ["ec2.amazonaws.com"]
      },
      "Effect": "Allow"
    }
  ]
}
EOB
}

resource "aws_iam_policy" "bastion" {
  name = "${var.env}_bastion_policy"

  policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Resource": [
        "${data.aws_ecr_repository.filebeat.arn}",
        "${data.aws_ecr_repository.ops_portal.arn}"
      ],
      "Action": [
        "ecr:BatchCheckLayerAvailability",
        "ecr:GetDownloadUrlForLayer",
        "ecr:BatchGetImage"
      ]
    },
    {
      "Effect": "Allow",
      "Resource": "*",
      "Action": [
        "ecr:GetAuthorizationToken"
      ]
    }
  ]
}
EOB
}

resource "aws_iam_role_policy_attachment" "bastion" {
  role       = aws_iam_role.bastion.name
  policy_arn = aws_iam_policy.bastion.arn
}

resource "aws_iam_role_policy_attachment" "bastion-s3-tmp" {
  role       = aws_iam_role.bastion.name
  policy_arn = data.aws_iam_policy.s3_tmp_rw.arn
}

resource "aws_route53_record" "bastion" {
  count   = local.bastion_domain == null ? 0 : 1
  zone_id = local.bastion_zone_id
  name    = local.bastion_domain
  type    = "A"
  ttl     = 20
  records = [aws_eip.bastion.public_ip]
}
