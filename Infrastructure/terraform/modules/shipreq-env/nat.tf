locals {
  nat_tags = merge(local.default_tags, { Name = "${var.env}-nat" })
}

resource "aws_key_pair" "nat" {
  key_name   = "${var.env}-nat"
  public_key = var.nat_public_key
}

resource "aws_instance" "nat" {
  ami                         = data.aws_ami.amazonlinux2.id
  availability_zone           = var.availability_zone
  instance_type               = "t3a.nano"
  subnet_id                   = aws_subnet.public.id
  vpc_security_group_ids      = [aws_security_group.nat.id]
  iam_instance_profile        = aws_iam_instance_profile.nat.id
  associate_public_ip_address = true
  source_dest_check           = false
  key_name                    = aws_key_pair.nat.key_name
  tags                        = local.nat_tags
  volume_tags                 = local.nat_tags

  user_data = trimspace(templatefile("${path.module}/nat-ec2-init.sh", {
    ENV            = var.env
    ES_HOSTS       = local.es_root_url_with_port
    FILEBEAT_IMAGE = data.aws_ecr_repository.filebeat.repository_url
    NAT_IMAGE      = data.aws_ecr_repository.nat.repository_url
  }))

  root_block_device {
    volume_type = "standard"
  }

  lifecycle { create_before_destroy = true }
}

resource "aws_cloudwatch_metric_alarm" "nat-recovery" {
  alarm_name          = "${var.env}-nat-recovery"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 1
  metric_name         = "StatusCheckFailed_System"
  namespace           = "AWS/EC2"
  period              = 60
  statistic           = "Minimum"
  threshold           = 0
  alarm_actions       = ["arn:aws:automate:${local.region}:ec2:recover"]
  dimensions          = { InstanceId = "${aws_instance.nat.id}" }
  tags                = local.nat_tags
}

resource "aws_iam_instance_profile" "nat" {
  name = "${var.env}_nat_instance_profile"
  role = aws_iam_role.nat.name
}

resource "aws_iam_role" "nat" {
  name = "${var.env}_nat_instance_role"
  tags = local.nat_tags

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

resource "aws_iam_policy" "nat" {
  name = "${var.env}_nat_policy"

  policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Resource": [
        "${data.aws_ecr_repository.filebeat.arn}",
        "${data.aws_ecr_repository.nat.arn}"
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

resource "aws_iam_role_policy_attachment" "nat" {
  role       = aws_iam_role.nat.name
  policy_arn = aws_iam_policy.nat.arn
}

resource "aws_security_group" "nat" {
  name   = "sg_${var.env}_nat"
  vpc_id = aws_vpc.main.id
  tags   = local.nat_tags

  ingress {
    protocol        = "tcp"
    from_port       = 22
    to_port         = 22
    security_groups = [aws_security_group.bastion.id]
    description     = "Bastion can SSH in"
  }

  ingress {
    protocol    = -1
    from_port   = 0
    to_port     = 0
    cidr_blocks = [aws_subnet.private.cidr_block]
    description = "Full access from private subnet"
  }

  egress {
    protocol    = "tcp"
    from_port   = 80
    to_port     = 80
    cidr_blocks = ["0.0.0.0/0"]
    description = "Internet HTTP"
  }

  egress {
    protocol    = "tcp"
    from_port   = 443
    to_port     = 443
    cidr_blocks = ["0.0.0.0/0"]
    description = "Internet HTTPS"
  }
}

resource "aws_route53_record" "nat" {
  zone_id = aws_route53_zone.internal.zone_id
  name    = local.nat_domain
  type    = "A"
  ttl     = local.dns_stable_ttl
  records = [aws_instance.nat.private_ip]
}
