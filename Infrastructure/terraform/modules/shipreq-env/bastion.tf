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
  }

  egress {
    protocol    = -1
    from_port   = 0
    to_port     = 0
    cidr_blocks = ["0.0.0.0/0"]
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

  user_data = templatefile("${path.module}/bastion-ec2-init.sh", {
    ENV            = var.env
    ENV_NAME       = var.name
    PROMETHEUS_URL = local.prometheus_url
    PORTAL_IMAGE   = data.aws_ecr_repository.shipreq_ops_portal.repository_url
  })

  root_block_device {
    volume_type = "standard"
  }

  lifecycle { create_before_destroy = true }
}

resource "aws_iam_instance_profile" "bastion" {
  name = "bastion_instance_profile"
  path = "/${var.env}/"
  role = aws_iam_role.bastion.name
}

resource "aws_iam_role" "bastion" {
  name = "bastion_instance_role"
  path = "/${var.env}/"
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

resource "aws_iam_role_policy_attachment" "bastion" {
  role       = "${aws_iam_role.bastion.name}"
  policy_arn = "${aws_iam_policy.bastion.arn}"
}

resource "aws_iam_policy" "bastion" {
  name = "bastion_policy"
  path = "/${var.env}/"

  policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Resource": [ "${data.aws_ecr_repository.shipreq_ops_portal.arn}" ],
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

# Created by ../global
data "aws_ecr_repository" "shipreq_ops_portal" {
  provider = aws.ecr
  name     = "shipreq/ops/portal"
}
