locals {
  app_tags = merge(local.default_tags, { Name = "${var.env}-app-cluster" })
}

resource "aws_key_pair" "app" {
  key_name   = "${var.env}-app"
  public_key = var.app_public_key
}

resource "aws_ecs_cluster" "app" {
  name = "${var.env}-app"
  tags = local.app_tags
}

resource "aws_autoscaling_group" "app" {
  name                = "${var.env}-app-cluster"
  min_size            = var.app_cluster_size
  max_size            = var.app_cluster_size
  desired_capacity    = var.app_cluster_size
  vpc_zone_identifier = [aws_subnet.private-app.id]
  tags                = [for k, v in local.app_tags : { key = k, value = v, propagate_at_launch = true }]

  launch_template {
    id      = aws_launch_template.app.id
    version = "$Latest"
  }
}

resource "aws_launch_template" "app" {
  name                   = "${var.env}-app-ecs"
  image_id               = data.aws_ssm_parameter.ami-ecs.value
  instance_type          = var.app_instance_type
  vpc_security_group_ids = [aws_security_group.app.id]
  key_name               = aws_key_pair.app.key_name
  tags                   = local.app_tags

  iam_instance_profile {
    arn = aws_iam_instance_profile.app-ecs.arn
  }

  block_device_mappings {
    device_name = "/dev/xvda"
    ebs {
      volume_size = 30 # Min size set by AMI snapshot
      volume_type = var.ecs_root_volume_type
    }
  }

  user_data = base64encode(trimspace(templatefile("${path.module}/app-ec2-init.sh", {
    cluster = aws_ecs_cluster.app.name
  })))

  tag_specifications {
    resource_type = "instance"
    tags          = local.app_tags
  }
  tag_specifications {
    resource_type = "volume"
    tags          = local.app_tags
  }
}

resource "aws_security_group" "app" {
  name   = "sg_${var.env}_app"
  vpc_id = aws_vpc.main.id
  tags   = local.app_tags

  ingress {
    protocol        = "tcp"
    from_port       = 22
    to_port         = 22
    security_groups = [aws_security_group.bastion.id]
  }

  egress {
    protocol    = "tcp"
    from_port   = 80
    to_port     = 80
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    protocol    = "tcp"
    from_port   = 443
    to_port     = 443
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_iam_role" "app-ecs" {
  name = "app_ecs_instance_role"
  path = "/${var.env}/"
  tags = local.app_tags

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

resource "aws_iam_instance_profile" "app-ecs" {
  name = "app_ecs_instance_profile"
  role = aws_iam_role.app-ecs.name
  path = aws_iam_role.app-ecs.path
}

resource "aws_iam_role_policy_attachment" "app-ecs-ec2" {
  role       = aws_iam_role.app-ecs.id
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonEC2ContainerServiceforEC2Role"
}
