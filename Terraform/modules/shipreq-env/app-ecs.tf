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
  vpc_zone_identifier = [aws_subnet.private.id]
  tags                = [for k, v in local.app_tags : { key = k, value = v, propagate_at_launch = true }]

  launch_template {
    id      = aws_launch_template.app.id
    version = "$Latest"
  }

  # The EC2s need a working internet connection to startup
  depends_on = [aws_instance.nat, aws_ecs_service.nat]
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
      volume_size = 30         # Min size set by AMI snapshot
      volume_type = "standard" # stop wasting my money
    }
  }

  user_data = base64encode(trimspace(templatefile("${path.module}/app-ec2-init.sh", {
    cluster               = aws_ecs_cluster.app.name
    ec2_service_discovery = module.app_ec2_sd.user_data
    install_nat_cert      = local.install_nat_cert
    wait_for_nat          = local.wait_for_nat
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
    description     = "Bastion can SSH in"
  }

  ingress {
    protocol    = "tcp"
    from_port   = local.ports.cadvisor
    to_port     = local.ports.cadvisor
    cidr_blocks = [aws_subnet.private.cidr_block]
    description = "Metrics: cadvisor"
  }

  ingress {
    protocol    = "tcp"
    from_port   = local.ports.node_exporter
    to_port     = local.ports.node_exporter
    cidr_blocks = [aws_subnet.private.cidr_block]
    description = "Metrics: node_exporter"
  }

  ingress {
    protocol    = "tcp"
    from_port   = local.ports.app.shipreq_taskman
    to_port     = local.ports.app.shipreq_taskman
    cidr_blocks = [aws_subnet.private.cidr_block]
    description = "Metrics: taskman"
  }

  ingress {
    protocol    = "tcp"
    from_port   = 32768
    to_port     = 65535
    cidr_blocks = [aws_subnet.public.cidr_block, aws_subnet.private.cidr_block]
    description = "Containers with dynamic ports"
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

  egress {
    protocol        = "tcp"
    from_port       = 5432
    to_port         = 5432
    security_groups = [aws_security_group.postgres.id]
    description     = "Postgres"
  }

  egress {
    protocol    = "tcp"
    from_port   = 6379
    to_port     = 6379
    cidr_blocks = [aws_subnet.private.cidr_block]
    description = "Redis"
  }
}

resource "aws_iam_role" "app-ecs" {
  name = "${var.env}_app_ecs_instance_role"
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
  name = "${var.env}_app_ecs_instance_profile"
  role = aws_iam_role.app-ecs.name
  path = aws_iam_role.app-ecs.path
}

resource "aws_iam_role_policy_attachment" "app-ecs-ec2" {
  role       = aws_iam_role.app-ecs.id
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonEC2ContainerServiceforEC2Role"
}

resource "aws_iam_role_policy_attachment" "app-ecs-ec2-s3tmp" {
  role       = aws_iam_role.app-ecs.id
  policy_arn = data.aws_iam_policy.s3_tmp_rw.arn
}

# Service discovery requires an ENI per service but there's a small ENI/instanceType limit that we exceed.
# Therefore, we use EC2 service discovery.
module "app_ec2_sd" {
  source = "../ec2-sd"

  ec2_name_tag    = local.app_tags.Name
  ec2_role_name   = aws_iam_role.app-ecs.name
  name            = "${var.env}-app"
  sd_name         = local.app_subdomain
  sd_namespace_id = aws_service_discovery_private_dns_namespace.internal.id
}
