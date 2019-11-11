locals {
  ops_tags = merge(local.default_tags, { Name = "${var.env}-ops-cluster" })
}

resource "aws_key_pair" "ops" {
  key_name   = "${var.env}-ops"
  public_key = var.ops_public_key
}

resource "aws_ecs_cluster" "ops" {
  name = "${var.env}-ops"
  tags = local.ops_tags
}

resource "aws_autoscaling_group" "ops" {
  name                = "${var.env}-ops-cluster"
  min_size            = 1
  max_size            = 1
  desired_capacity    = 1
  vpc_zone_identifier = [aws_subnet.private.id]
  tags                = [for k, v in local.ops_tags : { key = k, value = v, propagate_at_launch = true }]

  launch_template {
    id      = aws_launch_template.ops.id
    version = "$Latest"
  }
}

resource "aws_launch_template" "ops" {
  name                   = "${var.env}-ops-ecs"
  image_id               = data.aws_ssm_parameter.ami-ecs.value
  instance_type          = var.ops_instance_type
  vpc_security_group_ids = [aws_security_group.ops.id]
  key_name               = aws_key_pair.ops.key_name
  tags                   = local.ops_tags

  iam_instance_profile {
    arn = aws_iam_instance_profile.ops-ecs.arn
  }

  block_device_mappings {
    device_name = "/dev/xvda"
    ebs {
      volume_size = 30 # Min size set by AMI snapshot
      volume_type = var.ecs_root_volume_type
    }
  }

  user_data = base64encode(trimspace(templatefile("${path.module}/ops-ec2-init.sh", {
    cluster                      = aws_ecs_cluster.ops.name
    install_prometheus_tech_ebs1 = module.ecs_ebs_prometheus_tech.user_data1
    install_prometheus_tech_ebs2 = module.ecs_ebs_prometheus_tech.user_data2
    install_prometheus_biz_ebs1  = module.ecs_ebs_prometheus_biz.user_data1
    install_prometheus_biz_ebs2  = module.ecs_ebs_prometheus_biz.user_data2
    ec2_service_discovery        = module.ops_ec2_sd.user_data
  })))

  tag_specifications {
    resource_type = "instance"
    tags          = local.ops_tags
  }
  tag_specifications {
    resource_type = "volume"
    tags          = local.ops_tags
  }
}

resource "aws_security_group" "ops" {
  name   = "sg_${var.env}_ops"
  vpc_id = aws_vpc.main.id
  tags   = local.ops_tags

  ingress {
    protocol        = "tcp"
    from_port       = 0
    to_port         = 65535
    security_groups = [aws_security_group.bastion.id]
    description     = "Bastion can access everything"
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
    protocol    = "tcp"
    from_port   = local.app_cluster_ports.cadvisor
    to_port     = local.app_cluster_ports.cadvisor
    cidr_blocks = [aws_subnet.private.cidr_block]
    description = "Metrics: cadvisor"
  }

  egress {
    protocol    = "tcp"
    from_port   = local.app_cluster_ports.node_exporter
    to_port     = local.app_cluster_ports.node_exporter
    cidr_blocks = [aws_subnet.private.cidr_block]
    description = "Metrics: node_exporter"
  }

  egress {
    protocol        = "tcp"
    from_port       = 5432
    to_port         = 5432
    security_groups = [aws_security_group.postgres.id]
    description     = "Postgres"
  }
}

resource "aws_iam_role" "ops-ecs" {
  name = "${var.env}_ops_ecs_instance_role"
  tags = local.ops_tags

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

resource "aws_iam_instance_profile" "ops-ecs" {
  name = "ops_ecs_instance_profile"
  role = aws_iam_role.ops-ecs.name
  path = aws_iam_role.ops-ecs.path
}

resource "aws_iam_role_policy_attachment" "ops-ecs-ec2" {
  role       = aws_iam_role.ops-ecs.id
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonEC2ContainerServiceforEC2Role"
}

# Service discovery requires an ENI per service but there's a small ENI/instanceType limit that we exceed.
# Therefore, we use EC2 service discovery.
module "ops_ec2_sd" {
  source = "../ec2-sd"

  ec2_name_tag    = local.ops_tags.Name
  ec2_role_name   = aws_iam_role.ops-ecs.name
  name            = "${var.env}-ops"
  sd_name         = local.ops_subdomain
  sd_namespace_id = aws_service_discovery_private_dns_namespace.internal.id
}
