locals {
  nat_tags = merge(local.default_tags, { Name = "${var.env}-nat" })
}

resource "aws_key_pair" "nat" {
  key_name   = "${var.env}-nat"
  public_key = var.nat_public_key
}

resource "aws_instance" "nat" {
  ami                         = data.aws_ami.nat.id
  availability_zone           = var.availability_zone
  instance_type               = "t3a.nano"
  subnet_id                   = aws_subnet.public.id
  vpc_security_group_ids      = [aws_security_group.nat.id]
  associate_public_ip_address = true
  source_dest_check           = false
  key_name                    = aws_key_pair.nat.key_name
  tags                        = local.nat_tags
  volume_tags                 = local.nat_tags

  root_block_device {
    volume_type = "standard"
  }

  lifecycle { create_before_destroy = true }
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
  }

  ingress {
    protocol  = -1
    from_port = 0
    to_port   = 0
    cidr_blocks = [
      aws_subnet.private-app.cidr_block,
      aws_subnet.private-ops.cidr_block
    ]
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
