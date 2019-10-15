locals {
  bastion_tags = merge(local.default_tags, { Name = "${var.env} bastion" })
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
  key_name               = aws_key_pair.bastion.key_name
  tags                   = local.bastion_tags

  user_data = templatefile("${path.module}/bastion-ec2-init.sh", {
    env = var.env
  })

  lifecycle { create_before_destroy = true }
}
