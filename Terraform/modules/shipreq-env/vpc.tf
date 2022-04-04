resource "aws_vpc" "main" {
  cidr_block           = "${var.vpc_ip_prefix}.0.0/16"
  enable_dns_support   = true
  enable_dns_hostnames = true
  tags                 = local.default_tags
}

resource "aws_route53_zone" "internal" {
  name = local.internal_domain
  tags = local.default_tags
  vpc {
    vpc_id = aws_vpc.main.id
  }
}

resource "aws_service_discovery_private_dns_namespace" "internal" {
  count = local.enable_service_discovery ? 1 : 0
  name  = local.internal_sd_domain
  vpc   = aws_vpc.main.id
}

// ================================================================================================

resource "aws_subnet" "public" {
  vpc_id            = aws_vpc.main.id
  cidr_block        = "${var.vpc_ip_prefix}.0.0/24" # 10.0.0.*
  availability_zone = var.availability_zone
  tags              = merge(local.default_tags, { Name = "${var.env}-public" })
}

# Required for ALB
resource "aws_subnet" "public_2" {
  vpc_id            = aws_vpc.main.id
  cidr_block        = "${var.vpc_ip_prefix}.1.0/24" # 10.0.1.*
  availability_zone = var.availability_zone_2
  tags              = merge(local.default_tags, { Name = "${var.env}-public-2" })
}

resource "aws_internet_gateway" "public" {
  vpc_id = aws_vpc.main.id
  tags   = local.default_tags
}

resource "aws_route_table" "public" {
  vpc_id = aws_vpc.main.id
  tags   = merge(local.default_tags, { Name = "${var.env}-public" })

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.public.id
  }
}

resource "aws_route_table_association" "public" {
  subnet_id      = aws_subnet.public.id
  route_table_id = aws_route_table.public.id
}

resource "aws_route_table_association" "public_2" {
  subnet_id      = aws_subnet.public_2.id
  route_table_id = aws_route_table.public.id
}

// ================================================================================================

resource "aws_subnet" "private" {
  vpc_id            = aws_vpc.main.id
  cidr_block        = "${var.vpc_ip_prefix}.4.0/24" # 10.0.4.*
  availability_zone = var.availability_zone
  tags              = merge(local.default_tags, { Name = "${var.env}-private" })
}

# Required for RDS
resource "aws_subnet" "private_2" {
  vpc_id            = aws_vpc.main.id
  cidr_block        = "${var.vpc_ip_prefix}.5.0/24" # 10.0.5.*
  availability_zone = var.availability_zone_2
  tags              = merge(local.default_tags, { Name = "${var.env}-private-2" })
}

resource "aws_route_table" "private" {
  count  = length(aws_instance.nat)
  vpc_id = aws_vpc.main.id
  tags   = merge(local.default_tags, { Name = "${var.env}-private" })

  route {
    cidr_block  = "0.0.0.0/0"
    instance_id = aws_instance.nat[count.index].id
  }
}

resource "aws_route_table_association" "private" {
  count          = length(aws_route_table.private)
  subnet_id      = aws_subnet.private.id
  route_table_id = aws_route_table.private[count.index].id
}

resource "aws_route_table_association" "private_2" {
  count          = length(aws_route_table.private)
  subnet_id      = aws_subnet.private_2.id
  route_table_id = aws_route_table.private[count.index].id
}
