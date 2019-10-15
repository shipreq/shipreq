resource "aws_vpc" "main" {
  cidr_block = "${var.vpc_ip_prefix}.0.0/16"
  tags       = local.default_tags
}

// ================================================================================================

resource "aws_subnet" "public" {
  vpc_id            = aws_vpc.main.id
  cidr_block        = "${var.vpc_ip_prefix}.0.0/24"
  availability_zone = var.availability_zone
  tags              = merge(local.default_tags, { Name = "${var.env}-public" })
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

// ================================================================================================

resource "aws_subnet" "private-app" {
  vpc_id            = aws_vpc.main.id
  cidr_block        = "${var.vpc_ip_prefix}.1.0/24"
  availability_zone = var.availability_zone
  tags              = merge(local.default_tags, { Name = "${var.env}-private-app" })
}

resource "aws_route_table" "private-app" {
  vpc_id = aws_vpc.main.id
  tags   = merge(local.default_tags, { Name = "${var.env}-private-app" })

  route {
    cidr_block  = "0.0.0.0/0"
    instance_id = aws_instance.nat.id
  }
}

resource "aws_route_table_association" "private-app" {
  subnet_id      = aws_subnet.private-app.id
  route_table_id = aws_route_table.private-app.id
}

// ================================================================================================

resource "aws_subnet" "private-ops" {
  vpc_id            = aws_vpc.main.id
  cidr_block        = "${var.vpc_ip_prefix}.2.0/24"
  availability_zone = var.availability_zone
  tags              = merge(local.default_tags, { Name = "${var.env}-private-ops" })
}

resource "aws_route_table" "private-ops" {
  vpc_id = aws_vpc.main.id
  tags   = merge(local.default_tags, { Name = "${var.env}-private-ops" })

  route {
    cidr_block  = "0.0.0.0/0"
    instance_id = aws_instance.nat.id
  }
}

resource "aws_route_table_association" "private-ops" {
  subnet_id      = aws_subnet.private-ops.id
  route_table_id = aws_route_table.private-ops.id
}
