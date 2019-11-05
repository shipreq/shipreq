locals {
  name_ = replace(var.name, "-", "_")
}

resource "aws_service_discovery_service" "main" {
  name = var.sd_name

  dns_config {
    namespace_id   = var.sd_namespace_id
    routing_policy = "MULTIVALUE"

    dns_records {
      ttl  = 30
      type = "A"
    }
  }
}

resource "aws_iam_policy" "main" {
  name = "${local.name_}_ec2_sd_policy"

  policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Sid": "",
      "Effect": "Allow",
      "Action": [
        "ec2:DescribeInstances",
        "route53:GetHealthCheck",
        "route53:CreateHealthCheck",
        "route53:UpdateHealthCheck",
        "route53:ChangeResourceRecordSets"
      ],
      "Resource": "*"
    },
    {
      "Sid": "",
      "Effect": "Allow",
      "Action": [
        "servicediscovery:RegisterInstance"
      ],
      "Resource": "*",
      "Condition": {
        "ForAllValues:StringEquals": {
          "servicediscovery:ServiceArn": "${aws_service_discovery_service.main.arn}"
        }
      }
    }
  ]
}
EOB
}

resource "aws_iam_role_policy_attachment" "main" {
  role       = var.ec2_role_name
  policy_arn = aws_iam_policy.main.arn
}
