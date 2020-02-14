resource "aws_codebuild_project" "images" {
  name         = "images"
  description  = "All docker images built from Dockerfiles"
  service_role = aws_iam_role.images.arn
  tags         = local.default_tags

  environment {
    type            = "LINUX_CONTAINER"
    compute_type    = "BUILD_GENERAL1_SMALL"                                  # https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html
    image           = "aws/codebuild/amazonlinux2-x86_64-standard:1.0-1.13.0" # aws codebuild list-curated-environment-images
    privileged_mode = true


    environment_variable {
      name  = "NAT_URL"
      value = data.aws_ecr_repository.nat.repository_url
    }

    environment_variable {
      name  = "OPS_CADVISOR_URL"
      value = data.aws_ecr_repository.cadvisor.repository_url
    }

    environment_variable {
      name  = "OPS_ECS_EXPORTER_URL"
      value = data.aws_ecr_repository.ecs_exporter.repository_url
    }

    environment_variable {
      name  = "OPS_FILEBEAT_URL"
      value = data.aws_ecr_repository.filebeat.repository_url
    }

    environment_variable {
      name  = "OPS_GRAFANA_URL"
      value = data.aws_ecr_repository.grafana.repository_url
    }

    environment_variable {
      name  = "OPS_NODE_EXPORTER_URL"
      value = data.aws_ecr_repository.node_exporter.repository_url
    }

    environment_variable {
      name  = "OPS_PORTAL_URL"
      value = data.aws_ecr_repository.ops_portal.repository_url
    }

    environment_variable {
      name  = "OPS_POSTGRES_EXPORTER_URL"
      value = data.aws_ecr_repository.postgres_exporter.repository_url
    }

    environment_variable {
      name  = "OPS_PROMETHEUS_BIZ_URL"
      value = data.aws_ecr_repository.prometheus-biz.repository_url
    }

    environment_variable {
      name  = "OPS_PROMETHEUS_TECH_URL"
      value = data.aws_ecr_repository.prometheus-tech.repository_url
    }

    environment_variable {
      name  = "OPS_SQUID_EXPORTER_URL"
      value = data.aws_ecr_repository.squid_exporter.repository_url
    }

    environment_variable {
      name  = "SHIPREQ_BASE_URL"
      value = data.aws_ecr_repository.shipreq_base.repository_url
    }

    environment_variable {
      name  = "DEV_BUILD_ENV_URL"
      value = aws_ecr_repository.shipreq_dev_build_env.repository_url
    }

    environment_variable {
      name  = "DEV_NODE_URL"
      value = aws_ecr_repository.shipreq_dev_node.repository_url
    }

    environment_variable {
      name  = "DEV_POSTGRES_URL"
      value = aws_ecr_repository.shipreq_dev_postgres.repository_url
    }
  }

  source_version = "refs/heads/master"

  source {
    type            = "CODECOMMIT"
    location        = aws_codecommit_repository.shipreq.clone_url_http
    git_clone_depth = 1
    buildspec       = "Docker/build-all.yml"
  }

  artifacts {
    type = "NO_ARTIFACTS"
  }
}

resource "aws_iam_role_policy" "images" {
  role = aws_iam_role.images.name

  policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Resource": [
        "${aws_cloudwatch_log_group.images.arn}",
        "${aws_cloudwatch_log_group.images.arn}/*"
      ],
      "Action": [
        "logs:CreateLogStream",
        "logs:PutLogEvents"
      ]
    },
    {
      "Effect": "Allow",
      "Resource": [
        "${aws_ecr_repository.shipreq_dev_build_env.arn}",
        "${aws_ecr_repository.shipreq_dev_node.arn}",
        "${aws_ecr_repository.shipreq_dev_postgres.arn}",
        "${data.aws_ecr_repository.cadvisor.arn}",
        "${data.aws_ecr_repository.ecs_exporter.arn}",
        "${data.aws_ecr_repository.filebeat.arn}",
        "${data.aws_ecr_repository.grafana.arn}",
        "${data.aws_ecr_repository.nat.arn}",
        "${data.aws_ecr_repository.node_exporter.arn}",
        "${data.aws_ecr_repository.ops_portal.arn}",
        "${data.aws_ecr_repository.postgres_exporter.arn}",
        "${data.aws_ecr_repository.prometheus-biz.arn}",
        "${data.aws_ecr_repository.prometheus-tech.arn}",
        "${data.aws_ecr_repository.squid_exporter.arn}",
        "${data.aws_ecr_repository.shipreq_base.arn}"
      ],
      "Action": [
        "ecr:BatchCheckLayerAvailability",
        "ecr:BatchGetImage",
        "ecr:CompleteLayerUpload",
        "ecr:DescribeImages",
        "ecr:GetDownloadUrlForLayer",
        "ecr:InitiateLayerUpload",
        "ecr:ListImages",
        "ecr:PutImage",
        "ecr:UploadLayerPart"
      ]
    },
    {
      "Effect": "Allow",
      "Resource": "*",
      "Action": [
        "codecommit:GitPull",
        "ecr:GetAuthorizationToken"
      ]
    }
  ]
}
EOB
}

resource "aws_iam_role" "images" {
  name = "codebuild-images"
  tags = local.default_tags

  assume_role_policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Principal": { "Service": "codebuild.amazonaws.com" },
      "Action": "sts:AssumeRole"
    }
  ]
}
EOB
}

resource "aws_cloudwatch_log_group" "images" {
  name = "/aws/codebuild/images"
  tags = local.default_tags
}
