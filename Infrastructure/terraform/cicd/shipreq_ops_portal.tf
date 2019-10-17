# Created by ../global
data "aws_ecr_repository" "shipreq_ops_portal" {
  name = "shipreq/ops/portal"
}

resource "aws_ecr_repository_policy" "shipreq_ops_portal" {
  repository = data.aws_ecr_repository.shipreq_ops_portal.name

  policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Principal": { "Service": "codebuild.amazonaws.com" },
      "Action": [
        "ecr:BatchCheckLayerAvailability",
        "ecr:BatchGetImage",
        "ecr:GetDownloadUrlForLayer"
      ]
    }
  ]
}
EOB
}

resource "aws_codebuild_project" "shipreq_ops_portal" {
  name         = "shipreq_ops_portal"
  description  = "Docker image: shipreq/ops/portal"
  service_role = aws_iam_role.shipreq_ops_portal.arn
  tags         = local.default_tags

  environment {
    type            = "LINUX_CONTAINER"
    compute_type    = "BUILD_GENERAL1_SMALL"                                  # https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html
    image           = "aws/codebuild/amazonlinux2-x86_64-standard:1.0-1.13.0" # aws codebuild list-curated-environment-images
    privileged_mode = true

    environment_variable {
      name  = "IMAGE_URL"
      value = data.aws_ecr_repository.shipreq_ops_portal.repository_url
    }
  }

  source {
    type            = "CODECOMMIT"
    location        = aws_codecommit_repository.shipreq.clone_url_http
    git_clone_depth = 1
    buildspec       = "Images/ops-portal/buildspec.yml"
  }

  artifacts {
    type = "NO_ARTIFACTS"
  }
}

resource "aws_cloudwatch_log_group" "shipreq_ops_portal" {
  name = "/aws/codebuild/shipreq_ops_portal"
  tags = local.default_tags
}

resource "aws_iam_role" "shipreq_ops_portal" {
  name = "codebuild-shipreq_ops_portal"
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

resource "aws_iam_role_policy" "shipreq_ops_portal" {
  role = aws_iam_role.shipreq_ops_portal.name

  policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Resource": [
        "${aws_cloudwatch_log_group.shipreq_ops_portal.arn}",
        "${aws_cloudwatch_log_group.shipreq_ops_portal.arn}/*"
      ],
      "Action": [
        "logs:CreateLogStream",
        "logs:PutLogEvents"
      ]
    },
    {
      "Effect": "Allow",
      "Resource": [ "${data.aws_ecr_repository.shipreq_ops_portal.arn}" ],
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
