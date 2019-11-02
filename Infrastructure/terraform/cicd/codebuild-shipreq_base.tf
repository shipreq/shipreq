# Created by ../global
data "aws_ecr_repository" "shipreq_base" {
  name = "shipreq/base"
}

resource "aws_ecr_repository_policy" "shipreq_base" {
  repository = data.aws_ecr_repository.shipreq_base.name

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

resource "aws_codebuild_project" "shipreq_base" {
  name         = "shipreq_base"
  description  = "Docker image: shipreq/base"
  service_role = aws_iam_role.shipreq_base.arn
  tags         = local.default_tags

  environment {
    type            = "LINUX_CONTAINER"
    compute_type    = "BUILD_GENERAL1_SMALL"                                  # https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html
    image           = "aws/codebuild/amazonlinux2-x86_64-standard:1.0-1.13.0" # aws codebuild list-curated-environment-images
    privileged_mode = true

    environment_variable {
      name  = "IMAGE_URL"
      value = data.aws_ecr_repository.shipreq_base.repository_url
    }
  }

  source {
    type            = "CODECOMMIT"
    location        = aws_codecommit_repository.shipreq.clone_url_http
    git_clone_depth = 1
    buildspec       = "Images/shipreq-base/buildspec.yml"
  }

  artifacts {
    type = "NO_ARTIFACTS"
  }
}

resource "aws_cloudwatch_log_group" "shipreq_base" {
  name = "/aws/codebuild/shipreq_base"
  tags = local.default_tags
}

resource "aws_iam_role" "shipreq_base" {
  name = "codebuild-shipreq_base"
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

resource "aws_iam_role_policy" "shipreq_base" {
  role = aws_iam_role.shipreq_base.name

  policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Resource": [
        "${aws_cloudwatch_log_group.shipreq_base.arn}",
        "${aws_cloudwatch_log_group.shipreq_base.arn}/*"
      ],
      "Action": [
        "logs:CreateLogStream",
        "logs:PutLogEvents"
      ]
    },
    {
      "Effect": "Allow",
      "Resource": [ "${data.aws_ecr_repository.shipreq_base.arn}" ],
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
