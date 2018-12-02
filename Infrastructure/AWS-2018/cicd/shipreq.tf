data "aws_ecr_repository" "taskman" {
  name = "shipreq/taskman"
}

data "aws_ecr_repository" "webapp" {
  name = "shipreq/webapp"
}

resource "aws_codebuild_project" "shipreq" {
  name         = "shipreq"
  description  = "ShipReq"
  service_role = "${aws_iam_role.shipreq.arn}"

  environment {
    type            = "LINUX_CONTAINER"
    compute_type    = "BUILD_GENERAL1_LARGE"
    image           = "${aws_ecr_repository.build.repository_url}:latest"
    privileged_mode = true
  }

  source {
    type            = "CODECOMMIT"
    location        = "${data.aws_codecommit_repository.shipreq.clone_url_http}"
    git_clone_depth = 1
    buildspec       = "Code/buildspec.yml"
  }

  artifacts {
    type = "NO_ARTIFACTS"
  }

  cache {
    type     = "S3"
    location = "${aws_s3_bucket.cache_shipreq.bucket}"
  }
}

resource "aws_s3_bucket" "cache_shipreq" {
  bucket = "codebuild-cache-shipreq"
  acl    = "private"
}

resource "aws_cloudwatch_log_group" "shipreq" {
  name = "/aws/codebuild/shipreq"
}

resource "aws_iam_role" "shipreq" {
  name = "codebuild-shipreq"

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

resource "aws_iam_role_policy" "shipreq" {
  role = "${aws_iam_role.shipreq.name}"

  policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Resource": [
        "${aws_cloudwatch_log_group.shipreq.arn}",
        "${aws_cloudwatch_log_group.shipreq.arn}/*"
      ],
      "Action": [
        "logs:CreateLogStream",
        "logs:PutLogEvents"
      ]
    },
    {
      "Effect": "Allow",
      "Resource": [
        "${aws_s3_bucket.cache_shipreq.arn}",
        "${aws_s3_bucket.cache_shipreq.arn}/*"
      ],
      "Action": [
        "s3:*"
      ]
    },
    {
      "Effect": "Allow",
      "Resource": [
        "${data.aws_ecr_repository.taskman.arn}",
        "${data.aws_ecr_repository.webapp.arn}"
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
      "Resource": [ "${aws_ecr_repository.build.arn}" ],
      "Action": [
        "ecr:BatchCheckLayerAvailability",
        "ecr:BatchGetImage",
        "ecr:DescribeImages",
        "ecr:GetDownloadUrlForLayer",
        "ecr:ListImages"
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
