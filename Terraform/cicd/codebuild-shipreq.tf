resource "aws_codebuild_project" "shipreq" {
  name         = "shipreq"
  description  = "Taskman & Webapp"
  service_role = aws_iam_role.shipreq.arn
  tags         = merge(local.default_tags, { Name = "shipreq" })

  environment {
    type                        = "LINUX_CONTAINER"
    compute_type                = "BUILD_GENERAL1_LARGE" # https://docs.aws.amazon.com/codebuild/latest/userguide/build-env-ref-compute-types.html
    image                       = "${aws_ecr_repository.shipreq_dev_build_env.repository_url}:latest"
    image_pull_credentials_type = "SERVICE_ROLE"
    privileged_mode             = true

    environment_variable {
      name  = "DOCKER_BUILDKIT"
      value = "1"
    }

    environment_variable {
      name  = "BASE_IMAGE_URL"
      value = data.aws_ecr_repository.shipreq_base.repository_url
    }

    environment_variable {
      name  = "DEV_POSTGRES_IMAGE_URL"
      value = aws_ecr_repository.shipreq_dev_postgres.repository_url
    }

    environment_variable {
      name  = "WEBAPP_IMAGE_URL"
      value = data.aws_ecr_repository.webapp.repository_url
    }

    environment_variable {
      name  = "TASKMAN_IMAGE_URL"
      value = data.aws_ecr_repository.taskman.repository_url
    }
  }

  source_version = "refs/heads/master"

  source {
    type            = "CODECOMMIT"
    location        = aws_codecommit_repository.shipreq.clone_url_http
    git_clone_depth = 1
    buildspec       = "Code/buildspec.yml"
  }

  artifacts {
    type = "NO_ARTIFACTS"
  }

  cache {
    type     = "S3"
    location = aws_s3_bucket.cache_shipreq.bucket
  }
}

resource "aws_s3_bucket" "cache_shipreq" {
  bucket = "codebuild-cache-shipreq"
  acl    = "private"
  tags   = local.default_tags
}

resource "aws_s3_bucket_public_access_block" "cache_shipreq" {
  bucket                  = aws_s3_bucket.cache_shipreq.id
  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

resource "aws_cloudwatch_log_group" "shipreq" {
  name = "/aws/codebuild/shipreq"
  tags = local.default_tags
}

resource "aws_iam_role" "shipreq" {
  name = "codebuild-shipreq"
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

resource "aws_iam_role_policy" "shipreq" {
  role = aws_iam_role.shipreq.name

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
      "Resource": [
        "${aws_ecr_repository.shipreq_dev_build_env.arn}",
        "${aws_ecr_repository.shipreq_dev_postgres.arn}",
        "${data.aws_ecr_repository.shipreq_base.arn}"
      ],
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
