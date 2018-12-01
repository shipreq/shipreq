resource "aws_ecr_repository" "build" {
  name = "shipreq/build"
}

resource "aws_codebuild_project" "build" {
  name         = "shipreq-build"
  description  = "Docker image: shipreq/build"
  service_role = "${aws_iam_role.build.arn}"

  environment {
    type            = "LINUX_CONTAINER"
    compute_type    = "BUILD_GENERAL1_SMALL"
    image           = "aws/codebuild/docker:17.09.0"
    privileged_mode = true

    environment_variable {
      "name"  = "IMAGE_URL"
      "value" = "${aws_ecr_repository.build.repository_url}"
    }
  }

  source {
    type            = "CODECOMMIT"
    location        = "${data.aws_codecommit_repository.shipreq.clone_url_http}"
    git_clone_depth = 1
    buildspec       = "DockerImages/build/buildspec.yml"
  }

  artifacts {
    type = "NO_ARTIFACTS"
  }
}

resource "aws_cloudwatch_log_group" "build" {
  name = "/aws/codebuild/shipreq-build"
}

resource "aws_iam_role" "build" {
  name = "codebuild-shipreq-build"

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

resource "aws_iam_role_policy" "build" {
  role = "${aws_iam_role.build.name}"

  policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Resource": [
        "${aws_cloudwatch_log_group.build.arn}",
        "${aws_cloudwatch_log_group.build.arn}/*"
      ],
      "Action": [
        "logs:CreateLogStream",
        "logs:PutLogEvents"
      ]
    },
    {
      "Effect": "Allow",
      "Resource": [ "${aws_ecr_repository.build.arn}" ],
      "Action": [
        "ecr:BatchCheckLayerAvailability",
        "ecr:CompleteLayerUpload",
        "ecr:InitiateLayerUpload",
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
