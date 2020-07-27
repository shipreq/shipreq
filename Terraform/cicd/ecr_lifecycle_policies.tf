locals {

  ecr_lifecycle_policy_latest3 = <<EOB
{
  "rules": [
    {
      "rulePriority": 1,
      "description": "Only keep latest 3 images",
      "selection": {
        "tagStatus": "any",
        "countType": "imageCountMoreThan",
        "countNumber": 3
      },
      "action": {
        "type": "expire"
      }
    }
  ]
}
EOB

  ecr_lifecycle_policy_latest8 = <<EOB
{
  "rules": [
    {
      "rulePriority": 1,
      "description": "Only keep latest 8 images",
      "selection": {
        "tagStatus": "any",
        "countType": "imageCountMoreThan",
        "countNumber": 8
      },
      "action": {
        "type": "expire"
      }
    }
  ]
}
EOB
}

resource "aws_ecr_lifecycle_policy" "shipreq_dev_build_env" {
  repository = aws_ecr_repository.shipreq_dev_build_env.name
  policy     = local.ecr_lifecycle_policy_latest3
}

resource "aws_ecr_lifecycle_policy" "shipreq_dev_node" {
  repository = aws_ecr_repository.shipreq_dev_node.name
  policy     = local.ecr_lifecycle_policy_latest8
}

resource "aws_ecr_lifecycle_policy" "shipreq_dev_postgres" {
  repository = aws_ecr_repository.shipreq_dev_postgres.name
  policy     = local.ecr_lifecycle_policy_latest8
}

