resource "aws_backup_vault" "sole" {
  name = var.env
  tags = local.default_tags

  lifecycle {
    ignore_changes = [recovery_points]
  }
}

locals {
  recovery_point_tags = {
    env       = local.default_tags.env
    terraform = local.default_tags.terraform
  }
}

####################################################################################################

resource "aws_backup_plan" "db" {
  count = local.enable_app_backup_db ? 1 : 0
  name  = "${var.env}-db"
  tags  = local.default_tags
  rule {
    rule_name           = "${var.env}-db"
    target_vault_name   = aws_backup_vault.sole.name
    schedule            = "cron(1 16 * * ? *)" # 16:01 every day = 2am/3am Aussie East
    recovery_point_tags = local.recovery_point_tags
    lifecycle {
      delete_after = var.postgres_backup_retention_days
    }
  }
}

resource "aws_backup_selection" "db" {
  count        = length(aws_backup_plan.db)
  name         = "${var.env}-db"
  plan_id      = aws_backup_plan.db[count.index].id
  iam_role_arn = aws_iam_role.backup.arn
  resources    = [aws_db_instance.postgres.arn]
}

####################################################################################################

resource "aws_backup_plan" "prometheus-tech" {
  count = local.enable_ops_backup_metrics ? 1 : 0
  name  = "${var.env}-prometheus-tech"
  tags  = local.default_tags
  rule {
    rule_name           = "${var.env}-prometheus-tech"
    target_vault_name   = aws_backup_vault.sole.name
    schedule            = "cron(0 17 * * ? *)" # 17:00 every day = 3am/4am Aussie East
    recovery_point_tags = local.recovery_point_tags
    lifecycle {
      delete_after = var.prometheus_tech_backup_retention_days
    }
  }
}

resource "aws_backup_selection" "prometheus-tech" {
  count        = length(aws_backup_plan.prometheus-tech)
  name         = "${var.env}-prometheus-tech"
  plan_id      = aws_backup_plan.prometheus-tech[count.index].id
  iam_role_arn = aws_iam_role.backup.arn
  selection_tag {
    type  = "STRINGEQUALS"
    key   = module.ecs_ebs_prometheus_tech.tag_key
    value = module.ecs_ebs_prometheus_tech.tag_value
  }
}

####################################################################################################

resource "aws_backup_plan" "prometheus-biz" {
  count = local.enable_ops_backup_metrics ? 1 : 0
  name  = "${var.env}-prometheus-biz"
  tags  = local.default_tags
  rule {
    rule_name           = "${var.env}-prometheus-biz"
    target_vault_name   = aws_backup_vault.sole.name
    schedule            = "cron(0 17 * * ? *)" # 17:00 every day = 3am/4am Aussie East
    recovery_point_tags = local.recovery_point_tags
    lifecycle {
      delete_after = var.prometheus_biz_backup_retention_days
    }
  }
}

resource "aws_backup_selection" "prometheus-biz" {
  count        = length(aws_backup_plan.prometheus-biz)
  name         = "${var.env}-prometheus-biz"
  plan_id      = aws_backup_plan.prometheus-biz[count.index].id
  iam_role_arn = aws_iam_role.backup.arn
  selection_tag {
    type  = "STRINGEQUALS"
    key   = module.ecs_ebs_prometheus_biz.tag_key
    value = module.ecs_ebs_prometheus_biz.tag_value
  }
}

####################################################################################################

resource "aws_iam_role" "backup" {
  name               = "${var.env}_backup_role"
  tags               = local.default_tags
  assume_role_policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": ["sts:AssumeRole"],
      "Effect": "allow",
      "Principal": {
        "Service": ["backup.amazonaws.com"]
      }
    }
  ]
}
EOB
}

resource "aws_iam_role_policy_attachment" "backup" {
  policy_arn = "arn:aws:iam::aws:policy/service-role/AWSBackupServiceRolePolicyForBackup"
  role       = aws_iam_role.backup.name
}

