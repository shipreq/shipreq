locals {
  notify_phone_numbers = []
}

resource "aws_sns_topic" "cicd" {
  name = "cicd-notification"
}

resource "aws_sns_topic_policy" "cicd" {
  arn    = aws_sns_topic.cicd.arn
  policy = data.aws_iam_policy_document.cicd_sns.json
}

data "aws_iam_policy_document" "cicd_sns" {
  statement {
    actions = ["sns:Publish"]

    principals {
      type        = "Service"
      identifiers = ["codestar-notifications.amazonaws.com"]
    }

    resources = [aws_sns_topic.cicd.arn]
  }
}

resource "aws_codestarnotifications_notification_rule" "shipreq" {
  name        = "cicd-completion-shipreq"
  resource    = aws_codebuild_project.shipreq.arn
  detail_type = "FULL"
  tags        = merge(local.default_tags, { Name = "shipreq" })

  event_type_ids = [
    "codebuild-project-build-state-failed",
    "codebuild-project-build-state-succeeded",
  ]

  target {
    address = aws_sns_topic.cicd.arn
  }
}

resource "aws_codestarnotifications_notification_rule" "images" {
  name        = "cicd-completion-images"
  resource    = aws_codebuild_project.images.arn
  detail_type = "FULL"
  tags        = merge(local.default_tags, { Name = "images" })

  event_type_ids = [
    "codebuild-project-build-state-failed",
    "codebuild-project-build-state-succeeded",
  ]

  target {
    address = aws_sns_topic.cicd.arn
  }
}

resource "aws_sns_sms_preferences" "sms_prefs" {
  monthly_spend_limit = 1 // USD - need to request a limit increase to exceed this
  default_sender_id   = "ShipReq"
  default_sms_type    = "Transactional"
}

resource "aws_sns_topic_subscription" "sms" {
  for_each  = toset(local.notify_phone_numbers)
  topic_arn = aws_sns_topic.cicd.id
  protocol  = "sms"
  endpoint  = each.value
}
