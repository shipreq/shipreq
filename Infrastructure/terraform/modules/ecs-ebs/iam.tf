data "aws_caller_identity" "default" {}

data "aws_iam_policy_document" "mount" {
  statement {
    actions = [
      "ec2:DescribeVolumes",
    ]
    resources = ["*"]
  }

  statement {
    actions = [
      "ec2:AttachVolume",
    ]
    resources = flatten([
      [for v in aws_ebs_volume.drives : v.arn],
      "arn:aws:ec2:*:${data.aws_caller_identity.default.account_id}:instance/*"
    ])
  }
}

resource "aws_iam_policy" "mount" {
  name   = "${replace(var.name, "-", "_")}_ebs"
  policy = data.aws_iam_policy_document.mount.json
}

resource "aws_iam_role_policy_attachment" "mount" {
  role       = var.ec2_role.name
  policy_arn = aws_iam_policy.mount.arn
}
