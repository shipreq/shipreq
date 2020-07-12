resource "aws_s3_bucket" "billing" {
  bucket = "shipreq-billing"
  acl    = "private"
  tags   = local.default_tags

  versioning {
    enabled = true
  }

  lifecycle {
    prevent_destroy = true
  }
}

resource "aws_s3_bucket_public_access_block" "billing" {
  bucket                  = aws_s3_bucket.billing.id
  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

data "aws_billing_service_account" "main" {}

resource "aws_s3_bucket_policy" "billing" {
  bucket = aws_s3_bucket.billing.id
  policy = <<EOB
{
  "Version": "2008-10-17",
  "Statement": [
    {
      "Sid": "AllowCURBillingACLPolicy",
      "Effect": "Allow",
      "Principal": {
        "AWS": "${data.aws_billing_service_account.main.arn}"
      },
      "Action": [
        "s3:GetBucketAcl",
        "s3:GetBucketPolicy"
      ],
      "Resource": "${aws_s3_bucket.billing.arn}"
    },
    {
      "Sid": "AllowCURPutObject",
      "Effect": "Allow",
      "Principal": {
        "AWS": "arn:aws:iam::386209384616:root"
      },
      "Action": "s3:PutObject",
      "Resource": "arn:aws:s3:::${aws_s3_bucket.billing.id}/*"
    }
  ]
}
EOB
}
