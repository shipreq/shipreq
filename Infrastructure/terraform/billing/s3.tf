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

// https://docs.aws.amazon.com/cur/latest/userguide/cur-s3.html
resource "aws_s3_bucket_policy" "billing" {
  bucket = aws_s3_bucket.billing.id
  policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Principal": {
        "Service": "billingreports.amazonaws.com"
      },
      "Action": [
        "s3:GetBucketAcl",
        "s3:GetBucketPolicy"
      ],
      "Resource": "${aws_s3_bucket.billing.arn}"
    },
    {
      "Effect": "Allow",
      "Principal": {
        "Service": "billingreports.amazonaws.com"
      },
      "Action": "s3:PutObject",
      "Resource": "${aws_s3_bucket.billing.arn}/*"
    }
  ]
}
EOB
}
