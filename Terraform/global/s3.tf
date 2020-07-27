resource "aws_s3_bucket" "tmp" {
  bucket = "shipreq-tmp"
  acl    = "private"
  tags   = local.default_tags

  versioning {
    enabled = true
  }

  lifecycle {
    prevent_destroy = true
  }
}

resource "aws_s3_bucket_public_access_block" "tmp" {
  bucket                  = aws_s3_bucket.tmp.id
  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

resource "aws_iam_policy" "s3_tmp_rw" {
  name = "global_s3_tmp_rw_policy"

  policy = <<EOB
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "s3:GetBucketLocation",
        "s3:ListBucket"
      ],
      "Resource": [ "${aws_s3_bucket.tmp.arn}" ]
    },
    {
      "Effect": "Allow",
      "Action": [
        "s3:AbortMultipartUpload",
        "s3:DeleteObject",
        "s3:DeleteObjectTagging",
        "s3:DeleteObjectVersion",
        "s3:DeleteObjectVersionTagging",
        "s3:GetObject",
        "s3:GetObjectRetention",
        "s3:GetObjectTagging",
        "s3:GetObjectVersion",
        "s3:GetObjectVersionTagging",
        "s3:ListMultipartUploadParts",
        "s3:PutObject",
        "s3:PutObjectRetention",
        "s3:PutObjectTagging",
        "s3:PutObjectVersionTagging",
        "s3:ReplicateTags"
      ],
      "Resource": [ "${aws_s3_bucket.tmp.arn}/*" ]
    }
  ]
}
EOB
}

output "global_s3_tmp_rw_policy_arn" {
  value = aws_iam_policy.s3_tmp_rw.arn
}
