// Note: This can't be a private bucket that only CloudFront can read.
//
// It was originally private but according to
//
//   https://www.gatsbyjs.org/docs/deploying-to-s3-cloudfront/#setting-up-cloudfront
//
// we have to serve from S3 for Gatsby to work.

resource "aws_s3_bucket" "web" {
  acl           = "public-read"
  bucket        = var.s3_bucket_name
  force_destroy = true
  tags          = local.default_tags

  website {
    index_document = "index.html"
    error_document = "404.html"
  }

  policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Sid": "PublicRead_GetObject",
      "Effect": "Allow",
      "Principal": "*",
      "Action": [
        "s3:GetObject"
      ],
      "Resource": [
        "arn:aws:s3:::${var.s3_bucket_name}/*"
      ]
    }
  ]
}
  EOF
}

resource "aws_s3_bucket_public_access_block" "web" {
  bucket = aws_s3_bucket.web.id
}
