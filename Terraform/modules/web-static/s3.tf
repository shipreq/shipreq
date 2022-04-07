// Note: This can't be a private bucket that only CloudFront can read.
//
// It was originally private but according to
//
//   https://www.gatsbyjs.org/docs/deploying-to-s3-cloudfront/#setting-up-cloudfront
//
// we have to serve from S3 for Gatsby to work.

resource "aws_s3_bucket" "web" {
  bucket        = var.s3_bucket_name
  force_destroy = true
  tags          = local.default_tags
}

resource "aws_s3_bucket_acl" "web" {
  acl    = "public-read"
  bucket = aws_s3_bucket.web.id
}

resource "aws_s3_bucket_policy" "web" {
  bucket = aws_s3_bucket.web.id

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

resource "aws_s3_bucket_website_configuration" "web" {
  bucket = aws_s3_bucket.web.id

  index_document {
    suffix = "index.html"
  }

  error_document {
    key = "404.html"
  }
}
