resource "aws_cur_report_definition" "cost_and_usage_report" {
  provider   = aws.cur
  depends_on = [aws_s3_bucket_policy.billing]

  report_name                = "cost_and_usage_report"
  time_unit                  = "DAILY"
  format                     = "textORcsv"
  compression                = "GZIP"
  additional_schema_elements = ["RESOURCES"]
  s3_bucket                  = aws_s3_bucket.billing.bucket
  s3_region                  = aws_s3_bucket.billing.region
  additional_artifacts       = []
}
