resource "aws_s3_bucket" "terraform_state" {
  bucket = "shipreq-terraform-state"
  acl    = "private"

  versioning {
    enabled = true
  }

  lifecycle {
    prevent_destroy = true
  }

  tags = {
    createdBy = "terraform"
    env       = "n/a"
    terraform = "init"
  }
}
