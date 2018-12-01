terraform {
  backend "s3" {
    bucket = "shipreq-terraform-state"
    key    = "cicd.tfstate"
    region = "ap-southeast-2"
  }
}
