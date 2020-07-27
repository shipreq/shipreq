# Required for ElasticSearch instances to exist in this AWS account
resource "aws_iam_service_linked_role" "es" {
  aws_service_name = "es.amazonaws.com"
}
