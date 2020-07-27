resource "aws_resourcegroups_group" "cicd" {
  name = local.default_tags.terraform

  resource_query {
    query = <<EOB
{
  "ResourceTypeFilters": [
    "AWS::AllSupported"
  ],
  "TagFilters": [
    {
      "Key": "terraform",
      "Values": ["${local.default_tags.terraform}"]
    }
  ]
}
EOB
  }
}
