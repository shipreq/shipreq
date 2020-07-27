resource "aws_resourcegroups_group" "global" {
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
