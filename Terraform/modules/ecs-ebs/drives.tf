locals {

  drives = flatten(
    [for m in var.manifest :
      [for i in range(m.count) :
        {
          az        = m.availability_zone
          az_suffix = regex("[a-z]+$", m.availability_zone)
          az_ord    = i
          key       = "${m.availability_zone}-${i}"
        }
      ]
  ])

  drivemap = { for d in local.drives : d.key => d }

  tag_value = var.name

  tags = merge(var.tags, {
    ecs-ebs   = local.tag_value
    formatted = "n"
  })
}

resource "aws_ebs_volume" "drives" {
  for_each          = local.drivemap
  availability_zone = each.value.az
  size              = var.size
  type              = var.type
  tags              = merge({ Name = var.name }, local.tags)
  lifecycle {
    ignore_changes = [tags["formatted"]]
  }
}
