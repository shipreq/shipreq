package shipreq.webapp.shared.data

final case class CustomReqTypes(
  rev: delta.Rev,
  data: Seq[CustomReqType])

final case class Project(customReqTypes: CustomReqTypes) {
  def rev = customReqTypes.rev
}
