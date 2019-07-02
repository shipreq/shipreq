package shipreq.webapp.base.issue

final case class IssueCount(value: Int) extends AnyVal

final case class Issues(vector: Vector[Issue]) {
  def count = IssueCount(vector.length)
}
