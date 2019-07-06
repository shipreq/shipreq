package shipreq.webapp.base.issue

final case class IssueCount(value: Int) extends AnyVal

final case class Issues(vector: Vector[Issue]) {

  @inline def isEmpty = vector.isEmpty

  def count = IssueCount(vector.length)

  lazy val stats = IssueStats.fromIssues(this)
}
