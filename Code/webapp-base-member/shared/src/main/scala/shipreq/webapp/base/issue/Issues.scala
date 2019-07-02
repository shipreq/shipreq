package shipreq.webapp.base.issue

import japgolly.univeq.UnivEq

final case class IssueCount(value: Int) extends AnyVal

final case class IssueId(value: Int)
object IssueId {
  implicit def univEq: UnivEq[IssueId] = UnivEq.derive
}

final case class IssueWithId(id: IssueId, issue: Issue)
object IssueWithId {
  implicit def univEq: UnivEq[IssueWithId] = UnivEq.derive
}

final case class Issues(vector: Vector[IssueWithId]) {
  def count = IssueCount(vector.length)
}
