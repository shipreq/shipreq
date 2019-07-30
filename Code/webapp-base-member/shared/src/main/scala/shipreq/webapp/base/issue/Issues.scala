package shipreq.webapp.base.issue

import shipreq.webapp.base.filter.CompiledFilter

final case class IssueCount(value: Int) extends AnyVal

final case class Issues(vector: Vector[Issue]) {

  @inline def isEmpty = vector.isEmpty

  def count = IssueCount(vector.length)

  lazy val stats = IssueStats.fromIssues(this)

  def filter(f: CompiledFilter): Issues =
    Issues(vector.filter {
      case i: Issue.BlankCustomField      => f.req(i.req)
      case i: Issue.BlankTitle            => f.req(i.req)
      case i: Issue.BlankUseCaseStep      => f.req(i.step.uc)
      case i: Issue.ConflictingTags       => f.req(i.req)
      case i: Issue.DeadIssueTagInRcg     => f.codeGroup(i.rcg)
      case i: Issue.DeadIssueTagInReq     => f.req(i.req)
      case i: Issue.DeadRefInRcg          => f.codeGroup(i.rcg)
      case i: Issue.DeadRefInReq          => f.req(i.req)
      case i: Issue.DeadTag               => f.req(i.req)
      case i: Issue.EmptyCodeGroup        => f.codeGroup(i.rcg)
      case i: Issue.ImplicationRequired   => f.req(i.req)
      case i: Issue.IssueTagInRcg         => f.codeGroup(i.rcg)
      case i: Issue.IssueTagInReq         => f.req(i.req)
      case _: Issue.UninhabitableTagField => false
    })
}
