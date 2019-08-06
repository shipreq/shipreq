package shipreq.webapp.base.issue

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq.UnivEq
import shipreq.webapp.base.data.{ReqCodeGroupId, ReqId}
import shipreq.webapp.base.filter.CompiledFilter

final case class IssueCount(value: Int) extends AnyVal

final case class Issues(vector: Vector[Issue]) {
  import Issues._

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
      case _: Issue.ManualIssue           => false // TODO I'd expect text-based filters to apply to manual issue text
    })

  lazy val bySource: BySource = {
    var byReq  = UnivEq.emptyMap[ReqId, ForSource]
    var byRcg  = UnivEq.emptyMap[ReqCodeGroupId, ForSource]
    var config = ForSource.empty

    def addReq(i: Issue, id: ReqId         ): Unit = byReq = byReq.initAndModifyValue(id, ForSource.empty, _.add(i))
    def addRcg(i: Issue, id: ReqCodeGroupId): Unit = byRcg = byRcg.initAndModifyValue(id, ForSource.empty, _.add(i))

    vector.foreach {
      case i: Issue.BlankCustomField      => addReq(i, i.req.id)
      case i: Issue.BlankTitle            => addReq(i, i.req.id)
      case i: Issue.BlankUseCaseStep      => addReq(i, i.step.useCaseId)
      case i: Issue.ConflictingTags       => addReq(i, i.req.id)
      case i: Issue.DeadIssueTagInRcg     => addRcg(i, i.rcg.id)
      case i: Issue.DeadIssueTagInReq     => addReq(i, i.req.id)
      case i: Issue.DeadRefInRcg          => addRcg(i, i.rcg.id)
      case i: Issue.DeadRefInReq          => addReq(i, i.req.id)
      case i: Issue.DeadTag               => addReq(i, i.req.id)
      case i: Issue.EmptyCodeGroup        => addRcg(i, i.rcg.id)
      case i: Issue.ImplicationRequired   => addReq(i, i.req.id)
      case i: Issue.IssueTagInRcg         => addRcg(i, i.rcg.id)
      case i: Issue.IssueTagInReq         => addReq(i, i.req.id)
      case i: Issue.UninhabitableTagField => config = config.add(i)
      case _: Issue.ManualIssue           => ()
    }

    BySource(byReq, byRcg, config)
  }
}

object Issues {

  final case class BySource(byReq : Map[ReqId, ForSource],
                            byRcg : Map[ReqCodeGroupId, ForSource],
                            config: ForSource) {

    def apply(id: ReqId): ForSource =
      byReq.getOrElse(id, ForSource.empty)

    def apply(id: ReqCodeGroupId): ForSource =
      byRcg.getOrElse(id, ForSource.empty)
  }

  final case class ForSource(issues: List[Issue], categories: Set[IssueCategory]) {
    def add(i: Issue): ForSource =
      ForSource(i :: issues, categories + i.category)
  }
  object ForSource {
    val empty = apply(Nil, Set.empty)
  }

}