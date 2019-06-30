package shipreq.webapp.base.issue

import japgolly.univeq.UnivEq
import shipreq.webapp.base.data._

trait IssueDetector {
  val detect: IssueDetector.Ctx => Unit
}

object IssueDetector {

  final case class Ctx(project       : Project,
                       add           : Issue => Unit,
                       foreachLiveReq: (() => Req => Unit) => Unit,
                       foreachLiveRcg: (() => LiveCodeGroup => Unit) => Unit,
                       foreachLiveUcs: (() => UseCaseStep.Focus => Unit) => Unit)

  /** Reference equality is the default, and it's desired. */
  implicit def univEq: UnivEq[IssueDetector] = UnivEq.force
}
