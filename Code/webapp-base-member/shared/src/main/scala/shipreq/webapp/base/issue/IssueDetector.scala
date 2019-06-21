package shipreq.webapp.base.issue

import japgolly.univeq.UnivEq
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.EventSeqSummary
import IssueDetector._

trait IssueDetector {
  def init(i: Init): Unit
  def increment(i: Increment): Unit
}

object IssueDetector {

  final case class Action(add                : Issue => Unit,
                          foreachDirtyLiveReq: (() => Req => Unit) => Unit)

  final case class Init(action : Action,
                        project: Project)

  final case class Increment(init         : Init,
                             eventSummary : EventSeqSummary,
                             invalidateAll: () => Unit)

  /** Reference equality is the default, and it's desired. */
  implicit def univEq: UnivEq[IssueDetector] = UnivEq.force
}
