package shipreq.webapp.base.issue

import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event._
import IssueDetector._

final case class IssueDetector(initU: Init => Unit, incrementU: Increment => Unit) {

  def init(project: Project): List[Issue] = {
    var is = List.empty[Issue]
    initU(Init(project, is ::= _))
    is
  }

//  def increment(events: Events, project: Project, issues: Issues): Changes

}

object IssueDetector {
  type Events = NonEmptyVector[Event]

  final case class Init(project: Project, add: Issue => Unit)

  final case class Increment(events : Events,
                             project: Project,
                             issues : Issues,
                             add    : Issue => Unit,
                             del    : IssueId => Unit)

//  final case class Changes(del: Set[IssueId], add: List[Issues])
//  object Changes {
//    def empty = apply(Set.empty, Nil)
//  }

  def empty = apply(_ => (), _ => ())

  def compose(detectors: IssueDetector*): IssueDetector = {
    val ds = detectors.toArray
    ds.length match {
      case 0 => empty
      case 1 => ds.head
      case _ =>
        IssueDetector(
          i => ds.foreach(_.initU(i)),
          i => ds.foreach(_.incrementU(i)))
    }
  }
}
