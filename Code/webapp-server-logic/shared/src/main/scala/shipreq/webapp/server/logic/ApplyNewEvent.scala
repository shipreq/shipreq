package shipreq.webapp.server.logic

import scalaz.{-\/, \/-}
import scalaz.syntax.equal._
import shipreq.base.util.{ErrorMsg, PotentialChange}
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event._
import PotentialChange._

object ApplyNewEvent {

  final case class Updated(project: Project, event: ActiveEvent)

  type Result = PotentialChange[ErrorMsg, Updated]

  def apply(e: ActiveEvent, p1: Project): Result =
    ApplyEvent.untrusted.apply1(e)(p1) match {
      case \/-(p2) =>
        if (p1 === p2)
          Unchanged
        else
          Success(Updated(p2, e))
      case -\/(err) =>
        Failure(err)
    }

  def apply(r: MakeEvent.Result, p1: Project): Result =
    r.flatMap(apply(_, p1))

  def mustApply(e: ActiveEvent, p1: Project): Updated =
    apply(e, p1) match {
      case Success(a) => a
      case x          => ErrorMsg(s"Success expected. Got: $x.").throwException()
    }
}
