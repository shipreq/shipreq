package shipreq.webapp.base.server

import scalaz.{-\/, \/-}
import shipreq.base.util.ValidUpdate
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event._
import shipreq.webapp.base.hash.HashRec
import ValidUpdate._

object ApplyNewEvent {

  case class Updated(project: Project, ae: ActiveEvent, ve: VerifiedEvent)

  type Result = ValidUpdate[String, Updated]

  def apply(e: ActiveEvent, p1: Project): Result =
    ApplyEvent.untrusted.apply1(e)(p1) match {
      case \/-(p2) =>
        val hrs = HashRec.changes(p1, p2)
        if (hrs.isEmpty)
          Unchanged
        else {
          val ve = VerifiedEvent(e, hrs)
          Success(Updated(p2, e, ve))
        }
      case -\/(err) => Failure(err)
    }

  def apply(r: MakeEvent.Result, p1: Project): Result =
    r.flatMap(apply(_, p1))

  def mustApply(e: ActiveEvent, p1: Project): Updated =
    apply(e, p1) match {
      case Success(a) => a
      case x          => sys error s"Success expected. Got: $x."
    }
}
