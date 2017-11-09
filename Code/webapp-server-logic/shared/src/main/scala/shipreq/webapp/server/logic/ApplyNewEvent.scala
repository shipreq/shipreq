package shipreq.webapp.server.logic

import scalaz.{-\/, \/-}
import shipreq.base.util.PotentialChange
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event._
import shipreq.webapp.base.hash.{HashRecs, HashSchemes}
import PotentialChange._

object ApplyNewEvent {

  final case class Updated(project: Project, event: ActiveEvent, hashRecs: HashRecs)

  type Result = PotentialChange[String, Updated]

  def apply(e: ActiveEvent, p1: Project): Result =
    ApplyEvent.untrusted.apply1(e)(p1) match {
      case \/-(p2) =>
        val hrs = HashSchemes.latest.changes(p1, p2)
        if (hrs.isEmpty)
          Unchanged
        else
          Success(Updated(p2, e, hrs))
      case -\/(err) =>
        Failure(err)
    }

  def apply(r: MakeEvent.Result, p1: Project): Result =
    r.flatMap(apply(_, p1))

  def mustApply(e: ActiveEvent, p1: Project): Updated =
    apply(e, p1) match {
      case Success(a) => a
      case x          => sys error s"Success expected. Got: $x."
    }
}
