package shipreq.webapp.server.logic.event

import scalaz.syntax.equal._
import shipreq.base.util.PotentialChange._
import shipreq.base.util.{ErrorMsg, PotentialChange}
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event._

object ApplyNewEvent {

  /**
   * @param projectPartial Project with the event applied, but without its history updated.
   */
  final case class Updated(projectPartial: Project,
                           event         : ActiveEvent) {

    def completeProject(ve: VerifiedEvent): Project = {
      assert(ve.event eq event)
      Project.history.modify(_ + ve)(projectPartial)
    }
  }

  type Result = PotentialChange[ErrorMsg, Updated]

  def apply(e: ActiveEvent, p1: Project): Result =
    ApplyEvent.untrusted.partialApplyUnverified(e)(p1) match {
      case \/-(p2) =>
        import Project.Equality.IgnoringHistory._
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
