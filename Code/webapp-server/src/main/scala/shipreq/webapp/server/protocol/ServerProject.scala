package shipreq.webapp.server.protocol

import scalaz.{-\/, \/-}
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.hash.HashScheme
import shipreq.webapp.base.protocol.MakeEvent

object ServerProject { // TODO Move and rename later

  // ALWAYS use the latest to ensure that all parts of Project are hashed.
  // Alternative hash schemes exist so that Project can evolve without breaking old hashes.
  // New events should NEVER use old hash schemes.
  val hashScheme = HashScheme.latest
  val hashProject = hashScheme.hashProject

  case class State(hash: Int, project: Project)

  sealed trait Result
  case class  Updated(newState: State, ves: VerifiedEvents) extends Result
  case class  Failed(reason: String)                        extends Result
  case object NoChange                                      extends Result

  def initState(p: Project): State =
    State(hashProject hash p, p)

  def applyEvent(e: Event, state: State): Result =
    ApplyEvent.untrusted.apply1(e)(state.project) match {
      case \/-(p2) =>
        val h2 = hashProject.hash(p2)
        if (h2 == state.hash)
          NoChange
        else {
          val s2 = State(h2, p2)
          val ve = VerifiedEvent(hashScheme, h2, e)
          Updated(s2, Vector1(ve))
        }
      case -\/(err) => Failed(err)
    }

  def applyMakeEventResult(r: MakeEvent.Result, state: State): Result =
    r match {
      case MakeEvent.MadeEvent(e) => applyEvent(e, state)
      case MakeEvent.NoChange     => NoChange
      case MakeEvent.Failed(e)    => Failed(e)
    }
}
