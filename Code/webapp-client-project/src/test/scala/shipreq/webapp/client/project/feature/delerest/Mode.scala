package shipreq.webapp.client.project.feature.delerest

import japgolly.microlibs.nonempty.NonEmptySet
import shipreq.webapp.base.data.{Dead, Live, ReqId}
import shipreq.webapp.base.event.{ContentRestore, Event, ReqsDelete}

sealed trait Mode {
  def unary_! : Mode
  def from: Live
  final def to = !from
  def perform(reqIds: Set[ReqId]): Option[Event]
}

object Mode {

  case object Delete extends Mode {
    override def unary_! =
      Restore
    override def from =
      Live
    override def perform(reqIds: Set[ReqId]) =
      NonEmptySet.option(reqIds).map(ReqsDelete(_, Set.empty, Vector.empty))
  }

  case object Restore extends Mode {
    override def unary_! =
      Delete
    override def from =
      Dead
    override def perform(reqIds: Set[ReqId]) =
      Some(ContentRestore(reqIds, Set.empty))
  }

}
