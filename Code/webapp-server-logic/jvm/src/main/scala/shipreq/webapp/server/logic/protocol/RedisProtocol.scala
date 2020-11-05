package shipreq.webapp.server.logic.protocol

import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._
import shipreq.webapp.member.data.Project
import shipreq.webapp.member.event.{EventOrd, VerifiedEvent}
import shipreq.webapp.server.logic.algebra.Redis.ProjectSnapshot

object RedisProtocol {

  val picklerProjectSnapshot: SafePickler[ProjectSnapshot] = {
    import boopickle.DefaultBasic._
    import shipreq.webapp.member.protocol.binary.v1.PostEvents.picklerEventOrdLatest
    import shipreq.webapp.member.protocol.binary.v1.Rev7.picklerProject

    val p: Pickler[ProjectSnapshot] =
      new Pickler[ProjectSnapshot] {
        override def pickle(a: ProjectSnapshot)(implicit state: PickleState): Unit = {
          state.pickle(a.project)
          state.pickle(a.ord)
        }
        override def unpickle(implicit state: UnpickleState): ProjectSnapshot = {
          val project = state.unpickle[Project]
          val ord     = state.unpickle[EventOrd.Latest]
          ProjectSnapshot(project, ord)
        }
      }

    p.asV1(7).withMagicNumbers(0x713D305C, 0xB72AC2DE)
  }

  // ===================================================================================================================

  val picklerEvent: SafePickler[VerifiedEvent] = {
    import shipreq.webapp.member.protocol.binary.v1.Rev7.picklerVerifiedEvent

    // no magic numbers - overhead to high proportional to the event size, too frequent
    picklerVerifiedEvent.asV1(7)
  }

}
