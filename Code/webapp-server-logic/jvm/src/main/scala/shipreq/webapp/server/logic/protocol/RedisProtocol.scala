package shipreq.webapp.server.logic.protocol

import shipreq.webapp.base.protocol.Version
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.{EventOrd, VerifiedEvent}
import shipreq.webapp.server.logic.algebra.Redis.ProjectSnapshot

object RedisProtocol {

  val picklerProjectSnapshot: SafePickler[ProjectSnapshot] = {

    val ver = Version.fromInts(2, 0) // Bump this when any of following imports change
    import boopickle.DefaultBasic._
    import shipreq.webapp.member.project.protocol.binary.v1.PostEvents.picklerEventOrdLatest
    import shipreq.webapp.member.project.protocol.binary.v2.Rev0.picklerProject

    val pickler: Pickler[ProjectSnapshot] =
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

    pickler
      .asVersion(ver)
      .withMagicNumbers(0x713D305C, 0xB72AC2DE)
  }

  // ===================================================================================================================

  val picklerEvent: SafePickler[VerifiedEvent] = {
    import shipreq.webapp.member.project.protocol.binary.v1.Rev7.picklerVerifiedEvent

    // no magic numbers - overhead to high proportional to the event size, too frequent
    picklerVerifiedEvent.asV1(7)
  }

}
