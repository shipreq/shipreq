package shipreq.webapp.client.ww.api

import boopickle.DefaultBasic._
import shipreq.webapp.base.protocol.Version
import shipreq.webapp.member.project.event.EventOrd

sealed trait WebWorkerPushCmd

object WebWorkerPushCmd {

  /** WebWorker is indicating that is missing events. */
  final case class MissingEvents(ords: NonEmptySet[EventOrd]) extends WebWorkerPushCmd

  // ===================================================================================================================

  val protocolVer = Version.fromInts(1, 0) // Bump this when any of following imports change
  import shipreq.webapp.base.protocol.binary.v1.BaseData._
  import shipreq.webapp.member.project.protocol.binary.v1.PostEvents._

  private implicit val picklerNonEmptySetEventOrd: Pickler[NonEmptySet[EventOrd]] =
    pickleNES

  private implicit val picklerMissingEvents: Pickler[MissingEvents] =
    transformPickler(MissingEvents.apply)(_.ords)

  implicit val picklerCmd: Pickler[WebWorkerPushCmd] =
    new Pickler[WebWorkerPushCmd] {
      private[this] final val KeyMissingEvents = 0

      override def pickle(a: WebWorkerPushCmd)(implicit state: PickleState): Unit =
        a match {
          case b: MissingEvents => state.enc.writeByte(KeyMissingEvents); state.pickle(b)
        }

      override def unpickle(implicit state: UnpickleState): WebWorkerPushCmd =
        state.dec.readByte match {
          case KeyMissingEvents => state.unpickle[MissingEvents]
        }
    }
}