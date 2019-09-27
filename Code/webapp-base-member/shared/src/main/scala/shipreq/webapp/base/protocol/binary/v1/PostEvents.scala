package shipreq.webapp.base.protocol.binary.v1

import boopickle.DefaultBasic._
import java.time.Instant
import scalaz.\/
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event._

object PostEvents {
  import BaseData.{pickleDisj, picklerErrorMsg, picklerInstant}
  import BaseMemberData2.picklerProject
  import Events._

  implicit val picklerEventOrd: Pickler[EventOrd] =
    transformPickler(EventOrd.apply)(_.value)

  implicit val picklerEventOrdLatest: Pickler[EventOrd.Latest] =
    transformPickler(EventOrd.Latest.apply)(_.value)

  implicit val picklerVerifiedEvent: Pickler[VerifiedEvent] =
    new Pickler[VerifiedEvent] {
      override def pickle(a: VerifiedEvent)(implicit state: PickleState): Unit = {
        state.pickle(a.ord)
        state.pickle(a.event)
        state.pickle(a.createdAt)
      }
      override def unpickle(implicit state: UnpickleState): VerifiedEvent = {
        val ord       = state.unpickle[EventOrd]
        val event     = state.unpickle[Event]
        val createdAt = state.unpickle[Instant]
        VerifiedEvent(ord, event, createdAt)
      }
    }

  implicit val picklerVerifiedEventSeq: Pickler[VerifiedEvent.Seq] =
    iterablePickler

  implicit val picklerVerifiedEventNonEmptySeq: Pickler[VerifiedEvent.NonEmptySeq] =
    new Pickler[VerifiedEvent.NonEmptySeq] {
      override def pickle(a: VerifiedEvent.NonEmptySeq)(implicit state: PickleState): Unit = {
        state.pickle(a.head)
        state.pickle(a.tail)
      }
      override def unpickle(implicit state: UnpickleState): VerifiedEvent.NonEmptySeq = {
        val head = state.unpickle[VerifiedEvent]
        val tail = state.unpickle[VerifiedEvent.Seq]
        VerifiedEvent.NonEmptySeq(head, tail)
      }
    }

  implicit val pickleErrorMsgOrVerifiedEventSeq: Pickler[ErrorMsg \/ VerifiedEvent.Seq] =
    pickleDisj

  implicit lazy val pickleProjectAndOrd: Pickler[ProjectAndOrd] =
    new Pickler[ProjectAndOrd] {
      override def pickle(a: ProjectAndOrd)(implicit state: PickleState): Unit = {
        state.pickle(a.project)
        state.pickle(a.ord)
      }
      override def unpickle(implicit state: UnpickleState): ProjectAndOrd = {
        val project = state.unpickle[Project]
        val ord     = state.unpickle[Option[EventOrd.Latest]]
        ProjectAndOrd(project, ord)
      }
    }

}
