package shipreq.taskman.server.logic

import japgolly.univeq.UnivEq
import java.time.{Duration, Instant}
import shipreq.base.util.ArticulateError
import shipreq.taskman.api.{Msg, MsgId, Priority}

final case class NodeId(value: Int) extends AnyVal

final case class WorkerId(value: Short) extends AnyVal

final case class MsgHeader(id: MsgId, priority: Priority, created: Instant) {
  // override def toString = s"MsgHeader($id,$p,new DateTime(${created.getMillis}))\n"
  override def equals(other: Any): Boolean = other match {
    case MsgHeader(id2, _, _) if id.value == id2.value => true
    case _ => false
  }
  override def hashCode: Int = (id.value ^ (id.value >>> 32)).toInt
}

object MsgHeader {
  implicit def univEq: UnivEq[MsgHeader] = UnivEq.derive
}

final case class MsgDetail(hdr: MsgHeader, msg: Msg, failureCount: Int) {
  assert(failureCount >= 0, s"Failure count = $failureCount")

  @inline def id = hdr.id
  @inline def priority = hdr.priority

  override lazy val toString =
    s"MsgDetail($hdr, ${msg.toString.replace("\n", "\\n")}, $failureCount)"
}

object MsgDetail {
  implicit def univEq: UnivEq[MsgDetail] = UnivEq.derive
}

final case class AssignmentTrustPeriod(value: Duration) extends AnyVal

/** Indication that an error is deliberate.
  * Deliberate errors may occur during testing and diagnosis.
  *
  * Support will not be notified when deliberate errors occur.
  */
case object Deliberate extends ArticulateError.Tag
