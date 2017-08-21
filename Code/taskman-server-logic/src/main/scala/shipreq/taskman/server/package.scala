package shipreq.taskman

import java.time.{Duration, Instant}
import scalaz.{\/-, ~>}
import shipreq.base.util.ErrorTag
import shipreq.base.util.FxModule._
import shipreq.base.util.effect.FxE
import shipreq.taskman.api.{MsgId, Msg, Priority}

package object server {

  case class NodeId(value: Int) extends AnyVal

  case class WorkerId(value: Short) extends AnyVal

  case class MsgHeader(id: MsgId, priority: Priority, created: Instant) {
    // override def toString = s"MsgHeader($id,$p,new DateTime(${created.getMillis}))\n"
    override def equals(other: Any): Boolean = other match {
      case MsgHeader(id2, _, _) if id.value == id2.value => true
      case _ => false
    }
    override def hashCode: Int = (id.value ^ (id.value >>> 32)).toInt
  }

  case class MsgDetail(hdr: MsgHeader, msg: Msg, failureCount: Short) {
    assert(failureCount >= 0, s"Failure count = $failureCount")

    override lazy val toString =
      s"MsgDetail($hdr, ${msg.toString.replace("\n", "\\n")}, $failureCount)"
  }

  final case class AssignmentTrustPeriod(value: Duration) extends AnyVal

  /**
   * Indication that an error is deterministic and will always occur.
   * In other words, there is no point in retrying because whatever caused this error the first time is guaranteed to
   * occur next time.
   */
  case object Deterministic extends ErrorTag

  /**
   * Indication that an error is deliberate.
   * Deliberate errors will likely only occur during testing and diagnosis.
   * Support will not be notified when deliberate errors occur.
   */
  case object Deliberate extends ErrorTag

  type SopReifier = ServerOp ~> Fx

  implicit class OpExt[F[_], A](val op: F[A]) extends AnyVal {
    def toFx(implicit opToIo: F ~> Fx): Fx[A] = opToIo(op)
    def toFxE(implicit opToIo: F ~> FxE): FxE[A] = opToIo(op)
    def liftFxE(implicit opToIo: F ~> Fx): FxE[A] = toFx.map(\/-(_))
  }

  implicit def MsgDetailToMsg(m: MsgDetail): Msg = m.msg
  implicit def MsgDetailToHdr(m: MsgDetail): MsgHeader = m.hdr
  implicit def MsgDetailToId(m: MsgDetail): MsgId = m.hdr.id
  implicit def MsgHeaderToId(m: MsgHeader): MsgId = m.id
}
