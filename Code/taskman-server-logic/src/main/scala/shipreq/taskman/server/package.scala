package shipreq.taskman

import org.joda.time.DateTime
import scalaz.{\/-, ~>}
import scalaz.effect.{MonadIO, IO}
import shipreq.base.util.{ErrorTag, ErrorOr}
import shipreq.taskman.api.{MsgId, Msg, Priority}

package object server {

  case class NodeId(value: Int) extends AnyVal

  case class WorkerId(value: Short) extends AnyVal

  case class MsgHeader(id: MsgId, priority: Priority, created: DateTime) {
    // override def toString = s"MsgHeader($id,$p,new DateTime(${created.getMillis}))\n"
    override def equals(other: Any): Boolean = other match {
      case MsgHeader(id2, _, _) if id.value == id2.value => true
      case _ => false
    }
    override def hashCode: Int = (id.value ^ (id.value >>> 32)).toInt
  }

  case class MsgDetail(hdr: MsgHeader, msg: Msg, failureCount: Short) {
    assert(failureCount >= 0, s"Failure count = $failureCount")
  }

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

  type IOE[A] = IO[ErrorOr[A]]

  type SopReifier = Sop ~> IO

  implicit class OpExt[F[_], A](val op: F[A]) extends AnyVal {
    def toIO(implicit opToIo: F ~> IO): IO[A] = opToIo(op)
    def toIOE(implicit opToIo: F ~> IOE): IOE[A] = opToIo(op)
    def liftIOM[M[_]](implicit opToIo: F ~> IO, m: MonadIO[M]): M[A] = toIO.liftIO[M]
    def liftIOE(implicit opToIo: F ~> IO): IOE[A] = toIO.map(\/-(_))
  }

  implicit def MsgDetailToMsg(m: MsgDetail): Msg = m.msg
  implicit def MsgDetailToHdr(m: MsgDetail): MsgHeader = m.hdr
  implicit def MsgDetailToId(m: MsgDetail): MsgId = m.hdr.id
  implicit def MsgHeaderToId(m: MsgHeader): MsgId = m.id
}
