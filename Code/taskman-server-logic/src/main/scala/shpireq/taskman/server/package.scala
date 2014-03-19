package shpireq.taskman

import org.joda.time.DateTime
import scalaz.{\/-, ~>}
import scalaz.effect.{MonadIO, IO}
import shipreq.base.util.ErrorOr
import shipreq.taskman.api.{Msg, Priority}

package object server {

  case class NodeId(value: Int) extends AnyVal

  case class WorkerId(value: Short) extends AnyVal

  case class MsgId(value: Long) extends AnyVal

  case class MsgHeader(id: MsgId, p: Priority, created: DateTime) {
    // override def toString = s"MsgHeader($id,$p,new DateTime(${created.getMillis}))\n"
    override def equals(other: Any): Boolean = other match {
      case MsgHeader(id2, _, _) if id.value == id2.value => true
      case _ => false
    }
    override def hashCode: Int = (id.value ^ (id.value >>> 32)).toInt
  }

  case class MsgDetail(h: MsgHeader, m: Msg, failureCount: Short) {
    assert(failureCount >= 0, s"Failure count = $failureCount")
  }

  implicit class OpExt[F[_], A](val op: F[A]) extends AnyVal {
    def toIO(implicit opToIo: F ~> IO): IO[A] = opToIo(op)
    def toIOM[M[_]](implicit opToIo: F ~> IO, m: MonadIO[M]): M[A] = toIO.liftIO[M]
    def toIOE(implicit opToIo: F ~> IO): IO[ErrorOr[A]] = toIO.map(\/-(_))
  }

}
