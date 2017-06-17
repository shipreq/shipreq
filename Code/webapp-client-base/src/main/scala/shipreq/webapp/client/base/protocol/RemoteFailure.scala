package shipreq.webapp.client.base.protocol

import scalaz.{-\/, \/, \/-}
import shipreq.webapp.base.protocol.ErrorMsg
import shipreq.webapp.client.base.data.TCB
import shipreq.webapp.client.base.lib.Logger

final case class RemoteFailure[A](value: Throwable \/ A) extends AnyVal {

  /**
   * Generic means of handling and consuming generic (protocol/ajax) failure.
   *
   * Eventually this should be replaced with something better.
   */
  def consume(implicit f: RemoteFailure.Format[A]): TCB.Failure =
    TCB.Failure(Logger(_.error(toText)))

  def consumeAnd(a: String => TCB.Failure)(implicit f: RemoteFailure.Format[A]): TCB.Failure =
    consume >> a(toText)

  def toText(implicit f: RemoteFailure.Format[A]): String =
    f format value
}

object RemoteFailure {

  implicit def covariance[A, B >: A](f: RemoteFailure[A]): RemoteFailure[B] =
    RemoteFailure(f.value)

  def exception(t: Throwable): RemoteFailure[Nothing] =
    RemoteFailure(-\/(t))

  def lift[A](a: A): RemoteFailure[A] =
    RemoteFailure(\/-(a))

  final case class Format[A](format: (Throwable \/ A) => String) extends AnyVal

  implicit val formatErrorMsg =
    Format[ErrorMsg] {
      case -\/(t) => Option(t.getMessage) match {
        case Some(m) => "AJAX error occurred: " + m
        case None    => "AJAX error occurred."
      }
      case \/-(e) => "Remote error occurred: " + e.msg
    }

}