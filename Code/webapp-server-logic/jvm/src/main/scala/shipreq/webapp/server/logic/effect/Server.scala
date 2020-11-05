package shipreq.webapp.server.logic.effect

import java.time.{Duration, Instant}
import shipreq.base.util.ErrorMsg
import shipreq.webapp.server.logic.data.IP

object Server {

  trait Time[F[_]] {
    val now: F[Instant]
    def measureDuration[A](f: F[A]): F[(A, Duration)]
    def measureDuration_[A](f: F[A]): F[Duration]
  }

  trait Schedule[F[_]] extends Time[F] {
    def delay[A](f: F[A], d: Duration): F[A]
    def fork[A](f: F[A]): F[Unit]
  }

  trait Session[F[_]] {
    val clientIP: F[Option[IP]]
  }

  trait Algebra[F[_]]
    extends Time[F]
       with Schedule[F]
       with Session[F]

  abstract class Delegate[F[_]](underlying: Algebra[F]) extends Algebra[F] {
    override def delay[A](f: F[A], d: Duration) = underlying.delay(f, d)
    override def fork[A](fa: F[A])              = underlying.fork(fa)
    override def measureDuration[A](fa: F[A])   = underlying.measureDuration(fa)
    override def measureDuration_[A](fa: F[A])  = underlying.measureDuration_(fa)
    override val clientIP                       = underlying.clientIP
    override val now                            = underlying.now
  }

  object ErrorMsgs {
    val ShouldNeverHappen: ErrorMsg =
      ErrorMsg("Something technical went wrong on our server.")

    val Timeout: ErrorMsg =
      ErrorMsg("Our servers are taking too long to respond. Please try again in a while.")
  }
}
