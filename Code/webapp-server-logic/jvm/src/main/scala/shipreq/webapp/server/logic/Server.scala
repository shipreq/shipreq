package shipreq.webapp.server.logic

import java.time.{Duration, Instant}
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.protocol.ServerSideProc

object Server {

  trait Time[F[_]] {
    def now: F[Instant]
  }

  trait Schedule[F[_]] extends Time[F] {
    def delay[A](f: F[A], d: Duration): F[A]
    def fork[A](f: F[A]): F[Unit]
  }

  trait Protocol[F[_]] {
    def createServerSideProc[I, O](p: ServerSideProc.Protocol[I, O])(localFn: I => F[O]): F[ServerSideProc[I, O]]
  }

  trait Session[F[_]] {
    val clientIP: F[Option[IP]]
  }

  trait Algebra[F[_]]
    extends Time[F]
       with Schedule[F]
       with Protocol[F]
       with Session[F]

  object ErrorMsgs {
    val ShouldNeverHappen: ErrorMsg =
      ErrorMsg("Something technical went wrong on our server.")

    val Timeout: ErrorMsg =
      ErrorMsg("Our servers are taking too long to respond. Please try again in a while.")
  }
}
