package shipreq.webapp.server.logic

import java.time.{Duration, Instant}
import shipreq.webapp.base.protocol.{ErrorMsg, ServerSideProc}

object Server {

  trait Time[F[_]] {
    def now: F[Instant]
    def delay[A](f: F[A], d: Duration): F[A]
  }

  trait Protocol[F[_]] {
    def createServerSideProc(p: ServerSideProc.Protocol)(localFn: p.Input => F[p.Response]): F[p.Instance]
  }

  trait Algebra[F[_]] extends Time[F] with Protocol[F]

  object ErrorMsgs {
    val ShouldNeverHappen: ErrorMsg =
      ErrorMsg("Something technical went wrong on our server.")

    val Timeout: ErrorMsg =
      ErrorMsg("Our servers are taking too long to respond. Please try again in a while.")
  }
}
