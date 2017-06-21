package shipreq.webapp.server.logic

import java.time.{Duration, Instant}
import shipreq.webapp.base.protocol.ServerSideProc

object Server {

  trait Algebra[F[_]] {
    def createServerSideProc(p: ServerSideProc.Protocol)(localFn: p.Input => F[p.Response]): F[p.Instance]
    def now: F[Instant]
    def delay[A](f: F[A], d: Duration): F[A]
  }

}
