package shipreq.webapp.server.logic

import boopickle.{PickleImpl, UnpickleImpl}
import java.nio.ByteBuffer
import java.time.{Duration, Instant}
import scalaz.{-\/, Monad, \/, \/-}
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.protocol._

object Server {

  trait Time[F[_]] {
    def now: F[Instant]
  }

  trait Schedule[F[_]] extends Time[F] {
    def delay[A](f: F[A], d: Duration): F[A]
    def fork[A](f: F[A]): F[Unit]
  }

  sealed trait ProtocolError
  final case class RequestPickleError(exception: Throwable) extends ProtocolError
  final case class ResponsePickleError(exception: Throwable) extends ProtocolError

  trait Protocol[F[_]] {

    val registerServerSideProc: (ByteBuffer => F[ProtocolError \/ ByteBuffer]) => F[ServerSideProcId]

    final def createServerSideProc[I, O](p: ServerSideProc.Protocol[I, O])
                                        (localFn: I => F[O])
                                        (implicit F: Monad[F]): F[ServerSideProc[I, O]] = {

      // very frequent use - avoid implicit syntax and EitherT - code is small so not a big readability loss

      val parseInput: ByteBuffer => F[RequestPickleError \/ I] =
        bb => F.point {
          try {
            \/-(UnpickleImpl(p.pickleInput).fromBytes(bb))
          } catch {
            case e: Throwable => -\/(RequestPickleError(e))
          }
        }

      val encodeOutput: O => F[ProtocolError \/ ByteBuffer] =
        o => F.point {
          try {
            \/-(PickleImpl.intoBytes(o)(implicitly, p.pickleOutput))
          } catch {
            case e: Throwable => -\/(ResponsePickleError(e))
          }
        }

      val process: ByteBuffer => F[ProtocolError \/ ByteBuffer] =
        bb => F.bind(parseInput(bb)) {
          case \/-(i)    => F.bind(localFn(i))(encodeOutput)
          case e@ -\/(_) => F pure e
        }

      F.map(registerServerSideProc(process))(ServerSideProc(_, p))
    }
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
