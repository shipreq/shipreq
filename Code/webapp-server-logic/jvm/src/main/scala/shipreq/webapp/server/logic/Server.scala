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

  type SspResponse = ProtocolError \/ ByteBuffer

  trait Protocol[F[_]] {

    val registerServerSideProc: (String, ByteBuffer => F[SspResponse]) => F[ServerSideProcId]

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

      F.map(registerServerSideProc(p.name, process))(ServerSideProc(_, p))
    }
  }

  trait Session[F[_]] {
    val clientIP: F[Option[IP]]
    val sessionId: F[Option[SessionId]]
  }

  trait Algebra[F[_]]
    extends Time[F]
       with Schedule[F]
       with Protocol[F]
       with Session[F]

  abstract class Delegate[F[_]](underlying: Algebra[F]) extends Algebra[F] {
    override def delay[A](f: F[A], d: Duration) = underlying.delay(f, d)
    override def fork[A](fa: F[A])              = underlying.fork(fa)
    override val clientIP                       = underlying.clientIP
    override val sessionId                      = underlying.sessionId
    override def now                            = underlying.now
    override val registerServerSideProc         = (name, f) => underlying.registerServerSideProc(name, f)
  }

  object ErrorMsgs {
    val ShouldNeverHappen: ErrorMsg =
      ErrorMsg("Something technical went wrong on our server.")

    val Timeout: ErrorMsg =
      ErrorMsg("Our servers are taking too long to respond. Please try again in a while.")
  }
}
