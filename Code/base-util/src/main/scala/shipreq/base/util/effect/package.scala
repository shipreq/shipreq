package shipreq.base.util

import scalaz.{\/, Catchable, Monad}
import scalaz.effect.IO
import ErrorOr.Implicits._

package object effect {

  type IOE[A] = IO[ErrorOr[A]]

  object IOE {
    @inline def apply[A](f: => A): IOE[A] = IO(ErrorOr safe f)

    @inline def pure[A](a: A): IOE[A] = IO(ErrorOr(a))

    @inline def error[A](e: Error)               : IOE[A] = IO(e.toErrorOr)
    @inline def error[A](m: String)              : IOE[A] = error(Error(m))
    @inline def error[A](e: Throwable)           : IOE[A] = error(Error(e))
    @inline def error[A](m: String, e: Throwable): IOE[A] = error(Error(m, e))

    @inline def safeExec[A](catchIo: Error => IO[Unit])(io: IOE[A]): IO[Unit] =
      io.except(error(_)).execE(catchIo)

    val nop = apply(())
    
    implicit val ioeInstance: Monad[IOE] = ErrorOr.Scalaz.monadInstance[IO]

    implicit val ioeCatchable: Catchable[IOE] =
      new Catchable[IOE] {
        override def attempt[A](f: IOE[A]): IOE[Throwable \/ A] = f.except(error(_)) >-> \/.right
        override def fail[A](e: Throwable): IOE[A]              = IOE error e
      }
  }
}
