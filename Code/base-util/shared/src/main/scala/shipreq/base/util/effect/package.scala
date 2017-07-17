package shipreq.base.util

import scalaz.{Catchable, Monad, \/}
import ErrorOr.Implicits._
import shipreq.base.util.FxModule._

package object effect {

  type FxE[A] = Fx[ErrorOr[A]]

  object FxE {
    @inline def apply[A](f: => A): FxE[A] = Fx(ErrorOr safe f)

    @inline def pure[A](a: A): FxE[A] = Fx(ErrorOr(a))

    @inline def error[A](e: Error)               : FxE[A] = Fx(e.toErrorOr)
    @inline def error[A](m: String)              : FxE[A] = error(Error(m))
    @inline def error[A](e: Throwable)           : FxE[A] = error(Error(e))
    @inline def error[A](m: String, e: Throwable): FxE[A] = error(Error(m, e))

    @inline def safeExec[A](catchIo: Error => Fx[Unit])(io: FxE[A]): Fx[Unit] =
      io.except(error(_)).execE(catchIo)

    val nop = apply(())
    
    implicit val ioeInstance: Monad[FxE] = ErrorOr.Scalaz.monadInstance[Fx]

    implicit val ioeCatchable: Catchable[FxE] =
      new Catchable[FxE] {
        override def attempt[A](f: FxE[A]): FxE[Throwable \/ A] = f.except(error(_)) >-> \/.right
        override def fail[A](e: Throwable): FxE[A]              = FxE error e
      }
  }
}
