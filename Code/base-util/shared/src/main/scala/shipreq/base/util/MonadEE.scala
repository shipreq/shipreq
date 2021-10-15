package shipreq.base.util

import cats.Monad
import shipreq.base.util.MonadEE._

/**
  * A monad stack of a monad M, and two error types E and F.
  * E and F can conveniently represent Error and Failure.
  *
  * Usage:
  *
  * 1. Create an instance with explicit types. `val stack = MonadEE[M, E, F]`
  * 2. Import contents. `import stack._`
  * 3. Use `.toStack`, `.mapToStack`, etc. then `.unstackFailure` when done.
  */
final class MonadEE[M[_], E, F]() extends Types[M, E, F] {
  implicit def toStackExt_MA[A](m: M[A])    : StackExt_MA[M, E, F, A] = new StackExt_MA(m)
  implicit def toStackExt_EA[A](m: E \/ A)  : StackExt_EA[M, E, F, A] = new StackExt_EA(m)
  implicit def toStackExt_FA[A](m: F \/ A)  : StackExt_FA[M, E, F, A] = new StackExt_FA(m)
  implicit def toStackExt_SA[A](m: Stack[A]): StackExt_SA[M, E, F, A] = new StackExt_SA(m.underlying)
}

object MonadEE {
  def apply[M[_] : Monad, E, F]: MonadEE[M, E, F] = new MonadEE

  sealed trait Types[M[_], E, F] extends Any {
    final type StackLeft = E \/ F
    final type Stack[A] = Instance[M, E, F, A]
  }

  final case class Instance[M[_], E, F, A](underlying: M[(E \/ F) \/ A]) extends AnyVal with Types[M, E, F] {
    def flatMap[B](f: A => Stack[B])(implicit M: Monad[M]): Stack[B] =
      Instance[M, E, F, B](M.flatMap(underlying) {
        case \/-(a) => f(a).underlying
        case -\/(e) => M pure -\/(e)
      })

    def map[B](f: A => B)(implicit M: Monad[M]): Stack[B] =
      Instance(M.map(underlying)(_.map(f)))
  }

  final class StackExt_MA[M[_], E, F, A](private val self: M[A]) extends AnyVal with Types[M, E, F] {
    def toStack(implicit M: Monad[M]): Stack[A] =
      Instance(M.map(self)(\/-(_)))

    def mapToStack[B](f: A => StackLeft \/ B)(implicit M: Monad[M]): Stack[B] =
      Instance(M.map(self)(f))
  }

  final class StackExt_EA[M[_], E, F, A](private val self: E \/ A) extends AnyVal with Types[M, E, F] {
    def toStack(implicit M: Monad[M]): Stack[A] =
      Instance(M.pure(self.leftMap(-\/(_))))
  }

  final class StackExt_FA[M[_], E, F, A](private val self: F \/ A) extends AnyVal with Types[M, E, F] {
    def toStack(implicit M: Monad[M]): Stack[A] =
      Instance(M.pure(self.leftMap(\/-(_))))
  }

  final class StackExt_SA[M[_], E, F, A](private val self: M[(E \/ F) \/ A]) extends AnyVal with Types[M, E, F] {
    @nowarn
    def unstackFailure[B >: A](g: F => E \/ B)(implicit M: Monad[M]): M[E \/ B] =
      M.map(self) {
        case a: \/-[A]      => a
        case -\/(\/-(f))    => g(f)
        case -\/(e: -\/[E]) => e
      }
  }
}