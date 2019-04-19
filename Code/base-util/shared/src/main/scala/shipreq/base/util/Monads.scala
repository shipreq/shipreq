package shipreq.base.util

import scalaz.{-\/, Monad, \/, \/-}

/** Using monad transformers directly, or manually stacking monads is
  * fucking verbose and annoying as hell - very hard to read/write in Scala.
  *
  * These are little wrappers with simple, concise DSLs.
  */
object Monads {

  final class FDisj[F[_], E, A](val value: F[E \/ A]) extends AnyVal {
    def flatMap[B](f: A => FDisj[F, E, B])(implicit F: Monad[F]): FDisj[F, E, B] =
      new FDisj(F.bind(value) {
        case \/-(a) => f(a).value
        case e: -\/[E] => F.pure(e)
      })

    def map[B](f: A => B)(implicit F: Monad[F]): FDisj[F, E, B] =
      new FDisj(F.map(value)(_ map f))
  }

  object FDisj {
    def apply[F[_]: Monad, E] = new Companion[F, E]

    final class Companion[F[_], E]()(implicit F: Monad[F]) {
      type Result[A] = FDisj[F, E, A]

      val unit: Result[Unit] =
        new FDisj(F.pure(\/-(())))

      def apply[A](x: F[E \/ A]): Result[A] =
        new FDisj(x)

      def lift[A](x: E \/ A): Result[A] =
        new FDisj(F.pure(x))

      def fail[A](e: E): Result[A] =
        lift(-\/(e))

      def rightF[A](f: F[A]): Result[A] =
        new FDisj(F.map(f)(\/-(_)))

      def ensure(cond: Boolean, e: => E): Result[Unit] =
        if (cond) unit else fail(e)

      def option[A](o: Option[A], e: => E): Result[A] =
        new FDisj(F.pure(o match {
          case Some(a) => \/-(a)
          case None    => -\/(e)
        }))

      def optionF[A](f: F[Option[A]], e: => E): Result[A] =
        new FDisj(F.map(f) {
          case Some(a) => \/-(a)
          case None    => -\/(e)
        })
    }
  }

}
