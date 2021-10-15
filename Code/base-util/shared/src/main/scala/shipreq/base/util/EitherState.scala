package shipreq.base.util

import cats.Monad
import shipreq.base.util.CatsExtra.ApplicativeDelay

/** Either monad + state monad stack.
  *
  * s => (s, e \/ a)
  */
object EitherState {
  import Trampoline.Default._

  type Underlying[S, E, A] = S => Trampoline[(S, E \/ A)]

  final case class Instance[S, E, A](self: Underlying[S, E, A]) extends AnyVal {
    type Self[B] = Instance[S, E, B]

    def widen[B >: A]: Self[B] =
      Instance(self.asInstanceOf[Underlying[S, E, B]])

    def map[B](f: A => B)(implicit F: Monad[Underlying[S, E, *]]): Self[B] =
      Instance(F.map(self)(f))

    def flatMap[B](f: A => Self[B])(implicit F: Monad[Underlying[S, E, *]]): Self[B] =
      Instance(F.flatMap(self)(f(_).self))

    def flatTap[B](f: A => Self[B])(implicit F: Monad[Underlying[S, E, *]]): Self[A] =
      for {
        a <- this
        _ <- f(a)
      } yield a

    def >>[B](next: Self[B])(implicit F: Monad[Underlying[S, E, *]]): Self[B] =
      flatMap(_ => next)

    @inline def <<[B](prev: Self[B])(implicit F: Monad[Underlying[S, E, *]]): Self[A] =
      prev >> this

    def andReturn[B](b: B)(implicit F: Monad[Underlying[S, E, *]]): Self[B] =
      map(_ => b)

    def void(implicit F: Monad[Underlying[S, E, *]]): Self[Unit] =
      map(_ => ())

    def catchErrors(h: Throwable => E): Self[A] =
      Instance(
          s =>
            Trampoline.delay {
              try
                Trampoline.run(self(s))
              catch {
                case t: Throwable => (s, -\/(h(t)))
              }
            }
      )

    def handleFailure(f: E => Self[A]): Self[A] =
      Instance(self(_).flatMap {
        case ok @ (_, \/-(_)) => Trampoline.pure(ok)
        case (s, -\/(e))      => f(e).self(s)
      })

    def run(s: S): (S, E \/ A) =
      Trampoline.run(self(s))

    def exec(s: S): E \/ S = {
      val r = run(s)
      r._2.map(_ => r._1)
    }

    def eval(s: S): E \/ A =
      run(s)._2
  }

  // ===================================================================================================================

  def ForTypes[S, E]: ForTypes[S, E] =
    new ForTypes[S, E]

  final class ForTypes[S, E] { self =>

    type Underlying[A] = EitherState.Underlying[S, E, A]
    type Instance  [A] = EitherState.Instance  [S, E, A]

    private[this] val rightUnit = \/-(())

    implicit val eitherStateUnderlyingMonad: Monad[Underlying] =
      new Monad[Underlying] {

        override def pure[A](a: A): Underlying[A] =
          s => Trampoline.delay((s, \/-(a)))

        override def map[A, B](fa: Underlying[A])(f: A => B): Underlying[B] =
          s => fa(s).map { result1 =>
            val b = result1._2.map(f)
            (result1._1, b)
          }

        override def flatMap[A, B](fa: Underlying[A])(f: A => Underlying[B]): Underlying[B] =
          s => Trampoline.suspend(fa(s).flatMap { result1 =>
            result1._2 match {
              case \/-(a)    => f(a)(result1._1)
              case e@ -\/(_) => Trampoline.pure((result1._1, e))
            }
          })

        override def tailRecM[A, B](a0: A)(f: A => Underlying[Either[A, B]]): Underlying[B] =
          ???
      }

    implicit val eitherStateMonad: Monad[Instance] =
      new Monad[Instance] {
        override def pure[A](a: A): Instance[A] =
          self.pure(a)

        override def map[A, B](fa: Instance[A])(f: A => B): Instance[B] =
          fa.map(f)

        override def flatMap[A, B](fa: Instance[A])(f: A => Instance[B]): Instance[B] =
          fa.flatMap(f)

        override def tailRecM[A, B](a: A)(f: A => Instance[Either[A,B]]): Instance[B] =
          ???
      }

    def apply[A](f: S => (S, E \/ A)): Instance[A] =
      Instance(s => Trampoline.pure(f(s)))

    def getFlatMap[A](f: S => Instance[A]): Instance[A] =
      get.flatMap(f)

    def pure[A](a: A): Instance[A] =
      either(\/-(a))

    def point[A](a: => A): Instance[A] =
      Instance(eitherStateUnderlyingMonad.delay(a))

    def either[A](ea: E \/ A): Instance[A] =
      apply((_, ea))

    def eithers[A](f: S => E \/ A): Instance[A] =
      apply(s => (s, f(s)))

    def fail[A](e: E): Instance[A] =
      either(-\/(e))

    def failOption(e: Option[E]): Instance[Unit] =
      e.fold(unit)(fail)

    def failOptions(f: S => Option[E]): Instance[Unit] =
      getFlatMap(s => failOption(f(s)))

    def mod(f: S => S): Instance[Unit] =
      apply(s => (f(s), rightUnit))

    val get: Instance[S] =
      apply(s => (s, \/-(s)))

    def gets[A](f: S => A): Instance[A] =
      apply(s => (s, \/-(f(s))))

    val unit: Instance[Unit] =
      pure(())

    val _unit: Any => Instance[Unit] =
      _ => unit

    def some[A](oa: Option[A], err: => E): Instance[A] =
      oa.fold[Instance[A]](fail(err))(pure)

    def test(isOk: Boolean, whenFalse: => E): Instance[Unit] =
      if (isOk) unit else fail(whenFalse)

    def tests(isOk: S => Boolean, whenFalse: => E): Instance[Unit] =
      Instance(s => test(isOk(s), whenFalse).self(s))

    def foldMapRun[A](as: IterableOnce[A])(f: A => Instance[Unit]): Instance[Unit] =
      as.iterator.foldLeft(unit)(_ >> f(_))
  }

}

