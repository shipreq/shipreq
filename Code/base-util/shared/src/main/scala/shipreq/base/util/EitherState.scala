package shipreq.base.util

import scalaz.{-\/, Applicative, Monad, \/, \/-}

/** Either monad + state monad stack.
  *
  * s => (s, e \/ a)
  */
object EitherState {

  type Underlying[S, E, A] = S => (S, E \/ A)

  final case class Instance[S, E, A](codensity: Codensity[Underlying[S, E, *], A]) extends AnyVal { self =>
    type Self[B] = Instance[S, E, B]

    def widen[B >: A]: Self[B] =
      map(a => a) // TODO *************************************************************************************************************************************

    def map[B](f: A => B): Self[B] =
      Instance(codensity.map(f))

    def flatMap[B](f: A => Self[B]): Self[B] =
      Instance(codensity.flatMap(f(_).codensity))

    def flatTap[B](f: A => Self[B]): Self[A] =
      Instance(codensity.flatTap(f(_).codensity))

    def >>[B](next: Self[B]): Self[B] =
      flatMap(_ => next)

    @inline def <<[B](prev: Self[B]): Self[A] =
      prev >> this

    def andReturn[B](b: B): Self[B] =
      Instance(codensity.andReturn(b))

    def void: Self[Unit] =
      map(_ => ())

    def catchErrors(h: Throwable => E): Self[A] =
      Instance(new Codensity[Underlying[S, E, *], A] {
        override def apply[B](f: A => Underlying[S, E, B]): Underlying[S, E, B] =
          s =>
            try
              codensity(f)(s)
            catch {
              case t: Throwable => (s, -\/(h(t)))
            }
      })

    def run(s: S)(implicit F: Applicative[Underlying[S, E, *]]): (S, E \/ A) =
      codensity.lower(F)(s)

    def exec(s: S)(implicit F: Applicative[Underlying[S, E, *]]): E \/ S = {
      val r = run(s)
      r._2.map(_ => r._1)
    }

    def eval(s: S)(implicit F: Applicative[Underlying[S, E, *]]): E \/ A =
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

        override def point[A](a: => A): Underlying[A] =
          s => (s, \/-(a))

        override def map[A, B](fa: Underlying[A])(f: A => B): Underlying[B] =
          s => {
            val result1 = fa(s)
            val b = result1._2.map(f)
            (result1._1, b)
          }

        override def bind[A, B](fa: Underlying[A])(f: A => Underlying[B]): Underlying[B] =
          s => {
            val result1 = fa(s)
            result1._2 match {
              case \/-(a)    => f(a)(result1._1)
              case e@ -\/(_) => (result1._1, e)
            }
          }
      }

    implicit val eitherStateMonad: Monad[Instance] =
      new Monad[Instance] {
        override def point[A](a: => A): Instance[A] =
          self.point(a)

        override def map[A, B](fa: Instance[A])(f: A => B): Instance[B] =
          fa.map(f)

        override def bind[A, B](fa: Instance[A])(f: A => Instance[B]): Instance[B] =
          fa.flatMap(f)
      }

    def apply[A](f: S => (S, E \/ A)): Instance[A] =
      Instance(Codensity.lift[Underlying, A](f))

    def getFlatMap[A](f: S => Instance[A]): Instance[A] =
      Instance {
        new Codensity[Underlying, A] {
          override def apply[B](g: A => Underlying[B]): Underlying[B] =
            s => f(s).codensity(g)(s)
        }
      }

    def restack[A](i: Instance[A]): Instance[A] =
      apply(s => i.run(s))

    def pure[A](a: A): Instance[A] =
      Instance(Codensity.pure[Underlying, A](a))

    def point[A](a: => A): Instance[A] =
      Instance(Codensity.point[Underlying, A](a))

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

    // TODO Some of these combinators might be faster by switching to use getFlatMap

    val unit: Instance[Unit] =
      pure(())

    val _unit: Any => Instance[Unit] =
      _ => unit

    def some[A](oa: Option[A], err: => E): Instance[A] =
      oa.fold[Instance[A]](fail(err))(pure)

    def test(isOk: Boolean, whenFalse: => E): Instance[Unit] =
      if (isOk) unit else fail(whenFalse)

    def tests(isOk: S => Boolean, whenFalse: => E): Instance[Unit] =
      get.flatMap(s => test(isOk(s), whenFalse))

    def foldMapRun[A](as: IterableOnce[A])(f: A => Instance[Unit]): Instance[Unit] = // TODO ***************************************************************
    // as.foldLeft(nop)(_ >> f(_))
      Util.mapReduce(as, unit)(f, _ >> _)
  }

}

