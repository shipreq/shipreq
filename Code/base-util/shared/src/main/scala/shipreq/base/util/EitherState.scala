package shipreq.base.util

import scalaz.{-\/, Applicative, Monad, \/, \/-}

/** Either monad + state monad stack.
  *
  * s => (s, e \/ a)
  */
object EitherState {

  object ScalaTrampoline {
    import scala.util.control.TailCalls._
    type Trampoline[A] = TailRec[A]
    object Trampoline {
      def pure[A](a: A): Trampoline[A] = done(a)
      def suspend[A](t: => Trampoline[A]): Trampoline[A] = tailcall(t)
      def delay[A](a: => A): Trampoline[A] = suspend(pure(a))
      def run[A](t: Trampoline[A]): A = t.result
    }
  }

  object ScalazTrampoline {
    import scalaz.Free
    import scalaz.std.function.function0Instance
    type Trampoline[A] = Free.Trampoline[A]
    object Trampoline {
      def pure[A](a: A): Trampoline[A] = Free.pure(a)
      def suspend[A](t: => Trampoline[A]): Trampoline[A] = Free.suspend(t)
      def delay[A](a: => A): Trampoline[A] = suspend(pure(a))
      def run[A](t: Trampoline[A]): A = t.run
    }
  }

  object NoTrampoline {
    final class Trampoline[A](val result: A) extends AnyVal {
      def map[B](f: A => B): Trampoline[B] = new Trampoline(f(result))
      def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = f(result)
    }
    object Trampoline {
      def pure[A](a: A): Trampoline[A] = new Trampoline(a)
      def suspend[A](t: => Trampoline[A]): Trampoline[A] = t
      def delay[A](a: => A): Trampoline[A] = suspend(pure(a))
      def run[A](t: Trampoline[A]): A = t.result
    }
  }

  import ScalazTrampoline._

  final class MutableState[S](initialState: S) {
    var state: S = initialState

    def mod(f: S => S): Unit =
      state = f(state)
  }

  object MutableState {
    def apply[S](initialState: S): MutableState[S] =
      new MutableState(initialState)
  }

  type Underlying[S, E, A] = MutableState[S] => Trampoline[E \/ A]

  final case class Instance[S, E, A](self: Underlying[S, E, A]) extends AnyVal {
    type Self[B] = Instance[S, E, B]

    def widen[B >: A]: Self[B] =
      map(a => a) // TODO *************************************************************************************************************************************

    def map[B](f: A => B): Self[B] =
      Instance(s => self(s).map(_.map(f)))

    def flatMap[B](f: A => Self[B]): Self[B] =
      Instance(s => Trampoline.suspend {
        self(s).flatMap {
          case \/-(a)    => f(a).self(s)
          case e@ -\/(_) => Trampoline.pure(e)
        }
      })

    def flatTap[B](f: A => Self[B]): Self[A] =
      Instance(s => Trampoline.suspend {
        self(s).flatMap {
          case \/-(a)    => f(a).self(s).map(_.map(_ => a))
          case e@ -\/(_) => Trampoline.pure(e)
        }
      })

    def >>[B](next: Self[B]): Self[B] =
      Instance(s => Trampoline.suspend {
        self(s).flatMap {
          case \/-(_)    => next.self(s)
          case e@ -\/(_) => Trampoline.pure(e)
        }
      })

    @inline def <<[B](prev: Self[B]): Self[A] =
      prev >> this

    def andReturn[B](b: B): Self[B] =
      map(_ => b)

    def void: Self[Unit] =
      andReturn(())

    def catchErrors(h: Throwable => E)(implicit F: Applicative[Underlying[S, E, *]]): Self[A] =
      Instance(
          s =>
            Trampoline.delay {
              try
                Trampoline.run(self(s))
              catch {
                case t: Throwable => -\/(h(t))
              }
            }
      )

    def run(s: S)(implicit F: Applicative[Underlying[S, E, *]]): (S, E \/ A) = {
      val ms = MutableState(s)
      val r = Trampoline.run(self(ms))
      (ms.state, r)
    }

    def exec(s: S)(implicit F: Applicative[Underlying[S, E, *]]): E \/ S = { // TODO wrap/unwrap S ****************************************************************
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
          s => Trampoline.delay(\/-(a))

        override def map[A, B](fa: Underlying[A])(f: A => B): Underlying[B] =
          Instance(fa).map(f).self

        override def bind[A, B](fa: Underlying[A])(f: A => Underlying[B]): Underlying[B] =
          Instance(fa).flatMap(a => Instance(f(a))).self
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

//    def apply[A](f: S => (S, E \/ A)): Instance[A] =
//      Instance(s => Trampoline.pure(f(s)))

    def getFlatMap[A](f: S => Instance[A]): Instance[A] =
      get.flatMap(f)

    def pure[A](a: A): Instance[A] =
      either(\/-(a))

    def point[A](a: => A): Instance[A] =
      Instance(eitherStateUnderlyingMonad.point(a))

    def either[A](ea: E \/ A): Instance[A] = {
      val t = Trampoline.pure(ea)
      Instance(_ => t)
    }

    def eithers[A](f: S => E \/ A): Instance[A] =
      Instance(s => Trampoline.pure(f(s.state)))

    def fail[A](e: E): Instance[A] =
      either(-\/(e))

    def failOption(e: Option[E]): Instance[Unit] =
      e.fold(unit)(fail)

    def failOptions(f: S => Option[E]): Instance[Unit] =
      getFlatMap(s => failOption(f(s)))

    def mod(f: S => S): Instance[Unit] =
      Instance(ms => Trampoline.delay { // TODO ?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
        ms.mod(f)
        rightUnit
      })

    val get: Instance[S] =
      eithers(\/-(_))

    def gets[A](f: S => A): Instance[A] =
      eithers(s => \/-(f(s)))

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

