package shipreq.base.util

object Trampoline {

  object Default {
    import scalaz.Free
    implicit val fn0 = ScalazExtra.applicativeFunction0
    type Trampoline[A] = Free.Trampoline[A]
    object Trampoline {
      def pure   [A](a: A)               : Trampoline[A] = Free.pure(a)
      def suspend[A](t: => Trampoline[A]): Trampoline[A] = Free.suspend(t)
      def delay  [A](a: => A)            : Trampoline[A] = suspend(pure(a))
      def run    [A](t: Trampoline[A])   : A             = t.run
    }
  }

//  object ScalaTrampoline {
//    import scala.util.control.TailCalls._
//    type Trampoline[A] = TailRec[A]
//    object Trampoline {
//      def pure[A](a: A): Trampoline[A] = done(a)
//      def suspend[A](t: => Trampoline[A]): Trampoline[A] = tailcall(t)
//      def delay[A](a: => A): Trampoline[A] = suspend(pure(a))
//      def run[A](t: Trampoline[A]): A = t.result
//    }
//  }
//
//  object NoTrampoline {
//    final class Trampoline[A](val result: A) extends AnyVal {
//      def map[B](f: A => B): Trampoline[B] = new Trampoline(f(result))
//      def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = f(result)
//    }
//    object Trampoline {
//      def pure[A](a: A): Trampoline[A] = new Trampoline(a)
//      def suspend[A](t: => Trampoline[A]): Trampoline[A] = t
//      def delay[A](a: => A): Trampoline[A] = suspend(pure(a))
//      def run[A](t: Trampoline[A]): A = t.result
//    }
//  }
//
//  object Fn0Trampoline {
//    final class Trampoline[A](val result: () => A) extends AnyVal {
//      def map[B](f: A => B): Trampoline[B] = new Trampoline(() => f(result()))
//      def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = f(result())
//    }
//    object Trampoline {
//      def pure[A](a: A): Trampoline[A] = new Trampoline(() => a)
//      def suspend[A](t: => Trampoline[A]): Trampoline[A] = new Trampoline(() => t.result())
//      def delay[A](a: => A): Trampoline[A] = suspend(pure(a))
//      def run[A](t: Trampoline[A]): A = t.result()
//    }
//  }

}
