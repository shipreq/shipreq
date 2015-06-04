package shipreq.webapp.base.test

import scalaz.Equal
import scalaz.syntax.equal._
import BaseTestUtil._

trait ActionTester {
  protected type S
  protected def newState: S
  protected def defaultLast: S => Unit

  import Action.Result

  final class Action[A](private[Action] val action: S => Result[A],
                        private[Action] val last: S => Unit) {

    private def newAction[B](a: S => Result[B]): Action[B] =
      new Action(a, last)

    private def compose[B, C](next: Action[B])(f: (Result[A], Result[B]) => Result[C]): Action[C] =
      new Action({ s1 =>
        val r1 = action(s1)
        val s2 = r1.state
        last(s2)
        val r2 = next.action(s2)
        f(r1, r2)
      }, next.last)

    def >>[B](next: Action[B]): Action[B] =
      compose(next)((r1, r2) => Result(r2.before, r2.state, r2.after))

    def =>>(next: Action[A]): Action[A] =
      compose(next)((r1, r2) => Result(r1.before, r2.state, r2.after))

    def <<[B](prev: Action[B]): Action[A] =
      prev >> this

    def <<=(prev: Action[A]): Action[A] =
      prev =>> this

    def run(s: S = newState): Unit =
      action(s)

    def >>-[B](f: A => Action[B]): Action[B] =
      >>=(r => f(r.after))

    def >>=[B](f: Result[A] => Action[B]): Action[B] =
      new Action({ s1 =>
        val r1 = action(s1)
        val s2 = r1.state
        last(s2)
        val next = f(r1)
        val r2 = next.action(s2)
        next.last(s2)
        r2
      }, Action.nopLast)

    def layer[B, C](f: S => B)(g: (B, Result[A], B) => Result[C]): Action[C] =
      newAction { s =>
        val before = f(s)
        val r      = action(s)
        val after  = f(r.state)
        g(before, r, after)
      }

    def focus[B](f: S => B): Action[B] =
      layer(f)((b1, r, b2) => Result(b1, r.state, b2))

    //    def focus2[B](f: Screen => B): Action[(A, B)] =
    //      layer(f)((b1, r, b2) => r.bimap((_, b1), (_, b2)))

    def mapFocus[B](f: A => B): Action[B] =
      newAction(action(_) map f)

    def when(f: S => Boolean): Action[Unit] =
      newAction { s1 =>
        val s2 = if (f(s1)) action(s1).state else s1
        Result unit s2
      }

    def unless(f: S => Boolean): Action[Unit] =
      when(!f(_))

    def times(n: Int): Action[A] =
      if (n > 1)
        Vector.fill(n)(this).reduce(_ =>> _)
      else if (n == 1)
        this
      else
        throw new java.lang.IllegalArgumentException(s"n ($n) must be ≥ 1.")

    def withResult(f: Result[A] => Unit): Action[A] =
      newAction { s =>
        val r = action(s)
        f(r)
        r
      }

    def assertBefore[B >: A : Equal](expect: B): Action[A] = assertBefore(expect, "Before action")
    def assertAfter [B >: A : Equal](expect: B): Action[A] = assertAfter (expect, "After action")

    def assertBefore[B >: A : Equal](expect: B, err: => String): Action[A] = withResult(r => assertEq(err, r.before, expect))
    def assertAfter [B >: A : Equal](expect: B, err: => String): Action[A] = withResult(r => assertEq(err,  r.after,  expect))

    def assertChange(implicit e: Equal[A]): Action[A] =
      withResult(r => if (r.before ≟ r.after) fail(s"Change expected from [${r.before}]"))

    def assertNoChange(implicit e: Equal[A]): Action[A] =
      withResult(r => assertEq("assertNoChange", r.after, r.before))

    def assertDelta(delta: A)(implicit n: Numeric[A]): Action[A] =
      withResult(r => {
        val actual = n.minus(r.after, r.before)
        assertEq("Delta", actual, delta)(Equal.equalA)
      })

    def test(f: (A, A) => Boolean, err: => String): Action[A] =
      withResult(r =>
        if (!f(r.before, r.after))
          fail(s"$err. [${r.before}] ==> [${r.after}]."))

    def testBefore(f: A => Boolean, err: => String): Action[A] =
      test((a, _) => f(a), err)

    def testAfter(f: A => Boolean, err: => String): Action[A] =
      test((_, a) => f(a), err)

    def assertIncrease(implicit n: Numeric[A]): Action[A] =
      test(n.gt, "Increase expected")

    def assertDecrease(implicit n: Numeric[A]): Action[A] =
      test(n.lt, "Decrease expected")

    def tapAction(f: Result[A] => Unit): Action[A] =
      newAction(action andThen { r => f(r); r })

    def logValues(name: String = null): Action[A] =
      tapAction { r =>
        val b = r.before.toString
        val a = r.after.toString
        val prefix = Option(name).fold("")(n => s"$n: ")
        if (a == b)
          println(s"${prefix}Before/After = [$a].")
        else
          println(s"${prefix}\n - Before = [$b]\n -  After = [$a]")
      }
  }

  object Action {
    object Result {
      def unit(s: S): Result[Unit] =
        Result((), s, ())
    }
    case class Result[A](before: A, state: S, after: A) {
      def map[B](f: A => B): Result[B] =
        bimap(f, f)

      def bimap[B](f: A => B, g: A => B): Result[B] =
        Result(f(before), state, g(after))
    }

    // ----------------------------------------------------------------------

    private[ActionTester] val nopLast = (_: Any) => ()

    def apply(f: S => Unit): Action[Unit] =
      Action(f, defaultLast)

    def apply(f: S => Unit, last: S => Unit): Action[Unit] = {
      val g = (s: S) => {f(s); newState}
      new Action(s => Result unit g(s), last)
    }

    def exec(f: => Unit): Action[Unit] =
      apply(_ => f)

    def exec2[A, B](f: S => A)(g: A => Unit): Action[Unit] =
      value(f) >>- (a => Action.exec(g(a)))

    def value[A](f: S => A): Action[A] =
      new Action(s => { val a = f(s); Result(a, s, a)}, nopLast)
  }

  def run(a: Action[_]): Unit = a.run()
}
