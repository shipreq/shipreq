package shipreq.webapp.base.test

import scala.io.AnsiColor._
import scalaz.Equal
import WebappTestUtil._

trait ActionTester {
  protected type S
  protected def newState: S
  protected def defaultLast: S => Unit

  import Action.Result

  abstract class ActionCtx {
    def indentInc(): Unit
    def indentDec(): Unit
    def log(l: => String): Unit
    def start(): Unit
    def pass(): Unit
    def fail(e: Throwable): Unit
  }

  abstract class BaseActionCtx extends ActionCtx {
    val indentInit = "[ACTION] "
    val indentLevel = ". "
    def failColour = (BOLD + RED)
    def passColour = GREEN
  }

  class QuietActionCtx extends BaseActionCtx {
    type Rec = (Int, () => String)
    def rec(l: => String): Rec = (indent, () => l)
    var record = Vector.empty[Rec]
    var indent = 0
    override def indentInc() = indent += 1
    override def indentDec() = indent -= 1
    override def log(l: => String) = record :+= rec(l)
    override def start() = ()
    override def pass() = ()
    override def fail(e: Throwable) = {
      println(s"\n${indentInit}Start")
      val real = record.map { case (i, msg) => (i, msg())}.filter(_._2.nonEmpty).zipWithIndex
      val last = real.length - 1
      for (((i, msg), j) <- real) {
        val c = if (j < last) passColour else failColour
        val ind = indentInit + (indentLevel * i)
        println(ind + c + msg + RESET)
      }
      println(s"${indentInit}${failColour}Fail$RESET\n")
    }
  }

  class DebugActionCtx extends BaseActionCtx {
    var indent = indentInit
    var last: String = null
    override def indentInc() = indent += indentLevel
    override def indentDec() = indent = indent.dropRight(indentLevel.length)
    override def log(l: => String) = {
      val msg = l
      if (msg.nonEmpty) {
        flushLast(true)
        last = msg
      }
    }
    def flushLast(passed: Boolean): Unit = {
      if (last ne null) {
        val c = if (passed) passColour else failColour
        println(indent + c + last + RESET)
        last = null
      }
    }
    override def start() = println(s"\n${indentInit}Start")
    override def pass() = end(true, "Pass")
    override def fail(e: Throwable) = end(false, "Fail")
    def end(passed: Boolean, msg: String) = {
      flushLast(passed)
      println(indentInit + msg)
      println()
    }
  }

  final class Action[A](private[Action] val action: (ActionCtx, S) => Result[A],
                        private[Action] val last: S => Unit) {

    private def newAction[B](a: (ActionCtx, S) => Result[B]): Action[B] =
      new Action(a, last)

    private def logCompose(ctx: ActionCtx): Unit =
      () // ctx.log("<compose>")

    private def compose[B, C](next: Action[B])(f: (Result[A], Result[B]) => Result[C]): Action[C] =
      new Action({ (ctx, s1) =>
        logCompose(ctx)
        val r1 = action(ctx, s1)
        ctx.indentInc()
        val s2 = r1.state
        last(s2)
        val r2 = next.action(ctx, s2)
        ctx.indentDec()
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
      _run(new QuietActionCtx, s)

    def runDebug(s: S = newState): Unit =
      _run(new DebugActionCtx, s)

    def _run(ctx: ActionCtx, s: S): Unit = {
      ctx.start()
      try {
        action(ctx, s)
        ctx.pass()
      } catch {
        case t: Throwable =>
          ctx.fail(t)
          throw t
      }
    }

    def >>-[B](f: A => Action[B]): Action[B] =
      >>=(r => f(r.after))

    def >>=[B](f: Result[A] => Action[B]): Action[B] =
      new Action({ (ctx, s1) =>
        logCompose(ctx)
        val r1 = action(ctx, s1)
        ctx.indentInc()
        val s2 = r1.state
        last(s2)
        val next = f(r1)
        val r2 = next.action(ctx, s2)
        next.last(s2)
        ctx.indentDec()
        r2
      }, Action.nopLast)

    def layer[B, C](f: S => B)(g: (B, Result[A], B) => Result[C]): Action[C] =
      newAction { (ctx, s) =>
        val before = f(s)
        val r      = action(ctx, s)
        val after  = f(r.state)
        g(before, r, after)
      }

    def focus[B](f: S => B): Action[B] =
      layer(f)((b1, r, b2) => Result(b1, r.state, b2))

    //    def focus2[B](f: Screen => B): Action[(A, B)] =
    //      layer(f)((b1, r, b2) => r.bimap((_, b1), (_, b2)))

    def mapFocus[B](f: A => B): Action[B] =
      newAction(action(_, _) map f)

    def when(f: S => Boolean): Action[Unit] =
      newAction { (ctx, s1) =>
        val s2 =
          if (f(s1)) {
            action(ctx, s1).state
          } else {
            ctx.log("<skip>")
            s1
          }
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
      newAction { (ctx, s) =>
        val r = action(ctx, s)
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
      newAction((ctx, s) => {
        val r = action(ctx, s)
        f(r)
        r
      })

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

  // ----------------------------------------------------------------------

  type ActionName = String
  def noActionName: ActionName = ""
  //implicit def stringToActionName(s: String): ActionName = _ => s

  object Action {
    private[ActionTester] val nopLast = (_: Any) => ()

    def apply(n: => ActionName, f: S => Unit): Action[Unit] =
      Action(n, f, defaultLast)

    def apply(n: => ActionName, f: S => Unit, last: S => Unit): Action[Unit] = {
      val g = (ctx: ActionCtx, s: S) => {
        ctx.log(n)
        f(s)
        Result unit newState
      }
      new Action(g, last)
    }

    def apply2[A](f: S => (ActionName, A))(g: (S, A) => Unit, last: S => Unit = defaultLast): Action[Unit] = {
      val h = (ctx: ActionCtx, s: S) => {
        val (n, a) = f(s)
        ctx.log(n)
        g(s, a)
        Result unit newState
      }
      new Action(h, last)
    }

    def exec(n: => ActionName, f: => Unit): Action[Unit] =
      apply(n, _ => f)

    def exec2[A, B](n: => ActionName, f: S => A)(g: A => Unit): Action[Unit] =
      value(f) >>- (a => Action.exec(n, g(a)))

    def value[A](f: S => A): Action[A] =
      new Action((_, s) => { val a = f(s); Result(a, s, a)}, nopLast)

    def readonly(f: S => Unit): Action[Unit] =
      Action(noActionName, f, nopLast)

    def assert(f: => Unit): Action[Unit] =
      Action(noActionName, _ => f, nopLast)

    lazy val nop: Action[Unit] =
      apply(noActionName, nopLast, nopLast)

    // ----------------------------------------------------------------------

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
  }

  def run(a: Action[_]): Unit = a.run()
  def runDebug(a: Action[_]): Unit = a.runDebug()
}
