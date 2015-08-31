package tmp

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.ReusableFn
import scala.scalajs.js
import org.scalajs.dom.console

object ReactPatches {

  implicit def patchCallbackOps[A](cb: CallbackTo[A]) = new PatchCallbackOps(cb.toScalaFn)

  final class PatchCallbackOps[A](private val f: () => A) extends AnyVal {
    def thiss = CallbackTo lift f
//    def conditionallyU(cond: => Boolean): Callback =
//      Callback(if (cond) f())

//    def ?:(cond: => Boolean): Callback =
//      Callback(if (cond) f())
    /*
    def ?:(passCriteria: => Boolean)(implicit c: Condition[A]): CallbackTo[c.Out] =
      runIf(passCriteria)(c)

    //  def runIf(cond: => Boolean): Callback =
    //    CallbackTo(if (cond) f() else undefined)

    def runIf(passCriteria: => Boolean)(implicit c: Condition[A]): CallbackTo[c.Out] =
      CallbackTo(if (passCriteria) c.pass(f()) else c.fail)

    def runUnless(failCriteria: => Boolean)(implicit c: Condition[A]): CallbackTo[c.Out] =
      runIf(!failCriteria)(c)
    */
//    def ?:(passCriteria: CallbackB)(implicit c: Condition[A]): CallbackTo[c.Out] =
//      runIf(passCriteria)(c)

//    def runsIf(passCriteria: CallbackB)(implicit c: Condition[A]): CallbackTo[c.Out] =
//      passCriteria.map(b => if (b) c.pass(f()) else c.fail)
//
//    def runsUnless(failCriteria: CallbackB)(implicit c: Condition[A]): CallbackTo[c.Out] =
//      runsIf(!failCriteria)(c)

//    def ifThen[B](r: CallbackTo[B])(implicit c: Condition[A], ev: A =:= Boolean): CallbackTo[c.Out] =
//      passCriteria.map(b => if (b) c.pass(f()) else c.fail)
  }

  implicit final class PatchCallback$Ops(private val ε: Callback.type) extends AnyVal {
//    def pf[A](f: PartialFunction[A, Callback]): A => Callback =
//      a => f.applyOrElse[A, Callback](a, _ => Callback.empty)
//
//    def pf[A](f: PartialFunction[A, Callback], mapMatched: Callback => Callback): A => Callback =
//      pf(f andThen mapMatched)
  }

//  trait Condition[In] {
//    type Out
//    val pass: In => Out
//    def fail: Out
//  }
//
//  trait ConditionLowPri {
//    implicit def fallback[A] = Condition[A, UndefOr[A]](a => a)(undefined)
//  }
//
//  object Condition extends ConditionLowPri {
//    type Aux[I, O] = Condition[I] {type Out = O}
//    def apply[I, O](p: I => O)(f: => O): Aux[I, O] =
//      new Condition[I] {
//        override type Out = O
//        override val pass = p
//        override def fail = f
//      }
//    implicit val unit = Condition[Unit, Unit](_ => ())(())
//    val and = Condition[Boolean, Boolean](identity)(false)
//    def pass[A] = Condition[A, Boolean](_ => true)(false)
//  }

  implicit class PatchBackendScope[P,S](private val $: BackendScope[P, S]) extends AnyVal {
    @inline def propsCB: CallbackTo[P] = CallbackTo($.props)
    @inline def stateCB: CallbackTo[S] = CallbackTo($.state)
  }
}
