package shipreq.base.util

import scala.runtime.AbstractFunction2
import PartialOrder.Cmp

/** Laws:
  *   - reflexivity
  *   - antisymmetry
  *   - transitivity
  */
final class PartialOrder[-A](asFn: (A, A) => Cmp) extends AbstractFunction2[A, A, Cmp] { self =>

  override def apply(a: A, b: A): Cmp =
    asFn(a, b)

  def orElse[AA <: A](next: PartialOrder[AA]): PartialOrder[AA] =
    PartialOrder((x, y) =>
      self(x, y) match {
        case Cmp.Separate => next(x, y)
        case c            => c
      }
    )
}

object PartialOrder {

  def apply[A](f: (A, A) => Cmp): PartialOrder[A] =
    new PartialOrder(f)

  sealed trait Cmp {
    import Cmp._

    final def flip: Cmp =
      this match {
        case Lesser  => Greater
        case Greater => Lesser
        case x       => x
      }
  }

  object Cmp {
    case object Equal    extends Cmp
    case object Lesser   extends Cmp
    case object Greater  extends Cmp
    case object Separate extends Cmp

    def keyedInt[A, K: UnivEq](key: A => K, value: A => Int): PartialOrder[A] =
      PartialOrder((x, y) =>
        if (key(x) != key(y))
          Separate
        else {
          val q = value(x) - value(y)
          if (q == 0)
            Equal
          else if (q > 0)
            Greater
          else
            Lesser
        }
      )

    implicit def univEq: UnivEq[Cmp] = UnivEq.derive
  }

  object ImplicitOps {
    import Cmp._

    implicit class PartialOrderOps[A](private val lhs: A) extends AnyVal {

      @inline def >=(rhs: A)(implicit po: PartialOrder[A]): Boolean =
        po(lhs, rhs) match {
          case Greater | Equal => true
          case _               => false
        }

      @inline def <=(rhs: A)(implicit po: PartialOrder[A]): Boolean =
        po(lhs, rhs) match {
          case Lesser | Equal => true
          case _              => false
        }

      @inline def <(rhs: A)(implicit po: PartialOrder[A]): Boolean =
        po(lhs, rhs) == Lesser

      @inline def >(rhs: A)(implicit po: PartialOrder[A]): Boolean =
        po(lhs, rhs) == Greater

      @inline def ===(rhs: A)(implicit po: PartialOrder[A]): Boolean =
        po(lhs, rhs) == Equal

      @inline def isComparableTo(rhs: A)(implicit po: PartialOrder[A]): Boolean =
        po(lhs, rhs) != Separate

      @inline def isSeparateTo(rhs: A)(implicit po: PartialOrder[A]): Boolean =
        po(lhs, rhs) == Separate
    }
  }

  // ===================================================================================================================

  object Props {
    def eval[A: PartialOrder](as: Set[A]) =
      new Props[A].prop(as)

    def assert[A: PartialOrder](as: Set[A]): Unit =
      eval(as).assertSuccess()
  }

  final class Props[A](implicit p: PartialOrder[A]) {
    import ImplicitOps._
    import nyaya.prop._

    private def imply(a: Boolean)(c: Boolean, err: => String): Option[String] =
      Option.when(a && !c)(err)

    private def reflexivity(a: A): EvalL =
      Eval.test("reflexivity", a, a <= a)

    private def antisymmetry(a: A, b: A): EvalL =
      Eval.atom("antisymmetry", (a, b), imply(a <= b && b <= a)(a === b, s"a cmp b = ${p(a, b)}"))

    private def transitivity(a: A, b: A, c: A): EvalL =
      Eval.atom("transitivity", (a, b, c), imply(a <= b && b <= c)(a <= c, s"a cmp c = ${p(a, c)}"))

    val prop: Prop[Set[A]] =
      Prop.eval[Set[A]] { as =>
        var result: EvalL = null

        def add(r: EvalL): Unit =
          result = if (result == null) r else result & r

        as.foreach { a =>
          add(reflexivity(a))
          as.foreach { b =>
            add(antisymmetry(a, b))
            as.foreach { c =>
              add(transitivity(a, b, c))
            }
          }
        }

        if (result == null) Eval.pass() else result
      }
  }

//  object Laws {
//    final case class Input[A](x: A, y: A, z: A)
//  }
//
//  final class Laws[A: scalaz.Equal](implicit p: PartialOrder[A]) {
//    import ImplicitOps._
//    import nyaya.prop._
//
//    type Input = Laws.Input[A]
//    type Laws  = Prop[Input]
//
////    private def prop2[A](name: String, p: Prop[A])(g: (ProvSet, ProvSet) => A): Laws = {
////      def f(desc: String, f1: Input => ProvSet, f2: Input => ProvSet): Laws =
////        Prop.evaln(s"$name ($desc)", i => {
////          val a = g(f1(i), f2(i))
////          p(a).liftL
////        })
////      f("a,b", _.a, _.b) & f("a,c", _.a, _.c) & f("b,c", _.b, _.c)
////    }
////
////    private def equal2[B: Equal](name: String,
////                                 e: (ProvSet, ProvSet) => B,
////                                 a: (ProvSet, ProvSet) => B): Laws = {
////      def f(desc: String, f1: Input => ProvSet, f2: Input => ProvSet): Laws = {
////        def mk(g: (ProvSet, ProvSet) => B): Input => B = i => g(f1(i), f2(i))
////        Prop.equal[Input, B](s"$name ($desc)", mk(a), expect = mk(e))
////      }
////      f("a,b", _.a, _.b) & f("a,c", _.a, _.c) & f("b,c", _.b, _.c)
////    }
//
////    private val idempotency: Laws =
////      equal2("idempotency",
////        (x, y) => (x ++ y) ++ y,
////        (x, y) => x ++ y)
////
////    private val associativity: Laws =
////      Prop.equal[Input, ProvSet]("associativity",
////        i => (i.a ++ i.b) ++ i.c,
////        i => i.a ++ (i.b ++ i.c))
////
////    private val commutativity: Laws =
////      equal2("commutativity",
////        (x, y) => x ++ y,
////        (x, y) => y ++ x)
////
////    private val validity: Laws = {
////      val props = new Props(module)
////      prop2("validity", props.provSet)(_ ++ _)
////    }
////
////    val laws: Laws =
////      List(
////        idempotency,
////        associativity,
////        commutativity,
////        validity,
////      ).reduce(_ & _)
//  }

}