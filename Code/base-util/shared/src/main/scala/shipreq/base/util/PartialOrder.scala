package shipreq.base.util

import scala.runtime.AbstractFunction2
import shipreq.base.util.PartialOrder.Cmp

/** Laws:
  *   - reflexivity
  *   - antisymmetry
  *   - transitivity
  */
final class PartialOrder[-A](asFn: (A, A) => Cmp) extends AbstractFunction2[A, A, Cmp] { self =>

  override def apply(a: A, b: A): Cmp =
    asFn(a, b)

  def contramap[B](f: B => A): PartialOrder[B] =
    PartialOrder((x, y) => self(f(x), f(y)))

  def orElse[AA <: A](next: PartialOrder[AA]): PartialOrder[AA] =
    PartialOrder((x, y) =>
      self(x, y) match {
        case Cmp.Separate => next(x, y)
        case c            => c
      }
    )

  @nowarn("cat=unused")
  def memo[AA <: A](implicit ev: UnivEq[AA]): PartialOrder[AA] = {
    val cache = collection.mutable.Map.empty[(AA, AA), Cmp]
    PartialOrder { (x, y) =>
      val k = (x, y)
      cache.getOrElseUpdate(k, self(x, y))
    }
  }

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
      Eval.atom("antisymmetry", (a, b), imply(a <= b && b <= a)(a === b, s"$a cmp $b = ${p(a, b)}"))

    private def transitivity(a: A, b: A, c: A): EvalL =
      Eval.atom("transitivity", (a, b, c), imply(a <= b && b <= c)(a <= c, s"$a ≤ $b ≤ $c but ($a cmp $c) = ${p(a, c)}"))

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

        val name = "PartialOrder laws"
        if (result == null) Eval.pass(name) else result.rename(name)
      }
  }

}