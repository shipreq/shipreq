package shipreq.base.util

import japgolly.microlibs.utils.BiMap
import monocle._
import scalaz.Leibniz.===
/**
  * +--------+     +--------+     +--------+
  * |        |     |        |     |        |
  * |   A1   +-----+   B1   +-----+   C1   |
  * |        |     |        |     |        |
  * |   A2   +-----+   B2   |     |        |
  * |        |     |        |     |        |
  * |   A3   |     |        |     |   C3   |
  * |        |     |        |     |        |
  * |   A4   |     |        |     |        |
  * |        |     |        |     |        |
  * |        |     |   B5   +-----+   C5   |
  * |        |     |        |     |        |
  * |        |     |   B6   |     |        |
  * |        |     |        |     |        |
  * |        |     |        |     |   C7   |
  * |        |     |        |     |        |
  * +--------+     +--------+     +--------+
  *
  * |<- Intersection[A,B] ->|
  *
  *                |<- Intersection[B,C] ->|
  *
  * |<-------- Intersection[A, C] -------->|
  */
abstract class Intersection[A, B] {

  def reverse: Intersection[B, A]

  def id: Option[B === A]

  val getOption: A => Option[B]

  def get(a: A, default: => B): B =
    getOption(a).getOrElse(default)

  def fold[C](a: A, f: B => C)(default: => C): C =
    getOption(a).fold(default)(f)

  @inline def foldFlip[C](a: A, default: => C)(f: B => C): C =
    fold(a, f)(default)

  def foldWarn[C](a: A, f: B => C)(default: => C): C =
    getOption(a).fold({
      Intersection.warnDiscard(a)
      default
    })(f)

  @inline def foldWarnFlip[C](a: A, default: => C)(f: B => C): C =
    foldWarn(a, f)(default)

  def getOptionMap[C](a: A, f: B => C): Option[C] =
    getOption(a).map(f)

  def <=>[C](that: Intersection[B, C]): Intersection[A, C] = {
    val f1 = getOption
    val g1 = reverse.getOption
    val f2 = that.getOption
    val g2 = that.reverse.getOption
    Intersection[A, C](f1(_) flatMap f2)(g2(_) flatMap g1)
  }

  final def <=>[C](that: Iso[B, C]): Intersection[A, C] =
    Intersection[A, C](getOption(_).map(that.get))(reverse.getOption compose that.reverseGet)

  final def <=>[C](that: Prism[B, C]): Intersection[A, C] =
    Intersection[A, C](getOption(_).flatMap(that.getOption))(reverse.getOption compose that.reverseGet)

  final def toIso: Iso[Option[A], Option[B]] =
    Iso((_: Option[A]) flatMap getOption)(_ flatMap reverse.getOption)

  final def toPrism: Prism[Option[A], B] =
    Prism((_: Option[A]) flatMap getOption)(reverse.getOption)

  def strengthL[L]: Intersection[(L, A), (L, B)] = {
    def lift[X, Y](f: X => Option[Y]): ((L, X)) => Option[(L, Y)] = lx => f(lx._2).map((lx._1, _))
    Intersection(lift(getOption))(lift(reverse.getOption))
  }

  def strengthR[R]: Intersection[(A, R), (B, R)] = {
    def lift[X, Y](f: X => Option[Y]): ((X, R)) => Option[(Y, R)] = xr => f(xr._1).map((_, xr._2))
    Intersection(lift(getOption))(lift(reverse.getOption))
  }

  def choiceL[L]: Intersection[L \/ A, L \/ B] = {
    @nowarn("cat=unused")
    def lift[X, Y](f: X => Option[Y]): L \/ X => Option[L \/ Y] = {
      case l@ -\/(_) => Some(l)
      case \/-(_) => None
    }
    Intersection(lift(getOption))(lift(reverse.getOption))
  }

  def choiceR[R]: Intersection[A \/ R, B \/ R] = {
    @nowarn("cat=unused")
    def lift[X, Y](f: X => Option[Y]): X \/ R => Option[Y \/ R] = {
      case r@ \/-(_) => Some(r)
      case -\/(_) => None
    }
    Intersection(lift(getOption))(lift(reverse.getOption))
  }

  def flattenL[L](implicit ev: Intersection[A, B] =:= Intersection[Option[L], B]): Intersection[L, B] = {
    val self = ev(this)
    val f = self.getOption
    val g = self.reverse.getOption
    Intersection[L, B](l => f(Some(l)))(g(_).flatten)
  }

  def flattenR[R](implicit ev: Intersection[A, B] =:= Intersection[A, Option[R]]): Intersection[A, R] =
    ev(this).reverse.flattenL[R].reverse

  def flatten[L, R](implicit ev: Intersection[A, B] =:= Intersection[Option[L], Option[R]]): Intersection[L, R] =
    ev(this).flattenL[L].flattenR[R]

  final def getThenFlatMap[C](f: B => Option[C]): A => Option[C] =
    id.fold[A => Option[C]](getOption(_).flatMap(f))(_.subst[? => Option[C]](f))
}

object Intersection {

  private final class Id[A] extends Intersection[A, A] {
    override lazy val id                                         = Some(implicitly[A === A])
    override val getOption                                       = Some(_: A)
    override def reverse                                         = this
    override def get         (a: A, default: => A)               = a
    override def fold        [C](a: A, f: A => C)(default: => C) = f(a)
    override def getOptionMap[C](a: A, f: A => C)                = Some(f(a))
    override def <=>         [C](that: Intersection[A, C])       = that
  }

  private[this] val idInstance = new Id[Any]

  def id[A]: Intersection[A, A] =
    idInstance.asInstanceOf[Id[A]]

  def apply[A, B](f: A => Option[B])(g: B => Option[A]): Intersection[A, B] = {
    var ab: Intersection[A, B] = null
    ab = new Intersection[A, B] {
        override val getOption = f
        override def id = None
        override val reverse =
          new Intersection[B, A] {
            override val getOption = g
            override def reverse = ab
            override def id = None
          }
      }
    ab
  }

  def fromBiMap[A, B](m: BiMap[A, B]): Intersection[A, B] =
    apply(m.forward.get)(m.backward.get)

  def fromPrism[A, B](p: Prism[A, B]): Intersection[A, B] =
    apply(p.getOption)(b => Some(p reverseGet b))

  def fromIso[A, B](i: Iso[A, B]): Intersection[A, B] =
    apply[A, B](a => Some(i get a))(b => Some(i reverseGet b))

  def toOption[A]: Intersection[A, Option[A]] =
    apply[A, Option[A]](a => Some(Some(a)))(Identity.apply)

  @elidable(elidable.ASSERTION)
  def warnDiscard(key: Any): Unit =
    System.err.println(s"$key is outside intersection.")
}
