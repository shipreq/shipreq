package shipreq.base.util

import monocle._

abstract class Intersection[A, B] {

  def reverse: Intersection[B, A]

  val getOption: A => Option[B]

  def get(a: A, default: => B): B =
    getOption(a).getOrElse(default)

  def fold[C](a: A, f: B => C)(default: => C): C =
    getOption(a).fold(default)(f)

  def getOptionMap[C](a: A, f: B => C): Option[C] =
    getOption(a).map(f)

  def composeIntersection[C](that: Intersection[B, C]): Intersection[A, C] =
    Intersection[A, C](getOption(_) flatMap that.getOption)(that.reverse.getOption(_) flatMap reverse.getOption)

  final def toIso: Iso[Option[A], Option[B]] =
    Iso((_: Option[A]) flatMap getOption)(_ flatMap reverse.getOption)

  final def toPrism: Prism[Option[A], B] =
    Prism((_: Option[A]) flatMap getOption)(reverse.getOption)
}

object Intersection {

  private final class Id[A] extends Intersection[A, A] {
    override val getOption                                              = Some(_: A)
    override val reverse                                                = this
    override def get                (a: A, default: => A)               = a
    override def fold               [C](a: A, f: A => C)(default: => C) = f(a)
    override def getOptionMap       [C](a: A, f: A => C)                = Some(f(a))
    override def composeIntersection[C](that: Intersection[A, C])       = that
  }

  private[this] val idInstance = new Id[Any]

  def id[A]: Intersection[A, A] =
    idInstance.asInstanceOf[Id[A]]

  def apply[A, B](f: A => Option[B])(g: B => Option[A]): Intersection[A, B] = {
    lazy val ab: Intersection[A, B] =
      new Intersection[A, B] {
        override val getOption = f
        override val reverse =
          new Intersection[B, A] {
            override val getOption = g
            override def reverse = ab
          }
      }
    ab
  }

  def fromPrism[A, B](p: Prism[A, B]): Intersection[A, B] =
    apply(p.getOption)(b => Some(p reverseGet b))

  def fromIso[A, B](i: Iso[A, B]): Intersection[A, B] =
    apply[A, B](a => Some(i get a))(b => Some(i reverseGet b))
}
