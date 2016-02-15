package shipreq.base.util

import monocle._

abstract class Intersection[A, B] {
  val getOption: A => Option[B]
  val reverseGetOption: B => Option[A]

  def reverse: Intersection[B, A] =
    Intersection(reverseGetOption)(getOption)

  def get(a: A, default: => B): B =
    getOption(a).getOrElse(default)

  def reverseGet(b: B, default: => A): A =
    reverseGetOption(b).getOrElse(default)

  def fold[C](a: A, f: B => C)(default: => C): C =
    getOption(a).fold(default)(f)

  def reverseFold[C](b: B, f: A => C)(default: => C): C =
    reverseGetOption(b).fold(default)(f)

  def composeIntersection[C](that: Intersection[B, C]): Intersection[A, C] =
    Intersection[A, C](getOption(_) flatMap that.getOption)(that.reverseGetOption(_) flatMap reverseGetOption)

  final def toIso: Iso[Option[A], Option[B]] =
    Iso((_: Option[A]) flatMap getOption)(_ flatMap reverseGetOption)
}

object Intersection {

  private final class Id[A] extends Intersection[A, A] {
    override val getOption                                              = Some(_: A)
    override val reverseGetOption                                       = getOption
    override def reverse                                                = this
    override def get                (a: A, default: => A)               = a
    override def reverseGet         (a: A, default: => A)               = a
    override def fold               [C](a: A, f: A => C)(default: => C) = f(a)
    override def reverseFold        [C](a: A, f: A => C)(default: => C) = f(a)
    override def composeIntersection[C](that: Intersection[A, C])       = that
  }

  private[this] val idInstance = new Id[Any]

  def id[A]: Intersection[A, A] =
    idInstance.asInstanceOf[Id[A]]

  def apply[A, B](f: A => Option[B])(g: B => Option[A]): Intersection[A, B] =
    new Intersection[A, B] {
      override val getOption = f
      override val reverseGetOption = g
    }
}
