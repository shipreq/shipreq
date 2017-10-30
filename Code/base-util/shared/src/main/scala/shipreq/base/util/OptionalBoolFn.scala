package shipreq.base.util

import japgolly.microlibs.stdlib_ext.StdlibExt._

final class OptionalBoolFn[A](val value: Option[A => Boolean]) extends AnyVal {

  def apply(a: A): Boolean =
    value.fold(true)(_(a))

  def toFn: A => Boolean =
    value getOrElse OptionalBoolFn.alwaysTrue

  @inline def isEmpty = value.isEmpty

  def unary_! : OptionalBoolFn[A] =
    new OptionalBoolFn(value.map(!_))

  def &&(that: OptionalBoolFn[A]): OptionalBoolFn[A] =
    merge(that, _ && _)

  def ||(that: OptionalBoolFn[A]): OptionalBoolFn[A] =
    merge(that, _ || _)

  private def merge(that: OptionalBoolFn[A], m: (A => Boolean, A => Boolean) => A => Boolean): OptionalBoolFn[A] =
    if (this.isEmpty)
      that
    else if (that.isEmpty)
      this
    else
      OptionalBoolFn(m(this.value.get, that.value.get))

  def map[B](f: (A => Boolean) => B => Boolean): OptionalBoolFn[B] =
    OptionalBoolFn(value map f)

  def contramap[B](f: B => A): OptionalBoolFn[B] =
    OptionalBoolFn(value.map(f.andThen))
}

object OptionalBoolFn {
  private val alwaysTrue = (_: Any) => true

  def apply[A](f: A => Boolean): OptionalBoolFn[A] =
    new OptionalBoolFn(Some(f))

  def apply[A](f: Option[A => Boolean]): OptionalBoolFn[A] =
    new OptionalBoolFn(f)

  def empty[A]: OptionalBoolFn[A] =
    new OptionalBoolFn(None)
}
