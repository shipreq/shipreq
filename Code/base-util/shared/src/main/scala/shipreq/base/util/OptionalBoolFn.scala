package shipreq.base.util

import japgolly.microlibs.stdlib_ext.StdlibExt._
import scala.collection.Factory

final class OptionalBoolFn[A](val value: Option[A => Boolean]) extends AnyVal {

  def apply(a: A): Boolean =
    value.fold(true)(_(a))

  def collection[C[x] <: Iterable[x], B](cb: C[B])(a: B => A)(implicit cbf: Factory[B, C[B]]): C[B] =
    value.fold(cb) { f =>
      val b = cbf.newBuilder
      b ++= cb.iterator.filter(f compose a)
      b.result()
    }

  def iterator[B](bs: Iterator[B])(a: B => A): Iterator[B] =
    value.fold(bs)(f => bs.filter(f compose a))

  def exists(as: IterableOnce[A]): Boolean =
    value.fold(as.iterator.nonEmpty)(as.iterator.exists)

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

  def fail[A]: OptionalBoolFn[A] =
    new OptionalBoolFn(Some(_ => false))
}
