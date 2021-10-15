package shipreq.base.util

import cats.Eq
import japgolly.microlibs.stdlib_ext.StdlibExt._

/**
  * Detect when an optional value is defined and conflicts with another value.
  *
  * Undefined values don't conflict (i.e. None doesn't conflict with None).
  */
final case class OptionalConflict[A](value: Option[A]) extends AnyVal {

  def conflictsWith(a: A)(implicit e: Eq[A]): Boolean =
    value.exists(e.eqv(a, _))

  def conflictsWithOption(o: Option[A])(implicit e: Eq[A]): Boolean =
    o.exists(conflictsWith)

  def conflictsWithAny(keys: => Iterator[A])(implicit e: Eq[A]): Boolean =
    value.exists(k => keys.exists(e.eqv(k, _)))

  def conflictsWithAnyOptions(options: => Iterator[Option[A]])(implicit e: Eq[A]): Boolean =
    conflictsWithAny(options.filterDefined)
}

object OptionalConflict {

  def equalOption[A: Eq]: Eq[Option[A]] =
    Eq.instance((a, b) => OptionalConflict(a).conflictsWithOption(b))
}