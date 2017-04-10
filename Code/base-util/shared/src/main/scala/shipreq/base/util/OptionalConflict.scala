package shipreq.base.util

import japgolly.microlibs.stdlib_ext.StdlibExt._
import scalaz.Equal

/**
  * Detect when an optional value is defined and conflicts with another value.
  *
  * Undefined values don't conflict (i.e. None doesn't conflict with None).
  */
final case class OptionalConflict[A](value: Option[A]) extends AnyVal {

  def conflictsWith(a: A)(implicit e: Equal[A]): Boolean =
    value.exists(e.equal(a, _))

  def conflictsWithOption(o: Option[A])(implicit e: Equal[A]): Boolean =
    o.exists(conflictsWith)

  def conflictsWithAny(keys: => Iterator[A])(implicit e: Equal[A]): Boolean =
    value.exists(k => keys.exists(e.equal(k, _)))

  def conflictsWithAnyOptions(options: => Iterator[Option[A]])(implicit e: Equal[A]): Boolean =
    conflictsWithAny(options.filterDefined)
}

object OptionalConflict {

  def equalOption[A: Equal]: Equal[Option[A]] =
    Equal.equal((a, b) => OptionalConflict(a).conflictsWithOption(b))
}