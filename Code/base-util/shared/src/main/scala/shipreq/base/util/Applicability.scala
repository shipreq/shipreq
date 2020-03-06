package shipreq.base.util

import scalaz.{-\/, \/-}

sealed trait Applicability extends IsoBool.WithBoolOps[Applicability] {
  override final def companion = Applicability
}

object Applicability extends IsoBool.Object[Applicability] {
  override def positive = Applicable
  override def negative = NotApplicable

  val always: Any => Applicability = _ => Applicable
  val never : Any => Applicability = _ => NotApplicable
}

case object NotApplicable extends Applicability {
  val left: IfApplicable[Nothing] = -\/(NotApplicable)
  @inline implicit def autoLeft[A](a: NotApplicable.type): IfApplicable[A] = left
}

case object Applicable extends Applicability {
  @inline def apply[A](a: A): IfApplicable[A] =
    \/-(a)
}