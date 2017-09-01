package shipreq.base.util

import scalaz.{-\/, \/-}

sealed trait Applicable extends IsoBool.WithBoolOps[Applicable] {
  override final def companion = Applicable
}

case object NotApplicable extends Applicable {
  val left: IfApplicable[Nothing] = -\/(NotApplicable)
  @inline implicit def autoLeft[A](a: NotApplicable.type): IfApplicable[A] = left
}

case object Applicable extends Applicable with IsoBool.Object[Applicable] {
  override def positive = Applicable
  override def negative = NotApplicable

  val always: Any => Applicable = _ => Applicable
  val never : Any => Applicable = _ => NotApplicable

  @inline def apply[A](a: A): IfApplicable[A] =
    \/-(a)
}
