package shipreq.base.util

sealed trait Applicable extends IsoBool.WithBoolOps[Applicable] {
  override final def companion = Applicable
}

case object NotApplicable extends Applicable

case object Applicable extends Applicable with IsoBool.Object[Applicable] {
  override def positive = Applicable
  override def negative = NotApplicable

  val always: Any => Applicable = _ => Applicable
  val never : Any => Applicable = _ => NotApplicable
}
