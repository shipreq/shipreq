package shipreq.base.util

sealed trait Validity extends IsoBool[Validity] with IsoBool.WithBoolOps[Validity] {
  override final def companion = Validity
}

object Validity extends IsoBool.Object[Validity] {
  override def positive = Valid
  override def negative = Invalid

  def apply(d: Any \/ Any): Validity =
    Valid when d.isRight
}

case object Valid extends Validity {
  val always: Any => Validity = _ => Valid
}

case object Invalid extends Validity
