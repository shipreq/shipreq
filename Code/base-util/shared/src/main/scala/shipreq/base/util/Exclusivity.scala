package shipreq.base.util

sealed trait Exclusivity extends IsoBool[Exclusivity] with IsoBool.WithBoolOps[Exclusivity] {
  override final def companion = Exclusivity
}

object Exclusivity extends IsoBool.Object[Exclusivity] {
  override def positive = Exclusive
  override def negative = NonExclusive
}

case object Exclusive extends Exclusivity
case object NonExclusive extends Exclusivity
