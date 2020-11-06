package shipreq.webapp.member.project.data

import shipreq.base.util.TaggedTypes._
import shipreq.base.util.{IsoBool, Valid, Validity}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

sealed abstract class Live extends IsoBool.WithBoolOps[Live] {
  override final def companion = Live
}

case object Live extends Live with IsoBool.Object[Live] {
  override def positive = this
  override def negative = Dead
  val whenValid: Validity => Live = fnToThisWhen(Valid)
}

case object Dead extends Live

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

sealed abstract class Mandatory(final val toText: String) extends IsoBool[Mandatory] {
  override final def companion = Mandatory
}

case object Mandatory extends Mandatory("Mandatory") with IsoBool.Object[Mandatory] {
  override def positive = this
  override def negative = Optional
}

case object Optional extends Mandatory("Optional")

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * A key by which users can refer to data.
 * These references require a hashtag prefix.
 *
 * Examples:
 * #TBD refers to a custom issue type.
 * #pri=high refers to a grouping.
 */
final case class HashRefKey(value: String) extends TaggedString {
  def with_# : String = "#" + value
}
