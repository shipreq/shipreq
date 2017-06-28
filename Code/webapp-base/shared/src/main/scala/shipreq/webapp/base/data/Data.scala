package shipreq.webapp.base.data

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

sealed trait ImplicationRequired extends IsoBool[ImplicationRequired] {
  override final def companion = ImplicationRequired
}

case object ImplicationRequired extends ImplicationRequired with IsoBool.Object[ImplicationRequired] {
  override def positive = this
  override def negative = Not
  case object Not extends ImplicationRequired
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * A key by which users can refer to data.
 * These references require a hashtag prefix.
 *
 * Examples:
 * #TBD refers to a custom issue type.
 * #pri=high refers to a grouping.
 */
final case class HashRefKey(value: String) extends TaggedString
