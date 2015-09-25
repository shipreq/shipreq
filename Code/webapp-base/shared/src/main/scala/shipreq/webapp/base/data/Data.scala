package shipreq.webapp.base.data

import shipreq.base.util.TaggedTypes._
import shipreq.base.util.IsoBool


sealed abstract class Live {
  final def &&(that: => Live): Live =
    Live <~ ((this :: Live) && (that :: Live))

  final def ||(that: => Live): Live =
    Live <~ ((this :: Live) || (that :: Live))
}
case object Live extends Live with IsoBool.Obj[Live] {
  override protected def neg = Dead
}
case object Dead extends Live with IsoBool[Live] {
  override protected def neg = Live
}


sealed trait ImplicationRequired
case object ImplicationRequired extends ImplicationRequired with IsoBool.Obj[ImplicationRequired] {
  override protected def neg = Not
  case object Not extends ImplicationRequired
}


/**
 * A key by which users can refer to data.
 * These references require a hashtag prefix.
 *
 * Examples:
 * #TBD refers to a custom issue type.
 * #pri=high refers to a grouping.
 */
final case class HashRefKey(value: String) extends TaggedString
