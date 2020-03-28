package shipreq.webapp.base.data

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.base.util.{Backwards, Direction, Forwards}

sealed trait SpecialBuiltInField {
  val name: String
}

object SpecialBuiltInField {

  sealed abstract class Instance(_name: String) extends SpecialBuiltInField {
    final val name = _name
  }

  sealed abstract trait Always   extends SpecialBuiltInField
  sealed abstract trait DeadOnly extends SpecialBuiltInField

  case object Pubid          extends Instance("ID")              with Always
  case object Code           extends Instance("Code")            with Always
  case object Codes          extends Instance("Codes")           with Always
  case object Title          extends Instance("Title")           with Always
  case object ImplyBackward  extends Instance("Implied By")      with Always
  case object ImplyForward   extends Instance("Implies")         with Always
  case object Tags           extends Instance("Tags")            with Always
  case object ReqType        extends Instance("Req Type")        with Always
  case object DeletionReason extends Instance("Deletion Reason") with DeadOnly

  val implication: Direction => Instance with Always = {
    case Backwards => ImplyBackward
    case Forwards  => ImplyForward
  }

  val values: NonEmptyVector[SpecialBuiltInField] =
    AdtMacros.adtValues[SpecialBuiltInField]

  val namesLowercase: Set[String] =
    values.iterator.map(_.name.toLowerCase).toSet
}
