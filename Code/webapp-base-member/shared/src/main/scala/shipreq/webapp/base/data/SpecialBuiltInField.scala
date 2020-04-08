package shipreq.webapp.base.data

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq.UnivEq
import shipreq.base.util.{Backwards, Direction, Forwards}

sealed trait SpecialBuiltInField {
  val name: String
}

object SpecialBuiltInField {

  sealed abstract class Instance(_name: String) extends SpecialBuiltInField {
    final val name = _name
  }

  sealed trait Always   extends SpecialBuiltInField
  sealed trait DeadOnly extends SpecialBuiltInField
  sealed trait FilterOk extends SpecialBuiltInField

  case object Pubid          extends Instance("ID")              with Always
  case object Code           extends Instance("Code")            with Always
  case object Codes          extends Instance("Codes")           with Always
  case object Title          extends Instance("Title")           with Always with FilterOk
  case object ImplyBackward  extends Instance("Implied By")      with Always
  case object ImplyForward   extends Instance("Implies")         with Always
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

  val filterOk: NonEmptyVector[FilterOk] =
    AdtMacros.adtValues[FilterOk]

  lazy val filterOkByName: Map[String, FilterOk] =
    filterOk.iterator.map(f => f.name -> f).toMap

  lazy val filterOkByNameLowercase: Map[String, FilterOk] =
    filterOkByName.mapKeysNow(_.toLowerCase)

  implicit def univEqF: UnivEq[FilterOk           ] = UnivEq.derive
  implicit def univEq : UnivEq[SpecialBuiltInField] = UnivEq.derive
}
