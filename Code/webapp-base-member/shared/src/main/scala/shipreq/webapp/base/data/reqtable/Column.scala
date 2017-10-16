package shipreq.webapp.base.data.reqtable

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._

sealed trait Column {
  // Ensure correct attribute traits are mixed in
  protected def __sortConcl: Nothing
  protected def __blankable: Nothing
}

object Column {

  sealed trait HasBlanks        extends Column   { final protected def __blankable = ??? }
  sealed trait NoBlanks         extends Column   { final protected def __blankable = ??? }

  sealed trait SortInconclusive extends Column   { final protected def __sortConcl = ??? }
  sealed trait SortConclusive   extends NoBlanks { final protected def __sortConcl = ??? }

  sealed trait BuiltIn extends Column
  sealed trait Mandatory extends BuiltIn {self: BuiltIn => }

  // -------------------------------------------------------------------------------------------------------------------

  // NOTE: Keep .builtInValues in sync
  case object Pubid                       extends BuiltIn with SortConclusive                  with Mandatory
  case object Code                        extends BuiltIn with SortInconclusive with HasBlanks
  case object Title                       extends BuiltIn with SortInconclusive with HasBlanks with Mandatory
  case object ReqType                     extends BuiltIn with SortInconclusive with NoBlanks
  case object Tags                        extends BuiltIn with SortInconclusive with HasBlanks
  case class Implications(dir: Direction) extends BuiltIn with SortInconclusive with HasBlanks
  case object DeletionReason              extends BuiltIn with SortInconclusive with HasBlanks

  // Field columns
  // - No applicable StaticFields, else they'd be added manually here.
  // - Currently allows any type of CustomField; this may change in future.
  case class CustomField(id: CustomFieldId) extends SortInconclusive with HasBlanks

  // -------------------------------------------------------------------------------------------------------------------

  object Implications {
    private val memo = Direction.memo(new Implications(_))
    def apply(d: Direction): Implications = memo(d)
  }

  @inline implicit def equalityCF : UnivEq[CustomField]                     = UnivEq.derive
  @inline implicit def equalityIHB: UnivEq[SortInconclusive with HasBlanks] = UnivEq.force
  @inline implicit def equalityINB: UnivEq[SortInconclusive with NoBlanks]  = UnivEq.force
  @inline implicit def equalityI  : UnivEq[SortInconclusive]                = UnivEq.derive
  @inline implicit def equalityC  : UnivEq[SortConclusive]                  = UnivEq.derive
  @inline implicit def equalityB  : UnivEq[BuiltIn]                         = UnivEq.derive
  @inline implicit def equality   : UnivEq[Column]                          = UnivEq.derive

  val builtInValues: NonEmptyVector[BuiltIn] =
    AdtMacros.adtValuesManually[BuiltIn](
      Pubid,
      Code,
      Title,
      ReqType,
      Tags,
      Implications(Forwards), Implications(Backwards),
      DeletionReason)

  def applicabilityForReq[Data](a: Applicability[FieldId, Data]): Applicability[Column, Data] =
    Applicability {
      case ReqType
         | Pubid
         | Code
         | Title
         | Tags
         | DeletionReason
         | _: Implications => Applicable.always
      case CustomField(id) => a.byField(id)
    }

  val applicabilityForCodeGroup: Applicability[Column, Any] =
    Applicability {
      case Code
         | Title           => Applicable.always
      case ReqType
         | Pubid
         | Tags
         | DeletionReason
         | _: CustomField
         | _: Implications => Applicable.never
    }

  def all(c: ProjectConfig): NonEmptySet[Column] =
    builtInValues.toNES[Column] ++ c.fields.customFields.valuesIterator.map(f => CustomField(f.id))
}
