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

  sealed trait SortConclusive            extends NoBlanks { final protected def __sortConcl = ??? }
  sealed trait SortInconclusive          extends Column   { final protected def __sortConcl = ??? }
  sealed trait SortInconclusiveHasBlanks extends SortInconclusive with HasBlanks
  sealed trait SortInconclusiveNoBlanks  extends SortInconclusive with NoBlanks

  sealed trait BuiltIn extends Column
  sealed trait Mandatory extends BuiltIn {self: BuiltIn => }

  // -------------------------------------------------------------------------------------------------------------------

  // NOTE: Keep .builtInValues in sync
  case object Pubid                       extends BuiltIn with SortConclusive            with Mandatory
  case object Code                        extends BuiltIn with SortInconclusiveHasBlanks
  case object Title                       extends BuiltIn with SortInconclusiveHasBlanks with Mandatory
  case object ReqType                     extends BuiltIn with SortInconclusiveNoBlanks
  case object Tags                        extends BuiltIn with SortInconclusiveHasBlanks
  case class Implications(dir: Direction) extends BuiltIn with SortInconclusiveHasBlanks
  case object DeletionReason              extends BuiltIn with SortInconclusiveHasBlanks

  // Field columns
  // - No applicable StaticFields, else they'd be added manually here.
  // - Currently allows any type of CustomField; this may change in future.
  case class CustomField(id: CustomFieldId) extends SortInconclusiveHasBlanks

  // -------------------------------------------------------------------------------------------------------------------

  object Implications {
    private val memo = Direction.memo(new Implications(_))
    def apply(d: Direction): Implications = memo(d)
  }

  @inline implicit def equalityCF : UnivEq[CustomField]               = UnivEq.derive
  @inline implicit def equalityIHB: UnivEq[SortInconclusiveHasBlanks] = UnivEq.derive
  @inline implicit def equalityINB: UnivEq[SortInconclusiveNoBlanks]  = UnivEq.derive
  @inline implicit def equalityI  : UnivEq[SortInconclusive]          = UnivEq.derive
  @inline implicit def equalityC  : UnivEq[SortConclusive]            = UnivEq.derive
  @inline implicit def equalityB  : UnivEq[BuiltIn]                   = UnivEq.derive
  @inline implicit def equalityM  : UnivEq[Mandatory]                 = UnivEq.derive
  @inline implicit def equality   : UnivEq[Column]                    = UnivEq.derive

  val builtInValues: NonEmptyVector[BuiltIn] =
    AdtMacros.adtValuesManually[BuiltIn](
      Pubid,
      Code,
      Title,
      ReqType,
      Tags,
      Implications(Forwards), Implications(Backwards),
      DeletionReason)

  val isMandatory: Column => Boolean = {
    case _: Mandatory   => true
    case _              => false
  }

  val mandatory: NonEmptySet[Mandatory] =
    NonEmptySet(Pubid, builtInValues.iterator.collect { case m: Mandatory => m }.toSet)

  def applicabilityForReq[Data](a: ProjectApplicability[FieldId, Data]): ProjectApplicability[Column, Data] =
    ProjectApplicability {
      case ReqType
         | Pubid
         | Code
         | Title
         | Tags
         | DeletionReason
         | _: Implications => Applicability.always
      case CustomField(id) => a.byField(id)
    }

  val applicabilityForCodeGroup: ProjectApplicability[Column, Any] =
    ProjectApplicability {
      case Code
         | Title           => Applicability.always
      case ReqType
         | Pubid
         | Tags
         | DeletionReason
         | _: CustomField
         | _: Implications => Applicability.never
    }

  def all(c: ProjectConfig): NonEmptySet[Column] =
    builtInValues.toNES[Column] ++ c.fields.customFields.valuesIterator.map(f => CustomField(f.id))
}
