package shipreq.webapp.client.project.app.reqtable2

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import scalaz.{-\/, \/-}
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data
import shipreq.webapp.client.base.lib.KeyGen
import shipreq.webapp.client.project.feature.EditorFeature

sealed trait Column {
  // Ensure correct attribute traits are mixed in
  protected def __sortConcl: Nothing
  protected def __blankable: Nothing

  /** A value that can be passed to React to quickly identify columns. */
  val key: String
}

object Column {

  sealed trait HasBlanks        extends Column   { final protected def __blankable = ??? }
  sealed trait NoBlanks         extends Column   { final protected def __blankable = ??? }

  sealed trait SortInconclusive extends Column   { final protected def __sortConcl = ??? }
  sealed trait SortConclusive   extends NoBlanks { final protected def __sortConcl = ??? }

  sealed trait BuiltIn extends Column {
    override final val key = KeyGen.global.next()
  }

  // -------------------------------------------------------------------------------------------------------------------

  // NOTE: Keep .builtInValues in sync
  case object Pubid                       extends BuiltIn with SortConclusive
  case object Code                        extends BuiltIn with SortInconclusive with HasBlanks
  case object Title                       extends BuiltIn with SortInconclusive with HasBlanks
  case object ReqType                     extends BuiltIn with SortInconclusive with NoBlanks
  case object Tags                        extends BuiltIn with SortInconclusive with HasBlanks
  case class Implications(dir: Direction) extends BuiltIn with SortInconclusive with HasBlanks
  case object DeletionReason              extends BuiltIn with SortInconclusive with HasBlanks

  // Field columns
  // - No applicable StaticFields, else they'd be added manually here.
  // - Currently allows any type of CustomField; this may change in future.
  case class CustomField(id: data.CustomFieldId) extends SortInconclusive with HasBlanks {
    override val key = "f" + id.value
  }

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
  @inline implicit def reusability: Reusability[Column]                     = Reusability.byEqual

  val builtInValues: NonEmptyVector[BuiltIn] =
    // TODO Add adtValuesExcept
    NonEmptyVector(
      Pubid,
      Code,
      Title,
      ReqType,
      Tags,
      Implications(Forwards), Implications(Backwards),
      DeletionReason)

  val mandatory: Set[BuiltIn] =
    UnivEq.emptySet[BuiltIn] + Pubid + Title

  val isMandatory: Column => Boolean = {
    case b: BuiltIn     => mandatory contains b
    case _: CustomField => false
  }

  val editorFieldCG = Intersection[Column, EditorFeature.FieldKey.ForCodeGroup] {
    case Column.Code            => Some(EditorFeature.FieldKey.Code)
    case Column.Title           => Some(EditorFeature.FieldKey.CodeGroupTitle)
    case Column.ReqType
       | Column.Tags
       | Column.DeletionReason
       | Column.Pubid
       | _: Column.CustomField
       | _: Column.Implications => None
  } {
    case EditorFeature.FieldKey.Code           => Some(Column.Code)
    case EditorFeature.FieldKey.CodeGroupTitle => Some(Column.Title)
  }

  val editorFieldGR = Intersection[Column, EditorFeature.FieldKey.ForGenericReq] {
    case Column.ReqType                                          => Some(EditorFeature.FieldKey.ReqType)
    case Column.Code                                             => Some(EditorFeature.FieldKey.Codes)
    case Column.Title                                            => Some(EditorFeature.FieldKey.GenericReqTitle)
    case Column.Tags                                             => Some(EditorFeature.FieldKey.Tags(None))
    case Column.Implications(dir)                                => Some(EditorFeature.FieldKey.Implications(\/-(dir)))
    case Column.CustomField(id: data.CustomField.Implication.Id) => Some(EditorFeature.FieldKey.Implications(-\/(id)))
    case Column.CustomField(id: data.CustomField.Tag        .Id) => Some(EditorFeature.FieldKey.Tags(Some(id)))
    case Column.CustomField(id: data.CustomField.Text       .Id) => Some(EditorFeature.FieldKey.CustomTextField(id))
    case Column.Pubid
       | Column.DeletionReason                                   => None
  } {
    case EditorFeature.FieldKey.ReqType                => Some(Column.ReqType)
    case EditorFeature.FieldKey.Codes                  => Some(Column.Code)
    case EditorFeature.FieldKey.GenericReqTitle        => Some(Column.Title)
    case EditorFeature.FieldKey.Tags(None)             => Some(Column.Tags)
    case EditorFeature.FieldKey.Implications(\/-(dir)) => Some(Column.Implications(dir))
    case EditorFeature.FieldKey.Implications(-\/(id))  => Some(Column.CustomField(id))
    case EditorFeature.FieldKey.Tags(Some(id))         => Some(Column.CustomField(id))
    case EditorFeature.FieldKey.CustomTextField(id)    => Some(Column.CustomField(id))
  }

  val editorFieldUC = Intersection[Column, EditorFeature.FieldKey.ForUseCase] {
    case Column.Code                                             => Some(EditorFeature.FieldKey.Codes)
    case Column.Title                                            => Some(EditorFeature.FieldKey.UseCaseTitle)
    case Column.Tags                                             => Some(EditorFeature.FieldKey.Tags(None))
    case Column.Implications(dir)                                => Some(EditorFeature.FieldKey.Implications(\/-(dir)))
    case Column.CustomField(id: data.CustomField.Implication.Id) => Some(EditorFeature.FieldKey.Implications(-\/(id)))
    case Column.CustomField(id: data.CustomField.Tag        .Id) => Some(EditorFeature.FieldKey.Tags(Some(id)))
    case Column.CustomField(id: data.CustomField.Text       .Id) => Some(EditorFeature.FieldKey.CustomTextField(id))
    case Column.Pubid
       | Column.DeletionReason
       | Column.ReqType                                          => None
  } {
    case EditorFeature.FieldKey.Codes                  => Some(Column.Code)
    case EditorFeature.FieldKey.UseCaseTitle           => Some(Column.Title)
    case EditorFeature.FieldKey.Tags(None)             => Some(Column.Tags)
    case EditorFeature.FieldKey.Implications(\/-(dir)) => Some(Column.Implications(dir))
    case EditorFeature.FieldKey.Implications(-\/(id))  => Some(Column.CustomField(id))
    case EditorFeature.FieldKey.Tags(Some(id))         => Some(Column.CustomField(id))
    case EditorFeature.FieldKey.CustomTextField(id)    => Some(Column.CustomField(id))
  }

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
