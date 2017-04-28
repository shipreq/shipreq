package shipreq.webapp.client.project.app.reqtable

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import scalaz.{-\/, \/-}
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data
import shipreq.webapp.base.UiText.ColumnNames
import shipreq.webapp.client.base.lib.KeyGen
import shipreq.webapp.client.project.feature.EditorFeature

sealed trait Column {
  // Ensure correct attribute traits are mixed in
  protected def __sortConcl: Nothing
  protected def __blankable: Nothing

  def live: Live

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
  sealed trait BuiltInLive extends BuiltIn {
    override final def live = Live
  }
  sealed trait BuiltInDead extends BuiltIn {
    override final def live = Dead
  }

  // -------------------------------------------------------------------------------------------------------------------

  // NOTE: Keep .builtInValues in sync
  case object Pubid                       extends BuiltInLive with SortConclusive
  case object Code                        extends BuiltInLive with SortInconclusive with HasBlanks
  case object Title                       extends BuiltInLive with SortInconclusive with HasBlanks
  case object ReqType                     extends BuiltInLive with SortInconclusive with NoBlanks
  case object Tags                        extends BuiltInLive with SortInconclusive with HasBlanks
  case class Implications(dir: Direction) extends BuiltInLive with SortInconclusive with HasBlanks
  case object DeletionReason              extends BuiltInDead with SortInconclusive with HasBlanks

  // Field columns
  // - No applicable StaticFields, else they'd be added manually here.
  // - Currently allows any type of CustomField; this may change in future.
  case class CustomField(id: data.CustomFieldId, live: Live) extends SortInconclusive with HasBlanks {
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

  def all(c: ProjectConfig): NonEmptyVector[Column] =
    c.fields.customFields.values.toVector.map(f => CustomField(f.id, f live c)) ++: builtInValues

  def all(c: ProjectConfig, fd: FilterDead): NonEmptyVector[Column] =
    NonEmptyVector.force(all(c).whole filter filterDead(fd))

  val filterDead: FilterDead => Column => Boolean =
    FilterDead.memo(_.filterFnBy[Column](_.live))

  val editorCellIntersection = Intersection[Column, EditorFeature.CellKey] {
    case Column.ReqType                                             => Some(EditorFeature.CellKey.ReqType)
    case Column.Code                                                => Some(EditorFeature.CellKey.Code)
    case Column.Title                                               => Some(EditorFeature.CellKey.Title)
    case Column.Tags                                                => Some(EditorFeature.CellKey.Tags(None))
    case Column.Implications(dir)                                   => Some(EditorFeature.CellKey.Implications(\/-(dir)))
    case Column.CustomField(id: data.CustomField.Implication.Id, _) => Some(EditorFeature.CellKey.Implications(-\/(id)))
    case Column.CustomField(id: data.CustomField.Tag        .Id, _) => Some(EditorFeature.CellKey.Tags(Some(id)))
    case Column.CustomField(id: data.CustomField.Text       .Id, _) => Some(EditorFeature.CellKey.CustomTextField(id))
    case Column.Pubid
       | Column.DeletionReason                                      => None
  } {
    case EditorFeature.CellKey.ReqType                => Some(Column.ReqType)
    case EditorFeature.CellKey.Code                   => Some(Column.Code)
    case EditorFeature.CellKey.Title                  => Some(Column.Title)
    case EditorFeature.CellKey.Tags(None)             => Some(Column.Tags)
    case EditorFeature.CellKey.Implications(\/-(dir)) => Some(Column.Implications(dir))
    case EditorFeature.CellKey.Implications(-\/(id))  => Some(Column.CustomField(id, Live)) // TODO Column shouldn't store Live
    case EditorFeature.CellKey.Tags(Some(id))         => Some(Column.CustomField(id, Live))
    case EditorFeature.CellKey.CustomTextField(id)    => Some(Column.CustomField(id, Live))
    case EditorFeature.CellKey.UseCaseStep(_)         => None
  }

  def field(c: Column, p: ProjectConfig): Option[Field] =
    c match {
      case ReqType
         | Pubid
         | Code
         | Title
         | Tags
         | Implications(_)
         | DeletionReason     => None
      case CustomField(id, _) => Some(p.customField(id))
    }

  def applicability(p: ProjectConfig): Column => Applicability =
    Memo(
      Applicability.fn(
        field(_, p).map(_.applicable), Applicable))

  // -------------------------------------------------------------------------------------------------------------------

  final case class NameResolver(customFieldNames: Map[data.CustomFieldId, String]) {

    @inline def apply(column: Column): String =
      fn(column)

    val fn: Column => String = {
      case b: BuiltIn         => NameResolver.builtIn(b)
      case CustomField(id, _) => customFieldNames(id)
    }
  }

  object NameResolver {
    def byProject(p: Project): NameResolver =
      byFields(p.config.fields.customFields, data.CustomField nameP p)

    def byFields(customFields: data.FieldSet.CustomFields, customFieldName: data.CustomField => String) =
      NameResolver(
        customFields.mapValues(cf =>
          customFieldName(cf)))

    val builtIn: BuiltIn => String = {
      case ReqType           => ColumnNames.reqType
      case Pubid             => ColumnNames.pubid
      case Code              => ColumnNames.code
      case Title             => ColumnNames.title
      case Tags              => ColumnNames.tags
      case Implications(dir) => ColumnNames.implications(dir)
      case DeletionReason    => ColumnNames.deletionReason
    }

    implicit val reusability: Reusability[NameResolver] = {
      implicit val m: Reusability[Map[data.CustomFieldId, String]] = Reusability.byRefOrEqual
      Reusability.caseClass
    }
  }
}
