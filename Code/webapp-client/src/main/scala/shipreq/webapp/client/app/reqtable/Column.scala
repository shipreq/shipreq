package shipreq.webapp.client.app.reqtable

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util._
import shipreq.webapp.base.data.{Dead, Live, Project, ProjectConfig, Field}
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data
import shipreq.webapp.base.UiText.ColumnNames
import shipreq.webapp.client.data.FilterDead
import shipreq.webapp.client.lib.KeyGen
import shipreq.webapp.client.feature.ContentEditorFeature.EditFieldKey

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

  case object Pubid          extends BuiltInLive with SortConclusive
  case object Code           extends BuiltInLive with SortInconclusive with HasBlanks
  case object Title          extends BuiltInLive with SortInconclusive with HasBlanks
  case object ReqType        extends BuiltInLive with SortInconclusive with NoBlanks
  case object Tags           extends BuiltInLive with SortInconclusive with HasBlanks
  case object ImplicationSrc extends BuiltInLive with SortInconclusive with HasBlanks
  case object ImplicationTgt extends BuiltInLive with SortInconclusive with HasBlanks
  case object DeletionReason extends BuiltInDead with SortInconclusive with HasBlanks

  // Field columns
  // - No applicable StaticFields, else they'd be added manually here.
  // - Currently allows any type of CustomField; this may change in future.
  case class CustomField(id: data.CustomFieldId, live: Live) extends SortInconclusive with HasBlanks {
    override val key = "f" + id.value
  }

  // -------------------------------------------------------------------------------------------------------------------

  @inline implicit def equalityCF : UnivEq[CustomField]                     = UnivEq.derive
  @inline implicit def equalityIHB: UnivEq[SortInconclusive with HasBlanks] = UnivEq.force
  @inline implicit def equalityINB: UnivEq[SortInconclusive with NoBlanks]  = UnivEq.force
  @inline implicit def equalityI  : UnivEq[SortInconclusive]                = UnivEq.derive
  @inline implicit def equalityC  : UnivEq[SortConclusive]                  = UnivEq.derive
  @inline implicit def equalityB  : UnivEq[BuiltIn]                         = UnivEq.derive
  @inline implicit def equality   : UnivEq[Column]                          = UnivEq.derive
  @inline implicit def reusability: Reusability[Column]                     = Reusability.byEqual

  val builtInValues: NonEmptyVector[BuiltIn] =
    NonEmptyVector(Pubid, Code, Title, ReqType, Tags, ImplicationSrc, ImplicationTgt, DeletionReason)

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
    FilterDead.memo(_.filterFnA[Column](_.live))

  val EditFieldKeyIntersection = Intersection[Column, EditFieldKey] {
    case Column.ReqType               => Some(EditFieldKey.ReqType        )
    case Column.Code                  => Some(EditFieldKey.Code           )
    case Column.Title                 => Some(EditFieldKey.Title          )
    case Column.Tags                  => Some(EditFieldKey.Tags           )
    case Column.ImplicationSrc        => Some(EditFieldKey.ImplicationSrc )
    case Column.ImplicationTgt        => Some(EditFieldKey.ImplicationTgt )
    case Column.CustomField(id, Live) => Some(EditFieldKey.CustomField(id))
    case Column.Pubid
       | Column.DeletionReason
       | Column.CustomField(_, Dead)  => None
  } {
    case EditFieldKey.ReqType         => Some(Column.ReqType              )
    case EditFieldKey.Code            => Some(Column.Code                 )
    case EditFieldKey.Title           => Some(Column.Title                )
    case EditFieldKey.Tags            => Some(Column.Tags                 )
    case EditFieldKey.ImplicationSrc  => Some(Column.ImplicationSrc       )
    case EditFieldKey.ImplicationTgt  => Some(Column.ImplicationTgt       )
    case EditFieldKey.CustomField(id) => Some(Column.CustomField(id, Live))
  }

  def field(c: Column, p: ProjectConfig): Option[Field] =
    c match {
      case ReqType
         | Pubid
         | Code
         | Title
         | Tags
         | ImplicationSrc
         | ImplicationTgt
         | DeletionReason     => None
      case CustomField(id, _) => Some(p.customField(id))
    }

  /**
   * Direction of implications relative to row-subject.
   *
   * If forwards, the user edits what this subject implies (ie. subject → edit-specified).
   * If backwards, then it's what implies this subject     (ie. subject ← edit-specified).
   *
   * Note: Copy of reqdetail.Cell.implicationDirection
   */
  def implicationDirection(column: Column): Direction =
    column match {
      case Column.CustomField(_, _) => data.CustomField.Implication.dir
      case Column.ImplicationTgt    => Forwards
      case _                        => Backwards
    }

  // -------------------------------------------------------------------------------------------------------------------

  case class NameResolver(customFieldNames: Map[data.CustomFieldId, String]) {

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
      case ReqType        => ColumnNames.reqType
      case Pubid          => ColumnNames.pubid
      case Code           => ColumnNames.code
      case Title          => ColumnNames.title
      case Tags           => ColumnNames.tags
      case ImplicationSrc => ColumnNames.implicationSrc
      case ImplicationTgt => ColumnNames.implicationTgt
      case DeletionReason => ColumnNames.deletionReason
    }

    implicit val reusability: Reusability[NameResolver] = {
      import UnivEq.Implicits._
      implicit val m: Reusability[Map[data.CustomFieldId, String]] = Reusability.byRefOrEqual
      Reusability.caseClass
    }
  }
}
