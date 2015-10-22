package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import scala.scalajs.js
import shipreq.base.util.{NonEmptyVector, UnivEq}
import shipreq.webapp.base.data.{Dead, Live, Project, ProjectConfig}
import shipreq.webapp.base.data
import shipreq.webapp.base.UiText.ColumnNames
import shipreq.webapp.client.lib.FilterDead

sealed trait Column {
  // Ensure correct attribute traits are mixed in
  protected def __sortConcl: Nothing
  protected def __blankable: Nothing

  def live: Live

  /** A value that can be passed to React to quickly identify columns. */
  val key: js.Any
}
object Column {

  sealed trait HasBlanks        extends Column   { final protected def __blankable = ??? }
  sealed trait NoBlanks         extends Column   { final protected def __blankable = ??? }

  sealed trait SortInconclusive extends Column   { final protected def __sortConcl = ??? }
  sealed trait SortConclusive   extends NoBlanks { final protected def __sortConcl = ??? }

  private val nextBuiltInKey: () => js.Any = {
    var i = 0
    () => { i += 1; i }
  }

  sealed trait BuiltIn extends Column {
    override final val key = nextBuiltInKey()
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
    override val key: js.Any = -id.value
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

  val filterDead: FilterDead => Column => Boolean =
    FilterDead.memo(_.filterFnA[Column](_.live))

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
