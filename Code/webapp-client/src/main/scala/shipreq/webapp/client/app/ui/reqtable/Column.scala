package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util.{NonEmptyVector, Must, IMap, UnivEq}
import shipreq.webapp.base.data.{Live, Project}
import shipreq.webapp.base.{UiText, data}
import shipreq.webapp.base.UiText.ColumnNames

sealed trait Column {
  // Ensure correct attribute traits are mixed in
  protected def __sortConcl: Nothing
  protected def __blankable: Nothing

  def live: Live
}
object Column {

  sealed trait HasBlanks        extends Column   { final protected def __blankable = ??? }
  sealed trait NoBlanks         extends Column   { final protected def __blankable = ??? }

  sealed trait SortInconclusive extends Column   { final protected def __sortConcl = ??? }
  sealed trait SortConclusive   extends NoBlanks { final protected def __sortConcl = ??? }

  sealed trait BuiltIn extends Column {
    override def live = Live
  }

  // -------------------------------------------------------------------------------------------------------------------

  case object Pubid          extends BuiltIn with SortConclusive
  case object Code           extends BuiltIn with SortInconclusive with HasBlanks
  case object Title          extends BuiltIn with SortInconclusive with HasBlanks
  case object ReqType        extends BuiltIn with SortInconclusive with NoBlanks
  case object Tags           extends BuiltIn with SortInconclusive with HasBlanks
  case object ImplicationSrc extends BuiltIn with SortInconclusive with HasBlanks
  case object ImplicationTgt extends BuiltIn with SortInconclusive with HasBlanks

  // Field columns
  // - No applicable StaticFields, else they'd be added manually here.
  // - Currently allows any type of CustomField; this may change in future.
  case class CustomField(id: data.CustomFieldId, live: Live) extends SortInconclusive with HasBlanks

  // -------------------------------------------------------------------------------------------------------------------

  @inline implicit def equalityCF : UnivEq[CustomField]                     = UnivEq.derive
  @inline implicit def equalityIHB: UnivEq[SortInconclusive with HasBlanks] = UnivEq.force
  @inline implicit def equalityINB: UnivEq[SortInconclusive with NoBlanks]  = UnivEq.force
  @inline implicit def equalityI  : UnivEq[SortInconclusive]                = UnivEq.derive
  @inline implicit def equalityC  : UnivEq[SortConclusive]                  = UnivEq.derive
  @inline implicit def equality   : UnivEq[Column]                          = UnivEq.derive
  @inline implicit def reusability: Reusability[Column]                     = Reusability.byEqual

  val builtInValues: NonEmptyVector[BuiltIn] =
    NonEmptyVector(Pubid, Code, Title, ReqType, Tags, ImplicationSrc, ImplicationTgt)

  val mandatory: Column => Boolean = {
    case Pubid
       | Title          => true
    case ReqType
       | Code
       | Tags
       | ImplicationSrc
       | ImplicationTgt
       | _: CustomField => false
  }

  object NameResolver {
    def byProject(p: Project): NameResolver =
      Column.NameResolver(p.config.fields.customFields, data.CustomField nameP p)

    val builtIn: BuiltIn => String = {
      case ReqType        => ColumnNames.reqType
      case Pubid          => ColumnNames.pubid
      case Code           => ColumnNames.code
      case Title          => ColumnNames.title
      case Tags           => ColumnNames.tags
      case ImplicationSrc => ColumnNames.implicationSrc
      case ImplicationTgt => ColumnNames.implicationTgt
    }
  }

  case class NameResolver(customFields   : IMap[data.CustomFieldId, data.CustomField],
                          customFieldName: data.CustomField => Must[String]) {

    @inline def apply(column: Column) = fn(column)

    val fn: Column => String = {
      case b: BuiltIn         => NameResolver.builtIn(b)
      case CustomField(id, _) => UiText.mustA(customFields(id) flatMap customFieldName)
    }
  }

  def all(customFields: TraversableOnce[data.CustomField]): NonEmptyVector[Column] =
    customFields.toVector.map(f => CustomField(f.id, f.live)) ++: builtInValues
}
