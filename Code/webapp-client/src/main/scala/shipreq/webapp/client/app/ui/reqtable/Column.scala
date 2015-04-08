package shipreq.webapp.client.app.ui.reqtable

import shipreq.base.util.{NonEmptyVector, Must, IMap, UnivEq}
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.{UiText, data}
import shipreq.webapp.base.UiText.ColumnNames
import shipreq.webapp.client.util.Reusable

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

  // -------------------------------------------------------------------------------------------------------------------

  case object Pubid          extends BuiltIn with SortConclusive
  case object Code           extends BuiltIn with SortInconclusive with HasBlanks
  case object Desc           extends BuiltIn with SortInconclusive with HasBlanks
  case object ReqType        extends BuiltIn with SortInconclusive with NoBlanks
  case object Tags           extends BuiltIn with SortInconclusive with HasBlanks
  case object ImplicationSrc extends BuiltIn with SortInconclusive with HasBlanks
  case object ImplicationTgt extends BuiltIn with SortInconclusive with HasBlanks

  // Field columns
  // - No applicable StaticFields, else they'd be added manually here.
  // - Currently allows any type of CustomField; this may change in future.
  case class CustomField(id: data.CustomField.Id) extends SortInconclusive with HasBlanks

  // -------------------------------------------------------------------------------------------------------------------

  @inline implicit def equalityIHB: UnivEq[SortInconclusive with HasBlanks] = UnivEq.force
  @inline implicit def equalityINB: UnivEq[SortInconclusive with NoBlanks]  = UnivEq.force
  @inline implicit def equalityI  : UnivEq[SortInconclusive]                = UnivEq.force
  @inline implicit def equalityC  : UnivEq[SortConclusive]                  = UnivEq.force
  @inline implicit def equality   : UnivEq[Column]                          = UnivEq.force
  @inline implicit def reusability: Reusable[Column]                        = Reusable.byUnivEq

  val builtInValues: NonEmptyVector[BuiltIn] =
    NonEmptyVector(Pubid, Code, Desc, ReqType, Tags, ImplicationSrc, ImplicationTgt)

  val mandatory: Column => Boolean = {
    case Pubid
       | Code
       | Desc            => true
    case ReqType
       | Tags
       | ImplicationSrc
       | ImplicationTgt
       | CustomField(_)  => false
  }

  object NameResolver {
    def byProject(p: Project): NameResolver =
      Column.NameResolver(p.fields.data.customFields, data.CustomField nameP p)
  }

  case class NameResolver(customFields   : IMap[data.CustomField.Id, data.CustomField],
                          customFieldName: data.CustomField => Must[String]) {

    @inline def apply(column: Column) = fn(column)

    val fn: Column => String = {
      case CustomField(id) => UiText.mustA(customFields(id) flatMap customFieldName)
      case ReqType         => ColumnNames.reqType
      case Pubid           => ColumnNames.pubid
      case Code            => ColumnNames.code
      case Desc            => ColumnNames.desc
      case Tags            => ColumnNames.tags
      case ImplicationSrc  => ColumnNames.implicationSrc
      case ImplicationTgt  => ColumnNames.implicationTgt
    }
  }

  def all(customFieldsIds: TraversableOnce[data.CustomField.Id]): NonEmptyVector[Column] =
    customFieldsIds.toVector.map(CustomField) ++: builtInValues
}
