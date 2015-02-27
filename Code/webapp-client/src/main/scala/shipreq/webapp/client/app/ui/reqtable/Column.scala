package shipreq.webapp.client.app.ui.reqtable

import scalaz.NonEmptyList
import shipreq.base.util.{Must, IMap, UnivEq}
import shipreq.webapp.base.{UiText, data}
import shipreq.webapp.base.UiText.ColumnNames

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

  case object PubId          extends BuiltIn with SortConclusive
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

  val builtInValues: NonEmptyList[BuiltIn] =
    NonEmptyList(PubId, Code, Desc, ReqType, Tags, ImplicationSrc, ImplicationTgt)

  val mandatory: Column => Boolean = {
    case PubId
       | Code
       | Desc            => true
    case ReqType
       | Tags
       | ImplicationSrc
       | ImplicationTgt
       | CustomField(_)  => false
  }

  case class NameResolver(customFields   : IMap[data.CustomField.Id, data.CustomField],
                          customFieldName: data.CustomField => Must[String]) {

    @inline def apply(column: Column) = fn(column)

    val fn: Column => String = {
      case Column.CustomField(id) => UiText.mustA(customFields(id) flatMap customFieldName)
      case Column.ReqType         => ColumnNames.reqType
      case Column.PubId           => ColumnNames.pubId
      case Column.Code            => ColumnNames.code
      case Column.Desc            => ColumnNames.desc
      case Column.Tags            => ColumnNames.tags
      case Column.ImplicationSrc  => ColumnNames.implicationSrc
      case Column.ImplicationTgt  => ColumnNames.implicationTgt
    }
  }

  def all(customFieldsIds: TraversableOnce[data.CustomField.Id]): Vector[Column] =
    customFieldsIds.toVector.map(Column.CustomField) ++ Column.builtInValues.list
}
