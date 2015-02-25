package shipreq.webapp.client.app.ui.reqtable

import scalaz.{Equal, NonEmptyList}
import shipreq.base.util.{Must, IMap, UnivEq}
import shipreq.webapp.base.{UiText, data}
import shipreq.webapp.base.UiText.ColumnNames

sealed trait Column {
  protected def __sortConclusiveness: Nothing // Ensure sort-conclusiveness is specified for all columns
}
object Column {

  sealed trait SortInconclusive extends Column {
    final protected def __sortConclusiveness = ??? // final for mutual-exclusivity
  }

  sealed trait SortConclusive extends Column {
    final protected def __sortConclusiveness = ??? // final for mutual-exclusivity
  }

  sealed trait BuiltIn extends Column
  case object PubId          extends BuiltIn with SortConclusive
  case object Code           extends BuiltIn with SortInconclusive
  case object Desc           extends BuiltIn with SortInconclusive
  case object ReqType        extends BuiltIn with SortInconclusive
  case object Tags           extends BuiltIn with SortInconclusive
  case object ImplicationSrc extends BuiltIn with SortInconclusive
  case object ImplicationTgt extends BuiltIn with SortInconclusive

  val builtInValues: NonEmptyList[BuiltIn] =
    NonEmptyList(PubId, Code, Desc, ReqType, Tags, ImplicationSrc, ImplicationTgt)

  // Field columns
  // - No applicable StaticFields, else they'd be added manually here.
  // - Currently allows any type of CustomField; this may change in future.
  case class CustomField(id: data.CustomField.Id) extends SortInconclusive

  // -------------------------------------------------------------------------------------------------------------------

  implicit val equalityI: UnivEq[SortInconclusive] = UnivEq.on
  implicit val equalityC: UnivEq[SortConclusive]   = UnivEq.on
  implicit val equality : UnivEq[Column]           = UnivEq.on

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
