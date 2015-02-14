package shipreq.webapp.client.app.ui.reqtable

import scalaz.{Equal, NonEmptyList}
import shipreq.base.util.IMap
import shipreq.webapp.base.data
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

  // Non-field columns (TODO rename to BuiltIn?)
  sealed trait NonField extends Column
  case object PubId   extends NonField with SortConclusive
  case object Code    extends NonField with SortInconclusive
  case object Desc    extends NonField with SortInconclusive
  case object ReqType extends NonField with SortInconclusive

  val nonFieldValues: NonEmptyList[NonField] =
    NonEmptyList(PubId, Code, Desc, ReqType)

  // Field columns
  // - No applicable StaticFields, else they'd be added manually here.
  // - Currently allows any type of CustomField; this may change in future.
  case class CustomField(id: data.CustomField.Id) extends SortInconclusive

  // -------------------------------------------------------------------------------------------------------------------

  implicit val equalityI: Equal[SortInconclusive] = Equal.equalA
  implicit val equalityC: Equal[SortConclusive]   = Equal.equalA
  implicit val equality : Equal[Column]           = Equal.equalA

  val mandatory: Column => Boolean = {
    case CustomField(_) => false
    case _: NonField    => true
  }

  case class NameResolver(customFields   : IMap[data.CustomField.Id, data.CustomField],
                          customFieldName: data.CustomField => String) {

    @inline def apply(column: Column) = fn(column)
    
    val fn: Column => String = {
      case Column.CustomField(id) => customFields.get(id).map(customFieldName) getOrElse "?"
      case Column.ReqType         => ColumnNames.reqType
      case Column.PubId           => ColumnNames.pubId
      case Column.Code            => ColumnNames.code
      case Column.Desc            => ColumnNames.desc
    }
  }
}
