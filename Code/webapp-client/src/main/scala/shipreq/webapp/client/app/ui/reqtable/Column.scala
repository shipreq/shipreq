package shipreq.webapp.client.app.ui.reqtable

import scalaz.{Equal, NonEmptyList}
import shipreq.webapp.base.data

sealed trait Column
object Column {

  // Non-field columns
  sealed trait NonField extends Column
  case object PubId   extends NonField
  case object Code    extends NonField
  case object Desc    extends NonField
  case object ReqType extends NonField

  val nonFieldValues: NonEmptyList[NonField] =
    NonEmptyList(PubId, Code, Desc, ReqType)

  // Field columns
  // - No applicable StaticFields, else they'd be added manually here.
  // - Currently allows any type of CustomField; this may change in future.
  case class CustomField(id: data.CustomField.Id) extends Column

  // -------------------------------------------------------------------------------------------------------------------

  implicit val equality = Equal.equalA[Column]

  val mandatory: Column => Boolean = {
    case CustomField(_) => false
    case _: NonField    => true
  }
}
