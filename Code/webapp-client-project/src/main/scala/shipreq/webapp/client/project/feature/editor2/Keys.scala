package shipreq.webapp.client.project.feature.editor2

import japgolly.scalajs.react.extra.Reusability
import scalaz.\/
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.client.project.lib.DataReusability._

sealed abstract class RowKey {
  type FieldKey <: shipreq.webapp.client.project.feature.editor2.FieldKey
}
object RowKey {
  type Aux[C] = RowKey { type FieldKey = C }

  import shipreq.webapp.base.data

  final case class Req(id: data.ReqId) extends RowKey {
    override type FieldKey = FieldKey.ForReq
  }

  final case class CodeGroup(id: data.ReqCodeId) extends RowKey {
    override type FieldKey = FieldKey.ForCodeGroup
  }

  case object UseCaseSteps extends RowKey {
    override type FieldKey = FieldKey.UseCaseStep
  }

  @inline implicit def equality: UnivEq[RowKey] =
    UnivEq.derive

  implicit val reusability: Reusability[RowKey] =
    Reusability.byUnivEq
}


/**
 * ADT representing all types of fields supported by the editor.
 * Meant to be used as a key for some given content (e.g. for requirement FR-1).
 */
sealed trait FieldKey
object FieldKey {
  sealed trait ForReq       extends FieldKey
  sealed trait ForCodeGroup extends FieldKey

  case object ReqType                                                         extends ForReq
  case object Code                                                            extends ForReq with ForCodeGroup
  case object Title                                                           extends ForReq with ForCodeGroup
  case class  CustomTextField(field: CustomField.Text.Id)                     extends ForReq
  case class  Tags           (field: Option[CustomField.Tag.Id])              extends ForReq
  case class  Implications   (scope: CustomField.Implication.Id \/ Direction) extends ForReq
  case class  UseCaseStep    (id: UseCaseStepId)                              extends FieldKey

  // DeletionReason is a bit odd in that it is append-only, not directly editable.
  // case object DeletionReason extends CellKey

  @inline implicit def equality: UnivEq[FieldKey] =
    UnivEq.derive

  implicit val reusability: Reusability[FieldKey] =
    Reusability.byUnivEq

  val filterForReq: Option[FieldKey] => Option[ForReq] =
    _.filter {
      case _: ForReq => true
      case _         => false
    }.asInstanceOf[Option[ForReq]]

  val filterForCodeGroup: Option[FieldKey] => Option[ForCodeGroup] =
    _.filter {
      case _: ForCodeGroup => true
      case _               => false
    }.asInstanceOf[Option[ForCodeGroup]]
}
