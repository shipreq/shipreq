package shipreq.webapp.client.project.feature.editor2

import japgolly.scalajs.react.extra.Reusability
import scalaz.\/
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data
import shipreq.webapp.client.project.lib.DataReusability._

sealed abstract class RowKey {
  type FieldKey <: shipreq.webapp.client.project.feature.editor2.FieldKey
}
object RowKey {
  type Aux[F <: FieldKey] = RowKey { type FieldKey = F }

  final case class GenericReq(id: data.GenericReqId) extends RowKey {
    override type FieldKey = FieldKey.ForGenericReq
  }

  final case class UseCase(id: data.UseCaseId) extends RowKey {
    override type FieldKey = FieldKey.ForUseCase
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

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * ADT representing all types of fields supported by the editor.
 * Meant to be used as a key for some given content (e.g. for requirement FR-1).
 */
sealed trait FieldKey
object FieldKey {

  sealed trait ForGenericReq  extends FieldKey
  case object ReqType         extends ForGenericReq
  case object GenericReqTitle extends ForGenericReq

  sealed trait ForUseCase  extends FieldKey
  case object UseCaseTitle extends ForUseCase

  sealed trait ForReq extends ForGenericReq with ForUseCase
  case object Codes                                                                extends ForReq
  case class  CustomTextField(field: data.CustomField.Text.Id)                     extends ForReq
  case class  Implications   (scope: data.CustomField.Implication.Id \/ Direction) extends ForReq
  case class  Tags           (field: Option[data.CustomField.Tag.Id])              extends ForReq

  sealed trait ForCodeGroup  extends FieldKey
  case object Code           extends ForCodeGroup
  case object CodeGroupTitle extends ForCodeGroup

  case class UseCaseStep(id: data.UseCaseStepId) extends FieldKey

  // DeletionReason is a bit odd in that it is append-only, not directly editable.
  // case object DeletionReason extends CellKey

  @inline implicit def equality: UnivEq[FieldKey] =
    UnivEq.derive

  implicit val reusability: Reusability[FieldKey] =
    Reusability.byUnivEq
}
