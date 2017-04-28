package shipreq.webapp.client.project.feature.editor

import japgolly.scalajs.react.extra.Reusability
import scalaz.\/
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.client.project.lib.DataReusability._

sealed abstract class RowKey {
  type CellKeyConstraint <: CellKey
}
object RowKey {
  type Aux[C] = RowKey { type CellKeyConstraint = C }

  import shipreq.webapp.base.data

  final case class Req(id: data.ReqId) extends RowKey {
    override type CellKeyConstraint = CellKey.ForReq
  }

  final case class ReqCodeGroup(id: data.ReqCodeId) extends RowKey {
    override type CellKeyConstraint = CellKey.ForReqCodeGroup
  }

  case object UseCaseSteps extends RowKey {
    override type CellKeyConstraint = CellKey.UseCaseStep
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
sealed trait CellKey
object CellKey {
  sealed trait ForReq          extends CellKey
  sealed trait ForReqCodeGroup extends CellKey

  case object ReqType                                                          extends ForReq
  case object Code                                                             extends ForReq with ForReqCodeGroup
  case object Title                                                            extends ForReq with ForReqCodeGroup
  case class  CustomTextField(field: CustomField.Text.Id)                      extends ForReq
  case class  Tags            (field: Option[CustomField.Tag.Id])              extends ForReq
  case class  Implications    (scope: CustomField.Implication.Id \/ Direction) extends ForReq
  case class  UseCaseStep     (id: UseCaseStepId)                              extends CellKey

  // DeletionReason is a bit odd in that it is append-only, not directly editable.
  // case object DeletionReason extends CellKey

  @inline implicit def equality: UnivEq[CellKey] =
    UnivEq.derive

  implicit val reusability: Reusability[CellKey] =
    Reusability.byUnivEq
}
