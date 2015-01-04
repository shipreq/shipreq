package shipreq.webapp.base.protocol

import shipreq.webapp.base.data._
import Field.ApplicableReqTypes

object FieldProtocol {

  sealed trait Values

  case class TextFieldValues(name     : String,
                             key      : RefKey,
                             mandatory: Mandatory,
                             reqTypes : ApplicableReqTypes) extends Values

  // The field immediately before which the subject field should be ordered. None means append.
  type Position = Option[Field.Id]

  sealed trait CfgAction
  object CfgAction {
    final case class Create      (newValues: Values)                     extends CfgAction
    final case class UpdateValues(id: CustomField.Id, newValues: Values) extends CfgAction
    final case class UpdateOrder (id: Field.Id, newPos: Position)        extends CfgAction
    final case class Delete      (id: Field.Id, action: DeletionAction)  extends CfgAction
  }
}