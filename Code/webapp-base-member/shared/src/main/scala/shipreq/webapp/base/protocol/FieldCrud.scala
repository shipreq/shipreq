package shipreq.webapp.base.protocol

import boopickle._
import shipreq.base.util.{RelPos => Pos}
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecMemberData._
import Field.ApplicableReqTypes

object FieldCrud {

  sealed trait Values

  case class TextFieldValues(name     : String,
                             key      : FieldRefKey,
                             mandatory: Mandatory,
                             reqTypes : ApplicableReqTypes) extends Values

  case class TagFieldValues(tagId    : TagId,
                            mandatory: Mandatory,
                            reqTypes : ApplicableReqTypes) extends Values

  case class ImplicationFieldValues(reqTypeId: ReqTypeId,
                                    mandatory: Mandatory,
                                    reqTypes : ApplicableReqTypes) extends Values

  implicit def equalText       : UnivEq[TextFieldValues]        = UnivEq.derive
  implicit def equalTag        : UnivEq[TagFieldValues]         = UnivEq.derive
  implicit def equalImplication: UnivEq[ImplicationFieldValues] = UnivEq.derive
  implicit def equalValues     : UnivEq[Values]                 = UnivEq.derive

  implicit val pickleFieldProtocolValues: Pickler[Values] = {
    implicit val pText        = pickleCaseClass[TextFieldValues]
    implicit val pTag         = pickleCaseClass[TagFieldValues]
    implicit val pImplication = pickleCaseClass[ImplicationFieldValues]
    pickleADT
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  type Position = Pos[FieldId]

  sealed trait CfgAction
  object CfgAction {
    final case class Create      (newValues: Values)                    extends CfgAction
    final case class UpdateValues(id: CustomFieldId, newValues: Values) extends CfgAction
    final case class UpdateOrder (id: FieldId, newPos: Position)        extends CfgAction
    final case class Delete      (id: FieldId)                          extends CfgAction
    final case class Restore     (id: FieldId)                          extends CfgAction

    implicit def univEq: UnivEq[CfgAction] = UnivEq.derive

    implicit val pickler: Pickler[CfgAction] = {
      implicit val pCreate       = pickleCaseClass[Create]
      implicit val pUpdateValues = pickleCaseClass[UpdateValues]
      implicit val pUpdateOrder  = pickleCaseClass[UpdateOrder]
      implicit val pDelete       = pickleCaseClass[Delete]
      implicit val pRestore      = pickleCaseClass[Restore]
      pickleADT
    }
  }
}