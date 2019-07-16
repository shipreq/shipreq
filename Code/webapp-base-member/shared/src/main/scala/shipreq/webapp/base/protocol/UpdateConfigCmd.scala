package shipreq.webapp.base.protocol

import boopickle._
import scalaz.\&/
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecMemberData._
import Field.ApplicableReqTypes

sealed trait UpdateConfigCmd

object UpdateConfigCmd {

  sealed trait ToModifyCustomIssueTypes                                                            extends UpdateConfigCmd
  final case class CustomIssueTypeCreate (values: CustomIssueTypeValues)                           extends ToModifyCustomIssueTypes
  final case class CustomIssueTypeUpdate (id: CustomIssueTypeId, newValues: CustomIssueTypeValues) extends ToModifyCustomIssueTypes
  final case class CustomIssueTypeDelete (id: CustomIssueTypeId)                                   extends ToModifyCustomIssueTypes
  final case class CustomIssueTypeRestore(id: CustomIssueTypeId)                                   extends ToModifyCustomIssueTypes

  sealed trait ToModifyCustomReqTypes                                                        extends UpdateConfigCmd
  final case class CustomReqTypeCreate (values: CustomReqTypeValues)                         extends ToModifyCustomReqTypes
  final case class CustomReqTypeUpdate (id: CustomReqTypeId, newValues: CustomReqTypeValues) extends ToModifyCustomReqTypes
  final case class CustomReqTypeDelete (id: CustomReqTypeId)                                 extends ToModifyCustomReqTypes
  final case class CustomReqTypeRestore(id: CustomReqTypeId)                                 extends ToModifyCustomReqTypes

  sealed trait ToModifyFields                                                                        extends UpdateConfigCmd
  final case class CustomFieldCreate    (values: CustomFieldValues)                                  extends ToModifyFields
  final case class CustomFieldUpdateImp (id: CustomField.Implication.Id, newValues: ImpFieldValues)  extends ToModifyFields
  final case class CustomFieldUpdateTag (id: CustomField.Tag        .Id, newValues: TagFieldValues)  extends ToModifyFields
  final case class CustomFieldUpdateText(id: CustomField.Text       .Id, newValues: TextFieldValues) extends ToModifyFields
  final case class FieldDelete          (id: FieldId)                                                extends ToModifyFields
  final case class FieldRestore         (id: FieldId)                                                extends ToModifyFields
  final case class FieldUpdateOrder     (id: FieldId, newPos: RelPos[FieldId])                       extends ToModifyFields

  sealed trait ToModifyTags                                  extends UpdateConfigCmd
  final case class TagCreate (values: TagData)               extends ToModifyTags
  final case class TagUpdate (id: TagId, newValues: TagData) extends ToModifyTags
  final case class TagDelete (id: TagId)                     extends ToModifyTags
  final case class TagRestore(id: TagId)                     extends ToModifyTags

  // ===================================================================================================================

  final case class CustomIssueTypeValues(key : HashRefKey,
                                         desc: Option[String])

  final case class CustomReqTypeValues(mnemonic: ReqType.Mnemonic,
                                       name    : String,
                                       imp     : ImplicationRequired)

  sealed trait CustomFieldValues

  final case class TextFieldValues(name     : String,
                                   key      : FieldRefKey,
                                   mandatory: Mandatory,
                                   reqTypes : ApplicableReqTypes) extends CustomFieldValues

  final case class TagFieldValues(tagId    : TagId,
                                  mandatory: Mandatory,
                                  reqTypes : ApplicableReqTypes) extends CustomFieldValues

  final case class ImpFieldValues(reqTypeId: ReqTypeId,
                                  mandatory: Mandatory,
                                  reqTypes : ApplicableReqTypes) extends CustomFieldValues

  sealed trait TagValues

  final case class TagGroupValues(name         : String,
                                  mutexChildren: MutexChildren,
                                  desc         : Option[String]) extends TagValues

  final case class ApplicableTagValues(name: String,
                                       key : HashRefKey,
                                       desc: Option[String]) extends TagValues

  type TagData = TagValues \&/ TagInTree.Relations

  // ===================================================================================================================

  implicit def univEqCustomIssueTypeValues : UnivEq[CustomIssueTypeValues] = UnivEq.derive
  implicit def univEqCustomReqTypeValues   : UnivEq[CustomReqTypeValues  ] = UnivEq.derive
  implicit def univEqTextFieldValues       : UnivEq[TextFieldValues      ] = UnivEq.derive
  implicit def univEqTagFieldValues        : UnivEq[TagFieldValues       ] = UnivEq.derive
  implicit def univEqImplicationFieldValues: UnivEq[ImpFieldValues       ] = UnivEq.derive
  implicit def univEqCustomFieldValues     : UnivEq[CustomFieldValues    ] = UnivEq.derive
  implicit def univEqTagGroupValues        : UnivEq[TagGroupValues       ] = UnivEq.derive
  implicit def univEqApplicableTagValues   : UnivEq[ApplicableTagValues  ] = UnivEq.derive
  implicit def univEqTagValues             : UnivEq[TagValues            ] = UnivEq.derive
  implicit def univEq                      : UnivEq[UpdateConfigCmd      ] = UnivEq.derive

  // ===================================================================================================================

  implicit val pickleCustomIssueTypeValues : Pickler[CustomIssueTypeValues ] = pickleCaseClass
  implicit val pickleCustomIssueTypeCreate : Pickler[CustomIssueTypeCreate ] = pickleCaseClass
  implicit val pickleCustomIssueTypeUpdate : Pickler[CustomIssueTypeUpdate ] = pickleCaseClass
  implicit val pickleCustomIssueTypeDelete : Pickler[CustomIssueTypeDelete ] = pickleCaseClass
  implicit val pickleCustomIssueTypeRestore: Pickler[CustomIssueTypeRestore] = pickleCaseClass
  implicit val pickleCustomReqTypeValues   : Pickler[CustomReqTypeValues   ] = pickleCaseClass
  implicit val pickleCustomReqTypeCreate   : Pickler[CustomReqTypeCreate   ] = pickleCaseClass
  implicit val pickleCustomReqTypeUpdate   : Pickler[CustomReqTypeUpdate   ] = pickleCaseClass
  implicit val pickleCustomReqTypeDelete   : Pickler[CustomReqTypeDelete   ] = pickleCaseClass
  implicit val pickleCustomReqTypeRestore  : Pickler[CustomReqTypeRestore  ] = pickleCaseClass
  implicit val pickleTextFieldValues       : Pickler[TextFieldValues       ] = pickleCaseClass
  implicit val pickleTagFieldValues        : Pickler[TagFieldValues        ] = pickleCaseClass
  implicit val pickleImpFieldValues        : Pickler[ImpFieldValues        ] = pickleCaseClass
  implicit val pickleCustomFieldValues     : Pickler[CustomFieldValues     ] = pickleADT
  implicit val pickleCustomFieldCreate     : Pickler[CustomFieldCreate     ] = pickleCaseClass
  implicit val pickleCustomFieldUpdateImp  : Pickler[CustomFieldUpdateImp  ] = pickleCaseClass
  implicit val pickleCustomFieldUpdateTag  : Pickler[CustomFieldUpdateTag  ] = pickleCaseClass
  implicit val pickleCustomFieldUpdateText : Pickler[CustomFieldUpdateText ] = pickleCaseClass
  implicit val pickleFieldDelete           : Pickler[FieldDelete           ] = pickleCaseClass
  implicit val pickleFieldRestore          : Pickler[FieldRestore          ] = pickleCaseClass
  implicit val pickleFieldUpdateOrder      : Pickler[FieldUpdateOrder      ] = pickleCaseClass
  implicit val pickleTagGroupValues        : Pickler[TagGroupValues        ] = pickleCaseClass
  implicit val pickleApplicableTagValues   : Pickler[ApplicableTagValues   ] = pickleCaseClass
  implicit val pickleTagValues             : Pickler[TagValues             ] = pickleADT
  implicit val pickleTagCreate             : Pickler[TagCreate             ] = pickleCaseClass
  implicit val pickleTagUpdate             : Pickler[TagUpdate             ] = pickleCaseClass
  implicit val pickleTagDelete             : Pickler[TagDelete             ] = pickleCaseClass
  implicit val pickleTagRestore            : Pickler[TagRestore            ] = pickleCaseClass
  implicit val pickle                      : Pickler[UpdateConfigCmd       ] = pickleADT
}
