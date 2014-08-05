package domainy

import shipreq.base.util.TaggedTypes._

object Data {

  // Fuck this. Use a code generator
  
  final case class CustomIssueTypeId(value: Long) extends TaggedLong
  implicit object CustomIssueTypeId extends TaggedTypeCtor[CustomIssueTypeId]
  final case class CustomIssueTypeV(key: String, desc: Option[String]) {
    def withId(id: CustomIssueTypeId) = CustomIssueType(id, key, desc)
  }
  object CustomIssueTypeV { val fromTuple = (apply _).tupled}
  final case class CustomIssueType(id: CustomIssueTypeId, key: String, desc: Option[String]) {
    def value = CustomIssueTypeV(key, desc)
  }

  // ---------------------------------------------------------------------------

  type ReqTypeMnemonic = String

  final case class CustomReqTypeId(value: Long) extends TaggedLong
  implicit object CustomReqTypeId extends TaggedTypeCtor[CustomReqTypeId]
  /*
  final case class CustomReqTypeV(mnemonic: ReqTypeMnemonic,
                                  oldMnemonics: Set[ReqTypeMnemonic],
                                  name: String,
                                  implicationRequired: Boolean,
                                  alive: Boolean) {
    def withId(id: CustomReqTypeId) = CustomReqType(id, mnemonic, oldMnemonics, name, implicationRequired, alive)
  }
  object CustomReqTypeV { val fromTuple = (apply _).tupled}
  final case class CustomReqType(id: CustomReqTypeId,
                                 mnemonic: ReqTypeMnemonic,
                                 oldMnemonics: Set[ReqTypeMnemonic],
                                 name: String,
                                 implicationRequired: Boolean,
                                 alive: Boolean) {
    def value = CustomReqTypeV(mnemonic, oldMnemonics, name, implicationRequired, alive)
  }
  */

  final case class CustomReqTypeNV(mnemonic: ReqTypeMnemonic, name: String, implicationRequired: Boolean)
  object CustomReqTypeNV { val fromTuple = (apply _).tupled}
  final case class CustomReqType(id: CustomReqTypeId,
                                 mnemonic: ReqTypeMnemonic,
                                 oldMnemonics: Set[ReqTypeMnemonic],
                                 name: String,
                                 implicationRequired: Boolean,
                                 alive: Boolean) {
    def value = CustomReqTypeNV(mnemonic, name, implicationRequired)
  }

  // TODO something like this? Let's wait until it's needed
//  sealed trait ReqType
//  object UseCaseReqType extends ReqType
//  final case class CustomReqType2(id: CustomReqTypeId,
//                                 mnemonic: ReqTypeMnemonic,
//                                 oldMnemonics: Set[ReqTypeMnemonic],
//                                 name: String,
//                                 alive: Boolean)
//  case class ReqTypesRequiringImplicitation(t: Set[ReqType])
}
