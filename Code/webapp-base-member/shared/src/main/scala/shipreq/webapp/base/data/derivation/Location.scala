package shipreq.webapp.base.data.derivation

import shipreq.webapp.base.data._

// =====================================================================================================================

sealed trait LocationOf

object LocationOf {

  sealed trait Tag extends LocationOf
  object Tag {
    final case class Req(id: ReqId, subLoc: InReq) extends Tag
    sealed trait InReq
  }

  sealed trait Text extends LocationOf
  object Text {
    final case class Req(id: ReqId, subLoc: InReq) extends Text
    final case class ReqCodeGroup(id: ReqCodeGroupId, subLoc: InReqCodeGroup) extends Text
    sealed trait InReq
    sealed trait InReqCodeGroup
  }

  implicit def univEqTagInReq : UnivEq[Tag.InReq ] = UnivEq.derive
  implicit def univEqTextInReq: UnivEq[Text.InReq] = UnivEq.derive
}

// =====================================================================================================================

sealed trait Location

object Location {

  // Level 1 (top)

  final case class Req(id: ReqId, subLoc: InReq) extends Location

  final case class ReqCodeGroup(id: ReqCodeGroupId, subLoc: InReqCodeGroup) extends Location

  // Level 2

  sealed trait InReq

  sealed trait InReqCodeGroup

  case object FieldDefault extends InReq with LocationOf.Tag.InReq

  case object Tags extends InReq with LocationOf.Tag.InReq

  sealed trait Text extends InReq with LocationOf.Tag.InReq with LocationOf.Text.InReq

  object Text {
    case object Title extends Text with InReqCodeGroup with LocationOf.Text.InReqCodeGroup

    final case class CustomTextField(fieldId: CustomField.Text.Id) extends Text

    final case class UseCaseStep(stepId: UseCaseStepId) extends Text
  }

  implicit def fromLocationOfTextInReq(x: LocationOf.Text.InReq): InReq =
    x match {
      case a: Text => a
    }

  implicit def fromLocationOfTextInRcg(x: LocationOf.Text.InReqCodeGroup): InReqCodeGroup =
    x match {
      case Text.Title => Text.Title
    }
}

// =====================================================================================================================

final case class LocAndValue[+L, +V](loc: L, value: V)

object LocAndValue {
  implicit def univEq[L: UnivEq, V: UnivEq]: UnivEq[LocAndValue[L, V]] = UnivEq.derive
}