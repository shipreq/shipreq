package shipreq.webapp.base.event

import scalaz.Equal
import shipreq.base.util.UtilMacros
import shipreq.base.util.UnivEq.Implicits._
import shipreq.webapp.base.text.Text.Equality._

object EventEquality extends EventEquality
trait EventEquality {
  implicit val equalAddStaticField       : Equal[AddStaticField       ] = UtilMacros.deriveEqual
  implicit val equalAddUseCaseStep       : Equal[AddUseCaseStep       ] = UtilMacros.deriveEqual
  implicit val equalApplyTemplate        : Equal[ApplyTemplate        ] = UtilMacros.deriveEqual
  implicit val equalCreateApplicableTag  : Equal[CreateApplicableTag  ] = UtilMacros.deriveEqual
  implicit val equalCreateCustomImpField : Equal[CreateCustomImpField ] = UtilMacros.deriveEqual
  implicit val equalCreateCustomIssueType: Equal[CreateCustomIssueType] = UtilMacros.deriveEqual
  implicit val equalCreateCustomReqType  : Equal[CreateCustomReqType  ] = UtilMacros.deriveEqual
  implicit val equalCreateCustomTagField : Equal[CreateCustomTagField ] = UtilMacros.deriveEqual
  implicit val equalCreateCustomTextField: Equal[CreateCustomTextField] = UtilMacros.deriveEqual
  implicit val equalCreateGenericReq     : Equal[CreateGenericReq     ] = UtilMacros.deriveEqual
  implicit val equalCreateReqCodeGroup   : Equal[CreateReqCodeGroup   ] = UtilMacros.deriveEqual
  implicit val equalCreateTagGroup       : Equal[CreateTagGroup       ] = UtilMacros.deriveEqual
  implicit val equalCreateUseCase        : Equal[CreateUseCase        ] = UtilMacros.deriveEqual
  implicit val equalDeleteCustomField    : Equal[DeleteCustomField    ] = UtilMacros.deriveEqual
  implicit val equalDeleteCustomIssueType: Equal[DeleteCustomIssueType] = UtilMacros.deriveEqual
  implicit val equalDeleteCustomReqType  : Equal[DeleteCustomReqType  ] = UtilMacros.deriveEqual
  implicit val equalDeleteReqCodeGroups  : Equal[DeleteReqCodeGroups  ] = UtilMacros.deriveEqual
  implicit val equalDeleteReqs           : Equal[DeleteReqs           ] = UtilMacros.deriveEqual
  implicit val equalDeleteStaticField    : Equal[DeleteStaticField    ] = UtilMacros.deriveEqual
  implicit val equalDeleteTag            : Equal[DeleteTag            ] = UtilMacros.deriveEqual
  implicit val equalDeleteUseCaseStep    : Equal[DeleteUseCaseStep    ] = UtilMacros.deriveEqual
  implicit val equalPatchImplicationSrc  : Equal[PatchImplicationSrc  ] = UtilMacros.deriveEqual
  implicit val equalPatchImplicationTgt  : Equal[PatchImplicationTgt  ] = UtilMacros.deriveEqual
  implicit val equalPatchReqCodes        : Equal[PatchReqCodes        ] = UtilMacros.deriveEqual
  implicit val equalPatchReqTags         : Equal[PatchReqTags         ] = UtilMacros.deriveEqual
  implicit val equalRepositionField      : Equal[RepositionField      ] = UtilMacros.deriveEqual
  implicit val equalRestoreContent       : Equal[RestoreContent       ] = UtilMacros.deriveEqual
  implicit val equalSetCustomTextField   : Equal[SetCustomTextField   ] = UtilMacros.deriveEqual
  implicit val equalSetGenericReqTitle   : Equal[SetGenericReqTitle   ] = UtilMacros.deriveEqual
  implicit val equalSetGenericReqType    : Equal[SetGenericReqType    ] = UtilMacros.deriveEqual
  implicit val equalSetUseCaseTitle      : Equal[SetUseCaseTitle      ] = UtilMacros.deriveEqual
  implicit val equalShiftUseCaseStepLeft : Equal[ShiftUseCaseStepLeft ] = UtilMacros.deriveEqual
  implicit val equalShiftUseCaseStepRight: Equal[ShiftUseCaseStepRight] = UtilMacros.deriveEqual
  implicit val equalUpdateApplicableTag  : Equal[UpdateApplicableTag  ] = UtilMacros.deriveEqual
  implicit val equalUpdateCustomImpField : Equal[UpdateCustomImpField ] = UtilMacros.deriveEqual
  implicit val equalUpdateCustomIssueType: Equal[UpdateCustomIssueType] = UtilMacros.deriveEqual
  implicit val equalUpdateCustomReqType  : Equal[UpdateCustomReqType  ] = UtilMacros.deriveEqual
  implicit val equalUpdateCustomTagField : Equal[UpdateCustomTagField ] = UtilMacros.deriveEqual
  implicit val equalUpdateCustomTextField: Equal[UpdateCustomTextField] = UtilMacros.deriveEqual
  implicit val equalUpdateReqCodeGroup   : Equal[UpdateReqCodeGroup   ] = UtilMacros.deriveEqual
  implicit val equalUpdateTagGroup       : Equal[UpdateTagGroup       ] = UtilMacros.deriveEqual
  implicit val equalUpdateUseCaseStep    : Equal[UpdateUseCaseStep    ] = UtilMacros.deriveEqual

  implicit val equalActiveEvent: Equal[ActiveEvent] = UtilMacros.deriveEqual
}
