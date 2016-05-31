package shipreq.webapp.base.event

import scalaz.Equal
import shipreq.base.util.UtilMacros
import shipreq.base.util.univeq._
import shipreq.webapp.base.text.Text.Equality._

object EventEquality extends EventEquality
trait EventEquality {
  implicit val equalApplicableTagCreate   : Equal[ApplicableTagCreate   ] = UtilMacros.deriveEqual
  implicit val equalApplicableTagUpdate   : Equal[ApplicableTagUpdate   ] = UtilMacros.deriveEqual
  implicit val equalContentRestore        : Equal[ContentRestore        ] = UtilMacros.deriveEqual
  implicit val equalCustomIssueTypeCreate : Equal[CustomIssueTypeCreate ] = UtilMacros.deriveEqual
  implicit val equalCustomIssueTypeDelete : Equal[CustomIssueTypeDelete ] = UtilMacros.deriveEqual
  implicit val equalCustomIssueTypeRestore: Equal[CustomIssueTypeRestore] = UtilMacros.deriveEqual
  implicit val equalCustomIssueTypeUpdate : Equal[CustomIssueTypeUpdate ] = UtilMacros.deriveEqual
  implicit val equalCustomReqTypeCreate   : Equal[CustomReqTypeCreate   ] = UtilMacros.deriveEqual
  implicit val equalCustomReqTypeDelete   : Equal[CustomReqTypeDelete   ] = UtilMacros.deriveEqual
  implicit val equalCustomReqTypeRestore  : Equal[CustomReqTypeRestore  ] = UtilMacros.deriveEqual
  implicit val equalCustomReqTypeUpdate   : Equal[CustomReqTypeUpdate   ] = UtilMacros.deriveEqual
  implicit val equalFieldCustomDelete     : Equal[FieldCustomDelete     ] = UtilMacros.deriveEqual
  implicit val equalFieldCustomImpCreate  : Equal[FieldCustomImpCreate  ] = UtilMacros.deriveEqual
  implicit val equalFieldCustomImpUpdate  : Equal[FieldCustomImpUpdate  ] = UtilMacros.deriveEqual
  implicit val equalFieldCustomRestore    : Equal[FieldCustomRestore    ] = UtilMacros.deriveEqual
  implicit val equalFieldCustomTagCreate  : Equal[FieldCustomTagCreate  ] = UtilMacros.deriveEqual
  implicit val equalFieldCustomTagUpdate  : Equal[FieldCustomTagUpdate  ] = UtilMacros.deriveEqual
  implicit val equalFieldCustomTextCreate : Equal[FieldCustomTextCreate ] = UtilMacros.deriveEqual
  implicit val equalFieldCustomTextUpdate : Equal[FieldCustomTextUpdate ] = UtilMacros.deriveEqual
  implicit val equalFieldReposition       : Equal[FieldReposition       ] = UtilMacros.deriveEqual
  implicit val equalFieldStaticAdd        : Equal[FieldStaticAdd        ] = UtilMacros.deriveEqual
  implicit val equalFieldStaticRemove     : Equal[FieldStaticRemove     ] = UtilMacros.deriveEqual
  implicit val equalGenericReqCreate      : Equal[GenericReqCreate      ] = UtilMacros.deriveEqual
  implicit val equalGenericReqTitleSet    : Equal[GenericReqTitleSet    ] = UtilMacros.deriveEqual
  implicit val equalGenericReqTypeSet     : Equal[GenericReqTypeSet     ] = UtilMacros.deriveEqual
  implicit val equalProjectNameSet        : Equal[ProjectNameSet        ] = UtilMacros.deriveEqual
  implicit val equalProjectTemplateApply  : Equal[ProjectTemplateApply  ] = UtilMacros.deriveEqual
  implicit val equalReqCodeGroupCreate    : Equal[ReqCodeGroupCreate    ] = UtilMacros.deriveEqual
  implicit val equalReqCodeGroupsDelete   : Equal[ReqCodeGroupsDelete   ] = UtilMacros.deriveEqual
  implicit val equalReqCodeGroupUpdate    : Equal[ReqCodeGroupUpdate    ] = UtilMacros.deriveEqual
  implicit val equalReqCodesPatch         : Equal[ReqCodesPatch         ] = UtilMacros.deriveEqual
  implicit val equalReqFieldCustomTextSet : Equal[ReqFieldCustomTextSet ] = UtilMacros.deriveEqual
  implicit val equalReqImplicationsPatch  : Equal[ReqImplicationsPatch  ] = UtilMacros.deriveEqual
  implicit val equalReqsDelete            : Equal[ReqsDelete            ] = UtilMacros.deriveEqual
  implicit val equalReqTagsPatch          : Equal[ReqTagsPatch          ] = UtilMacros.deriveEqual
  implicit val equalTagDelete             : Equal[TagDelete             ] = UtilMacros.deriveEqual
  implicit val equalTagGroupCreate        : Equal[TagGroupCreate        ] = UtilMacros.deriveEqual
  implicit val equalTagGroupUpdate        : Equal[TagGroupUpdate        ] = UtilMacros.deriveEqual
  implicit val equalTagRestore            : Equal[TagRestore            ] = UtilMacros.deriveEqual
  implicit val equalUseCaseCreate         : Equal[UseCaseCreate         ] = UtilMacros.deriveEqual
  implicit val equalUseCaseStepCreate     : Equal[UseCaseStepCreate     ] = UtilMacros.deriveEqual
  implicit val equalUseCaseStepDelete     : Equal[UseCaseStepDelete     ] = UtilMacros.deriveEqual
  implicit val equalUseCaseStepRestore    : Equal[UseCaseStepRestore    ] = UtilMacros.deriveEqual
  implicit val equalUseCaseStepShiftLeft  : Equal[UseCaseStepShiftLeft  ] = UtilMacros.deriveEqual
  implicit val equalUseCaseStepShiftRight : Equal[UseCaseStepShiftRight ] = UtilMacros.deriveEqual
  implicit val equalUseCaseStepUpdate     : Equal[UseCaseStepUpdate     ] = UtilMacros.deriveEqual
  implicit val equalUseCaseTitleSet       : Equal[UseCaseTitleSet       ] = UtilMacros.deriveEqual

  implicit val equalActiveEvent: Equal[ActiveEvent] = UtilMacros.deriveEqual
}
