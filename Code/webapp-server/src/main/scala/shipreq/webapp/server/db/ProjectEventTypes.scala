package shipreq.webapp.server.db

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.utils.Utils
import shipreq.webapp.member.project.event.Event
import shipreq.webapp.member.project.event.Event._

object ProjectEventTypes {

  // ======
  // Config
  // ======

  final val TypeProjectTemplateApply    = 1000

  final val TypeCustomReqTypeCreateV1   = 1010
  final val TypeCustomReqTypeUpdateV1   = 1011
  final val TypeCustomReqTypeDelete     = 1012
  final val TypeCustomReqTypeRestore    = 1013
  final val TypeCustomReqTypeDeleteSoft = 1014
  final val TypeCustomReqTypeDeleteHard = 1015
  final val TypeCustomReqTypeCreateV2   = 1016
  final val TypeCustomReqTypeUpdateV2   = 1017

  final val TypeTagGroupCreate          = 1020
  final val TypeTagGroupUpdate          = 1021
  final val TypeApplicableTagCreateV1   = 1024
  final val TypeApplicableTagUpdateV1   = 1025
  final val TypeApplicableTagCreateV2   = 1026
  final val TypeApplicableTagUpdateV2   = 1027
  final val TypeTagDelete               = 1028
  final val TypeTagRestore              = 1029

  final val TypeCustomIssueTypeCreate   = 1030
  final val TypeCustomIssueTypeUpdate   = 1031
  final val TypeCustomIssueTypeDelete   = 1032
  final val TypeCustomIssueTypeRestore  = 1033

  final val TypeFieldReposition         = 1100
  final val TypeFieldStaticAdd          = 1110
  final val TypeFieldStaticRemove       = 1111
  final val TypeFieldCustomDelete       = 1120
  final val TypeFieldCustomRestore      = 1121
  final val TypeFieldCustomImpCreateV1  = 1130
  final val TypeFieldCustomImpUpdateV1  = 1131
  final val TypeFieldCustomTagCreateV1  = 1132
  final val TypeFieldCustomTagUpdateV1  = 1133
  final val TypeFieldCustomTextCreateV1 = 1134
  final val TypeFieldCustomTextUpdateV1 = 1135
  final val TypeFieldCustomImpCreateV2  = 1140
  final val TypeFieldCustomImpUpdateV2  = 1141
  final val TypeFieldCustomTagCreateV2  = 1142
  final val TypeFieldCustomTagUpdateV2  = 1143
  final val TypeFieldCustomTextCreateV2 = 1144
  final val TypeFieldCustomTextUpdateV2 = 1145

  // =======
  // Content
  // =======

  final val TypeReqFieldCustomTextSet   = 2000
  final val TypeReqTagsPatch            = 2001
  final val TypeReqImplicationsPatch    = 2022
  final val TypeReqCodesPatch           = 2003

  final val TypeCodeGroupCreate         = 2050
  final val TypeCodeGroupUpdate         = 2051
  final val TypeCodeGroupsDelete        = 2052

  final val TypeReqsDelete              = 2099
  final val TypeContentRestore          = 2098

  final val TypeGenericReqCreate        = 2100
  final val TypeGenericReqTitleSet      = 2101
  final val TypeGenericReqTypeSet       = 2102

  final val TypeUseCaseCreate           = 2200
  final val TypeUseCaseTitleSet         = 2201
  final val TypeUseCaseStepCreate       = 2210
  final val TypeUseCaseStepUpdate       = 2211
  final val TypeUseCaseStepShiftLeft    = 2212
  final val TypeUseCaseStepShiftRight   = 2213
  final val TypeUseCaseStepDelete       = 2214
  final val TypeUseCaseStepRestore      = 2215

  // =====
  // Other
  // =====

  final val TypeProjectNameSet          = 3000

  final val TypeManualIssueCreate       = 3010
  final val TypeManualIssueUpdate       = 3011
  final val TypeManualIssueDelete       = 3012

  final val TypeSavedViewCreateV1       = 3020
  final val TypeSavedViewUpdateV1       = 3021
  final val TypeSavedViewDelete         = 3022
  final val TypeSavedViewDefaultSet     = 3023
  final val TypeSavedViewCreate         = 3024
  final val TypeSavedViewUpdate         = 3025

  final val TypeAccessUpdate            = 3100

  // ===================================================================================================================

  private def allTypes = AdtMacros.valuesForAdt[Event, Short] {
    case _: AccessUpdate            => TypeAccessUpdate
    case _: ApplicableTagCreateV1   => TypeApplicableTagCreateV1
    case _: ApplicableTagCreate     => TypeApplicableTagCreateV2
    case _: ApplicableTagUpdateV1   => TypeApplicableTagUpdateV1
    case _: ApplicableTagUpdate     => TypeApplicableTagUpdateV2
    case _: CodeGroupCreate         => TypeCodeGroupCreate
    case _: CodeGroupsDelete        => TypeCodeGroupsDelete
    case _: CodeGroupUpdate         => TypeCodeGroupUpdate
    case _: ContentRestore          => TypeContentRestore
    case _: CustomIssueTypeCreate   => TypeCustomIssueTypeCreate
    case _: CustomIssueTypeDelete   => TypeCustomIssueTypeDelete
    case _: CustomIssueTypeRestore  => TypeCustomIssueTypeRestore
    case _: CustomIssueTypeUpdate   => TypeCustomIssueTypeUpdate
    case _: CustomReqTypeCreateV1   => TypeCustomReqTypeCreateV1
    case _: CustomReqTypeCreate     => TypeCustomReqTypeCreateV2
    case _: CustomReqTypeDelete     => TypeCustomReqTypeDelete
    case _: CustomReqTypeDeleteHard => TypeCustomReqTypeDeleteHard
    case _: CustomReqTypeDeleteSoft => TypeCustomReqTypeDeleteSoft
    case _: CustomReqTypeRestore    => TypeCustomReqTypeRestore
    case _: CustomReqTypeUpdateV1   => TypeCustomReqTypeUpdateV1
    case _: CustomReqTypeUpdate     => TypeCustomReqTypeUpdateV2
    case _: FieldCustomDelete       => TypeFieldCustomDelete
    case _: FieldCustomImpCreateV1  => TypeFieldCustomImpCreateV1
    case _: FieldCustomImpCreate    => TypeFieldCustomImpCreateV2
    case _: FieldCustomImpUpdateV1  => TypeFieldCustomImpUpdateV1
    case _: FieldCustomImpUpdate    => TypeFieldCustomImpUpdateV2
    case _: FieldCustomRestore      => TypeFieldCustomRestore
    case _: FieldCustomTagCreateV1  => TypeFieldCustomTagCreateV1
    case _: FieldCustomTagCreate    => TypeFieldCustomTagCreateV2
    case _: FieldCustomTagUpdateV1  => TypeFieldCustomTagUpdateV1
    case _: FieldCustomTagUpdate    => TypeFieldCustomTagUpdateV2
    case _: FieldCustomTextCreateV1 => TypeFieldCustomTextCreateV1
    case _: FieldCustomTextCreate   => TypeFieldCustomTextCreateV2
    case _: FieldCustomTextUpdateV1 => TypeFieldCustomTextUpdateV1
    case _: FieldCustomTextUpdate   => TypeFieldCustomTextUpdateV2
    case _: FieldReposition         => TypeFieldReposition
    case _: FieldStaticAdd          => TypeFieldStaticAdd
    case _: FieldStaticRemove       => TypeFieldStaticRemove
    case _: GenericReqCreate        => TypeGenericReqCreate
    case _: GenericReqTitleSet      => TypeGenericReqTitleSet
    case _: GenericReqTypeSet       => TypeGenericReqTypeSet
    case _: ManualIssueCreate       => TypeManualIssueCreate
    case _: ManualIssueDelete       => TypeManualIssueDelete
    case _: ManualIssueUpdate       => TypeManualIssueUpdate
    case _: ProjectNameSet          => TypeProjectNameSet
    case _: ProjectTemplateApply    => TypeProjectTemplateApply
    case _: ReqCodesPatch           => TypeReqCodesPatch
    case _: ReqFieldCustomTextSet   => TypeReqFieldCustomTextSet
    case _: ReqImplicationsPatch    => TypeReqImplicationsPatch
    case _: ReqsDelete              => TypeReqsDelete
    case _: ReqTagsPatch            => TypeReqTagsPatch
    case _: SavedViewCreateV1       => TypeSavedViewCreateV1
    case _: SavedViewCreate         => TypeSavedViewCreate
    case _: SavedViewDefaultSet     => TypeSavedViewDefaultSet
    case _: SavedViewDelete         => TypeSavedViewDelete
    case _: SavedViewUpdateV1       => TypeSavedViewUpdateV1
    case _: SavedViewUpdate         => TypeSavedViewUpdate
    case _: TagDelete               => TypeTagDelete
    case _: TagGroupCreate          => TypeTagGroupCreate
    case _: TagGroupUpdate          => TypeTagGroupUpdate
    case _: TagRestore              => TypeTagRestore
    case _: UseCaseCreate           => TypeUseCaseCreate
    case _: UseCaseStepCreate       => TypeUseCaseStepCreate
    case _: UseCaseStepDelete       => TypeUseCaseStepDelete
    case _: UseCaseStepRestore      => TypeUseCaseStepRestore
    case _: UseCaseStepShiftLeft    => TypeUseCaseStepShiftLeft
    case _: UseCaseStepShiftRight   => TypeUseCaseStepShiftRight
    case _: UseCaseStepUpdate       => TypeUseCaseStepUpdate
    case _: UseCaseTitleSet         => TypeUseCaseTitleSet
  }

  private def dups = Utils.dups(allTypes.iterator).toSet

  assert(dups.isEmpty, dups.mkString("Duplicate event types: ", ",", ""))
}
