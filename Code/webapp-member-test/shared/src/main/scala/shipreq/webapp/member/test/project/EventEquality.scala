package shipreq.webapp.member.test.project

import cats.Eq
import japgolly.microlibs.cats_ext.CatsMacros
import java.time.Instant
import shipreq.webapp.member.project.event.Event._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.project.filter.Filter.Implicits._
import shipreq.webapp.member.project.text.Text.Equality._

object EventEquality extends EventEquality
trait EventEquality {
  implicit val equalAccessUpdate           : Eq[AccessUpdate           ] = CatsMacros.deriveEq
  implicit val equalApplicableTagCreate    : Eq[ApplicableTagCreate    ] = CatsMacros.deriveEq
  implicit val equalApplicableTagCreateV1  : Eq[ApplicableTagCreateV1  ] = CatsMacros.deriveEq
  implicit val equalApplicableTagUpdate    : Eq[ApplicableTagUpdate    ] = CatsMacros.deriveEq
  implicit val equalApplicableTagUpdateV1  : Eq[ApplicableTagUpdateV1  ] = CatsMacros.deriveEq
  implicit val equalContentRestore         : Eq[ContentRestore         ] = CatsMacros.deriveEq
  implicit val equalCustomIssueTypeCreate  : Eq[CustomIssueTypeCreate  ] = CatsMacros.deriveEq
  implicit val equalCustomIssueTypeDelete  : Eq[CustomIssueTypeDelete  ] = CatsMacros.deriveEq
  implicit val equalCustomIssueTypeRestore : Eq[CustomIssueTypeRestore ] = CatsMacros.deriveEq
  implicit val equalCustomIssueTypeUpdate  : Eq[CustomIssueTypeUpdate  ] = CatsMacros.deriveEq
  implicit val equalCustomReqTypeCreate    : Eq[CustomReqTypeCreate    ] = CatsMacros.deriveEq
  implicit val equalCustomReqTypeCreateV1  : Eq[CustomReqTypeCreateV1  ] = CatsMacros.deriveEq
  implicit val equalCustomReqTypeDelete    : Eq[CustomReqTypeDelete    ] = CatsMacros.deriveEq
  implicit val equalCustomReqTypeDeleteHard: Eq[CustomReqTypeDeleteHard] = CatsMacros.deriveEq
  implicit val equalCustomReqTypeDeleteSoft: Eq[CustomReqTypeDeleteSoft] = CatsMacros.deriveEq
  implicit val equalCustomReqTypeRestore   : Eq[CustomReqTypeRestore   ] = CatsMacros.deriveEq
  implicit val equalCustomReqTypeUpdate    : Eq[CustomReqTypeUpdate    ] = CatsMacros.deriveEq
  implicit val equalCustomReqTypeUpdateV1  : Eq[CustomReqTypeUpdateV1  ] = CatsMacros.deriveEq
  implicit val equalFieldCustomDelete      : Eq[FieldCustomDelete      ] = CatsMacros.deriveEq
  implicit val equalFieldCustomImpCreateV1 : Eq[FieldCustomImpCreateV1 ] = CatsMacros.deriveEq
  implicit val equalFieldCustomImpCreate   : Eq[FieldCustomImpCreate   ] = CatsMacros.deriveEq
  implicit val equalFieldCustomImpUpdateV1 : Eq[FieldCustomImpUpdateV1 ] = CatsMacros.deriveEq
  implicit val equalFieldCustomImpUpdate   : Eq[FieldCustomImpUpdate   ] = CatsMacros.deriveEq
  implicit val equalFieldCustomRestore     : Eq[FieldCustomRestore     ] = CatsMacros.deriveEq
  implicit val equalFieldCustomTagCreateV1 : Eq[FieldCustomTagCreateV1 ] = CatsMacros.deriveEq
  implicit val equalFieldCustomTagCreate   : Eq[FieldCustomTagCreate   ] = CatsMacros.deriveEq
  implicit val equalFieldCustomTagUpdateV1 : Eq[FieldCustomTagUpdateV1 ] = CatsMacros.deriveEq
  implicit val equalFieldCustomTagUpdate   : Eq[FieldCustomTagUpdate   ] = CatsMacros.deriveEq
  implicit val equalFieldCustomTextCreateV1: Eq[FieldCustomTextCreateV1] = CatsMacros.deriveEq
  implicit val equalFieldCustomTextCreate  : Eq[FieldCustomTextCreate  ] = CatsMacros.deriveEq
  implicit val equalFieldCustomTextUpdateV1: Eq[FieldCustomTextUpdateV1] = CatsMacros.deriveEq
  implicit val equalFieldCustomTextUpdate  : Eq[FieldCustomTextUpdate  ] = CatsMacros.deriveEq
  implicit val equalFieldReposition        : Eq[FieldReposition        ] = CatsMacros.deriveEq
  implicit val equalFieldStaticAdd         : Eq[FieldStaticAdd         ] = CatsMacros.deriveEq
  implicit val equalFieldStaticRemove      : Eq[FieldStaticRemove      ] = CatsMacros.deriveEq
  implicit val equalGenericReqCreate       : Eq[GenericReqCreate       ] = CatsMacros.deriveEq
  implicit val equalGenericReqTitleSet     : Eq[GenericReqTitleSet     ] = CatsMacros.deriveEq
  implicit val equalGenericReqTypeSet      : Eq[GenericReqTypeSet      ] = CatsMacros.deriveEq
  implicit val equalManualIssueCreate      : Eq[ManualIssueCreate      ] = CatsMacros.deriveEq
  implicit val equalManualIssueDelete      : Eq[ManualIssueDelete      ] = CatsMacros.deriveEq
  implicit val equalManualIssueUpdate      : Eq[ManualIssueUpdate      ] = CatsMacros.deriveEq
  implicit val equalProjectNameSet         : Eq[ProjectNameSet         ] = CatsMacros.deriveEq
  implicit val equalProjectTemplateApply   : Eq[ProjectTemplateApply   ] = CatsMacros.deriveEq
  implicit val equalCodeGroupCreate        : Eq[CodeGroupCreate        ] = CatsMacros.deriveEq
  implicit val equalCodeGroupsDelete       : Eq[CodeGroupsDelete       ] = CatsMacros.deriveEq
  implicit val equalCodeGroupUpdate        : Eq[CodeGroupUpdate        ] = CatsMacros.deriveEq
  implicit val equalReqCodesPatch          : Eq[ReqCodesPatch          ] = CatsMacros.deriveEq
  implicit val equalReqFieldCustomTextSet  : Eq[ReqFieldCustomTextSet  ] = CatsMacros.deriveEq
  implicit val equalReqImplicationsPatch   : Eq[ReqImplicationsPatch   ] = CatsMacros.deriveEq
  implicit val equalReqsDelete             : Eq[ReqsDelete             ] = CatsMacros.deriveEq
  implicit val equalReqTagsPatch           : Eq[ReqTagsPatch           ] = CatsMacros.deriveEq
  implicit val equalSavedViewCreateV1      : Eq[SavedViewCreateV1      ] = CatsMacros.deriveEq
  implicit val equalSavedViewCreate        : Eq[SavedViewCreate        ] = CatsMacros.deriveEq
  implicit val equalSavedViewDefaultSet    : Eq[SavedViewDefaultSet    ] = CatsMacros.deriveEq
  implicit val equalSavedViewDelete        : Eq[SavedViewDelete        ] = CatsMacros.deriveEq
  implicit val equalSavedViewUpdateV1      : Eq[SavedViewUpdateV1      ] = CatsMacros.deriveEq
  implicit val equalSavedViewUpdate        : Eq[SavedViewUpdate        ] = CatsMacros.deriveEq
  implicit val equalTagDelete              : Eq[TagDelete              ] = CatsMacros.deriveEq
  implicit val equalTagGroupCreate         : Eq[TagGroupCreate         ] = CatsMacros.deriveEq
  implicit val equalTagGroupUpdate         : Eq[TagGroupUpdate         ] = CatsMacros.deriveEq
  implicit val equalTagRestore             : Eq[TagRestore             ] = CatsMacros.deriveEq
  implicit val equalUseCaseCreate          : Eq[UseCaseCreate          ] = CatsMacros.deriveEq
  implicit val equalUseCaseStepCreate      : Eq[UseCaseStepCreate      ] = CatsMacros.deriveEq
  implicit val equalUseCaseStepDelete      : Eq[UseCaseStepDelete      ] = CatsMacros.deriveEq
  implicit val equalUseCaseStepRestore     : Eq[UseCaseStepRestore     ] = CatsMacros.deriveEq
  implicit val equalUseCaseStepShiftLeft   : Eq[UseCaseStepShiftLeft   ] = CatsMacros.deriveEq
  implicit val equalUseCaseStepShiftRight  : Eq[UseCaseStepShiftRight  ] = CatsMacros.deriveEq
  implicit val equalUseCaseStepUpdate      : Eq[UseCaseStepUpdate      ] = CatsMacros.deriveEq
  implicit val equalUseCaseTitleSet        : Eq[UseCaseTitleSet        ] = CatsMacros.deriveEq

  implicit val equalActiveEvent: Eq[ActiveEvent] = CatsMacros.deriveEq
  implicit val equalEvent      : Eq[Event      ] = CatsMacros.deriveEq

  implicit val equalVerifiedEvent   : Eq[VerifiedEvent            ] = CatsMacros.deriveEq
  implicit val equalVerifiedEventSeq: Eq[VerifiedEvent.Seq        ] = Eq.by(_.toList)
  implicit val equalVerifiedEventNES: Eq[VerifiedEvent.NonEmptySeq] = Eq.by(_.values.toList)

  object IgnoreEqualityOfVerifiedEventTimestamps {
    protected implicit val equalInstant: Eq[Instant] = Eq.instance((_, _ ) => true)
    implicit val equalVerifiedEvent   : Eq[VerifiedEvent            ] = CatsMacros.deriveEq
    implicit val equalVerifiedEventSeq: Eq[VerifiedEvent.Seq        ] = Eq.by(_.toList)
    implicit val equalVerifiedEventNES: Eq[VerifiedEvent.NonEmptySeq] = Eq.by(_.values.toList)
  }
}
