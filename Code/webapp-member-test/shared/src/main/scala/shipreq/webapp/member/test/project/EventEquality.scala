package shipreq.webapp.member.test.project

import japgolly.microlibs.scalaz_ext.ScalazMacros
import java.time.Instant
import scalaz.Equal
import scalaz.std.list.listEqual
import shipreq.webapp.member.project.event.Event._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.project.filter.Filter.Implicits._
import shipreq.webapp.member.project.text.Text.Equality._

object EventEquality extends EventEquality
trait EventEquality {
  implicit val equalApplicableTagCreate    : Equal[ApplicableTagCreate    ] = ScalazMacros.deriveEqual
  implicit val equalApplicableTagCreateV1  : Equal[ApplicableTagCreateV1  ] = ScalazMacros.deriveEqual
  implicit val equalApplicableTagUpdate    : Equal[ApplicableTagUpdate    ] = ScalazMacros.deriveEqual
  implicit val equalApplicableTagUpdateV1  : Equal[ApplicableTagUpdateV1  ] = ScalazMacros.deriveEqual
  implicit val equalContentRestore         : Equal[ContentRestore         ] = ScalazMacros.deriveEqual
  implicit val equalCustomIssueTypeCreate  : Equal[CustomIssueTypeCreate  ] = ScalazMacros.deriveEqual
  implicit val equalCustomIssueTypeDelete  : Equal[CustomIssueTypeDelete  ] = ScalazMacros.deriveEqual
  implicit val equalCustomIssueTypeRestore : Equal[CustomIssueTypeRestore ] = ScalazMacros.deriveEqual
  implicit val equalCustomIssueTypeUpdate  : Equal[CustomIssueTypeUpdate  ] = ScalazMacros.deriveEqual
  implicit val equalCustomReqTypeCreate    : Equal[CustomReqTypeCreate    ] = ScalazMacros.deriveEqual
  implicit val equalCustomReqTypeCreateV1  : Equal[CustomReqTypeCreateV1  ] = ScalazMacros.deriveEqual
  implicit val equalCustomReqTypeDelete    : Equal[CustomReqTypeDelete    ] = ScalazMacros.deriveEqual
  implicit val equalCustomReqTypeDeleteHard: Equal[CustomReqTypeDeleteHard] = ScalazMacros.deriveEqual
  implicit val equalCustomReqTypeDeleteSoft: Equal[CustomReqTypeDeleteSoft] = ScalazMacros.deriveEqual
  implicit val equalCustomReqTypeRestore   : Equal[CustomReqTypeRestore   ] = ScalazMacros.deriveEqual
  implicit val equalCustomReqTypeUpdate    : Equal[CustomReqTypeUpdate    ] = ScalazMacros.deriveEqual
  implicit val equalCustomReqTypeUpdateV1  : Equal[CustomReqTypeUpdateV1  ] = ScalazMacros.deriveEqual
  implicit val equalFieldCustomDelete      : Equal[FieldCustomDelete      ] = ScalazMacros.deriveEqual
  implicit val equalFieldCustomImpCreateV1 : Equal[FieldCustomImpCreateV1 ] = ScalazMacros.deriveEqual
  implicit val equalFieldCustomImpCreate   : Equal[FieldCustomImpCreate   ] = ScalazMacros.deriveEqual
  implicit val equalFieldCustomImpUpdateV1 : Equal[FieldCustomImpUpdateV1 ] = ScalazMacros.deriveEqual
  implicit val equalFieldCustomImpUpdate   : Equal[FieldCustomImpUpdate   ] = ScalazMacros.deriveEqual
  implicit val equalFieldCustomRestore     : Equal[FieldCustomRestore     ] = ScalazMacros.deriveEqual
  implicit val equalFieldCustomTagCreateV1 : Equal[FieldCustomTagCreateV1 ] = ScalazMacros.deriveEqual
  implicit val equalFieldCustomTagCreate   : Equal[FieldCustomTagCreate   ] = ScalazMacros.deriveEqual
  implicit val equalFieldCustomTagUpdateV1 : Equal[FieldCustomTagUpdateV1 ] = ScalazMacros.deriveEqual
  implicit val equalFieldCustomTagUpdate   : Equal[FieldCustomTagUpdate   ] = ScalazMacros.deriveEqual
  implicit val equalFieldCustomTextCreateV1: Equal[FieldCustomTextCreateV1] = ScalazMacros.deriveEqual
  implicit val equalFieldCustomTextCreate  : Equal[FieldCustomTextCreate  ] = ScalazMacros.deriveEqual
  implicit val equalFieldCustomTextUpdateV1: Equal[FieldCustomTextUpdateV1] = ScalazMacros.deriveEqual
  implicit val equalFieldCustomTextUpdate  : Equal[FieldCustomTextUpdate  ] = ScalazMacros.deriveEqual
  implicit val equalFieldReposition        : Equal[FieldReposition        ] = ScalazMacros.deriveEqual
  implicit val equalFieldStaticAdd         : Equal[FieldStaticAdd         ] = ScalazMacros.deriveEqual
  implicit val equalFieldStaticRemove      : Equal[FieldStaticRemove      ] = ScalazMacros.deriveEqual
  implicit val equalGenericReqCreate       : Equal[GenericReqCreate       ] = ScalazMacros.deriveEqual
  implicit val equalGenericReqTitleSet     : Equal[GenericReqTitleSet     ] = ScalazMacros.deriveEqual
  implicit val equalGenericReqTypeSet      : Equal[GenericReqTypeSet      ] = ScalazMacros.deriveEqual
  implicit val equalManualIssueCreate      : Equal[ManualIssueCreate      ] = ScalazMacros.deriveEqual
  implicit val equalManualIssueDelete      : Equal[ManualIssueDelete      ] = ScalazMacros.deriveEqual
  implicit val equalManualIssueUpdate      : Equal[ManualIssueUpdate      ] = ScalazMacros.deriveEqual
  implicit val equalProjectNameSet         : Equal[ProjectNameSet         ] = ScalazMacros.deriveEqual
  implicit val equalProjectTemplateApply   : Equal[ProjectTemplateApply   ] = ScalazMacros.deriveEqual
  implicit val equalCodeGroupCreate        : Equal[CodeGroupCreate        ] = ScalazMacros.deriveEqual
  implicit val equalCodeGroupsDelete       : Equal[CodeGroupsDelete       ] = ScalazMacros.deriveEqual
  implicit val equalCodeGroupUpdate        : Equal[CodeGroupUpdate        ] = ScalazMacros.deriveEqual
  implicit val equalReqCodesPatch          : Equal[ReqCodesPatch          ] = ScalazMacros.deriveEqual
  implicit val equalReqFieldCustomTextSet  : Equal[ReqFieldCustomTextSet  ] = ScalazMacros.deriveEqual
  implicit val equalReqImplicationsPatch   : Equal[ReqImplicationsPatch   ] = ScalazMacros.deriveEqual
  implicit val equalReqsDelete             : Equal[ReqsDelete             ] = ScalazMacros.deriveEqual
  implicit val equalReqTagsPatch           : Equal[ReqTagsPatch           ] = ScalazMacros.deriveEqual
  implicit val equalSavedViewCreateV1      : Equal[SavedViewCreateV1      ] = ScalazMacros.deriveEqual
  implicit val equalSavedViewCreate        : Equal[SavedViewCreate        ] = ScalazMacros.deriveEqual
  implicit val equalSavedViewDefaultSet    : Equal[SavedViewDefaultSet    ] = ScalazMacros.deriveEqual
  implicit val equalSavedViewDelete        : Equal[SavedViewDelete        ] = ScalazMacros.deriveEqual
  implicit val equalSavedViewUpdateV1      : Equal[SavedViewUpdateV1      ] = ScalazMacros.deriveEqual
  implicit val equalSavedViewUpdate        : Equal[SavedViewUpdate        ] = ScalazMacros.deriveEqual
  implicit val equalTagDelete              : Equal[TagDelete              ] = ScalazMacros.deriveEqual
  implicit val equalTagGroupCreate         : Equal[TagGroupCreate         ] = ScalazMacros.deriveEqual
  implicit val equalTagGroupUpdate         : Equal[TagGroupUpdate         ] = ScalazMacros.deriveEqual
  implicit val equalTagRestore             : Equal[TagRestore             ] = ScalazMacros.deriveEqual
  implicit val equalUseCaseCreate          : Equal[UseCaseCreate          ] = ScalazMacros.deriveEqual
  implicit val equalUseCaseStepCreate      : Equal[UseCaseStepCreate      ] = ScalazMacros.deriveEqual
  implicit val equalUseCaseStepDelete      : Equal[UseCaseStepDelete      ] = ScalazMacros.deriveEqual
  implicit val equalUseCaseStepRestore     : Equal[UseCaseStepRestore     ] = ScalazMacros.deriveEqual
  implicit val equalUseCaseStepShiftLeft   : Equal[UseCaseStepShiftLeft   ] = ScalazMacros.deriveEqual
  implicit val equalUseCaseStepShiftRight  : Equal[UseCaseStepShiftRight  ] = ScalazMacros.deriveEqual
  implicit val equalUseCaseStepUpdate      : Equal[UseCaseStepUpdate      ] = ScalazMacros.deriveEqual
  implicit val equalUseCaseTitleSet        : Equal[UseCaseTitleSet        ] = ScalazMacros.deriveEqual

  implicit val equalActiveEvent: Equal[ActiveEvent] = ScalazMacros.deriveEqual
  implicit val equalEvent      : Equal[Event      ] = ScalazMacros.deriveEqual

  implicit val equalVerifiedEvent   : Equal[VerifiedEvent            ] = ScalazMacros.deriveEqual
  implicit val equalVerifiedEventSeq: Equal[VerifiedEvent.Seq        ] = Equal.equalBy(_.toList)
  implicit val equalVerifiedEventNES: Equal[VerifiedEvent.NonEmptySeq] = Equal.equalBy(_.values.toList)

  object IgnoreEqualityOfVerifiedEventTimestamps {
    protected implicit val equalInstant: Equal[Instant] = Equal.equal((_, _ ) => true)
    implicit val equalVerifiedEvent   : Equal[VerifiedEvent            ] = ScalazMacros.deriveEqual
    implicit val equalVerifiedEventSeq: Equal[VerifiedEvent.Seq        ] = Equal.equalBy(_.toList)
    implicit val equalVerifiedEventNES: Equal[VerifiedEvent.NonEmptySeq] = Equal.equalBy(_.values.toList)
  }
}
