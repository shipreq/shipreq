package shipreq.webapp.base.protocol

import boopickle._
import scalaz.\/
import shipreq.webapp.base.event._
import shipreq.webapp.base.hash._
import shipreq.webapp.base.util.GenericDataMacros._
import shipreq.base.util.ErrorMsg
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecBaseData._
import BinCodecMemberData._
import ReqTableDataPicklers._
import AtomPicklers.instances._
import ApplyEvent.LogicVer

object BinCodecEvents {

  implicit val pickleApplicableTagGD   = binpickler(ApplicableTagGD  ).nev
  implicit val pickleCodeGroupGD       = binpickler(CodeGroupGD      ).nev
  implicit val pickleCustomImpFieldGD  = binpickler(CustomImpFieldGD ).nev
  implicit val pickleCustomIssueTypeGD = binpickler(CustomIssueTypeGD).nev
  implicit val pickleCustomReqTypeGD   = binpickler(CustomReqTypeGD  ).nev
  implicit val pickleCustomTagFieldGD  = binpickler(CustomTagFieldGD ).nev
  implicit val pickleCustomTextFieldGD = binpickler(CustomTextFieldGD).nev
  implicit val pickleGenericReqGD      = binpickler(GenericReqGD     ).values
  implicit val pickleSavedViewGD       = binpickler(SavedViewGD      ).nev
  implicit val pickleTagGroupGD        = binpickler(TagGroupGD       ).nev
  implicit val pickleUseCaseGD         = binpickler(UseCaseGD        ).values
  implicit val pickleUseCaseStepGD     = binpickler(UseCaseStepGD    ).nev

  implicit val pickleProjectTemplate: Pickler[ProjectTemplate] = pickleEnum(ProjectTemplate.values)

  implicit val pickleApplicableTagCreate   : Pickler[ApplicableTagCreate   ] = pickleCaseClass
  implicit val pickleApplicableTagUpdate   : Pickler[ApplicableTagUpdate   ] = pickleCaseClass
  implicit val pickleContentRestore        : Pickler[ContentRestore        ] = pickleCaseClass
  implicit val pickleCustomIssueTypeCreate : Pickler[CustomIssueTypeCreate ] = pickleCaseClass
  implicit val pickleCustomIssueTypeDelete : Pickler[CustomIssueTypeDelete ] = pickleCaseClass
  implicit val pickleCustomIssueTypeRestore: Pickler[CustomIssueTypeRestore] = pickleCaseClass
  implicit val pickleCustomIssueTypeUpdate : Pickler[CustomIssueTypeUpdate ] = pickleCaseClass
  implicit val pickleCustomReqTypeCreate   : Pickler[CustomReqTypeCreate   ] = pickleCaseClass
  implicit val pickleCustomReqTypeDelete   : Pickler[CustomReqTypeDelete   ] = pickleCaseClass
  implicit val pickleCustomReqTypeRestore  : Pickler[CustomReqTypeRestore  ] = pickleCaseClass
  implicit val pickleCustomReqTypeUpdate   : Pickler[CustomReqTypeUpdate   ] = pickleCaseClass
  implicit val pickleFieldCustomDelete     : Pickler[FieldCustomDelete     ] = pickleCaseClass
  implicit val pickleFieldCustomImpCreate  : Pickler[FieldCustomImpCreate  ] = pickleCaseClass
  implicit val pickleFieldCustomImpUpdate  : Pickler[FieldCustomImpUpdate  ] = pickleCaseClass
  implicit val pickleFieldCustomRestore    : Pickler[FieldCustomRestore    ] = pickleCaseClass
  implicit val pickleFieldCustomTagCreate  : Pickler[FieldCustomTagCreate  ] = pickleCaseClass
  implicit val pickleFieldCustomTagUpdate  : Pickler[FieldCustomTagUpdate  ] = pickleCaseClass
  implicit val pickleFieldCustomTextCreate : Pickler[FieldCustomTextCreate ] = pickleCaseClass
  implicit val pickleFieldCustomTextUpdate : Pickler[FieldCustomTextUpdate ] = pickleCaseClass
  implicit val pickleFieldReposition       : Pickler[FieldReposition       ] = pickleCaseClass
  implicit val pickleFieldStaticAdd        : Pickler[FieldStaticAdd        ] = pickleCaseClass
  implicit val pickleFieldStaticRemove     : Pickler[FieldStaticRemove     ] = pickleCaseClass
  implicit val pickleGenericReqCreate      : Pickler[GenericReqCreate      ] = pickleCaseClass
  implicit val pickleGenericReqTitleSet    : Pickler[GenericReqTitleSet    ] = pickleCaseClass
  implicit val pickleGenericReqTypeSet     : Pickler[GenericReqTypeSet     ] = pickleCaseClass
  implicit val pickleProjectNameSet        : Pickler[ProjectNameSet        ] = pickleCaseClass
  implicit val pickleProjectTemplateApply  : Pickler[ProjectTemplateApply  ] = pickleCaseClass
  implicit val pickleCodeGroupCreate       : Pickler[CodeGroupCreate       ] = pickleCaseClass
  implicit val pickleCodeGroupsDelete      : Pickler[CodeGroupsDelete      ] = pickleCaseClass
  implicit val pickleCodeGroupUpdate       : Pickler[CodeGroupUpdate       ] = pickleCaseClass
  implicit val pickleReqCodesPatch         : Pickler[ReqCodesPatch         ] = pickleCaseClass
  implicit val pickleReqFieldCustomTextSet : Pickler[ReqFieldCustomTextSet ] = pickleCaseClass
  implicit val pickleReqImplicationsPatch  : Pickler[ReqImplicationsPatch  ] = pickleCaseClass
  implicit val pickleReqsDelete            : Pickler[ReqsDelete            ] = pickleCaseClass
  implicit val pickleReqTagsPatch          : Pickler[ReqTagsPatch          ] = pickleCaseClass
  implicit val pickleSavedViewCreate       : Pickler[SavedViewCreate       ] = pickleCaseClass
  implicit val pickleSavedViewDefaultSet   : Pickler[SavedViewDefaultSet   ] = pickleCaseClass
  implicit val pickleSavedViewDelete       : Pickler[SavedViewDelete       ] = pickleCaseClass
  implicit val pickleSavedViewUpdate       : Pickler[SavedViewUpdate       ] = pickleCaseClass
  implicit val pickleTagDelete             : Pickler[TagDelete             ] = pickleCaseClass
  implicit val pickleTagGroupCreate        : Pickler[TagGroupCreate        ] = pickleCaseClass
  implicit val pickleTagGroupUpdate        : Pickler[TagGroupUpdate        ] = pickleCaseClass
  implicit val pickleTagRestore            : Pickler[TagRestore            ] = pickleCaseClass
  implicit val pickleUseCaseCreate         : Pickler[UseCaseCreate         ] = pickleCaseClass
  implicit val pickleUseCaseStepCreate     : Pickler[UseCaseStepCreate     ] = pickleCaseClass
  implicit val pickleUseCaseStepDelete     : Pickler[UseCaseStepDelete     ] = pickleCaseClass
  implicit val pickleUseCaseStepRestore    : Pickler[UseCaseStepRestore    ] = pickleCaseClass
  implicit val pickleUseCaseStepShiftLeft  : Pickler[UseCaseStepShiftLeft  ] = pickleCaseClass
  implicit val pickleUseCaseStepShiftRight : Pickler[UseCaseStepShiftRight ] = pickleCaseClass
  implicit val pickleUseCaseStepUpdate     : Pickler[UseCaseStepUpdate     ] = pickleCaseClass
  implicit val pickleUseCaseTitleSet       : Pickler[UseCaseTitleSet       ] = pickleCaseClass

  implicit val pickleActiveEvent: Pickler[ActiveEvent] = pickleADT
  implicit val pickleEvent      : Pickler[Event      ] = pickleADT

  implicit val pickleHashScheme: Pickler[HashScheme] =
    intPickler.xmap(HashSchemes unsafeGet HashSchemeId(_))(_.id.index)

  implicit val pickleHashScope: Pickler[HashScope] =
    pickleEnum(HashScope.all)

  implicit val pickleHashRecs: Pickler[HashRecs] = {
    implicit val pickleHashRecs2: Pickler[HashRecsForScheme] = mapPickler
    mapPickler[HashScheme, HashRecsForScheme, Map]
  }

  implicit val pickleEventOrd        : Pickler[EventOrd                 ] = pickleCaseClass
  implicit val pickleEventOrdLatest  : Pickler[EventOrd.Latest          ] = pickleCaseClass
  implicit val pickleVerifiedEvent   : Pickler[VerifiedEvent            ] = pickleCaseClass
  implicit val pickleVerifiedEventSeq: Pickler[VerifiedEvent.Seq        ] = iterablePickler
  implicit val pickleVerifiedEventNES: Pickler[VerifiedEvent.NonEmptySeq] = pickleCaseClass
  implicit val pickleProjectAndOrd   : Pickler[ProjectAndOrd            ] = pickleCaseClass

  implicit val pickleErrorMsgOrVerifiedEventSeq: Pickler[ErrorMsg \/ VerifiedEvent.Seq] = pickleXor
}
