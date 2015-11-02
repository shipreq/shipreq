package shipreq.webapp.base.protocol

import boopickle._
import shipreq.webapp.base.event._
import shipreq.webapp.base.hash._
import shipreq.webapp.base.util.GenericDataMacros._
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecData._
import AtomPicklers.instances._
import ApplyEvent.LogicVer

object BinCodecEvents {

  implicit val pickleDeletionAction     = pickleEnum(DeletionAction.values)

  implicit val pickleCustomIssueTypeGD  = binpickler(CustomIssueTypeGD).nev
  implicit val pickleCustomReqTypeGD    = binpickler(CustomReqTypeGD).nev
  implicit val pickleTagGroupGD         = binpickler(TagGroupGD).nev
  implicit val pickleApplicableTagGD    = binpickler(ApplicableTagGD).nev
  implicit val pickleCustomTextFieldGD  = binpickler(CustomTextFieldGD).nev
  implicit val pickleCustomTagFieldGD   = binpickler(CustomTagFieldGD).nev
  implicit val pickleCustomImpFieldGD   = binpickler(CustomImpFieldGD).nev
  implicit val pickleCreateGenericReqGD = binpickler(CreateGenericReqGD).values
  implicit val pickleReqCodeGroupGD     = binpickler(ReqCodeGroupGD).nev

  implicit val pickleProjectTemplate: Pickler[ProjectTemplate] = pickleEnum(ProjectTemplate.values)

  implicit val pickleAddStaticField       : Pickler[AddStaticField]        = pickleCaseClass
  implicit val pickleApplyTemplate        : Pickler[ApplyTemplate]         = pickleCaseClass
  implicit val pickleCreateApplicableTag  : Pickler[CreateApplicableTag]   = pickleCaseClass
  implicit val pickleCreateCustomImpField : Pickler[CreateCustomImpField]  = pickleCaseClass
  implicit val pickleCreateCustomIssueType: Pickler[CreateCustomIssueType] = pickleCaseClass
  implicit val pickleCreateCustomReqType  : Pickler[CreateCustomReqType]   = pickleCaseClass
  implicit val pickleCreateCustomTagField : Pickler[CreateCustomTagField]  = pickleCaseClass
  implicit val pickleCreateCustomTextField: Pickler[CreateCustomTextField] = pickleCaseClass
  implicit val pickleCreateGenericReq     : Pickler[CreateGenericReq]      = pickleCaseClass
  implicit val pickleCreateReqCodeGroup   : Pickler[CreateReqCodeGroup]    = pickleCaseClass
  implicit val pickleCreateTagGroup       : Pickler[CreateTagGroup]        = pickleCaseClass
  implicit val pickleDeleteCustomField    : Pickler[DeleteCustomField]     = pickleCaseClass
  implicit val pickleDeleteCustomIssueType: Pickler[DeleteCustomIssueType] = pickleCaseClass
  implicit val pickleDeleteCustomReqType  : Pickler[DeleteCustomReqType]   = pickleCaseClass
  implicit val pickleDeleteReqCodeGroups  : Pickler[DeleteReqCodeGroups]   = pickleCaseClass
  implicit val pickleDeleteReqs           : Pickler[DeleteReqs]            = pickleCaseClass
  implicit val pickleDeleteStaticField    : Pickler[DeleteStaticField]     = pickleCaseClass
  implicit val pickleDeleteTag            : Pickler[DeleteTag]             = pickleCaseClass
  implicit val picklePatchImplicationSrc  : Pickler[PatchImplicationSrc]   = pickleCaseClass
  implicit val picklePatchImplicationTgt  : Pickler[PatchImplicationTgt]   = pickleCaseClass
  implicit val picklePatchReqCodes        : Pickler[PatchReqCodes]         = pickleCaseClass
  implicit val picklePatchReqTags         : Pickler[PatchReqTags]          = pickleCaseClass
  implicit val pickleRepositionField      : Pickler[RepositionField]       = pickleCaseClass
  implicit val pickleRestoreContent       : Pickler[RestoreContent]        = pickleCaseClass
  implicit val pickleSetCustomTextField   : Pickler[SetCustomTextField]    = pickleCaseClass
  implicit val pickleSetGenericReqTitle   : Pickler[SetGenericReqTitle]    = pickleCaseClass
  implicit val pickleSetGenericReqType    : Pickler[SetGenericReqType]     = pickleCaseClass
  implicit val pickleUpdateApplicableTag  : Pickler[UpdateApplicableTag]   = pickleCaseClass
  implicit val pickleUpdateCustomImpField : Pickler[UpdateCustomImpField]  = pickleCaseClass
  implicit val pickleUpdateCustomIssueType: Pickler[UpdateCustomIssueType] = pickleCaseClass
  implicit val pickleUpdateCustomReqType  : Pickler[UpdateCustomReqType]   = pickleCaseClass
  implicit val pickleUpdateCustomTagField : Pickler[UpdateCustomTagField]  = pickleCaseClass
  implicit val pickleUpdateCustomTextField: Pickler[UpdateCustomTextField] = pickleCaseClass
  implicit val pickleUpdateReqCodeGroup   : Pickler[UpdateReqCodeGroup]    = pickleCaseClass
  implicit val pickleUpdateTagGroup       : Pickler[UpdateTagGroup]        = pickleCaseClass

  implicit val pickleActiveEvent: Pickler[ActiveEvent] = pickleADT
  implicit val pickleEvent      : Pickler[Event]       = pickleADT

  implicit val pickleHashScheme    : Pickler[HashScheme]          = pickleEnum(HashScheme.all)
  implicit val pickleHashScope     : Pickler[HashScope]           = pickleEnum(HashScope.all)
  implicit val pickleLogicVer      : Pickler[ApplyEvent.LogicVer] = ConstPickler(ApplyEvent.LogicVer.Current)

  implicit val pickleHashRec: Pickler[HashRec] =
    new Pickler[HashRec] {
      val oi = optionPickler[Int]
      override def pickle(x: HashRec)(implicit s: PickleState): Unit = {
        s.pickle(x.scope)
        s.pickle(x.logicVer)
        s.pickle(x.scheme)
        s.pickle(x.hash)(oi)
      }

      override def unpickle(implicit s: UnpickleState) = HashRec(
        s.unpickle[HashScope],
        s.unpickle[LogicVer],
        s.unpickle[HashScheme])(
        s.unpickle(oi))
    }

  implicit val pickleVerifiedEvent : Pickler[VerifiedEvent]  = pickleCaseClass
  implicit val pickleVerifiedEvents: Pickler[VerifiedEvents] = iterablePickler
}
