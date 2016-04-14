package shipreq.webapp.base.protocol

import boopickle._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.AtomTC
import DataImplicits._
import BinCodecGeneric._
import BoopickleMacros._

object BinCodecData {

  implicit val pickleVectorTreeLoc: Pickler[VectorTree.Location] = pickleNEV

  implicit val pickleVectorTreeParentLoc: Pickler[VectorTree.ParentLocation] =
    implicitly[Pickler[Vector[Int]]].imap(VectorTree.ParentLocation.isoVector)

  implicit val pickleLive         : Pickler[Live               ] = pickleBool(Live)
  implicit val pickleDirection    : Pickler[Direction          ] = pickleBool(Forwards)
  implicit val pickleImplRequired : Pickler[ImplicationRequired] = pickleBool(ImplicationRequired)
  implicit val pickleMandatory    : Pickler[Mandatory          ] = pickleBool(Mandatory)
  implicit val pickleDeletable    : Pickler[Deletable          ] = pickleBool(Deletable)
  implicit val pickleMutexChildren: Pickler[MutexChildren      ] = pickleBool(MutexChildren)

  implicit val pickleUseCaseId                = pickleTaggedI(UseCaseId                 ).reuseByUnivEq
  implicit val pickleUseCaseStepId            = pickleTaggedI(UseCaseStepId             ).reuseByUnivEq
  implicit val pickleGenericReqId             = pickleTaggedI(GenericReqId              ).reuseByUnivEq
  implicit val pickleReqCodeId                = pickleTaggedI(ReqCodeId                 ).reuseByUnivEq
  implicit val pickleCustomReqTypeId          = pickleTaggedI(CustomReqTypeId           ).reuseByUnivEq
  implicit val pickleCustomIssueTypeId        = pickleTaggedI(CustomIssueTypeId         ).reuseByUnivEq
  implicit val pickleApplicableTagId          = pickleTaggedI(ApplicableTagId           ).reuseByUnivEq
  implicit val pickleTagGroupId               = pickleTaggedI(TagGroupId                ).reuseByUnivEq
  implicit val pickleCustomFieldTagId         = pickleTaggedI(CustomField.Tag.Id        ).reuseByUnivEq
  implicit val pickleCustomFieldTextId        = pickleTaggedI(CustomField.Text.Id       ).reuseByUnivEq
  implicit val pickleCustomFieldImplicationId = pickleTaggedI(CustomField.Implication.Id).reuseByUnivEq
  implicit val pickleReqTypePos               = pickleTaggedI(ReqTypePos                )
  implicit val pickleHashRefKey               = pickleTaggedS(HashRefKey                )
  implicit val pickleFieldRefKey              = pickleTaggedS(FieldRefKey               )
  implicit val pickleReqTypeMnemonic          = pickleTaggedS(ReqType.Mnemonic          )

  implicit val pickleReqId        : Pickler[ReqId        ] = pickleADT
  implicit val pickleSubReqId     : Pickler[SubReqId     ] = pickleADT
  implicit val pickleReqOrSubReqId: Pickler[ReqOrSubReqId] = pickleADT

  implicit val pickleImplications: Pickler[Implications] = pickleCaseClass

  object AtomPicklers extends AtomTC[Pickler] {
    import shipreq.webapp.base.text._
    import Atom._
    import Text.Equality._

    override def lazily[A](f: => Pickler[A]): Pickler[A] = pickleLazily(f)

    override def vec[A](implicit a: Pickler[A]) = implicitly

    override def nev[A](as: Pickler[Vector[A]])(implicit a: Pickler[A]) = implicitly

    override def sum[T <: Atom.Base](t: T)(f: t.Atom => Pickler[t.Atom], index: t.Atom => Int, all: Vector[Pickler[t.Atom]]): Pickler[t.Atom] =
      new Pickler[t.Atom] {
        override def pickle(a: t.Atom)(implicit state: PickleState): Unit = {
          val i = index(a)
          state.enc.writeInt(i)
          all(i).pickle(a)
        }
        override def unpickle(implicit state: UnpickleState): t.Atom = {
          val i = state.dec.readInt
          all(i).unpickle
        }
      }

    override def blankLine     [T <: NewLine        ](t: T): Pickler[t.BlankLine     ] = ConstPickler(t.blankLine)
    override def literal       [T <: Literal        ](t: T): Pickler[t.Literal       ] = pickleCaseClass
    override def webAddress    [T <: PlainTextMarkup](t: T): Pickler[t.WebAddress    ] = pickleCaseClass
    override def emailAddress  [T <: PlainTextMarkup](t: T): Pickler[t.EmailAddress  ] = pickleCaseClass
    override def mathTeX       [T <: PlainTextMarkup](t: T): Pickler[t.MathTeX       ] = pickleCaseClass
    override def reqRef        [T <: ReqRef         ](t: T): Pickler[t.ReqRef        ] = pickleCaseClass
    override def codeRef       [T <: ReqRef         ](t: T): Pickler[t.CodeRef       ] = pickleCaseClass
    override def tagRef        [T <: TagRef         ](t: T): Pickler[t.TagRef        ] = pickleCaseClass
    override def useCaseStepRef[T <: UseCaseStepRef ](t: T): Pickler[t.UseCaseStepRef] = pickleCaseClass

    override def issue[T <: Issue](t: T)(implicit h: Pickler[Text.InlineIssueDesc.OptionalText]): Pickler[t.Issue] =
      pickleCaseClass

    override def unorderedList[T <: ListMarkup](t: T)(implicit h: Pickler[NonEmptyVector[t.ListItem]]): Pickler[t.UnorderedList] =
      pickleCaseClass
  }

  import AtomPicklers.instances._

  implicit val pickleReqDataText       : Pickler[ReqData.Text       ] = pickleMap
  implicit val pickleReqCodeNode       : Pickler[ReqCode.Node       ] = pickleCaseClass // xmap[String] already reuses
  implicit val pickleLiveReqCodeGroup  : Pickler[LiveReqCodeGroup   ] = pickleCaseClass
  implicit val pickleDeadReqCodeGroup  : Pickler[DeadReqCodeGroup   ] = pickleCaseClass
  implicit val pickleReqCodeGroup      : Pickler[ReqCodeGroup       ] = pickleADT
  implicit val pickleReqCodeInactive   : Pickler[ReqCode.Inactive   ] = pickleCaseClass
  implicit val pickleReqCodeActiveGroup: Pickler[ReqCode.ActiveGroup] = pickleCaseClass
  implicit val pickleReqCodeActiveReq  : Pickler[ReqCode.ActiveReq  ] = pickleCaseClass
  implicit val pickleReqCodeData       : Pickler[ReqCode.Data       ] = pickleADT
  implicit val pickleReqCodeIdAndValue : Pickler[ReqCode.IdAndValue ] = pickleCaseClass
  implicit val pickleReqCodeTrie       : Pickler[ReqCode.Trie       ] = pickleTrie
  implicit val pickleReqCodes          : Pickler[ReqCodes           ] = pickleCaseClass

  implicit val pickleStaticReqTypeUC: Pickler[StaticReqType.UseCase.type] = pickleObject
  implicit val pickleStaticReqType  : Pickler[StaticReqType             ] = pickleADT
  implicit val pickleReqTypeId      : Pickler[ReqTypeId                 ] = pickleADT

  implicit val picklePubidRegister         : Pickler[PubidRegister    ] = pickleCaseClass
  implicit val picklePubid                 : Pickler[Pubid            ] = pickleCaseClass
  implicit def picklePubidT[T <: ReqTypeId]: Pickler[PubidT[T]        ] = picklePubid.asInstanceOf[Pickler[PubidT[T]]]
  implicit val pickleGenericReq            : Pickler[GenericReq       ] = pickleCaseClass
  implicit val pickleUseCaseStep           : Pickler[UseCaseStep      ] = pickleCaseClass
  implicit val pickleUseCaseSteps          : Pickler[UseCaseSteps     ] = pickleCaseClass
  implicit val pickleUseCase               : Pickler[UseCase          ] = pickleCaseClass
  implicit val pickleReq                   : Pickler[Req              ] = pickleADT
  implicit val pickleGenericReqsById       : Pickler[GenericReqIMap   ] = pickleIMapD
  implicit val pickleUseCasesById          : Pickler[UseCaseIMap      ] = pickleIMapD
  implicit val pickleUseCasesStepFlow      : Pickler[UseCases.StepFlow] = pickleCaseClass
  implicit val pickleUseCases              : Pickler[UseCases         ] = pickleCaseClass[UseCases.Stateless] imap UseCases.statelessIso
  implicit val pickleRequirements          : Pickler[Requirements     ] = pickleCaseClass

  implicit val pickleCustomIssueType : Pickler[CustomIssueType    ] = pickleCaseClass
  implicit val pickleCustomIssueTypes: Pickler[CustomIssueTypeIMap] = pickleIMapD
  implicit val pickleCustomReqType   : Pickler[CustomReqType      ] = pickleCaseClass
  implicit val pickleCustomReqTypes  : Pickler[CustomReqTypeIMap  ] = pickleIMapD

  implicit val pickleTagId        : Pickler[TagId        ] = pickleADT
  implicit val pickleApplicableTag: Pickler[ApplicableTag] = pickleCaseClass
  implicit val pickleTagGroup     : Pickler[TagGroup     ] = pickleCaseClass
  implicit val pickleTag          : Pickler[Tag          ] = pickleADT
  implicit val pickleTagInTree    : Pickler[TagInTree    ] = pickleCaseClass
  implicit val pickleTagTree      : Pickler[TagTree      ] = pickleIMap(TagTree.empty)

  implicit val pickleApplReqTypes     : Pickler[Field.ApplicableReqTypes          ] = pickleISubset
  implicit val pickleCustomFieldTypeIM: Pickler[CustomFieldType.Implication.type  ] = pickleObject
  implicit val pickleCustomFieldTypeTA: Pickler[CustomFieldType.Tag.type          ] = pickleObject
  implicit val pickleCustomFieldTypeTX: Pickler[CustomFieldType.Text.type         ] = pickleObject
  implicit val pickleStaticFieldTypeSG: Pickler[StaticFieldType.StepGraph.type    ] = pickleObject
  implicit val pickleStaticFieldTypeST: Pickler[StaticFieldType.StepTree.type     ] = pickleObject
  implicit val pickleCustomFieldType  : Pickler[CustomFieldType                   ] = pickleADT
  implicit val pickleStaticFieldType  : Pickler[StaticFieldType                   ] = pickleADT
  implicit val pickleFieldType        : Pickler[FieldType                         ] = pickleADT
  implicit val pickleCustomFieldIM    : Pickler[CustomField.Implication           ] = pickleCaseClass
  implicit val pickleCustomFieldTA    : Pickler[CustomField.Tag                   ] = pickleCaseClass
  implicit val pickleCustomFieldTX    : Pickler[CustomField.Text                  ] = pickleCaseClass
  implicit val pickleStaticFieldSG    : Pickler[StaticField.StepGraph.type        ] = pickleObject
  implicit val pickleStaticFieldNS    : Pickler[StaticField.NormalAltStepTree.type] = pickleObject
  implicit val pickleStaticFieldES    : Pickler[StaticField.ExceptionStepTree.type] = pickleObject
  implicit val pickleStaticFieldUCST  : Pickler[StaticField.UseCaseStepTree       ] = pickleADT
  implicit val pickleStaticField      : Pickler[StaticField                       ] = pickleADT
  implicit val pickleCustomFieldId    : Pickler[CustomFieldId                     ] = pickleADT
  implicit val pickleCustomField      : Pickler[CustomField                       ] = pickleADT
  implicit val pickleFieldId          : Pickler[FieldId                           ] = pickleADT
  implicit val pickleCustomFields     : Pickler[FieldSet.CustomFields             ] = pickleIMap(FieldSet.emptyCustomFields)
  implicit val pickleFieldSet         : Pickler[FieldSet                          ] = pickleCaseClass

  implicit val pickleDeletionReasonIdO = optionPickler(pickleTaggedI(DeletionReasonId)).reuseByUnivEq
  implicit val pickleDeletionReasons   = pickleCaseClass[DeletionReasons]

  implicit val pickleIdCeilings   : Pickler[IdCeilings   ] = pickleCaseClass
  implicit val pickleProjectConfig: Pickler[ProjectConfig] = pickleCaseClass
  implicit val pickleProject      : Pickler[Project      ] = pickleCaseClass
}
