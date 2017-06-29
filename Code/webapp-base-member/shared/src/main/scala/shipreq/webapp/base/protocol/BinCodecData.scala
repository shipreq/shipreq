package shipreq.webapp.base.protocol

import boopickle._
import japgolly.microlibs.nonempty.{NonEmpty, NonEmptyVector}
import japgolly.univeq.UnivEq
import java.time.Instant
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.user._
import shipreq.webapp.base.text.AtomTC
import DataImplicits._
import BinCodecGeneric._
import BoopickleMacros._

object BinCodecData {

  def pickleIMapD[K: UnivEq : Pickler, V: Pickler](implicit d: DataIdAux[V, K]): Pickler[IMap[K, V]] =
    pickleIMap(d.emptyIMap)

  implicit lazy val pickleInstant: Pickler[Instant] =
    xmap(Instant.ofEpochMilli)(_.toEpochMilli)

  implicit lazy val pickleVectorTreeLoc: Pickler[VectorTree.Location] = pickleNEV

  implicit lazy val pickleVectorTreeParentLoc: Pickler[VectorTree.ParentLocation] =
    implicitly[Pickler[Vector[Int]]].imap(VectorTree.ParentLocation.isoVector)

  implicit lazy val pickleLive         : Pickler[Live               ] = pickleBool(Live)
  implicit lazy val pickleDirection    : Pickler[Direction          ] = pickleBool(Forwards)
  implicit lazy val pickleImplRequired : Pickler[ImplicationRequired] = pickleBool(ImplicationRequired)
  implicit lazy val pickleMandatory    : Pickler[Mandatory          ] = pickleBool(Mandatory)
  implicit lazy val pickleDeletable    : Pickler[Deletable          ] = pickleBool(Deletable)
  implicit lazy val pickleMutexChildren: Pickler[MutexChildren      ] = pickleBool(MutexChildren)
  implicit lazy val pickleFilterDead   : Pickler[FilterDead         ] = pickleBool(ShowDead)

  implicit lazy val pickleUseCaseId                = pickleTaggedI(UseCaseId                 ).reuseByUnivEq
  implicit lazy val pickleUseCaseStepId            = pickleTaggedI(UseCaseStepId             ).reuseByUnivEq
  implicit lazy val pickleGenericReqId             = pickleTaggedI(GenericReqId              ).reuseByUnivEq
  implicit lazy val pickleReqCodeId                = pickleTaggedI(ReqCodeId                 ).reuseByUnivEq
  implicit lazy val pickleCustomReqTypeId          = pickleTaggedI(CustomReqTypeId           ).reuseByUnivEq
  implicit lazy val pickleCustomIssueTypeId        = pickleTaggedI(CustomIssueTypeId         ).reuseByUnivEq
  implicit lazy val pickleApplicableTagId          = pickleTaggedI(ApplicableTagId           ).reuseByUnivEq
  implicit lazy val pickleTagGroupId               = pickleTaggedI(TagGroupId                ).reuseByUnivEq
  implicit lazy val pickleCustomFieldTagId         = pickleTaggedI(CustomField.Tag.Id        ).reuseByUnivEq
  implicit lazy val pickleCustomFieldTextId        = pickleTaggedI(CustomField.Text.Id       ).reuseByUnivEq
  implicit lazy val pickleCustomFieldImplicationId = pickleTaggedI(CustomField.Implication.Id).reuseByUnivEq
  implicit lazy val pickleReqTypePos               = pickleTaggedI(ReqTypePos                )
  implicit lazy val pickleHashRefKey               = pickleTaggedS(HashRefKey                )
  implicit lazy val pickleFieldRefKey              = pickleTaggedS(FieldRefKey               )
  implicit lazy val pickleReqTypeMnemonic          = pickleTaggedS(ReqType.Mnemonic          )

  implicit lazy val pickleUsername: Pickler[Username] = pickleCaseClass

  implicit def pickleExternalId[T]: Pickler[ExternalId[T]] = pickleCaseClass

  implicit lazy val pickleProjectMetaData: Pickler[ProjectMetaData] = pickleCaseClass

  implicit lazy val pickleReqId        : Pickler[ReqId        ] = pickleADT
  implicit lazy val pickleSubReqId     : Pickler[SubReqId     ] = pickleADT
  implicit lazy val pickleReqOrSubReqId: Pickler[ReqOrSubReqId] = pickleADT

  implicit lazy val pickleImplications: Pickler[Implications] = pickleCaseClass

  implicit lazy val pickleReqIdsByDirection: Pickler[Direction.Values[Set[ReqId]]] = pickleIsoBoolValues

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

  implicit lazy val pickleReqDataText       : Pickler[ReqData.Text       ] = pickleMap
  implicit lazy val pickleReqCodeNode       : Pickler[ReqCode.Node       ] = pickleCaseClass // xmap[String] already reuses
  implicit lazy val pickleLiveCodeGroup     : Pickler[LiveCodeGroup      ] = pickleCaseClass
  implicit lazy val pickleDeadCodeGroup     : Pickler[DeadCodeGroup      ] = pickleCaseClass
  implicit lazy val pickleCodeGroup         : Pickler[CodeGroup          ] = pickleADT
  implicit lazy val pickleReqCodeInactive   : Pickler[ReqCode.Inactive   ] = pickleCaseClass
  implicit lazy val pickleReqCodeActiveGroup: Pickler[ReqCode.ActiveGroup] = pickleCaseClass
  implicit lazy val pickleReqCodeActiveReq  : Pickler[ReqCode.ActiveReq  ] = pickleCaseClass
  implicit lazy val pickleReqCodeData       : Pickler[ReqCode.Data       ] = pickleADT
  implicit lazy val pickleReqCodeIdAndValue : Pickler[ReqCode.IdAndValue ] = pickleCaseClass
  implicit lazy val pickleReqCodeTrie       : Pickler[ReqCode.Trie       ] = pickleTrie
  implicit lazy val pickleReqCodes          : Pickler[ReqCodes           ] = pickleCaseClass

  implicit lazy val pickleStaticReqTypeUC: Pickler[StaticReqType.UseCase.type] = pickleObject
  implicit lazy val pickleStaticReqType  : Pickler[StaticReqType             ] = pickleADT
  implicit lazy val pickleReqTypeId      : Pickler[ReqTypeId                 ] = pickleADT

  implicit lazy val picklePubidRegister         : Pickler[PubidRegister    ] = pickleCaseClass
  implicit lazy val picklePubid                 : Pickler[Pubid            ] = pickleCaseClass
  implicit      def picklePubidT[T <: ReqTypeId]: Pickler[PubidT[T]        ] = picklePubid.asInstanceOf[Pickler[PubidT[T]]]
  implicit lazy val pickleGenericReq            : Pickler[GenericReq       ] = pickleCaseClass
  implicit lazy val pickleUseCaseStep           : Pickler[UseCaseStep      ] = pickleCaseClass
  implicit lazy val pickleUseCaseSteps          : Pickler[UseCaseSteps     ] = pickleCaseClass
  implicit lazy val pickleUseCase               : Pickler[UseCase          ] = pickleCaseClass
  implicit lazy val pickleReq                   : Pickler[Req              ] = pickleADT
  implicit lazy val pickleGenericReqsById       : Pickler[GenericReqIMap   ] = pickleIMapD
  implicit lazy val pickleUseCasesById          : Pickler[UseCaseIMap      ] = pickleIMapD
  implicit lazy val pickleUseCasesStepFlow      : Pickler[UseCases.StepFlow] = pickleCaseClass
  implicit lazy val pickleUseCases              : Pickler[UseCases         ] = pickleCaseClass[UseCases.Stateless] imap UseCases.statelessIso
  implicit lazy val pickleRequirements          : Pickler[Requirements     ] = pickleCaseClass

  implicit lazy val pickleCustomIssueType : Pickler[CustomIssueType    ] = pickleCaseClass
  implicit lazy val pickleCustomIssueTypes: Pickler[CustomIssueTypeIMap] = pickleIMapD
  implicit lazy val pickleCustomReqType   : Pickler[CustomReqType      ] = pickleCaseClass
  implicit lazy val pickleCustomReqTypes  : Pickler[ReqTypes.Custom    ] = pickleIMapD
  implicit lazy val pickleReqTypes        : Pickler[ReqTypes           ] = pickleCaseClass

  implicit lazy val pickleTagId        : Pickler[TagId        ] = pickleADT
  implicit lazy val pickleApplicableTag: Pickler[ApplicableTag] = pickleCaseClass
  implicit lazy val pickleTagGroup     : Pickler[TagGroup     ] = pickleCaseClass
  implicit lazy val pickleTag          : Pickler[Tag          ] = pickleADT
  implicit lazy val pickleTagInTree    : Pickler[TagInTree    ] = pickleCaseClass
  implicit lazy val pickleTagTree      : Pickler[TagTree      ] = pickleIMap(TagTree.empty)

  implicit lazy val pickleApplReqTypes     : Pickler[Field.ApplicableReqTypes             ] = pickleISubset
  implicit lazy val pickleCustomFieldTypeIM: Pickler[CustomFieldType.Implication.type     ] = pickleObject
  implicit lazy val pickleCustomFieldTypeTA: Pickler[CustomFieldType.Tag.type             ] = pickleObject
  implicit lazy val pickleCustomFieldTypeTX: Pickler[CustomFieldType.Text.type            ] = pickleObject
  implicit lazy val pickleStaticFieldTypeST: Pickler[StaticFieldType.StepTree.type        ] = pickleObject
  implicit lazy val pickleStaticFieldTypeSG: Pickler[StaticFieldType.StepGraph.type       ] = pickleObject
  implicit lazy val pickleStaticFieldTypeIG: Pickler[StaticFieldType.ImplicationGraph.type] = pickleObject
  implicit lazy val pickleCustomFieldType  : Pickler[CustomFieldType                      ] = pickleADT
  implicit lazy val pickleStaticFieldType  : Pickler[StaticFieldType                      ] = pickleADT
  implicit lazy val pickleFieldType        : Pickler[FieldType                            ] = pickleADT
  implicit lazy val pickleCustomFieldIM    : Pickler[CustomField.Implication              ] = pickleCaseClass
  implicit lazy val pickleCustomFieldTA    : Pickler[CustomField.Tag                      ] = pickleCaseClass
  implicit lazy val pickleCustomFieldTX    : Pickler[CustomField.Text                     ] = pickleCaseClass
  implicit lazy val pickleStaticFieldNS    : Pickler[StaticField.NormalAltStepTree.type   ] = pickleObject
  implicit lazy val pickleStaticFieldES    : Pickler[StaticField.ExceptionStepTree.type   ] = pickleObject
  implicit lazy val pickleStaticFieldUCST  : Pickler[StaticField.UseCaseStepTree          ] = pickleADT
  implicit lazy val pickleStaticFieldSG    : Pickler[StaticField.StepGraph.type           ] = pickleObject
  implicit lazy val pickleStaticFieldIG    : Pickler[StaticField.ImplicationGraph.type    ] = pickleObject
  implicit lazy val pickleStaticField      : Pickler[StaticField                          ] = pickleADT
  implicit lazy val pickleCustomFieldId    : Pickler[CustomFieldId                        ] = pickleADT
  implicit lazy val pickleCustomField      : Pickler[CustomField                          ] = pickleADT
  implicit lazy val pickleFieldId          : Pickler[FieldId                              ] = pickleADT
  implicit lazy val pickleCustomFields     : Pickler[FieldSet.CustomFields                ] = pickleIMap(FieldSet.emptyCustomFields)
  implicit lazy val pickleFieldSet         : Pickler[FieldSet                             ] = pickleCaseClass

  implicit lazy val pickleDeletionReasonIdO = optionPickler(pickleTaggedI(DeletionReasonId)).reuseByUnivEq
  implicit lazy val pickleDeletionReasons   = pickleCaseClass[DeletionReasons]

  implicit lazy val pickleIdCeilings   : Pickler[IdCeilings   ] = pickleCaseClass
  implicit lazy val pickleProjectConfig: Pickler[ProjectConfig] = pickleCaseClass
  implicit lazy val pickleProject      : Pickler[Project      ] = pickleCaseClass
}
