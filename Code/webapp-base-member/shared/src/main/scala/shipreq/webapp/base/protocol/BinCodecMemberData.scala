package shipreq.webapp.base.protocol

import boopickle._
import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import japgolly.univeq.UnivEq
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.{Filter, FilterAst, IntensionalReqSet}
import shipreq.webapp.base.filter.Filter.Implicits._
import shipreq.webapp.base.sort.SortMethod
import shipreq.webapp.base.text.{AtomTC, ProjectText}
import DataImplicits._
import BinCodecGeneric._
import BinCodecBaseData._
import BoopickleMacros._

object BinCodecMemberData {

  def pickleIMapD[K: UnivEq : Pickler, V: Pickler](implicit d: DataIdAux[V, K]): Pickler[IMap[K, V]] =
    pickleIMap(d.emptyIMap)

  implicit lazy val pickleLive         : Pickler[Live               ] = pickleBool(Live)
  implicit lazy val pickleImplRequired : Pickler[ImplicationRequired] = pickleBool(ImplicationRequired)
  implicit lazy val pickleMandatory    : Pickler[Mandatory          ] = pickleBool(Mandatory)
  implicit lazy val pickleDeletable    : Pickler[Deletable          ] = pickleBool(Deletable)
  implicit lazy val pickleMutexChildren: Pickler[MutexChildren      ] = pickleBool(MutexChildren)
  implicit lazy val pickleFilterDead   : Pickler[FilterDead         ] = pickleBool(ShowDead)

  implicit lazy val pickleUseCaseId                = pickleTaggedI(UseCaseId                 ).reuseByUnivEq
  implicit lazy val pickleUseCaseStepId            = pickleTaggedI(UseCaseStepId             ).reuseByUnivEq
  implicit lazy val pickleGenericReqId             = pickleTaggedI(GenericReqId              ).reuseByUnivEq
  implicit lazy val pickleApReqCodeId              = pickleTaggedI(ApReqCodeId.apply         ).reuseByUnivEq
  implicit lazy val pickleReqCodeGroupId           = pickleTaggedI(ReqCodeGroupId            ).reuseByUnivEq
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

  implicit lazy val pickleProjectMetaData: Pickler[ProjectMetaData] = pickleCaseClass

  implicit lazy val pickleReqId        : Pickler[ReqId        ] = pickleADT
  implicit lazy val pickleSubReqId     : Pickler[SubReqId     ] = pickleADT
  implicit lazy val pickleReqOrSubReqId: Pickler[ReqOrSubReqId] = pickleADT
  implicit lazy val pickleReqCodeId    : Pickler[ReqCodeId    ] = pickleADT

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
    override def reqRef        [T <: ContentRef     ](t: T): Pickler[t.ReqRef        ] = pickleCaseClass
    override def codeRef       [T <: ContentRef     ](t: T): Pickler[t.CodeRef       ] = pickleCaseClass
    override def useCaseStepRef[T <: ContentRef     ](t: T): Pickler[t.UseCaseStepRef] = pickleCaseClass
    override def tagRef        [T <: TagRef         ](t: T): Pickler[t.TagRef        ] = pickleCaseClass

    override def issue[T <: Issue](t: T)(implicit h: Pickler[Text.InlineIssueDesc.OptionalText]): Pickler[t.Issue] =
      pickleCaseClass

    override def unorderedList[T <: ListMarkup](t: T)(implicit h: Pickler[NonEmptyVector[t.ListItem]]): Pickler[t.UnorderedList] =
      pickleCaseClass
  }

  import AtomPicklers.instances._

  implicit lazy val pickleProjectTextContextReq : Pickler[ProjectText.Context.Req ] = pickleCaseClass
  implicit lazy val pickleProjectTextContextNone: Pickler[ProjectText.Context.None] = pickleCaseClass
  implicit lazy val pickleProjectTextContext    : Pickler[ProjectText.Context     ] = pickleADT

  implicit lazy val pickleReqDataText        : Pickler[ReqData.Text        ] = pickleMap
  implicit lazy val pickleReqCodeNode        : Pickler[ReqCode.Node        ] = pickleCaseClass // xmap[String] already reuses
  implicit lazy val pickleLiveCodeGroup      : Pickler[LiveCodeGroup       ] = pickleCaseClass
  implicit lazy val pickleDeadCodeGroup      : Pickler[DeadCodeGroup       ] = pickleCaseClass
  implicit lazy val pickleCodeGroup          : Pickler[CodeGroup           ] = pickleADT
  implicit lazy val pickleReqCodeData        : Pickler[ReqCode.Data        ] = derivePickler
  implicit lazy val pickleApReqCodeIdAndValue: Pickler[ApReqCodeId.AndValue] = pickleCaseClass
  implicit lazy val pickleReqCodeTrie        : Pickler[ReqCode.Trie        ] = pickleTrie
  implicit lazy val pickleReqCodes           : Pickler[ReqCodes            ] = pickleCaseClass

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

  implicit lazy val pickleTagId          : Pickler[TagId              ] = pickleADT
  implicit lazy val pickleApplicableTag  : Pickler[ApplicableTag      ] = pickleCaseClass
  implicit lazy val pickleTagGroup       : Pickler[TagGroup           ] = pickleCaseClass
  implicit lazy val pickleTag            : Pickler[Tag                ] = pickleADT
  implicit lazy val pickleTagInTree      : Pickler[TagInTree          ] = pickleCaseClass
  implicit lazy val pickleTagTree        : Pickler[TagTree            ] = pickleIMap(TagTree.empty)
  implicit lazy val pickleTags           : Pickler[Tags               ] = pickleCaseClass
  implicit lazy val pickleTagPovRelations: Pickler[TagInTree.Relations] = pickleCaseClass

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

  object ReqTableDataPicklers {
    import reqtable._

    implicit val pickleColumnCode          : Pickler[Column.Code          .type] = pickleObject
    implicit val pickleColumnCustomField   : Pickler[Column.CustomField        ] = pickleCaseClass
    implicit val pickleColumnDeletionReason: Pickler[Column.DeletionReason.type] = pickleObject
    implicit val pickleColumnImplications  : Pickler[Column.Implications       ] = pickleCaseClass
    implicit val pickleColumnPubid         : Pickler[Column.Pubid         .type] = pickleObject
    implicit val pickleColumnReqType       : Pickler[Column.ReqType       .type] = pickleObject
    implicit val pickleColumnTags          : Pickler[Column.Tags          .type] = pickleObject
    implicit val pickleColumnTitle         : Pickler[Column.Title         .type] = pickleObject

    implicit val pickleColumnIB : Pickler[Column.SortInconclusiveHasBlanks] = pickleADT
    implicit val pickleColumnIN : Pickler[Column.SortInconclusiveNoBlanks ] = pickleADT
    implicit val pickleColumnSI : Pickler[Column.SortInconclusive         ] = pickleADT
    implicit val pickleColumnSC : Pickler[Column.SortConclusive           ] = pickleADT
    implicit val pickleColumnSIs: Pickler[Vector[Column.SortInconclusive] ] = iterablePickler
    implicit val pickleColumn   : Pickler[Column                          ] = pickleADT
    implicit val pickleColumnNEV: Pickler[NonEmptyVector[Column]          ] = pickleNEV

    implicit val pickleSortMethodAsc           : Pickler[SortMethod.Asc           .type] = pickleObject
    implicit val pickleSortMethodAscThenBlanks : Pickler[SortMethod.AscThenBlanks .type] = pickleObject
    implicit val pickleSortMethodBlanksThenAsc : Pickler[SortMethod.BlanksThenAsc .type] = pickleObject
    implicit val pickleSortMethodBlanksThenDesc: Pickler[SortMethod.BlanksThenDesc.type] = pickleObject
    implicit val pickleSortMethodDesc          : Pickler[SortMethod.Desc          .type] = pickleObject
    implicit val pickleSortMethodDescThenBlanks: Pickler[SortMethod.DescThenBlanks.type] = pickleObject
    implicit val pickleSortMethodIB            : Pickler[SortMethod.IgnoreBlanks       ] = pickleADT
    implicit val pickleSortMethodCB            : Pickler[SortMethod.ConsiderBlanks     ] = pickleADT
    implicit val pickleSortMethod              : Pickler[SortMethod                    ] = pickleADT

    implicit val pickleSortCriterionICB: Pickler[SortCriterion.InconclusiveCB      ] = pickleCaseClass
    implicit val pickleSortCriterionIIB: Pickler[SortCriterion.InconclusiveIB      ] = pickleCaseClass
    implicit val pickleSortCriterionC  : Pickler[SortCriterion.Conclusive          ] = pickleCaseClass
    implicit val pickleSortCriterionI  : Pickler[SortCriterion.Inconclusive        ] = pickleADT
    implicit val pickleSortCriterionIs : Pickler[Vector[SortCriterion.Inconclusive]] = iterablePickler
    implicit val pickleSortCriterion   : Pickler[SortCriterion                     ] = pickleADT
    implicit val pickleSortCriteria    : Pickler[SortCriteria                      ] = pickleCaseClass

    implicit val pickleView         : Pickler[View                 ] = pickleCaseClass
    implicit val pickleSavedViewId  : Pickler[SavedView.Id         ] = pickleCaseClass
    implicit val pickleSavedViewName: Pickler[SavedView.Name       ] = pickleCaseClass
    implicit val pickleSavedView    : Pickler[SavedView            ] = pickleCaseClass
    implicit val pickleSavedViewsND : Pickler[SavedViews.NonDefault] = pickleIMap(SavedViews.emptyNonDefault)
    implicit val pickleSavedViews   : Pickler[SavedViews.NonEmpty  ] = pickleCaseClass
  }
  import ReqTableDataPicklers.pickleSavedViews

  implicit val pickleValidFilter: Pickler[Filter.Valid] = {
    import Filter._
    implicit val pickleNonEmptyVectorUnit : Pickler[NonEmptyVector[Unit]                   ] = implicitly[Pickler[Int]].xmap(NonEmptyVector force Vector.fill(_)(()))(_.length)
    implicit val pickleNonEmptySetInt     : Pickler[NonEmptySet[Int]                       ] = pickleNES
    implicit def pickleIRSetS [A: Pickler]: Pickler[IntensionalReqSet.SomeOfType[A]        ] = pickleCaseClass
    implicit def pickleIRSetW [A: Pickler]: Pickler[IntensionalReqSet.WholeType [A]        ] = pickleCaseClass
    implicit def pickleIRSet  [A: Pickler]: Pickler[IntensionalReqSet           [A]        ] = pickleADT
    implicit val pickleValidReqSubset     : Pickler[Valid.ReqSubset                        ] = pickleADT
    implicit val pickleValidReqSet        : Pickler[Valid.ReqSet                           ] = pickleNEV
    implicit val pickleValidAttr          : Pickler[FilterAst.Attr                         ] = derivePickler
    implicit val pickleValidText          : Pickler[FilterAst.Text                         ] = pickleCaseClass
    implicit val pickleValidRegex         : Pickler[FilterAst.Regex                        ] = pickleCaseClass
    implicit val pickleValidPresence      : Pickler[FilterAst.Presence      [Valid.Attr]   ] = pickleCaseClass
    implicit val pickleValidLack          : Pickler[FilterAst.Lack          [Valid.Attr]   ] = pickleCaseClass
    implicit val pickleValidReqs          : Pickler[FilterAst.Reqs          [Valid.ReqSet] ] = pickleCaseClass
    implicit val pickleValidReqType       : Pickler[FilterAst.ReqType       [Valid.ReqType]] = pickleCaseClass
    implicit val pickleValidHashRef       : Pickler[FilterAst.HashRef       [Valid.HashTag]] = pickleCaseClass
    implicit val pickleValidImpliesAnyOf  : Pickler[FilterAst.ImpliesAnyOf  [Valid.ReqSet] ] = pickleCaseClass
    implicit val pickleValidImpliedByAnyOf: Pickler[FilterAst.ImpliedByAnyOf[Valid.ReqSet] ] = pickleCaseClass
    implicit val pickleValidAllOf         : Pickler[FilterAst.AllOf         [Unit]         ] = pickleCaseClass
    implicit val pickleValidAnyOf         : Pickler[FilterAst.AnyOf         [Unit]         ] = pickleCaseClass
    implicit val pickleValidNot           : Pickler[FilterAst.Not           [Unit]         ] = pickleCaseClass
    implicit val pickleValidF             : Pickler[ValidF                  [Unit]         ] = pickleADT
    pickleFix[ValidF]
  }

  implicit lazy val pickleIdCeilings    : Pickler[IdCeilings    ] = pickleCaseClass
  implicit lazy val pickleProjectConfig : Pickler[ProjectConfig ] = pickleCaseClass
  implicit lazy val pickleProjectContent: Pickler[ProjectContent] = pickleCaseClass
  implicit lazy val pickleProject       : Pickler[Project       ] = pickleCaseClass
}
