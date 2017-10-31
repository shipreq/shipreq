package shipreq.webapp.base.hash2

import japgolly.microlibs.nonempty._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.{Filter, FilterAst, IntensionalReqSet}
import shipreq.webapp.base.filter.Filter.Implicits._
import shipreq.webapp.base.text.AtomTC

object ProjectHasher {
  private val algorithm = MurmurHash3
  import algorithm._

  private val genericData = new GenericDashHasher(algorithm)
  import genericData._

  val hashProjectName: HashFn[Project.Name] = hashString

  protected implicit val hashLive         : HashFn[Live               ] = HashFn by Live.from
  protected implicit val hashImplRequired : HashFn[ImplicationRequired] = HashFn by ImplicationRequired.from
  protected implicit val hashMandatory    : HashFn[Mandatory          ] = HashFn by Mandatory.from
  protected implicit val hashDeletable    : HashFn[Deletable          ] = HashFn by Deletable.from
  protected implicit val hashDirection    : HashFn[Direction          ] = HashFn by Forwards.from
  protected implicit val hashFilterDead   : HashFn[FilterDead         ] = HashFn by ShowDead.from
  protected implicit val hashMutexChildren: HashFn[MutexChildren      ] = HashFn by MutexChildren.from

  protected implicit val hashUseCaseStepId            = hashTaggedType[UseCaseStepId]
  protected implicit val hashUseCaseId                = hashTaggedType[UseCaseId]
  protected implicit val hashDeletionReasonId         = hashTaggedType[DeletionReasonId]
  protected implicit val hashGenericReqId             = hashTaggedType[GenericReqId]
  protected implicit val hashReqCodeId                = hashTaggedType[ReqCodeId]
  protected implicit val hashCustomReqTypeId          = hashTaggedType[CustomReqTypeId]
  protected implicit val hashCustomIssueTypeId        = hashTaggedType[CustomIssueTypeId]
  protected implicit val hashApplicableTagId          = hashTaggedType[ApplicableTagId]
  protected implicit val hashTagGroupId               = hashTaggedType[TagGroupId]
  protected implicit val hashCustomFieldId            = hashTaggedType[CustomFieldId]
  protected implicit val hashCustomFieldTagId         = hashTaggedType[CustomField.Tag.Id]
  protected implicit val hashCustomFieldTextId        = hashTaggedType[CustomField.Text.Id]
  protected implicit val hashCustomFieldImplicationId = hashTaggedType[CustomField.Implication.Id]
  protected implicit val hashHashRefKey               = hashTaggedType[HashRefKey]
  protected implicit val hashReqTypePos               = hashTaggedType[ReqTypePos]
  protected implicit val hashFieldRefKey              = hashTaggedType[FieldRefKey]
  protected implicit val hashReqTypeMnemonic          = hashTaggedType[ReqType.Mnemonic]

  protected implicit val hashReqId: HashFn[ReqId] = HashFn.by(_.value)

  implicit val hashImplications: HashFn[Implications] = withName("Imp", hashCaseClass)

  implicit val hashReqDataTags: HashFn[ReqData.Tags] = withName("RDTags", hashMultimap)

  private object HashAtoms extends AtomTC[HashFn] {
    import shipreq.webapp.base.text._
    import Atom._

    override def lazily[A](a: => HashFn[A]): HashFn[A] = {
      lazy val b = a
      HashFn(a => b(a))
    }

    override def vec[A](implicit a: HashFn[A]) =
      hashVector(a)

    override def nev[A](as: HashFn[Vector[A]])(implicit a: HashFn[A]) =
      hashNEV(a)

    override def sum[T <: Atom.Base](t: T)(f: t.Atom => HashFn[t.Atom], i: t.Atom => Int, v: Vector[HashFn[t.Atom]]): HashFn[t.Atom] =
      HashFn[t.Atom](a => f(a)(a))

    override def blankLine     [T <: NewLine        ](t: T): HashFn[t.BlankLine     ] = hashConstClass("BL")
    override def literal       [T <: Literal        ](t: T): HashFn[t.Literal       ] = withName("LI", hashCaseClass)
    override def webAddress    [T <: PlainTextMarkup](t: T): HashFn[t.WebAddress    ] = withName("WA", hashCaseClass)
    override def emailAddress  [T <: PlainTextMarkup](t: T): HashFn[t.EmailAddress  ] = withName("EA", hashCaseClass)
    override def mathTeX       [T <: PlainTextMarkup](t: T): HashFn[t.MathTeX       ] = withName("MX", hashCaseClass)
    override def reqRef        [T <: ReqRef         ](t: T): HashFn[t.ReqRef        ] = withName("RR", hashCaseClass)
    override def codeRef       [T <: ReqRef         ](t: T): HashFn[t.CodeRef       ] = withName("CR", hashCaseClass)
    override def useCaseStepRef[T <: UseCaseStepRef ](t: T): HashFn[t.UseCaseStepRef] = withName("UR", hashCaseClass)
    override def tagRef        [T <: TagRef         ](t: T): HashFn[t.TagRef        ] = withName("TR", hashCaseClass)

    override def issue[T <: Issue](t: T)(implicit h: HashFn[Text.InlineIssueDesc.OptionalText]): HashFn[t.Issue] =
      withName("IS", hashCaseClass)

    override def unorderedList[T <: ListMarkup](t: T)(implicit h: HashFn[NonEmptyVector[t.ListItem]]): HashFn[t.UnorderedList] =
      withName("UL", hashCaseClass)
  }

  import HashAtoms.instances._

            implicit val hashReqDataText       : HashFn[ReqData.Text       ] = withName("RDText", hashMap)
  protected implicit val hashReqCodeNode       : HashFn[ReqCode.Node       ] = hashCaseClass
  protected implicit val hashLiveCodeGroup     : HashFn[LiveCodeGroup      ] = hashCaseClass
  protected implicit val hashDeadCodeGroup     : HashFn[DeadCodeGroup      ] = hashCaseClass
  protected implicit val hashCodeGroup         : HashFn[CodeGroup          ] = hashADT
  protected implicit val hashReqCodeInactive   : HashFn[ReqCode.Inactive   ] = hashCaseClass
  protected implicit val hashReqCodeActiveGroup: HashFn[ReqCode.ActiveGroup] = hashCaseClass
  protected implicit val hashReqCodeActiveReq  : HashFn[ReqCode.ActiveReq  ] = hashCaseClass
  protected implicit val hashReqCodeData       : HashFn[ReqCode.Data       ] = hashADT
  protected implicit val hashReqCodeTrie       : HashFn[ReqCode.Trie       ] = hashTrie
            implicit val hashReqCodes          : HashFn[ReqCodes           ] = withName("RCs", hashCaseClass)

  protected implicit val hashStaticReqTypeUC: HashFn[StaticReqType.UseCase.type] = hashConstClass("UC")
  protected implicit val hashStaticReqType  : HashFn[StaticReqType             ] = hashADT
  protected implicit val hashReqTypeId      : HashFn[ReqTypeId                 ] = hashADT

            implicit val hashPubidRegister         : HashFn[PubidRegister    ] = withName("PR", hashCaseClass)
  protected implicit val hashPubid                 : HashFn[Pubid            ] = hashCaseClass
  protected implicit def hashPubidT[T <: ReqTypeId]: HashFn[PubidT[T]        ] = hashPubid.narrow
  protected implicit val hashGenericReq            : HashFn[GenericReq       ] = hashCaseClass
            implicit val hashGenericReqs           : HashFn[GenericReqIMap   ] = withName("GRs", hashIMap)
  protected implicit val hashUseCaseStep           : HashFn[UseCaseStep      ] = hashCaseClass
  protected implicit val hashUseCaseSteps          : HashFn[UseCaseSteps     ] = hashCaseClass
  protected implicit val hashUseCase               : HashFn[UseCase          ] = hashCaseClass
  protected implicit val hashUseCaseIMap           : HashFn[UseCaseIMap      ] = withName("UCs", hashIMap)
  protected implicit val hashUseCasesStepFlow      : HashFn[UseCases.StepFlow] = hashCaseClass
            implicit val hashUseCases              : HashFn[UseCases         ] = hashCaseClassSubset('imap -> true, 'stepFlow -> true, 'stepIndex -> false)
  protected implicit val hashReq                   : HashFn[Req              ] = hashADT
  protected implicit val hashRequirements          : HashFn[Requirements     ] = hashCaseClass

  protected implicit val hashCustomIssueType : HashFn[CustomIssueType    ] = hashCaseClass
            implicit val hashCustomIssueTypes: HashFn[CustomIssueTypeIMap] = withName("CIT", hashIMap)
  protected implicit val hashCustomReqType   : HashFn[CustomReqType      ] = hashCaseClass
  protected implicit val hashCustomReqTypes  : HashFn[ReqTypes.Custom    ] = withName("CRT", hashIMap)
            implicit val hashReqTypes        : HashFn[ReqTypes           ] = hashCaseClass

  protected implicit val hashTagId        : HashFn[TagId        ] = hashADT
  protected implicit val hashApplicableTag: HashFn[ApplicableTag] = hashCaseClass
  protected implicit val hashTagGroup     : HashFn[TagGroup     ] = hashCaseClass
  protected implicit val hashTag          : HashFn[Tag          ] = hashADT
  protected implicit val hashTagInTree    : HashFn[TagInTree    ] = hashCaseClass
            implicit val hashTagTree      : HashFn[TagTree      ] = withName("TT", hashIMap)

  protected implicit val hashApplReqTypes     : HashFn[Field.ApplicableReqTypes             ] = hashISubset
  protected implicit val hashCustomFieldTypeIM: HashFn[CustomFieldType.Implication.type     ] = hashConstClass("IM")
  protected implicit val hashCustomFieldTypeTA: HashFn[CustomFieldType.Tag.type             ] = hashConstClass("TA")
  protected implicit val hashCustomFieldTypeTX: HashFn[CustomFieldType.Text.type            ] = hashConstClass("TX")
  protected implicit val hashStaticFieldTypeST: HashFn[StaticFieldType.StepTree.type        ] = hashConstClass("ST")
  protected implicit val hashStaticFieldTypeSG: HashFn[StaticFieldType.StepGraph.type       ] = hashConstClass("SG")
  protected implicit val hashStaticFieldTypeIG: HashFn[StaticFieldType.ImplicationGraph.type] = hashConstClass("IG")
  protected implicit val hashCustomFieldType  : HashFn[CustomFieldType                      ] = hashADT
  protected implicit val hashStaticFieldType  : HashFn[StaticFieldType                      ] = hashADT
  protected implicit val hashFieldType        : HashFn[FieldType                            ] = hashADT
  protected implicit val hashCustomFieldIM    : HashFn[CustomField.Implication              ] = hashCaseClass
  protected implicit val hashCustomFieldTA    : HashFn[CustomField.Tag                      ] = hashCaseClass
  protected implicit val hashCustomFieldTX    : HashFn[CustomField.Text                     ] = hashCaseClass
  protected implicit val hashStaticFieldNS    : HashFn[StaticField.NormalAltStepTree.type   ] = hashConstClass("NS")
  protected implicit val hashStaticFieldES    : HashFn[StaticField.ExceptionStepTree.type   ] = hashConstClass("ES")
  protected implicit val hashStaticFieldSG    : HashFn[StaticField.StepGraph.type           ] = hashConstClass("SG")
  protected implicit val hashStaticFieldIG    : HashFn[StaticField.ImplicationGraph.type    ] = hashConstClass("IG")
  protected implicit val hashCustomField      : HashFn[CustomField                          ] = hashADT
  protected implicit val hashStaticFieldUCT   : HashFn[StaticField.UseCaseStepTree          ] = hashADT
  protected implicit val hashStaticField      : HashFn[StaticField                          ] = hashADT
  protected implicit val hashFieldId          : HashFn[FieldId                              ] = hashADT
            implicit val hashFieldSet         : HashFn[FieldSet                             ] = hashCaseClass

  implicit val hashDeletionReasons: HashFn[DeletionReasons] = hashCaseClass

  private object ReqTableData {
    import reqtable._

    implicit val hashColumnCode          : HashFn[Column.Code          .type] = hashConstClass("RC")
    implicit val hashColumnCustomField   : HashFn[Column.CustomField        ] = hashCaseClass
    implicit val hashColumnDeletionReason: HashFn[Column.DeletionReason.type] = hashConstClass("DR")
    implicit val hashColumnImplications  : HashFn[Column.Implications       ] = hashCaseClass
    implicit val hashColumnPubid         : HashFn[Column.Pubid         .type] = hashConstClass("Id")
    implicit val hashColumnReqType       : HashFn[Column.ReqType       .type] = hashConstClass("RT")
    implicit val hashColumnTags          : HashFn[Column.Tags          .type] = hashConstClass("Ta")
    implicit val hashColumnTitle         : HashFn[Column.Title         .type] = hashConstClass("Ti")

    implicit val hashColumnSIB: HashFn[Column.SortInconclusiveHasBlanks] = hashADT
    implicit val hashColumnSIN: HashFn[Column.SortInconclusiveNoBlanks ] = hashADT
    implicit val hashColumnSI : HashFn[Column.SortInconclusive         ] = hashADT
    implicit val hashColumnSC : HashFn[Column.SortConclusive           ] = hashADT
    implicit val hashColumn   : HashFn[Column                          ] = hashADT

    implicit val hashSortMethodAsc           : HashFn[SortMethod.Asc           .type] = hashConstClass("A")
    implicit val hashSortMethodAscThenBlanks : HashFn[SortMethod.AscThenBlanks .type] = hashConstClass("AB")
    implicit val hashSortMethodBlanksThenAsc : HashFn[SortMethod.BlanksThenAsc .type] = hashConstClass("BA")
    implicit val hashSortMethodBlanksThenDesc: HashFn[SortMethod.BlanksThenDesc.type] = hashConstClass("BD")
    implicit val hashSortMethodDesc          : HashFn[SortMethod.Desc          .type] = hashConstClass("D")
    implicit val hashSortMethodDescThenBlanks: HashFn[SortMethod.DescThenBlanks.type] = hashConstClass("DB")
    implicit val hashSortMethodIB            : HashFn[SortMethod.IgnoreBlanks       ] = hashADT
    implicit val hashSortMethodCB            : HashFn[SortMethod.ConsiderBlanks     ] = hashADT
    implicit val hashSortMethod              : HashFn[SortMethod                    ] = hashADT

    implicit val hashSortCriterionICB: HashFn[SortCriterion.InconclusiveCB] = hashCaseClass
    implicit val hashSortCriterionIIB: HashFn[SortCriterion.InconclusiveIB] = hashCaseClass
    implicit val hashSortCriterionC  : HashFn[SortCriterion.Conclusive    ] = hashCaseClass
    implicit val hashSortCriterionI  : HashFn[SortCriterion.Inconclusive  ] = hashADT
    implicit val hashSortCriterion   : HashFn[SortCriterion               ] = hashADT
    implicit val hashSortCriteria    : HashFn[SortCriteria                ] = hashCaseClass

    implicit val hashView         : HashFn[View               ] = hashCaseClass
    implicit val hashSavedViewId  : HashFn[SavedView.Id       ] = hashCaseClass
    implicit val hashSavedViewName: HashFn[SavedView.Name     ] = hashCaseClass
    implicit val hashSavedView    : HashFn[SavedView          ] = hashCaseClass
    implicit val hashSavedViewsNE : HashFn[SavedViews.NonEmpty] = hashCaseClass
    implicit val hashSavedViews   : HashFn[SavedViews.Optional] = hashOption
  }
  implicit val hashSavedViews = ReqTableData.hashSavedViews

  protected implicit val hashNonEmptySetInt             : HashFn[NonEmptySet[Int]               ] = hashNES
  protected implicit def hashIntensionalReqSetS[A: HashFn]: HashFn[IntensionalReqSet.SomeOfType[A]] = hashCaseClass
  protected implicit def hashIntensionalReqSetW[A: HashFn]: HashFn[IntensionalReqSet.WholeType [A]] = hashCaseClass
  protected implicit def hashIntensionalReqSet [A: HashFn]: HashFn[IntensionalReqSet           [A]] = hashADT

  protected implicit val hashValidFilter: HashFn[Filter.Valid] = {
    import Filter._
    implicit val hashValidReqSubset     : HashFn[Valid.ReqSubset                        ] = hashADT
    implicit val hashValidReqSet        : HashFn[Valid.ReqSet                           ] = hashNEV
    implicit val hashValidText          : HashFn[FilterAst.Text                         ] = hashCaseClass
    implicit val hashValidRegex         : HashFn[FilterAst.Regex                        ] = hashCaseClass
    implicit val hashValidAttrI         : HashFn[FilterAst.Attr.AnyIssue .type          ] = hashConstClass("I")
    implicit val hashValidAttrT         : HashFn[FilterAst.Attr.AnyTag   .type          ] = hashConstClass("T")
    implicit val hashValidAttr          : HashFn[FilterAst.Attr                         ] = hashADT
    implicit val hashValidPresence      : HashFn[FilterAst.Presence      [Valid.Attr]   ] = hashCaseClass
    implicit val hashValidLack          : HashFn[FilterAst.Lack          [Valid.Attr]   ] = hashCaseClass
    implicit val hashValidReqs          : HashFn[FilterAst.Reqs          [Valid.ReqSet] ] = hashCaseClass
    implicit val hashValidReqType       : HashFn[FilterAst.ReqType       [Valid.ReqType]] = hashCaseClass
    implicit val hashValidHashRef       : HashFn[FilterAst.HashRef       [Valid.HashTag]] = hashCaseClass
    implicit val hashValidImpliesAnyOf  : HashFn[FilterAst.ImpliesAnyOf  [Valid.ReqSet] ] = hashCaseClass
    implicit val hashValidImpliedByAnyOf: HashFn[FilterAst.ImpliedByAnyOf[Valid.ReqSet] ] = hashCaseClass
    implicit val hashValidAllOf         : HashFn[FilterAst.AllOf         [Int]          ] = hashCaseClass
    implicit val hashValidAnyOf         : HashFn[FilterAst.AnyOf         [Int]          ] = hashCaseClass
    implicit val hashValidNot           : HashFn[FilterAst.Not           [Int]          ] = hashCaseClass
             val hashValidInt           : HashFn[ValidF                  [Int]          ] = hashADT
    hashFix(hashValidInt.hashFn)
  }
}
