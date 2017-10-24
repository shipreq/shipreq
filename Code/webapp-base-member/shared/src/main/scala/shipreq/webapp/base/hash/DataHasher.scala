package shipreq.webapp.base.hash

import japgolly.microlibs.nonempty._
import japgolly.microlibs.recursion._
import nyaya.util.Multimap
import scalaz.{Functor, \/}
import shipreq.base.util.TaggedTypes.TaggedType
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.{Filter, FilterAst, IntensionalReqSet}
import shipreq.webapp.base.filter.Filter.Implicits._
import shipreq.webapp.base.text.{AtomTC, Text}
import Hash.HashableValueOps

sealed abstract class GenericDashHasher {
  protected val algorithm: Hash.Algorithm
  import algorithm._

  /**
   * Mixes the hash with a hash of `name` so that identical values in different places don't have identical hashes.
   *
   * @param name Some arbitrary string.
   */
  final protected def withName[A](name: String, h: Hash[A]): Hash[A] = {
    val q = hashString.hash(name) :: Nil
    Hash.fn[A](a => joinHashes(h.hash(a) :: q))
  }

  protected def hashTaggedType[T <: TaggedType](implicit h: Hash[T#U]): Hash[T] =
    h.cmap(_.value)

  protected implicit def hashMultimap[K, L[_], V](implicit h: Hash[Map[K, L[V]]]): Hash[Multimap[K, L, V]] =
    h.cmap(_.m)

  private val hashNone = "∅".hash

  protected implicit def hashOption[A: Hash]: Hash[Option[A]] = {
    val some = withName("!", Hash[A])
    Hash.fn(_.fold(hashNone)(some.hash))
  }

  protected implicit def hashNEV[A: Hash]: Hash[NonEmptyVector[A]] = Hash.by(_.whole)

  protected implicit def hashNES[A: Hash]: Hash[NonEmptySet[A]] = Hash.by(_.whole)

  protected implicit def hashTrie[K: Hash, V: Hash]: Hash[MTrie.Trie[K, V]] = {
    import MTrie.{Branch, Node, Trie, Value}
    implicit      val value : Hash[Value [K, V]] = hashCaseClass
    implicit      val valueO                     = hashOption(value)
    implicit lazy val branch: Hash[Branch[K, V]] = hashCaseClass
    implicit lazy val node  : Hash[Node  [K, V]] = Hash.fn(_.fold(branch.hash, value.hash))
    implicit lazy val trie  : Hash[Trie  [K, V]] = hashMap
    trie
  }

  protected implicit def hashVectorTree[A](implicit ha: Hash[A]): Hash[VectorTree[A]] = {
    import VectorTree._

    implicit lazy val node: Hash[Node[A]] =
      Hash.fn[Node[A]](n => joinHashes(
        ha.hash(n.value) :: children.hash(n.children) :: Nil))

    implicit lazy val children: Hash[Children[A]] =
      hashVector[Node[A]]

    hashCaseClass
  }

  protected implicit def hashIMap[K, V: Hash]: Hash[IMap[K, V]] =
    hashUnordered[Iterable, V].cmap(_.values)

  protected implicit def disjunction[A: Hash, B: Hash]: Hash[A \/ B] = {
    val l = withName("!", Hash[A])
    val r = Hash[B]
    Hash.fn(_.fold(l.hash, r.hash))
  }

  protected def hashISubset[A: Hash]: Hash[ISubset[A]] = {
    import ISubset._
    implicit val anes = hashNES[A]
    implicit val all : Hash[All [A]] = hashConstClass("Al")
    implicit val only: Hash[Only[A]] = withName("On", hashCaseClass)
    implicit val not : Hash[Not [A]] = withName("No", hashCaseClass)
    hashADT
  }

  protected def hashFix[F[_]: Functor](algebra: Algebra[F, Int]): Hash[Fix[F]] =
    Hash.fn(Recursion.cata(algebra))
}

sealed abstract class DataHasher extends GenericDashHasher {
  import algorithm._

  def apply(scope: HashScope, p: Project): Int

  protected implicit val hashLive         : Hash[Live]                = Hash by Live.from
  protected implicit val hashImplRequired : Hash[ImplicationRequired] = Hash by ImplicationRequired.from
  protected implicit val hashMandatory    : Hash[Mandatory]           = Hash by Mandatory.from
  protected implicit val hashDeletable    : Hash[Deletable]           = Hash by Deletable.from
  protected implicit val hashDirection    : Hash[Direction]           = Hash by Forwards.from
  protected implicit val hashFilterDead   : Hash[FilterDead]          = Hash by ShowDead.from
  protected implicit val hashMutexChildren: Hash[MutexChildren]       = Hash by MutexChildren.from

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

  protected implicit val hashReqId: Hash[ReqId] = Hash.by(_.value)

  protected implicit val hashImplications: Hash[Implications] = withName("Imp", hashCaseClass)

  protected implicit val hashReqDataTags: Hash[ReqData.Tags] = withName("RDTags", hashMultimap)

  private object HashAtoms extends AtomTC[Hash] {
    import shipreq.webapp.base.text._
    import Atom._

    override def lazily[A](a: => Hash[A]): Hash[A] = {
      lazy val b = a
      Hash.fn(a => b hash a)
    }

    override def vec[A](implicit a: Hash[A]) =
      hashVector(a)

    override def nev[A](as: Hash[Vector[A]])(implicit a: Hash[A]) =
      hashNEV(a)

    override def sum[T <: Atom.Base](t: T)(f: t.Atom => Hash[t.Atom], i: t.Atom => Int, v: Vector[Hash[t.Atom]]): Hash[t.Atom] =
      Hash.fn[t.Atom](a => f(a) hash a)

    override def blankLine     [T <: NewLine        ](t: T): Hash[t.BlankLine     ] = hashConstClass("BL")
    override def literal       [T <: Literal        ](t: T): Hash[t.Literal       ] = withName("LI", hashCaseClass)
    override def webAddress    [T <: PlainTextMarkup](t: T): Hash[t.WebAddress    ] = withName("WA", hashCaseClass)
    override def emailAddress  [T <: PlainTextMarkup](t: T): Hash[t.EmailAddress  ] = withName("EA", hashCaseClass)
    override def mathTeX       [T <: PlainTextMarkup](t: T): Hash[t.MathTeX       ] = withName("MX", hashCaseClass)
    override def reqRef        [T <: ReqRef         ](t: T): Hash[t.ReqRef        ] = withName("RR", hashCaseClass)
    override def codeRef       [T <: ReqRef         ](t: T): Hash[t.CodeRef       ] = withName("CR", hashCaseClass)
    override def useCaseStepRef[T <: UseCaseStepRef ](t: T): Hash[t.UseCaseStepRef] = withName("UR", hashCaseClass)
    override def tagRef        [T <: TagRef         ](t: T): Hash[t.TagRef        ] = withName("TR", hashCaseClass)

    override def issue[T <: Issue](t: T)(implicit h: Hash[Text.InlineIssueDesc.OptionalText]): Hash[t.Issue] =
      withName("IS", hashCaseClass)

    override def unorderedList[T <: ListMarkup](t: T)(implicit h: Hash[NonEmptyVector[t.ListItem]]): Hash[t.UnorderedList] =
      withName("UL", hashCaseClass)
  }

  import HashAtoms.instances._

  protected implicit val hashReqDataText       : Hash[ReqData.Text       ] = withName("RDText", hashMap)
  protected implicit val hashReqCodeNode       : Hash[ReqCode.Node       ] = hashCaseClass
  protected implicit val hashLiveCodeGroup     : Hash[LiveCodeGroup      ] = hashCaseClass
  protected implicit val hashDeadCodeGroup     : Hash[DeadCodeGroup      ] = hashCaseClass
  protected implicit val hashCodeGroup         : Hash[CodeGroup          ] = hashADT
  protected implicit val hashReqCodeInactive   : Hash[ReqCode.Inactive   ] = hashCaseClass
  protected implicit val hashReqCodeActiveGroup: Hash[ReqCode.ActiveGroup] = hashCaseClass
  protected implicit val hashReqCodeActiveReq  : Hash[ReqCode.ActiveReq  ] = hashCaseClass
  protected implicit val hashReqCodeData       : Hash[ReqCode.Data       ] = hashADT
  protected implicit val hashReqCodeTrie       : Hash[ReqCode.Trie       ] = hashTrie
  protected implicit val hashReqCodes          : Hash[ReqCodes           ] = withName("RCs", hashCaseClass)

  protected implicit val hashStaticReqTypeUC: Hash[StaticReqType.UseCase.type] = hashConstClass("UC")
  protected implicit val hashStaticReqType  : Hash[StaticReqType             ] = hashADT
  protected implicit val hashReqTypeId      : Hash[ReqTypeId                 ] = hashADT

  protected implicit val hashPubidRegister         : Hash[PubidRegister    ] = withName("PR", hashCaseClass)
  protected implicit val hashPubid                 : Hash[Pubid            ] = hashCaseClass
  protected implicit def hashPubidT[T <: ReqTypeId]: Hash[PubidT[T]        ] = hashPubid.narrow
  protected implicit val hashGenericReq            : Hash[GenericReq       ] = hashCaseClass
  protected implicit val hashGenericReqs           : Hash[GenericReqIMap   ] = withName("GRs", hashIMap)
  protected implicit val hashUseCaseStep           : Hash[UseCaseStep      ] = hashCaseClass
  protected implicit val hashUseCaseSteps          : Hash[UseCaseSteps     ] = hashCaseClass
  protected implicit val hashUseCase               : Hash[UseCase          ] = hashCaseClass
  protected implicit val hashUseCaseIMap           : Hash[UseCaseIMap      ] = withName("UCs", hashIMap)
  protected implicit val hashUseCasesStepFlow      : Hash[UseCases.StepFlow] = hashCaseClass
  protected implicit val hashUseCases              : Hash[UseCases         ] = hashCaseClassSubset('imap -> true, 'stepFlow -> true, 'stepIndex -> false)
  protected implicit val hashReq                   : Hash[Req              ] = hashADT
  protected implicit val hashRequirements          : Hash[Requirements     ] = hashCaseClass

  protected implicit val hashCustomIssueType : Hash[CustomIssueType    ] = hashCaseClass
  protected implicit val hashCustomIssueTypes: Hash[CustomIssueTypeIMap] = withName("CIT", hashIMap)
  protected implicit val hashCustomReqType   : Hash[CustomReqType      ] = hashCaseClass
  protected implicit val hashCustomReqTypes  : Hash[ReqTypes.Custom    ] = withName("CRT", hashIMap)
  protected implicit val hashReqTypes        : Hash[ReqTypes           ] = hashCaseClass

  protected implicit val hashTagId        : Hash[TagId        ] = hashADT
  protected implicit val hashApplicableTag: Hash[ApplicableTag] = hashCaseClass
  protected implicit val hashTagGroup     : Hash[TagGroup     ] = hashCaseClass
  protected implicit val hashTag          : Hash[Tag          ] = hashADT
  protected implicit val hashTagInTree    : Hash[TagInTree    ] = hashCaseClass
  protected implicit val hashTagTree      : Hash[TagTree      ] = withName("TT", hashIMap)

  protected implicit val hashApplReqTypes     : Hash[Field.ApplicableReqTypes             ] = hashISubset
  protected implicit val hashCustomFieldTypeIM: Hash[CustomFieldType.Implication.type     ] = hashConstClass("IM")
  protected implicit val hashCustomFieldTypeTA: Hash[CustomFieldType.Tag.type             ] = hashConstClass("TA")
  protected implicit val hashCustomFieldTypeTX: Hash[CustomFieldType.Text.type            ] = hashConstClass("TX")
  protected implicit val hashStaticFieldTypeST: Hash[StaticFieldType.StepTree.type        ] = hashConstClass("ST")
  protected implicit val hashStaticFieldTypeSG: Hash[StaticFieldType.StepGraph.type       ] = hashConstClass("SG")
  protected implicit val hashStaticFieldTypeIG: Hash[StaticFieldType.ImplicationGraph.type] = hashConstClass("IG")
  protected implicit val hashCustomFieldType  : Hash[CustomFieldType                      ] = hashADT
  protected implicit val hashStaticFieldType  : Hash[StaticFieldType                      ] = hashADT
  protected implicit val hashFieldType        : Hash[FieldType                            ] = hashADT
  protected implicit val hashCustomFieldIM    : Hash[CustomField.Implication              ] = hashCaseClass
  protected implicit val hashCustomFieldTA    : Hash[CustomField.Tag                      ] = hashCaseClass
  protected implicit val hashCustomFieldTX    : Hash[CustomField.Text                     ] = hashCaseClass
  protected implicit val hashStaticFieldNS    : Hash[StaticField.NormalAltStepTree.type   ] = hashConstClass("NS")
  protected implicit val hashStaticFieldES    : Hash[StaticField.ExceptionStepTree.type   ] = hashConstClass("ES")
  protected implicit val hashStaticFieldSG    : Hash[StaticField.StepGraph.type           ] = hashConstClass("SG")
  protected implicit val hashStaticFieldIG    : Hash[StaticField.ImplicationGraph.type    ] = hashConstClass("IG")
  protected implicit val hashCustomField      : Hash[CustomField                          ] = hashADT
  protected implicit val hashStaticFieldUCT   : Hash[StaticField.UseCaseStepTree          ] = hashADT
  protected implicit val hashStaticField      : Hash[StaticField                          ] = hashADT
  protected implicit val hashFieldId          : Hash[FieldId                              ] = hashADT
  protected implicit val hashFieldSet         : Hash[FieldSet                             ] = hashCaseClass

  protected implicit val hashDeletionReasons: Hash[DeletionReasons] = hashCaseClass

  private object ReqTableData {
    import reqtable._

    implicit val hashColumnCode          : Hash[Column.Code          .type] = hashConstClass("RC")
    implicit val hashColumnCustomField   : Hash[Column.CustomField        ] = hashCaseClass
    implicit val hashColumnDeletionReason: Hash[Column.DeletionReason.type] = hashConstClass("DR")
    implicit val hashColumnImplications  : Hash[Column.Implications       ] = hashCaseClass
    implicit val hashColumnPubid         : Hash[Column.Pubid         .type] = hashConstClass("Id")
    implicit val hashColumnReqType       : Hash[Column.ReqType       .type] = hashConstClass("RT")
    implicit val hashColumnTags          : Hash[Column.Tags          .type] = hashConstClass("Ta")
    implicit val hashColumnTitle         : Hash[Column.Title         .type] = hashConstClass("Ti")

    implicit val hashColumnSIB: Hash[Column.SortInconclusiveHasBlanks] = hashADT
    implicit val hashColumnSIN: Hash[Column.SortInconclusiveNoBlanks ] = hashADT
    implicit val hashColumnSI : Hash[Column.SortInconclusive         ] = hashADT
    implicit val hashColumnSC : Hash[Column.SortConclusive           ] = hashADT
    implicit val hashColumn   : Hash[Column                          ] = hashADT

    implicit val hashSortMethodAsc           : Hash[SortMethod.Asc           .type] = hashConstClass("A")
    implicit val hashSortMethodAscThenBlanks : Hash[SortMethod.AscThenBlanks .type] = hashConstClass("AB")
    implicit val hashSortMethodBlanksThenAsc : Hash[SortMethod.BlanksThenAsc .type] = hashConstClass("BA")
    implicit val hashSortMethodBlanksThenDesc: Hash[SortMethod.BlanksThenDesc.type] = hashConstClass("BD")
    implicit val hashSortMethodDesc          : Hash[SortMethod.Desc          .type] = hashConstClass("D")
    implicit val hashSortMethodDescThenBlanks: Hash[SortMethod.DescThenBlanks.type] = hashConstClass("DB")
    implicit val hashSortMethodIB            : Hash[SortMethod.IgnoreBlanks       ] = hashADT
    implicit val hashSortMethodCB            : Hash[SortMethod.ConsiderBlanks     ] = hashADT
    implicit val hashSortMethod              : Hash[SortMethod                    ] = hashADT

    implicit val hashSortCriterionICB: Hash[SortCriterion.InconclusiveCB] = hashCaseClass
    implicit val hashSortCriterionIIB: Hash[SortCriterion.InconclusiveIB] = hashCaseClass
    implicit val hashSortCriterionC  : Hash[SortCriterion.Conclusive    ] = hashCaseClass
    implicit val hashSortCriterionI  : Hash[SortCriterion.Inconclusive  ] = hashADT
    implicit val hashSortCriterion   : Hash[SortCriterion               ] = hashADT
    implicit val hashSortCriteria    : Hash[SortCriteria                ] = hashCaseClass

    implicit val hashSavedViewId  : Hash[SavedView.Id       ] = hashCaseClass
    implicit val hashSavedViewName: Hash[SavedView.Name     ] = hashCaseClass
    implicit val hashSavedView    : Hash[SavedView          ] = hashCaseClass
    implicit val hashSavedViews   : Hash[SavedViews.NonEmpty] = hashCaseClass
  }
  protected implicit def hashSavedViews = ReqTableData.hashSavedViews

  implicit val hashNonEmptySetInt             : Hash[NonEmptySet[Int]               ] = hashNES
  implicit def hashIntensionalReqSetS[A: Hash]: Hash[IntensionalReqSet.SomeOfType[A]] = hashCaseClass
  implicit def hashIntensionalReqSetW[A: Hash]: Hash[IntensionalReqSet.WholeType [A]] = hashCaseClass
  implicit def hashIntensionalReqSet [A: Hash]: Hash[IntensionalReqSet           [A]] = hashADT

  protected implicit val hashValidFilter: Hash[Filter.Valid] = {
    import Filter._
    implicit val hashValidReqSubset     : Hash[Valid.ReqSubset                        ] = hashADT
    implicit val hashValidReqSet        : Hash[Valid.ReqSet                           ] = hashNEV
    implicit val hashValidText          : Hash[FilterAst.Text                         ] = hashCaseClass
    implicit val hashValidRegex         : Hash[FilterAst.Regex                        ] = hashCaseClass
    implicit val hashValidAttrI         : Hash[FilterAst.Attr.AnyIssue .type          ] = hashConstClass("I")
    implicit val hashValidAttrT         : Hash[FilterAst.Attr.AnyTag   .type          ] = hashConstClass("T")
    implicit val hashValidAttr          : Hash[FilterAst.Attr                         ] = hashADT
    implicit val hashValidPresence      : Hash[FilterAst.Presence      [Valid.Attr]   ] = hashCaseClass
    implicit val hashValidLack          : Hash[FilterAst.Lack          [Valid.Attr]   ] = hashCaseClass
    implicit val hashValidReqs          : Hash[FilterAst.Reqs          [Valid.ReqSet] ] = hashCaseClass
    implicit val hashValidReqType       : Hash[FilterAst.ReqType       [Valid.ReqType]] = hashCaseClass
    implicit val hashValidHashRef       : Hash[FilterAst.HashRef       [Valid.HashTag]] = hashCaseClass
    implicit val hashValidImpliesAnyOf  : Hash[FilterAst.ImpliesAnyOf  [Valid.ReqSet] ] = hashCaseClass
    implicit val hashValidImpliedByAnyOf: Hash[FilterAst.ImpliedByAnyOf[Valid.ReqSet] ] = hashCaseClass
    implicit val hashValidAllOf         : Hash[FilterAst.AllOf         [Int]          ] = hashCaseClass
    implicit val hashValidAnyOf         : Hash[FilterAst.AnyOf         [Int]          ] = hashCaseClass
    implicit val hashValidNot           : Hash[FilterAst.Not           [Int]          ] = hashCaseClass
             val hashValidInt           : Hash[ValidF                  [Int]          ] = hashADT
    hashFix(hashValidInt.hash)
  }

  protected implicit val hashProjectConfig : Hash[ProjectConfig] = hashCaseClass
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
final class DataHasherV1(protected val algorithm: Hash.Algorithm) extends DataHasher { // TODO Replace with V2
  import algorithm._

  protected val hashProjectContent: Hash[Project] = {
    implicit val hashIdCeilings: Hash[IdCeilings] =
      hashCaseClassSubset(
        'customIssueType -> true,
        'customReqType   -> true,
        'customField     -> true,
        'tag             -> true,
        'req             -> true,
        'useCaseStep     -> true,
        'reqCode         -> true,
        'reqtableView    -> false)
    hashCaseClassSubset(
      'name            -> false,
      'config          -> false,
      'reqs            -> true,
      'reqCodes        -> true,
      'reqText         -> true,
      'reqTags         -> true,
      'implications    -> true,
      'deletionReasons -> true,
      'reqtableViews   -> false,
      'idCeilings      -> true)
  }

  protected val hashProjectOther: Hash[Project] =
    hashCaseClassSubset(
      'name            -> true,
      'config          -> false,
      'reqs            -> false,
      'reqCodes        -> false,
      'reqText         -> false,
      'reqTags         -> false,
      'implications    -> false,
      'deletionReasons -> false,
      'reqtableViews   -> false,
      'idCeilings      -> false)

  protected val hashProject: Hash[Project] =
    Hash.fn[Project](p => joinHashes(
      hashProjectContent.hash(p)        ::
      hashProjectConfig .hash(p.config) ::
      hashProjectOther  .hash(p)        ::
      Nil))

  override def apply(scope: HashScope, p: Project): Int =
    scope match {
      case HashScope.WholeProject    => hashProject          hash p
      case HashScope.Config          => hashProjectConfig    hash p.config
      case HashScope.CfgIssueTypes   => hashCustomIssueTypes hash p.config.customIssueTypes
      case HashScope.CfgReqTypes     => hashReqTypes         hash p.config.reqTypes
      case HashScope.CfgFields       => hashFieldSet         hash p.config.fields
      case HashScope.CfgTags         => hashTagTree          hash p.config.tags
      case HashScope.Content         => hashProjectContent   hash p
      case HashScope.Reqs            => hashRequirements     hash p.reqs
      case HashScope.GenericReqs     => hashGenericReqs      hash p.reqs.genericReqs
      case HashScope.UseCases        => hashUseCases         hash p.reqs.useCases
      case HashScope.PubidRegister   => hashPubidRegister    hash p.reqs.pubids
      case HashScope.ReqCodes        => hashReqCodes         hash p.reqCodes
      case HashScope.TextFieldData   => hashReqDataText      hash p.reqText
      case HashScope.TagData         => hashReqDataTags      hash p.reqTags
      case HashScope.ImplicationData => hashImplications     hash p.implications
      case HashScope.DeletionReasons => hashDeletionReasons  hash p.deletionReasons
      case HashScope.Other           => hashProjectOther     hash p
    }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
final class DataHasherV2(protected val algorithm: Hash.Algorithm) extends DataHasher {
  import algorithm._

  protected val hashProjectContent: Hash[Project] = {
    implicit val hashIdCeilings: Hash[IdCeilings] =
      hashCaseClassSubset(
        'customIssueType -> true,
        'customReqType   -> true,
        'customField     -> true,
        'tag             -> true,
        'req             -> true,
        'useCaseStep     -> true,
        'reqCode         -> true,
        'reqtableView    -> false)
    hashCaseClassSubset(
      'name            -> false,
      'config          -> false,
      'reqs            -> true,
      'reqCodes        -> true,
      'reqText         -> true,
      'reqTags         -> true,
      'implications    -> true,
      'deletionReasons -> true,
      'reqtableViews   -> false,
      'idCeilings      -> true)
  }

  protected val hashProjectOther: Hash[Project] = {
    implicit val hashIdCeilings: Hash[IdCeilings] =
      hashCaseClassSubset(
        'customIssueType -> false,
        'customReqType   -> false,
        'customField     -> false,
        'tag             -> false,
        'req             -> false,
        'useCaseStep     -> false,
        'reqCode         -> false,
        'reqtableView    -> true)
    hashCaseClassSubset(
      'name            -> true,
      'config          -> false,
      'reqs            -> false,
      'reqCodes        -> false,
      'reqText         -> false,
      'reqTags         -> false,
      'implications    -> false,
      'deletionReasons -> false,
      'reqtableViews   -> true,
      'idCeilings      -> true)
  }

  protected val hashProject: Hash[Project] =
    Hash.fn[Project](p => joinHashes(
      hashProjectContent.hash(p)        ::
      hashProjectConfig .hash(p.config) ::
      hashProjectOther  .hash(p)        ::
      Nil))

  override def apply(scope: HashScope, p: Project): Int =
    scope match {
      case HashScope.WholeProject    => hashProject          hash p
      case HashScope.Config          => hashProjectConfig    hash p.config
      case HashScope.CfgIssueTypes   => hashCustomIssueTypes hash p.config.customIssueTypes
      case HashScope.CfgReqTypes     => hashReqTypes         hash p.config.reqTypes
      case HashScope.CfgFields       => hashFieldSet         hash p.config.fields
      case HashScope.CfgTags         => hashTagTree          hash p.config.tags
      case HashScope.Content         => hashProjectContent   hash p
      case HashScope.Reqs            => hashRequirements     hash p.reqs
      case HashScope.GenericReqs     => hashGenericReqs      hash p.reqs.genericReqs
      case HashScope.UseCases        => hashUseCases         hash p.reqs.useCases
      case HashScope.PubidRegister   => hashPubidRegister    hash p.reqs.pubids
      case HashScope.ReqCodes        => hashReqCodes         hash p.reqCodes
      case HashScope.TextFieldData   => hashReqDataText      hash p.reqText
      case HashScope.TagData         => hashReqDataTags      hash p.reqTags
      case HashScope.ImplicationData => hashImplications     hash p.implications
      case HashScope.DeletionReasons => hashDeletionReasons  hash p.deletionReasons
      case HashScope.Other           => hashProjectOther     hash p
    }
}