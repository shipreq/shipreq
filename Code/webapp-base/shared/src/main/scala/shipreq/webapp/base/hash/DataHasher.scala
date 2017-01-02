package shipreq.webapp.base.hash

import japgolly.microlibs.nonempty._
import nyaya.util.Multimap
import scalaz.\/
import shipreq.base.util.TaggedTypes.TaggedType
import shipreq.base.util._
import shipreq.webapp.base.data._
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
  final def withName[A](name: String, h: Hash[A]): Hash[A] = {
    val q = hashString.hash(name) :: Nil
    Hash.fn[A](a => joinHashes(h.hash(a) :: q))
  }

  def hashTaggedType[T <: TaggedType](implicit h: Hash[T#U]): Hash[T] =
    h.cmap(_.value)

  implicit def hashMultimap[K, L[_], V](implicit h: Hash[Map[K, L[V]]]): Hash[Multimap[K, L, V]] =
    h.cmap(_.m)

  private val hashNone = "∅".hash

  implicit def hashOption[A: Hash]: Hash[Option[A]] = {
    val some = withName("!", Hash[A])
    Hash.fn(_.fold(hashNone)(some.hash))
  }

  implicit def hashNEV[A: Hash]: Hash[NonEmptyVector[A]] = Hash.by(_.whole)

  implicit def hashNES[A: Hash]: Hash[NonEmptySet[A]] = Hash.by(_.whole)

  implicit def hashTrie[K: Hash, V: Hash]: Hash[MTrie.Trie[K, V]] = {
    import MTrie.{Branch, Node, Trie, Value}
    implicit      val value : Hash[Value [K, V]] = hashCaseClass
    implicit      val valueO                     = hashOption(value)
    implicit lazy val branch: Hash[Branch[K, V]] = hashCaseClass
    implicit lazy val node  : Hash[Node  [K, V]] = Hash.fn(_.fold(branch.hash, value.hash))
    implicit lazy val trie  : Hash[Trie  [K, V]] = hashMap
    trie
  }

  implicit def hashVectorTree[A](implicit ha: Hash[A]): Hash[VectorTree[A]] = {
    import VectorTree._

    implicit lazy val node: Hash[Node[A]] =
      Hash.fn[Node[A]](n => joinHashes(
        ha.hash(n.value) :: children.hash(n.children) :: Nil))

    implicit lazy val children: Hash[Children[A]] =
      hashVector[Node[A]]

    hashCaseClass
  }

  implicit def hashIMap[K, V: Hash]: Hash[IMap[K, V]] =
    hashUnordered[Iterable, V].cmap(_.values)

  implicit def disjunction[A: Hash, B: Hash]: Hash[A \/ B] = {
    val l = withName("!", Hash[A])
    val r = Hash[B]
    Hash.fn(_.fold(l.hash, r.hash))
  }

  def hashISubset[A: Hash]: Hash[ISubset[A]] = {
    import ISubset._
    implicit val anes = hashNES[A]
    implicit val all : Hash[All [A]] = hashConstClass("Al")
    implicit val only: Hash[Only[A]] = withName("On", hashCaseClass)
    implicit val not : Hash[Not [A]] = withName("No", hashCaseClass)
    hashADT
  }
}

sealed abstract class DataHasher extends GenericDashHasher {
  import algorithm._

  implicit val hashLive         : Hash[Live]                = Hash by Live.from
  implicit val hashImplRequired : Hash[ImplicationRequired] = Hash by ImplicationRequired.from
  implicit val hashMandatory    : Hash[Mandatory]           = Hash by Mandatory.from
  implicit val hashDeletable    : Hash[Deletable]           = Hash by Deletable.from
  implicit val hashMutexChildren: Hash[MutexChildren]       = Hash by MutexChildren.from

  implicit val hashUseCaseStepId            = hashTaggedType[UseCaseStepId]
  implicit val hashUseCaseId                = hashTaggedType[UseCaseId]
  implicit val hashDeletionReasonId         = hashTaggedType[DeletionReasonId]
  implicit val hashGenericReqId             = hashTaggedType[GenericReqId]
  implicit val hashReqCodeId                = hashTaggedType[ReqCodeId]
  implicit val hashCustomReqTypeId          = hashTaggedType[CustomReqTypeId]
  implicit val hashCustomIssueTypeId        = hashTaggedType[CustomIssueTypeId]
  implicit val hashApplicableTagId          = hashTaggedType[ApplicableTagId]
  implicit val hashTagGroupId               = hashTaggedType[TagGroupId]
  implicit val hashCustomFieldId            = hashTaggedType[CustomFieldId]
  implicit val hashCustomFieldTagId         = hashTaggedType[CustomField.Tag.Id]
  implicit val hashCustomFieldTextId        = hashTaggedType[CustomField.Text.Id]
  implicit val hashCustomFieldImplicationId = hashTaggedType[CustomField.Implication.Id]
  implicit val hashHashRefKey               = hashTaggedType[HashRefKey]
  implicit val hashReqTypePos               = hashTaggedType[ReqTypePos]
  implicit val hashFieldRefKey              = hashTaggedType[FieldRefKey]
  implicit val hashReqTypeMnemonic          = hashTaggedType[ReqType.Mnemonic]

  implicit val hashReqId: Hash[ReqId] = Hash.by(_.value)

  implicit val hashImplications: Hash[Implications] = withName("Imp", hashCaseClass)

  implicit val hashReqDataTags: Hash[ReqData.Tags] = withName("RDTags", hashMultimap)

  object HashAtoms extends AtomTC[Hash] {
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

  implicit val hashReqDataText       : Hash[ReqData.Text       ] = withName("RDText", hashMap)
  implicit val hashReqCodeNode       : Hash[ReqCode.Node       ] = hashCaseClass
  implicit val hashLiveReqCodeGroup  : Hash[LiveReqCodeGroup   ] = hashCaseClass
  implicit val hashDeadReqCodeGroup  : Hash[DeadReqCodeGroup   ] = hashCaseClass
  implicit val hashReqCodeGroup      : Hash[ReqCodeGroup       ] = hashADT
  implicit val hashReqCodeInactive   : Hash[ReqCode.Inactive   ] = hashCaseClass
  implicit val hashReqCodeActiveGroup: Hash[ReqCode.ActiveGroup] = hashCaseClass
  implicit val hashReqCodeActiveReq  : Hash[ReqCode.ActiveReq  ] = hashCaseClass
  implicit val hashReqCodeData       : Hash[ReqCode.Data       ] = hashADT
  implicit val hashReqCodeTrie       : Hash[ReqCode.Trie       ] = hashTrie
  implicit val hashReqCodes          : Hash[ReqCodes           ] = withName("RCs", hashCaseClass)

  implicit val hashStaticReqTypeUC: Hash[StaticReqType.UseCase.type] = hashConstClass("UC")
  implicit val hashStaticReqType  : Hash[StaticReqType             ] = hashADT
  implicit val hashReqTypeId      : Hash[ReqTypeId                 ] = hashADT

  implicit val hashPubidRegister         : Hash[PubidRegister    ] = withName("PR", hashCaseClass)
  implicit val hashPubid                 : Hash[Pubid            ] = hashCaseClass
  implicit def hashPubidT[T <: ReqTypeId]: Hash[PubidT[T]        ] = hashPubid.narrow
  implicit val hashGenericReq            : Hash[GenericReq       ] = hashCaseClass
  implicit val hashGenericReqs           : Hash[GenericReqIMap   ] = withName("GRs", hashIMap)
  implicit val hashUseCaseStep           : Hash[UseCaseStep      ] = hashCaseClass
  implicit val hashUseCaseSteps          : Hash[UseCaseSteps     ] = hashCaseClass
  implicit val hashUseCase               : Hash[UseCase          ] = hashCaseClass
  implicit val hashUseCaseIMap           : Hash[UseCaseIMap      ] = withName("UCs", hashIMap)
  implicit val hashUseCasesStepFlow      : Hash[UseCases.StepFlow] = hashCaseClass
  implicit val hashUseCases              : Hash[UseCases         ] = hashCaseClassExcept('stepIndex)
  implicit val hashReq                   : Hash[Req              ] = hashADT
  implicit val hashRequirements          : Hash[Requirements     ] = hashCaseClass

  implicit val hashCustomIssueType : Hash[CustomIssueType    ] = hashCaseClass
  implicit val hashCustomIssueTypes: Hash[CustomIssueTypeIMap] = withName("CIT", hashIMap)
  implicit val hashCustomReqType   : Hash[CustomReqType      ] = hashCaseClass
  implicit val hashCustomReqTypes  : Hash[ReqTypes.Custom    ] = withName("CRT", hashIMap)
  implicit val hashReqTypes        : Hash[ReqTypes           ] = hashCaseClass

  implicit val hashTagId        : Hash[TagId        ] = hashADT
  implicit val hashApplicableTag: Hash[ApplicableTag] = hashCaseClass
  implicit val hashTagGroup     : Hash[TagGroup     ] = hashCaseClass
  implicit val hashTag          : Hash[Tag          ] = hashADT
  implicit val hashTagInTree    : Hash[TagInTree    ] = hashCaseClass
  implicit val hashTagTree      : Hash[TagTree      ] = withName("TT", hashIMap)

  implicit val hashApplReqTypes     : Hash[Field.ApplicableReqTypes             ] = hashISubset
  implicit val hashCustomFieldTypeIM: Hash[CustomFieldType.Implication.type     ] = hashConstClass("IM")
  implicit val hashCustomFieldTypeTA: Hash[CustomFieldType.Tag.type             ] = hashConstClass("TA")
  implicit val hashCustomFieldTypeTX: Hash[CustomFieldType.Text.type            ] = hashConstClass("TX")
  implicit val hashStaticFieldTypeST: Hash[StaticFieldType.StepTree.type        ] = hashConstClass("ST")
  implicit val hashStaticFieldTypeSG: Hash[StaticFieldType.StepGraph.type       ] = hashConstClass("SG")
  implicit val hashStaticFieldTypeIG: Hash[StaticFieldType.ImplicationGraph.type] = hashConstClass("IG")
  implicit val hashCustomFieldType  : Hash[CustomFieldType                      ] = hashADT
  implicit val hashStaticFieldType  : Hash[StaticFieldType                      ] = hashADT
  implicit val hashFieldType        : Hash[FieldType                            ] = hashADT
  implicit val hashCustomFieldIM    : Hash[CustomField.Implication              ] = hashCaseClass
  implicit val hashCustomFieldTA    : Hash[CustomField.Tag                      ] = hashCaseClass
  implicit val hashCustomFieldTX    : Hash[CustomField.Text                     ] = hashCaseClass
  implicit val hashStaticFieldNS    : Hash[StaticField.NormalAltStepTree.type   ] = hashConstClass("NS")
  implicit val hashStaticFieldES    : Hash[StaticField.ExceptionStepTree.type   ] = hashConstClass("ES")
  implicit val hashStaticFieldSG    : Hash[StaticField.StepGraph.type           ] = hashConstClass("SG")
  implicit val hashStaticFieldIG    : Hash[StaticField.ImplicationGraph.type    ] = hashConstClass("IG")
  implicit val hashCustomField      : Hash[CustomField                          ] = hashADT
  implicit val hashStaticFieldUCT   : Hash[StaticField.UseCaseStepTree          ] = hashADT
  implicit val hashStaticField      : Hash[StaticField                          ] = hashADT
  implicit val hashFieldId          : Hash[FieldId                              ] = hashADT
  implicit val hashFieldSet         : Hash[FieldSet                             ] = hashCaseClass

  implicit val hashDeletionReasons: Hash[DeletionReasons] = hashCaseClass

  implicit val hashIdCeilings    : Hash[IdCeilings   ] = hashCaseClass
  implicit val hashProjectConfig : Hash[ProjectConfig] = hashCaseClass

  val hashProjectContent: Hash[Project] =
    hashCaseClassExcept('name, 'config)

  val hashProjectOther: Hash[Project] =
    hashCaseClassExcept(
      // name is included
      'config         ,
      'reqs           ,
      'reqCodes       ,
      'reqText        ,
      'reqTags        ,
      'implications   ,
      'deletionReasons,
      'idCeilings     )

  implicit val hashProject: Hash[Project] =
    Hash.fn[Project](p => joinHashes(
      hashProjectContent.hash(p)        ::
      hashProjectConfig .hash(p.config) ::
      hashProjectOther  .hash(p)        ::
      Nil))
}

final class DataHasherCurrent(protected val algorithm: Hash.Algorithm) extends DataHasher