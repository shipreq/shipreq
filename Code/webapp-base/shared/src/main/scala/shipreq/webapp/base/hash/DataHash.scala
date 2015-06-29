package shipreq.webapp.base.hash

import japgolly.nyaya.util.Multimap
import scalaz.\/
import shipreq.base.util.TaggedTypes.TaggedType
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Atom, Text}
import Hash.HashableValueOps

abstract class GenericDashHash {
  protected val algorithm: Hash.Algorithm
  import algorithm._

  implicit class Hash_StringExt(val str: String) { // TODO extends AnyVal
    def @@[A](h: Hash[A]): Hash[A] = {
      val nameHash = hashString.hash(str) :: Nil
      Hash.fn[A](a => joinHashes(h.hash(a) :: nameHash))
    }
  }

  def hashTaggedType[T <: TaggedType](implicit h: Hash[T#U]): Hash[T] =
    h.cmap(_.value)

  implicit def hashMultimap[K, L[_], V](implicit h: Hash[Map[K, L[V]]]): Hash[Multimap[K, L, V]] =
    h.cmap(_.m)

  private val hashNone = "∅".hash

  implicit def hashOption[A: Hash]: Hash[Option[A]] = {
    val some = "!" @@ Hash[A]
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

  implicit def hashIMap[K, V: Hash]: Hash[IMap[K, V]] =
    hashUnordered[Iterable, V].cmap(_.values)

  implicit def hashIMapK[T, K[+_ <: T], V[+_ <: T]](implicit h: Hash[V[T]]): Hash[IMapK[T, K, V]] =
    hashUnordered[Iterable, V[T]].cmap(_.values)

  implicit def disjunction[A: Hash, B: Hash]: Hash[A \/ B] = {
    val l = "!" @@ Hash[A]
    val r = Hash[B]
    Hash.fn(_.fold(l.hash, r.hash))
  }

  def hashISubset[A: Hash]: Hash[ISubset[A]] = {
    import ISubset._
    implicit val anes = hashNES[A]
    implicit val all : Hash[All [A]] = hashConstClass("Al")
    implicit val only: Hash[Only[A]] = "On" @@ hashCaseClass
    implicit val not : Hash[Not [A]] = "No" @@ hashCaseClass
    // TODO hashADT can't handle this ↓
    Hash.fn {
      case a: All [A] => all  hash a
      case a: Only[A] => only hash a
      case a: Not [A] => not  hash a
    }
  }

  implicit def hashRevAnd[A](implicit hr: Hash[Rev], ha: Hash[A]): Hash[RevAnd[A]] = hashCaseClass
}

final class DataHash(protected val algorithm: Hash.Algorithm) extends GenericDashHash {
  import algorithm._

  implicit val hashLive         : Hash[Live]                = Hash by Live.from
  implicit val hashImplRequired : Hash[ImplicationRequired] = Hash by ImplicationRequired.from
  implicit val hashMandatory    : Hash[Mandatory]           = Hash by Mandatory.from
  implicit val hashDeletable    : Hash[Deletable]           = Hash by Deletable.from
  implicit val hashMutexChildren: Hash[MutexChildren]       = Hash by MutexChildren.from

  implicit val hashRev                      = hashTaggedType[Rev]
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

  implicit val hashImplications: Hash[Implications] = hashCaseClass

  implicit val hashReqDataTags: Hash[ReqData.Tags] = hashMultimap

  object HashAtoms {
    @inline implicit def hashTextNE[T <: Text.Generic]: Hash[T#NonEmptyText] = hashTextImplNE.narrow
    @inline implicit def hashTextO[T <: Text.Generic] : Hash[T#OptionalText] = hashTextImplO.narrow

    implicit      val hashAtomBlankLine   : Hash[Atom.NewLine        #BlankLine    ] = hashConstClass("BL")
    implicit      val hashAtomLiteral     : Hash[Atom.Literal        #Literal      ] = "LI" @@ hashCaseClass
    implicit      val hashAtomReqRef      : Hash[Atom.ReqRef         #ReqRef       ] = "RR" @@ hashCaseClass
    implicit      val hashAtomCodeRef     : Hash[Atom.ReqRef         #CodeRef      ] = "CR" @@ hashCaseClass
    implicit      val hashAtomWebAddress  : Hash[Atom.PlainTextMarkup#WebAddress   ] = "WA" @@ hashCaseClass
    implicit      val hashAtomEmailAddress: Hash[Atom.PlainTextMarkup#EmailAddress ] = "EA" @@ hashCaseClass
    implicit      val hashAtomMathTeX     : Hash[Atom.PlainTextMarkup#MathTeX      ] = "MX" @@ hashCaseClass
    implicit      val hashAtomTagRef      : Hash[Atom.TagRef         #TagRef       ] = "TR" @@ hashCaseClass
    implicit lazy val hashAtomIssue       : Hash[Atom.Issue          #Issue        ] = "IS" @@ hashCaseClass
    implicit lazy val hashAtomUL          : Hash[Atom.ListMarkup     #UnorderedList] = "UL" @@ hashCaseClass

    lazy val hashAtomImpl: Hash[Atom.AnyAtom] = {
      import Atom._
      Hash.fn {
        case a: NewLine         # BlankLine     => a.hash
        case a: Literal         # Literal       => a.hash
        case a: ReqRef          # ReqRef        => a.hash
        case a: ReqRef          # CodeRef       => a.hash
        case a: Issue           # Issue         => a.hash
        case a: PlainTextMarkup # WebAddress    => a.hash
        case a: PlainTextMarkup # EmailAddress  => a.hash
        case a: PlainTextMarkup # MathTeX       => a.hash
        case a: TagRef          # TagRef        => a.hash
        case a: ListMarkup      # UnorderedList => a.hash
      }
    }

    @inline implicit def hashAtom[T <: Atom.Base]: Hash[T#Atom] = hashAtomImpl.narrow

    lazy val hashTextImplNE = hashNEV(hashAtomImpl)
    lazy val hashTextImplO  = hashVector(hashAtomImpl)
  }

  private val hashTextImplNE = HashAtoms.hashTextImplNE
  private val hashTextImplO  = HashAtoms.hashTextImplO
  @inline implicit def hashTextNE[T <: Text.Generic]: Hash[T#NonEmptyText] = hashTextImplNE.narrow
  @inline implicit def hashTextO [T <: Text.Generic]: Hash[T#OptionalText] = hashTextImplO.narrow

  implicit val hashReqDataText       : Hash[ReqData.Text]       = hashMap
  implicit val hashReqCodeNode       : Hash[ReqCode.Node]       = hashCaseClass
  implicit val hashReqCodeGroup      : Hash[ReqCodeGroup]       = hashCaseClass
  implicit val hashReqCodeTarget     : Hash[ReqCode.Target]     = hashADT
  implicit val hashReqCodeActiveData : Hash[ReqCode.ActiveData] = hashCaseClass
  implicit val hashReqCodeData       : Hash[ReqCode.Data]       = hashCaseClass
  implicit val hashReqCodeTrie       : Hash[ReqCode.Trie]       = hashTrie
  implicit val hashReqCodes          : Hash[ReqCodes]           = hashCaseClass

  implicit val hashStaticReqTypeUC: Hash[StaticReqType.UseCase.type] = hashConstClass("UC")
  implicit val hashStaticReqType  : Hash[StaticReqType]              = hashADT
  implicit val hashReqTypeId      : Hash[ReqTypeId]                  = hashADT

  implicit val hashPubidRegister         : Hash[PubidRegister]     = hashCaseClass
  implicit val hashPubid                 : Hash[Pubid]             = hashCaseClass
  implicit def hashPubidT[T <: ReqTypeId]: Hash[PubidT[T]]         = hashPubid.narrow
  implicit val hashGenericReq            : Hash[GenericReq]        = hashCaseClass
  implicit val hashReq                   : Hash[Req]               = hashADT
  implicit val hashReqsById              : Hash[Requirements.ById] = hashIMapK[ReqTypeId, ReqIdT, ReqT]
  implicit val hashRequirements          : Hash[Requirements]      = hashCaseClass

  implicit val hashCustomIssueType : Hash[CustomIssueType]     = hashCaseClass
  implicit val hashCustomIssueTypes: Hash[CustomIssueTypeIMap] = hashIMap
  implicit val hashCustomReqType   : Hash[CustomReqType]       = hashCaseClass
  implicit val hashCustomReqTypes  : Hash[CustomReqTypeIMap]   = hashIMap

  implicit val hashTagId        : Hash[TagId]         = hashADT
  implicit val hashApplicableTag: Hash[ApplicableTag] = hashCaseClass
  implicit val hashTagGroup     : Hash[TagGroup]      = hashCaseClass
  implicit val hashTag          : Hash[Tag]           = hashADT
  implicit val hashTagInTree    : Hash[TagInTree]     = hashCaseClass
  implicit val hashTagTree      : Hash[TagTree]       = hashIMap

  implicit val hashApplReqTypes     : Hash[Field.ApplicableReqTypes]           = hashISubset
  implicit val hashCustomFieldTypeIM: Hash[CustomFieldType.Implication.type]   = hashConstClass("IM")
  implicit val hashCustomFieldTypeTA: Hash[CustomFieldType.Tag.type]           = hashConstClass("TA")
  implicit val hashCustomFieldTypeTX: Hash[CustomFieldType.Text.type]          = hashConstClass("TX")
  implicit val hashStaticFieldTypeSG: Hash[StaticFieldType.StepGraph.type]     = hashConstClass("SG")
  implicit val hashStaticFieldTypeST: Hash[StaticFieldType.StepTree.type]      = hashConstClass("ST")
  implicit val hashCustomFieldType  : Hash[CustomFieldType]                    = hashADT
  implicit val hashStaticFieldType  : Hash[StaticFieldType]                    = hashADT
  implicit val hashFieldType        : Hash[FieldType]                          = hashADT
  implicit val hashCustomFieldIM    : Hash[CustomField.Implication]            = hashCaseClass
  implicit val hashCustomFieldTA    : Hash[CustomField.Tag]                    = hashCaseClass
  implicit val hashCustomFieldTX    : Hash[CustomField.Text]                   = hashCaseClass
  implicit val hashStaticFieldSG    : Hash[StaticField.StepGraph.type]         = hashConstClass("SG")
  implicit val hashStaticFieldNS    : Hash[StaticField.NormalAltStepTree.type] = hashConstClass("NS")
  implicit val hashStaticFieldES    : Hash[StaticField.ExceptionStepTree.type] = hashConstClass("ES")
  implicit val hashCustomField      : Hash[CustomField]                        = hashADT
  implicit val hashStaticField      : Hash[StaticField]                        = hashADT
  implicit val hashFieldId          : Hash[FieldId]                            = hashADT
  implicit val hashFieldSet         : Hash[FieldSet]                           = hashCaseClass

  implicit val hashProjectConfig: Hash[ProjectConfig] = hashCaseClass
  implicit val hashProject      : Hash[Project]       = hashCaseClass
}