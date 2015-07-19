package shipreq.webapp.base.protocol

import boopickle._
import japgolly.nyaya.util.{Multimap, MultiValues}
import shipreq.base.util._
import BoopickleMacros._

object BinGenericCodecs extends BasicImplicitPicklers {
  import shipreq.webapp.base.data.DataIdAux

  def taggedL[T <: TaggedTypes.TaggedLong]  (apply: Long   => T) = xmap(apply)(_.value)
  def taggedI[T <: TaggedTypes.TaggedInt]   (apply: Int    => T) = xmap(apply)(_.value)
  def taggedS[T <: TaggedTypes.TaggedString](apply: String => T) = xmap(apply)(_.value)

  def pickleBool[T](iso: IsoBool[T]): Pickler[T] =
    xmap(iso.to)(iso.from)

  implicit def pickleMap[K: Pickler, V: Pickler]: Pickler[Map[K, V]] =
    mapPickler[K, V, Map]

  def pickleIMap[K: UnivEq, V: Pickler](empty: IMap[K, V]): Pickler[IMap[K, V]] =
    xmap(empty ++ (_: Iterable[V]))(_.values)

  @inline def pickleIMapD[K: UnivEq : Pickler, V: Pickler](implicit d: DataIdAux[V, K]): Pickler[IMap[K, V]] =
    pickleIMap(d.emptyIMap)

  implicit def pickleNEV[A](implicit p: Pickler[Vector[A]]): Pickler[NonEmptyVector[A]] =
    p.xmap(l => NonEmptyVector(l.head, l.tail))(_.whole)

  implicit def pickleNES[A: UnivEq](implicit p: Pickler[Set[A]]): Pickler[NonEmptySet[A]] =
    p.xmap(l => NonEmptySet(l.head, l.tail))(_.whole)

  implicit def pickleISubset[A: UnivEq](implicit as: Pickler[NonEmptySet[A]]): Pickler[ISubset[A]] = {
    import ISubset._
    implicit val a: Pickler[All [A]] = pickleCaseClass
    implicit val o: Pickler[Only[A]] = pickleCaseClass
    implicit val n: Pickler[Not [A]] = pickleCaseClass
    pickleADT
  }

  implicit def pickleMultimap[K: UnivEq, L[_], V](implicit p: Pickler[Map[K, L[V]]], l: MultiValues[L]): Pickler[Multimap[K, L, V]] =
    p.xmap(Multimap(_))(_.m)

  implicit def setDiff[A: UnivEq](implicit rw: Pickler[Set[A]]): Pickler[SetDiff[A]] =
    pickleCaseClass

  implicit def pickleTrie[K: Pickler, V: Pickler]: Pickler[MTrie.Trie[K, V]] = {
    import MTrie.{Branch, Node, Trie, Value}
    implicit      val value : Pickler[Value [K, V]] = pickleCaseClass
    implicit      val valueO                        = optionPickler(value)
    implicit lazy val branch: Pickler[Branch[K, V]] = pickleCaseClass
    implicit lazy val node  : Pickler[Node  [K, V]] = pickleADT
    implicit lazy val trie  : Pickler[Trie  [K, V]] = lazily(pickleMap)
    trie
  }
}

// =====================================================================================================================
object BinDataCodecs {
  import shipreq.webapp.base.data._
  import shipreq.webapp.base.text.AtomTC
  import DataImplicits._
  import BinGenericCodecs._

  implicit val live         : Pickler[Live]                = pickleBool(Live)
  implicit val implRequired : Pickler[ImplicationRequired] = pickleBool(ImplicationRequired)
  implicit val mandatory    : Pickler[Mandatory]           = pickleBool(Mandatory)
  implicit val deletable    : Pickler[Deletable]           = pickleBool(Deletable)
  implicit val mutexChildren: Pickler[MutexChildren]       = pickleBool(MutexChildren)

  implicit val rev                      = taggedL(Rev                       )
  implicit val genericReqId             = taggedL(GenericReqId              ).reuseByUnivEq
  implicit val reqCodeId                = taggedL(ReqCodeId                 ).reuseByUnivEq
  implicit val customReqTypeId          = taggedL(CustomReqTypeId           ).reuseByUnivEq
  implicit val customIssueTypeId        = taggedL(CustomIssueTypeId         ).reuseByUnivEq
  implicit val applicableTagId          = taggedL(ApplicableTagId           ).reuseByUnivEq
  implicit val tagGroupId               = taggedL(TagGroupId                ).reuseByUnivEq
  implicit val customFieldTagId         = taggedL(CustomField.Tag.Id        ).reuseByUnivEq
  implicit val customFieldTextId        = taggedL(CustomField.Text.Id       ).reuseByUnivEq
  implicit val customFieldImplicationId = taggedL(CustomField.Implication.Id).reuseByUnivEq
  implicit val reqTypePos               = taggedI(ReqTypePos)
  implicit val hashRefKey               = taggedS(HashRefKey)
  implicit val fieldRefKey              = taggedS(FieldRefKey)
  implicit val reqTypeMnemonic          = taggedS(ReqType.Mnemonic)

  implicit def pickleRevAnd[A: Pickler]: Pickler[RevAnd[A]] = pickleCaseClass

  implicit val reqId: Pickler[ReqId] = pickleADT

  implicit val implications: Pickler[Implications] = pickleCaseClass

  implicit val reqDataTags: Pickler[ReqData.Tags] = pickleMultimap
  implicit val revAndReqDataTags = pickleRevAnd(reqDataTags)

  object AtomPicklers extends AtomTC[Pickler] {
    import shipreq.webapp.base.text._
    import Atom._
    import Text.Equality._

    override def lazily[A](f: => Pickler[A]): Pickler[A] =
      BoopickleMacros.lazily(f)

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

    override def blankLine   [T <: NewLine        ](t: T): Pickler[t.BlankLine   ] = ConstPickler(t.blankLine)
    override def literal     [T <: Literal        ](t: T): Pickler[t.Literal     ] = pickleCaseClass
    override def webAddress  [T <: PlainTextMarkup](t: T): Pickler[t.WebAddress  ] = pickleCaseClass
    override def emailAddress[T <: PlainTextMarkup](t: T): Pickler[t.EmailAddress] = pickleCaseClass
    override def mathTeX     [T <: PlainTextMarkup](t: T): Pickler[t.MathTeX     ] = pickleCaseClass
    override def reqRef      [T <: ReqRef         ](t: T): Pickler[t.ReqRef      ] = pickleCaseClass
    override def codeRef     [T <: ReqRef         ](t: T): Pickler[t.CodeRef     ] = pickleCaseClass
    override def tagRef      [T <: TagRef         ](t: T): Pickler[t.TagRef      ] = pickleCaseClass

    override def issue[T <: Issue](t: T)(implicit h: Pickler[Text.InlineIssueDesc.OptionalText]): Pickler[t.Issue] =
      pickleCaseClass

    override def unorderedList[T <: ListMarkup](t: T)(implicit h: Pickler[NonEmptyVector[t.ListItem]]): Pickler[t.UnorderedList] =
      pickleCaseClass
  }

  import AtomPicklers.instances._

  implicit val pickleReqDataText       : Pickler[ReqData.Text]       = pickleMap
  implicit val pickleReqCodeNode       : Pickler[ReqCode.Node]       = pickleCaseClass // xmap[String] already reuses
  implicit val pickleReqCodeGroup      : Pickler[ReqCodeGroup]       = pickleCaseClass
  implicit val pickleReqCodeTarget     : Pickler[ReqCode.Target]     = pickleADT
  implicit val pickleReqCodeActiveData : Pickler[ReqCode.ActiveData] = pickleCaseClass
  implicit val pickleReqCodeData       : Pickler[ReqCode.Data]       = pickleCaseClass
  implicit val pickleReqCodeTrie       : Pickler[ReqCode.Trie]       = pickleTrie
  implicit val pickleReqCodes          : Pickler[ReqCodes]           = pickleCaseClass

  implicit val pickleStaticReqTypeUC: Pickler[StaticReqType.UseCase.type] = pickleObject
  implicit val pickleStaticReqType  : Pickler[StaticReqType]              = pickleADT
  implicit val pickleReqTypeId      : Pickler[ReqTypeId]                  = pickleADT

  implicit val picklePubidRegister         : Pickler[PubidRegister]  = pickleCaseClass
  implicit val picklePubid                 : Pickler[Pubid]          = pickleCaseClass
  implicit def picklePubidT[T <: ReqTypeId]: Pickler[PubidT[T]]      = picklePubid.asInstanceOf[Pickler[PubidT[T]]]
  implicit val pickleGenericReq            : Pickler[GenericReq]     = pickleCaseClass
  implicit val pickleReq                   : Pickler[Req]            = pickleADT
  implicit val pickleReqsById              : Pickler[GenericReqIMap] = pickleIMapD
  implicit val pickleRequirements          : Pickler[Requirements]   = pickleCaseClass

  implicit val pickleCustomIssueType : Pickler[CustomIssueType]     = pickleCaseClass
  implicit val pickleCustomIssueTypes: Pickler[CustomIssueTypeIMap] = pickleIMapD
  implicit val pickleCustomReqType   : Pickler[CustomReqType]       = pickleCaseClass
  implicit val pickleCustomReqTypes  : Pickler[CustomReqTypeIMap]   = pickleIMapD

  implicit val pickleTagId        : Pickler[TagId]         = pickleADT
  implicit val pickleApplicableTag: Pickler[ApplicableTag] = pickleCaseClass
  implicit val pickleTagGroup     : Pickler[TagGroup]      = pickleCaseClass
  implicit val pickleTag          : Pickler[Tag]           = pickleADT
  implicit val pickleTagInTree    : Pickler[TagInTree]     = pickleCaseClass
  implicit val pickleTagTree      : Pickler[TagTree]       = pickleIMap(TagTree.empty)

  implicit val pickleApplReqTypes     : Pickler[Field.ApplicableReqTypes]           = pickleISubset
  implicit val pickleCustomFieldTypeIM: Pickler[CustomFieldType.Implication.type]   = pickleObject
  implicit val pickleCustomFieldTypeTA: Pickler[CustomFieldType.Tag.type]           = pickleObject
  implicit val pickleCustomFieldTypeTX: Pickler[CustomFieldType.Text.type]          = pickleObject
  implicit val pickleStaticFieldTypeSG: Pickler[StaticFieldType.StepGraph.type]     = pickleObject
  implicit val pickleStaticFieldTypeST: Pickler[StaticFieldType.StepTree.type]      = pickleObject
  implicit val pickleCustomFieldType  : Pickler[CustomFieldType]                    = pickleADT
  implicit val pickleStaticFieldType  : Pickler[StaticFieldType]                    = pickleADT
  implicit val pickleFieldType        : Pickler[FieldType]                          = pickleADT
  implicit val pickleCustomFieldIM    : Pickler[CustomField.Implication]            = pickleCaseClass
  implicit val pickleCustomFieldTA    : Pickler[CustomField.Tag]                    = pickleCaseClass
  implicit val pickleCustomFieldTX    : Pickler[CustomField.Text]                   = pickleCaseClass
  implicit val pickleStaticFieldSG    : Pickler[StaticField.StepGraph.type]         = pickleObject
  implicit val pickleStaticFieldNS    : Pickler[StaticField.NormalAltStepTree.type] = pickleObject
  implicit val pickleStaticFieldES    : Pickler[StaticField.ExceptionStepTree.type] = pickleObject
  implicit val pickleStaticField      : Pickler[StaticField]                        = pickleADT
  implicit val pickleCustomFieldId    : Pickler[CustomFieldId]                      = pickleADT
  implicit val pickleCustomField      : Pickler[CustomField]                        = pickleADT
  implicit val pickleFieldId          : Pickler[FieldId]                            = pickleADT
  implicit val pickleCustomFields     : Pickler[FieldSet.CustomFields]              = pickleIMap(FieldSet.emptyCustomFields)
  implicit val pickleFieldSet         : Pickler[FieldSet]                           = pickleCaseClass

  implicit val pickleProjectConfig: Pickler[ProjectConfig] = pickleCaseClass
  implicit val pickleProject      : Pickler[Project]       = pickleCaseClass
}
