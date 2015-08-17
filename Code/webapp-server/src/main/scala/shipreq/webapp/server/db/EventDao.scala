package shipreq.webapp.server.db

import scala.annotation.tailrec
import shipreq.webapp.base.event._
import shipreq.webapp.base.hash.{ProjectHash, HashScheme}
import shipreq.webapp.server.lib.Types.ProjectId
import EventDao.EventSeq

object EventDbCodecs {
  import japgolly.nyaya.util.{Multimap, MultiValues}
  import upickle._
  import upickle.Fns._
  import upickle.BaseCodecs.StringRW
  import shipreq.base.util._
  import shipreq.webapp.base.data._
  import shipreq.webapp.base.protocol.MPickleMacros._
  import shipreq.webapp.base.text.AtomTC
  import shipreq.webapp.base.util.GenericDataMacros._
  import EventDbMacros._

  private[this] val jsNum0 = Js.Num(0)
  private[this] val jsNum1 = Js.Num(1)

  implicit val pickleInt: ReadWriter[Int] =
    ReadWriter(i => Js.Num(i), { case Js.Num(i) => i.toInt })

  implicit class ReadWriterExt[A](private val rw: ReadWriter[A]) extends AnyVal {
    import StdlibCodecs.All._
    private implicit def _rw = rw

    def set(implicit ev: UnivEq[A]): ReadWriter[Set[A]] =
      ReadWriter.merge(SeqishR[A, Set], SeqishW[A, Set])

    def vector: ReadWriter[Vector[A]] =
      ReadWriter.merge(SeqishR[A, Vector], SeqishW[A, Vector])

    def nev: ReadWriter[NonEmptyVector[A]] =
      pickleNEV(vector)

    def nes(implicit ev: UnivEq[A]): ReadWriter[NonEmptySet[A]] =
      pickleNES(ev, set)
  }

  private def boolCase[T](iso: IsoBool[T]): ReadWriter[T] =
    ReadWriter(
      t => if (t :: iso) jsNum1 else jsNum0,
      { case Js.Num(n) => iso <~ (n.toInt != 0) })

  def addOptionWithNoneAs0[A](rw: ReadWriter[A]): ReadWriter[Option[A]] =
    ReadWriter({
      case None    => jsNum0
      case Some(t) => rw write t
    }, {
      case `jsNum0` => None
      case j        => Some(readJs(j)(rw))
    })

  def setDiff[A: UnivEq](implicit rw: ReadWriter[A]): ReadWriter[SetDiff[A]] = {
    val rws = rw.set
    ReadWriter(sd => {
      var kvs: List[(String, Js.Value)] = Nil
      if (sd.added.nonEmpty)
        kvs ::= ("+" -> rws.write(sd.added))
      if (sd.removed.nonEmpty)
        kvs ::= ("-" -> rws.write(sd.removed))
      Js.Obj(kvs: _*)
    }, {
      case o: Js.Obj =>
        var del = Set.empty[A]
        var add = Set.empty[A]
        o.value.foreach(kv => kv._1 match {
          case "-" => del = rws.read(kv._2)
          case "+" => add = rws.read(kv._2)
        })
        SetDiff(removed = del, add)
    })
  }

  def pickleNonEmpty[N, E](f: N => E)(implicit rw: ReadWriter[E], proof: NonEmpty.Proof[E, N]): ReadWriter[N] =
    ReadWriter.xmapf(f)(NonEmpty require_! _)

  def pickleNonEmptyA[A](implicit rw: ReadWriter[A], proof: NonEmpty.ProofA[A]): ReadWriter[NonEmpty[A]] =
    pickleNonEmpty(_.value)

  def pickleNEV[A](implicit rw: ReadWriter[Vector[A]]): ReadWriter[NonEmptyVector[A]] =
    pickleNonEmpty(_.whole)

  def pickleNES[A: UnivEq](implicit rw: ReadWriter[Set[A]]): ReadWriter[NonEmptySet[A]] =
    pickleNonEmpty(_.whole)

  def pickleNESD[A: UnivEq](implicit rw: ReadWriter[A]): ReadWriter[Event.NESD[A]] = {
    implicit val sd = setDiff[A]
    pickleNonEmptyA
  }

  implicit val pickleLive          = boolCase(Live)
  implicit val pickleImplRequired  = boolCase(ImplicationRequired)
  implicit val pickleMandatory     = boolCase(Mandatory)
  implicit val pickleDeletable     = boolCase(Deletable)
  implicit val pickleMutexChildren = boolCase(MutexChildren)

  implicit val pickleGenericReqId            : ReadWriter[GenericReqId              ] = caseClass
  implicit val pickleReqCodeId               : ReadWriter[ReqCodeId                 ] = caseClass
  implicit val pickleCustomReqTypeId         : ReadWriter[CustomReqTypeId           ] = caseClass
  implicit val pickleCustomIssueTypeId       : ReadWriter[CustomIssueTypeId         ] = caseClass
  implicit val pickleApplicableTagId         : ReadWriter[ApplicableTagId           ] = caseClass
  implicit val pickleTagGroupId              : ReadWriter[TagGroupId                ] = caseClass
  implicit val pickleCustomFieldTagId        : ReadWriter[CustomField.Tag.Id        ] = caseClass
  implicit val pickleCustomFieldTextId       : ReadWriter[CustomField.Text.Id       ] = caseClass
  implicit val pickleCustomFieldImplicationId: ReadWriter[CustomField.Implication.Id] = caseClass
  implicit val pickleReqTypePos              : ReadWriter[ReqTypePos                ] = caseClass
  implicit val pickleHashRefKey              : ReadWriter[HashRefKey                ] = caseClass
  implicit val pickleFieldRefKey             : ReadWriter[FieldRefKey               ] = caseClass
  implicit val pickleReqTypeMnemonic         : ReadWriter[ReqType.Mnemonic          ] = caseClass

  implicit val pickleReqId: ReadWriter[ReqId] = pickleAdtOS {
    case _: GenericReqId => ""
  }

  implicit val pickleReqIdNESD = pickleNESD[ReqId]

  implicit val pickleReqCodeIdSet: ReadWriter[Set[ReqCodeId]] =
    pickleReqCodeId.set

  implicit val pickleFieldId: ReadWriter[FieldId] = pickleAdtOS {
    case _: CustomField.Text       .Id => "x"
    case _: CustomField.Tag        .Id => "t"
    case _: CustomField.Implication.Id => "i"
    case StaticField.NormalAltStepTree => "n"
    case StaticField.ExceptionStepTree => "e"
    case StaticField.StepGraph         => "g"
  }

  implicit val pickleFieldIdPosition: ReadWriter[Position[FieldId]] =
    addOptionWithNoneAs0(pickleFieldId)

  implicit val pickleTagId: ReadWriter[TagId] = pickleAdtOS[TagId] {
    case _: ApplicableTagId => ""
    case _: TagGroupId      => "g"
  }

  implicit val pickleApplicableTagIdNESD = pickleNESD[ApplicableTagId]

  implicit val pickleTagPosition: ReadWriter[Position[TagId]] =
    addOptionWithNoneAs0(pickleTagId)

  implicit val pickleTagTreeParents: ReadWriter[TagInTree.Parents] = {
    val w: Writer[TagInTree.Parents] = StdlibCodecs.All.MapW
    val r: Reader[TagInTree.Parents] = StdlibCodecs.All.MapR
    ReadWriter.merge(r, w)
  }

  implicit val pickleTagTreeChildren: ReadWriter[TagInTree.Children] =
    pickleTagId.vector

  private val reqCodeValueToString: ReqCode.Value => String =
    ReqCode.valueToStr(_, '.')

  private val reqCodeValueFromString: String => ReqCode.Value =
    s => {
      var found = Vector.empty[ReqCode.Node]
      @tailrec def go(rem: String): ReqCode.Value = {
        val i = rem.indexOf('.')
        if (i == -1)
          NonEmptyVector.force(found :+ ReqCode.Node(rem))
        else {
          val h = rem.substring(0, i)
          found :+= ReqCode.Node(h)
          go(rem.substring(i + 1))
        }
      }
      go(s)
    }

  implicit val pickleReqCodeValue: ReadWriter[ReqCode.Value] =
    ReadWriter.xmap(reqCodeValueFromString)(reqCodeValueToString)

  implicit val pickleStaticReqType: ReadWriter[StaticReqType] = pickleAdtN {
    case StaticReqType.UseCase => -1
  }

  implicit val pickleReqTypeId: ReadWriter[ReqTypeId] =
    ReadWriter({
      case i: CustomReqTypeId => pickleCustomReqTypeId write i
      case s: StaticReqType   => pickleStaticReqType   write s
    }, {
      case j@ Js.Num(n) =>
        if (n > 0) pickleCustomReqTypeId read j
        else       pickleStaticReqType   read j
    })


  implicit def pickleISubset[A: UnivEq : ReadWriter]: ReadWriter[ISubset[A]] = {
    import ISubset._
    implicit val as = implicitly[ReadWriter[A]].nes
    implicit val o: ReadWriter[Only[A]] = caseClass
    implicit val n: ReadWriter[Not [A]] = caseClass
    pickleAdtOS {
      case _: All [A] => "*"
      case _: Only[A] => "+"
      case _: Not [A] => "-"
    }
  }

  implicit val pickleApplicableReqTypes: ReadWriter[Field.ApplicableReqTypes] =
    pickleISubset

  // TODO Macros can improve this - sum shouldn't use trail-n-error, leaves shouldn't need to identify themselves
  object TextCodecs extends AtomTC[ReadWriter] {
    import shipreq.webapp.base.text._
    import Atom._

    private def strkeyW[A](k: String, a: A)(implicit A: Writer[A]) =
      Js.Arr(Js.Str(k), A write a)

    private def strkeyW2[A, B](k: String, a: A, b: B)(implicit A: Writer[A], B: Writer[B]) =
      Js.Arr(Js.Str(k), A write a, B write b)

    override def lazily[A](a: => ReadWriter[A]): ReadWriter[A] = {
      lazy val b = a
      ReadWriter(a => b write a, { case j => b read j })
    }

    override def vec[A](implicit a: ReadWriter[A]) =
      a.vector

    override def nev[A](as: ReadWriter[Vector[A]])(implicit a: ReadWriter[A]) =
      pickleNEV(as)

    private[this] final val BLANKLINE = 0
    private[this] final val WEBADD    = "/"
    private[this] final val EMAILADD  = "@"
    private[this] final val MATHTEX   = "="
    private[this] final val UL        = "*"
    private[this] final val ISSUE     = "i"
    private[this] final val REQREF    = "r"
    private[this] final val CODEREF   = "c"
    private[this] final val TAGREF    = "t"

    override def sum[T <: Atom.Base](t: T)(f: t.Atom => ReadWriter[t.Atom], i: t.Atom => Int, all: Vector[ReadWriter[t.Atom]]): ReadWriter[t.Atom] = {
      val readPF = all.map(_.read).reduce(_ orElse _)
      ReadWriter[t.Atom](a => f(a).write(a), readPF)
    }

    override def blankLine[T <: NewLine](t: T): ReadWriter[t.BlankLine] = ReadWriter(
      a => Js.Num(BLANKLINE),
      { case Js.Num(n) if n.toInt == 0 => t.blankLine })

    override def literal[T <: Literal](t: T): ReadWriter[t.Literal] = ReadWriter(
      a => Js.Str(a.value),
      { case Js.Str(s) => t.Literal(s) })

    override def webAddress[T <: PlainTextMarkup](t: T): ReadWriter[t.WebAddress] = ReadWriter(
      a => strkeyW(WEBADD, a.value),
      { case Js.Arr(Js.Str(WEBADD), v) => t.WebAddress(readJs[String](v)) })

    override def emailAddress[T <: PlainTextMarkup](t: T): ReadWriter[t.EmailAddress] = ReadWriter(
      a => strkeyW(EMAILADD, a.value),
      { case Js.Arr(Js.Str(EMAILADD), v) => t.EmailAddress(readJs[String](v)) })

    override def mathTeX[T <: PlainTextMarkup](t: T): ReadWriter[t.MathTeX] = ReadWriter(
      a => strkeyW(MATHTEX, a.value),
      { case Js.Arr(Js.Str(MATHTEX), v) => t.MathTeX(readJs[String](v)) })

    override def reqRef[T <: ReqRef](t: T): ReadWriter[t.ReqRef] = ReadWriter(
      a => strkeyW(REQREF, a.value),
      { case Js.Arr(Js.Str(REQREF), v) => t.ReqRef(readJs[ReqId](v)) })

    override def codeRef[T <: ReqRef](t: T): ReadWriter[t.CodeRef] = ReadWriter(
      a => strkeyW(CODEREF, a.value),
      { case Js.Arr(Js.Str(CODEREF), v) => t.CodeRef(readJs[ReqCodeId](v)) })

    override def tagRef[T <: TagRef](t: T): ReadWriter[t.TagRef] = ReadWriter(
      a => strkeyW(TAGREF, a.value),
      { case Js.Arr(Js.Str(TAGREF), v) => t.TagRef(readJs[ApplicableTagId](v)) })

    override def issue[T <: Issue](t: T)(implicit s: ReadWriter[Text.InlineIssueDesc.OptionalText]): ReadWriter[t.Issue] = ReadWriter(
      a => strkeyW2(ISSUE, a.typ, a.desc),
      { case Js.Arr(Js.Str(ISSUE), a, b) => t.Issue(readJs[CustomIssueTypeId](a), readJs[Text.InlineIssueDesc.OptionalText](b)) })

    override def unorderedList[T <: ListMarkup](t: T)(implicit s: ReadWriter[NonEmptyVector[t.ListItem]]): ReadWriter[t.UnorderedList] = ReadWriter(
      a => strkeyW(UL, a.items),
      { case Js.Arr(Js.Str(UL), v) => t.UnorderedList(readJs(v)(s)) })
  }
  import TextCodecs.instances._

  implicit val pickleOptionString: ReadWriter[Option[String]] =
    ReadWriter.xmap((s: String) => if (s.isEmpty) None else Some(s))(_ getOrElse "")

  implicit val pickleApplicableTagIdNES: ReadWriter[NonEmptySet[ApplicableTagId]] =
    pickleApplicableTagId.nes

  implicit val pickleReqIdNES: ReadWriter[NonEmptySet[ReqId]] =
    pickleReqId.nes

  implicit val pickleDeletionAction: ReadWriter[DeletionAction] = pickleAdtOS {
    case SoftDel => "s"
    case Restore => "r"
    case HardDel => "h"
  }

  implicit val pickleSoftDeletionAction: ReadWriter[SoftDeletionAction] = pickleAdtOS {
    case SoftDel => "s"
    case Restore => "r"
  }

  /**
   * Serialises into an object of {"codeₙ":idₙ}.
   *
   * This is not implicit because there are cases where the same value can be associated with multiple ids.
   * If this representation is used in such a case then there will be duplicate object keys.
   */
  private val pickleReqCodeIdAndValueNES: ReadWriter[NonEmptySet[ReqCode.IdAndValue]] =
    ReadWriter(
      nes => {
        val kvs = nes.foldLeft(List.empty[(String, Js.Value)])((q, iv) =>
            (reqCodeValueToString(iv.value), Js.Num(iv.id.value)) :: q)
        Js.Obj(kvs: _*)
      }, {
        case o: Js.Obj =>
          val cs = o.value.foldLeft(Set.empty[ReqCode.IdAndValue])((q, kv) => {
              val Js.Num(n) = kv._2
              val c = reqCodeValueFromString(kv._1)
              q + ReqCode.IdAndValue(ReqCodeId(n.toInt), c)
            })
        NonEmptySet.force(cs)
      }
    )

  implicit val pickleCustomIssueTypeGD = gdMPickler(CustomIssueTypeGD, true) {
    case CustomIssueTypeGD.Key  => "k"
    case CustomIssueTypeGD.Desc => "d"
  } nev

  implicit val pickleCustomReqTypeGD = gdMPickler(CustomReqTypeGD, true) {
    case CustomReqTypeGD.Name     => "n"
    case CustomReqTypeGD.Mnemonic => "m"
    case CustomReqTypeGD.Imp      => "i"
  } nev

  implicit val pickleTagGroupGD = gdMPickler(TagGroupGD, true) {
    case TagGroupGD.Name          => "n"
    case TagGroupGD.Desc          => "d"
    case TagGroupGD.MutexChildren => "m"
    case TagGroupGD.Parents       => "p"
    case TagGroupGD.Children      => "c"
  } nev

  implicit val pickleApplicableTagGD = gdMPickler(ApplicableTagGD, true) {
    case ApplicableTagGD.Name     => "n"
    case ApplicableTagGD.Desc     => "d"
    case ApplicableTagGD.Key      => "k"
    case ApplicableTagGD.Parents  => "p"
    case ApplicableTagGD.Children => "c"
  } nev

  implicit val pickleCustomTextFieldGD = gdMPickler(CustomTextFieldGD, true) {
    case CustomTextFieldGD.Name      => "n"
    case CustomTextFieldGD.Key       => "k"
    case CustomTextFieldGD.Mandatory => "m"
    case CustomTextFieldGD.ReqTypes  => "a"
  } nev

  implicit val pickleCustomTagFieldGD = gdMPickler(CustomTagFieldGD, true) {
    case CustomTagFieldGD.TagId     => "t"
    case CustomTagFieldGD.Mandatory => "m"
    case CustomTagFieldGD.ReqTypes  => "a"
  } nev

  implicit val pickleCustomImpFieldGD = gdMPickler(CustomImpFieldGD, true) {
    case CustomImpFieldGD.ReqTypeId => "r"
    case CustomImpFieldGD.Mandatory => "m"
    case CustomImpFieldGD.ReqTypes  => "a"
  } nev

  implicit val pickleCreateGenericReqGD = {
    implicit val x = pickleReqCodeIdAndValueNES
    // Using "r" for reqtype
    gdMPickler(CreateGenericReqGD, false) {
      case CreateGenericReqGD.Title    => "t"
      case CreateGenericReqGD.ReqCodes => "c"
      case CreateGenericReqGD.Tags     => "#"
      case CreateGenericReqGD.ImpSrcs  => ">"
      case CreateGenericReqGD.ImpTgts  => "<"
    }
  } values

  implicit val pickleReqCodeGroupGD = gdMPickler(ReqCodeGroupGD, true) {
    case ReqCodeGroupGD.Code  => "c"
    case ReqCodeGroupGD.Title => "t"
  } nev

  implicit val pickleReqCodeValueToIds: ReadWriter[Multimap[ReqCode.Value, Set, ReqCodeId]] = {
    val empty = UnivEq.emptySetMultimap[ReqCode.Value, ReqCodeId]
    ReadWriter(mm => {
      var o: List[(String, Js.Value)] = Nil
      mm.m.foreach { kv =>
        val c = reqCodeValueToString(kv._1)
        val ids = kv._2
        val v =
          if (ids.tail.isEmpty)
            pickleReqCodeId write ids.head
          else {
            val vs = ids.foldLeft[List[Js.Value]](Nil)((q, id) => pickleReqCodeId.write(id) :: q)
            Js.Arr(vs: _*)
          }
        o ::= (c, v)
      }
      Js.Obj(o: _*)
    }, {
      case o: Js.Obj =>
        o.value.foldLeft(empty)((q, kv) => {
          val c   = reqCodeValueFromString(kv._1)
          val ids = kv._2 match {
            case a: Js.Arr => a.value.foldLeft(Set.empty[ReqCodeId])(_ + pickleReqCodeId.read(_))
            case x         => Set(pickleReqCodeId read x)
          }
          q.setvs(c, ids)
        })
    })
  }

  implicit val pickleProjectTemplate: ReadWriter[ProjectTemplate] = pickleAdtN {
    case ProjectTemplate.Default => 1
  }

  implicit val idTypeReqCodeId                = DbCodec.monoId(noDataIdType, ReqCodeId)
  implicit val idTypeCustomReqTypeId          = DbCodec.monoId('r', CustomReqTypeId)
  implicit val idTypeCustomIssueTypeId        = DbCodec.monoId('i', CustomIssueTypeId)

  implicit val idTypeGenericReqId             = DbCodec.monoId('g', GenericReqId)
  implicit val idTypeReqId                    = DbCodec.polyId[ReqId]

  implicit val idTypeApplicableTagId          = DbCodec.monoId('a', ApplicableTagId)
  implicit val idTypeTagGroupId               = DbCodec.monoId('g', TagGroupId)
  implicit val idTypeTagId                    = DbCodec.polyId[TagId]

  implicit val idTypeCustomFieldTextId        = DbCodec.monoId('x', CustomField.Text.Id)
  implicit val idTypeCustomFieldTagId         = DbCodec.monoId('t', CustomField.Tag.Id)
  implicit val idTypeCustomFieldImplicationId = DbCodec.monoId('i', CustomField.Implication.Id)
  implicit val idTypeCustomFieldId            = DbCodec.polyId[CustomFieldId]

  implicit val idTypeStaticField: DbCodec.MonoId[StaticField] =
    new DbCodec.MonoId[StaticField]('s', {
        case StaticField.NormalAltStepTree => -1
        case StaticField.ExceptionStepTree => -2
        case StaticField.StepGraph         => -3
      }, _.intValue match {
        case -1 => StaticField.NormalAltStepTree
        case -2 => StaticField.ExceptionStepTree
        case -3 => StaticField.StepGraph
      })

  implicit val idTypeFieldId: DbCodec.PolyId[FieldId] =
    new DbCodec.PolyId[FieldId]({
        case i: CustomFieldId => idTypeCustomFieldId bytePG i
        case _: StaticField   => idTypeStaticField.bytePG
      }, {
        case i: CustomFieldId => i.value
        case f: StaticField   => idTypeStaticField integer f
      }, {
        case 's' => idTypeStaticField.make
        case b   => idTypeCustomFieldId make b
      })

  implicit val dbCodecAddStaticField       : DbCodec[AddStaticField]        = dbCodecIdOnly
  implicit val dbCodecApplyTemplate        : DbCodec[ApplyTemplate]         = dbCodecDataOnly
  implicit val dbCodecCreateApplicableTag  : DbCodec[CreateApplicableTag]   = dbCodec2
  implicit val dbCodecCreateCustomImpField : DbCodec[CreateCustomImpField]  = dbCodec2
  implicit val dbCodecCreateCustomIssueType: DbCodec[CreateCustomIssueType] = dbCodec2
  implicit val dbCodecCreateCustomReqType  : DbCodec[CreateCustomReqType]   = dbCodec2
  implicit val dbCodecCreateCustomTagField : DbCodec[CreateCustomTagField]  = dbCodec2
  implicit val dbCodecCreateCustomTextField: DbCodec[CreateCustomTextField] = dbCodec2
  implicit val dbCodecCreateGenericReq     : DbCodec[CreateGenericReq]      = dbCodecIdGdAnd('vs, 'rt -> "r")
  implicit val dbCodecCreateReqCodeGroup   : DbCodec[CreateReqCodeGroup]    = dbCodec2
  implicit val dbCodecCreateTagGroup       : DbCodec[CreateTagGroup]        = dbCodec2
  implicit val dbCodecDeleteCustomField    : DbCodec[DeleteCustomField]     = dbCodec2
  implicit val dbCodecDeleteCustomIssueType: DbCodec[DeleteCustomIssueType] = dbCodec2
  implicit val dbCodecDeleteCustomReqType  : DbCodec[DeleteCustomReqType]   = dbCodec2
  implicit val dbCodecDeleteReqCodeGroup   : DbCodec[DeleteReqCodeGroup]    = dbCodecIdOnly
  implicit val dbCodecDeleteReq            : DbCodec[DeleteReq]             = dbCodec2
  implicit val dbCodecDeleteStaticField    : DbCodec[DeleteStaticField]     = dbCodecIdOnly
  implicit val dbCodecDeleteTagGroup       : DbCodec[DeleteTag]             = dbCodec2
  implicit val dbCodecPatchImplicationSrc  : DbCodec[PatchImplicationSrc]   = dbCodec2
  implicit val dbCodecPatchImplicationTgt  : DbCodec[PatchImplicationTgt]   = dbCodec2
  implicit val dbCodecPatchReqCodes        : DbCodec[PatchReqCodes]         = dbCodecIdAnd('remove -> "-", 'add -> "+", 'restore -> "^")
  implicit val dbCodecPatchReqTags         : DbCodec[PatchReqTags]          = dbCodec2
  implicit val dbCodecRepositionField      : DbCodec[RepositionField]       = dbCodec2
  implicit val dbCodecSetCustomTextField   : DbCodec[SetCustomTextField]    = dbCodecIdAnd('fid -> "f", 'value -> "t")
  implicit val dbCodecSetGenericReqTitle   : DbCodec[SetGenericReqTitle]    = dbCodec2
  implicit val dbCodecSetGenericReqType    : DbCodec[SetGenericReqType]     = dbCodec2
  implicit val dbCodecUpdateApplicableTag  : DbCodec[UpdateApplicableTag]   = dbCodec2
  implicit val dbCodecUpdateCustomImpField : DbCodec[UpdateCustomImpField]  = dbCodec2
  implicit val dbCodecUpdateCustomIssueType: DbCodec[UpdateCustomIssueType] = dbCodec2
  implicit val dbCodecUpdateCustomReqType  : DbCodec[UpdateCustomReqType]   = dbCodec2
  implicit val dbCodecUpdateCustomTagField : DbCodec[UpdateCustomTagField]  = dbCodec2
  implicit val dbCodecUpdateCustomTextField: DbCodec[UpdateCustomTextField] = dbCodec2
  implicit val dbCodecUpdateReqCodeGroup   : DbCodec[UpdateReqCodeGroup]    = dbCodec2
  implicit val dbCodecUpdateTagGroup       : DbCodec[UpdateTagGroup]        = dbCodec2

  /**
   * Assigns each event a `type_id`.
   *
   * This is only seen by the DB and doesn't affect binary codecs, thus there's no need to keep IDs in [0,127] for
   * efficient BooPickle int encoding.
   */
  val eventCodecRegistry = DbCodec.registry[Event, ActiveEvent] {
    // Content

    case _: DeleteReq             => 200
    case _: PatchImplicationSrc   => 201
    case _: PatchImplicationTgt   => 202
    case _: PatchReqCodes         => 203
    case _: PatchReqTags          => 204
    case _: SetCustomTextField    => 205

    case _: CreateGenericReq      => 230
    case _: SetGenericReqTitle    => 231
    case _: SetGenericReqType     => 232

    case _: CreateReqCodeGroup    => 240
    case _: UpdateReqCodeGroup    => 241
    case _: DeleteReqCodeGroup    => 242

    // Config

    case _: ApplyTemplate         =>   0

    case _: CreateCustomReqType   =>  10
    case _: UpdateCustomReqType   =>  11
    case _: DeleteCustomReqType   =>  12

    case _: CreateCustomIssueType =>  20
    case _: UpdateCustomIssueType =>  21
    case _: DeleteCustomIssueType =>  22

    case _: DeleteTag             =>  30
    case _: CreateApplicableTag   =>  31
    case _: UpdateApplicableTag   =>  32
    case _: CreateTagGroup        =>  33
    case _: UpdateTagGroup        =>  34

    case _: AddStaticField        =>  40
    case _: DeleteStaticField     =>  41
    case _: RepositionField       =>  42

    case _: DeleteCustomField     =>  50
    case _: CreateCustomTextField =>  51
    case _: UpdateCustomTextField =>  52
    case _: CreateCustomTagField  =>  53
    case _: UpdateCustomTagField  =>  54
    case _: CreateCustomImpField  =>  55
    case _: UpdateCustomImpField  =>  56
  }
}

// =====================================================================================================================

object EventSqlHelpers {
  import scala.slick.jdbc.{GetResult, SetParameter, PositionedResult, PositionedParameters}
  import shipreq.base.db.SqlHelpers._
  import EventDbCodecs.eventCodecRegistry

  implicit val GR_EventSeq = GetResult(r => EventSeq(r.nextInt()))
  implicit object SP_EventSeq extends SetParameter[EventSeq] {
    def apply(v: EventSeq, pp: PositionedParameters): Unit =
      pp setInt v.value
  }

  implicit val GR_HashScheme = GetResult(HashScheme unsafeGet _.nextShort())
  implicit object SP_HashScheme extends SetParameter[HashScheme] {
    def apply(v: HashScheme, pp: PositionedParameters): Unit =
      pp setShort v.id
  }

  implicit val GR_ProjectHash = GetResult(r => ProjectHash(r.<<, r.<<))
  implicit object SP_ProjectHash extends SetParameter[ProjectHash] {
    def apply(v: ProjectHash, pp: PositionedParameters): Unit = {
      SP_HashScheme(v.scheme, pp)
      pp setInt v.hash
    }
  }

  implicit object GR_Event extends GetResult[Event] {
    def apply(r: PositionedResult) = {
      val typeId     = r.nextShort()
      val dataIdType = r.nextString().head.toByte
      val dataId     = r.nextObject().asInstanceOf[Integer]
      val data       = r.nextString()
      val codec      = eventCodecRegistry.reader(typeId)
      codec.read(dataIdType, dataId, data)
    }
  }

  implicit object SP_ActiveEvent extends SetParameter[ActiveEvent] {
    def apply(e: ActiveEvent, pp: PositionedParameters): Unit = {
      val c = eventCodecRegistry.writer(e)
      val d = c._2.write(e)
      pp setShort c._1
      pp.setObject(d._1, java.sql.Types.OTHER)
      pp.setObject(d._2, java.sql.Types.INTEGER)
      pp.setObject(pgObject("json", d._3), java.sql.Types.OTHER)
    }
  }

  implicit object GR_VerifiedEvent extends GetResult[VerifiedEvent] {
    def apply(r: PositionedResult) = {
      val event      = GR_Event(r)
      val hashScheme = GR_HashScheme(r)
      val hash       = r.nextInt()
      VerifiedEvent(hashScheme, hash, event)
    }
  }
}

// =====================================================================================================================

object EventDao {
  case class EventSeq(value: Int) extends AnyVal
}

trait EventDao {
  this: DaoS =>
  import Sql._

  def createEvent(p: ProjectId, seq: EventSeq, e: ActiveEvent, h: ProjectHash): Unit =
    InsertEvent(p, seq, e, h).execute

  def findEvent(p: ProjectId, seq: EventSeq): Option[VerifiedEvent] =
    SelectEvent(p, seq).firstOption

  /**
   * @return Events in order from lowest to highest seq.
   */
  def findAllEvents(p: ProjectId): List[(EventSeq, VerifiedEvent)] =
    SelectAllEvents(p).list
}
