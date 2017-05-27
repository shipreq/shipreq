package shipreq.webapp.server.db

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty._
import scala.annotation.tailrec
import scalaz.Isomorphism.<=>
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.event._
import shipreq.webapp.base.hash._
import ApplyEvent.LogicVer
import TaggedTypes.JsonStr

object EventDbCodecs {
  import nyaya.util.{Multimap, MultiValues}
  import upickle._
  import upickle.Fns._
  import upickle.BaseCodecs.StringRW
  import shipreq.webapp.base.data._
  import shipreq.webapp.base.protocol.MPickleMacros._
  import shipreq.webapp.base.text.AtomTC
  import shipreq.webapp.base.util.GenericDataMacros._
  import EventDbMacros._

  private[this] val jsNum0 = Js.Num(0)
  private[this] val jsNum1 = Js.Num(1)

  implicit val pickleInt: ReadWriter[Int] =
    ReadWriter(i => Js.Num(i), { case Js.Num(i) => i.toInt })

  private def isOneElemCollection[A](c: Iterable[A]): Boolean = {
    val i = c.iterator
    if (i.hasNext) {
      i.next()
      !i.hasNext
    } else
      false // empty set
  }

  implicit class ReadWriterExt[A](private val rw: ReadWriter[A]) extends AnyVal {
    import StdlibCodecs.All._
    private implicit def _rw = rw

    def set(implicit ev: UnivEq[A]): ReadWriter[Set[A]] =
      ReadWriter.merge(SeqishR[A, Set], SeqishW[A, Set])

    /** A single-value set is stored as just that sole value directly.
      * Unambiguous as long as A itself never encodes to a Js.Arr. */
    def setNice(implicit ev: UnivEq[A]): ReadWriter[Set[A]] = {
      val rws = set
      val es = Set.empty[A]
      val r1 = rw.read.andThen(es + _)
      val w1 = rw.write
      ReadWriter(
        s => if (isOneElemCollection(s)) w1(s.head) else rws.write(s),
        r1 orElse rws.read)
    }

    def vector: ReadWriter[Vector[A]] =
      ReadWriter.merge(SeqishR[A, Vector], SeqishW[A, Vector])

    /** A single-value set is stored as just that sole value directly.
      * Unambiguous as long as A itself never encodes to a Js.Arr. */
    def vectorNice: ReadWriter[Vector[A]] = {
      val rws = vector
      val es = Vector.empty[A]
      val r1 = rw.read.andThen(es :+ _)
      val w1 = rw.write
      ReadWriter(
        s => if (isOneElemCollection(s)) w1(s.head) else rws.write(s),
        r1 orElse rws.read)
    }

    def nev: ReadWriter[NonEmptyVector[A]] =
      pickleNEV(vector)

    /** A single-value nes is stored as just that sole value directly.
      * Unambiguous as long as A itself never encodes to a Js.Arr. */
    def nesNice(implicit ev: UnivEq[A]): ReadWriter[NonEmptySet[A]] = {
      val rws = nes
      val r1 = rw.read.andThen(NonEmptySet one _)
      val w1 = rw.write
      ReadWriter(
        s => if (s.tail.isEmpty) w1(s.head) else rws.write(s),
        r1 orElse rws.read)
    }

    def nes(implicit ev: UnivEq[A]): ReadWriter[NonEmptySet[A]] =
      pickleNES(ev, set)
  }

  private def boolCase[T](iso: Boolean <=> T): ReadWriter[T] =
    ReadWriter(
      t => if (iso from t) jsNum1 else jsNum0,
      { case Js.Num(n) => iso to (n.toInt != 0) })

  def addOptionWithNoneAs0[A](rw: ReadWriter[A]): ReadWriter[Option[A]] =
    ReadWriter({
      case None    => jsNum0
      case Some(t) => rw write t
    }, {
      case `jsNum0` => None
      case j        => Some(readJs(j)(rw))
    })

  def setDiff[A: UnivEq](implicit rw: ReadWriter[A]): ReadWriter[SetDiff[A]] = {
    val rws = rw.setNice
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
          case _   => () // Be forgiving. Allows this to be squashed with other data.
        })
        SetDiff(removed = del, add)
    })
  }

  def pickleNonEmpty[N, E](f: N => E)(implicit rw: ReadWriter[E], proof: NonEmpty.Proof[E, N]): ReadWriter[N] =
    ReadWriter.xmapf(f)(NonEmpty require_! _)

  def pickleNonEmptyA[A](implicit rw: ReadWriter[A], proof: NonEmpty.ProofMono[A]): ReadWriter[NonEmpty[A]] =
    pickleNonEmpty(_.value)

  def pickleNEV[A](implicit rw: ReadWriter[Vector[A]]): ReadWriter[NonEmptyVector[A]] =
    pickleNonEmpty(_.whole)

  def pickleNES[A: UnivEq](implicit rw: ReadWriter[Set[A]]): ReadWriter[NonEmptySet[A]] =
    pickleNonEmpty(_.whole)

  def pickleNESD[A: UnivEq](implicit rw: ReadWriter[A]): ReadWriter[SetDiff.NE[A]] = {
    implicit val sd = setDiff[A]
    pickleNonEmptyA
  }

  implicit val pickleLive          = boolCase(Live)
  implicit val pickleImplRequired  = boolCase(ImplicationRequired)
  implicit val pickleMandatory     = boolCase(Mandatory)
  implicit val pickleDeletable     = boolCase(Deletable)
  implicit val pickleMutexChildren = boolCase(MutexChildren)

  implicit val pickleUseCaseId               : ReadWriter[UseCaseId                 ] = caseClass
  implicit val pickleUseCaseStepId           : ReadWriter[UseCaseStepId             ] = caseClass
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
    case _: UseCaseId    => "u"
  }

  implicit val pickleReqIdSet: ReadWriter[Set[ReqId]] =
    pickleReqId.setNice

  implicit val pickleReqIdNES: ReadWriter[NonEmptySet[ReqId]] =
    pickleReqId.nesNice

  implicit val pickleReqIdNESD = pickleNESD[ReqId]

  implicit val pickleUseCaseStepIdNESD = pickleNESD[UseCaseStepId]

  implicit val pickleReqCodeIdSet: ReadWriter[Set[ReqCodeId]] =
    pickleReqCodeId.setNice

  implicit val pickleReqCodeIdNES: ReadWriter[NonEmptySet[ReqCodeId]] =
    pickleReqCodeId.nesNice

  implicit val pickleDirection: ReadWriter[Direction] = pickleAdtOS {
    case Forwards  => "f"
    case Backwards => "b"
  }

  implicit val pickleUseCaseStepTreeField: ReadWriter[StaticField.UseCaseStepTree] = pickleAdtOS {
    case StaticField.NormalAltStepTree => "n"
    case StaticField.ExceptionStepTree => "e"
  }

  implicit val pickleFieldId: ReadWriter[FieldId] = pickleAdtOS {
    case _: CustomField.Text       .Id => "x"
    case _: CustomField.Tag        .Id => "t"
    case _: CustomField.Implication.Id => "i"
    case StaticField.NormalAltStepTree => "n"
    case StaticField.ExceptionStepTree => "e"
    case StaticField.StepGraph         => "g"
    case StaticField.ImplicationGraph  => "I"
  }

  implicit val pickleFieldIdPosition: ReadWriter[RelPos[FieldId]] =
    addOptionWithNoneAs0(pickleFieldId)

  implicit val pickleTagId: ReadWriter[TagId] = pickleAdtOS[TagId] {
    case _: ApplicableTagId => ""
    case _: TagGroupId      => "g"
  }

  implicit val pickleApplicableTagIdNESD = pickleNESD[ApplicableTagId]

  implicit val pickleTagPosition: ReadWriter[RelPos[TagId]] =
    addOptionWithNoneAs0(pickleTagId)

  implicit val pickleTagTreeParents: ReadWriter[TagInTree.Parents] = {
    val w: Writer[TagInTree.Parents] = StdlibCodecs.All.MapW
    val r: Reader[TagInTree.Parents] = StdlibCodecs.All.MapR
    ReadWriter.merge(r, w)
  }

  implicit val pickleTagTreeChildren: ReadWriter[TagInTree.Children] =
    pickleTagId.vector

  implicit val pickleVectorTreeParLoc: ReadWriter[VectorTree.ParentLocation] = {
    val iso = VectorTree.ParentLocation.isoVector
    val is = pickleInt.vectorNice
    ReadWriter.xmap(iso.get)(iso.reverseGet)(is, is)
  }

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


  def pickleISubsetNice[A: UnivEq : ReadWriter]: ReadWriter[ISubset[A]] = {
    import ISubset._
    implicit val as = implicitly[ReadWriter[A]].nesNice
    implicit val o: ReadWriter[Only[A]] = caseClass
    implicit val n: ReadWriter[Not [A]] = caseClass
    pickleAdtOS {
      case _: All [A] => "*"
      case _: Only[A] => "+"
      case _: Not [A] => "-"
    }
  }

  implicit val pickleApplicableReqTypes: ReadWriter[Field.ApplicableReqTypes] =
    pickleISubsetNice

  // TODO Performance can be improved, probably significantly
  object TextCodecs extends AtomTC[ReadWriter] {
    import shipreq.webapp.base.text._
    import Atom._

    @inline private def strkeyW[A](k: String, a: A)(implicit A: Writer[A]) =
      Js.Obj((k, A write a))

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
    private[this] final val ISSUEDESC = "?"
    private[this] final val REQREF    = "r"
    private[this] final val CODEREF   = "c"
    private[this] final val UCSTEPREF = "u"
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
      { case Js.Obj((WEBADD, v)) => t.WebAddress(readJs[String](v)) })

    override def emailAddress[T <: PlainTextMarkup](t: T): ReadWriter[t.EmailAddress] = ReadWriter(
      a => strkeyW(EMAILADD, a.value),
      { case Js.Obj((EMAILADD, v)) => t.EmailAddress(readJs[String](v)) })

    override def mathTeX[T <: PlainTextMarkup](t: T): ReadWriter[t.MathTeX] = ReadWriter(
      a => strkeyW(MATHTEX, a.value),
      { case Js.Obj((MATHTEX, v)) => t.MathTeX(readJs[String](v)) })

    override def reqRef[T <: ReqRef](t: T): ReadWriter[t.ReqRef] = ReadWriter(
      a => strkeyW(REQREF, a.value),
      { case Js.Obj((REQREF, v)) => t.ReqRef(readJs[ReqId](v)) })

    override def codeRef[T <: ReqRef](t: T): ReadWriter[t.CodeRef] = ReadWriter(
      a => strkeyW(CODEREF, a.value),
      { case Js.Obj((CODEREF, v)) => t.CodeRef(readJs[ReqCodeId](v)) })

    override def useCaseStepRef[T <: UseCaseStepRef](t: T): ReadWriter[t.UseCaseStepRef] = ReadWriter(
      a => strkeyW(UCSTEPREF, a.value),
      { case Js.Obj((UCSTEPREF, v)) => t.UseCaseStepRef(readJs[UseCaseStepId](v)) })

    override def tagRef[T <: TagRef](t: T): ReadWriter[t.TagRef] = ReadWriter(
      a => strkeyW(TAGREF, a.value),
      { case Js.Obj((TAGREF, v)) => t.TagRef(readJs[ApplicableTagId](v)) })

    override def issue[T <: Issue](t: T)(implicit s: ReadWriter[Text.InlineIssueDesc.OptionalText]): ReadWriter[t.Issue] = {
      val e = Vector.empty[(String, Js.Value)]
      ReadWriter(
      a => {
        var v = e :+ ((ISSUE, writeJs(a.typ)))
        if (a.desc.nonEmpty)
          v :+= ((ISSUEDESC, s write a.desc))
        Js.Obj(v: _*)
      }, {
        case Js.Obj((ISSUE, a))                 => t.Issue(readJs[CustomIssueTypeId](a), Vector.empty)
        case Js.Obj((ISSUE, a), (ISSUEDESC, b)) => t.Issue(readJs[CustomIssueTypeId](a), s read b)
        //case Js.Obj((ISSUEDESC, b), (ISSUE, a)) => t.Issue(readJs[CustomIssueTypeId](a), s read b)
      })
    }

    override def unorderedList[T <: ListMarkup](t: T)(implicit s: ReadWriter[NonEmptyVector[t.ListItem]]): ReadWriter[t.UnorderedList] = ReadWriter(
      a => strkeyW(UL, a.items),
      { case Js.Obj((UL, v)) => t.UnorderedList(readJs(v)(s)) })
  }
  import TextCodecs.instances._

  implicit val pickleOptionString: ReadWriter[Option[String]] =
    ReadWriter.xmap((s: String) => if (s.isEmpty) None else Some(s))(_ getOrElse "")

  implicit val pickleApplicableTagIdNES: ReadWriter[NonEmptySet[ApplicableTagId]] =
    pickleApplicableTagId.nesNice

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
    case CustomImpFieldGD.ReqTypeId => "T"
    case CustomImpFieldGD.Mandatory => "m"
    case CustomImpFieldGD.ReqTypes  => "a"
  } nev

  implicit val pickleCreateGenericReqGD = {
    implicit val x = pickleReqCodeIdAndValueNES
    // Using "T" for reqtype
    gdMPickler(GenericReqGD, false) {
      case GenericReqGD.Title    => "t"
      case GenericReqGD.ReqCodes => "c"
      case GenericReqGD.Tags     => "#"
      case GenericReqGD.ImpSrcs  => ">"
      case GenericReqGD.ImpTgts  => "<"
    }
  } values

  implicit val pickleCreateUseCaseGD = {
    implicit val x = pickleReqCodeIdAndValueNES
    // Using "s" for stepId
    gdMPickler(UseCaseGD, false) {
      case UseCaseGD.Title    => "t"
      case UseCaseGD.ReqCodes => "c"
      case UseCaseGD.Tags     => "#"
      case UseCaseGD.ImpSrcs  => ">"
      case UseCaseGD.ImpTgts  => "<"
    }
  } values

  implicit val pickleCodeGroupGD = gdMPickler(CodeGroupGD, true) {
    case CodeGroupGD.Code  => "c"
    case CodeGroupGD.Title => "t"
  } nev

  implicit val pickleUseCaseStepGD = gdMPickler(UseCaseStepGD, true) {
    case UseCaseStepGD.Title   => "t"
    case UseCaseStepGD.FlowIn  => "<"
    case UseCaseStepGD.FlowOut => ">"
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
  implicit val idTypeUseCaseId                = DbCodec.monoId('u', UseCaseId)
  implicit val idTypeReqId                    = DbCodec.polyId[ReqId]

  implicit val idTypeUseCaseStepId            = DbCodec.monoId(noDataIdType, UseCaseStepId)

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
        case StaticField.ImplicationGraph  => -4
      }, _.intValue match {
        case -1 => StaticField.NormalAltStepTree
        case -2 => StaticField.ExceptionStepTree
        case -3 => StaticField.StepGraph
        case -4 => StaticField.ImplicationGraph
      })

  implicit val idTypeFieldId: DbCodec.PolyId[FieldId] =
    new DbCodec.PolyId[FieldId]({
        case i: CustomFieldId => idTypeCustomFieldId bytePG i
        case _: StaticField   => idTypeStaticField.bytePG
      }, {
        case i: CustomFieldId => i.value
        case f: StaticField   => idTypeStaticField int f
      }, {
        case 's' => idTypeStaticField.make
        case b   => idTypeCustomFieldId make b
      })

  implicit val dbCodecApplicableTagCreate   : DbCodec[ApplicableTagCreate   ] = dbCodec2
  implicit val dbCodecApplicableTagUpdate   : DbCodec[ApplicableTagUpdate   ] = dbCodec2
  implicit val dbCodecContentRestore        : DbCodec[ContentRestore        ] = dbCodecJust('reqs_? -> "r", 'codeGroups_? -> "c")
  implicit val dbCodecCustomIssueTypeCreate : DbCodec[CustomIssueTypeCreate ] = dbCodec2
  implicit val dbCodecCustomIssueTypeDelete : DbCodec[CustomIssueTypeDelete ] = dbCodecIdOnly
  implicit val dbCodecCustomIssueTypeRestore: DbCodec[CustomIssueTypeRestore] = dbCodecIdOnly
  implicit val dbCodecCustomIssueTypeUpdate : DbCodec[CustomIssueTypeUpdate ] = dbCodec2
  implicit val dbCodecCustomReqTypeCreate   : DbCodec[CustomReqTypeCreate   ] = dbCodec2
  implicit val dbCodecCustomReqTypeDelete   : DbCodec[CustomReqTypeDelete   ] = dbCodecIdOnly
  implicit val dbCodecCustomReqTypeRestore  : DbCodec[CustomReqTypeRestore  ] = dbCodecIdOnly
  implicit val dbCodecCustomReqTypeUpdate   : DbCodec[CustomReqTypeUpdate   ] = dbCodec2
  implicit val dbCodecFieldCustomDelete     : DbCodec[FieldCustomDelete     ] = dbCodecIdOnly
  implicit val dbCodecFieldCustomImpCreate  : DbCodec[FieldCustomImpCreate  ] = dbCodec2
  implicit val dbCodecFieldCustomImpUpdate  : DbCodec[FieldCustomImpUpdate  ] = dbCodec2
  implicit val dbCodecFieldCustomRestore    : DbCodec[FieldCustomRestore    ] = dbCodecIdOnly
  implicit val dbCodecFieldCustomTagCreate  : DbCodec[FieldCustomTagCreate  ] = dbCodec2
  implicit val dbCodecFieldCustomTagUpdate  : DbCodec[FieldCustomTagUpdate  ] = dbCodec2
  implicit val dbCodecFieldCustomTextCreate : DbCodec[FieldCustomTextCreate ] = dbCodec2
  implicit val dbCodecFieldCustomTextUpdate : DbCodec[FieldCustomTextUpdate ] = dbCodec2
  implicit val dbCodecFieldReposition       : DbCodec[FieldReposition       ] = dbCodec2
  implicit val dbCodecFieldStaticAdd        : DbCodec[FieldStaticAdd        ] = dbCodecIdOnly
  implicit val dbCodecFieldStaticRemove     : DbCodec[FieldStaticRemove     ] = dbCodecIdOnly
  implicit val dbCodecGenericReqCreate      : DbCodec[GenericReqCreate      ] = dbCodecIdGdAnd('vs, 'rt -> "T")
  implicit val dbCodecGenericReqTitleSet    : DbCodec[GenericReqTitleSet    ] = dbCodec2
  implicit val dbCodecGenericReqTypeSet     : DbCodec[GenericReqTypeSet     ] = dbCodec2
  implicit val dbCodecProjectNameSet        : DbCodec[ProjectNameSet        ] = dbCodecDataOnly
  implicit val dbCodecProjectTemplateApply  : DbCodec[ProjectTemplateApply  ] = dbCodecDataOnly
  implicit val dbCodecCodeGroupCreate       : DbCodec[CodeGroupCreate       ] = dbCodec2
  implicit val dbCodecCodeGroupsDelete      : DbCodec[CodeGroupsDelete      ] = dbCodecDataOnly
  implicit val dbCodecCodeGroupUpdate       : DbCodec[CodeGroupUpdate       ] = dbCodec2
  implicit val dbCodecReqCodesPatch         : DbCodec[ReqCodesPatch         ] = dbCodecIdAnd('remove_? -> "-", 'add_? -> "+", 'restore_? -> "^")
  implicit val dbCodecReqFieldCustomTextSet : DbCodec[ReqFieldCustomTextSet ] = dbCodecIdAnd('fid -> "f", 'value -> "t")
  implicit val dbCodecReqImplicationsPatch  : DbCodec[ReqImplicationsPatch  ] = dbCodecIdAnd('dir -> "d", 'patch -> "")
  implicit val dbCodecReqsDelete            : DbCodec[ReqsDelete            ] = dbCodecJust('reqs -> "r", 'codeGroups_? -> "g", 'reason_? -> "j")
  implicit val dbCodecReqTagsPatch          : DbCodec[ReqTagsPatch          ] = dbCodec2
  implicit val dbCodecTagDelete             : DbCodec[TagDelete             ] = dbCodecIdOnly
  implicit val dbCodecTagGroupCreate        : DbCodec[TagGroupCreate        ] = dbCodec2
  implicit val dbCodecTagGroupUpdate        : DbCodec[TagGroupUpdate        ] = dbCodec2
  implicit val dbCodecTagRestore            : DbCodec[TagRestore            ] = dbCodecIdOnly
  implicit val dbCodecUseCaseCreate         : DbCodec[UseCaseCreate         ] = dbCodecIdGdAnd('vs, 'stepId -> "s")
  implicit val dbCodecUseCaseStepCreate     : DbCodec[UseCaseStepCreate     ] = dbCodecIdAnd('ucId -> "u", 'field -> "f", 'at_? -> "@")
  implicit val dbCodecUseCaseStepDelete     : DbCodec[UseCaseStepDelete     ] = dbCodecIdOnly
  implicit val dbCodecUseCaseStepRestore    : DbCodec[UseCaseStepRestore    ] = dbCodecIdOnly
  implicit val dbCodecUseCaseStepShiftLeft  : DbCodec[UseCaseStepShiftLeft  ] = dbCodecIdOnly
  implicit val dbCodecUseCaseStepShiftRight : DbCodec[UseCaseStepShiftRight ] = dbCodecIdOnly
  implicit val dbCodecUseCaseStepUpdate     : DbCodec[UseCaseStepUpdate     ] = dbCodec2
  implicit val dbCodecUseCaseTitleSet       : DbCodec[UseCaseTitleSet       ] = dbCodec2

  /**
   * Assigns each event a `type_id` ∈ [0,32767].
   *
   * This is only seen by the DB and doesn't affect binary codecs, thus there's no need to keep IDs in [0,127] for
   * efficient BooPickle int encoding.
   */
  val eventCodecRegistry = DbCodec.registry[Event, ActiveEvent] {

    // =======================
    // Content: Shared & codes
    // =======================

    case _: ReqsDelete             => 10
    case _: ContentRestore         => 11

    case _: ReqCodesPatch          => 20
    case _: ReqFieldCustomTextSet  => 21
    case _: ReqImplicationsPatch   => 22
    case _: ReqTagsPatch           => 23

    case _: CodeGroupCreate        => 90
    case _: CodeGroupUpdate        => 91
    case _: CodeGroupsDelete       => 92

    // =============
    // Content: Reqs
    // =============

    case _: GenericReqCreate       => 100
    case _: GenericReqTitleSet     => 101
    case _: GenericReqTypeSet      => 102

    case _: UseCaseCreate          => 200
    case _: UseCaseTitleSet        => 201

    case _: UseCaseStepCreate      => 230
    case _: UseCaseStepUpdate      => 231
    case _: UseCaseStepShiftLeft   => 232
    case _: UseCaseStepShiftRight  => 233
    case _: UseCaseStepDelete      => 234
    case _: UseCaseStepRestore     => 235

    // ======
    // Config
    // ======

    case _: ProjectTemplateApply   => 1000

    case _: CustomReqTypeCreate    => 1010
    case _: CustomReqTypeUpdate    => 1011
    case _: CustomReqTypeDelete    => 1012
    case _: CustomReqTypeRestore   => 1013

    case _: TagGroupCreate         => 1020
    case _: TagGroupUpdate         => 1021
    case _: ApplicableTagCreate    => 1024
    case _: ApplicableTagUpdate    => 1025
    case _: TagDelete              => 1028
    case _: TagRestore             => 1029

    case _: CustomIssueTypeCreate  => 1030
    case _: CustomIssueTypeUpdate  => 1031
    case _: CustomIssueTypeDelete  => 1032
    case _: CustomIssueTypeRestore => 1033

    case _: FieldReposition        => 1100
    case _: FieldStaticAdd         => 1110
    case _: FieldStaticRemove      => 1111
    case _: FieldCustomDelete      => 1120
    case _: FieldCustomRestore     => 1121
    case _: FieldCustomImpCreate   => 1130
    case _: FieldCustomImpUpdate   => 1131
    case _: FieldCustomTagCreate   => 1132
    case _: FieldCustomTagUpdate   => 1133
    case _: FieldCustomTextCreate  => 1134
    case _: FieldCustomTextUpdate  => 1135

    // =====================================
    // Cosmetic. No impact on content/config
    // =====================================

    case _: ProjectNameSet         => 2000
  }
}

// =====================================================================================================================

object EventSqlHelpers {
  import doobie.imports._
  import shipreq.base.db.SqlHelpers._
  import EventDbCodecs.eventCodecRegistry

  implicit val doobieMetaEventSeq: Meta[EventSeq] =
    doobieMetaCaseClass[EventSeq]

  implicit val doobieMetaHashScheme: Meta[HashScheme] =
    doobieMetaChar.xmap(HashScheme unsafeGet HashSchemeId(_), _.id.value)

  private val (hashScopeToChar, charToHashScope, _, _) =
    AdtMacros.adtIso[HashScope, Char] {
      case HashScope.WholeProject    => '*'
      case HashScope.Config          => '?'
      case HashScope.CfgIssueTypes   => 'I'
      case HashScope.CfgReqTypes     => 'R'
      case HashScope.CfgFields       => 'F'
      case HashScope.CfgTags         => 'T'
      case HashScope.Content         => '!'
      case HashScope.Reqs            => 'r'
      case HashScope.GenericReqs     => 'g'
      case HashScope.UseCases        => 'u'
      case HashScope.PubidRegister   => 'p'
      case HashScope.ReqCodes        => 'c'
      case HashScope.TextFieldData   => 'x'
      case HashScope.TagData         => 't'
      case HashScope.ImplicationData => 'i'
      case HashScope.DeletionReasons => 'd'
      case HashScope.Other           => '0'
    }
  implicit val doobieMetaHashScope: Meta[HashScope] =
    doobieMetaChar.xmap(charToHashScope, hashScopeToChar)

  implicit val doobieMetaLogicVer: Meta[LogicVer] =
    doobieMetaChar.xmap(LogicVer.apply, _.value)

  final val eventHR = "scope,logic_ver,hash_scheme,hash"
  final val eventHR_? = "?,?,?,?"
  implicit val doobieCompositeHashRec: Composite[HashRec] =
    Composite[(HashScope, LogicVer, HashScheme, Option[Int])].xmap[HashRec](
      x => HashRec(x._1, x._2, x._3)(x._4),
      hr => (hr.scope, hr.logicVer, hr.scheme, hr.hash))

  private type MsgJson = JsonStr[Any]
  private implicit val doobieMetaMsgJson: Meta[MsgJson] = jsonStr

  final val eventE = "type_id,data_id_type,data_id,data"
  final val eventE_? = "?,?,?,?"
  implicit val doobieCompositeEvent: Composite[Event] =
    Composite[(Short, PGChar, Option[Int], Option[MsgJson])].readOnly { case (typeId, dataIdType, dataId, data) =>
      val codec = eventCodecRegistry.reader(typeId)
      codec.read(dataIdType.toByte, dataId, data getOrElse[MsgJson] JsonStr(null))
    }
  implicit val doobieCompositeActiveEvent: Composite[ActiveEvent] =
    Composite[(Short, PGChar, Option[Int], Option[MsgJson])].writeOnly { e =>
      val (typeId, codec) = eventCodecRegistry.writer(e)
      val (dataIdType, dataId, data) = codec.write(e)
      (typeId, dataIdType, dataId, if (data == null) None else Some(JsonStr(data)))
    }
}
