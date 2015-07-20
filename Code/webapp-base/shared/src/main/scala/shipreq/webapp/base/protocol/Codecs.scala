package shipreq.webapp.base.protocol

import japgolly.nyaya.util.{MultiValues, Multimap}
import scala.collection.generic.CanBuildFrom
import scalaz.{OneAnd, \&/, \/, -\/, \/-, Name, Need}
import scalaz.Isomorphism.<=>

import upickle._
import upickle.Fns._
import upickle.TupleCodecs._
import MPickleMacros.{caseClass, _caseClass}

import shipreq.base.util._
import shipreq.base.util.TaggedTypes._
import shipreq.webapp.base.text.Text

// =====================================================================================================================
private[protocol] object CodecBase extends StdlibCodecs.Maps {

  def tagS[T <: TaggedString](C: String => T) =
    ReadWriter[T](i => Js.Str(i.value), { case Js.Str(i) => C(i)})

  def tagI[T <: TaggedInt](C: Int => T) =
    ReadWriter[T](i => Js.Str(i.value.toString), { case Js.Str(i) => C(i.toInt)})

  def tagL[T <: TaggedLong](C: Long => T) =
    ReadWriter[T](i => Js.Str(i.value.toString), { case Js.Str(i) => C(i.toLong)})

  def boolCase[T](iso: IsoBool[T]) =
    ReadWriter[T](t => if (t :: iso) Js.Num(1) else Js.Num(0), {
      case Js.Num(n) => iso <~ (n.toInt != 0)
    })

  // UNSAFE. Make sure tests using exhaustive pattern matching to cover this hierarchy
  def enum[T: UnivEq](ts: NonEmptyVector[T]) = {
    val table = BiMap(ts.whole.zipWithIndex.map(p => p._1 -> ('0' + p._2).toChar.toString).toMap)
    ReadWriter[T](t => Js.Str(table.ab(t)), {
      case Js.Str(k) if table.ba.contains(k) => table.ba(k)
    })
  }

  // UNSAFE. Make sure tests using exhaustive pattern matching to cover this hierarchy
  def enumS[T: UnivEq](ts: NonEmptySet[T]) = enum(ts.toNEV)

  def xmap[A, B](f: A => B)(g: B => A)(implicit RB: Reader[B], WB: Writer[B]) =
    ReadWriter[A](WB.write compose f, RB.read andThen g)

  implicit class ReaderExt[T](val r: Reader[T]) extends AnyVal {
    @inline def readSet[TT >: T: UnivEq](s: Seq[Js.Value]): Set[TT] =
      foldlSeq[Set[TT]](s, UnivEq.emptySet)(_ + _)

    @inline def readList(s: Seq[Js.Value]): List[T] =
      foldlSeq[List[T]](s, Nil)((a, b) => b :: a)
      // foldrSeq[List[T]](s, Nil)(_ :: _)

    @inline def foldlSeq[B](s: Seq[Js.Value], z: B)(f: (B, T) => B): B =
      s.foldLeft(z)((b, j) => f(b, r read j))

    @inline def foldrSeq[B](s: Seq[Js.Value], z: B)(f: (T, B) => B): B =
      s.foldRight(z)((j, b) => f(r read j, b))

    @inline def widen[A >: T]: Reader[A] = Reader(r.read)
  }

  implicit class WriterExt[A](val w: Writer[A]) extends AnyVal {
    @inline def narrow[B <: A]: Writer[B] = Writer(w.write)
//    import scalaz.Liskov._
//    @inline def narrowL[B](implicit ev: B <~< A): Writer[B] = Writer(b => w.write(ev(b)))
//    @inline def cmap[B](f: B => A): Writer[B] = Writer(b => w.write(f(b)))
  }

  def writeIterable[T](ts: Iterable[T])(implicit W: Writer[T]) =
    Js.Arr(ts.foldLeft(List.empty[Js.Value])((q,i) => W.write(i) :: q): _*)

  def intkeyW[T](k: Int, t: T)(implicit T: Writer[T]) =
    Js.Arr(Js.Num(k), T write t)

  def intkeyW2[A, B](k: Int, a: A, b: B)(implicit A: Writer[A], B: Writer[B]) =
    Js.Arr(Js.Num(k), A write a, B write b)

  def intkeyW3[A, B, C](k: Int, a: A, b: B, c: C)(implicit A: Writer[A], B: Writer[B], C: Writer[C]) =
    Js.Arr(Js.Num(k), A write a, B write b, C write c)

  def intkeyW4[A, B, C, D](k: Int, a: A, b: B, c: C, d: D)(implicit A: Writer[A], B: Writer[B], C: Writer[C], D: Writer[D]) =
    Js.Arr(Js.Num(k), A write a, B write b, C write c, D write d)

  def strkeyW[A](k: String, a: A)(implicit A: Writer[A]) =
    Js.Arr(Js.Str(k), A write a)

  def strkeyW2[A, B](k: String, a: A, b: B)(implicit A: Writer[A], B: Writer[B]) =
    Js.Arr(Js.Str(k), A write a, B write b)

  def iMap[K: UnivEq, V: Reader : Writer](key: V => K): ReadWriter[IMap[K, V]] =
    xmap((_: IMap[K, V]).values)(IMap.empty(key) ++ _)

  @inline def mergeRW[A](implicit r: Reader[A], w: Writer[A]): ReadWriter[A] =
    ReadWriter[A](w.write, r.read)

  /*
  Something like this for ADTs maybe?
  val vv = Vector[ReadWriter[_ <: Tag]](tagGroup, applicableTag)
  def w: Tag => ReadWriter[_ <: Tag] = {
    case t: TagGroup      => vv(0)
    case t: ApplicableTag => vv(1)
  }
  implicit def tag2 = ReadWriter[Tag](
  t => w(t).asInstanceOf[Writer[Tag]] write t, {
    case Js.Arr(Js.Num(n), v) => vv(n.toInt) read0 v
  })
  */
}

// =====================================================================================================================
object GenericCodecs {
  import shipreq.webapp.base.data.DataIdAux
  import CodecBase._

  @inline implicit def string = BaseCodecs.StringRW
  @inline implicit def unit   = BaseCodecs.UnitRW

  implicit def option[A: Reader: Writer]: ReadWriter[Option[A]] =
    ReadWriter[Option[A]](
    _.fold(Js.Arr())(a => Js.Arr(writeJs(a))), {
      case Js.Arr()  => None
      case Js.Arr(a) => Some(readJs[A](a))
    })

  implicit def nonEmptyVectorR[A: Reader]: Reader[NonEmptyVector[A]] =
    Reader(implicitly[Reader[Vector[A]]].read andThen (l => NonEmptyVector(l.head, l.tail)))

  implicit def nonEmptyVectorW[A: Writer]: Writer[NonEmptyVector[A]] =
    Writer(n => implicitly[Writer[Vector[A]]] write n.whole)

//  implicit def nonEmptyVectorRW[A: Reader: Writer]: ReadWriter[NonEmptyVector[A]] =
//    xmap[NonEmptyVector[A], Vector[A]](_.whole)(l => NonEmptyVector(l.head, l.tail))

  implicit def nonEmptySetR[A: UnivEq: Reader]: Reader[NonEmptySet[A]] =
    Reader(implicitly[Reader[Set[A]]].read andThen (l => NonEmptySet(l.head, l.tail)))

  implicit def nonEmptySetW[A: UnivEq: Writer]: Writer[NonEmptySet[A]] =
    Writer(n => implicitly[Writer[Set[A]]] write n.whole)

  implicit def disjunction[A: Reader: Writer, B: Reader: Writer]: ReadWriter[A \/ B] =
    ReadWriter[A \/ B]({
      case -\/(a)    => intkeyW(0, a)
      case \/-(b)    => intkeyW(1, b)
    }, {
      case Js.Arr(Js.Num(n), v) => n.toInt match {
        case 0 => -\/(readJs[A](v))
        case 1 => \/-(readJs[B](v))
      }
    })

  implicit def these[A: Reader: Writer, B: Reader: Writer]: ReadWriter[A \&/ B] = {
    import \&/._
    ReadWriter[A \&/ B]({
      case This(a)    => intkeyW(1, a)
      case That(b)    => intkeyW(2, b)
      case Both(a, b) => Js.Arr(Js.Num(3), writeJs(a), writeJs(b))
    }, {
      case Js.Arr(Js.Num(n), v) => n.toInt match {
        case 1 => This(readJs[A](v))
        case 2 => That(readJs[B](v))
      }
      case Js.Arr(Js.Num(n), a, b) if n.toInt == 3 =>
        Both(readJs[A](a), readJs[B](b))
    })
  }

  implicit def isubset[A](implicit RF: Reader[NonEmptySet[A]], WF: Writer[NonEmptySet[A]]): ReadWriter[ISubset[A]] = {
    import ISubset._
    ReadWriter[ISubset[A]]({
      case All()    => Js.Num(0)
      case Only(as) => intkeyW(1, as)
      case Not (as) => intkeyW(2, as)
    }, {
      case Js.Num(n) if n.toInt == 0 => All()
      case Js.Arr(Js.Num(n), as) => n.toInt match {
        case 1 => Only(RF read as)
        case 2 => Not (RF read as)
      }
    })
  }

  implicit def bimap[K: UnivEq, V: UnivEq](implicit r: Reader[Map[K, V]], w: Writer[Map[K, V]]): ReadWriter[BiMap[K, V]] =
    ReadWriter(
      b => w write b.ab,
      r.read.andThen(BiMap(_)))

  implicit def multimap[K: UnivEq, L[_], V](implicit r: Reader[Map[K, L[V]]], w: Writer[Map[K, L[V]]], l: MultiValues[L]): ReadWriter[Multimap[K, L, V]] =
    ReadWriter(
      mm => w write mm.m,
      r.read.andThen(Multimap(_))) // TODO Multimap reader could be optimised

  @inline implicit def iMapAuto[K: UnivEq : Reader : Writer, V: Reader : Writer](implicit d: DataIdAux[V, K]): ReadWriter[IMap[K, V]] =
    iMap(d.id)

  implicit def mtrie[K: UnivEq, V](implicit rk: Reader[K],  wk: Writer[K],  rv: Reader[V],  wv: Writer[V]): ReadWriter[MTrie.Trie[K, V]] = {
    val types = new MTrie.Types[K, V]
    import types._

    lazy val nodeRW: ReadWriter[Node] = {
      implicit val targetRW = caseClass[Value]
      implicit val branchRW = caseClass[Branch]

      ReadWriter[Node]({
        case i: Branch => intkeyW(0, i)
        case i: Value  => intkeyW(1, i)
      }, {
        case Js.Arr(Js.Num(n), v) => n.toInt match {
          case 0 => readJs[Branch](v)
          case 1 => readJs[Value ](v)
        }
      })
    }

    lazy val trieRW = {
      lazy val w = MapW(wk, nodeRW)
      lazy val r = MapR(rk, nodeRW)
      ReadWriter[Trie](i => w write i, {case i => r read i})
    }

    trieRW
  }

  implicit def setDiff[A: UnivEq](implicit r: Reader[A], w: Writer[A]): ReadWriter[SetDiff[A]] =
    xmap((s: SetDiff[A]) => (s.removed, s.added))(t => SetDiff(t._1, t._2))
}

// =====================================================================================================================
object DataCodecs {
  import shipreq.webapp.base.data._
  import DataImplicits._
  import CodecBase._
  import GenericCodecs._

  implicit final val rev = tagL(Rev.apply)

  @inline implicit def revAnd[T](implicit WT: Writer[T], RT: Reader[T]) =
    caseClass[RevAnd[T]]

  implicit final val live           = boolCase(Live)
  implicit final val impReq         = boolCase(ImplicationRequired)
  implicit final val mutexChildren  = boolCase(MutexChildren)
  implicit final val mandatory      = boolCase(Mandatory)
  implicit final val deletable      = boolCase(Deletable)
  implicit final val hashRefKey     = tagS(HashRefKey.apply)
  implicit final val fieldRefKey    = tagS(FieldRefKey.apply)
  implicit final val deletionAction = enum(DeletionAction.values)

  implicit final val customIssueTypeId = tagL(CustomIssueTypeId.apply)
  implicit final val customIssueType   = caseClass[CustomIssueType]

  // -------------------------------------------------------------------------------------------------------------------
  // ReqTypes

  implicit final val reqTypeId = {
    import StaticReqType._
    ReadWriter[ReqTypeId]({
      case i: CustomReqTypeId => Js.Str(i.value.toString)
      case UseCase            => Js.Str("u")
    }, {
      case Js.Str(ParseLong(i)) => CustomReqTypeId(i)
      case Js.Str("u")          => UseCase
    })
  }

  implicit final val reqTypeMnemonic = tagS(ReqType.Mnemonic.apply)
  implicit final val customReqTypeId = tagL(CustomReqTypeId.apply)
  implicit final val customReqType   = caseClass[CustomReqType]

  // -------------------------------------------------------------------------------------------------------------------
  // Tags

  implicit final val tagGroupId      = tagL(TagGroupId.apply)
  implicit final val applicableTagId = tagL(ApplicableTagId.apply)
  implicit final val tagId           = tagIdRW
  implicit final val tagGroup        = caseClass[TagGroup]
  implicit final val applicableTag   = caseClass[ApplicableTag]
  implicit final val tag             = tagRW
  implicit final val tagInTree       = caseClass[TagInTree]
  implicit final val tagTree         = iMap[TagId, TagInTree](_.tag.id)

  private[this] def tagIdRW = ReadWriter[TagId]({
    case i: ApplicableTagId => intkeyW(0, i)(applicableTagId)
    case i: TagGroupId      => intkeyW(1, i)(tagGroupId)
  }, {
    case Js.Arr(Js.Num(n), v) => n.toInt match {
      case 0 => readJs(v)(applicableTagId)
      case 1 => readJs(v)(tagGroupId)
    }
  })

  private[this] def tagRW = ReadWriter[Tag]({
    case t: TagGroup      => intkeyW(0, t)(tagGroup)
    case t: ApplicableTag => intkeyW(1, t)(applicableTag)
  }, {
    case Js.Arr(Js.Num(n), v) => n.toInt match {
      case 0 => readJs(v)(tagGroup)
      case 1 => readJs(v)(applicableTag)
    }
  })

  // -------------------------------------------------------------------------------------------------------------------
  // Fields

  implicit final val customFieldImplId = tagL(CustomField.Implication.Id.apply)
  implicit final val customFieldTextId = tagL(CustomField.Text       .Id.apply)
  implicit final val customFieldTagId  = tagL(CustomField.Tag        .Id.apply)
  implicit final val customFieldId     = ReadWriter[CustomFieldId]({
    case f: CustomField.Text       .Id => strkeyW("x", f)
    case f: CustomField.Tag        .Id => strkeyW("t", f)
    case f: CustomField.Implication.Id => strkeyW("i", f)
  }, {
    case Js.Arr(Js.Str(k), v) => k match {
      case "x" => readJs[CustomField.Text       .Id](v)
      case "t" => readJs[CustomField.Tag        .Id](v)
      case "i" => readJs[CustomField.Implication.Id](v)
    }
  })

  implicit final val customFieldImpl = caseClass[CustomField.Implication]
  implicit final val customFieldText = caseClass[CustomField.Text]
  implicit final val customFieldTag  = caseClass[CustomField.Tag]
  implicit final val customField     = ReadWriter[CustomField]({
    case f: CustomField.Text        => strkeyW("x", f)
    case f: CustomField.Tag         => strkeyW("t", f)
    case f: CustomField.Implication => strkeyW("i", f)
  }, {
    case Js.Arr(Js.Str(k), v) => k match {
      case "x" => readJs[CustomField.Text       ](v)
      case "t" => readJs[CustomField.Tag        ](v)
      case "i" => readJs[CustomField.Implication](v)
    }
  })

  implicit final val staticField = {
    import StaticField._
    ReadWriter[StaticField]({
      case NormalAltStepTree => Js.Str("n")
      case ExceptionStepTree => Js.Str("e")
      case StepGraph         => Js.Str("g")
    }, {
      case Js.Str("n") => NormalAltStepTree
      case Js.Str("e") => ExceptionStepTree
      case Js.Str("g") => StepGraph
    })
  }

  implicit final val fieldId = ReadWriter[FieldId]({
    case i: CustomFieldId => writeJs(i)
    case i: StaticField   => writeJs(i)
  },
    // Shape determines type. Arr(Str(_), _) or Str(_)
    customFieldId.read orElse staticField.read
  )

  implicit final val fieldSet = caseClass[FieldSet]

  // -------------------------------------------------------------------------------------------------------------------
  /** Text Codecs */
  import shipreq.webapp.base.text.AtomTC
  object TextCodecs extends AtomTC[ReadWriter] {
    import shipreq.webapp.base.text._
    import Atom._

    override def lazily[A](a: => ReadWriter[A]): ReadWriter[A] = {
      lazy val b = a
      ReadWriter(a => b write a, { case j => b read j })
    }

    override def vec[A](implicit a: ReadWriter[A]) = mergeRW

    override def nev[A](as: ReadWriter[Vector[A]])(implicit a: ReadWriter[A]) = mergeRW

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

  // -------------------------------------------------------------------------------------------------------------------
  // Requirements

  implicit final val reqTypePos    = tagI(ReqTypePos.apply)
  implicit final def pubid[T <: ReqTypeId : Reader : Writer] = caseClass[PubidT[T]]
  implicit final val genericReqId  = tagL(GenericReqId.apply)
  implicit final val genericReq    = caseClass[GenericReq]
  implicit final val reqId         = _reqId
  implicit final val req           = _req
  implicit final val pubidRegister = xmap((_: PubidRegister).value)(PubidRegister.apply)
  implicit final val requirements  = caseClass[Requirements]
  implicit final val implications  = xmap((_: Implications).srcToTgt)(Implications.apply)

  private def _req = ReadWriter[Req]({
    case r: GenericReq => intkeyW(0, r)(genericReq)
  }, {
    case Js.Arr(Js.Num(n), v) => n.toInt match {
      case 0 => readJs(v)(genericReq)
    }
  })

  private def _reqId = ReadWriter[ReqId]({
    case i: GenericReqId => intkeyW(0, i)(genericReqId)
  }, {
    case Js.Arr(Js.Num(n), v) => n.toInt match {
      case 0 => readJs(v)(genericReqId)
    }
  })

  // -------------------------------------------------------------------------------------------------------------------
  // Req Codes

  implicit final val reqCodeGroup  = caseClass[ReqCodeGroup]
  implicit final val reqCodeNode   = xmap[ReqCode.Node, String](_.value)(ReqCode.Node.applyFn)
  implicit final val reqCodeTarget = _reqCodeTarget
  implicit final val reqCodeId     = tagL(ReqCodeId.apply)
  implicit final val reqCodeAData  = caseClass[ReqCode.ActiveData]
  implicit final val reqCodeData   = caseClass[ReqCode.Data]
  implicit final val reqCodeTrie   = (mtrie: ReadWriter[ReqCode.Trie])
  implicit final val reqCodes      = caseClass[ReqCodes]

  private def _reqCodeTarget = ReadWriter[ReqCode.Target]({
    case i: ReqId          => intkeyW(0, i)(reqId)
    case i: ReqCodeGroup   => intkeyW(1, i)(reqCodeGroup)
  }, {
    case Js.Arr(Js.Num(n), v) => n.toInt match {
      case 0 => readJs(v)(reqId)
      case 1 => readJs(v)(reqCodeGroup)
    }
  })

  // -------------------------------------------------------------------------------------------------------------------
  implicit final val projectConfig = caseClass[ProjectConfig]
  implicit final val project       = caseClass[Project]
}
