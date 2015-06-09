package shipreq.webapp.base.protocol

import japgolly.nyaya.util.{MultiValues, Multimap}
import scalaz.{OneAnd, \&/, \/, -\/, \/-, Name, Need}
import scalaz.Isomorphism.<=>

import upickle._
import upickle.Fns._
import upickle.TupleCodecs._
import upickle.StdlibCodecs.{SeqishR, SeqishW}
import upickle.StdlibCodecs.{MapR, MapW}

import shipreq.base.util._
import shipreq.base.util.TaggedTypes._
import shipreq.webapp.base.text.Text

// =====================================================================================================================
private[protocol] object CodecBase {

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

  def caseclass1[A, Z](y: A => Z, u: Z => Option[A])(implicit RA: Reader[A], WA: Writer[A]) =
    ReadWriter[Z](z => WA write u(z).get, RA.read andThen y)

  def caseclass2[A: Reader : Writer, B: Reader : Writer, Z]
  (y: (A, B) => Z, u: Z => Option[(A, B)]): ReadWriter[Z] = {
    val r = Tuple2R[A, B].read
    val w = Tuple2W[A, B].write
    ReadWriter[Z](z => w(u(z).get), r andThen y.tupled)
  }

  def caseclass3[A: Reader : Writer, B: Reader : Writer, C: Reader : Writer, Z]
  (y: (A, B, C) => Z, u: Z => Option[(A, B, C)]): ReadWriter[Z] = {
    val r = Tuple3R[A, B, C].read
    val w = Tuple3W[A, B, C].write
    ReadWriter[Z](z => w(u(z).get), r andThen y.tupled)
  }

  def caseclass4[A: Reader : Writer, B: Reader : Writer, C: Reader : Writer, D: Reader : Writer, Z]
  (y: (A, B, C, D) => Z, u: Z => Option[(A, B, C, D)]): ReadWriter[Z] = {
    val r = Tuple4R[A, B, C, D].read
    val w = Tuple4W[A, B, C, D].write
    ReadWriter[Z](z => w(u(z).get), r andThen y.tupled)
  }
  
  def caseclass5[A: Reader : Writer, B: Reader : Writer, C: Reader : Writer, D: Reader : Writer, E: Reader : Writer, Z]
  (y: (A, B, C, D, E) => Z, u: Z => Option[(A, B, C, D, E)]): ReadWriter[Z] = {
    val r = Tuple5R[A, B, C, D, E].read
    val w = Tuple5W[A, B, C, D, E].write
    ReadWriter[Z](z => w(u(z).get), r andThen y.tupled)
  }

  def caseclass6[A: Reader : Writer, B: Reader : Writer, C: Reader : Writer, D: Reader : Writer, E: Reader : Writer, F: Reader : Writer, Z]
  (y: (A, B, C, D, E, F) => Z, u: Z => Option[(A, B, C, D, E, F)]): ReadWriter[Z] = {
    val r = Tuple6R[A, B, C, D, E, F].read
    val w = Tuple6W[A, B, C, D, E, F].write
    ReadWriter[Z](z => w(u(z).get), r andThen y.tupled)
  }

  def caseclass7[A: Reader : Writer, B: Reader : Writer, C: Reader : Writer, D: Reader : Writer, E: Reader : Writer, F: Reader : Writer, G: Reader : Writer, Z]
  (y: (A, B, C, D, E, F, G) => Z, u: Z => Option[(A, B, C, D, E, F, G)]): ReadWriter[Z] = {
    val r = Tuple7R[A, B, C, D, E, F, G].read
    val w = Tuple7W[A, B, C, D, E, F, G].write
    ReadWriter[Z](z => w(u(z).get), r andThen y.tupled)
  }

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

  def iMapK[T, K[+ _ <: T], V[+ _ <: T]](rel: RelationProof[T, V, K])(implicit rv: Reader[V[T]], wv: Writer[V[T]], ue: UnivEq[K[T]]): ReadWriter[IMapK[T, K, V]] =
    xmap((_: IMapK[T, K, V]).values)(rel.emptyIMapK ++ _)

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

  implicit def mtrie[K, V](implicit rk: Reader[K],  wk: Writer[K],  rv: Reader[V],  wv: Writer[V]): ReadWriter[MTrie.Trie[K, V]] = {
    import MTrie._

    lazy val nodeRW: ReadWriter[Node[K, V]] = {
      implicit val targetRW =
        caseclass1(Value.apply[K, V], Value.unapply[K, V])
      implicit val branchRW =
        caseclass2(MTrie.Branch.apply[K, V], MTrie.Branch.unapply[K, V])(implicitly, implicitly, trieRW, trieRW)

      ReadWriter[Node[K, V]]({
        case i: Branch[K, V] => intkeyW(0, i)
        case i: Value[K, V] => intkeyW(1, i)
      }, {
        case Js.Arr(Js.Num(n), v) => n.toInt match {
          case 0 => readJs[Branch[K, V]](v)
          case 1 => readJs[Value[K, V]](v)
        }
      })
    }

    lazy val trieRW = {
      lazy val w = MapW(wk, nodeRW)
      lazy val r = MapR(rk, nodeRW)
      ReadWriter[Trie[K, V]](i => w write i, {case i => r read i})
    }

    trieRW
  }
}

// =====================================================================================================================
object DataCodecs {
  import shipreq.webapp.base.data._
  import DataImplicits._
  import CodecBase._
  import GenericCodecs._

  @inline implicit def revAnd[T](implicit WT: Writer[T], RT: Reader[T]) =
    caseclass2(RevAnd.apply[T], RevAnd.unapply[T])(rev, rev, RT, WT)

  implicit final val rev            = tagL(Rev.apply)
  implicit final val live           = boolCase(Live)
  implicit final val impReq         = boolCase(ImplicationRequired)
  implicit final val mutexChildren  = boolCase(MutexChildren)
  implicit final val mandatory      = boolCase(Mandatory)
  implicit final val deletable      = boolCase(Deletable)
  implicit final val hashRefKey     = tagS(HashRefKey.apply)
  implicit final val fieldRefKey    = tagS(FieldRefKey.apply)
  implicit final val deletionAction = enum(DeletionAction.values)

  implicit final val customIssueTypeId = tagL(CustomIssueTypeId.apply)
  implicit final val customIssueType   = caseclass4(CustomIssueType.apply, CustomIssueType.unapply)

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
  implicit final val customReqType   = caseclass6(CustomReqType.apply, CustomReqType.unapply)

  // -------------------------------------------------------------------------------------------------------------------
  // Tags

  implicit final val tagGroupId      = tagL(TagGroupId.apply)
  implicit final val applicableTagId = tagL(ApplicableTagId.apply)
  implicit final val tagId           = tagIdRW
  implicit final val tagGroup        = caseclass5(TagGroup.apply, TagGroup.unapply)
  implicit final val applicableTag   = caseclass5(ApplicableTag.apply, ApplicableTag.unapply)
  implicit final val tag             = tagRW
  implicit final val tagInTree       = caseclass2(TagInTree.apply, TagInTree.unapply)
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

  implicit final val customFieldImpl = caseclass5(CustomField.Implication.apply, CustomField.Implication.unapply)
  implicit final val customFieldText = caseclass6(CustomField.Text       .apply, CustomField.Text       .unapply)
  implicit final val customFieldTag  = caseclass5(CustomField.Tag        .apply, CustomField.Tag        .unapply)
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

  implicit final val fieldSet = caseclass2(FieldSet.apply, FieldSet.unapply)

  // -------------------------------------------------------------------------------------------------------------------
  /** Text Codecs */
  private[this] object TC {
    import shipreq.webapp.base.text.Atom._

    private[this] final val BLANKLINE = 0

    private[this] final val WEBADD   = "/"
    private[this] final val EMAILADD = "@"
    private[this] final val MATHTEX  = "="
    private[this] final val UL       = "*"
    private[this] final val ISSUE    = "i"
    private[this] final val REQREF   = "r"
    private[this] final val CODEREF  = "c"
    private[this] final val TAGREF   = "t"

    lazy val writeAny: Writer[AnyAtom] =
      Writer[AnyAtom]({
        case a: Literal         # Literal       => Js.Str(a.value)
        case a: NewLine         # BlankLine     => Js.Num(BLANKLINE)
        case a: ReqRef          # ReqRef        => strkeyW (REQREF,   a.value)
        case a: ReqRef          # CodeRef       => strkeyW (CODEREF,  a.value)
        case a: Issue           # Issue         => strkeyW2(ISSUE,    a.typ, a.desc)
        case a: PlainTextMarkup # WebAddress    => strkeyW (WEBADD,   a.value)
        case a: PlainTextMarkup # EmailAddress  => strkeyW (EMAILADD, a.value)
        case a: PlainTextMarkup # MathTeX       => strkeyW (MATHTEX,  a.value)
        case a: ListMarkup      # UnorderedList => strkeyW (UL,       a.items)(writeListItemNEV)
        case a: TagRef          # TagRef        => strkeyW (TAGREF,   a.value)
      })

    lazy val writeListItem: Writer[ListMarkup#ListItem] = {
      implicit val a: Writer[ListMarkup#Atom] = writeAny.narrow
      implicitly
    }

    lazy val writeListItemNEV: Writer[NonEmptyVector[ListMarkup#ListItem]] = {
      implicit val li = writeListItem
      implicitly
    }

    // Partial Reader
    type PR[A] = PartialFunction[Js.Value, A]

    def readLiteral(t: Literal): PR[t.Literal] =
      { case Js.Str(s) => t.Literal(s) }

    def readBlankLine(t: NewLine): PR[t.BlankLine] =
      { case Js.Num(n) if n.toInt == 0 => t.blankLine }

    def readPlainTextMarkup(t: PlainTextMarkup): PR[t.Atom] = {
      case Js.Arr(Js.Str(WEBADD),   v) => t.WebAddress  (readJs[String](v))
      case Js.Arr(Js.Str(EMAILADD), v) => t.EmailAddress(readJs[String](v))
      case Js.Arr(Js.Str(MATHTEX),  v) => t.MathTeX     (readJs[String](v))
    }

    def readListMarkup(t: ListMarkup)(implicit ra: Name[Reader[t.Atom]]): PR[t.Atom] = {
      lazy val liNev: Reader[NonEmptyVector[t.ListItem]] = nonEmptyVectorR(readerListItem(t)(ra.value));
      { case Js.Arr(Js.Str(UL), v) => t.UnorderedList(readJs(v)(liNev)) }
    }

    def readerListItem(t: ListMarkup)(implicit r: Reader[t.Atom]): Reader[t.ListItem] = implicitly

    def readSingleLine(t: SingleLine): PR[t.Atom] =
      readLiteral(t) orElse readPlainTextMarkup(t)

    def readMultiLine(t: MultiLine)(implicit ra: Name[Reader[t.Atom]]): PR[t.Atom] =
      readSingleLine(t) orElse readBlankLine(t) orElse readListMarkup(t)

    def readIssue(t: Issue): PR[t.Issue] =
      { case Js.Arr(Js.Str(ISSUE), a, b) => t.Issue(readJs[CustomIssueTypeId](a), readJs[Text.InlineIssueDesc.OptionalText](b)) }

    def readReqRef(t: ReqRef): PR[t.Atom] = {
      case Js.Arr(Js.Str(REQREF),  v) => t.ReqRef (readJs[ReqId]    (v))
      case Js.Arr(Js.Str(CODEREF), v) => t.CodeRef(readJs[ReqCodeId](v))
    }

    def readTagRef(t: TagRef): PR[t.TagRef] =
      { case Js.Arr(Js.Str(TAGREF), v) => t.TagRef(readJs[ApplicableTagId](v)) }

    def readReqTitle(t: ReqTitle): PR[t.Atom] =
      readSingleLine(t) orElse readReqRef(t) orElse readTagRef(t) orElse readIssue(t)


//    def stuff(t: Generic)(implicit
//              lit:    Literal         = null,
//              nl:     NewLine         = null,
//              sl:     SingleLine      = null,
//              ml:     MultiLine       = null,
//              ptm:    PlainTextMarkup = null,
//              reqRef: ReqRef          = null,
//              tagRef: TagRef          = null,
//              issue:  Issue           = null): ReadWriter[t.Atom] = {
//
//      lazy val rw: Name[ReadWriter[t.Atom]] =
//        Need(ReadWriter(writeAny.write, pr))
//
//      @inline def castReader(a: Generic) = rw.asInstanceOf[Name[Reader[a.Atom]]]
//
//      def pr: PR[t.Atom] = {
//        var prs: List[PR[t.Atom]] = Nil
//
//        if (ml     ne null) prs ::= readMultiLine      (ml)(castReader(ml)) else
//        if (sl     ne null) prs ::= readSingleLine     (sl)
//        if (issue  ne null) prs ::= readIssue          (issue)
//        if (tagRef ne null) prs ::= readTagRef         (tagRef)
//        if (ptm    ne null) prs ::= readPlainTextMarkup(ptm)
//        if (reqRef ne null) prs ::= readReqRef         (reqRef)
//        if (nl     ne null) prs ::= readNewLine        (nl)
//        if (lit    ne null) prs ::= readLiteral        (lit)
//
//        println("PRs = "+(prs.length))
//        prs.reduce(_ orElse _)
//      }
//
//      rw.value
//    }

    def apply(t: Text.Generic)(pr: (t.type, Name[Reader[t.Atom]]) => PR[t.Atom]): (ReadWriter[t.OptionalText], ReadWriter[t.NonEmptyText]) = {
      type A = t.Atom
      implicit lazy val a: ReadWriter[A] = ReadWriter(writeAny.write, pr(t, Name(a)))
      val otxt  = mergeRW[Vector[A]]
      val netxt = mergeRW[NonEmptyVector[A]]
      (otxt, netxt)
    }
  }

  // Specific text types

  implicit final val (reqCodeGroupDesc, _) = TC(Text.ReqCodeGroupTitle)((t, _) =>
    TC.readReqTitle(t))

  implicit final val (genericReqDesc, _) = TC(Text.GenericReqTitle)((t, _) =>
    TC.readReqTitle(t))

  // lazy because TC.readIssue calls it
  implicit final lazy val (inlineIssueDesc, inlineIssueDescNE) = TC(Text.InlineIssueDesc)((t, _) =>
    TC.readSingleLine(t) orElse
    TC.readReqRef    (t) )

  implicit final val (_, customTextFieldText) = TC(Text.CustomTextField)((t, a) =>
    TC.readMultiLine(t)(a) orElse
    TC.readReqRef   (t)    orElse
    TC.readIssue    (t)    orElse
    TC.readTagRef   (t)    )

  // -------------------------------------------------------------------------------------------------------------------
  // Requirements

  implicit final val reqTypePos    = tagI(ReqTypePos.apply)
  implicit final def pubid[T <: ReqTypeId : Reader : Writer] = caseclass2(PubidT.apply[T], PubidT.unapply[T])
  implicit final val genericReqId  = tagL(GenericReqId.apply)
  implicit final val genericReq    = caseclass4(GenericReq.apply, GenericReq.unapply)
  implicit final val reqId         = _reqId
  implicit final val req           = _req
  implicit final val pubidRegister = xmap((_: PubidRegister).value)(PubidRegister.apply)
  implicit final val requirementsD = iMapK[ReqTypeId, ReqIdT, ReqT](ReqT.idProof)
  implicit final val requirements  = caseclass2(Requirements.apply, Requirements.unapply)
  implicit final val implications  = xmap((_: ReqFieldData.Implications).srcToTgt)(ReqFieldData.Implications)
  implicit final val reqFieldData  = caseclass3(ReqFieldData.apply, ReqFieldData.unapply)

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

  implicit final val reqCodeGroup  = caseclass1(ReqCodeGroup.apply, ReqCodeGroup.unapply)
  implicit final val reqCodeNode   = xmap[ReqCode.Node, String](_.value)(ReqCode.Node.applyFn)
  implicit final val reqCodeTarget = _reqCodeTarget
  implicit final val reqCodeId     = tagL(ReqCodeId.apply)
  implicit final val reqCodeAData  = caseclass2(ReqCode.ActiveData.apply, ReqCode.ActiveData.unapply)
  implicit final val reqCodeData   = caseclass3(ReqCode.Data.apply, ReqCode.Data.unapply)
  implicit final val reqCodeTrie   = (mtrie: ReadWriter[ReqCode.Trie])
  implicit final val reqCodes      = caseclass1(ReqCodes.apply, ReqCodes.unapply)

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
  implicit final val project = caseclass7(Project.apply, Project.unapply)
}

// =====================================================================================================================
object ProtocolDataCodecs {
  import shipreq.webapp.base.data._
  import CodecBase._
  import GenericCodecs._
  import DataCodecs._

  implicit final val deletionAction = enum(DeletionAction.values)

  def crudAction[I, V](implicit WI: Writer[I], RI: Reader[I], WV: Writer[V], RV: Reader[V]): ReadWriter[CrudAction[I, V]] =
    ReadWriter[CrudAction[I, V]]({
      case CrudAction.Create(v)    => Js.Arr(WV write v)
      case CrudAction.Update(i, v) => Js.Arr(WI write i, WV write v)
      case CrudAction.Delete(i, a) => Js.Arr(WI write i, deletionAction write a, Js.Obj())
    }, {
      case Js.Arr(v)       => CrudAction.Create(RV read v)
      case Js.Arr(i, v)    => CrudAction.Update(RI read i, RV read v)
      case Js.Arr(i, a, _) => CrudAction.Delete(RI read i, deletionAction read a)
    })

  // ------------------------------------------------------------------------------------
  // Field
  import shipreq.webapp.base.protocol.{FieldProtocol => FP}

  implicit final val fieldProtocolDelta = caseclass2(FP.Delta.apply, FP.Delta.unapply)
  implicit final val fieldProtocolValues = {
    import FP._, Field.ApplicableReqTypes
    ReadWriter[Values]({
      case TextFieldValues(a, b, c, d)     => intkeyW4(0, a, b, c, d)
      case TagFieldValues(a, b, c)         => intkeyW3(1, a, b, c)
      case ImplicationFieldValues(a, b, c) => intkeyW3(2, a, b, c)
    }, {
      case Js.Arr(Js.Num(n), a, b, c, d) => n.toInt match {
        case 0 => TextFieldValues(readJs[String](a), readJs[FieldRefKey](b), readJs[Mandatory](c), readJs[ApplicableReqTypes](d))
      }
      case Js.Arr(Js.Num(n), a, b, c) => n.toInt match {
        case 1 => TagFieldValues(readJs[TagId](a), readJs[Mandatory](b), readJs[ApplicableReqTypes](c))
        case 2 => ImplicationFieldValues(readJs[ReqTypeId](a), readJs[Mandatory](b), readJs[ApplicableReqTypes](c))
      }
    })
  }
  implicit final val fieldProtocolCfgAction = {
    import FP._, CfgAction._
    ReadWriter[CfgAction]({
      case Create(a)          => intkeyW (0, a)
      case UpdateValues(a, b) => intkeyW2(1, a, b)
      case UpdateOrder(a, b)  => intkeyW2(2, a, b)
      case Delete(a, b)       => intkeyW2(3, a, b)
    }, {
      case Js.Arr(Js.Num(n), a) => n.toInt match {
        case 0 => Create(readJs[Values](a))
      }
      case Js.Arr(Js.Num(n), a, b) => n.toInt match {
        case 1 => UpdateValues(readJs[CustomFieldId](a), readJs[Values        ](b))
        case 2 => UpdateOrder (readJs[FieldId      ](a), readJs[Position      ](b))
        case 3 => Delete      (readJs[FieldId      ](a), readJs[DeletionAction](b))
      }
    })
  }

  // ------------------------------------------------------------------------------------
  // Tag
  import shipreq.webapp.base.protocol.{TagProtocol => TP}

  implicit final val tagPovRelations     = caseclass2(TP.PovRelations.apply,        TP.PovRelations.unapply)
  implicit final val tagPov              = caseclass2(TP.PovTag.apply,              TP.PovTag.unapply)
  implicit final val tagGroupValues      = caseclass3(TP.TagGroupValues.apply,      TP.TagGroupValues.unapply)
  implicit final val applicableTagValues = caseclass3(TP.ApplicableTagValues.apply, TP.ApplicableTagValues.unapply)
  implicit final val tagValues           = ReadWriter[TP.Values]({
    case t: TP.TagGroupValues      => intkeyW(0, t)(tagGroupValues)
    case t: TP.ApplicableTagValues => intkeyW(1, t)(applicableTagValues)
  }, {
    case Js.Arr(Js.Num(n), v) => n.toInt match {
      case 0 => readJs(v)(tagGroupValues)
      case 1 => readJs(v)(applicableTagValues)
    }
  })
}

// =====================================================================================================================
object ProtocolRemoteCodecs {
  import CodecBase._
  import Routines._

  def remoteRoutine[R <: Routine.Desc](d: R): ReadWriter[d.Remote] =
    ReadWriter[d.Remote](r => Js.Str(r.n), { case Js.Str(n) => Routine.Remote(n, d) })

  implicit final val projectInit   = remoteRoutine(ProjectInit)
  implicit final val issueTypeCrud = remoteRoutine(CustomIssueTypeCrud)
  implicit final val reqTypeCrud   = remoteRoutine(CustomReqTypeCrud)
  implicit final val reqTypeImpMod = remoteRoutine(ReqTypeImplicationMod)
  implicit final val fieldMandMod  = remoteRoutine(FieldMandatorinessMod)
  implicit final val fieldCrud     = remoteRoutine(FieldCrud)
  implicit final val tagCrud       = remoteRoutine(TagCrud)

  implicit final val projectSPA = caseclass7(ProjectSPA.apply, ProjectSPA.unapply)
}

// =====================================================================================================================
object DeltaCodecs {
  import shipreq.webapp.base.delta._
  import CodecBase._
  import DataCodecs.rev

  implicit final val partitions = enum(Partition.values)

  implicit final val remoteDeltaGW = Writer[RemoteDeltaG](r => {
    import r.p.{wi, wd}
    val dp = r.forceDeltaP[r.p.type](r.p)
    val a = partitions write r.p
    val b = rev write r.from
    val c = rev write r.to
    val d = writeIterable(dp.del)(wi)
    val e = writeIterable(dp.upd)(wd)
    Js.Arr(a, b, c, d, e)
  })

  implicit final val remoteDeltaGR = Reader[RemoteDeltaG]({
    case Js.Arr(a, b, c, Js.Arr(d@_*), Js.Arr(e@_*)) =>
      val p = partitions read a
      val f = rev read b
      val t = rev read c
      val x = p.ri.readSet(d)(p.ui)
      val y = p.rd.readList(e)
      RemoteDeltaG(p, f, t)(x, y)
  })

//  implicit final val remoteDelta: ReadWriter[RemoteDelta] = implicitly[ReadWriter[List[RemoteDeltaG]]]
  implicit final val remoteDelta = ReadWriter[RemoteDelta](
    SeqishW[RemoteDeltaG, List].write,
    SeqishR[RemoteDeltaG, List].read)
}