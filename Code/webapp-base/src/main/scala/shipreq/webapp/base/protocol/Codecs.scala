package shipreq.webapp.base.protocol

import scalaz.{OneAnd, NonEmptyList, \&/, \/, -\/, \/-}
import scalaz.Isomorphism.<=>

import upickle._
import upickle.Fns._
import upickle.TupleCodecs._
import upickle.StdlibCodecs.{SeqishR, SeqishW}
import upickle.StdlibCodecs.{MapR, MapW}

import shipreq.base.util._
import shipreq.base.util.TaggedTypes._
import shipreq.webapp.base.data._
import DataImplicits._

private[protocol] object CodecBase {

  def tagS[T <: TaggedString](C: String => T) =
    ReadWriter[T](i => Js.Str(i.value), { case Js.Str(i) => C(i)})

  def tagL[T <: TaggedLong](C: Long => T) =
    ReadWriter[T](i => Js.Str(i.value.toString), { case Js.Str(i) => C(i.toLong)})

  def boolCase[T](iso: Boolean <=> T) =
    ReadWriter[T](t => if (iso from t) Js.Num(1) else Js.Num(0), {
      case Js.Num(n) => iso to (n.toInt != 0)
    })

  // UNSAFE. Make sure tests using exhaustive pattern matching to cover this hierarchy
  def enum[T](ts: NonEmptyList[T]) = {
    val table = BiMap(ts.list.zipWithIndex.map(p => p._1 -> ('0' + p._2).toChar.toString).toMap)
    ReadWriter[T](t => Js.Str(table.ab(t)), {
      case Js.Str(k) if table.ba.contains(k) => table.ba(k)
    })
  }

  def xmap[A, B](f: A => B)(g: B => A)(implicit RB: Reader[B], WB: Writer[B]) =
    ReadWriter[A](WB.write compose f, RB.read andThen g)

  implicit class ReaderExt[T](val r: Reader[T]) extends AnyVal {
    @inline def readSet[TT >: T](s: Seq[Js.Value]): Set[TT] =
      foldlSeq[Set[TT]](s, Set.empty)(_ + _)

    @inline def readList(s: Seq[Js.Value]): List[T] =
      foldlSeq[List[T]](s, Nil)((a, b) => b :: a)
      // foldrSeq[List[T]](s, Nil)(_ :: _)

    @inline def foldlSeq[B](s: Seq[Js.Value], z: B)(f: (B, T) => B): B =
      s.foldLeft(z)((b, j) => f(b, r read j))

    @inline def foldrSeq[B](s: Seq[Js.Value], z: B)(f: (T, B) => B): B =
      s.foldRight(z)((j, b) => f(r read j, b))
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

  def strkeyW[T](k: String, t: T)(implicit T: Writer[T]) =
    Js.Arr(Js.Str(k), T write t)

  def iMap[K: Reader : Writer, V: Reader : Writer](key: V => K): ReadWriter[IMap[K, V]] =
    xmap((_: IMap[K, V]).underlyingMap)(m => IMap.empty(key).replaceUnderlying(m))

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
import CodecBase._

// =====================================================================================================================
object DataCodecs {

  @inline implicit def string = BaseCodecs.StringRW
  @inline implicit def unit   = BaseCodecs.UnitRW

  implicit def option[A: Reader: Writer]: ReadWriter[Option[A]] =
    ReadWriter[Option[A]](
    _.fold(Js.Arr())(a => Js.Arr(writeJs(a))), {
      case Js.Arr()  => None
      case Js.Arr(a) => Some(readJs[A](a))
    })

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

  implicit def isubset[F[_], A: Reader: Writer](implicit RF: Reader[F[A]], WF: Writer[F[A]]): ReadWriter[ISubset[F, A]] = {
    import ISubset._
    ReadWriter[ISubset[F, A]]({
      case All()    => Js.Num(0)
      case Only(as) => intkeyW2(1, as.head, as.tail)
      case Not(as)  => intkeyW2(2, as.head, as.tail)
    }, {
      case Js.Num(n) if n.toInt == 0 => All()
      case Js.Arr(Js.Num(n), a, as) => n.toInt match {
        case 1 => Only(OneAnd(readJs[A](a), readJs[F[A]](as)))
        case 2 => Not (OneAnd(readJs[A](a), readJs[F[A]](as)))
      }
    })
  }

  @inline implicit def iMapAuto[K: Reader : Writer, V: Reader : Writer](implicit d: DataIdAux[V, K]): ReadWriter[IMap[K, V]] =
    iMap(d.id)

  @inline implicit def revAnd[T](implicit WT: Writer[T], RT: Reader[T]) =
    caseclass2(RevAnd.apply[T], RevAnd.unapply[T])(rev, rev, RT, WT)

  implicit final val rev            = tagL(Rev.apply)
  implicit final val alive          = boolCase(Alive)
  implicit final val impReq         = boolCase(ImplicationRequired)
  implicit final val mutexChildren  = boolCase(MutexChildren)
  implicit final val mandatory      = boolCase(Mandatory)
  implicit final val deletable      = boolCase(Deletable)
  implicit final val hashRefKey     = tagS(HashRefKey.apply)
  implicit final val fieldRefKey    = tagS(FieldRefKey.apply)
  implicit final val deletionAction = enum(DeletionAction.values)

  implicit final val customIssueTypeId = tagL(CustomIssueType.Id.apply)
  implicit final val customIssueType   = caseclass4(CustomIssueType.apply, CustomIssueType.unapply)
  implicit final val reqTypeId = {
    import StaticReqType._
    ReadWriter[ReqType.Id]({
      case i: CustomReqType.Id => Js.Str(i.value.toString)
      case UseCase             => Js.Str("u")
    }, {
      case Js.Str(ParseLong(i)) => CustomReqType.Id(i)
      case Js.Str("u")          => UseCase
    })
  }

  implicit final val reqTypeMnemonic = tagS(ReqType.Mnemonic.apply)
  implicit final val customReqTypeId = tagL(CustomReqType.Id.apply)
  implicit final val customReqType   = caseclass6(CustomReqType.apply, CustomReqType.unapply)

  implicit final val tagGroupId      = tagL(TagGroup.Id.apply)
  implicit final val applicableTagId = tagL(ApplicableTag.Id.apply)
  implicit final val tagId           = tagIdRW
  implicit final val tagGroup        = caseclass5(TagGroup.apply, TagGroup.unapply)
  implicit final val applicableTag   = caseclass5(ApplicableTag.apply, ApplicableTag.unapply)
  implicit final val tag             = tagRW
  implicit final val tagInTree       = caseclass2(TagInTree.apply, TagInTree.unapply)
  implicit final val tagTree         = iMap[Tag.Id, TagInTree](_.tag.id)
  private[this] def tagIdRW = ReadWriter[Tag.Id]({
    case i: ApplicableTag.Id => intkeyW(0, i)(applicableTagId)
    case i: TagGroup     .Id => intkeyW(1, i)(tagGroupId)
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

  implicit final val customFieldImplId = tagL(CustomField.Implication.Id.apply)
  implicit final val customFieldTextId = tagL(CustomField.Text       .Id.apply)
  implicit final val customFieldTagId  = tagL(CustomField.Tag        .Id.apply)
  implicit final val customFieldId     = ReadWriter[CustomField.Id]({
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
  implicit final val fieldId = ReadWriter[Field.Id]({
    case i: CustomField.Id => writeJs(i)
    case i: StaticField    => writeJs(i)
  },
    // Shape determines type. Arr(Str(_), _) or Str(_)
    customFieldId.read orElse staticField.read
  )
  implicit final val fieldSet = caseclass2(FieldSet.apply, FieldSet.unapply)

  implicit final val project = caseclass4(Project.apply, Project.unapply)
}

import DataCodecs._

// =====================================================================================================================
object ProtocolDataCodecs {

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
        case 1 => TagFieldValues(readJs[Tag.Id](a), readJs[Mandatory](b), readJs[ApplicableReqTypes](c))
        case 2 => ImplicationFieldValues(readJs[ReqType.Id](a), readJs[Mandatory](b), readJs[ApplicableReqTypes](c))
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
        case 1 => UpdateValues(readJs[CustomField.Id](a), readJs[Values        ](b))
        case 2 => UpdateOrder (readJs[Field.Id      ](a), readJs[Position      ](b))
        case 3 => Delete      (readJs[Field.Id      ](a), readJs[DeletionAction](b))
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
      val x = p.ri.readSet(d)
      val y = p.rd.readList(e)
      RemoteDeltaG(p, f, t)(x, y)
  })

//  implicit final val remoteDelta: ReadWriter[RemoteDelta] = implicitly[ReadWriter[List[RemoteDeltaG]]]
  implicit final val remoteDelta = ReadWriter[RemoteDelta](
    SeqishW[RemoteDeltaG, List].write,
    SeqishR[RemoteDeltaG, List].read)
}