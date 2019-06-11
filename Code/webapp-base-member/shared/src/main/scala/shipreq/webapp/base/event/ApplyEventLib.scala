package shipreq.webapp.base.event

import japgolly.microlibs.utils.Utils
import monocle._
import scala.reflect.ClassTag
import scalaz.{-\/, Equal, \/, \/-}
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.reqtable.SavedView
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.util.GenericData
import shipreq.webapp.base.validation.{Composite, Simple}

private[event] object ApplyEventLib {
  type Error = String
  val SE = StateEither.Fix[Project, Error]
  import SE._

  /** Has a subject been validated or not yet? */
  sealed trait Validated extends IsoBool.WithBoolOps[Validated] {
    override final def companion = Validated
    def mapValidated[I, O](input: I)(validate: I => SE[I])(f: I => SE[O]): SE[O]
  }

  case object Validated extends Validated with IsoBool.Object[Validated] {
    override def positive = Validated
    override def negative = Unvalidated
    override def mapValidated[I, O](input: I)(validate: I => SE[I])(f: I => SE[O]): SE[O] = f(input)
  }

  case object Unvalidated extends Validated {
    override def mapValidated[I, O](input: I)(validate: I => SE[I])(f: I => SE[O]): SE[O] =
      validate(input) >>= f
  }

  // ===================================================================================================================
  // Trust & validation

  final class TrustedAlt[+A](val trusted: A) extends AnyVal
  object TrustedAlt {
    implicit val unit    = new TrustedAlt[SE[Unit]       ](nop)
    implicit def endo[A] = new TrustedAlt[A => SE[A]     ](ret)
    implicit val toUnit  = new TrustedAlt[Any => SE[Unit]](_nop)
  }

  @inline def whenUntrusted[A](a: => A)(implicit trust: Trust, alt: TrustedAlt[A]): A =
    if (trust is Trusted) alt.trusted else a

  implicit def resultFromValidationResult[A](r: Composite.Invalidity \/ A): SE[A] =
    r match {
      case \/-(s) => ret(s)
      case -\/(f) => fail(Composite.Invalidity.toText(f))
    }

  def validateA[A](v: => Composite.Stateless[A, A, A])(implicit trust: Trust, eq: Equal[A]): A => SE[A] =
    whenUntrusted(_validate(v))

  def validateO[I, O](v: => Composite.Stateless[I, O, O])(implicit trust: Trust, eq: Equal[I]): O => SE[O] =
    whenUntrusted(o => _validate(v)(v.corrector.uncorrect(o)))

  def validateI[I, O](v: => Composite.Stateless[I, I, O])(f: O => I)(implicit trust: Trust, eq: Equal[I]): O => SE[O] =
    whenUntrusted(o => _validate(v)(f(o)))

  private def _validate[I, C, O](v: Composite.Stateless[I, C, O])(i: I)(implicit trust: Trust, eq: Equal[I]): SE[O] = {
    val c = v.corrector(i)
    if (eq.equal(i, v.corrector.uncorrect(c)))
      v.named.auditor(c)
    else
      fail(s"Preprocessing not applied to ${v.name}:\na: [$i]\ne: [$c]")
  }

  def ensureNone[A](oa: Option[A])(err: A => String)(implicit trust: Trust): SE[Unit] =
    whenUntrusted(oa.fold(nop)(a => fail(err(a))))

  def ensureDistinct[A](field: String, as: => TraversableOnce[A])(implicit trust: Trust, u: UnivEq[A]): SE[Unit] =
    whenUntrusted {
      val dups = Utils.dups(as)
      if (dups.isEmpty) nop else SE.fail(s"Duplicates found in $field: ${dups.toVector.distinct.mkString(", ")}")
    }

  def ensureLiveIs(actual: Live)(expect: Live, name: => String)(implicit trust: Trust): SE[Unit] =
    whenUntrusted(
      expect match {
        case Live => ensureLive(actual)(name)
        case Dead => ensureDead(actual)(name)
      })

  @inline def ensureLiveIsNot(actual: Live)(expectNot: Live, name: => String)(implicit trust: Trust): SE[Unit] =
    ensureLiveIs(actual)(!expectNot, name)

  def ensureLive(l: Live)(name: => String)(implicit trust: Trust): SE[Unit] =
    whenUntrusted(test(l is Live, s"$name is dead."))

  def ensureDead(l: Live)(name: => String)(implicit trust: Trust): SE[Unit] =
    whenUntrusted(test(l is Dead, s"$name is live."))

  def ensureTagIsLive(id: TagId)(implicit trust: Trust): SE[Unit] =
    whenUntrusted(
      for {
        p <- SE.get
        t <- imapNeed(p.config.tags.tree)(id)
        _ <- ensureLive(t.tag.live)(show(id))
      } yield ())

  def ensureReqTypeIsLive(id: ReqTypeId)(implicit trust: Trust): SE[Unit] =
    whenUntrusted(
      id.foldId(f => ensureLive(f.live)(show(id)), ensureCustomReqTypeIsLive))

  def ensureCustomReqTypeIsLive(id: CustomReqTypeId)(implicit trust: Trust): SE[Unit] =
    whenUntrusted(
      for {
        p  <- SE.get
        rt <- imapNeed(p.config.reqTypes.custom)(id)
        _  <- ensureLive(rt.live)(show(id))
      } yield ())

  // ===================================================================================================================
  // Lib

  def show(v: ReqCode.Value   ): String = s"Code [${PlainText reqCode v}]"
  def show(v: ReqCodeId       ): String = s"Code #${v.value}"
  def show(v: TagId           ): String = s"Tag #${v.value}"
  def show(v: CustomFieldId   ): String = v.toString
  def show(v: Req             ): String = show(v.id)
  def show(v: ReqId           ): String = v.toString
  def show(v: CustomField     ): String = s"Field #${v.id.value}"
  def show(v: CustomField.Text): String = s"Text field [${v.name}]"
  def show(v: ReqType         ): String = s"ReqType [${v.mnemonic.value}]"
  def show(v: ReqTypeId       ): String = v.foldId(r => show(r: ReqType), show)
  def show(v: CustomReqTypeId ): String = s"ReqType #${v.value}"
  def show(v: UseCaseId       ): String = s"Use case #${v.value}"
  def show(v: UseCaseStepId   ): String = s"Use case step #${v.value}"
  def show(v: SavedView.Id    ): String = s"Saved view #${v.value}"

  def showLoc(v: VectorTree.Location): String = v.whole.mkString("loc ", ":", "")

  def set1[A](a: A): Set[A] =
    Set.empty[A] + a

  @inline implicit def projectModToSE(f: Project => Project): SE[Unit] =
    mod(f)

  def lensMod[A](l: Lens[Project, A])(mod: A => SE[A]): SE[Unit] =
    SE.get(l.get) >>= mod >>= l.set

  def fieldUpdateFn[S, T, A, B](l: PLens[S, T, A, B]): B => S => SE[T] =
    b => {
      // TODO Modify to reject NOP changes?
      val f = l set b
      s => ret(f(s))
    }

  implicit class AToSeBOps[A, B](private val f: A => SE[B]) extends AnyVal {
    def thenUpdate[C, D](g: (C, B) => D): A => C => SE[D] =
      f.andThen(seb => c => seb.map(g(c, _)))

    def thenUpdateBC[C, D](g: B => C => D): A => C => SE[D] =
      thenUpdate[C, D]((c, b) => g(b)(c))

    def thenUpdateCB[C, D](g: C => B => D): A => C => SE[D] =
      //a => f(a) |> g(_)
      thenUpdate[C, D]((c, b) => g(c)(b))

    def >>=@[C, D](l: PLens[C, D, A, B]): A => C => SE[D] =
      thenUpdateBC(l.set)
  }

  def optionalModSE[A](l: monocle.Optional[Project, A], notFound: => String)(mod: A => SE[A]): SE[Unit] =
    for {
      p   ← SE.get
      sv1 ← optionGet(l.getOption(p), notFound)
      sv2 ← mod(sv1)
      _   ← l.set(sv2)
    } yield ()

  def foldMapBind[A, B](b: B, as: Iterable[A])(f: A => B => SE[B]): SE[B] =
    // ret(b).foldMapBind(as)(f)
    Util.mapReduceB(as, ret(b))(f(_)(b), f)(_ >>= _)

  def foldMapBindFns[A, B](as: Iterable[A])(f: A => B => SE[B]): B => SE[B] =
    // as.foldLeft(ret[B] _)((x, y) => x.andThen(_ >>= f(y)))
    Util.mapReduce[A, B => SE[B]](as, ret)(f, (x, y) => x.andThen(_ >>= y))

  def optionGet[A](oa: Option[A], err: => String): SE[A] =
    oa.fold[SE[A]](fail(err))(ret)

  def optionGetR[A](p: Project, oa: Option[A], err: => String): Result[A] =
    oa.fold[Result[A]](Failure(err))(Ok(p, _))

  def narrowCC[A, B <: A](a: A)(implicit cc: ClassTag[B], trust: Trust): SE[B] =
    narrowCC[A, B](a, s"${cc.runtimeClass.getSimpleName} ∌ $a")

  def narrowCC[A, B <: A](a: A, failure: => Error)(implicit cc: ClassTag[B], trust: Trust): SE[B] =
    if (trust is Untrusted)
      cc.unapply(a) match {
        case Some(b) => ret(b)
        case None    => fail(failure)
      }
    else
      ret(a.asInstanceOf[B])

  def appendNewToVector[A: UnivEq](implicit trust: Trust): A => Vector[A] => SE[Vector[A]] = {
    def doit(a: A, as: Vector[A]): Vector[A] = as :+ a
    if (trust is Trusted)
      a => as => ret(doit(a, as))
    else
      a => as =>
        if (as.exists(_ ==* a))
          fail(s"Element $a already exists in $as.")
        else
          ret(doit(a, as))
  }

  def removeFromVector[A: UnivEq](implicit trust: Trust): A => Vector[A] => SE[Vector[A]] = {
    def doit(a: A, as: Vector[A]): Vector[A] = as.filterNot(_ ==* a)
    if (trust is Trusted)
      a => as => ret(doit(a, as))
    else
      a => as => {
        val o = doit(a, as)
        if (o.length == as.length)
          fail(s"Element not found: Expected to find $a in $as.")
        else
          ret(o)
      }
  }

  def repositionFn[A: UnivEq](implicit trust: Trust): (A, Option[A]) => Vector[A] => SE[Vector[A]] =
    if (trust is Trusted)
      (a, pos) => as => ret(RelPos.set(as, a, pos))
    else
      (a, pos) => as =>
        if (!as.exists(_ ==* a))
          fail(s"Element not found: Expected to find $a in $as.")
        else
          ret(RelPos.set(as, a, pos))

  def updateIdCeilingFn(lens: Lens[IdCeilings, Int]): Int => SE[Unit] =
    x => mod(Project.idCeilings.modify(_.update(lens, x)))

  def imapNeed[K, V](imap: IMap[K, V])(k: K): SE[V] =
    SE.retOption(imap get k, s"$k not found.")

  def imapCreate[K, V](imap: IMap[K, V])(v: V)(implicit trust: Trust): SE[IMap[K, V]] = {
    val updated = imap + v
    if (trust is Trusted)
      ret(updated)
    else
      SE.test(!imap.containsV(v), s"$v already exists.") |>> updated
  }

  def toggleLiveCheckBeforeAfter[V](v1: V, newLive: Live)(get: V => Live, set: Live => V => V, name: => String)(implicit trust: Trust): SE[V] = {
    val v2 = set(newLive)(v1)
    trust match {
      case Trusted =>
        SE ret v2
      case Untrusted =>
        for {
          _ <- ensureLiveIsNot(newLive)(get(v1), name)
          _ <- ensureLiveIs(newLive)(get(v2), name + " after change")
        } yield v2
    }
  }

  sealed abstract class LiveAccessor[V] {
    val ensureLive: V => SE[Unit]
    val ensureDead: V => SE[Unit]
    val makeLive  : V => SE[V]
    val makeDead  : V => SE[V]
  }
  def LiveAccessor[V](l: Lens[V, Live])(name: V => String)(implicit trust: Trust): LiveAccessor[V] = {
    val setLive = l set Live
    val setDead = l set Dead
    trust match {
      case Trusted =>
        new LiveAccessor[V] {
          override val ensureLive: V => SE[Unit] = _nop
          override val ensureDead: V => SE[Unit] = _nop
          override val makeLive  : V => SE[V]    = v => ret(setLive(v))
          override val makeDead  : V => SE[V]    = v => ret(setDead(v))
        }
      case Untrusted =>
        new LiveAccessor[V] {
          override val ensureLive: V => SE[Unit] = v => ApplyEventLib.ensureLive(l get v)(name(v))
          override val ensureDead: V => SE[Unit] = v => ApplyEventLib.ensureDead(l get v)(name(v))
          override val makeLive  : V => SE[V]    = v => ensureDead(v) |>> setLive(v)
          override val makeDead  : V => SE[V]    = v => ensureLive(v) |>> setDead(v)
        }
    }
  }

  // -------------------------------------------------------------------------------------------------------------------
  @inline def IMapStore[Id, Data](L: Lens[Project, IMap[Id, Data]])(implicit trust: Trust) =
    new IMapStore(L)(trust)

  class IMapStore[Id, Data](L: Lens[Project, IMap[Id, Data]])(implicit trust: Trust) {
    final def ensureAbsent(id: Id): SE[Unit] =
      whenUntrusted(test(p => !L.get(p).containsK(id), s"$id already exists."))

    final def ensurePresent(id: Id): SE[Unit] =
      whenUntrusted(test(p => L.get(p).containsK(id), s"$id not found."))

    final def ensureAbsentData(data: Data): SE[Unit] =
      whenUntrusted(test(p => !L.get(p).containsV(data), s"$data already exists."))

    final def need(id: Id): SE[Data] =
      getE(p => L.get(p).attempt(id))

    final def create(data: Data): SE[Unit] =
      ensureAbsentData(data) >> addOrUpdate(data)

    final def addOrUpdate(data: Data): SE[Unit] =
      StateEither.mod(L.modify(_ add data))

    final def update(id: Id, updateFn: Data => SE[Data]): SE[Unit] =
      need(id) >>= updateFn >>= addOrUpdate

    final def updateF(id: Id, updateFn: Data => Data): SE[Unit] =
      need(id) |> updateFn >>= addOrUpdate

    final def hardDelete(id: Id): SE[Unit] =
      ensurePresent(id) >> StateEither.mod(L.modify(_ - id))
  }

  // -------------------------------------------------------------------------------------------------------------------
  @inline def IMapStoreL[Id, Data](L: Lens[Project, IMap[Id, Data]])(liveLens: Lens[Data, Live])(implicit trust: Trust) =
    new IMapStoreL(L, liveLens)(trust)

  final class IMapStoreL[Id, Data](L: Lens[Project, IMap[Id, Data]], liveLens: Lens[Data, Live])(implicit trust: Trust) extends IMapStore(L) {
    def updateLive(id: Id, updateFn: Data => SE[Data]): SE[Unit] =
      update(id, d =>
        ensureLive(liveLens get d)(id.toString) >> updateFn(d))

    def setLive(id: Id, newValue: Live): SE[Unit] =
      update(id, d =>
        ensureLiveIsNot(liveLens get d)(newValue, id.toString) |>> liveLens.set(newValue)(d))
  }

  // -------------------------------------------------------------------------------------------------------------------
  @inline def GenericDataApp[D](gd: GenericData): GenericDataApp {type Data = D; val ^ : gd.type} =
    new GenericDataApp {
      override type Data = D
      override val ^ : gd.type = gd
    }

  sealed abstract class GenericDataApp {
    val ^ : GenericData
    type Data

    /**
     * Get a value which must exist.
     *
     * @return Failure unless value exists.
     */
    final def need[A](a: ^.Attr)(implicit vs: ^.NonEmptyValues): SE[a.Data] =
      a.get(vs) match {
        case Some(v) => ret(v.value)
        case None    => fail(s"Attribute $a required but missing from [${vs.value.values mkString ", "}].")
      }

    /**
     * Get a value if it exists. Use a default otherwise.
     */
    final def want[A](a: ^.Attr)(default: a.Data)(implicit vs: ^.NonEmptyValues): a.Data =
      a.get(vs) match {
        case Some(v) => v.value
        case None    => default
      }

    /**
     * Get a value if it exists.
     */
    final def read[A](a: ^.Attr)(implicit vs: ^.NonEmptyValues): Option[a.Data] =
      a.get(vs).map(_.value)

    final def updateEachValue(updateFn: ^.Value => Data => SE[Data]): ^.NonEmptyValues => Data => SE[Data] =
      vs => foldMapBindFns(vs.values)(updateFn)
  }
}
