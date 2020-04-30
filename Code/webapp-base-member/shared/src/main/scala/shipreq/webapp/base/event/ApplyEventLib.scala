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
import shipreq.webapp.base.validation.Composite

private[event] object ApplyEventLib {
  type Error = String

  val Eval = EitherState.ForTypes[Project, Error]
  type Eval[A] = Eval.Instance[A]
  implicit val evalUnderlyingMonad = Eval.eitherStateUnderlyingMonad
  implicit val evalMonad = Eval.eitherStateMonad

  type EndoFn[A] = A => A

  /** Has a subject been validated or not yet? */
  sealed trait Validated extends IsoBool.WithBoolOps[Validated] {
    override final def companion = Validated
    def mapValidated[I, O](input: I)(validate: I => Eval[I])(f: I => Eval[O]): Eval[O]
  }

  case object Validated extends Validated with IsoBool.Object[Validated] {
    override def positive = Validated
    override def negative = Unvalidated
    override def mapValidated[I, O](input: I)(validate: I => Eval[I])(f: I => Eval[O]): Eval[O] = f(input)
  }

  case object Unvalidated extends Validated {
    override def mapValidated[I, O](input: I)(validate: I => Eval[I])(f: I => Eval[O]): Eval[O] =
      validate(input).flatMap(f)
  }

  // ===================================================================================================================
  // Trust & validation

  final class TrustedAlt[+A](val trusted: A) extends AnyVal
  object TrustedAlt {
    implicit val unit    = new TrustedAlt[Eval[Unit]       ](Eval.unit)
    implicit def endo[A] = new TrustedAlt[A => Eval[A]     ](Eval.pure)
    implicit val toUnit  = new TrustedAlt[Any => Eval[Unit]](Eval._unit)
  }

  @inline def whenUntrusted[A](a: => A)(implicit trust: Trust, alt: TrustedAlt[A]): A =
    if (trust is Trusted) alt.trusted else a

  implicit def resultFromValidationResult[A](r: Composite.Invalidity \/ A): Eval[A] =
    r match {
      case \/-(s) => Eval.pure(s)
      case -\/(f) => Eval.fail(Composite.Invalidity.toText(f))
    }

  def validateA[A](v: => Composite.Stateless[A, A, A])(implicit trust: Trust, eq: Equal[A]): A => Eval[A] =
    whenUntrusted(_validate(v))

  def validateO[I, O](v: => Composite.Stateless[I, O, O])(implicit trust: Trust, eq: Equal[I]): O => Eval[O] =
    whenUntrusted(o => _validate(v)(v.corrector.uncorrect(o)))

  def validateI[I, O](v: => Composite.Stateless[I, I, O])(f: O => I)(implicit trust: Trust, eq: Equal[I]): O => Eval[O] =
    whenUntrusted(o => _validate(v)(f(o)))

  private def _validate[I, C, O](v: Composite.Stateless[I, C, O])(i: I)(implicit eq: Equal[I]): Eval[O] = {
    val c = v.corrector(i)
    if (eq.equal(i, v.corrector.uncorrect(c)))
      v.named.auditor(c)
    else
      Eval.fail(s"Preprocessing not applied to ${v.name}:\na: [$i]\ne: [$c]")
  }

  def ensureNone[A](oa: Option[A])(err: A => String)(implicit trust: Trust): Eval[Unit] =
    whenUntrusted(oa.fold(Eval.unit)(a => Eval.fail(err(a))))

  def ensureDistinct[A](field: String, as: => IterableOnce[A])(implicit trust: Trust, u: UnivEq[A]): Eval[Unit] =
    whenUntrusted {
      val dups = Utils.dups(as)
      Eval.test(dups.isEmpty, s"Duplicates found in $field: ${dups.toVector.distinct.mkString(", ")}")
    }

  def ensureLiveIs(actual: Live)(expect: Live, name: => String)(implicit trust: Trust): Eval[Unit] =
    whenUntrusted(
      expect match {
        case Live => ensureLive(actual)(name)
        case Dead => ensureDead(actual)(name)
      })

  @inline def ensureLiveIsNot(actual: Live)(expectNot: Live, name: => String)(implicit trust: Trust): Eval[Unit] =
    ensureLiveIs(actual)(!expectNot, name)

  def ensureLive(l: Live)(name: => String)(implicit trust: Trust): Eval[Unit] =
    whenUntrusted(Eval.test(l is Live, s"$name is dead."))

  def ensureDead(l: Live)(name: => String)(implicit trust: Trust): Eval[Unit] =
    whenUntrusted(Eval.test(l is Dead, s"$name is live."))

  def ensureTagIsLive(id: TagId)(implicit trust: Trust): Eval[Unit] =
    whenUntrusted(
      for {
        p <- Eval.get
        t <- imapNeed(p.config.tags.tree)(id)
        _ <- ensureLive(t.tag.live)(show(id))
      } yield ())

  def ensureReqTypeIsLive(id: ReqTypeId)(implicit trust: Trust): Eval[Unit] =
    whenUntrusted(
      id.foldId(f => ensureLive(f.live)(show(id)), ensureCustomReqTypeIsLive))

  def ensureCustomReqTypeIsLive(id: CustomReqTypeId)(implicit trust: Trust): Eval[Unit] =
    whenUntrusted(
      for {
        p  <- Eval.get
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
  def show(v: ManualIssueId   ): String = s"Manual issue #${v.value}"

  def showLoc(v: VectorTree.Location): String = v.whole.mkString("loc ", ":", "")

  def set1[A](a: A): Set[A] =
    Set.empty[A] + a

  @inline implicit def projectModToSE(f: Project => Project): Eval[Unit] =
    Eval.mod(f)

  def lensMod[A](l: Lens[Project, A])(mod: A => Eval[A]): Eval[Unit] =
    Eval.gets(l.get).flatMap(mod).flatMap(l.set)

  def fieldUpdateFn[S, T, A, B](l: PLens[S, T, A, B]): B => S => Eval[T] =
    b => {
      // TODO Modify to reject NOP changes?
      val f = l set b
      s => Eval.pure(f(s))
    }

  implicit class AToSeBOps[A, B](private val f: A => Eval[B]) extends AnyVal {
    def thenUpdate[C, D](g: (C, B) => D): A => C => Eval[D] =
      f.andThen(seb => c => seb.map(g(c, _)))

    def thenUpdateBC[C, D](g: B => C => D): A => C => Eval[D] =
      thenUpdate[C, D]((c, b) => g(b)(c))

    def thenUpdateCB[C, D](g: C => B => D): A => C => Eval[D] =
      //a => f(a) |> g(_)
      thenUpdate[C, D]((c, b) => g(c)(b))

    def >>=@[C, D](l: PLens[C, D, A, B]): A => C => Eval[D] =
      thenUpdateBC(l.set)
  }

  def optionalModEval[A](l: monocle.Optional[Project, A], notFound: => String)(mod: A => Eval[A]): Eval[Unit] =
    for {
      p   <- Eval.get
      sv1 <- Eval.some(l.getOption(p), notFound)
      sv2 <- mod(sv1)
      _   <- l.set(sv2)
    } yield ()

  def foldMapBind[A, B](b: B, as: Iterable[A])(f: A => B => Eval[B]): Eval[B] =
    as.foldLeft(Eval.pure(b))((x, a) => x.flatMap(f(a)))

  def foldMapBindFns[A, B](as: Iterable[A])(f: A => B => Eval[B]): B => Eval[B] = {
    val fs = as.map(f)
    b => fs.foldLeft(Eval.pure(b))((x, y) => x.flatMap(y))
  }

  def narrowCC[A, B <: A](a: A, failure: => Error)(implicit cc: ClassTag[B], trust: Trust): Eval[B] =
    if (trust is Untrusted)
      cc.unapply(a) match {
        case Some(b) => Eval.pure(b)
        case None    => Eval.fail(failure)
      }
    else
      Eval.pure(a.asInstanceOf[B])

  def narrowCC[A, B <: A](a: A)(implicit cc: ClassTag[B], trust: Trust): Eval[B] =
    narrowCC[A, B](a, s"${cc.runtimeClass.getSimpleName} ∌ $a")

  def appendNewToVector[A: UnivEq](implicit trust: Trust): A => Vector[A] => Eval[Vector[A]] = {
    def doit(a: A, as: Vector[A]): Vector[A] = as :+ a
    if (trust is Trusted)
      a => as => Eval.pure(doit(a, as))
    else
      a => as =>
        if (as.exists(_ ==* a))
          Eval.fail(s"Element $a already exists in $as.")
        else
          Eval.pure(doit(a, as))
  }

  def removeFromVector[A: UnivEq](implicit trust: Trust): A => Vector[A] => Eval[Vector[A]] = {
    def doit(a: A, as: Vector[A]): Vector[A] = as.filterNot(_ ==* a)
    if (trust is Trusted)
      a => as => Eval.pure(doit(a, as))
    else
      a => as => {
        val o = doit(a, as)
        if (o.length == as.length)
          Eval.fail(s"Element not found: Expected to find $a in $as.")
        else
          Eval.pure(o)
      }
  }

  def repositionFn[A: UnivEq](implicit trust: Trust): (A, Option[A]) => Vector[A] => Eval[Vector[A]] =
    if (trust is Trusted)
      (a, pos) => as => Eval.pure(RelPos.set(as, a, pos))
    else
      (a, pos) => as =>
        if (!as.exists(_ ==* a))
          Eval.fail(s"Element not found: Expected to find $a in $as.")
        else
          Eval.pure(RelPos.set(as, a, pos))

  def updateIdCeilingFn(lens: Lens[IdCeilings, Int]): Int => Eval[Unit] =
    x => Eval.mod(Project.idCeilings.modify(_.update(lens, x)))

  def imapNeed[K, V](imap: IMap[K, V])(k: K): Eval[V] =
    Eval.some(imap get k, s"$k not found.")

  def imapCreate[K, V](imap: IMap[K, V])(v: V)(implicit trust: Trust): Eval[IMap[K, V]] = {
    val updated = imap + v
    if (trust.is(Trusted) || !imap.containsV(v))
      Eval.pure(updated)
    else
      Eval.fail(s"$v already exists.")
  }

  def toggleLiveCheckBeforeAfter[V](v1: V, newLive: Live)(get: V => Live, set: Live => V => V, name: => String)(implicit trust: Trust): Eval[V] = {
    val v2 = set(newLive)(v1)
    trust match {
      case Trusted =>
        Eval.pure(v2)
      case Untrusted =>
        for {
          _ <- ensureLiveIsNot(newLive)(get(v1), name)
          _ <- ensureLiveIs(newLive)(get(v2), name + " after change")
        } yield v2
    }
  }

  sealed abstract class LiveAccessor[V] {
    val ensureLive: V => Eval[Unit]
    val ensureDead: V => Eval[Unit]
    val makeLive  : V => Eval[V]
    val makeDead  : V => Eval[V]
  }
  def LiveAccessor[V](l: Lens[V, Live])(name: V => String)(implicit trust: Trust): LiveAccessor[V] = {
    val setLive = l set Live
    val setDead = l set Dead
    trust match {
      case Trusted =>
        new LiveAccessor[V] {
          override val ensureLive: V => Eval[Unit] = Eval._unit
          override val ensureDead: V => Eval[Unit] = Eval._unit
          override val makeLive  : V => Eval[V]    = v => Eval.pure(setLive(v))
          override val makeDead  : V => Eval[V]    = v => Eval.pure(setDead(v))
        }
      case Untrusted =>
        new LiveAccessor[V] {
          override val ensureLive: V => Eval[Unit] = v => ApplyEventLib.ensureLive(l get v)(name(v))
          override val ensureDead: V => Eval[Unit] = v => ApplyEventLib.ensureDead(l get v)(name(v))
          override val makeLive  : V => Eval[V]    = v => ensureDead(v).andReturn(setLive(v))
          override val makeDead  : V => Eval[V]    = v => ensureLive(v).andReturn(setDead(v))
        }
    }
  }

  // -------------------------------------------------------------------------------------------------------------------
  @inline def IMapStore[Id, Data](L: Lens[Project, IMap[Id, Data]])(implicit trust: Trust) =
    new IMapStore(L)(trust)

  class IMapStore[Id, Data](L: Lens[Project, IMap[Id, Data]])(implicit trust: Trust) {
    final def ensureAbsent(id: Id): Eval[Unit] =
      whenUntrusted(Eval.tests(p => !L.get(p).containsK(id), s"$id already exists."))

    final def ensurePresent(id: Id): Eval[Unit] =
      whenUntrusted(Eval.tests(p => L.get(p).containsK(id), s"$id not found."))

    final def ensureAbsentData(data: Data): Eval[Unit] =
      whenUntrusted(Eval.tests(p => !L.get(p).containsV(data), s"$data already exists."))

    final def need(id: Id): Eval[Data] =
      Eval.eithers(p => L.get(p).attempt(id))

    final def create(data: Data): Eval[Unit] =
      ensureAbsentData(data) >> addOrUpdate(data)

    final def addOrUpdate(data: Data): Eval[Unit] =
      Eval.mod(L.modify(_ add data))

    final def update(id: Id, updateFn: Data => Eval[Data]): Eval[Unit] =
      need(id).flatMap(updateFn).flatMap(addOrUpdate)

    final def updateF(id: Id, updateFn: Data => Data): Eval[Unit] =
      need(id).map(updateFn).flatMap(addOrUpdate)

    final def hardDelete(id: Id): Eval[Unit] =
      ensurePresent(id) >> Eval.mod(L.modify(_ - id))
  }

  // -------------------------------------------------------------------------------------------------------------------
  @inline def IMapStoreL[Id, Data](L: Lens[Project, IMap[Id, Data]])(liveLens: Lens[Data, Live])(implicit trust: Trust) =
    new IMapStoreL(L, liveLens)(trust)

  final class IMapStoreL[Id, Data](L: Lens[Project, IMap[Id, Data]], liveLens: Lens[Data, Live])(implicit trust: Trust) extends IMapStore(L) {
    def updateLive(id: Id, updateFn: Data => Eval[Data]): Eval[Unit] =
      update(id, d =>
        ensureLive(liveLens get d)(id.toString) >> updateFn(d))

    def setLive(id: Id, newValue: Live): Eval[Unit] =
      update(id, d =>
        ensureLiveIsNot(liveLens get d)(newValue, id.toString).andReturn(liveLens.set(newValue)(d)))
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
    final def need[A](a: ^.Attr)(implicit vs: ^.NonEmptyValues): Eval[a.Data] =
      a.get(vs) match {
        case Some(v) => Eval.pure(v.value)
        case None    => Eval.fail(s"Attribute $a required but missing from [${vs.value.values mkString ", "}].")
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

    final def updateEachValue(updateFn: ^.Value => Data => Eval[Data]): ^.NonEmptyValues => Data => Eval[Data] =
      vs => foldMapBindFns(vs.values)(updateFn)
  }
}
