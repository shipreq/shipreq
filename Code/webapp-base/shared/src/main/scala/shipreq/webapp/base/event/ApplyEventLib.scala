package shipreq.webapp.base.event

import monocle._
import scala.collection.GenTraversable
import scala.reflect.ClassTag
import scalaz.{Equal, -\/, \/, \/-, Traverse}
import scalaz.syntax.equal._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.util.GenericData
import shipreq.webapp.base.validation.{ValidatorU, ValidationResult}

/**
 * Syntax summary:
 * ===============
 *
 * Operator format: <TYPE₁><CONN><TYPE₂>
 *
 * TYPES
 * -----
 * >    App
 * >>   x => App
 * ?    Result
 * @    Lens
 *
 * CONNs
 * -----
 * -  Successful output of lhs discarded
 * =  Successful output of lhs passed to input of rhs
 *
 * App a b          >->   App a c         =  App a c
 * App a c          <-<   App a b         =  App a c
 * App a b          >=>   App b c         =  App a c
 * App b c          <=<   App a b         =  App a c
 * App a b          >=>>  (b => App c d)  =  (a => App c d)
 * App a b          <=@   Lens s t a b    =  App s t
 * App a b          <=?   Result a        =  Result b
 * Result a         ?=>   App a b         =  Result b
 * Result a         ?=>>  (a => App b c)  =  App b c
 * Lens s t a b     @=>   App a b         =  App s t
 * (b => App c d)  <<=<   App a b         =  (a => App c d)
 * (a => App b c)  <<=?   Result a        =  App b c
 * Result a         ?-?   Result b        =  Result b
 * Result a         ?->   App b c         =  App b c
 */
private[event] object ApplyEventLib {

  // ===================================================================================================================
  // Result

  type Result[+A] = String \/ A
  @inline def ok[A](a: A): Result[A] = \/-(a)
  @inline def fail(f: String): Result[Nothing] = -\/(f)

  def test[A](a: A, f: Validity)(err: => String): Result[A] =
    if (f :: Valid) ok(a) else fail(err)

  implicit class ResultOps[A](private val r: Result[A]) extends AnyVal {

    def ?=>>[B, C](f: => (A => App[B, C])): App[B, C] =
      r match {
        case \/-(a)    => f(a)
        case e@ -\/(_) => App(_ => e)
      }

    def ?=>[B](f: => App[A, B]): Result[B] =
      r match {
        case \/-(a)    => f run a
        case e@ -\/(_) => e
      }

    def ?-?[B](f: => Result[B]): Result[B] =
      r.flatMap(_ => f)

    def ?->[B, C](f: => App[B, C]): App[B, C] =
      r match {
        case \/-(_)    => f
        case e@ -\/(_) => App(_ => e)
      }

    @inline def join[X, Y](implicit ev: A =:= App[X, Y]): App[X, Y] =
      r ?=>> ev

    @inline def joinE[X](implicit ev: A =:= App[X, X]): AE[X] =
      r ?=>> ev
  }

  // ===================================================================================================================
  // App

  /** Custom Kleisli arrow */
  case class App[-A, +B](run: A => Result[B]) extends AnyVal {

    @inline def apply(a: A) = run(a)

    @inline def >->[AA <: A, C](g: App[AA, C]): App[AA, C] =
      App(a => run(a) ?-? g(a))

    @inline def <-<[AA <: A, C](g: App[AA, C]): App[AA, B] =
      g >-> this

    @inline def >=>[C](g: App[B, C]): App[A, C] =
      App(run(_) flatMap g.run)

    @inline def <=<[C](g: App[C, A]): App[C, B] =
      g >=> this

    def >=>>[C, D](f: B => App[C, D]): A => App[C, D] =
      run(_) ?=>> f

    @inline def <=?(g: Result[A]): Result[B] =
      g flatMap run

    @inline def <=@[S, T, AA <: A, BB >: B](l: PLens[S, T, AA, BB]): App[S, T] =
      App(l.modifyF[Result](run))

    def map[C](f: B => C): App[A, C] =
      App(run(_) map f)

    @inline def cmap[Z](f: Z => A): App[Z, B] =
      App(run compose f)

    def traverse[F[_], AA <: A, BB >: B](fa: F[AA])(implicit F: Traverse[F]): Result[F[BB]] =
      F.traverse(fa)(run)

    def traverseSet[AA <: A, BB >: B](fa: Set[AA])(implicit ev: UnivEq[BB]): Result[Set[BB]] =
      fa.foldLeft(ok(Set.empty[BB]))((q, a) => for {bs <- q; b <- run(a)} yield bs + b)

    def attempt: App[A, B] =
      App(a =>
        try run(a)
        catch {
          case e: Throwable =>
            val msg = Option(e.getMessage).filter(_.nonEmpty)
            fail(msg getOrElse s"Error occurred: $e")
        })
  }

//  implicit class InvariantAppOps[A, B](private val app: App[A, B]) extends AnyVal {
//    def traverse[F[_]](fa: F[A])(implicit F: Traverse[F]): Result[F[B]] =
//      F.traverse(fa)(app.run)
//
//    def traverseSet(fa: Set[A])(implicit ev: UnivEq[B]): Result[Set[B]] =
//      fa.foldLeft(ok(Set.empty[B]))((q, a) => for {bs <- q; b <- app.run(a)} yield bs + b)
//  }

  object App {
    import ApplyEventLib.{ok => OK}

    @inline def ok[A, B](f: A => B): App[A, B] =
      App(a => OK(f(a)))

    def test[A](t: A => Validity, err: A => String): AE[A] =
      App(a => if (t(a) :: Valid) OK(a) else fail(err(a)))

    @deprecated("Use nop instead of App.id.", "")
    def id[A]: AE[A] = nop

//    def feed[A, B](f: A => App[A, B]): App[A, B] =
//      App(a => f(a) run a)
  }

  /** App Endo */
  type AE[A] = App[A, A]

  /** App Project */
  type AP = AE[Project]

  // ===================================================================================================================
  // Symmetrical ops

  implicit class PLensOps[S, T, A, B](private val l: PLens[S, T, A, B]) extends AnyVal {
    @inline def @=>(a: App[A, B]): App[S, T] = a <=@ l

//    def @=>>[X](f: X => App[A, B]): X => App[S, T] =
//      f(_) <=@ l
  }

  implicit class FnAppOps[A, B, C](private val f: A => App[B, C]) extends AnyVal {
    @inline def <<=<[Z](a: App[Z, A]): Z => App[B, C] = a >=>> f
    @inline def <<=?(r: Result[A]): App[B, C] = r ?=>> f
  }

  // ===================================================================================================================
  // Lib

  val okUnit   = ok(())
  val nopUnit  = App((_: Any) => okUnit)
  val _nopUnit = (_: Any) => nopUnit

  private[this] val nopInstance = App[Any, Any](ok)
  private[this] val _nopInstance = (_: Any) => nopInstance

  def nop[A] = nopInstance.asInstanceOf[AE[A]]
  def _nop[A, B] = _nopInstance.asInstanceOf[A => AE[B]]

  class TrustMask[A](val trusted: A) extends AnyVal
  trait TrustMaskLowPri {
    implicit def trust_App_A_Any[A]      = new TrustMask[App[A, Any]](nopUnit)
    implicit def trust_A_App_B_Any[A, B] = new TrustMask[A => App[B, Any]](_nopUnit)
  }
  object TrustMask extends TrustMaskLowPri {
    implicit def trust_AE[A]      = new TrustMask[AE[A]](nop)
    implicit def trust_A_AE[A, B] = new TrustMask[A => AE[B]](_nop)
  }

  implicit def resultFromValidation[A](r: ValidationResult[A]): Result[A] =
    r match {
      case scalaz.Success(s) => \/-(s)
      case scalaz.Failure(f) => -\/(f.toText)
    }

  implicit class OptionAppOps[A](private val o: Option[A]) extends AnyVal {
    @inline def ensureSome(err: => String): Result[A] =
      o.fold[Result[A]](fail(err))(ok)
  }

  /**
   * If there is an implicit value, allows [[App]]s to be used in for-comprehensions nicely.
   */
  @inline implicit def autoRun[A, B](f: App[A, B])(implicit a: A): Result[B] =
    f run a

  def apFoldLeft[A, B](as: GenTraversable[A])(f: A => AE[B]): AE[B] =
    App(b => as.foldLeft(ok(b))(_ ?=> f(_)))
    // ↓ Should be a tiny bit faster - save for benchmarks
    // vs => App { start =>
    //   val i = vs.value.values.iterator
    //   var q = f(i.next()) run start
    //   while (i.hasNext && q.isRight)
    //     q = f(i.next()) =<< q
    //   q
    // }

  // TODO Modify to reject NOP changes?
  @inline def updateL[A, B](l: Lens[A, B]): B => AE[A] =
    updateC(l.set)

  def updateC[A, B](f: B => A => A): B => AE[A] =
    b => App.ok(f(b))

  def updateF[A, B](f: (A, B) => A): B => AE[A] =
    b => App.ok(f(_, b))

  @inline def whenUntrusted[A](a: => A)(implicit trust: Trust, mask: TrustMask[A]): A =
    if (trust :: Trusted) mask.trusted else a

  def untrustedTest(mustPass: => Boolean, err: => String)(implicit trust: Trust): Result[Unit] =
    if ((trust :: Trusted) || mustPass) okUnit else fail(err)

  def ensureLiveFnP[I, D](f: (Project, I) => Option[D])(getLive: (D, Project) => Live)(implicit trust: Trust): I => App[Project, Any] =
    whenUntrusted(id =>
      App(p => f(p, id) match {
        case Some(d) if getLive(d, p) :: Live => okUnit
        case Some(_)                          => fail(s"$id is dead.")
        case None                             => fail(s"$id not found.")
      })
    )

  def ensureLiveFn[I, D](f: (Project, I) => Option[D])(getLive: D => Live)(implicit trust: Trust): I => App[Project, Any] =
    ensureLiveFnP(f)((d, _) => getLive(d))

  def ensureEqual[A](expect: A)(implicit e: Equal[A], trust: Trust): AE[A] =
    if (trust :: Trusted) nop else
      App(a => if (e.equal(a, expect)) ok(a) else fail(s"Expected $expect, got $a"))

  def ensureSome[A](err: =>  String)(implicit trust: Trust): App[Option[A], A] =
    if (trust :: Trusted)
      App.ok(_.get)
    else
      App(_.fold[Result[A]](fail(err))(ok))

  def ensureNone[A](err: A => String)(implicit trust: Trust): AE[Option[A]] =
    if (trust :: Trusted)
      nop
    else
      App(_.fold[Result[Option[A]]](ok(None))(a => fail(err(a))))

  def validateWith[A](v: ValidatorU[A, _, A])(implicit trust: Trust): AE[A] =
    whenUntrusted(App(v correctAndValidateU _))

  def validateWithF[I, A](v: ValidatorU[I, _, A])(i: A => I)(implicit trust: Trust): AE[A] =
    whenUntrusted(App(o => v correctAndValidateU i(o)))

  // TODO rename
  def withUntrustedCheck[B, C](check: (B, C) => Option[String])(ap: (B, C) => B)(implicit trust: Trust): C => AE[B] =
    if (trust :: Trusted)
      c => App.ok(b => ap(b, c))
    else
      c => App(b => check(b, c).fold(ok(ap(b, c)))(fail))

  def ensureLiveBy[V](live: V => Live)(implicit trust: Trust): AE[V] =
    whenUntrusted(App(v => if (live(v) :: Live) ok(v) else fail(s"Datum is dead: $v")))

  def ensureDeadBy[V](live: V => Live)(implicit trust: Trust): AE[V] =
    whenUntrusted(App(v => if (live(v) :: Dead) ok(v) else fail(s"Datum is live: $v")))

  case class LiveApp[V](l: Lens[V, Live])(implicit trust: Trust) {
    private[this] val setLive = l set Live
    private[this] val setDead = l set Dead
    val ensureLive: AE[V] = ensureLiveBy(l.get)
    val ensureDead: AE[V] = ensureDeadBy(l.get)
    val makeLive  : AE[V] = ensureDead map setLive
    val makeDead  : AE[V] = ensureLive map setDead
  }

  def narrowCC[A, B <: A](implicit cc: ClassTag[B], trust: Trust): App[A, B] =
    if (trust :: Untrusted)
      App(a => cc.unapply(a) match {
        case Some(b) => ok(b)
        case None    => fail(s"Expected a ${cc.runtimeClass.getName}, got: $a.")
      })
    else
      App.ok(a => a.asInstanceOf[B])

  def appendNewToVector[A: Equal](implicit trust: Trust): A => AE[Vector[A]] = {
    def doit(a: A, as: Vector[A]): Vector[A] = as :+ a
    if (trust :: Trusted)
      a => App.ok(doit(a, _))
    else
      a => App { as =>
        if (as.exists(_ ≟ a))
          fail(s"Element $a already exists in $as.")
        else
          ok(doit(a, as))
      }
  }

  def removeFromVector[A: Equal](implicit trust: Trust): A => AE[Vector[A]] = {
    def doit(a: A, as: Vector[A]): Vector[A] = as.filterNot(_ ≟ a)
    if (trust :: Trusted)
      a => App.ok(doit(a, _))
    else
      a => App { i =>
        val o = doit(a, i)
        if (o.length == i.length)
          fail(s"Element not found: Expected to find $a in $i.")
        else
          ok(o)
      }
  }

  def reposition[A: Equal](implicit trust: Trust): (A, Option[A]) => AE[Vector[A]] = {
    if (trust :: Trusted)
      (a, pos) => App.ok(Position.set(_, a, pos))
    else
      (a, pos) => App { as =>
        if (!as.exists(_ ≟ a))
          fail(s"Element not found: Expected to find $a in $as.")
        else
          ok(Position.set(as, a, pos))
      }
  }

  def updateIdCeilingFn(lens: Lens[IdCeilings, Int]): Int => AP = {
    val l = Project.idCeilings ^|-> lens
    n => App.ok { p =>
      val i = l.get(p)
      if (n > i)
        l.set(n)(p)
      else
        p
    }
  }

  trait AskTrust {
    protected implicit def trust: Trust
  }

  // -------------------------------------------------------------------------------------------------------------------
  object IMapApp {
    @inline def apply[K, V](implicit trust: Trust) = new IMapApp[K, V]
    @inline def like[K, V](m: => IMap[K, V])(implicit trust: Trust) = apply[K, V]
    @inline def data[O, D, Id](o: O)(implicit O: ObjDataId[O, D, Id], trust: Trust) = apply[Id, D]
  }

  final class IMapApp[K, V](private implicit val trust: Trust) {
    type M = IMap[K, V]

    /**
     * Get a value which must exist.
     *
     * @return Failure unless value exists.
     */
    def need(k: K): App[M, V] =
      App(_.get(k) ensureSome s"$k not found.")

    /** @return Failure if untrusted and value already exists. */
    def add: V => AE[M] =
      withUntrustedCheck[M, V](
        (m, v) => if (m containsV v) Some(s"$v already exists.") else None)(
        _ + _)

    /** @return Failure unless value exists. */
    def update(k: K, f: AE[V]): AE[M] = {
      val g = need(k) >=> f
      App(m => g(m).map(m + _))
    }

    /** @return Failure if untrusted and key not found. */
    val remove: K => AE[M] =
      if (trust :: Trusted)
        k => App.ok(_ - k)
      else
        k => App(m => if (m containsK k) ok(m - k) else fail(s"$k not found."))

    def needM[R](k: K)(f: M => App[V, R]): App[M, R] =
      App(m => need(k)(m) ?=> f(m))
  }

  // -------------------------------------------------------------------------------------------------------------------
//  object MultimapApp {
//    @inline def apply[K, L[_]: MultiValues, V](implicit trust: Trust) = new MultimapApp[K, L, V]
//    @inline def like[K, L[_]: MultiValues, V](m: => Multimap[K, L, V])(implicit trust: Trust) = apply[K, L, V]
//  }
//
//  final class MultimapApp[K, L[_], V](private implicit val trust: Trust, L: MultiValues[L]) {
//    type M = Multimap[K, L, V]
//
//    def update(k: K, f: AE[L[V]]): AE[M] =
//      App(m => f(m(k)).map(m.setvs(k, _)))
//  }

  // -------------------------------------------------------------------------------------------------------------------
  trait GenericDataApp {

    val ^ : GenericData
    type Data

    final type AD = AE[Data]

    /**
     * Get a value which must exist.
     *
     * @return Failure unless value exists.
     */
    final def need[A](attr: ^.Attr): App[^.NonEmptyValues, attr.Data] =
      App(vs =>
        attr.get(vs) match {
          case Some(v) => ok(v.value)
          case None    => fail(s"Attribute $attr required but missing from [${vs.value.values mkString ", "}].")
        }
      )

    /**
     * Get a value if it exists. Use a default otherwise.
     */
    final def want[A](a: ^.Attr)(default: a.Data): App[^.NonEmptyValues, a.Data] =
      App.ok(vs =>
        a.get(vs) match {
          case Some(v) => v.value
          case None    => default
        }
      )

    /**
     * Get a value if it exists.
     */
    final def read[A](a: ^.Attr): App[^.NonEmptyValues, Option[a.Data]] =
      App.ok(a.get(_).map(_.value))

    final def updateEachValue(updateFn: ^.Value => AD): ^.NonEmptyValues => AD =
      vs => apFoldLeft(vs.values)(updateFn)
  }

  // -------------------------------------------------------------------------------------------------------------------
  /**
   * Logic for CRUD events that simply update an ID-keyed IMap.
   */
  trait IMapStore extends AskTrust {
    this: GenericDataApp =>

    type Id
    val L: Lens[Project, IMap[Id, Data]]
    def liveLens: Lens[Data, Live]

    final val imap       = IMapApp[Id, Data]
    final val ensureLive = ensureLiveBy(liveLens.get)
    final val updateLive = updateL(liveLens)

    final def create(newObject: Result[Data]): AP =
      L @=> (newObject ?=>> imap.add)

    final def update(id: Id, updateValues: AD): AP =
      L @=> imap.update(id, ensureLive >=> updateValues)

    final def delete(id: Id, da: DeletionAction): AP =
      da match {
        case Delete  => setLive(id, Dead)
        case Restore => setLive(id, Live)
      }

    private def setLive(id: Id, newValue: Live): AP =
      L @=> imap.update(id, updateLive(newValue))
  }
}
