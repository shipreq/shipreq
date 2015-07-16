package shipreq.webapp.base.event

import monocle._
import scala.collection.GenTraversable
import scala.reflect.ClassTag
import scalaz.{Equal, -\/, \/, \/-}
import scalaz.syntax.equal._
import shipreq.base.util._
import shipreq.webapp.base.data.{Dead, Live, ObjDataId, Project}
import shipreq.webapp.base.util.GenericData
import shipreq.webapp.base.validation.{ValidatorU, ValidationResult}
import DeletionAction._

/**
 * Syntax summary:
 * ===============
 *
 * Operator format: <SYM1>=<SYM2>
 *
 * SYM  TYPE
 * ---  --------
 * >    App
 * >>   x => App
 * ?    Result
 * @    Lens
 *
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
  }

  // ===================================================================================================================
  // App

  /** Custom Kleisli arrow */
  case class App[-A, +B](run: A => Result[B]) extends AnyVal {

    @inline def apply(a: A) = run(a)

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
  }

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

  private[this] val _nop = App[Any, Any](ok)

  def nop[A] = _nop.asInstanceOf[AE[A]]

  implicit def resultFromValidation[A](r: ValidationResult[A]): Result[A] =
    r match {
      case scalaz.Success(s) => \/-(s)
      case scalaz.Failure(f) => -\/(f.toText)
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

  @inline def whenUntrusted[A](a: => AE[A])(implicit trust: Trust): AE[A] =
    if (trust :: Trusted) nop else a

  def validateWith[A](v: ValidatorU[A, _, A])(implicit trust: Trust): AE[A] =
    whenUntrusted(App(v correctAndValidateU _))

  def validateWithF[I, A](v: ValidatorU[I, _, A])(i: A => I)(implicit trust: Trust): AE[A] =
    whenUntrusted(App(o => v correctAndValidateU i(o)))

  def withUntrustedCheck[B, C](check: (B, C) => Option[String])(ap: (B, C) => B)(implicit trust: Trust): C => AE[B] =
    if (trust :: Trusted)
      c => App.ok(b => ap(b, c))
    else
      c => App(b => check(b, c).fold(ok(ap(b, c)))(fail))

  def ensureLiveBy[V](live: V => Live)(implicit trust: Trust): AE[V] =
    whenUntrusted(App(v => if (live(v) :: Live) ok(v) else fail(s"Subject is dead: $v")))

  def narrowCC[A, B <: A](implicit cc: ClassTag[B], trust: Trust): App[A, B] =
    if (trust :: Untrusted)
      App(a => cc.unapply(a) match {
        case Some(b) => ok(b)
        case None    => fail(s"Expected a ${cc.runtimeClass.getName}, got: $a.")
      })
    else
      App.ok(a => a.asInstanceOf[B])

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
      App(_.get(k) match {
        case Some(v) => ok(v)
        case None    => fail(s"$k not found.")
      })

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
        attr(vs) match {
          case Some(v) => ok(v.value)
          case None    => fail(s"Attribute $attr required but missing from [${vs.value.values mkString ", "}].")
        }
      )

    /**
     * Get a value if it exists. Use a default otherwise.
     */
    final def want[A](a: ^.Attr)(default: a.Data): App[^.NonEmptyValues, a.Data] =
      App.ok(vs =>
        a(vs) match {
          case Some(v) => v.value
          case None    => default
        }
      )

    /**
     * Get a value if it exists.
     */
    final def read[A](a: ^.Attr): App[^.NonEmptyValues, Option[a.Data]] =
      App.ok(a(_).map(_.value))

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
        case Restore => setLive(id, Live)
        case SoftDel => setLive(id, Dead)
        case HardDel => L @=> imap.remove(id)
      }

    private def setLive(id: Id, newValue: Live): AP =
      L @=> imap.update(id, updateLive(newValue))
  }
}
