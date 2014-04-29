package shipreq.base.util

import scalaz.Scalaz.Id
import scalaz.{-\/, \/, \/-, \&/, Applicative, Catchable, Monad, Lens}
import scalaz.\&/.{Both, That, This}

object ErrorOr {
  def apply[A](a: A): ErrorOr[A] = \/-(a)

  val unit = apply(())

  @inline final def error[A](m: String)              : ErrorOr[A] = -\/(Error(m))
  @inline final def error[A](e: Throwable)           : ErrorOr[A] = -\/(Error(e))
  @inline final def error[A](m: String, e: Throwable): ErrorOr[A] = -\/(Error(m,e))

  def fromOption[A](o: Option[A], errMsg: => String): ErrorOr[A] =
    o match {
      case Some(a) => apply(a)
      case None    => error(errMsg)
    }

  def catchException[A](a: => ErrorOr[A]): ErrorOr[A] =
    catchExceptionM[Id, A](a)

  def catchExceptionM[M[_], A](a: => M[ErrorOr[A]])(implicit M: Applicative[M]): M[ErrorOr[A]] =
    try a catch {
      case ErrorAsThrowable(e) => M.point(e.toErrorOr)
      case e: Throwable        => M.point(error(e))
    }

  def annotate[A](ann: => String)(eoa: ErrorOr[A]): ErrorOr[A] =
    eoa.leftMap(_ annotate ann)

  def annotateM[M[_], A](ann: => String)(a: M[ErrorOr[A]])(implicit M: Applicative[M]): M[ErrorOr[A]] =
    M.map(a)(annotate(ann))

  def catchAndAnnotate[A](ann: => String)(a: => ErrorOr[A]): ErrorOr[A] =
    annotate(ann)(catchException(a))

  def catchAndAnnotateM[M[_], A](ann: => String)(a: => M[ErrorOr[A]])(implicit M: Applicative[M]): M[ErrorOr[A]] =
    annotateM(ann)(catchExceptionM(a))

  def tag[A](t: => ErrorTag)(eoa: ErrorOr[A]): ErrorOr[A] =
    eoa.leftMap(_ tag t)

  def tagM[M[_], A](t: => ErrorTag)(m: M[ErrorOr[A]])(implicit M: Applicative[M]): M[ErrorOr[A]] =
    M.map(m)(_.leftMap(_ tag t))

  def catchAndTag[A](t: => ErrorTag)(a: => ErrorOr[A]): ErrorOr[A] =
    tag(t)(catchException(a))

  def catchAndTagM[M[_], A](t: => ErrorTag)(a: => M[ErrorOr[A]])(implicit M: Applicative[M]): M[ErrorOr[A]] =
    tagM(t)(catchExceptionM(a))

  def safe[A](a: => A): ErrorOr[A] =
    catchException(ErrorOr(a))

  def safeA[A](ann: => String)(a: => A): ErrorOr[A] =
    catchAndAnnotate(ann)(ErrorOr(a))

  def safeT[A](t: => ErrorTag)(a: => A): ErrorOr[A] =
    catchAndTag(t)(ErrorOr(a))

  def require_![A](eoa: ErrorOr[A]): A =
    eoa match {
      case \/-(v) => v
      case -\/(e) => e.throw_!()
    }

  def withResource[R, O](r: => R)(close: R => Unit)(f: R => O): ErrorOr[O] =
    withResourceE(r)(close)(apply[O]_  compose f)

  def withResourceE[R, O](r: => R)(close: R => Unit)(f: R => ErrorOr[O]): ErrorOr[O] =
    catchException {
      val rr = r
      execFinally(catchException(f(rr)), close(rr))
    }

  /**
   * Runs a effect and if it throws an exception then it is returned gracefully.
   * If the `tried` result is already an error, then the error from the finally-effect is ignored.
   *
   * Avoiding wrapping `finallyFn` in `safe` for a small performance gain.
   *
   * @param tried The default response (strict).
   * @param finallyFn The effect function.
   */
  def execFinally[A](tried: ErrorOr[A], finallyFn: => Unit): ErrorOr[A] =
    try {
      finallyFn
      tried
    } catch {
      case t: Throwable => Error.choose(tried, Error(t))
    }

  def toError[A](ea: ErrorOr[A])(f: => A => Error): Error =
    ea match {
      case -\/(e) => e
      case \/-(a) => f(a)
    }

  def toErrorN[A](ea: ErrorOr[A])(f: => A => Error): ErrorOr[Nothing] = toError(ea)(f).toErrorOr[Nothing]

  object Implicits {

    implicit def ErrorOrAsIdMonad[A](ea: Id[ErrorOr[A]]) = new MonadExt[Id, A](ea)

    implicit class MonadExt[M[_], A](val mea: M[ErrorOr[A]]) extends AnyVal {

      @inline def mapE[B](f: => A => B)(implicit M: Monad[M]): M[ErrorOr[B]] =
        // fmapE(a => M point ErrorOr(f(a)))
        M.map(mea)(_ map f)

      @inline def emapE[B](f: => A => ErrorOr[B])(implicit M: Monad[M]): M[ErrorOr[B]] =
        // fmapE(a => M point f(a))
        M.map(mea)(_ flatMap f)

      @inline def fmapE[B](f: => A => M[ErrorOr[B]])(implicit M: Monad[M]): M[ErrorOr[B]] =
        M.bind(mea) {
          case    \/-(a) => f(a)
          case e@ -\/(_) => M.point(e)
        }

      @inline def cmapE[B](f: => A => M[B], e: => Error => M[B])(implicit M: Monad[M]): M[B] =
        M.bind(mea) {
          case \/-(a) => f(a)
          case -\/(r) => e(r)
        }

      @inline def >-> [B](f: => A => B)            (implicit M: Monad[M]): M[ErrorOr[B]] = mapE(f)
      @inline def >=> [B](f: => A => ErrorOr[B])   (implicit M: Monad[M]): M[ErrorOr[B]] = emapE(f)
      @inline def >==>[B](f: => A => M[ErrorOr[B]])(implicit M: Monad[M]): M[ErrorOr[B]] = fmapE(f)

      @inline def >->! (f: => A => Nothing)            (implicit M: Monad[M]): M[ErrorOr[Nothing]] = mapE[Nothing](f)
      @inline def >=>! (f: => A => ErrorOr[Nothing])   (implicit M: Monad[M]): M[ErrorOr[Nothing]] = emapE[Nothing](f)
      @inline def >==>!(f: => A => M[ErrorOr[Nothing]])(implicit M: Monad[M]): M[ErrorOr[Nothing]] = fmapE[Nothing](f)

      @inline def _mapE [B](b: => B            )(implicit M: Monad[M]): M[ErrorOr[B]] = mapE(_ => b)
      @inline def _emapE[B](b: => ErrorOr[B]   )(implicit M: Monad[M]): M[ErrorOr[B]] = emapE(_ => b)
      @inline def _fmapE[B](b: => M[ErrorOr[B]])(implicit M: Monad[M]): M[ErrorOr[B]] = fmapE(_ => b)
      @inline def |>->  [B](b: => B            )(implicit M: Monad[M]): M[ErrorOr[B]] = _mapE(b)
      @inline def |>=>  [B](b: => ErrorOr[B]   )(implicit M: Monad[M]): M[ErrorOr[B]] = _emapE(b)
      @inline def |>==> [B](b: => M[ErrorOr[B]])(implicit M: Monad[M]): M[ErrorOr[B]] = _fmapE(b)

      @inline def ftapE(f: A => M[ErrorOr[Unit]])(implicit M: Monad[M]): M[ErrorOr[A]] = fmapE(a => f(a) |>-> a)
      @inline def >==>^(f: A => M[ErrorOr[Unit]])(implicit M: Monad[M]): M[ErrorOr[A]] = ftapE(f)

      @inline def tapE(f: A => M[Unit])(implicit M: Monad[M]): M[ErrorOr[A]] = fmapE(a => M.map(f(a))(_ => ErrorOr(a)))
      @inline def <<| (f: A => M[Unit])(implicit M: Monad[M]): M[ErrorOr[A]] = tapE(f)

      @inline def _tapE(f: M[Unit])(implicit M: Monad[M]): M[ErrorOr[A]] = tapE(_ => f)
      @inline def |<<| (f: M[Unit])(implicit M: Monad[M]): M[ErrorOr[A]] = _tapE(f)

      @inline def toErrorM(f: => A => Error)(implicit M: Monad[M]): M[Error] =
        M.map(mea)(ea => toError(ea)(f))

      @inline def toErrorNM(f: => A => Error)(implicit M: Monad[M]): M[ErrorOr[Nothing]] =
        M.map(mea)(ea => toErrorN(ea)(f))

      @inline def ftoErrorM(f: => A => M[Error])(implicit M: Monad[M]): M[Error] =
        mea.cmapE(f, M.point(_))

      @inline def ftoErrorNM(f: => A => M[Error])(implicit M: Monad[M]): M[ErrorOr[Nothing]] =
        M.map(mea.ftoErrorM(f))(_.toErrorOr[Nothing])

      @inline def maybeFail(f: => A => Option[Error])(implicit M: Monad[M]): M[ErrorOr[Unit]] =
        mea >=> f.andThen(_ map (_.toErrorOr) getOrElse ErrorOr.unit)

      @inline def execE(f: Error => M[Unit])(implicit M: Monad[M]): M[Unit] =
        M.bind(mea){
          case \/-(_) => M.point(())
          case -\/(e) => f(e)
        }

      @inline def joinE[B](implicit M: Monad[M], ev: A =:= ErrorOr[B]): M[ErrorOr[B]] =
        mea emapE ev

      @inline def joinT[B](implicit M: Monad[M], ev: A =:= \/[Throwable, B]): M[ErrorOr[B]] =
        mea.emapE(ev(_) match {
          case    -\/(t) => error(t)
          case r@ \/-(_) => r
        })
    }
  }

  object Scalaz {
    import Implicits._

    def monadInstance[M[_]](implicit M: Monad[M]): Monad[({type λ[α] = M[ErrorOr[α]]})#λ] = {
      type ME[a] = M[ErrorOr[a]]
      new Monad[ME] {
        override def point[A](a: => A): ME[A] = M.point(ErrorOr(a))
        override def bind[A, B](m: ME[A])(f: A => ME[B]): ME[B] = m >==> f
        override def map[A, B](m: ME[A])(f: A => B): ME[B] = m >-> f
      }
    }

    def catchableInstance[F[_]](implicit F: Applicative[F]): Catchable[({type λ[α] = F[ErrorOr[α]]})#λ] = {
      type FE[a] = F[ErrorOr[a]]
      new Catchable[FE] {
        def fail[A](e: Throwable): FE[A] = F.point(ErrorOr error e)
        def attempt[A](f: FE[A]): FE[Throwable \/ A] =
          try F.map(f) {
                case    -\/(e) => ErrorOr(-\/(e.throwable))
                case r@ \/-(_) => ErrorOr(r)
              }
          catch { case t: Throwable => F.point(ErrorOr(-\/(t))) }
      }
    }
  }
}

trait ErrorTag

final case class Error(reason: String \&/ Throwable, tags: Set[ErrorTag] = Set.empty, supp: Option[Any] = None) {
  import Error._

  def annotate(a: String): Error =
    reasonLens.mod({
      case This(m)    => This(merge(a, m))
      case That(e)    => Both(a, e)
      case Both(m, e) => Both(merge(a, m), e)
    }, this)

  def tag(t: ErrorTag) =
    tagsLens.mod(_ + t, this)

  def is(t: ErrorTag): Boolean =
    tags contains t

  def withSupp(s: Any): Error =
    copy(supp = Some(s))

  def withoutSupp: Error =
    copy(supp = None)

  def trySupp[A](f: PartialFunction[Any, A]): Option[A] =
    supp.filter(f.isDefinedAt).map(f.apply)

  def throw_!(): Nothing =
    throw throwable

  def toErrorOr[A]: ErrorOr[A] =
    -\/(this)

  def msg: String = reason match {
    case This(m)    => m
    case That(e)    => e.getMessage
    case Both(m, e) => merge(m, e.getMessage)
  }

  def cause: Option[Throwable] = reason match {
    case This(_)    => None
    case That(e)    => Some(e)
    case Both(_, e) => Some(e)
  }

  def throwable: Throwable = this match {
    case Error(That(e), _, None) if tags.isEmpty => e
    case _                                       => ErrorAsThrowable(this)
  }

  def stackTraceStr: String = Error stackTraceStr throwable
}

object Error {
  val reasonLens = Lens.lensg[Error, String \&/ Throwable](e => r => e.copy(reason = r), _.reason)
  val tagsLens   = Lens.lensg[Error, Set[ErrorTag]       ](e => t => e.copy(tags = t)  , _.tags)
  val suppLens   = Lens.lensg[Error, Option[Any]         ](e => s => e.copy(supp = s)  , _.supp)

  @inline final def apply[A](m: String)              : Error = Error(This(m))
  @inline final def apply[A](e: Throwable)           : Error = Error(That(e))
  @inline final def apply[A](m: String, e: Throwable): Error = Error(Both(m, e))

  private def merge(a: String, b: String): String =
    if (a.isEmpty) b
    else if (b.isEmpty) a
    else {
      val l: Int = a.last
      val p = if (Character.isLetterOrDigit(l)) ". "
         else if (Character.isSpaceChar(l)) ""
         else " "
      a + p + b
    }

  def stackTraceStr(t: Throwable): String = {
    val sw = new java.io.StringWriter
    val pw = new java.io.PrintWriter(sw)
    t.printStackTrace(pw)
    sw.toString
  }

  def choose[N](a: ErrorOr[_], b: => Error): ErrorOr[N] =
    a match {
      case e@ -\/(_) => e
      case \/-(_)    => b.toErrorOr
    }
}

final case class ErrorAsThrowable(e: Error) extends RuntimeException(e.msg, e.cause getOrElse null)
