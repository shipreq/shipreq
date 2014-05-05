package shipreq.base.util

import scalaz.{\/-, -\/}
import scalaz.std.option.optionInstance
import ErrorOr.Implicits.MonadExt

/**
 * Reads values from some kind of external source.
 *
 * Interface to the source is provided by implicit Retriever[T] type-classes.
 *
 * External source value keys can be transformed implicitly using PropScope.
 */
object ExternalValueReader {

  case class Retriever[T](run: String => Option[ErrorOr[T]]) {
    def ?>>?(that: Retriever[T]): Retriever[T] =
      Retriever(k => this.run(k) orElse that.run(k))

    def map[B](f: T => B): Retriever[B] =
      Retriever(k => this.run(k).map(_ map f))

    def emap[B](f: T => ErrorOr[B]): Retriever[B] =
      fmap(t => Some(f(t)))

    def fmap[B](f: T => Option[ErrorOr[B]]): Retriever[B] =
      Retriever(k => this.run(k).flatMap {
        case -\/(e) => Some(-\/(e))
        case \/-(t) => f(t)
      })
  }

  case class PropScope(run: String => String)

  val GlobalScope = PropScope(identity)

  type ValTest[T] = T => Option[Error]

  def scopeByPrefix(prefix: String) =
    PropScope(prefix + _)

  def scopeByNS(ns1: String, ns: String*) =
    scopeByPrefix((ns1 +: ns).map(_.replaceFirst("\\.+$", "")).filter(_.nonEmpty).mkString("", ".", "."))

  def getOE[T](name: String)(implicit s: PropScope, r: Retriever[T]): Option[ErrorOr[T]] =
    ErrorOr.catchAndAnnotateM(errorMsgWhenThrows(name))(
      r.run(s.run(name)))

  def getO[T](name: String)(implicit s: PropScope, r: Retriever[T]): Option[T] =
    getOE(name) map ErrorOr.require_!

  def get[T](name: String)(implicit s: PropScope, r: Retriever[T]): ErrorOr[T] =
    getOE(name) getOrElse ErrorOr.error(errorMsgWhenMissing(name))

  def get[T](name: String, default: => T)(implicit s: PropScope, r: Retriever[T]): ErrorOr[T] =
    getOE(name) getOrElse ErrorOr(default)

  def tryGet[T](name: String, moreNames: String*)(implicit s: PropScope, r: Retriever[T]): ErrorOr[T] = {
    val es = (name #:: moreNames.toStream).map(getOE(_))
    es.filter(_.isDefined).headOption.map(_.get) getOrElse get(name)
  }

  def need[T](name: String)(implicit s: PropScope, r: Retriever[T]): T =
    ErrorOr require_! get(name)

  def tryNeed[T](name: String, default: => T)(implicit s: PropScope, r: Retriever[T]): T =
    getO(name) getOrElse default

  def tryUse[T](name: String)(f: T => Unit)(implicit s: PropScope, r: Retriever[T]): Unit =
    get(name) foreach f

  def valTest[T](f: T => Boolean, errMsg: String): ValTest[T] =
    t => if (f(t)) None else Some(Error(errMsg))

  def valTestNotError[T]: ValTest[ErrorOr[T]] =
    _.swap.toOption

  def validate[T](name: String, f: String => T)(test: ValTest[T])(implicit s: PropScope): T =
    _test[T, T](name, f, test)(Some(_), _.throw_!())

  def validateO[T](name: String, f: String => Option[T])(test: ValTest[T])(implicit s: PropScope): Option[T] =
    _test[T, Option[T]](name, f, test)(identity, _.throw_!())

  def test[T](name: String, f: String => ErrorOr[T])(test: ValTest[T])(implicit s: PropScope): ErrorOr[T] =
    _test[T, ErrorOr[T]](name, f, test)(_.toOption, _.toErrorOr)

  def testO[T](name: String, f: String => Option[ErrorOr[T]])(test: ValTest[T])(implicit s: PropScope): Option[ErrorOr[T]] =
    _test[T, Option[ErrorOr[T]]](name, f, test)(_.flatMap(_.toOption), e => Some(e.toErrorOr))

  private def _test[T, R](name: String, f: String => R, test: ValTest[T])
                         (getTestable: R => Option[T], onTestFail: Error => R)
                         (implicit s: PropScope): R = {
    val r = f(name)
    getTestable(r) match {
      case None => r
      case Some(t) =>
        testVal(name, test)(s)(t) match {
          case -\/(err) => onTestFail(err)
          case \/-(_) => r
        }
    }
  }

  private def testVal[T](name: String, test: ValTest[T])(implicit s: PropScope): T => ErrorOr[T] =
    v => test(v) match {
      case Some(err) => err.annotate(errorMsgValueTestFails(name, v)).toErrorOr
      case None      => ErrorOr(v)
    }

  private def errorMsgValueTestFails(name: String, v: Any)(implicit s: PropScope): String =
    s"[${s run name}] Illegal value of '$v'."

  private def errorMsgWhenThrows(name: String)(implicit s: PropScope): String =
    s"[${s run name}] Error occurred retrieving value."

  private def errorMsgWhenMissing(name: String)(implicit s: PropScope): String =
    s"[${s run name}] Value not specified."
}

// =====================================================================================================================

object StringParsingBase {
  import java.util.regex.Pattern

  val RegexT = Pattern.compile("^(?:t(?:rue)?|y(?:es)?|1|on|enabled?)$", Pattern.CASE_INSENSITIVE)
  val RegexF = Pattern.compile("^(?:f(?:alse)?|n(?:o)?|0|off|disabled?)$", Pattern.CASE_INSENSITIVE)
  val RemoveComments = Pattern.compile("\\s*#.*$")

  def removeComment(s: String): String =
    RemoveComments.matcher(s).replaceFirst("")
}

import ExternalValueReader.Retriever
import StringParsingBase._

abstract class StringParsingBase(_retrieverS: Retriever[String]) {

  protected val normalisedRS = _retrieverS.fmap(s => {
    val v = removeComment(s).trim
    if (v.isEmpty) None else Some(ErrorOr(v))
  })

  final def tryParse[T](f: String => T): Retriever[T] =
    tryParseE(k => ErrorOr(f(k)))

  final def tryParseE[T](f: String => ErrorOr[T]): Retriever[T] =
    tryParseOE(k => Some(f(k)))

  final def tryParseOE[T](f: String => Option[ErrorOr[T]]): Retriever[T] =
    Retriever(k =>
      ErrorOr.catchExceptionM(
        normalisedRS.run(k) fmapE f))
}

/**
 * Reads a bunch of differently-typed values by parsing strings.
 */
class StringBasedValueReader(_retrieverS: Retriever[String]) extends StringParsingBase(_retrieverS) {

  implicit final def retrieverS = normalisedRS

  implicit val retrieverI: Retriever[Int] =
    tryParse(Integer.parseInt)

  implicit val retrieverL: Retriever[Long] =
    tryParse(java.lang.Long.parseLong)

  implicit val retrieverB: Retriever[Boolean] = tryParseE(s =>
    if (RegexT.matcher(s).matches)
      ErrorOr(true)
    else if (RegexF.matcher(s).matches)
      ErrorOr(false)
    else
      ErrorOr error s"Unable to parse '$s'"
  )
}

// =====================================================================================================================

/**
 * Provides a bunch of implicit ExternalValueReader.Retrievers that sources read from a given Properties instance.
 */
object JPropertiesValueReader {
  import java.util.Properties

  def apply(p: Properties) = new StringBasedValueReader(
    Retriever[String](k => Option(p.getProperty(k)).map(ErrorOr(_)))
  )
}
