package shipreq.base.util

import scalaz.{\/-, -\/}
import scalaz.std.option.optionInstance

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
      Retriever(s => this.run(s) orElse that.run(s))
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
    getOE(name) getOrElse Error(errorMsgWhenMissing(name))

  def tryGet[T](name: String, moreNames: String*)(implicit s: PropScope, r: Retriever[T]): ErrorOr[T] = {
    val es = (name #:: moreNames.toStream).map(get(_))
    es.filter(_.isRight).headOption.getOrElse(es.head)
  }

  def need[T](name: String)(implicit s: PropScope, r: Retriever[T]): T =
    ErrorOr require_! get(name)

  def tryNeed[T](name: String, default: => T)(implicit s: PropScope, r: Retriever[T]): T =
    getO(name) getOrElse default

  def tryUse[T](name: String)(f: T => Unit)(implicit s: PropScope, r: Retriever[T]): Unit =
    get(name) foreach f

  def valTest[T](f: T => Boolean, errMsg: String): ValTest[T] =
    t => if (f(t)) None else Some(Error error errMsg)

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

object StringBasedValueReader {
  import java.util.regex.Pattern

  val RegexT = Pattern.compile("^(?:t(?:rue)|y(?:es)|1|on|enabled?)$", Pattern.CASE_INSENSITIVE)
  val RegexF = Pattern.compile("^(?:f(?:alse)|n(?:o)|0|off|disabled?)$", Pattern.CASE_INSENSITIVE)
  val RemoveComments = Pattern.compile("\\s*#.*$")

  def removeComment(s: String): String =
    RemoveComments.matcher(s).replaceFirst("")
}

import ExternalValueReader.Retriever

/**
 * Reads a bunch of differently-typed values by parsing strings.
 */
class StringBasedValueReader(_retrieverS: Retriever[String]) {
  import StringBasedValueReader._

  implicit final def retrieverS = _retrieverS

  def tryParse[T](f: String => T): Retriever[T] =
    tryParseE(s => ErrorOr(f(s)))

  def tryParseE[T](f: String => ErrorOr[T]): Retriever[T] =
    tryParseOE(s => Some(f(s)))

  def tryParseOE[T](f: String => Option[ErrorOr[T]]): Retriever[T] =
    Retriever(k =>
      retrieverS.run(k) match {
        case Some(\/-(s)) => f(s)
        case Some(-\/(e)) => Some(-\/(e))
        case None         => None
      })

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
      Error(s"Unable to parse '$s'")
  )
}

// =====================================================================================================================

/**
 * Provides a bunch of implicit ExternalValueReader.Retrievers that sources read from a given Properties instance.
 */
object JPropertiesValueReader {
  import java.util.Properties
  import StringBasedValueReader._

  def apply(p: Properties) = new StringBasedValueReader(
    Retriever[String](k =>
      Option(p.getProperty(k)) match {
        case None     => None
        case Some(vv) =>
          val v = removeComment(vv).trim
          if (v.isEmpty) None else Some(ErrorOr(v))
      }))
}
