package shipreq.base.util

import scalaz.{\/-, -\/}

/**
 * Reads values from some kind of external source.
 *
 * Interface to the source is provided by implicit Retriever[T] type-classes.
 *
 * External source value keys can be transformed implicitly using PropScope.
 */
object ExternalValueReader {

  case class Retriever[T](run: String => Option[ErrorOr[T]])

  case class PropScope(run: String => String)

  val GlobalScope = PropScope(identity)

  def scopeByPrefix(prefix: String) =
    PropScope(prefix + _)

  def scopeByNS(ns1: String, ns: String*) =
    scopeByPrefix((ns1 +: ns).map(_.replaceFirst("\\.+$", "")).filter(_.nonEmpty).mkString("", ".", "."))

  def getOE[T](name: String)(implicit s: PropScope, r: Retriever[T]): Option[ErrorOr[T]] =
    r.run(s.run(name))

  def get[T](name: String)(implicit s: PropScope, r: Retriever[T]): ErrorOr[T] =
    getOE(name) getOrElse defaultError(name)

  def getO[T](name: String)(implicit s: PropScope, r: Retriever[T]): Option[T] =
    getOE(name).flatMap(_.toOption)

  def tryGet[T](name: String, moreNames: String*)(implicit s: PropScope, r: Retriever[T]): ErrorOr[T] = {
    val es = (name #:: moreNames.toStream).map(get(_))
    es.filter(_.isRight).headOption.getOrElse(es.head)
  }

  def need[T](name: String)(implicit s: PropScope, r: Retriever[T]): T =
    get(name) match {
      case \/-(t) => t
      case -\/(e) => throw Error.throwable(e)
    }

  def tryNeed[T](name: String, default: T)(implicit s: PropScope, r: Retriever[T]): T =
    getO(name) getOrElse default

  def tryUse[T](name: String)(f: T => Unit)(implicit s: PropScope, r: Retriever[T]): Unit =
    get(name) foreach f

  def defaultErrorMsg(name: String)(implicit s: PropScope): String =
    s"Unable to retrieve external value: ${s.run(name)}"

  def defaultError(name: String)(implicit s: PropScope): ErrorOr[Nothing] =
    Error(defaultErrorMsg(name))
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

  protected def tryParse[T](f: String => T): Retriever[T] =
    tryParseE(s => \/-(f(s)))

  protected def tryParseE[T](f: String => ErrorOr[T]): Retriever[T] =
    tryParseOE(s => Some(f(s)))

  protected def tryParseOE[T](f: String => Option[ErrorOr[T]]): Retriever[T] =
    Retriever(k =>
        ErrorOr.annotateO(s"Error parsing $k")(
          ErrorOr.catchExceptionO(
            retrieverS.run(k) match {
              case Some(\/-(s)) => f(s)
              case Some(-\/(e)) => Some(-\/(e))
              case None         => None
            })))

  implicit val retrieverI: Retriever[Int] =
    tryParse(Integer.parseInt)

  implicit val retrieverL: Retriever[Long] =
    tryParse(java.lang.Long.parseLong)

  implicit val retrieverB: Retriever[Boolean] = tryParseE(s =>
    if (RegexT.matcher(s).matches)
      \/-(true)
    else if (RegexF.matcher(s).matches)
      \/-(false)
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
          if (v.isEmpty) None else Some(\/-(v))
      }))
}
