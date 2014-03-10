package shipreq.base.util

/**
 * Reads values from some kind of external source.
 *
 * Interface to the source is provided by implicit Retriever[T] type-classes.
 *
 * External source value keys can be transformed implicitly using PropScope.
 */
object ExternalValueReader {

  case class Retriever[T](run: String => Either[Option[String], T])

  case class PropScope(run: String => String)

  val GlobalScope = PropScope(identity)

  def scopeByPrefix(prefix: String) =
    PropScope(prefix + _)

  def scopeByNS(ns: String) =
    if (ns.isEmpty) GlobalScope else scopeByPrefix(s"$ns.")

  def getEO[T](name: String)(implicit s: PropScope, r: Retriever[T]): Either[Option[String], T] =
    r.run(s.run(name))

  def get[T](name: String)(implicit s: PropScope, r: Retriever[T]): Either[String, T] =
    getEO(name).left.map(_ getOrElse defaultErrorMsg(name))

  def getO[T](name: String)(implicit s: PropScope, r: Retriever[T]): Option[T] =
    get(name).right.toOption

  def tryGet[T](name: String, moreNames: String*)(implicit s: PropScope, r: Retriever[T]): Either[String, T] = {
    val es = (name #:: moreNames.toStream).map(get(_))
    es.filter(_.isRight).headOption.getOrElse(es.head)
  }

  def need[T](name: String)(implicit s: PropScope, r: Retriever[T]): T =
    get(name) match {
      case Right(t)  => t
      case Left(msg) => throw new RuntimeException(msg)
    }

  def tryNeed[T](name: String, default: T)(implicit s: PropScope, r: Retriever[T]): T =
    getO(name) getOrElse default

  def tryUse[T](name: String)(f: T => Unit)(implicit s: PropScope, r: Retriever[T]): Unit =
    get(name).right foreach f

  def defaultErrorMsg(name: String)(implicit s: PropScope): String =
    s"Unable to retrieve external value: ${s.run(name)}"
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
    tryParseF(v => Right(f(v)))

  protected def tryParseF[T](f: String => Either[Option[String], T]): Retriever[T] =
    Retriever(k =>
      try retrieverS.run(k) match {
        case Right(v)      => f(v)
        case Left(None)    => Left(None)
        case Left(Some(e)) => Left(Some(s"Error parsing $k: $e"))
      }
      catch { case e: Throwable => Left(Some(s"Error parsing $k: ${e.getMessage}")) }
    )

  implicit val retrieverI: Retriever[Int] =
    tryParse(Integer.parseInt)

  implicit val retrieverL: Retriever[Long] =
    tryParse(java.lang.Long.parseLong)

  implicit val retrieverB: Retriever[Boolean] = tryParseF(s =>
    if (RegexT.matcher(s).matches)
      Right(true)
    else if (RegexF.matcher(s).matches)
      Right(false)
    else
      Left(Some(s"Unable to parse '$s'"))
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
        case None =>
          Left(None)
        case Some(vv) =>
          val v = removeComment(vv).trim
          if (v.isEmpty) Left(None) else Right(v)
      }))
}
