package com.beardedlogic.usecase.lib

import java.util.{Date, TimeZone}
import java.text.SimpleDateFormat
import net.liftweb.common.Logger
import net.liftweb.http.S
import org.joda.time.DateTime
import scala.annotation.tailrec
import scala.util.Random
import scalaz.Cord

import com.beardedlogic.usecase.app.AppConfig
import Types._
import AppConfig._

object Misc extends Misc with Logger {

  final val RNG = new Random()

  final val SingleSpace = Cord(" ")

  final val WhitespaceRegex = "\\s+".r

  final val NoEffect1: (Any => Unit) = _ => ()

  private final val ISO8601Format = {
    val tz = TimeZone.getTimeZone("UTC")
    val df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
    df.setTimeZone(tz)
    df
  }

  implicit class AnyExt[V](val v: V) extends AnyVal {
    def modIf[VV >: V](cond: Boolean)(mod: V => VV): VV = if (cond) mod(v) else v
  }

  implicit class ShortExt[V](val a: Short) extends AnyVal {
    def +!(b: Int = 1): Short = (a + b).toShort
  }
}

trait Misc {
  import Misc._

  def clientIp: Option[String] = (
    S.originalRequest.map(_.remoteAddr)
      or S.containerRequest.map(_.remoteAddress)
      or S.request.map(_.remoteAddr)
    // println("X-Real-IP: " + req.header("X-Real-IP"))
    // println("X-Forwarded-For: " + req.header("X-Forwarded-For"))
    )

  def clientIp_Or_? = clientIp.getOrElse("?")

  def currentTimeAsIso8601Str: String @@ ISO8601 = ISO8601Format.synchronized(ISO8601Format.format(new Date)).tag[ISO8601]

  def isConfirmationTokenExpired_?(dateIssued: DateTime): Boolean = TokenLifespan.ago.isAfter(dateIssued)

  def normaliseWhitespaceInSingleLineString(str: String) = Misc.WhitespaceRegex.replaceAllIn(str, " ").trim

  def randomConfirmationToken = randomString(ConfirmationTokenLength)

  def randomString(length: Int): String = RNG.alphanumeric.take(length).mkString

  def removeAllWhitespace(input: String) = WhitespaceRegex.replaceAllIn(input, "")

  //def modIf[V, VV >: V](v: V, cond: Boolean)(mod: V => VV): VV = if (cond) mod(v) else v

  @tailrec
  final def retry[T](n: Int, firstError: Option[Throwable] = None)(fn: => T): T = {
    import scala.util.{Try, Success, Failure}
    Try { fn } match {
      case Success(result)      => result
      case Failure(e) if n > 0  => retry(n - 1, firstError orElse Some(e))(fn)
      case Failure(e) if n <= 0 =>
        firstError.foreach(debug("First retry failure.", _))
        throw e
    }
  }
}