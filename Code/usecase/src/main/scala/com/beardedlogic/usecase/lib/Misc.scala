package com.beardedlogic.usecase.lib

import java.util.{Date, TimeZone}
import java.text.SimpleDateFormat
import net.liftweb.http.S
import org.joda.time.DateTime
import scala.util.Random
import com.beardedlogic.usecase.app.AppConfig
import AppConfig._
import Misc._

object Misc extends Misc {

  final val RNG = new Random()

  final val WhitespaceRegex = "\\s+".r

  private final val ISO8601Format = {
    val tz = TimeZone.getTimeZone("UTC")
    val df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
    df.setTimeZone(tz)
    df
  }
}

trait Misc {

  def clientIp: Option[String] = (
    S.originalRequest.map(_.remoteAddr)
      or S.containerRequest.map(_.remoteAddress)
      or S.request.map(_.remoteAddr)
    // println("X-Real-IP: " + req.header("X-Real-IP"))
    // println("X-Forwarded-For: " + req.header("X-Forwarded-For"))
    )

  def clientIp_Or_? = clientIp.getOrElse("?")

  def currentTimeAsIso8601Str: String = ISO8601Format.synchronized(ISO8601Format.format(new Date))

  def isConfirmationTokenExpired_?(dateIssued: DateTime): Boolean = TokenLifespan.ago.isAfter(dateIssued)

  def normaliseWhitespaceInSingleLineString(str: String) = Misc.WhitespaceRegex.replaceAllIn(str, " ").trim

  def randomConfirmationToken = randomString(ConfirmationTokenLength)

  def randomString(length: Int): String = RNG.alphanumeric.take(length).mkString

  def removeAllWhitespace(input: String) = WhitespaceRegex.replaceAllIn(input, "")
}