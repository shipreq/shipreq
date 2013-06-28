package com.beardedlogic.usecase.lib

import java.util.{Date, TimeZone}
import java.text.SimpleDateFormat
import scala.util.Random
import com.beardedlogic.usecase.app.AppConfig
import AppConfig._
import Misc._

object Misc extends Misc {

  final val RNG = new Random()

  final val WhitespaceRegex = "\\s+".r

  final val ValidEmailRegex = "^[^&<>]+@[^&<>]+$".r.pattern

  private final val ISO8601Format = {
    val tz = TimeZone.getTimeZone("UTC")
    val df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
    df.setTimeZone(tz)
    df
  }

  /**
   * Run running single tests from the IDE. the run-mode is still development. This changes it to test.
   */
  def ensureTestModeDuringTests() {
    if (Thread.currentThread.getStackTrace.toList.find(_.getClassName.contains("scalatest")).isDefined)
      System.setProperty("run.mode", "test")
  }
}

trait Misc {

  def currentTimeAsIso8601Str: String = ISO8601Format.synchronized(ISO8601Format.format(new Date))

  def randomString(length: Int): String = RNG.alphanumeric.take(length).mkString

  def randomConfirmationToken = randomString(ConfirmationTokenLength)

  def normaliseEmail(email: String) = WhitespaceRegex.replaceAllIn(email, "")

  def isEmailValid_?(email: String) = ValidEmailRegex.matcher(email).matches

  def isConfirmationTokenExpired_?(dateIssued: Date): Boolean = TokenLifespan.ago.after(dateIssued)
}