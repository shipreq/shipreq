package com.beardedlogic.usecase.lib

import java.util.{Date, TimeZone}
import java.text.SimpleDateFormat

object Misc {

  /**
   * Run running single tests from the IDE. the run-mode is still development. This changes it to test.
   */
  def ensureTestModeDuringTests() {
    if (Thread.currentThread.getStackTrace.toList.find(_.getClassName.contains("scalatest")).isDefined)
      System.setProperty("run.mode", "test")
  }

  private val ISO8601Format = {
    val tz = TimeZone.getTimeZone("UTC")
    val df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
    df.setTimeZone(tz)
    df
  }

  def currentTimeAsIso8601Str: String =
    ISO8601Format.synchronized(ISO8601Format.format(new Date))
}