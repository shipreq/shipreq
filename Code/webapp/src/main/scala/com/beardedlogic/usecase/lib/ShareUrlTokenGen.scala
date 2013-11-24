package com.beardedlogic.usecase.lib

import net.liftweb.common.Logger
import scala.util.Random
import Types._

/**
 * Generates a random string to be used as a ShareUrlToken.
 */
final object ShareUrlTokenGen extends Logger {

  val len = 8

  private val chars = {
    val all = "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz0123456789"
    val excl = "0Oo 1Il" // S5 Z2
    all.toCharArray.filterNot(excl contains _)
  }
  val charSpace = chars.length

  assume(charSpace >= 40 && charSpace < 62)
  debug(s"ShareUrlToken space = $charSpace^$len = ${Math.pow(charSpace, len).toLong}")

  private val rnd = new Random()

  def nextToken: ShareUrlToken = {
    val sb = new StringBuilder(len)
    var i = len
    while (i != 0) {
      sb append chars(rnd.nextInt(charSpace))
      i -= 1
    }
    sb.toString.tag
  }

  val fn: () => ShareUrlToken = nextToken _
}
