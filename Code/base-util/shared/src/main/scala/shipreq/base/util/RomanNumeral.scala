package shipreq.base.util

import scala.annotation.switch

/**
 * Conversion to and from roman numerals.
 */
object RomanNumeral {

  private[this] val genData: List[(String, Int)] =
    List(
      ("M", 1000), ("CM", 900), ("D", 500), ("CD", 400), ("C", 100), ("XC", 90),
      ("L", 50), ("XL", 40), ("X", 10), ("IX", 9), ("V", 5), ("IV", 4), ("I", 1))

  def apply(number: Int): String = {
    require(number > 0)
    val sb = new StringBuilder
    var n = number
    for (d <- genData) {
      val r = d._1
      for (_ <- 0 until (n / d._2))
        sb append r
      n = n % d._2
    }
    sb.result()
  }

  private def parseChar(c: Char): Option[Int] =
    (c.toUpper: @switch) match {
      case 'I' => Some(1)
      case 'V' => Some(5)
      case 'X' => Some(10)
      case 'L' => Some(50)
      case 'C' => Some(100)
      case 'D' => Some(500)
      case 'M' => Some(1000)
      case _   => None
    }

  def parse(s: String): Option[Int] =
    if (s.isEmpty)
      None
    else {
      var ok = true
      var sum = 0
      var last = 0
      for (c <- s) {
        parseChar(c) match {
          case Some(r) =>
            sum += r
            if (last < r) sum -= last << 1
            last = r
          case None =>
            ok = false
        }
      }
      if (ok && s.compareToIgnoreCase(apply(sum)) == 0)
        Some(sum)
      else
        None
    }
}
