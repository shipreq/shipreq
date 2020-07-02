package shipreq.webapp.base.util

import org.parboiled2.ParserInput
import scala.annotation.switch

final class PreProcessed private[util] (private val chars: Array[Char]) extends AnyVal {
  def value: ParserInput = chars

  // For tests
  def asString: String = String valueOf chars
}

object PreProcessor {

  type FixChar = (Array[Char], Int) => Unit

  private def _fixChar(a: Array[Char], i: Int, c: Char, nl: Char): Unit =
    c match {

      case '\u0085'    // NEL: Next Line
         | '\u2028'    // LS : Line Separator
         | '\u2029' => // PS : Paragraph Separator
        a(i) = nl

      case _ =>
        if (
          c < 32
            || c == 130 // BREAK PERMITTED HERE (basically blank char)
            || (c >= 55296 && c <= 63743) // invalid + private-use chars
            || (c >= 64976 && c <= 65007) // private-use chars
            || c == 65279 // BOM
            || c >= 65534 // private-use chars (ffff is special to Parboiled)
        ) a(i) = ' '
    }

  val fixCharSingleLine: FixChar =
    (a, i) => _fixChar(a, i, a(i), ' ')

  val fixCharMultiLine: FixChar =
    (a, i) =>
      (a(i): @switch) match {
          case '\n' | '\r' => ()
          case c => _fixChar(a, i, c, '\n')
        }

  type CanTrim = (Array[Char], Int) => Boolean

  @inline def canTrimWhitespaceFn(c: Char): Boolean =
    java.lang.Character.isWhitespace(c)

  val canTrimWhitespace: CanTrim =
    (a, i) => canTrimWhitespaceFn(a(i))

  val singleLine = apply(fixCharSingleLine, canTrimWhitespace)

  def apply(fixChar: FixChar, canTrim: CanTrim): String => PreProcessed =
    input => {
      val inputArray = input.toCharArray

      // ----------------------------------------------------------------
      // Replace bad chars

      {
        var i = inputArray.length
        while (i > 0) {
          i -= 1
          fixChar(inputArray, i)
        }
      }

      // ----------------------------------------------------------------
      // Trim

      val resultArray: Array[Char] = {
        val last = inputArray.length - 1
        var x = 0
        var y = last

        while (y >= 0 && canTrim(inputArray, y)) y -= 1
        while (x < y && canTrim(inputArray, x)) x += 1

        if (y < 0)
          Array.empty
        else if (x == 0 && y == last)
          inputArray
        else
          java.util.Arrays.copyOfRange(inputArray, x, y + 1)
      }

      new PreProcessed(resultArray)
    }
}