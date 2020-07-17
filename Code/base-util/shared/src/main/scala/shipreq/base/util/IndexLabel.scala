package shipreq.base.util

import japgolly.microlibs.stdlib_ext._
/**
 * Provides labels based on index for items in an ordered sequence.
 */
trait IndexLabel {

  /**
   * @param index ≥ 0
   */
  def label(index: Int): String

  /**
   * Attempt to interpret a label.
   *
   * Parsing should be lenient and accept differences in case, trailing zeros, etc.
   * Whitespace on the other hand shouldn't be considered.
   *
   * @return Option(_ ≥ 0)
   */
  def parse(label: String): Option[Int]
}

object IndexLabel {

  /**
   * 0. Index 0
   * 1. Index 1
   * 2. Index 2
   * ...
   */
  object NumericFrom0 extends IndexLabel {
    override def label(index: Int)    = index.toString
    override def parse(label: String) = ParseInt.unapply(label).filter(_ >= 0)
  }

  /**
   * 1. Index 0
   * 2. Index 1
   * 3. Index 2
   * ...
   */
  object NumericFrom1 extends IndexLabel {
    override def label(index: Int)    = (index + 1).toString
    override def parse(label: String) = ParseInt.unapply(label).map(_ - 1).filter(_ >= 0)
  }

  /**
   * i.   Index 0
   * ii.  Index 1
   * iii. Index 2
   * ...
   */
  object Roman extends IndexLabel {
    override def label(index: Int)    = RomanNumeral(index + 1).toLowerCase
    override def parse(label: String) = RomanNumeral.parse(label).map(_ - 1).filter(_ >= 0)
  }

  /**
   * a. Index 0
   * b. Index 1
   * c. Index 2
   * ...
   */
  object Alpha extends IndexLabel {
    private final val First = 'a'

    override def label(index: Int) = {
      assert(index >= 0, s"Alpha.label($index)")
      @tailrec
      def go(n: Int, s: String): String = {
        val q = n / 26
        val r = n % 26
        val cur = (r + First).toChar.toString
        val s2 = if (s eq null) cur else cur + s
        if (q == 0)
          s2
        else
          go(q - 1, s2)
      }
      go(index, null)
    }

    override def parse(label: String) = {
      var ok = true
      var sum = 0
      for (c <- label) {
        val v = c.toLower - First + 1
        if (v <= 0 || v > 26)
          ok = false
        else
          sum = sum * 26 + v
      }
      if (ok)
        Some(sum - 1)
      else
        None
    }
  }
}
