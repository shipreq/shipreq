package shipreq.base.util.algorithm

import scala.collection.immutable.IntMap

/**
  * Boyer-Moore-Horspool.
  *
  * It is a simplification of the Boyer–Moore string search algorithm which is related to the Knuth–Morris–Pratt algorithm.
  * The algorithm trades space for time in order to obtain an average-case complexity of O(N) on random text,
  * although it has O(MN) in the worst case, where the length of the pattern is M and the length of the search string is N.
  */
final class BoyerMooreHorspool(pat: Array[Char]) {
  private[this] val m            = pat.length
  private[this] val `m - 1`      = m - 1
  private[this] val badCharShift = new BoyerMooreHorspool.CharToInt(m)

  locally {
    var i = 0
    while (i < `m - 1`) {
      badCharShift(pat.charAt(i)) = `m - 1` - i
      i += 1
    }
  }

  def search(txt: Array[Char]): Boolean = {
    val n = txt.length
    if (n > 0) {
      val stop = n - m
      var start = 0
      while (start <= stop) {
        var index = `m - 1`
        while (txt.charAt(start + index) == pat.charAt(index)) {
          if (index == 0)
            return true
          index -= 1
        }
        start += badCharShift(txt.charAt(start + `m - 1`))
      }
    }
    false
  }
}

object BoyerMooreHorspool {

  private final val CharArrayCutoff = 127

  private[BoyerMooreHorspool] final class CharToInt(default: Int) {
    private[this] val array = Array.fill(CharArrayCutoff)(default)
    private[this] var map   = IntMap.empty[Int]

    def apply(ch: Char): Int = {
      val i = ch.toInt
      if (i < CharArrayCutoff)
        array(i)
      else
        map.getOrElse(i, default)
    }

    def update(ch: Char, v: Int): Unit = {
      val i = ch.toInt
      if (i < CharArrayCutoff)
        array(i) = v
      else
        map = map.updated(i, v)
    }
  }

}