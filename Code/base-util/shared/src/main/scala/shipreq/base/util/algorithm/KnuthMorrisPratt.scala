package shipreq.base.util.algorithm

import scala.collection.immutable.IntMap

/**
 * Knuth-Morris-Pratt DFA substring search.
 *
 * "Algorithms, Fourth Edition", pg. 768.
 */
final class KnuthMorrisPratt(pat: Array[Char]) {
  private val M = pat.length

  private val emptyV = IntMap.empty[Int]
  private val data = Array.fill(M)(emptyV)
  private def get(ch: Char, pos: Int): Int =
    data(pos).getOrElse(ch, 0)

  @inline private def init(): Unit = {
    def set(ch: Char, pos: Int)(v: Int): Unit = {
      val m = data(pos)
      data(pos) = m.updated(ch, v)
    }
    set(pat.charAt(0), 0)(1)
    var x = 0
    var j = 1
    while (j < M) {
      data(j) = data(x)
      val ch = pat.charAt(j)
      set(ch, j)(j + 1)
      x = get(ch, x)
      j += 1
    }
  }
  init()

  def search(txt: Array[Char]): Boolean = {
    val N = txt.length
    var i = 0
    var j = 0
    while (i < N && j < M) {
      j = get(txt.charAt(i), j)
      i += 1
    }
    j == M
  }
}
